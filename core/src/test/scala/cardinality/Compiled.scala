package cardinality

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.{Counter, Definition, FinitePart, Size}
import cardinality.compiled.TastyFrontEnd
import cardinality.reporting.{FrontEnd, Report}
import cardinality.tasty.{CompiledTypes, TastyStubs}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap

/** Test sources as the build plugins read a module: compiled first, so every member, local, pattern
  * name and lambda parameter written without a type has the compiler's (`CompiledTypes`, rule T).
  *
  * A fixture the compiler rejects is read as written, and only where it is meant to be rejected:
  * `rejected-fixtures.txt` (a test resource) lists those fixtures, each with its example and the
  * reason. One that is rejected and not listed, or listed and compiles, fails its test.
  *
  * A fixture that names a declaration the analysis is deliberately not supplied still compiles: its
  * text carries the declaration as support (`code + Compiled.withheld(support)`), and `inputs`
  * parses the support as a source of its own, at a path that starts with [[Withheld]]. Support
  * sources are compiled first, the fixture is compiled against their classes, and they are then
  * dropped: the analyzer gets neither their text nor their classes.
  *
  * Compilations are memoised by the compiler options and the sources' text, in memory and on disk
  * under `target/compiled-fixtures/<digest>`, so a rerun compiles only what changed. An entry holds
  * `src` (every source, support under `src/withheld`), `support` (the support sources' classes),
  * `classes` (the fixture's own), `classpath.txt` (what a fixture that compiled was compiled
  * against beside `support`, one path a line) and a marker, `ok` or `failed`: the test that first
  * asked, then what stopped the compiler.
  *
  * The rows a test reads (`analyze`, `definitions`, `report`) come from the front end the system
  * property `cardinality.frontend` names: [[FrontEndMode]]. What only the scalameta front end has
  * (`source` and `ascribed`, its tree; `analyzeOpen`, the open world; dependency stubs given as
  * sources) fails under any other and says so.
  */
object Compiled {

  // The test classpath (the build passes it in): the Scala library and everything a fixture may
  // name.
  private val classpath: List[Path] =
    sys.props
      .getOrElse("cardinality.test.classpath", sys.props("java.class.path"))
      .split(java.io.File.pathSeparator)
      .toList
      .map(Path.of(_))

  // The Scala library jars: what the compiled types are read against, and with the class
  // directories of the test classpath what most fixtures compile against (a full classpath costs
  // every compile its jars).
  private val libraries: List[Path] = classpath.filter { p =>
    val name = p.getFileName.toString
    name.startsWith("scala-library") || name.startsWith("scala3-library")
  }

  private val minimal: List[Path] = libraries ++ classpath.filter(Files.isDirectory(_))

  private val cache: Path =
    Path.of(sys.props.getOrElse("cardinality.compiled.cache", "target/compiled-fixtures"))

  // Where this JVM writes a compile before its entry is moved into place: a directory of its own
  // under the cache's `staging`. What a JVM that has ended left there (a compiler that ran out of
  // time writes on after its entry is moved) goes when this one starts.
  private val staging: Path = {
    val all = Files.createDirectories(cache.toAbsolutePath.resolve("staging"))
    val self = ProcessHandle.current.pid
    val left = Files.list(all)
    try
      left.forEach { directory =>
        val running = directory.getFileName.toString.toLongOption
          .exists(pid => pid != self && ProcessHandle.of(pid).isPresent)
        if (!running) deleted(directory)
      }
    finally left.close()
    Files.createDirectories(all.resolve(self.toString))
  }

  private val ClasspathFile = "classpath.txt"

  private val compiled = new ConcurrentHashMap[String, Option[Path]]()

  // The path prefix of a support source: compiled, never analyzed.
  private val Withheld = "withheld/"

  // Between a fixture's text and the support it carries. It is no Scala, so a text that carries
  // support and is parsed without `inputs` does not parse.
  private val Carried = "\u0000"

  /** Appended to a fixture's text: declarations it names that the analysis is not supplied. One
    * text carries one support; two packages are written in the block form (`package a { … }`).
    */
  def withheld(support: String): String = Carried + support

  /** The same support as a file of its own, for a set of files: one entry of the list given to
    * `inputs` or `report`, where `withheld` is appended to one source's text.
    */
  def withheldFile(support: String): (String, String) =
    s"$Withheld${support.hashCode.toHexString}.scala" -> support

  /** The fixture's sources parsed: each text up to the support it carries, and that support as a
    * source under [[Withheld]].
    */
  def inputs(files: (String, String)*): List[MethodAnalysis.Input] =
    files.toList.flatMap { (path, text) =>
      val parts = text.split(Carried, -1).toList
      require(parts.sizeIs <= 2, s"$path carries ${parts.size - 1} supports: write them as one")
      // The support's file is named apart from the fixture's: both may hold top-level definitions,
      // which compile to a class named after the file.
      parts.zip(List(path, s"$Withheld${path.stripSuffix(".scala")}.support.scala")).map {
        (code, at) => MethodAnalysis.Input(at, dialects.Scala3(code).parse[Source].get)
      }
    }

  /** Which front end answers a request for rows: the scalameta pipeline a report runs, unless the
    * system property `cardinality.frontend` names another.
    */
  private enum FrontEndMode {
    case Scalameta
    // `TastyFrontEnd` over the fixture's compiled classes, and nothing of the scalameta pipeline.
    case Tasty
    // As `Tasty`, each row not read yet holding a count of its own that no example expects: an
    // example that passes under `Tasty` and fails here passed on a refusal that was only `not read
    // yet`. One that only says what such a row is not passes under both.
    case Sentinel
    // As `Tasty`, each row not read yet holding the count the scalameta pipeline gives its name:
    // an example that fails under `Tasty` and passes here fails for those counts and nothing else.
    case Filled
    // No rows, every request for them fails: an example that fails so is one that reads rows.
    case Probe
  }

  private val frontEnd: FrontEndMode =
    FrontEndMode.valueOf(sys.props.getOrElse("cardinality.frontend", "scalameta").capitalize)

  // The one place that decides where rows come from: `old` is what the scalameta pipeline answers
  // for the request, evaluated only where the mode reads it.
  private def read(inputs: List[MethodAnalysis.Input])(old: => Report): Report = frontEnd match {
    case FrontEndMode.Scalameta => old
    case FrontEndMode.Probe     =>
      throw new AssertionError("rows requested from the fixture harness")
    case FrontEndMode.Tasty    => compiledRows(inputs)
    case FrontEndMode.Sentinel =>
      // Negative counts, and sizes no fixture has (`7ε₀ + nω`), each used once: two rows not read
      // yet are not equal either.
      val next = Iterator.from(1)
      unreadReplaced(compiledRows(inputs))(
        _.copy(size = Some(Size(7, 7 + next.next(), FinitePart.Zero)), unbound = Nil),
        _.copy(count = Count.Finite(-next.next()))
      )
    case FrontEndMode.Filled =>
      val (scalameta, compiled) = (old, compiledRows(inputs))
      // Each row with the scalameta one of its key: the n-th of a key with the n-th, where both
      // front ends list as many.
      def paired[A, K](old: List[A], rows: List[A])(key: A => K): Map[A, A] =
        rows.groupBy(key).flatMap { (shared, keyed) =>
          val known = old.filter(key(_) == shared)
          if (known.sizeIs == keyed.size) keyed.zip(known) else Nil
        }
      val definitions =
        paired(scalameta.definitions, compiled.definitions)(row => (row.name, row.kind))
      val methods = paired(scalameta.methods, compiled.methods)(_.name)
      // Only what `not read yet` stands for is taken, the count and the captures: the row keeps
      // the name, kind, signature and line the compiled front end gives it.
      unreadReplaced(compiled)(
        row => definitions.get(row).fold(row)(of => row.copy(size = of.size, unbound = of.unbound)),
        row => methods.get(row).fold(row)(of => row.copy(count = of.count, captures = of.captures))
      )
  }

  // The report with other rows for the ones the compiled front end does not read yet: those whose
  // every reason starts by saying so.
  private def unreadReplaced(report: Report)(
      definition: Definition => Definition,
      method: MethodAnalysis.Entry => MethodAnalysis.Entry
  ): Report = {
    def unread(reasons: List[String]) =
      reasons.nonEmpty && reasons.forall(_.startsWith(TastyFrontEnd.Uncovered))
    report.copy(sources = report.sources.map { source =>
      source.copy(
        definitions = source.definitions.map { row =>
          if (unread(row.unbound.map(_.render))) definition(row) else row
        },
        methods = source.methods.map { row =>
          row.count match {
            case Count.Unresolved(reasons) if unread(reasons) => method(row)
            case _                                            => row
          }
        }
      )
    })
  }

  // The fixture's rows as the compiled front end reads them: the supplied sources against the
  // fixture's classes and what it was compiled with, the withheld support apart. A fixture scalac
  // rejects has no classes to read, and dependency stubs given as sources have no reader.
  private def compiledRows(inputs: List[MethodAnalysis.Input]): Report = {
    if (inputs.exists(_.path.startsWith(TastyStubs.Prefix)))
      scalametaOnly("dependency stubs given as sources")
    val supplied = inputs.filterNot(_.path.startsWith(Withheld))
    val found = classes(inputs).getOrElse(
      throw new AssertionError("no compiled classes: scalac rejects this fixture")
    )
    val compiledWith = found.output.flatMap { dir =>
      Files.readString(dir.resolveSibling(ClasspathFile)).linesIterator.map(Path.of(_))
    }
    val answered = TastyFrontEnd.rows(
      FrontEnd.Module(
        supplied.map(input => input.path -> input.tree.pos.input.text),
        Report.Environment(classes = Some(found.copy(classpath = compiledWith)))
      )
    )
    if (answered.warnings.nonEmpty) throw new AssertionError(answered.warnings.mkString("; "))
    Report(
      answered.sources.zip(supplied).map {
        case (Right(source), _) => source
        // A source that defines nothing has no classes, and no rows.
        case (Left(Report.Error(_, NoClasses)), input)
            if input.tree.collect { case defined: Defn => defined }.isEmpty =>
          Report.Source(input.path, Nil)
        case (Left(error), _) => throw new AssertionError(s"${error.path}: ${error.message}")
      },
      Nil
    )
  }

  // How `TastyFrontEnd` answers a source no compiled class names.
  private val NoClasses = "no compiled classes for this source"

  // Fails where the caller reads `what` and another front end than the scalameta one answers.
  private def scalametaOnly(what: String): Unit =
    if (frontEnd != FrontEndMode.Scalameta)
      throw new AssertionError(s"reads $what, which only the scalameta front end has")

  /** The inputs as the scalameta pipeline reads them (`typed`): its tree, which no other front end
    * has.
    */
  def ascribed(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Input] = {
    scalametaOnly("the scalameta tree")
    typed(inputs)
  }

  // The inputs the analysis reads, with the compiler's types written in where they compile.
  private def typed(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Input] = {
    val supplied = inputs.filterNot(_.path.startsWith(Withheld))
    val (stubs, own) = supplied.partition(_.path.startsWith(TastyStubs.Prefix))
    classes(inputs).fold(supplied) { found =>
      val (typed, problems) = CompiledTypes.ascribe(own, found)
      // A fixture that compiled and did not get its types would be read half typed.
      if (problems.nonEmpty) throw new AssertionError(problems.mkString("; "))
      typed ++ stubs
    }
  }

  /** One source as the analysis reads it: compiled, with the compiler's types written in. */
  def source(code: String): Source = ascribed(inputs("Example.scala" -> code)).head.tree

  /** The definition rows of one source, as a report lists them. */
  def definitions(code: String): List[Definition] = {
    val all = inputs("Example.scala" -> code)
    read(all)(asReport(Counter.definitions(typed(all).head.tree), Nil)).definitions
  }

  private def asReport(definitions: List[Definition], methods: List[MethodAnalysis.Entry]): Report =
    Report(List(Report.Source("", definitions, methods)), Nil)

  /** The report of a source set laid out as files, as a build's report reads a compiled module: the
    * files on disk and their classes. Withheld support is compiled against and not written. Under
    * another front end the report is its rows alone, each source at the fixture's path.
    */
  def report(files: (String, String)*): Report = {
    val all = inputs(files*)
    read(all) {
      val root = Files.createTempDirectory("cardinality")
      try {
        all
          .filterNot(_.path.startsWith(Withheld))
          .foreach(input => written(root, input.path, input))
        val found = Report.of(List(root), Report.Environment(classes = classes(all)))
        // A fixture that compiled and did not get its types would be read half typed.
        if (found.warnings.nonEmpty) throw new AssertionError(found.warnings.mkString("; "))
        found
      } finally deleted(root)
    }
  }

  // One input's text written at `path` below `root`: its file.
  private def written(root: Path, path: String, input: MethodAnalysis.Input): Path = {
    val file = root.resolve(path.stripPrefix("/"))
    Files.createDirectories(file.getParent)
    Files.writeString(file, input.tree.pos.input.text, UTF_8)
  }

  // A directory and what it holds, removed as far as it can be.
  private def deleted(directory: Path): Unit =
    try {
      val held = Files.walk(directory)
      try
        held.sorted(java.util.Comparator.reverseOrder()).forEach { path =>
          try Files.delete(path)
          catch { case _: java.io.IOException => () }
        }
      finally held.close()
    } catch { case _: java.io.IOException | _: java.io.UncheckedIOException => () }

  // The fixture's compiled classes with what their types are read against: none where the
  // compiler rejects the fixture, which only a listed fixture may be.
  private def classes(inputs: List[MethodAnalysis.Input]): Option[CompiledTypes.Classes] = {
    val sources = inputs.filterNot(_.path.startsWith(TastyStubs.Prefix))
    val key =
      digest(
        Options.mkString(s"$Compiler $ClasspathFile -sourceroot ", " ", "") :: sources.map(part)
      )
    // Not `computeIfAbsent`: a compile runs outside the map's locks, so compiles run in parallel.
    val out = Option(compiled.get(key)).getOrElse {
      val result = compile(key, sources)
      compiled.putIfAbsent(key, result)
      result
    }
    val name = digest(sources.map(part).sorted)
    (out, rejected(name)) match {
      case (None, false) if sources.nonEmpty =>
        val entry = cache.toAbsolutePath.resolve(key)
        throw new AssertionError(
          s"scalac rejects a fixture that $Rejected does not list as $name (entry $entry: " +
            s"delete it where the compiler only ran out of time), ${Files.readString(entry.resolve("failed"))}"
        )
      case (Some(_), true) =>
        throw new AssertionError(s"$Rejected lists a fixture that compiles: remove $name")
      case _ =>
    }
    // The support classes are only what the compiled types are spelled against: a type naming a
    // withheld declaration is written by its name, which the analysis then does not resolve.
    out.map { dir =>
      val support = List(dir.resolveSibling("support")).filter(Files.isDirectory(_))
      CompiledTypes.Classes(List(dir), support ++ libraries)
    }
  }

  // The fixtures scalac is meant to reject, named by the digest of their sources in path order
  // (whatever the compiler options): the first word of each line of the list.
  private val Rejected = "rejected-fixtures.txt"

  private val rejected: Set[String] = {
    val resource = Option(getClass.getResourceAsStream(s"/$Rejected"))
      .getOrElse(throw new AssertionError(s"test resource $Rejected not found"))
    val list = scala.io.Source.fromInputStream(resource, "UTF-8")
    try list.getLines().map(_.takeWhile(!_.isWhitespace)).filter(_.length == 64).toSet
    finally list.close()
  }

  /** The method rows of a source set. `limits` are the scalameta pipeline's: the compiled front end
    * takes none.
    */
  def analyze(
      inputs: List[MethodAnalysis.Input],
      limits: MethodAnalysis.Limits = MethodAnalysis.Limits()
  ): List[MethodAnalysis.Entry] =
    read(inputs)(asReport(Nil, MethodAnalysis.analyze(typed(inputs), limits))).methods

  /** The method rows as an open world reads the sources: a reading only the scalameta front end
    * has.
    */
  def analyzeOpen(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Entry] = {
    scalametaOnly("the open world")
    MethodAnalysis.analyzeOpen(typed(inputs))
  }

  // What every fixture is compiled with, beside its output and classpath. `-Xkind-projector` reads
  // the `F[A, *]` placeholder some fixtures spell, as the libraries that write it compile.
  private val Options = List("-nowarn", "-Xkind-projector")

  // The compiler every fixture is compiled with.
  private val Compiler = dotty.tools.dotc.config.Properties.versionString

  // One source as a digest reads it. A cache key is the digest of the compiler, its options, the
  // name of the classpath file an entry records and the sources: an entry compiled otherwise, or
  // written before entries recorded their classpath, is not this compile (`-sourceroot` stands for
  // the option `scalac` adds with each compile's own directory).
  private def part(source: MethodAnalysis.Input): String =
    s"${source.path.stripPrefix("/")}\u0000${source.tree.pos.input.text}"

  private def digest(parts: List[String]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(parts.mkString("\u0001").getBytes(UTF_8))
      .map("%02x".format(_))
      .mkString

  // A compile the compiler does not finish (a fixture of 70 aliases and a partial alias application
  // keeps 3.9.0's typer busy) counts as rejected: the fixture is read as written.
  private val Limit = 60000L

  // `body` on a daemon thread: its answer (false where it threw), none where it is still running
  // after `millis`.
  private def within(millis: Long)(body: => Boolean): Option[Boolean] = {
    @volatile var answer = false
    val thread = new Thread(() => answer = body, "fixture-compile")
    thread.setDaemon(true)
    thread.start()
    thread.join(millis)
    Option.when(!thread.isAlive)(answer)
  }

  // What stopped one compiler run into `out`: nothing where it compiled. `rejected:` heads the
  // compiler's errors, and stands alone where the compiler threw. TASTy records each source by its
  // path below `sourceRoot`, the fixture's own (`p/A.scala`), so the entry's `src` holds every source
  // where its classes say it is, wherever the entry is moved.
  private def scalac(
      sourceRoot: Path,
      out: Path,
      cp: List[Path],
      files: List[String]
  ): List[String] = {
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    val args = List(
      "-d",
      Files.createDirectories(out).toString,
      "-classpath",
      cp.mkString(java.io.File.pathSeparator),
      "-sourceroot",
      sourceRoot.toString
    ) ++ Options ++ files
    within(Limit)(!dotty.tools.dotc.Main.process(args.toArray, reporter).hasErrors) match {
      case Some(true)  => Nil
      case Some(false) => "rejected:" :: reporter.allErrors.map(_.message)
      case None        => List(s"not finished in ${Limit / 1000} s")
    }
  }

  // The test asking for a compile: this project's frames on its stack outside this object,
  // innermost first (the spec's helpers, then the example).
  private def asker: String =
    Thread.currentThread.getStackTrace.toList
      .map(frame => frame.getClassName -> s"${frame.getFileName}:${frame.getLineNumber}")
      .collect {
        case (owner, at) if owner.startsWith("cardinality.") && !owner.startsWith(Self) => at
      }
      .distinct
      .mkString("asked by ", " < ", "")

  // This object's class, which its lambdas' classes extend by name.
  private val Self = getClass.getName

  // One entry per key: `ok` marks a compile that succeeded, `failed` one that was rejected. Two
  // threads asking for the same new key both compile it, and the first to finish keeps the entry.
  private def compile(key: String, sources: List[MethodAnalysis.Input]): Option[Path] = {
    val root = cache.toAbsolutePath.resolve(key)
    val out = root.resolve("classes")
    if (Files.exists(root.resolve("ok"))) Some(out)
    else if (Files.exists(root.resolve("failed")) || sources.isEmpty) None
    else {
      val staged = staging.resolve(s"$key.${Thread.currentThread.threadId}")
      // The support sources alone, then the fixture against their classes.
      val src = staged.resolve("src")
      val (support, own) = sources.partition(_.path.startsWith(Withheld)) match {
        case (held, fixture) =>
          def files(of: List[MethodAnalysis.Input]) = of.map(i => written(src, i.path, i).toString)
          (files(held), files(fixture))
      }
      def run(cp: List[Path]): List[String] =
        if (support.isEmpty) scalac(src, staged.resolve("classes"), cp, own)
        else {
          val held = staged.resolve("support")
          scalac(src, held, cp, support) match {
            case Nil      => scalac(src, staged.resolve("classes"), held :: cp, own)
            case problems => "in the withheld support:" :: problems
          }
        }
      // The full classpath is tried where the small one is not enough, not where the compiler ran
      // out of time: it would run out again.
      val (problems, compiledWith) = run(minimal) match {
        case Nil                                                            => (Nil, minimal)
        case small if small.lastOption.exists(_.startsWith("not finished")) => (small, minimal)
        case _ => (run(classpath), classpath)
      }
      // The classpath is written before the marker: an entry marked `ok` has it.
      val files =
        if (problems.isEmpty) List(ClasspathFile -> compiledWith.mkString("\n"), "ok" -> asker)
        else List("failed" -> (asker :: problems).mkString("\n"))
      files.foreach((name, text) => Files.writeString(staged.resolve(name), text))
      try Files.move(staged, root)
      catch {
        // Another thread compiled the same text first (the move then fails as file exists or as
        // directory not empty): its entry stands and this copy goes. A compile that timed out may
        // still be writing here, so what cannot be deleted stays.
        case moved: java.nio.file.FileSystemException =>
          if (!Files.isDirectory(root)) throw moved
          deleted(staged)
      }
      Option.when(Files.exists(root.resolve("ok")))(out)
    }
  }

}
