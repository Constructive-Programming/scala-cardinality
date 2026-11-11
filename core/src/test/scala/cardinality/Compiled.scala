package cardinality

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.reporting.Report
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
  * `classes` (the fixture's own) and a marker, `ok` or `failed`: the test that first asked, then
  * what stopped the compiler.
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

  // The Scala library jars: what the compiled types are read against, and with the test classes
  // what most fixtures compile against (a full classpath costs every compile its jars).
  private val libraries: List[Path] = classpath.filter { p =>
    val name = p.getFileName.toString
    name.startsWith("scala-library") || name.startsWith("scala3-library")
  }

  private val minimal: List[Path] = libraries ++ classpath.filter(Files.isDirectory(_))

  private val cache: Path =
    Path.of(sys.props.getOrElse("cardinality.compiled.cache", "target/compiled-fixtures"))

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

  /** The inputs the analysis reads, with the compiler's types written in where they compile. */
  def ascribed(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Input] = {
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

  /** The report of a source set laid out as files, as a build's report reads a compiled module: the
    * files on disk and their classes. Withheld support is compiled against and not written.
    */
  def report(files: (String, String)*): Report = {
    val all = inputs(files*)
    val root = Files.createTempDirectory("cardinality")
    try {
      all.filterNot(_.path.startsWith(Withheld)).foreach(input => written(root, input.path, input))
      val read = Report.of(List(root), Report.Environment(classes = classes(all)))
      // A fixture that compiled and did not get its types would be read half typed.
      if (read.warnings.nonEmpty) throw new AssertionError(read.warnings.mkString("; "))
      read
    } finally deleted(root)
  }

  // One input's text written at `path` below `root`: its file.
  private def written(root: Path, path: String, input: MethodAnalysis.Input): Path = {
    val file = root.resolve(path.stripPrefix("/"))
    Files.createDirectories(file.getParent)
    Files.writeString(file, input.tree.pos.input.text, UTF_8)
  }

  // A directory and what it holds, removed as far as it can be.
  private def deleted(directory: Path): Unit = {
    val held = Files.walk(directory)
    try held.sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
    catch { case _: java.io.IOException | _: java.io.UncheckedIOException => () }
    finally held.close()
  }

  // The fixture's compiled classes with what their types are read against: none where the
  // compiler rejects the fixture, which only a listed fixture may be.
  private def classes(inputs: List[MethodAnalysis.Input]): Option[CompiledTypes.Classes] = {
    val sources = inputs.filterNot(_.path.startsWith(TastyStubs.Prefix))
    val key = digest(Options.mkString(s"$Compiler -sourceroot ", " ", "") :: sources.map(part))
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

  def analyze(
      inputs: List[MethodAnalysis.Input],
      limits: MethodAnalysis.Limits = MethodAnalysis.Limits()
  ): List[MethodAnalysis.Entry] = MethodAnalysis.analyze(ascribed(inputs), limits)

  def analyzeOpen(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyzeOpen(ascribed(inputs))

  // What every fixture is compiled with, beside its output and classpath. `-Xkind-projector` reads
  // the `F[A, *]` placeholder some fixtures spell, as the libraries that write it compile.
  private val Options = List("-nowarn", "-Xkind-projector")

  // The compiler every fixture is compiled with.
  private val Compiler = dotty.tools.dotc.config.Properties.versionString

  // One source as a digest reads it. A cache key is the digest of the compiler, its options and the
  // sources: an entry compiled otherwise is not this compile (`-sourceroot` stands for the option
  // `scalac` adds with each compile's own directory).
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

  // One entry per key: `classes` holds a successful compile, `failed` marks a rejected one. Two
  // threads asking for the same new key both compile it, and the first to finish keeps the entry.
  private def compile(key: String, sources: List[MethodAnalysis.Input]): Option[Path] = {
    val root = cache.toAbsolutePath.resolve(key)
    val out = root.resolve("classes")
    if (Files.exists(root.resolve("ok"))) Some(out)
    else if (Files.exists(root.resolve("failed")) || sources.isEmpty) None
    else {
      val staging =
        Files
          .createDirectories(cache.toAbsolutePath)
          .resolve(s"$key.${Thread.currentThread.threadId}")
      // The support sources alone, then the fixture against their classes.
      val src = staging.resolve("src")
      val (support, own) = sources.partition(_.path.startsWith(Withheld)) match {
        case (held, fixture) =>
          def files(of: List[MethodAnalysis.Input]) = of.map(i => written(src, i.path, i).toString)
          (files(held), files(fixture))
      }
      def run(cp: List[Path]): List[String] =
        if (support.isEmpty) scalac(src, staging.resolve("classes"), cp, own)
        else {
          val held = staging.resolve("support")
          scalac(src, held, cp, support) match {
            case Nil      => scalac(src, staging.resolve("classes"), held :: cp, own)
            case problems => "in the withheld support:" :: problems
          }
        }
      // The full classpath is tried where the small one is not enough, not where the compiler ran
      // out of time: it would run out again.
      val problems = run(minimal) match {
        case small @ List(only) if only.startsWith("not finished") => small
        case Nil                                                   => Nil
        case _                                                     => run(classpath)
      }
      Files.writeString(
        staging.resolve(if (problems.isEmpty) "ok" else "failed"),
        (asker :: problems).mkString("\n")
      )
      try Files.move(staging, root)
      catch {
        // Another thread compiled the same text first (the move then fails as file exists or as
        // directory not empty): its entry stands and this copy goes. A compile that timed out may
        // still be writing here, so what cannot be deleted stays.
        case moved: java.nio.file.FileSystemException =>
          if (!Files.isDirectory(root)) throw moved
          deleted(staging)
      }
      Option.when(Files.exists(root.resolve("ok")))(out)
    }
  }

}
