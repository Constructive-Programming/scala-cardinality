package cardinality

import cardinality.capacity.Definition
import cardinality.compiled.TastyFrontEnd
import cardinality.reporting.Report
import cardinality.tasty.CompiledClasses
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap

/** Test sources as the build plugins read a module: compiled first, then read by the TASTy front
  * end (`TastyFrontEnd`) from their classes.
  *
  * A fixture the compiler rejects has no classes and no rows, and is rejected only where it is
  * meant to be: `rejected-fixtures.txt` (a test resource) lists those fixtures, each with its
  * example and the reason. One that is rejected and not listed, or listed and compiles, fails its
  * test.
  *
  * A fixture that names a declaration the analysis is deliberately not supplied still compiles: its
  * text carries the declaration as support (`code + Compiled.withheld(support)`), and `inputs`
  * reads the support as a source of its own, at a path that starts with [[Withheld]]. Support
  * sources are compiled before the fixture, the fixture is compiled against their classes, and they
  * are then dropped: the analyzer gets neither their text nor their classes.
  *
  * Compilations are memoised by the compiler options and the sources' text, in memory and on disk
  * under `target/compiled-fixtures/<digest>`, so a rerun compiles only what changed. An entry holds
  * `src` (every source, support under `src/withheld`), `support` (the support sources' classes),
  * `dependency/<unit>` (each dependency unit's), `classes` (the fixture's own), `classpath.txt` (a
  * record for whoever reads the cache: what a fixture that compiled was compiled against beside its
  * own support and units, one path a line) and a marker, `ok` or `failed`: the test that first
  * asked, then what stopped the compiler.
  *
  * A fixture's dependency is compiled apart too, and supplied: as classes, not as sources of the
  * module (`Compiled.dependency`, `Compiled.reportOver`). The units are compiled before the
  * withheld support, so a unit cannot name what the fixture withholds.
  *
  * Temporary directories (`scratch`) are made under `java.io.tmpdir` and deleted after use.
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

  /** One source of a fixture: where it is laid out, and its text. */
  final case class Input(path: String, text: String)

  // The path prefix of a support source: compiled, never analyzed.
  private[cardinality] val Withheld = "withheld/"

  // Between a fixture's text and the support it carries. It is no Scala, so a text that carries
  // support and is compiled without `inputs` does not compile.
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

  /** The path prefix of a dependency's source, before its unit's name: compiled, and given to the
    * analysis as classes. The cache keeps a unit's sources under it in `src`, and its classes under
    * it beside `classes`.
    */
  val Dependency = "dependency/"

  /** One source of the dependency unit `unit`, as a file of a set given to `reportOver`. A unit is
    * compiled alone, against the units the set names before it.
    */
  def dependency(unit: String, code: String): (String, String) = {
    require(unit.nonEmpty && !unit.contains('/'), s"a unit is one name, not $unit")
    s"$Dependency$unit/${code.hashCode.toHexString}.scala" -> code
  }

  // The dependency units a fixture names, in its order.
  private def units(inputs: List[Input]): List[String] =
    inputs
      .map(_.path)
      .collect {
        case path if path.startsWith(Dependency) => path.stripPrefix(Dependency).takeWhile(_ != '/')
      }
      .distinct

  // A source the analysis is not given as one of the module's: withheld support, a dependency.
  private def apart(input: Input): Boolean =
    input.path.startsWith(Withheld) || input.path.startsWith(Dependency)

  /** The fixture's sources: each text up to the support it carries, and that support as a source
    * under [[Withheld]].
    */
  def inputs(files: (String, String)*): List[Input] =
    files.toList.flatMap { (path, text) =>
      val parts = text.split(Carried, -1).toList
      require(parts.sizeIs <= 2, s"$path carries ${parts.size - 1} supports: write them as one")
      // The support's file is named apart from the fixture's: both may hold top-level definitions,
      // which compile to a class named after the file.
      parts.zip(List(path, s"$Withheld${path.stripSuffix(".scala")}.support.scala")).map {
        (code, at) => Input(at, code)
      }
    }

  // The fixture's rows as the compiled front end reads them: the supplied sources against the
  // fixture's classes, with its dependencies and the Scala library as the classpath, and nothing
  // else the fixture compiled against (the withheld support, the rest of the test classpath). The
  // Scala library and the JDK are always there to read: `report` and `reportOver` with no
  // dependency are one request. A fixture scalac rejects has no classes to read.
  private def read(inputs: List[Input], beside: Option[List[Path]] = None): Report = {
    // Only a request with a dependency classpath may carry dependency units.
    require(beside.nonEmpty || units(inputs).isEmpty, "a dependency is read by `reportOver`")
    val dependencies = beside.getOrElse(Nil)
    val module = inputs.filterNot(apart)
    val found = classes(inputs).getOrElse(
      throw new AssertionError("no compiled classes: scalac rejects this fixture")
    )
    val classpath = supplied(inputs, found.output.head, dependencies)
    val answered = TastyFrontEnd.rows(
      module.map(input => input.path -> input.text),
      Some(found.copy(classpath = classpath))
    )
    if (answered.warnings.nonEmpty) throw new AssertionError(answered.warnings.mkString("; "))
    Report(
      answered.sources.map {
        case Right(source) => source
        case Left(error)   => throw new AssertionError(s"${error.path}: ${error.message}")
      },
      Nil
    )
  }

  /** The definition rows of one source, as a report lists them. */
  def definitions(code: String): List[Definition] =
    read(inputs("Example.scala" -> code)).definitions

  /** The rows of a source set, as a build's report reads a compiled module, each source at the
    * fixture's path. Withheld support is compiled against and not read.
    */
  def report(files: (String, String)*): Report = read(inputs(files*))

  /** The report of a module with dependencies, as a build's reads one: the files on disk, their
    * classes, and as the dependency classpath the set's own dependency units (`dependency`), then
    * `dependencies` (class directories or jars of classes the test classpath holds, which is what
    * the module compiles against), then the Scala library, which a build always passes.
    */
  def reportOver(dependencies: List[Path], files: (String, String)*): Report =
    read(inputs(files*), Some(dependencies))

  /** A source set's compiled classes and the classpath a build hands with them (the set's own units
    * and the Scala library), for a caller that writes the files itself.
    */
  def classesOf(files: (String, String)*): CompiledClasses =
    classes(inputs(files*)).getOrElse(
      throw new AssertionError("no compiled classes: scalac rejects this fixture")
    )

  // One input's text written at `path` below `root`: its file.
  private def written(root: Path, path: String, input: Input): Path = {
    val file = root.resolve(path.stripPrefix("/"))
    Files.createDirectories(file.getParent)
    Files.writeString(file, input.text, UTF_8)
  }

  /** `body` over a new temporary directory under `java.io.tmpdir`, deleted after it. */
  def scratch[A](prefix: String)(body: Path => A): A = {
    val directory = Files.createTempDirectory(prefix)
    try body(directory)
    finally deleted(directory)
  }

  /** A new temporary directory, for files read after the caller returns (compiled classes a row
    * reads lazily): under one directory of this test JVM in `java.io.tmpdir`, deleted when it
    * exits.
    */
  def temporary(prefix: String): Path = Files.createTempDirectory(session, prefix)

  private lazy val session: Path = {
    val root = Files.createTempDirectory("cardinality-tests")
    Runtime.getRuntime.addShutdownHook(new Thread(() => deleted(root)))
    root
  }

  // A directory and what it holds, removed as far as it can be.
  private[cardinality] def deleted(directory: Path): Unit =
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
  private def classes(
      inputs: List[Input],
      dependencies: List[Path] = Nil
  ): Option[CompiledClasses] = {
    val sources = inputs // every source, support and units included
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
      CompiledClasses(List(dir), supplied(inputs, dir, dependencies ++ support))
    }
  }

  // The class directories of the fixture's dependency units, in the entry that holds `classes`.
  private def unitClasses(inputs: List[Input], classes: Path): List[Path] =
    units(inputs).map(unit => classes.resolveSibling(Dependency + unit))

  // The dependency classpath of a fixture, in one order wherever it is read, the first to declare a
  // class winning: its units as it names them, then `dependencies`, then the Scala library.
  private def supplied(
      inputs: List[Input],
      classes: Path,
      dependencies: List[Path]
  ): List[Path] = unitClasses(inputs, classes) ++ dependencies ++ libraries

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

  /** The method rows of a source set. */
  def analyze(inputs: List[Input]): List[Report.Entry] = read(inputs).methods

  // What every fixture is compiled with, beside its output and classpath. `-Xkind-projector` reads
  // the `F[A, *]` placeholder some fixtures spell, as the libraries that write it compile.
  private val Options = List("-nowarn", "-Xkind-projector")

  // The compiler every fixture is compiled with.
  private val Compiler = dotty.tools.dotc.config.Properties.versionString

  // One source as a digest reads it. A cache key is the digest of the compiler, its options, the
  // name of the classpath file an entry records and the sources: an entry compiled otherwise, or
  // written before entries recorded their classpath, is not this compile (`-sourceroot` stands for
  // the option `scalac` adds with each compile's own directory).
  private def part(source: Input): String =
    s"${source.path.stripPrefix("/")}\u0000${source.text}"

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
  private def compile(key: String, sources: List[Input]): Option[Path] = {
    val root = cache.toAbsolutePath.resolve(key)
    val out = root.resolve("classes")
    if (Files.exists(root.resolve("ok"))) Some(out)
    else if (Files.exists(root.resolve("failed")) || sources.isEmpty) None
    else {
      val staged = staging.resolve(s"$key.${Thread.currentThread.threadId}")
      val src = staged.resolve("src")
      def laidOut(of: List[Input]) = of.map(i => written(src, i.path, i).toString)
      // What is compiled before the fixture, each part alone and against the ones before it: the
      // dependency units in the fixture's order, then the withheld support.
      val before = units(sources).map { unit =>
        val of = sources.filter(_.path.startsWith(s"$Dependency$unit/"))
        (s"the dependency $unit", staged.resolve(Dependency + unit), laidOut(of))
      } ++ Option(sources.filter(_.path.startsWith(Withheld))).filter(_.nonEmpty).map { held =>
        ("the withheld support", staged.resolve("support"), laidOut(held))
      }
      val own = laidOut(sources.filterNot(apart))
      def run(cp: List[Path]): List[String] =
        before
          .foldLeft[Either[List[String], List[Path]]](Right(Nil)) {
            case (Right(built), (what, into, files)) =>
              scalac(src, into, built ++ cp, files) match {
                case Nil      => Right(built :+ into)
                case problems => Left(s"in $what:" :: problems)
              }
            case (failed, _) => failed
          }
          .fold(identity, built => scalac(src, staged.resolve("classes"), built ++ cp, own))
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
