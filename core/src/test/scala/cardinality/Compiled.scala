package cardinality

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.tasty.{CompiledTypes, TastyStubs}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap

/** Test sources as the build plugins read a module: compiled first, so every member, local, pattern
  * name and lambda parameter written without a type has the compiler's (`CompiledTypes`, rule T). A
  * fixture the compiler rejects is read as written.
  *
  * A fixture that names a declaration the analysis is deliberately not supplied still compiles: its
  * text carries the declaration as support (`code + Compiled.withheld(support)`), and `inputs`
  * parses the support as a source of its own, at a path that starts with [[Withheld]]. Support
  * sources are compiled first, the fixture is compiled against their classes, and they are then
  * dropped: the analyzer gets neither their text nor their classes. Support is there to make its
  * fixture compile, so a fixture that has some and is still rejected fails its test.
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

  /** The path prefix of a support source: compiled, never analyzed. */
  val Withheld = "withheld/"

  // Between a fixture's text and the support it carries. It is no Scala, so a text that carries
  // support and is parsed without `inputs` does not parse.
  private val Carried = "\u0000"

  /** Appended to a fixture's text: declarations it names that the analysis is not supplied. One
    * text carries one support; two packages are written in the block form (`package a { … }`).
    */
  def withheld(support: String): String = Carried + support

  /** The same support as a file of its own, for a source set that also goes to a reader which
    * compiles nothing. That reader has to leave the file out, as `capacity.Rows.rowsOf` does.
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
    val sources =
      inputs
        .filterNot(_.path.startsWith(TastyStubs.Prefix))
        .map(i => i.path -> i.tree.pos.input.text)
    val key = digest(sources)
    // Not `computeIfAbsent`: a compile runs outside the map's locks, so compiles run in parallel.
    val out = Option(compiled.get(key)).getOrElse {
      val result = compile(key, sources)
      compiled.putIfAbsent(key, result)
      result
    }
    out match {
      case None if supplied.sizeIs < inputs.size =>
        val problems = Files.readString(cache.toAbsolutePath.resolve(key).resolve("failed"))
        throw new AssertionError(s"a fixture with withheld support does not compile, $problems")
      case None => supplied
      // The support classes are only what the compiled types are spelled against: a type naming a
      // withheld declaration is written by its name, which the analysis then does not resolve.
      case Some(dir) =>
        val support = List(dir.resolveSibling("support")).filter(Files.isDirectory(_))
        CompiledTypes
          .ascribe(own, CompiledTypes.Classes(List(dir), support ++ libraries))
          ._1 ++ stubs
    }
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

  // The options are part of the key: an entry compiled with other options is not this compile.
  private def digest(sources: List[(String, String)]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(
        (Options.mkString(" ") :: sources.map((p, t) => s"$p\u0000$t"))
          .mkString("\u0001")
          .getBytes(UTF_8)
      )
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
  // compiler's errors, and stands alone where the compiler threw.
  private def scalac(out: Path, cp: List[Path], files: List[String]): List[String] = {
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    val args = List(
      "-d",
      Files.createDirectories(out).toString,
      "-classpath",
      cp.mkString(java.io.File.pathSeparator)
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
  private def compile(key: String, sources: List[(String, String)]): Option[Path] = {
    val root = cache.toAbsolutePath.resolve(key)
    val out = root.resolve("classes")
    if (Files.exists(root.resolve("ok"))) Some(out)
    else if (Files.exists(root.resolve("failed")) || sources.isEmpty) None
    else {
      val staging =
        Files
          .createDirectories(cache.toAbsolutePath)
          .resolve(s"$key.${Thread.currentThread.threadId}")
      val (support, own) = sources.partition(_._1.startsWith(Withheld)) match {
        case (held, fixture) => (held.map(written(staging, _)), fixture.map(written(staging, _)))
      }
      // The support sources alone, then the fixture against their classes.
      def run(cp: List[Path]): List[String] =
        if (support.isEmpty) scalac(staging.resolve("classes"), cp, own)
        else {
          val held = staging.resolve("support")
          scalac(held, cp, support) match {
            case Nil      => scalac(staging.resolve("classes"), held :: cp, own)
            case problems => "in the withheld support:" :: problems
          }
        }
      val problems = if (run(minimal).isEmpty) Nil else run(classpath)
      Files.writeString(
        staging.resolve(if (problems.isEmpty) "ok" else "failed"),
        (asker :: problems).mkString("\n")
      )
      try Files.move(staging, root)
      catch {
        // Another thread compiled the same text first: its entry stands and this copy goes. A
        // compile that timed out may still be writing here, so what cannot be deleted stays.
        case _: java.nio.file.FileAlreadyExistsException |
            _: java.nio.file.DirectoryNotEmptyException =>
          val copy = Files.walk(staging)
          try copy.sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
          catch { case _: java.io.IOException | _: java.io.UncheckedIOException => () }
          finally copy.close()
      }
      Option.when(Files.exists(root.resolve("ok")))(out)
    }
  }

  // One source written below the staging directory's `src`: its file.
  private def written(staging: Path, source: (String, String)): String = {
    val file = staging.resolve("src").resolve(source._1.stripPrefix("/"))
    Files.createDirectories(file.getParent)
    Files.writeString(file, source._2, UTF_8)
    file.toString
  }

}
