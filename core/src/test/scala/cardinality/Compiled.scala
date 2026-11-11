package cardinality

import cardinality.analysis.MethodAnalysis
import cardinality.tasty.{CompiledTypes, TastyStubs}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap

/** Test sources as the build plugins read a module: compiled first, so every member, local, pattern
  * name and lambda parameter written without a type has the compiler's (`CompiledTypes`, rule T). A
  * fixture the compiler rejects (one naming what is deliberately not supplied) is read as written.
  * Compilations are memoised by the sources' text, in memory and on disk under
  * `target/compiled-fixtures`, so a rerun compiles only what changed.
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

  /** The inputs with the compiler's types written in, where they compile. */
  def ascribed(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Input] = {
    val (stubs, own) = inputs.partition(_.path.startsWith(TastyStubs.Prefix))
    val sources = own.map(i => i.path -> i.tree.pos.input.text)
    val key = digest(sources)
    // Not `computeIfAbsent`: a compile runs outside the map's locks, so compiles run in parallel.
    val out = Option(compiled.get(key)).getOrElse {
      val result = compile(key, sources)
      compiled.putIfAbsent(key, result)
      result
    }
    out.fold(inputs)(dir =>
      CompiledTypes.ascribe(own, CompiledTypes.Classes(List(dir), libraries))._1 ++ stubs
    )
  }

  def analyze(
      inputs: List[MethodAnalysis.Input],
      limits: MethodAnalysis.Limits = MethodAnalysis.Limits()
  ): List[MethodAnalysis.Entry] = MethodAnalysis.analyze(ascribed(inputs), limits)

  def analyzeOpen(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyzeOpen(ascribed(inputs))

  private def digest(sources: List[(String, String)]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(sources.map((p, t) => s"$p\u0000$t").mkString("\u0001").getBytes(UTF_8))
      .map("%02x".format(_))
      .mkString

  // A compile the compiler does not finish (a fixture of 70 aliases and a partial alias application
  // keeps 3.9.0's typer busy) counts as rejected: the fixture is read as written.
  private val Limit = 60000L

  // `body` on a daemon thread, false unless it answers true within `millis`.
  private def within(millis: Long)(body: => Boolean): Boolean = {
    @volatile var answer = false
    val thread = new Thread(() => answer = body, "fixture-compile")
    thread.setDaemon(true)
    thread.start()
    thread.join(millis)
    !thread.isAlive && answer
  }

  // Compiled once per text: `classes` holds a successful compile, `failed` marks a rejected one.
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
      val classes = Files.createDirectories(staging.resolve("classes"))
      val files = sources.map { (path, text) =>
        val file = staging.resolve("src").resolve(path.stripPrefix("/"))
        Files.createDirectories(file.getParent)
        Files.writeString(file, text, UTF_8)
        file.toString
      }
      def run(cp: List[Path]) = {
        val args = List(
          "-d",
          classes.toString,
          "-classpath",
          cp.mkString(java.io.File.pathSeparator),
          "-nowarn"
        ) ++ files
        within(Limit)(
          !dotty.tools.dotc.Main
            .process(args.toArray, new dotty.tools.dotc.reporting.StoreReporter())
            .hasErrors
        )
      }
      val ok = run(minimal) || run(classpath)
      Files.writeString(staging.resolve(if (ok) "ok" else "failed"), "")
      try Files.move(staging, root)
      catch { case _: java.nio.file.FileAlreadyExistsException => () }
      Option.when(Files.exists(root.resolve("ok")))(out)
    }
  }

}
