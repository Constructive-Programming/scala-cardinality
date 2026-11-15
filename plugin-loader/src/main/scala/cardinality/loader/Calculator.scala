package cardinality.loader

import java.lang.reflect.InvocationTargetException
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.Optional

/** The calculator (`scala-cardinality`), loaded apart from the build tool: its jars over the JDK's
  * platform loader, so the tool's Scala library and the calculator's never meet and only JDK types
  * are shared. A plugin is then built for its tool's Scala, whatever the calculator's is. Every
  * call goes through `cardinality.Entry`, whose signatures hold JDK types alone.
  *
  * One source compiled into each plugin (sbt 1 on Scala 2.12, sbt 2 and Mill on Scala 3), so it
  * keeps to the syntax and library the two share.
  */
final class Calculator private (jars: Seq[Path]) {

  private val loader =
    new URLClassLoader(jars.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)

  private val entry = loader.loadClass("cardinality.Entry")

  /** Writes the report over `sources` to `out` (with `names`, the report of those qualified methods
    * alone), read from their compiled `classes`; the number of errors it holds.
    */
  def report(sources: Seq[Path], names: Seq[String], classes: Calculator.Classes, out: Path): Int =
    count(call("report", list(sources), list(names), compiled(classes), out))

  private def list[A](values: Seq[A]): java.util.List[A] = {
    val result = new java.util.ArrayList[A]
    values.foreach(result.add(_))
    result
  }

  private def compiled(classes: Calculator.Classes) =
    Optional.ofNullable(
      classes.output.map(paths => java.util.Map.entry(list(paths), list(classes.classpath))).orNull
    )

  private def count(result: AnyRef): Int = result.asInstanceOf[java.lang.Integer].intValue

  // The entry point's method of that name, run with the calculator's loader as the thread's. One
  // call at a time in the JVM: a report holds its sources' and its dependencies' compiled trees, and
  // a build tool running one per module in parallel (a root `cardinalityReport`) would hold them all
  // at once and exhaust the heap that each report alone fits in.
  // ponytail: a JVM-wide lock, so a waiting task keeps its worker thread; a build-tool tag would
  // free it, at one per plugin.
  private def call(method: String, arguments: AnyRef*): AnyRef = Calculator.running.synchronized {
    val thread = Thread.currentThread
    val before = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try
      entry.getMethods
        .find(m => m.getName == method && m.getParameterCount == arguments.size)
        .getOrElse(
          throw new NoSuchMethodException(
            s"cardinality.Entry.$method with ${arguments.size} parameters: the calculator " +
              s"(cardinalityVersion) is not one this plugin (${Calculator.version}) can call"
          )
        )
        .invoke(null, arguments*)
    catch { case e: InvocationTargetException => throw e.getCause }
    finally thread.setContextClassLoader(before)
  }

}

object Calculator {

  /** The classes a report reads: the sources' compiled `output` (`None` when they did not compile,
    * and every source is refused), and the dependency `classpath` its types refer to.
    */
  final case class Classes(output: Option[Seq[Path]], classpath: Seq[Path])

  /** The calculator's version: the plugin's own. */
  val version: String = BuildInfo.version

  /** The oldest JDK the calculator runs on: its bytecode and Scala 3's library are Java 17. */
  val Java = 17

  // One loader per set of jars, kept while the build tool runs: a second report skips loading the
  // calculator again, and its library threads are started once.
  private var loaded: Option[(Seq[(Path, Long)], Calculator)] = None

  // Held while a calculator runs: one report at a time, whichever loader.
  private val running = new Object

  /** The calculator of these jars; refused where the build tool's JVM cannot run it. */
  def of(jars: Seq[Path]): Calculator = synchronized {
    val java = sys.props("java.specification.version").split('.').last.toInt
    if (java < Java)
      throw new IllegalStateException(
        s"scala-cardinality runs inside the build tool and needs JDK $Java or later: " +
          s"this one is JDK $java (${sys.props("java.home")})"
      )
    val key = jars.map(jar => jar -> Files.getLastModifiedTime(jar).toMillis)
    loaded.collect { case (`key`, calculator) => calculator }.getOrElse {
      val calculator = new Calculator(jars)
      loaded = Some(key -> calculator)
      calculator
    }
  }

}
