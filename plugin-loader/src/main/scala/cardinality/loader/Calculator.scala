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

  /** Renders the declarations of a compiled classpath into the stub file `out`. */
  def stubs(classpath: Seq[Path], out: Path): Unit = {
    val _ = call("stubs", list(classpath), out)
  }

  /** Writes the report over `sources` to `out`; the number of errors it holds. */
  def report(sources: Seq[Path], stubs: Option[Path], out: Path): Int =
    count(call("report", list(sources), optional(stubs), out))

  /** Writes the bounded report for the qualified method `names`; the number of errors it holds. */
  def selected(
      sources: Seq[Path],
      support: Seq[Path],
      names: Seq[String],
      stubs: Option[Path],
      limits: Map[String, Long],
      out: Path
  ): Int = {
    val overrides = new java.util.HashMap[String, java.lang.Long]
    limits.foreach { case (name, value) => overrides.put(name, value) }
    count(
      call("selected", list(sources), list(support), list(names), optional(stubs), overrides, out)
    )
  }

  private def list[A](values: Seq[A]): java.util.List[A] = {
    val result = new java.util.ArrayList[A]
    values.foreach(result.add(_))
    result
  }

  private def optional(stubs: Option[Path]): Optional[Path] =
    stubs.fold(Optional.empty[Path])(Optional.of(_))

  private def count(result: AnyRef): Int = result.asInstanceOf[java.lang.Integer].intValue

  // The entry point's method of that name, run with the calculator's loader as the thread's.
  private def call(method: String, arguments: AnyRef*): AnyRef = {
    val thread = Thread.currentThread
    val before = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try
      entry.getMethods
        .find(_.getName == method)
        .getOrElse(throw new NoSuchMethodException(s"cardinality.Entry.$method"))
        .invoke(null, arguments*)
    catch { case e: InvocationTargetException => throw e.getCause }
    finally thread.setContextClassLoader(before)
  }

}

object Calculator {

  /** The calculator's version: the plugin's own. */
  val version: String = BuildInfo.version

  /** The oldest JDK the calculator runs on: its bytecode and Scala 3's library are Java 17. */
  val Java = 17

  // One loader per set of jars, kept while the build tool runs: a second report skips loading the
  // calculator again, and its library threads are started once.
  private var loaded: Option[(Seq[(Path, Long)], Calculator)] = None

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
