package cardinality

import scala.collection.JavaConverters.*

import java.lang.reflect.InvocationTargetException
import java.net.URLClassLoader
import sbt.*
import sbt.Keys.*

// The sbt 1 face of scala-cardinality. sbt 1 runs on Scala 2.12 and cannot load `core` (Scala 3)
// beside its own classes, so this plugin resolves `scala-cardinality_3` on its own — outside the
// project's `update`, which would evict whatever the project itself builds (cats-eo, in cats-eo's
// build) — and loads it in a classloader apart from sbt's (`Calculator`). The calculator runs in
// sbt's JVM, which must therefore be the JDK it needs.
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport extends CardinalityKeys {

    val cardinalityDependencyClasspath = taskKey[Seq[File]](
      "Compiled dependencies whose declarations the reports read; `Nil` reads sources only."
    )

    val cardinalityVersion =
      settingKey[String]("The scala-cardinality version the reports run.")

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    cardinalityVersion := BuildInfo.version,
    cardinalityReportFile := target.value / "cardinality" / "report.txt",
    cardinalityQuerySupport := Nil,
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.files,
    // ponytail: dependency stubs are rendered on every run; sbt 2's plugin caches them by jar hash.
    cardinalityReport := {
      val out = cardinalityReportFile.value
      val errors = calculator.value.call(
        "report",
        paths((Compile / sources).value),
        paths(cardinalityDependencyClasspath.value),
        out.toPath
      )
      finish(out, errors, "source(s) could not be read or parsed", streams.value.log)
    },
    cardinalityReportOf := {
      val sources = Def.spaceDelimited("<path>...").parsed.map(file)
      val out = cardinalityReportFile.value
      val errors = calculator.value.call("report", paths(sources), paths(Nil), out.toPath)
      finish(out, errors, "source(s) could not be read or parsed", streams.value.log)
    },
    cardinalityReportSelected := {
      val names = Def.spaceDelimited("<qualified-method-name>...").parsed
      if (names.isEmpty) sys.error("provide at least one qualified method name")
      val out = cardinalityReportFile.value
      val errors = calculator.value.call(
        "selected",
        paths((Compile / sources).value),
        paths(cardinalityQuerySupport.value),
        names.asJava,
        paths(cardinalityDependencyClasspath.value),
        out.toPath
      )
      finish(out, errors, "selected report error(s)", streams.value.log)
    }
  )

  private def paths(files: Seq[File]): java.util.List[java.nio.file.Path] =
    files.map(_.getAbsoluteFile.toPath).asJava

  // Report-then-fail, as in the sbt 2 plugin: a report over sources the calculator could not read
  // is incomplete, and failing quietly would present the numbers it did find as the whole story.
  private def finish(output: File, errors: Int, what: String, log: Logger): Unit = {
    log.info(IO.read(output))
    log.info(s"report written to $output")
    if (errors > 0) sys.error(s"$errors $what")
  }

  private def calculator = Def.task {
    val java = sys.props("java.specification.version").split('.').last.toInt
    if (java < Calculator.Java)
      sys.error(
        s"scala-cardinality runs inside sbt and needs JDK ${Calculator.Java} or later: " +
          s"sbt is running on JDK $java (${sys.props("java.home")})"
      )
    val jars = dependencyResolution.value
      .retrieve(
        "dev.constructive" % "scala-cardinality_3" % cardinalityVersion.value,
        None,
        target.value / "cardinality" / "tool",
        streams.value.log
      )
      .fold(warning => throw warning.resolveException, _.toSeq)
    Calculator.of(jars)
  }

}

// The calculator, loaded apart from sbt: its jars over the JDK's platform loader, so sbt's Scala
// 2.12 library and the calculator's Scala 3 one never meet and only JDK types are shared. Every
// call goes through `cardinality.Entry`, whose signatures hold JDK types alone.
final private class Calculator(jars: Seq[File]) {

  private val loader =
    new URLClassLoader(jars.map(_.toURI.toURL).toArray, ClassLoader.getPlatformClassLoader)

  private val entry = loader.loadClass("cardinality.Entry")

  /** Runs the entry point's method of that name; the number of errors in the report it wrote. */
  def call(method: String, arguments: AnyRef*): Int = {
    val thread = Thread.currentThread
    val before = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try
      entry.getMethods
        .find(_.getName == method)
        .getOrElse(sys.error(s"cardinality.Entry has no method $method"))
        .invoke(null, arguments*)
        .asInstanceOf[Int]
    catch { case e: InvocationTargetException => throw e.getCause }
    finally thread.setContextClassLoader(before)
  }

}

private object Calculator {

  /** The JDK the calculator's dependencies are built for (kyo: Java 25 bytecode). */
  val Java = 25

  // One loader per set of jars, kept for the sbt session: a second report skips loading the
  // calculator again, and its library threads are started once.
  private var loaded: Option[(Seq[(File, Long)], Calculator)] = None

  def of(jars: Seq[File]): Calculator = synchronized {
    val key = jars.map(jar => jar -> jar.lastModified)
    loaded.collect { case (`key`, calculator) => calculator }.getOrElse {
      val calculator = new Calculator(jars)
      loaded = Some(key -> calculator)
      calculator
    }
  }

}
