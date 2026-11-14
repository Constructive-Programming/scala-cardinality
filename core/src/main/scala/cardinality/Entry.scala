package cardinality

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import cardinality.reporting.Report
import cardinality.tasty.CompiledClasses
import java.nio.file.{Files, Path}
import java.util.{List as JList, Optional}

/** The calculator's entry point for the build-tool plugins. Each plugin is built for its tool's
  * Scala (sbt 1: 2.12; sbt 2 and Mill: whichever Scala 3 they run), which need not read this one's
  * classes. Only JDK types cross this object, so a plugin loads it in a classloader of its own —
  * the calculator's jars over the JDK's platform loader — and calls it reflectively.
  *
  * `output` is the sources' compiled classes, read against the dependency `classpath`
  * (`Report.of`'s `CompiledClasses`); empty, the sources did not compile and every one is refused.
  * A report is written to `out`; the result is the number of errors it holds.
  */
object Entry {

  /** The report over `sources`, read from their compiled classes; with `names`, the implementation
    * report of those qualified methods alone (`Report.query`).
    */
  def report(
      sources: JList[Path],
      names: JList[String],
      output: Optional[JList[Path]],
      classpath: JList[Path],
      out: Path
  ): Int = {
    val paths = sources.asScala.toSeq
    val classes =
      output.toScala.map(own => CompiledClasses(own.asScala.toSeq, classpath.asScala.toSeq))
    val (rendered, errors) =
      if (names.isEmpty) {
        val found = Report.of(paths, classes)
        (found.render, found.errors.size)
      } else {
        val found = Report.query(paths, names.asScala.toSet, classes)
        (found.render, found.report.errors.size)
      }
    Option(out.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
    val _ = Files.writeString(out, rendered)
    errors
  }

}
