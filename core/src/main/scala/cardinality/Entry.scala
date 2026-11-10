package cardinality

import scala.jdk.CollectionConverters.*

import cardinality.reporting.Report
import java.nio.file.{Files, Path}
import java.util.List as JList

/** The calculator's entry point for a caller built on another Scala version: the sbt 1 plugin runs
  * on Scala 2.12, and a build tool on a later Scala 3 cannot read this one's classes either. Only
  * JDK types cross it, so such a caller loads this class in a classloader of its own — the
  * calculator's jars over the JDK's platform loader — and calls it reflectively.
  *
  * Each method writes the rendered report to `out` and returns the number of errors it holds.
  */
object Entry {

  /** The report over `sources`, reading the declarations of the compiled `classpath`. */
  def report(sources: JList[Path], classpath: JList[Path], out: Path): Int = {
    val found = Report.of(sources.asScala.toSeq, classpath.asScala.toSeq)
    write(out, found.render)
    found.errors.size
  }

  /** The bounded implementation report for the qualified method `names`. */
  def selected(
      sources: JList[Path],
      support: JList[Path],
      names: JList[String],
      classpath: JList[Path],
      out: Path
  ): Int = {
    val found = Report.query(
      Report.Query(
        sources.asScala.toSeq,
        support.asScala.toSeq,
        names.asScala.toSet,
        classpath = classpath.asScala.toSeq
      )
    )
    write(out, found.render)
    found.report.errors.size
  }

  private def write(out: Path, rendered: String): Unit = {
    Option(out.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
    val _ = Files.writeString(out, rendered)
  }

}
