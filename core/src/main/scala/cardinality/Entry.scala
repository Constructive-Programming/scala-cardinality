package cardinality

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import cardinality.analysis.MethodAnalysis
import cardinality.reporting.Report
import cardinality.request.{AnalysisQuery, SourceSnapshot}
import cardinality.tasty.StubFile
import java.nio.file.{Files, Path}
import java.util.{List as JList, Map as JMap, Optional}

/** The calculator's entry point for the build-tool plugins. Each plugin is built for its tool's
  * Scala (sbt 1: 2.12; sbt 2 and Mill: whichever Scala 3 they run), which need not read this one's
  * classes. Only JDK types cross this object, so a plugin loads it in a classloader of its own —
  * the calculator's jars over the JDK's platform loader — and calls it reflectively.
  *
  * A report is written to `out`; the result is the number of errors it holds.
  */
object Entry {

  /** Renders the declarations of a compiled classpath into the stub file `out`. */
  def stubs(classpath: JList[Path], out: Path): Unit =
    StubFile.render(classpath.asScala.toList, out)

  /** The report over `sources`, reading dependency declarations from a stub file. */
  def report(sources: JList[Path], stubs: Optional[Path], out: Path): Int = {
    val found = Report.of(sources.asScala.toSeq, stubs = stubs.toScala)
    write(out, found.render)
    found.errors.size
  }

  /** The bounded implementation report for the qualified method `names`. `limits` overrides the
    * defaults of `SourceSnapshot.Limits`, `MethodAnalysis.Limits` and `AnalysisQuery.Budget` by
    * field name; a name none of them has is refused.
    */
  def selected(
      sources: JList[Path],
      support: JList[Path],
      names: JList[String],
      stubs: Optional[Path],
      limits: JMap[String, java.lang.Long],
      out: Path
  ): Int = {
    val set = new Overrides(limits.asScala.view.mapValues(_.toLong).toMap)
    val query = Report.Query(
      sources.asScala.toSeq,
      support.asScala.toSeq,
      names.asScala.toSet,
      capture(set),
      solver(set),
      budget(set),
      stubs = stubs.toScala
    )
    if (set.unknown.nonEmpty)
      throw new IllegalArgumentException(
        s"unknown limit(s) ${set.unknown.toList.sorted.mkString(", ")}; " +
          s"known: ${set.known.toList.sorted.mkString(", ")}"
      )
    val found = Report.query(query)
    write(out, found.render)
    found.report.errors.size
  }

  private def capture(set: Overrides): SourceSnapshot.Limits = {
    val default = SourceSnapshot.Limits()
    SourceSnapshot.Limits(
      set.int("maxFiles", default.maxFiles),
      set.long("maxFileBytes", default.maxFileBytes),
      set.long("maxTotalBytes", default.maxTotalBytes),
      set.int("maxArchiveEntries", default.maxArchiveEntries),
      set.int("maxDirectoryEntries", default.maxDirectoryEntries),
      set.long("maxArchiveBytes", default.maxArchiveBytes),
      set.int("maxRoots", default.maxRoots)
    )
  }

  private def solver(set: Overrides): MethodAnalysis.Limits = {
    val default = MethodAnalysis.Limits()
    MethodAnalysis.Limits(
      set.int("maxTypeDepth", default.maxTypeDepth),
      set.int("maxStates", default.maxStates)
    )
  }

  private def budget(set: Overrides): AnalysisQuery.Budget = {
    val default = AnalysisQuery.Budget()
    AnalysisQuery.Budget(
      set.long("maxIndexWork", default.maxIndexWork),
      set.int("maxTargets", default.maxTargets),
      set.long("maxWorkPerTarget", default.maxWorkPerTarget),
      set.long("maxRequestWork", default.maxRequestWork),
      set.int("maxTreeDepth", default.maxTreeDepth)
    )
  }

  // The limits a caller set, read by name; the names read are the known ones.
  final private class Overrides(values: Map[String, Long]) {
    val known: mutable.Set[String] = mutable.Set.empty

    def long(name: String, default: Long): Long = {
      known += name
      values.getOrElse(name, default)
    }

    def int(name: String, default: Int): Int = Math.toIntExact(long(name, default.toLong))

    def unknown: Set[String] = values.keySet.diff(known)
  }

  private def write(out: Path, rendered: String): Unit = {
    Option(out.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
    val _ = Files.writeString(out, rendered)
  }

}
