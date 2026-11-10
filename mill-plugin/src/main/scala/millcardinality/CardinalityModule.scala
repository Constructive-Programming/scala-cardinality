package millcardinality

import cardinality.analysis.MethodAnalysis
import cardinality.reporting.Report
import cardinality.request.*
import cardinality.tasty.StubFile
import mainargs.Leftover
import mill.*
import mill.api.Result
import mill.scalalib.ScalaModule

/** The Mill face of scala-cardinality, the counterpart of the sbt plugin: mix it into a
  * `ScalaModule` and run `mill <module>.cardinalityReport`. Everything it computes is delegated to
  * `core`; each report is written to the command's `Task.dest` as `report.txt`.
  */
trait CardinalityModule extends ScalaModule {

  /** Explicit supporting source roots for selected reports; never report targets. */
  def cardinalityQuerySupport: T[Seq[PathRef]] = Task(Seq.empty[PathRef])

  /** Logical indexing and per-target/request work limits for selected reports. */
  def cardinalityQueryBudget: AnalysisQuery.Budget = AnalysisQuery.Budget()

  /** Type depth and solver state limits for selected reports. */
  def cardinalityQueryLimits: MethodAnalysis.Limits = MethodAnalysis.Limits()

  /** Input acquisition limits for selected reports. */
  def cardinalitySnapshotLimits: SourceSnapshot.Limits = SourceSnapshot.Limits()

  // An input of the cached stub task, so stubs another renderer or JDK wrote are never read.
  def cardinalityStubFormat: T[String] = Task.Input(StubFile.key)

  /** The compiled dependencies' declarations rendered as stubs, cached by their contents. */
  def cardinalityDependencyStubs: T[PathRef] = Task {
    val _ = cardinalityStubFormat()
    val jars = compileClasspath().map(_.path).filter(os.exists).map(_.toNIO).toList.distinct
    val out = Task.dest / "stubs.bin"
    StubFile.render(jars, out.toNIO)
    PathRef(out)
  }

  /** Report the cardinality of this module's sources. */
  def cardinalityReport(): Command[PathRef] = Task.Command {
    val stubs = cardinalityDependencyStubs().path.toNIO
    report(Report.of(allSourceFiles().map(_.path.toNIO), stubs = Some(stubs)))
  }

  /** Report the cardinality of the Scala sources at the given paths: files, directories, or a
    * sources jar such as the one a published library ships.
    */
  def cardinalityReportOf(paths: Leftover[String]): Command[PathRef] = Task.Command {
    report(Report.of(paths.value.map(os.Path(_, mill.api.BuildCtx.workspaceRoot).toNIO)))
  }

  /** Bounded implementation report for the given qualified method names. */
  def cardinalityReportSelected(names: Leftover[String]): Command[PathRef] = Task.Command {
    if (names.value.isEmpty) Task.fail("provide at least one qualified method name")
    val found = Report.query(
      Report.Query(
        allSourceFiles().map(_.path.toNIO),
        cardinalityQuerySupport().map(_.path.toNIO),
        names.value.toSet,
        cardinalitySnapshotLimits,
        cardinalityQueryLimits,
        cardinalityQueryBudget,
        stubs = Some(cardinalityDependencyStubs().path.toNIO)
      )
    )
    written(found.render, found.report.errors.size, "selected report error(s)")
  }

  // Fails the command when a source could not be read or parsed: a report over sources the
  // calculator could not read is incomplete, and failing quietly would present the numbers it did
  // find as the whole story.
  private def report(found: Report)(using mill.api.TaskCtx): Result[PathRef] =
    written(found.render, found.errors.size, "source(s) could not be read or parsed")

  private def written(text: String, errors: Int, what: String)(using
      mill.api.TaskCtx
  ): Result[PathRef] = {
    val output = Task.dest / "report.txt"
    os.write.over(output, text)
    Task.log.info(text)
    Task.log.info(s"report written to $output")
    if (errors > 0) Result.Failure(s"$errors $what") else Result.Success(PathRef(output))
  }

}
