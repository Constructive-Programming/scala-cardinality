package millcardinality

import cardinality.loader.Calculator
import mainargs.Leftover
import mill.*
import mill.api.Result
import mill.scalalib.*

/** The Mill face of scala-cardinality, the counterpart of the sbt plugins: mix it into a
  * `ScalaModule` and run `mill <module>.cardinalityReport`. Everything it computes is delegated to
  * the calculator, reached through the loader the three plugins share
  * (`cardinality.loader.Calculator`); each report is written to the command's `Task.dest` as
  * `report.txt`.
  */
trait CardinalityModule extends ScalaModule {

  /** The scala-cardinality version the reports run: the plugin's own by default. */
  def cardinalityVersion: T[String] = Task(Calculator.version)

  /** Explicit supporting source roots for selected reports; never report targets. */
  def cardinalityQuerySupport: T[Seq[PathRef]] = Task(Seq.empty[PathRef])

  /** Limits of selected reports that differ from the calculator's defaults, by name:
    * `maxRequestWork`, `maxWorkPerTarget`, `maxTypeDepth`, `maxTotalBytes`, ... (an unknown name is
    * refused with the known ones).
    */
  def cardinalityQueryLimits: T[Map[String, Long]] = Task(Map.empty[String, Long])

  /** The calculator's jars, resolved on their own: never part of the module's classpath, and
    * without the module's dependency mapping, which forces its Scala version onto the Scala library
    * of what it resolves.
    */
  def cardinalityCalculatorJars: T[Seq[PathRef]] = Task {
    defaultResolver().classpath(
      Seq(mvn"dev.constructive:scala-cardinality_3:${cardinalityVersion()}"),
      resolutionParamsMapOpt = Some(_ => coursier.params.ResolutionParams()),
      mapDependencies = Some(identity)
    )
  }

  // The JDK whose classes render too: an input of the cached stub task, with the calculator's
  // jars, so stubs another calculator or JDK rendered are never read.
  def cardinalityJava: T[String] = Task.Input(sys.props("java.version"))

  /** The compiled dependencies' declarations rendered as stubs, cached by their contents. */
  def cardinalityDependencyStubs: T[PathRef] = Task {
    val _ = cardinalityJava()
    val jars = compileClasspath().map(_.path).filter(os.exists).map(_.toNIO).distinct
    val out = Task.dest / "stubs.bin"
    calculator(cardinalityCalculatorJars()).stubs(jars, out.toNIO)
    PathRef(out)
  }

  /** Report the cardinality of this module's sources. */
  def cardinalityReport(): Command[PathRef] = Task.Command {
    val out = Task.dest / "report.txt"
    val errors = calculator(cardinalityCalculatorJars()).report(
      allSourceFiles().map(_.path.toNIO),
      Some(cardinalityDependencyStubs().path.toNIO),
      out.toNIO
    )
    finish(out, errors, "source(s) could not be read or parsed")
  }

  /** Report the cardinality of the Scala sources at the given paths: files, directories, or a
    * sources jar such as the one a published library ships.
    */
  def cardinalityReportOf(paths: Leftover[String]): Command[PathRef] = Task.Command {
    val out = Task.dest / "report.txt"
    val sources = paths.value.map(os.Path(_, mill.api.BuildCtx.workspaceRoot).toNIO)
    val errors = calculator(cardinalityCalculatorJars()).report(sources, None, out.toNIO)
    finish(out, errors, "source(s) could not be read or parsed")
  }

  /** Bounded implementation report for the given qualified method names. */
  def cardinalityReportSelected(names: Leftover[String]): Command[PathRef] = Task.Command {
    if (names.value.isEmpty) Task.fail("provide at least one qualified method name")
    val out = Task.dest / "report.txt"
    val errors = calculator(cardinalityCalculatorJars()).selected(
      allSourceFiles().map(_.path.toNIO),
      cardinalityQuerySupport().map(_.path.toNIO),
      names.value,
      Some(cardinalityDependencyStubs().path.toNIO),
      cardinalityQueryLimits(),
      out.toNIO
    )
    finish(out, errors, "selected report error(s)")
  }

  private def calculator(jars: Seq[PathRef]): Calculator = Calculator.of(jars.map(_.path.toNIO))

  // Fails the command when a source could not be read or parsed: a report over sources the
  // calculator could not read is incomplete, and failing quietly would present the numbers it did
  // find as the whole story.
  private def finish(output: os.Path, errors: Int, what: String)(using
      mill.api.TaskCtx
  ): Result[PathRef] = {
    Task.log.info(os.read(output))
    Task.log.info(s"report written to $output")
    if (errors > 0) Result.Failure(s"$errors $what") else Result.Success(PathRef(output))
  }

}
