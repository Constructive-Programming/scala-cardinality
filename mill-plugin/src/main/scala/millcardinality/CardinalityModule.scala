package millcardinality

import cardinality.loader.{Calculator, Skill}
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

  // The compiled dependencies that exist on disk.
  private def dependencyJars = Task.Anon {
    compileClasspath().map(_.path).filter(os.exists).map(_.toNIO).distinct
  }

  // This module's compiled classes, which the report reads against `dependencyJars`.
  private def ownClasses = Task.Anon(Some(Seq(compile().classes.path.toNIO)))

  /** Report the cardinality of this module's sources, read from their compiled classes. */
  def cardinalityReport(): Command[PathRef] = Task.Command {
    report(
      cardinalityCalculatorJars(),
      allSourceFiles().map(_.path.toNIO),
      Nil,
      Classes(ownClasses(), dependencyJars()),
      "source(s) could not be read"
    )
  }

  /** Report the cardinality of the Scala sources at the given paths: files, directories, or a
    * sources jar such as the one a published library ships, beside the library's binary jar: its
    * compiled classes are read against this module's compile classpath.
    */
  def cardinalityReportOf(paths: Leftover[String]): Command[PathRef] = Task.Command {
    report(
      cardinalityCalculatorJars(),
      paths.value.map(os.Path(_, mill.api.BuildCtx.workspaceRoot).toNIO),
      Nil,
      Classes(Some(Nil), dependencyJars()),
      "source(s) could not be read"
    )
  }

  /** The implementation report of the given qualified method names alone. */
  def cardinalityReportSelected(names: Leftover[String]): Command[PathRef] = Task.Command {
    if (names.value.isEmpty) Task.fail("provide at least one qualified method name")
    report(
      cardinalityCalculatorJars(),
      allSourceFiles().map(_.path.toNIO),
      names.value,
      Classes(ownClasses(), dependencyJars()),
      "selected report error(s)"
    )
  }

  /** Where `cardinalitySeedSkill` writes the reduction skill: under the build's root. */
  def cardinalitySkillFile: T[os.Path] = Task.Input(
    mill.api.BuildCtx.workspaceRoot / ".claude" / "skills" / "cardinality-reduction" / "SKILL.md"
  )

  /** Seed the cardinality-reduction skill into this repository: the template that ships with the
    * plugin, with this module's commands. Refreshes a file the plugin generated; refuses to
    * overwrite one it did not.
    */
  def cardinalitySeedSkill(): Command[PathRef] = Task.Command {
    val module = moduleSegments.render
    val values = Map(
      "project" -> mill.api.BuildCtx.workspaceRoot.last,
      "version" -> Calculator.version,
      "seed" -> s"$module.cardinalitySeedSkill",
      "module" -> module
    )
    val output = cardinalitySkillFile()
    val changed =
      try Skill.seed(output.toNIO, "mill-cardinality", "mill", values)
      catch { case e: IllegalStateException => Task.fail(e.getMessage) }
    Task.log.info(
      if (changed) s"seeded the cardinality-reduction skill: $output"
      else s"skill already up to date: $output"
    )
    PathRef(output)
  }

  private def calculator(jars: Seq[PathRef]): Calculator = Calculator.of(jars.map(_.path.toNIO))

  // The classes a report reads: the sources' compiled output (`None`: every source is refused) and
  // the dependency classpath its types refer to.
  final private case class Classes(
      output: Option[Seq[java.nio.file.Path]],
      classpath: Seq[java.nio.file.Path]
  )

  // Writes the report to the command's `Task.dest` and fails the command when it holds errors: a
  // report over sources the calculator could not read is incomplete, and failing quietly would
  // present the numbers it did find as the whole story.
  private def report(
      jars: Seq[PathRef],
      sources: Seq[java.nio.file.Path],
      names: Seq[String],
      classes: Classes,
      what: String
  )(using mill.api.TaskCtx): Result[PathRef] = {
    val output = Task.dest / "report.txt"
    val errors =
      calculator(jars).report(sources, names, classes.output, classes.classpath, output.toNIO)
    Task.log.info(os.read(output))
    Task.log.info(s"report written to $output")
    if (errors > 0) Result.Failure(s"$errors $what") else Result.Success(PathRef(output))
  }

}
