package cardinality

import cardinality.loader.{Calculator, Skill}
import java.nio.file.Path as NioPath
import lmcoursier.{CoursierConfiguration, CoursierDependencyResolution}
import sbt.*
import sbt.Keys.{baseDirectory, target}

/** What the two sbt plugins do the same way: one source compiled into each. */
private[cardinality] object CardinalityTool {

  /** The settings both plugins give the shared keys. */
  def defaults(keys: CardinalityKeys): Seq[Setting[?]] = Seq(
    keys.cardinalityVersion := Calculator.version,
    keys.cardinalityReportFile := target.value / "cardinality" / "report.txt",
    keys.cardinalityQuerySupport := Nil,
    keys.cardinalityQueryLimits := Map.empty,
    // Seeding is a repository-level act: the default lands beside the other agent instructions
    // at the build's root, wherever the task is run from.
    keys.cardinalitySkillFile := (ThisBuild / baseDirectory).value / ".claude" / "skills" /
      "cardinality-reduction" / "SKILL.md"
  )

  val paths = Def.spaceDelimited("<path>...")
  val names = Def.spaceDelimited("<qualified-method-name>...")

  /** The jars of `scala-cardinality_3` at `version`, resolved apart from the project: with its
    * repositories, but neither its Scala version (sbt forces a project's own onto the Scala library
    * of what it resolves) nor its own modules, which would evict whatever the project itself builds
    * (cats-eo, in cats-eo's build).
    */
  def jars(project: CoursierConfiguration, version: String, target: File, log: Logger): Seq[File] =
    CoursierDependencyResolution(
      project.withAutoScalaLibrary(false).withInterProjectDependencies(Vector.empty)
    )
      .retrieve(
        "dev.constructive" % "scala-cardinality_3" % version,
        None,
        target / "cardinality" / "tool",
        log
      )
      .fold(warning => throw warning.resolveException, _.toSeq)

  /** The report over `sources`, written to `output` and logged. */
  def report(
      calculator: Calculator,
      sources: Seq[File],
      stubs: Option[NioPath],
      output: File,
      log: Logger
  ): Unit =
    finish(
      output,
      calculator.report(nio(sources), stubs, output.toPath),
      "source(s) could not be read or parsed",
      log
    )

  /** The bounded report for the qualified method names, written to `output` and logged. */
  def selected(
      calculator: Calculator,
      query: Query,
      stubs: Option[NioPath],
      output: File,
      log: Logger
  ): Unit = {
    if (query.names.isEmpty) sys.error("provide at least one qualified method name")
    finish(
      output,
      calculator.selected(
        nio(query.sources),
        nio(query.support),
        query.names,
        stubs,
        query.limits,
        output.toPath
      ),
      "selected report error(s)",
      log
    )
  }

  /** What a selected report asks: the names, over the sources, with their support and limits. */
  final case class Query(
      names: Seq[String],
      sources: Seq[File],
      support: Seq[File],
      limits: Map[String, Long]
  )

  /** Writes the reduction skill this repository's agents use, parameterized by the build and
    * stamped with the plugin's version, not the project's (the template is shared with
    * mill-cardinality, see `Skill`).
    */
  def seedSkill(log: Logger, output: File, buildRoot: File): File = {
    val values = Map(
      "project" -> buildRoot.getName,
      "version" -> Calculator.version,
      "seed" -> "cardinalitySeedSkill"
    )
    val changed =
      try Skill.seed(output.toPath, "sbt-cardinality", "sbt", values)
      catch { case e: IllegalStateException => sys.error(e.getMessage) }
    log.info(
      if (changed) s"seeded the cardinality-reduction skill: $output"
      else s"skill already up to date: $output"
    )
    output
  }

  private def nio(files: Seq[File]): Seq[NioPath] = files.map(_.getAbsoluteFile.toPath)

  // Report-then-fail: a report over sources the calculator could not read is incomplete, and
  // failing quietly would present the numbers it did find as the whole story.
  private def finish(output: File, errors: Int, what: String, log: Logger): Unit = {
    log.info(IO.read(output))
    log.info(s"report written to $output")
    if (errors > 0) sys.error(s"$errors $what")
  }

}
