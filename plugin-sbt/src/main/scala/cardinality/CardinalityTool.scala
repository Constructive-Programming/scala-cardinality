package cardinality

import cardinality.loader.{Calculator, Skill}
import java.nio.file.Path as NioPath
import lmcoursier.{CoursierConfiguration, CoursierDependencyResolution}
import sbt.*
import sbt.Keys.{baseDirectory, csrConfiguration, streams, target}

/** What the two sbt plugins do the same way: one source compiled into each. */
private[cardinality] object CardinalityTool {

  /** The settings both plugins give the shared keys. */
  def defaults(keys: CardinalityKeys): Seq[Setting[?]] = Seq(
    keys.cardinalityVersion := Calculator.version,
    keys.cardinalityReportFile := target.value / "cardinality" / "report.txt",
    // Seeding is a repository-level act: the default lands beside the other agent instructions
    // at the build's root, wherever the task is run from.
    keys.cardinalitySkillFile := (ThisBuild / baseDirectory).value / ".claude" / "skills" /
      "cardinality-reduction" / "SKILL.md"
  )

  val paths = Def.spaceDelimited("<path>...")
  val names = Def.spaceDelimited("<qualified-method-name>...")

  /** What a report task runs with: the calculator of `cardinalityVersion` (its jars resolved every
    * run, so a republished snapshot is the one loaded), the report file and the task's log.
    */
  def run(keys: CardinalityKeys): Def.Initialize[Task[Run]] = Def.task {
    val log = streams.value.log
    val found = jars(csrConfiguration.value, keys.cardinalityVersion.value, target.value, log)
    Run(Calculator.of(found.map(_.toPath)), keys.cardinalityReportFile.value, log)
  }

  // The jars of `scala-cardinality_3` at `version`, resolved apart from the project: with its
  // repositories, but neither its Scala version (sbt forces a project's own onto the Scala library
  // of what it resolves) nor its own modules, which would evict whatever the project itself builds
  // (cats-eo, in cats-eo's build).
  private def jars(project: CoursierConfiguration, version: String, target: File, log: Logger) =
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

  /** The classes a report reads: the sources' compiled `output` (`None` when they did not compile,
    * and every source is refused), and the dependency `classpath` its types refer to.
    */
  final case class Classes(output: Option[Seq[File]], classpath: Seq[File])

  /** A report task's calculator, the file it writes the report to, and its log. */
  final case class Run(calculator: Calculator, output: File, log: Logger) {

    /** The report over `sources`, written to `output` and logged. */
    def report(sources: Seq[File], classes: Classes): Unit =
      finish(sources, Nil, classes, "source(s) could not be read")

    /** The report of the qualified method `names` alone, written to `output` and logged. */
    def selected(names: Seq[String], sources: Seq[File], classes: Classes): Unit = {
      if (names.isEmpty) sys.error("provide at least one qualified method name")
      finish(sources, names, classes, "selected report error(s)")
    }

    // Report-then-fail: a report over sources the calculator could not read is incomplete, and
    // failing quietly would present the numbers it did find as the whole story.
    private def finish(sources: Seq[File], names: Seq[String], classes: Classes, what: String) = {
      val errors = calculator.report(
        nio(sources),
        names,
        Calculator.Classes(classes.output.map(nio), nio(classes.classpath)),
        output.toPath
      )
      log.info(IO.read(output))
      log.info(s"report written to $output")
      if (errors > 0) sys.error(s"$errors $what")
    }

  }

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

}
