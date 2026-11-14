package millcardinality

import cardinality.reporting.{Report, Skill}
import cardinality.tasty.CompiledClasses
import mainargs.Leftover
import mill.*
import mill.api.{BuildCtx, Result}
import mill.scalalib.ScalaModule

/** The Mill face of scala-cardinality, the counterpart of the sbt plugin: mix it into a
  * `ScalaModule` and run `mill <module>.cardinalityReport`. Everything it computes is delegated to
  * `core`; each report is written to the command's `Task.dest` as `report.txt`, and
  * `cardinalitySeedSkill` writes the reduction skill into the repository.
  */
trait CardinalityModule extends ScalaModule {

  // The compiled dependencies that exist on disk.
  private def dependencyJars = Task.Anon {
    compileClasspath().map(_.path).filter(os.exists).map(_.toNIO).toList.distinct
  }

  // This module's compiled classes and the classpath their types refer to: the report reads them.
  private def compiled = Task.Anon {
    CompiledClasses(Seq(compile().classes.path.toNIO), dependencyJars())
  }

  /** Report the cardinality of this module's sources. */
  def cardinalityReport(): Command[PathRef] = Task.Command {
    report(Report.of(allSourceFiles().map(_.path.toNIO), Some(compiled())))
  }

  /** Report the cardinality of the Scala sources at the given paths: files, directories, or a
    * sources jar such as the one a published library ships, beside the library's binary jar: its
    * compiled classes are read against this module's compile classpath.
    */
  def cardinalityReportOf(paths: Leftover[String]): Command[PathRef] = Task.Command {
    report(
      Report.of(
        paths.value.map(os.Path(_, mill.api.BuildCtx.workspaceRoot).toNIO),
        Some(CompiledClasses(Nil, dependencyJars()))
      )
    )
  }

  /** The implementation report of the given qualified method names alone. */
  def cardinalityReportSelected(names: Leftover[String]): Command[PathRef] = Task.Command {
    if (names.value.isEmpty) Task.fail("provide at least one qualified method name")
    val found =
      Report.query(allSourceFiles().map(_.path.toNIO), names.value.toSet, Some(compiled()))
    written(found.render, found.report.errors.size, "selected report error(s)")
  }

  /** Where `cardinalitySeedSkill` writes the reduction skill, under the workspace root by default.
    */
  def cardinalitySkillFile: os.Path =
    BuildCtx.workspaceRoot / ".claude" / "skills" / "cardinality-reduction" / "SKILL.md"

  /** Seed the cardinality-reduction skill into this repository: write the template that ships with
    * the plugin, parameterized by the build, to `cardinalitySkillFile`. Refreshes a file the plugin
    * generated; refuses to overwrite one it did not.
    */
  def cardinalitySeedSkill(): Command[PathRef] = Task.Command {
    val output = cardinalitySkillFile
    // Seeding writes into the repository, outside `Task.dest`, which Mill otherwise forbids.
    BuildCtx.withFilesystemCheckerDisabled(
      Skill.seed(Skill.Tool.Mill, output.toNIO, BuildCtx.workspaceRoot.last)
    ) match {
      case Left(error) => Result.Failure(error)
      case Right(done) => Task.log.info(done); Result.Success(PathRef(output))
    }
  }

  // Fails the command when a source could not be read: a report over sources the
  // calculator could not read is incomplete, and failing quietly would present the numbers it did
  // find as the whole story.
  private def report(found: Report)(using mill.api.TaskCtx): Result[PathRef] =
    written(found.render, found.errors.size, "source(s) could not be read")

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
