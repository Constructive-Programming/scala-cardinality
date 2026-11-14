package cardinality

import cardinality.reporting.{Report, Skill}
import cardinality.tasty.CompiledTypes
import sbt.*
import sbt.Keys.*
import sbt.util.CacheImplicits.given
import sbt.util.Logger

// The sbt face of scala-cardinality. Triggered automatically so a build only has to add the plugin
// to `project/plugins.sbt`; everything it computes is delegated to the `core` module, where the
// coverage and mutation gates bite.
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val cardinalityReport =
      taskKey[Unit]("Report the cardinality of the Scala sources of this project.")

    val cardinalityReportOf = inputKey[Unit](
      "Report the cardinality of the Scala sources at the given paths: files, directories, or a " +
        "sources jar such as the one a published library ships, given beside the library's " +
        "binary jar, whose compiled classes are read.",
    )

    val cardinalityReportSelected =
      inputKey[Unit]("The implementation report of the given qualified method names alone.")

    // Content-hashed references, as sbt 2 hands out the dependency jars.
    val cardinalityDependencyClasspath = taskKey[Seq[HashedVirtualFileRef]](
      "Compiled dependencies the project's compiled classes are read against."
    )

    val cardinalityReportFile =
      settingKey[File](
        "Where a report is written, under the project's target directory by default."
      )

    val cardinalitySkillFile = settingKey[File](
      "Where cardinalitySeedSkill writes the reduction skill, under the build's root by default."
    )

    val cardinalitySeedSkill = taskKey[File](
      "Seed the cardinality-reduction skill into this repository: write the template that ships " +
        "with the plugin, parameterized by the build, to cardinalitySkillFile. Refreshes a file " +
        "the plugin generated; refuses to overwrite one it did not."
    )

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = dependencySettings ++ Seq(
    cardinalityReportFile := target.value / "cardinality" / "report.txt",
    // Seeding is a repository-level act: the default lands beside the other agent instructions
    // at the build's root, wherever the task is run from.
    cardinalitySkillFile := (ThisBuild / baseDirectory).value / ".claude" / "skills" /
      "cardinality-reduction" / "SKILL.md",
    // `Def.uncached`: seeding must re-check the target file every run, never be served stale.
    cardinalitySeedSkill := Def.uncached {
      seedSkill(
        streams.value.log,
        cardinalitySkillFile.value,
        (ThisBuild / baseDirectory).value
      )
    },
    // `Def.uncached`: a report over transient inputs (`sources` is excluded from sbt 2's cache
    // key) must re-run every time, never be served stale from the task cache.
    cardinalityReport := Def.uncached {
      val log = streams.value.log
      val scalaSources = (Compile / sources).value
      report(
        log,
        Report.of(scalaSources.map(_.toPath), Report.Environment(compiled.value)),
        cardinalityReportFile.value
      )
    },
    // The same report over sources the build does not compile itself — a library's published
    // sources jar, or another project's checkout — so that a dependency's types can be measured
    // without vendoring them into this build.
    // `.parsed` in the setting body is what makes this an input task. Written through
    // `Def.inputTask` instead, the key is registered as a plain task and sbt rejects the arguments.
    cardinalityReportOf := {
      val log = streams.value.log
      val paths = Def.spaceDelimited("<path>...").parsed
      Def.uncached {
        report(
          log,
          Report.of(
            paths.map(path => file(path).toPath),
            Report.Environment(Some(libraryClasses.value))
          ),
          cardinalityReportFile.value
        )
      }
    },
    cardinalityReportSelected := {
      val names = Def.spaceDelimited("<qualified-method-name>...").parsed
      Def.uncached {
        if (names.isEmpty) sys.error("provide at least one qualified method name")
        val found = Report.query(
          Report.Query(
            (Compile / sources).value.map(_.toPath),
            names.toSet,
            Report.Environment(compiled.value)
          )
        )
        val output = cardinalityReportFile.value
        IO.write(output, found.render)
        streams.value.log.info(found.render)
        streams.value.log.info(s"report written to $output")
        if (found.report.errors.nonEmpty)
          sys.error(s"${found.report.errors.size} selected report error(s)")
      }
    },
  )

  // The project's compiled classes and the jars their types refer to, which the report reads.
  // `exportedProducts` compiles first, so the classes match the sources the report reads. Sources
  // that do not compile hand no classes, and the report refuses every one of them.
  private val compiled = Def.task {
    val converter = fileConverter.value
    val classpath = cardinalityDependencyClasspath.value.map(converter.toPath)
    (Compile / exportedProducts).result.value.toEither match {
      case Right(products) =>
        Some(CompiledTypes.Classes(products.map(entry => converter.toPath(entry.data)), classpath))
      case Left(_) =>
        streams.value.log.warn("sources do not compile: the report reads none of them")
        None
    }
  }

  // What a binary jar given to `cardinalityReportOf` is read against: the project's dependencies.
  private val libraryClasses = Def.task {
    CompiledTypes.Classes(Nil, cardinalityDependencyClasspath.value.map(fileConverter.value.toPath))
  }

  // The project's own Compile dependencies, never `fullClasspath`: dependency paths stay explicit.
  // sbt 2 hands out content-addressed jars, so each entry goes through the file converter.
  private def dependencySettings: Seq[Setting[?]] = Seq(
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.map(_.data)
  )

  // Writes the report under `target/`, logs it, and fails the task when a source could not be read
  // or parsed: a report over sources the calculator could not read is incomplete, and failing
  // quietly would present the numbers it did find as the whole story. M3's report-then-fail, one
  // milestone early.
  private def report(log: Logger, found: Report, output: File): Unit = {
    IO.write(output, found.render)
    log.info(found.render)
    log.info(s"report written to $output")
    if (found.errors.nonEmpty)
      sys.error(s"${found.errors.size} source(s) could not be read or parsed")
  }

  // Writes the reduction skill this repository's agents use, parameterized by the build, so a
  // project that adopts the plugin can seed the same workflow (`Skill.seed` owns the ownership
  // rule: refresh a generated file, refuse a user's).
  private def seedSkill(log: Logger, output: File, buildRoot: File): File =
    Skill.seed(Skill.Tool.Sbt, output.toPath, buildRoot.getName) match {
      case Left(error) => sys.error(error)
      case Right(done) => log.info(done); output
    }

}
