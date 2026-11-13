package cardinality

import cardinality.analysis.MethodAnalysis
import cardinality.reporting.{Report, Skill}
import cardinality.request.{AnalysisQuery, SourceSnapshot}
import cardinality.tasty.{CompiledTypes, StubFile}
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
        "sources jar such as the one a published library ships.",
    )

    val cardinalityReportSelected =
      inputKey[Unit]("Bounded implementation report for the given qualified method names.")

    val cardinalityQuerySupport =
      settingKey[Seq[File]](
        "Explicit supporting source roots for selected reports; never report targets."
      )

    val cardinalityQueryBudget =
      settingKey[AnalysisQuery.Budget]("Logical indexing and per-target/request work limits.")

    val cardinalityQueryLimits =
      settingKey[MethodAnalysis.Limits]("Type depth and solver state limits for selected reports.")

    val cardinalitySnapshotLimits =
      settingKey[SourceSnapshot.Limits]("Input acquisition limits for selected reports.")

    // Content-hashed references: the cached stub task below is keyed by the jars' contents.
    val cardinalityDependencyClasspath = taskKey[Seq[HashedVirtualFileRef]](
      "Compiled dependencies whose declarations the reports read; `Nil` reads sources only."
    )

    val cardinalityDependencyStubs = taskKey[HashedVirtualFileRef](
      "The dependencies' declarations rendered as stubs: cached by sbt by the jars' contents."
    )

    val cardinalityReportFile =
      settingKey[File](
        "Where a report is written, under the project's target directory by default."
      )

    val cardinalityFrontEnd = settingKey[Report.Front](
      "Which front end reads the sources: `CardinalityFrontEnd.Tasty` reads the compiled classes " +
        "alone and refuses sources that do not compile."
    )

    val CardinalityFrontEnd = Report.Front

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

  // The stub file's format (`StubFile.Format`): an input of the cached stub task, so a renderer
  // whose stubs differ never reads a file an earlier one wrote.
  private val cardinalityStubFormat =
    settingKey[String]("The format of the dependency stub file (internal).")

  override def projectSettings: Seq[Setting[?]] = dependencySettings ++ Seq(
    cardinalityReportFile := target.value / "cardinality" / "report.txt",
    cardinalityFrontEnd := Report.Environment().frontEnd,
    cardinalityQuerySupport := Nil,
    cardinalityQueryBudget := AnalysisQuery.Budget(),
    cardinalityQueryLimits := MethodAnalysis.Limits(),
    cardinalitySnapshotLimits := SourceSnapshot.Limits(),
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
      val stubs = fileConverter.value.toPath(cardinalityDependencyStubs.value)
      report(
        log,
        Report.of(
          scalaSources.map(_.toPath),
          Report.Environment(
            stubs = Some(stubs),
            classes = compiled.value,
            frontEnd = cardinalityFrontEnd.value
          )
        ),
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
            Report.Environment(frontEnd = cardinalityFrontEnd.value)
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
            cardinalityQuerySupport.value.map(_.toPath),
            names.toSet,
            cardinalitySnapshotLimits.value,
            cardinalityQueryLimits.value,
            cardinalityQueryBudget.value,
            Report.Environment(
              stubs = Some(fileConverter.value.toPath(cardinalityDependencyStubs.value)),
              classes = compiled.value,
              frontEnd = cardinalityFrontEnd.value
            )
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

  // The project's compiled classes and the jars their types refer to: a member written without a
  // result type reads the compiler's (`CompiledTypes`). `exportedProducts` compiles first, so the
  // classes match the sources the report reads. Sources that do not compile are still reported
  // from their syntax alone by the scalameta front end, and refused by the TASTy one.
  private val compiled = Def.task {
    val converter = fileConverter.value
    val classpath = cardinalityDependencyClasspath.value.map(converter.toPath)
    val front = cardinalityFrontEnd.value
    (Compile / exportedProducts).result.value.toEither match {
      case Right(products) =>
        Some(CompiledTypes.Classes(products.map(entry => converter.toPath(entry.data)), classpath))
      case Left(_) =>
        streams.value.log.warn(
          if (front == Report.Front.Tasty)
            "sources do not compile: the TASTy front end reads none of them"
          else "sources do not compile: reporting from their syntax alone"
        )
        None
    }
  }

  // The dependency declarations the reports read: which jars, and their stubs rendered once.
  private def dependencySettings: Seq[Setting[?]] = Seq(
    // The project's own Compile dependencies, never `fullClasspath`: dependency paths stay explicit.
    // sbt 2 hands out content-addressed jars, so each entry goes through the file converter.
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.map(_.data),
    // An sbt 2 cached task: keyed by the jars' content hashes (`HashedVirtualFileRef`) and the stub
    // format, its file is restored from the machine-wide cache instead of rendered again.
    cardinalityStubFormat := StubFile.key,
    cardinalityDependencyStubs := {
      val converter = fileConverter.value
      val _ = cardinalityStubFormat.value
      val jars = cardinalityDependencyClasspath.value.map(converter.toPath).toList.distinct
      // Under sbt's output root, which the converter writes as `${OUT}`: the declared file is then
      // a relative reference, restorable on any machine sharing the cache.
      val out = rootOutputDirectory.value.resolve(s"cardinality/${name.value}/stubs.bin")
      StubFile.render(jars, out)
      val file = converter.toVirtualFile(out)
      Def.declareOutput(file)
      file: HashedVirtualFileRef
    }
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
