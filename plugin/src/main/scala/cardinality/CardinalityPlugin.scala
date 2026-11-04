package cardinality

import cardinality.analysis.MethodAnalysis
import cardinality.reporting.Report
import cardinality.request.{AnalysisQuery, SourceSnapshot}
import cardinality.tasty.{StubFile, TastyStubs}
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

  }

  import autoImport.*

  // The stub file's format (`StubFile.Format`): an input of the cached stub task, so a renderer
  // whose stubs differ never reads a file an earlier one wrote.
  private val cardinalityStubFormat =
    settingKey[String]("The format of the dependency stub file (internal).")

  override def projectSettings: Seq[Setting[?]] = Seq(
    cardinalityReportFile := target.value / "cardinality" / "report.txt",
    cardinalityQuerySupport := Nil,
    cardinalityQueryBudget := AnalysisQuery.Budget(),
    cardinalityQueryLimits := MethodAnalysis.Limits(),
    cardinalitySnapshotLimits := SourceSnapshot.Limits(),
    // The project's own Compile dependencies, never `fullClasspath`: dependency paths stay explicit.
    // sbt 2 hands out content-addressed jars, so each entry goes through the file converter.
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.map(_.data),
    // An sbt 2 cached task: keyed by the jars' content hashes (`HashedVirtualFileRef`) and the stub
    // format, its file is restored from the machine-wide cache instead of rendered again.
    cardinalityStubFormat := StubFile.Format,
    cardinalityDependencyStubs := {
      val converter = fileConverter.value
      val _ = cardinalityStubFormat.value
      val jars = cardinalityDependencyClasspath.value.map(converter.toPath).toList.distinct
      // Under sbt's output root, which the converter writes as `${OUT}`: the declared file is then
      // a relative reference, restorable on any machine sharing the cache.
      val out = rootOutputDirectory.value.resolve(s"cardinality/${name.value}/stubs.bin")
      val rendered =
        if (jars.isEmpty) TastyStubs.Rendered(Nil)
        else
          try TastyStubs.render(jars, _ => true)
          catch {
            case scala.util.control.NonFatal(error) =>
              TastyStubs.Rendered(Nil, List(s"dependency classpath not read: $error"))
          }
      StubFile.write(rendered, out)
      val file = converter.toVirtualFile(out)
      Def.declareOutput(file)
      file: HashedVirtualFileRef
    },
    // `Def.uncached`: a report over transient inputs (`sources` is excluded from sbt 2's cache
    // key) must re-run every time, never be served stale from the task cache.
    cardinalityReport := Def.uncached {
      val log = streams.value.log
      val scalaSources = (Compile / sources).value
      val stubs = fileConverter.value.toPath(cardinalityDependencyStubs.value)
      report(
        log,
        Report.of(scalaSources.map(_.toPath), stubs = Some(stubs)),
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
        report(log, Report.of(paths.map(path => file(path).toPath)), cardinalityReportFile.value)
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
            stubs = Some(fileConverter.value.toPath(cardinalityDependencyStubs.value))
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

}
