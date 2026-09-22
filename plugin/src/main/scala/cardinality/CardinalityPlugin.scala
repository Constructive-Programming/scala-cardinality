package cardinality

import java.nio.file.Path
import sbt.*
import sbt.Keys.*
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

    val cardinalityReportFile =
      settingKey[File](
        "Where a report is written, under the project's target directory by default."
      )

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    cardinalityReportFile := target.value / "cardinality" / "report.txt",
    // `Def.uncached`: a report over transient inputs (`sources` is excluded from sbt 2's cache
    // key) must re-run every time, never be served stale from the task cache.
    cardinalityReport := Def.uncached {
      val log = streams.value.log
      val scalaSources = (Compile / sources).value
      report(log, scalaSources.map(_.toPath), cardinalityReportFile.value)
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
        report(log, paths.map(path => file(path).toPath), cardinalityReportFile.value)
      }
    },
  )

  // Writes the report under `target/`, logs it, and fails the task when a source could not be read
  // or parsed: a report over sources the calculator could not read is incomplete, and failing
  // quietly would present the numbers it did find as the whole story. M3's report-then-fail, one
  // milestone early.
  private def report(log: Logger, paths: Seq[Path], output: File): Unit = {
    val found = Report.of(paths)
    IO.write(output, found.render)
    log.info(found.render)
    log.info(s"report written to $output")
    if (found.errors.nonEmpty)
      sys.error(s"${found.errors.size} source(s) could not be read or parsed")
  }

}
