package cardinality

import sbt.*

/** The keys the sbt 2 plugin (`plugin`) and the sbt 1 plugin (`sbt1-plugin`) both offer. One source
  * compiled into each, so a build moves between sbt 1 and sbt 2 without renaming a key.
  */
trait CardinalityKeys {

  val cardinalityReport =
    taskKey[Unit]("Report the cardinality of the Scala sources of this project.")

  val cardinalityReportOf = inputKey[Unit](
    "Report the cardinality of the Scala sources at the given paths: files, directories, or a " +
      "sources jar such as the one a published library ships."
  )

  val cardinalityReportSelected =
    inputKey[Unit]("Bounded implementation report for the given qualified method names.")

  val cardinalityQuerySupport = settingKey[Seq[File]](
    "Explicit supporting source roots for selected reports; never report targets."
  )

  val cardinalityQueryLimits = settingKey[Map[String, Long]](
    "Limits of selected reports that differ from the calculator's defaults, by name: " +
      "maxRequestWork, maxWorkPerTarget, maxTypeDepth, maxTotalBytes, ... (an unknown name is " +
      "refused with the known ones)."
  )

  val cardinalityReportFile = settingKey[File](
    "Where a report is written, under the project's target directory by default."
  )

  val cardinalityVersion = settingKey[String](
    "The scala-cardinality version the reports run: the plugin's own by default."
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
