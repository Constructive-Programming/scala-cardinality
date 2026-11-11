package cardinality

import cardinality.loader.Calculator
import sbt.*
import sbt.Keys.*

// The sbt 1 face of scala-cardinality. The calculator is reached through the loader the three
// plugins share (`cardinality.loader.Calculator`): it runs in sbt's JVM, apart from sbt's Scala
// 2.12, so that JVM must be the JDK the calculator needs.
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport extends CardinalityKeys {

    val cardinalityDependencyClasspath = taskKey[Seq[File]](
      "Compiled dependencies whose declarations the reports read; `Nil` reads sources only."
    )

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = CardinalityTool.defaults(autoImport) ++ Seq(
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.files,
    cardinalitySeedSkill := CardinalityTool
      .seedSkill(streams.value.log, cardinalitySkillFile.value, (ThisBuild / baseDirectory).value),
    cardinalityReport := CardinalityTool.report(
      calculator.value,
      (Compile / sources).value,
      Some(stubs.value),
      cardinalityReportFile.value,
      streams.value.log
    ),
    cardinalityReportOf := {
      val sources = CardinalityTool.paths.parsed.map(file)
      CardinalityTool
        .report(calculator.value, sources, None, cardinalityReportFile.value, streams.value.log)
    },
    cardinalityReportSelected := {
      val query = CardinalityTool.Query(
        CardinalityTool.names.parsed,
        (Compile / sources).value,
        cardinalityQuerySupport.value,
        cardinalityQueryLimits.value
      )
      CardinalityTool.selected(
        calculator.value,
        query,
        Some(stubs.value),
        cardinalityReportFile.value,
        streams.value.log
      )
    }
  )

  private def calculator = Def.task {
    val jars = CardinalityTool.jars(
      csrConfiguration.value,
      cardinalityVersion.value,
      target.value,
      streams.value.log
    )
    Calculator.of(jars.map(_.toPath))
  }

  // ponytail: the dependencies' stubs are rendered on every run; sbt 2's plugin caches them by
  // the jars' contents. sbt 1's `FileFunction.cached` would do once class directories hash.
  private def stubs = Def.task {
    val out = (target.value / "cardinality" / "stubs.bin").toPath
    calculator.value.stubs(cardinalityDependencyClasspath.value.map(_.toPath), out)
    out
  }

}
