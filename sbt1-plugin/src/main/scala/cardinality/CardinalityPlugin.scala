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

  private lazy val calculator = Def.task(Calculator.of(calculatorJars.value.map(_.toPath)))

  private lazy val calculatorJars = Def.task {
    CardinalityTool.jars(
      csrConfiguration.value,
      cardinalityVersion.value,
      target.value,
      streams.value.log
    )
  }

  // The dependencies' stubs, rendered again only when their stamp changes: every file of the
  // classpath (a class directory walked) and of the calculator by size and modification time, and
  // the JDK, whose classes render too. sbt 2's plugin keys its cached task by the same inputs.
  private lazy val stubs = Def.task {
    val dir = target.value / "cardinality"
    val out = dir / "stubs.bin"
    val stampFile = dir / "stubs.stamp"
    val classpath = cardinalityDependencyClasspath.value
    val stamp = stampOf(calculatorJars.value ++ classpath)
    val log = streams.value.log
    val render = calculator.value
    if (out.exists && stampFile.exists && IO.read(stampFile) == stamp)
      log.debug(s"dependency stubs up to date: $out")
    else {
      render.stubs(classpath.map(_.toPath), out.toPath)
      IO.write(stampFile, stamp)
    }
    out.toPath
  }

  private def stampOf(files: Seq[File]): String = {
    val stamps = files.flatMap { root =>
      val all = if (root.isDirectory) (root ** -DirectoryFilter).get.sorted else Seq(root)
      all.map(f => s"${f.getAbsolutePath} ${f.length} ${f.lastModified}")
    }
    Hash.toHex(Hash((sys.props("java.version") +: stamps).mkString("\n")))
  }

}
