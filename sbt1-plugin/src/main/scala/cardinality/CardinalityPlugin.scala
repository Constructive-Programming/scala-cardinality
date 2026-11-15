package cardinality

import cardinality.CardinalityTool.Classes
import sbt.*
import sbt.Keys.*

// The sbt 1 face of scala-cardinality. The calculator is reached through the loader the three
// plugins share (`cardinality.loader.Calculator`): it runs in sbt's JVM, apart from sbt's Scala
// 2.12, so that JVM must be the JDK the calculator needs.
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport extends CardinalityKeys {

    val cardinalityDependencyClasspath = taskKey[Seq[File]](
      "Compiled dependencies, jars and class directories, the project's classes are read against."
    )

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    // The project's own Compile dependencies, never `fullClasspath`: dependency paths stay explicit.
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.files,
    cardinalitySeedSkill := CardinalityTool
      .seedSkill(streams.value.log, cardinalitySkillFile.value, (ThisBuild / baseDirectory).value),
    cardinalityReport := run.value.report((Compile / sources).value, compiled.value),
    // A library's binary jar given beside its sources jar is read against the project's
    // dependencies.
    cardinalityReportOf := {
      val sources = CardinalityTool.paths.parsed.map(file)
      run.value.report(sources, Classes(Some(Nil), cardinalityDependencyClasspath.value))
    },
    cardinalityReportSelected :=
      run.value.selected(CardinalityTool.names.parsed, (Compile / sources).value, compiled.value)
  ) ++ CardinalityTool.defaults(autoImport)

  private val run = CardinalityTool.run(autoImport)

  // The project's compiled classes and the jars their types refer to, which the report reads.
  // `exportedProducts` compiles first, so the classes match the sources the report reads. Sources
  // that do not compile hand no classes, and the report refuses every one of them.
  private val compiled = Def.task {
    val classpath = cardinalityDependencyClasspath.value
    val log = streams.value.log
    (Compile / exportedProducts).result.value match {
      case Value(products) => Classes(Some(products.files), classpath)
      case Inc(_)          =>
        log.warn("sources do not compile: the report reads none of them")
        Classes(None, classpath)
    }
  }

}
