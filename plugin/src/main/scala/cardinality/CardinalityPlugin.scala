package cardinality

import cardinality.CardinalityTool.Classes
import sbt.*
import sbt.Keys.*
import sbt.util.CacheImplicits.given

// The sbt 2 face of scala-cardinality. Triggered automatically so a build only has to add the
// plugin to `project/plugins.sbt`; everything it computes is delegated to the `core` module,
// where the coverage and mutation gates bite, reached through the loader the three plugins share
// (`cardinality.loader.Calculator`).
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport extends CardinalityKeys {

    // Content-hashed references, as sbt 2 hands out the dependency jars.
    val cardinalityDependencyClasspath = taskKey[Seq[HashedVirtualFileRef]](
      "Compiled dependencies the project's compiled classes are read against."
    )

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = CardinalityTool.defaults(autoImport) ++ Seq(
    // The project's own Compile dependencies, never `fullClasspath`: dependency paths stay explicit.
    // sbt 2 hands out content-addressed jars, so each entry goes through the file converter.
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.map(_.data),
    // `Def.uncached`: seeding must re-check the target file every run, never be served stale.
    cardinalitySeedSkill := Def.uncached {
      CardinalityTool.seedSkill(
        streams.value.log,
        cardinalitySkillFile.value,
        (ThisBuild / baseDirectory).value
      )
    },
    // `Def.uncached`: a report over transient inputs (`sources` is excluded from sbt 2's cache key)
    // must re-run every time, never be served stale from the task cache.
    cardinalityReport := Def.uncached {
      CardinalityTool.report(
        calculator.value,
        (Compile / sources).value,
        compiled.value,
        cardinalityReportFile.value,
        streams.value.log
      )
    },
    // The same report over sources the build does not compile itself — a library's published
    // sources jar beside its binary jar, read against the project's dependencies — so that a
    // dependency's types can be measured without vendoring them into this build.
    // `.parsed` in the setting body is what makes this an input task. Written through
    // `Def.inputTask` instead, the key is registered as a plain task and sbt rejects the arguments.
    cardinalityReportOf := {
      val sources = CardinalityTool.paths.parsed.map(file)
      Def.uncached {
        CardinalityTool.report(
          calculator.value,
          sources,
          Classes(Some(Nil), dependencies.value),
          cardinalityReportFile.value,
          streams.value.log
        )
      }
    },
    cardinalityReportSelected := {
      val names = CardinalityTool.names.parsed
      Def.uncached {
        CardinalityTool.selected(
          calculator.value,
          names,
          (Compile / sources).value,
          compiled.value,
          cardinalityReportFile.value,
          streams.value.log
        )
      }
    }
  )

  // The calculator of the jars resolved at `cardinalityVersion`, every run: a republished snapshot
  // is the one loaded.
  private val calculator = Def.task {
    cardinality.loader.Calculator.of(
      CardinalityTool
        .jars(csrConfiguration.value, cardinalityVersion.value, target.value, streams.value.log)
        .map(_.toPath)
    )
  }

  private val dependencies = Def.task {
    val converter = fileConverter.value
    cardinalityDependencyClasspath.value.map(ref => converter.toPath(ref).toFile)
  }

  // The project's compiled classes and the jars their types refer to, which the report reads.
  // `exportedProducts` compiles first, so the classes match the sources the report reads. Sources
  // that do not compile hand no classes, and the report refuses every one of them.
  private val compiled = Def.task {
    val converter = fileConverter.value
    val classpath = dependencies.value
    val log = streams.value.log
    (Compile / exportedProducts).result.value.toEither match {
      case Right(products) =>
        Classes(Some(products.map(entry => converter.toPath(entry.data).toFile)), classpath)
      case Left(_) =>
        log.warn("sources do not compile: the report reads none of them")
        Classes(None, classpath)
    }
  }

}
