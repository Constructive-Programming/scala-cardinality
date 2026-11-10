package cardinality

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

    // Content-hashed references: the cached stub task below is keyed by the jars' contents.
    val cardinalityDependencyClasspath = taskKey[Seq[HashedVirtualFileRef]](
      "Compiled dependencies whose declarations the reports read; `Nil` reads sources only."
    )

    val cardinalityDependencyStubs = taskKey[HashedVirtualFileRef](
      "The dependencies' declarations rendered as stubs: cached by sbt by the jars' contents."
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

  // The calculator's jars and the JDK: inputs of the cached stub task, so stubs another calculator
  // or JDK rendered (the JDK's classes render too) are never read.
  private val cardinalityCalculatorJars =
    taskKey[Seq[HashedVirtualFileRef]]("The calculator's jars (internal).")

  private val cardinalityJava =
    settingKey[String]("The JDK the dependency stubs are rendered on (internal).")

  private val cardinalityProject =
    settingKey[String]("The project's id, which names its stub file (internal).")

  override def projectSettings: Seq[Setting[?]] = CardinalityTool.defaults(autoImport) ++
    dependencySettings ++ reportSettings ++ Seq(
      // Seeding is a repository-level act: the default lands beside the other agent instructions
      // at the build's root, wherever the task is run from.
      cardinalitySkillFile := (ThisBuild / baseDirectory).value / ".claude" / "skills" /
        "cardinality-reduction" / "SKILL.md",
      // `Def.uncached`: seeding must re-check the target file every run, never be served stale.
      cardinalitySeedSkill := Def.uncached {
        seedSkill(
          streams.value.log,
          cardinalitySkillFile.value,
          (ThisBuild / baseDirectory).value,
          version.value
        )
      },
    )

  // `Def.uncached`: a report over transient inputs (`sources` is excluded from sbt 2's cache key)
  // must re-run every time, never be served stale from the task cache.
  private def reportSettings: Seq[Setting[?]] = Seq(
    cardinalityReport := Def.uncached {
      val converter = fileConverter.value
      CardinalityTool.report(
        calculator(cardinalityCalculatorJars.value, converter),
        (Compile / sources).value,
        Some(converter.toPath(cardinalityDependencyStubs.value)),
        cardinalityReportFile.value,
        streams.value.log
      )
    },
    // The same report over sources the build does not compile itself — a library's published
    // sources jar, or another project's checkout — so that a dependency's types can be measured
    // without vendoring them into this build.
    // `.parsed` in the setting body is what makes this an input task. Written through
    // `Def.inputTask` instead, the key is registered as a plain task and sbt rejects the arguments.
    cardinalityReportOf := {
      val sources = CardinalityTool.paths.parsed.map(file)
      Def.uncached {
        CardinalityTool.report(
          calculator(cardinalityCalculatorJars.value, fileConverter.value),
          sources,
          None,
          cardinalityReportFile.value,
          streams.value.log
        )
      }
    },
    cardinalityReportSelected := {
      val names = CardinalityTool.names.parsed
      Def.uncached {
        val converter = fileConverter.value
        CardinalityTool.selected(
          calculator(cardinalityCalculatorJars.value, converter),
          CardinalityTool.Query(
            names,
            (Compile / sources).value,
            cardinalityQuerySupport.value,
            cardinalityQueryLimits.value
          ),
          Some(converter.toPath(cardinalityDependencyStubs.value)),
          cardinalityReportFile.value,
          streams.value.log
        )
      }
    }
  )

  private def calculator(jars: Seq[HashedVirtualFileRef], converter: xsbti.FileConverter) =
    cardinality.loader.Calculator.of(jars.map(converter.toPath))

  // The dependency declarations the reports read: which jars, and their stubs rendered once.
  private def dependencySettings: Seq[Setting[?]] = Seq(
    // The project's own Compile dependencies, never `fullClasspath`: dependency paths stay explicit.
    // sbt 2 hands out content-addressed jars, so each entry goes through the file converter.
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.map(_.data),
    // `Def.uncached`: resolved every run, so a republished snapshot is the one loaded.
    cardinalityCalculatorJars := Def.uncached {
      val converter = fileConverter.value
      CardinalityTool
        .jars(
          csrConfiguration.value,
          cardinalityVersion.value,
          target.value,
          streams.value.log
        )
        .map(jar => converter.toVirtualFile(jar.toPath): HashedVirtualFileRef)
    },
    cardinalityJava := sys.props("java.version"),
    cardinalityProject := thisProject.value.id,
    // An sbt 2 cached task: keyed by the content hashes (`HashedVirtualFileRef`) of the
    // dependencies' jars and of the calculator's, and by the JDK, its file is restored from the
    // machine-wide cache instead of rendered again.
    cardinalityDependencyStubs := {
      val converter = fileConverter.value
      val _ = cardinalityJava.value
      val jars = cardinalityDependencyClasspath.value.map(converter.toPath).toList.distinct
      // Under sbt's output root, which the converter writes as `${OUT}`: the declared file is then
      // a relative reference, restorable on any machine sharing the cache.
      // By project id: two projects may publish under one name (this build's two sbt plugins do).
      val out =
        rootOutputDirectory.value.resolve(s"cardinality/${cardinalityProject.value}/stubs.bin")
      calculator(cardinalityCalculatorJars.value, converter).stubs(jars, out)
      val file = converter.toVirtualFile(out)
      Def.declareOutput(file)
      file: HashedVirtualFileRef
    }
  )

  // Writes the reduction skill this repository's agents use, parameterized by the build, so a
  // project that adopts the plugin can seed the same workflow. The generated marker comment is
  // the ownership check: the task refreshes files it generated and refuses to touch a file it
  // did not, because a user's edits to their copy must never be silently clobbered.
  private def seedSkill(log: Logger, output: File, buildRoot: File, pluginVersion: String): File = {
    val url = getClass.getResource("/cardinality/cardinality-reduction-skill.md")
    if (url == null) sys.error("skill template not found on the plugin classpath")
    val skill = new String(
      url.openStream().readAllBytes(),
      java.nio.charset.StandardCharsets.UTF_8
    )
      .replace("{{project}}", buildRoot.getName)
      .replace("{{version}}", pluginVersion)
    if (output.exists() && !IO.read(output).contains("Generated by sbt-cardinality"))
      sys.error(
        s"$output exists and was not generated by sbt-cardinality; move or delete it to seed a fresh copy"
      )
    if (output.exists() && IO.read(output) == skill)
      log.info(s"skill already up to date: $output")
    else {
      IO.createDirectory(output.getParentFile)
      IO.write(output, skill)
      log.info(s"seeded the cardinality-reduction skill: $output")
    }
    output
  }

}
