package cardinality

import sbt.*
import sbt.Keys.*

// The sbt 1 face of scala-cardinality. sbt 1 runs on Scala 2.12 and cannot load `core` (Scala 3),
// so this plugin resolves `scala-cardinality_3` on its own (outside the project's `update`, which
// would evict whatever the project itself builds — cats-eo, in cats-eo's build) and forks
// `cardinality.Main` on it. Same keys as the sbt 2 plugin where the semantics carry over.
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

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

    val cardinalityDependencyClasspath = taskKey[Seq[File]](
      "Compiled dependencies whose declarations the reports read; `Nil` reads sources only."
    )

    val cardinalityReportFile = settingKey[File](
      "Where a report is written, under the project's target directory by default."
    )

    val cardinalityVersion =
      settingKey[String]("The scala-cardinality version the reports run.")

    val cardinalityJavaHome = settingKey[Option[File]](
      "JDK the reports fork on (25 or later); `None` uses the JVM running sbt."
    )

  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    cardinalityVersion := BuildInfo.version,
    cardinalityJavaHome := None,
    cardinalityReportFile := target.value / "cardinality" / "report.txt",
    cardinalityQuerySupport := Nil,
    cardinalityDependencyClasspath := (Compile / dependencyClasspath).value.files,
    cardinalityReport := {
      // ponytail: dependency stubs are rendered on every run; sbt 2's plugin caches them by jar hash.
      run(
        toolClasspath.value,
        cardinalityJavaHome.value,
        Seq("--classpath", Path.makeString(cardinalityDependencyClasspath.value)) ++
          (Compile / sources).value.map(_.getAbsolutePath),
        cardinalityReportFile.value,
        streams.value.log
      )
    },
    cardinalityReportOf := {
      val paths = Def.spaceDelimited("<path>...").parsed
      run(
        toolClasspath.value,
        cardinalityJavaHome.value,
        paths.map(file(_).getAbsolutePath),
        cardinalityReportFile.value,
        streams.value.log
      )
    },
    cardinalityReportSelected := {
      val names = Def.spaceDelimited("<qualified-method-name>...").parsed
      if (names.isEmpty) sys.error("provide at least one qualified method name")
      run(
        toolClasspath.value,
        cardinalityJavaHome.value,
        names.flatMap(Seq("--select", _)) ++
          cardinalityQuerySupport.value.flatMap(s => Seq("--support", s.getAbsolutePath)) ++
          Seq("--classpath", Path.makeString(cardinalityDependencyClasspath.value)) ++
          (Compile / sources).value.map(_.getAbsolutePath),
        cardinalityReportFile.value,
        streams.value.log
      )
    }
  )

  private def toolClasspath = Def.task {
    dependencyResolution.value
      .retrieve(
        "dev.constructive" % "scala-cardinality_3" % cardinalityVersion.value,
        None,
        target.value / "cardinality" / "tool",
        streams.value.log
      )
      .fold(warning => throw warning.resolveException, _.toSeq)
  }

  private def run(
      classpath: Seq[File],
      javaHome: Option[File],
      args: Seq[String],
      output: File,
      log: Logger
  ): Unit = {
    val options = ForkOptions()
      .withJavaHome(javaHome)
      .withOutputStrategy(Some(LoggedOutput(log)))
      // core needs JDK 25 (kyo), where Scala's LazyVals otherwise warns on stderr.
      .withRunJVMOptions(Vector("--sun-misc-unsafe-memory-access=allow"))
    val exit = Fork.java(
      options,
      Seq("-cp", Path.makeString(classpath), "cardinality.Main", "--out", output.getAbsolutePath) ++
        args
    )
    log.info(s"report written to $output")
    if (exit != 0) sys.error(s"cardinality report failed (exit $exit); see $output")
  }

}
