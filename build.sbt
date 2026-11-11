// Bare settings are sbt 2 common settings: they apply to every subproject, so
// only cross-cutting values live here. Anything module-specific (name,
// dependencies, gates) sits on its project below.
// A release build gets its version from the tag (`.github/workflows/release.yml`).
version := sys.env.getOrElse("RELEASE_VERSION", "0.1.0-SNAPSHOT")
organization := "dev.constructive"
organizationName := "Constructive"
homepage := Some(uri("https://github.com/Constructive-Programming/scala-cardinality"))
licenses := Seq("GPL-2.0-only" -> uri("https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"))
scmInfo := Some(
  ScmInfo(
    uri("https://github.com/Constructive-Programming/scala-cardinality"),
    "scm:git:https://github.com/Constructive-Programming/scala-cardinality.git",
    Some("scm:git:git@github.com:Constructive-Programming/scala-cardinality.git")
  )
)
developers := List(
  Developer("kryptt", "Rodolfo Hansen", "@kryptt", uri("https://github.com/kryptt"))
)
// Maven Central through sbt's own Central Portal support: `publishSigned` stages the signed
// artifacts here, `sonaRelease` uploads and releases them (SONATYPE_USERNAME / SONATYPE_PASSWORD).
publishTo := localStaging.value

// The Scala sbt 2.x itself runs on (sbt 2.1.0-RC1's metabuild is 3.9.0, matching cats-eo 1.3.0).
// The sbt 2 plugin must be binary-loadable inside sbt, and Scala 3 binary compatibility is backward
// only. `core` no longer has to follow: the plugins load it apart (`loaderSettings` below).
scalaVersion := "3.9.0"

// The root project only aggregates; it publishes nothing and has no sources.
lazy val root = (project in file("."))
  .aggregate(core, plugin, sbt1Plugin, millPlugin)
  .settings(
    name := "scala-cardinality-root",
    publish / skip := true
  )

// The cardinality calculator itself: a pure library (scalameta parsing + the
// Size algebra) with no sbt types anywhere, so every behavior is testable with
// plain specs2.
lazy val core = project
  .settings(
    name := "scala-cardinality",
    // tasty-query reads TASTy up to the Scala it is built on, so `core` follows tasty-query's Scala.
    scalaVersion := coreScalaVersion,
    description := "Counts the values of Scala types and the implementations of Scala signatures",
    libraryDependencies ++= Seq(
      "dev.constructive" %% "cats-eo" % "1.3.0",
      "dev.constructive" %% "cats-eo-generics" % "1.3.0",
      "dev.constructive" %% "cats-eo-schemes" % "1.3.0",
      "org.scalameta" %% "scalameta" % "4.17.4",
      // Compiled dependency declarations (TASTy); 1.10.0 is built on Scala 3.10.0 and reads TASTy
      // up to 28.10, so dependencies compiled by Scala 3.0 through 3.10.
      "ch.epfl.scala" %% "tasty-query" % "1.10.0",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.specs2" %% "specs2-core" % "4.23.0" % Test,
      "org.specs2" %% "specs2-cats" % "4.23.0" % Test
    ),
    // Coverage floors live here, not in the common settings: `plugin` is thin
    // sbt wiring exercised by scripted tests, which run outside scoverage's
    // instrumentation, so a floor there would be red by construction.
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 80
  )

// Experimental: `core` as a Scala Native binary (`sbt coreNative/nativeLink`). The same sources,
// minus the TASTy reader: tasty-query has no Native build, and a native binary holds no JDK to
// read. `core-native` supplies a stand-in for `TastyStubs` (a classpath is reported as not read; a
// stub file rendered on the JVM is still read), a SHA-256 `MessageDigest` (absent from Scala
// Native's Java library) and a main. Its reports match the JVM's; it is slower on large inputs.
// Not aggregated: nothing here is published, tested or gated.
lazy val coreNative = (project in file("core-native"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "scala-cardinality-native",
    // NATIVE_RELEASE=1 builds the optimized binary (minutes); the default debug build links fast.
    nativeConfig ~= { config =>
      if (sys.env.contains("NATIVE_RELEASE"))
        config
          .withMode(scala.scalanative.build.Mode.releaseFast)
          .withLTO(scala.scalanative.build.LTO.thin)
      else config
    },
    publish / skip := true,
    coverageEnabled := false,
    Compile / unmanagedSourceDirectories += (core / Compile / scalaSource).value,
    Compile / unmanagedSources := (Compile / unmanagedSources).value.filterNot { source =>
      val path = source.toString.replace('\\', '/')
      path.contains("/core/src/main/scala/cardinality/tasty/") && !path.endsWith("/StubFile.scala")
    },
    libraryDependencies ++= Seq(
      "dev.constructive" %% "cats-eo" % "1.3.0",
      "dev.constructive" %% "cats-eo-generics" % "1.3.0",
      "dev.constructive" %% "cats-eo-schemes" % "1.3.0",
      "org.scalameta" %% "scalameta" % "4.17.4",
      // scalameta's Native artifact leaves out the fastparse it shades on the JVM.
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "org.typelevel" %% "cats-core" % "2.13.0"
    )
  )

// JMH benchmarks over a real checkout (bench/README.md). Not aggregated by `root`: benchmarks run
// on demand, never in CI, and publish nothing.
lazy val bench = project
  .enablePlugins(JmhPlugin)
  .dependsOn(core)
  .settings(
    name := "scala-cardinality-bench",
    scalaVersion := coreScalaVersion,
    publish / skip := true,
    coverageEnabled := false
  )

// The three build-tool plugins share one way to reach `core`: none depends on it. Each resolves
// `scala-cardinality_3` at its own version (`BuildInfo`) and calls `cardinality.Entry` in a
// classloader apart from the build tool's (`plugin-loader`, one source compiled into each). A
// plugin is then built for its tool's Scala, whatever `core`'s is: sbt 1 runs Scala 2.12, and a
// tool on another Scala 3 minor cannot read this one's classes either.
lazy val loaderSettings = Seq(
  Compile / unmanagedSourceDirectories +=
    (ThisBuild / baseDirectory).value / "plugin-loader" / "src" / "main" / "scala",
  Compile / unmanagedResourceDirectories +=
    (ThisBuild / baseDirectory).value / "plugin-loader" / "src" / "main" / "resources",
  Compile / sourceGenerators += Def.task {
    val out = (Compile / sourceManaged).value / "cardinality" / "loader" / "BuildInfo.scala"
    val text = s"""package cardinality.loader
                  |
                  |private[loader] object BuildInfo { val version = "${version.value}" }
                  |""".stripMargin
    IO.write(out, text)
    Seq(out)
  }.taskValue
)

// What the two sbt plugins share beside the loader: the keys and the helpers both use (`plugin-sbt`, one
// source compiled into each), and scripted, which boots a real sbt of the plugin's kind on each
// test project with the plugin and the `core` it resolves published locally first.
lazy val sbtPluginSettings = loaderSettings ++ Seq(
  name := "sbt-cardinality",
  Compile / unmanagedSourceDirectories +=
    (ThisBuild / baseDirectory).value / "plugin-sbt" / "src" / "main" / "scala",
  // scripted resolves the plugin by version, so the tests pass it through a system property.
  scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
  scriptedBufferLog := false,
  scriptedDependencies := {
    val _ = (core / publishLocal).value
    publishLocal.value
  }
)

// The sbt 2 plugin: reads each subproject's sources and hands them to `core`.
// Kept as thin as possible — logic that can live in `core` must live in
// `core`, where the coverage and mutation gates bite.
lazy val plugin = project
  .enablePlugins(SbtPlugin)
  .settings(
    sbtPluginSettings,
    description := "sbt plugin reporting the cardinality of a project's definitions and methods"
  )

// The sbt 1 plugin, for builds that cannot move to sbt 2 yet. Same artifact name as `plugin`;
// sbt's cross suffixes (`_2.12_1.0` vs `_sbt2_3`) keep them apart.
lazy val sbt1Plugin = (project in file("sbt1-plugin"))
  .enablePlugins(SbtPlugin)
  .settings(
    sbtPluginSettings,
    description := "sbt 1 plugin reporting the cardinality of a project's definitions and methods",
    scalaVersion := "2.12.20",
    pluginCrossBuild / sbtVersion := "1.10.0",
    // The Scala 3 flags in ThisBuild / scalacOptions are not 2.12 flags.
    scalacOptions := Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Ywarn-unused",
      "-Xfatal-warnings",
      // Scala 3 syntax (`import sbt.*`), so the sources shared with the Scala 3 plugins compile
      // here and scalafix's OrganizeImports rules hold.
      "-Xsource:3"
    ),
    coverageEnabled := false
  )

// The Mill plugin: `CardinalityModule`, mixed into a `ScalaModule`. Built with the Scala Mill 1.3
// compiles builds with; the `_mill1` suffix is Mill's convention for plugins on its 1.x API.
lazy val millPlugin = (project in file("mill-plugin"))
  .settings(
    loaderSettings,
    name := "mill-cardinality_mill1",
    description := "Mill plugin reporting the cardinality of a module's definitions and methods",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "mill-libs" % millVersion % Provided,
      "com.lihaoyi" %% "mill-testkit" % millVersion % Test,
      "org.specs2" %% "specs2-core" % "4.23.0" % Test
    ),
    // Mill's own classpath mixes `_3` and `_2.13` artifacts of scala-xml and collection-compat
    // (zinc); Mill loads them as it ships them, so sbt's cross-suffix check does not apply here.
    conflictWarning := ConflictWarning.disable,
    // The testkit evaluates the plugin's tasks in this JVM; forked, so its daemon threads end with
    // the run. The plugin resolves `core` at its own version: published locally first.
    Test / fork := true,
    Test / compile := (Test / compile).dependsOn(core / publishLocal).value
  )

lazy val millVersion = "1.3.1"

// `core`'s Scala: tasty-query's (above). The plugins stay on their tools' Scala.
lazy val coreScalaVersion = "3.10.1"

// ----------------------------------------------------------------
// Compiler options
// ----------------------------------------------------------------
// Scala 3 recommendations from sbt-typelevel-settings:
// https://github.com/typelevel/sbt-typelevel/blob/main/settings/src/main/scala/org/typelevel/sbt/TypelevelSettingsPlugin.scala
// Spelled out because this build uses sbt 2. `-Wunused:all` also checks pattern
// bindings and @nowarn annotations beyond Typelevel's individual unused flags.
// Warnings are fatal in both Compile and Test, locally and in CI.
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Werror",
  // Java 17 bytecode, compiled against Java 17's API whatever JDK builds it: the calculator and
  // the plugins run on JDK 17, 21 and 25 (CI builds on each).
  "-java-output-version",
  "17"
)

// Keep the plugin binary-loadable inside sbt's Scala 3.8.4 runtime. Enable the
// Scala 3.9+ optimizer only when a future sbt-compatible compiler supports it.
scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((3, minor)) if minor >= 9 => Seq("-opt", "-opt-inline:<sources>")
  case _                              => Nil
})

// ----------------------------------------------------------------
// Scalafix (semantic rules + typelevel-scalafix)
// ----------------------------------------------------------------
// The semantic rules in `.scalafix.conf` (RemoveUnused, OrganizeImports, ...)
// need SemanticDB exports. `semanticdbEnabled` adds the right flag for the
// running Scala version (the `-Xsemanticdb` compile option on Scala 3, the
// `semanticdb-scalac` plugin on Scala 2).
//
// `typelevel-scalafix` supplies `TypelevelMapSequence` / `TypelevelAs`. It is
// resolved against sbt-scalafix's 2.13 binary version even on Scala 3 because
// scalafix rules run in the scalafix classloader, not the project's.
ThisBuild / semanticdbEnabled := true
ThisBuild / scalafixDependencies +=
  "org.typelevel" %% "typelevel-scalafix" % "0.5.0"

// ----------------------------------------------------------------
// Coverage (scoverage)
// ----------------------------------------------------------------
// A regression floor, not an aspiration: the suites cover the `Size` algebra and
// the arithmetic in `docs/type-arithmetic.md`, but not every escalation arm, so
// the baseline sits around 85% statements. Statements are gated just below that;
// the number should ratchet up as the algebra gains tests, not be treated as a
// target. Report-only would let coverage rot silently, and an aspirational
// number here would be red on day one. The floor itself is set on `core` above,
// where the instrumented code lives.
coverageHighlighting := true

// Self-measurement (`-Dcardinality.self`, see project/plugins.sbt) reports one module at a time:
// the two sbt plugins' reports each read the stubs of all of sbt, and four reports at once do not
// fit the heap `.jvmopts` gives.
Global / concurrentRestrictions ++=
  (if (sys.props.contains("cardinality.self")) Seq(Tags.limitAll(1)) else Nil)

// Full coverage sweep used by CI (`sbt coverageAll`). `clean` first so a
// rebuild starts from the sources: on a cold sbt cache this discards any stale
// instrumented classes. (sbt 2 may serve `clean` from its machine-wide task
// cache; see the README note on cold runs.) `core` only: it holds the instrumented code, and
// the plugins' tests load a published `core`, which must not be an instrumented one.
addCommandAlias(
  "coverageAll",
  "clean; coverage; core/test; coverageReport"
)

// Mutation-testing sweep used on demand and at release (`sbt mutationAll`).
// Scoped to `core`: the algebra is where mutants are meaningful, and the
// `plugin` module's behavior lives in scripted tests stryker cannot run.
// Cross-cutting knobs stay in `stryker4s.conf`.
addCommandAlias("mutationAll", "core/stryker")

// ----------------------------------------------------------------
// Documentation site
// ----------------------------------------------------------------
// `docs/` is the source of truth for the repository and for the site, so there is no
// copy to keep in sync: `siteRender` runs Laika (the same engine and Helium theme the
// sister project `eo` uses) over that directory and writes `target/site`.
//
// Two deviations from `eo`'s setup, both forced by sbt 2: its `sbt-typelevel-site`
// plugin has no sbt 2 build, so the render step lives in `project/SiteRenderer.scala`
// instead of a plugin; and `sbt-mdoc` cannot be used here at all, because it puts
// `scalameta_2.13` on the classpath of a project that depends on `scalameta_3` (see
// `project/plugins.sbt`). Docs examples are therefore pinned by the test suite rather
// than compiled from the pages.
// `siteRender` is declared at the build level so `sbt siteRender` resolves, but only the
// root project renders: the site belongs to the repository, not to a module, and aggregating
// the task would write it once per module. Root has no sources, so nothing else is tied to it.
lazy val siteRender = taskKey[Unit]("Render docs/ into the static site under target/site")

// Uncached on purpose: writing the site is a side effect, so it must re-read `docs/` on
// every run rather than trust a cache entry that only tracks a directory path.
root / siteRender := Def.uncached {
  // Not `target.value`: sbt 2 nests that under `target/out/jvm/...`, and the deploy
  // workflow wants one path it can point at.
  val output = (ThisBuild / baseDirectory).value / "target" / "site"
  IO.delete(output)
  SiteRenderer.render((ThisBuild / baseDirectory).value / "docs", output)
  streams.value.log.info(s"Site written to ${output.getAbsolutePath}")
}
