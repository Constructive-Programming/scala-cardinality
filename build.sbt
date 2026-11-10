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

// Pinned to the Scala version sbt 2.x itself runs on (sbt 2.1.0-RC1's metabuild is
// 3.9.0, matching cats-eo 1.2.1): the sbt plugin — and `core`, which it loads — must
// be binary-loadable inside sbt, and Scala 3 binary compatibility is backward only.
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
    description := "Counts the values of Scala types and the implementations of Scala signatures",
    libraryDependencies ++= Seq(
      "dev.constructive" %% "cats-eo" % "1.2.1",
      "dev.constructive" %% "cats-eo-generics" % "1.2.1",
      "dev.constructive" %% "cats-eo-schemes" % "1.2.1",
      "org.scalameta" %% "scalameta" % "4.17.4",
      // Compiled dependency declarations (TASTy); 1.9.0 is the release built on Scala 3.9.0.
      "ch.epfl.scala" %% "tasty-query" % "1.9.0",
      // Parallel measurement (Measuring): kyo 1.0.0-RC7 is built on Scala 3.9.0.
      "io.getkyo" %% "kyo-core" % "1.0.0-RC7",
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

// JMH benchmarks over a real checkout (bench/README.md). Not aggregated by `root`: benchmarks run
// on demand, never in CI, and publish nothing.
lazy val bench = project
  .enablePlugins(JmhPlugin)
  .dependsOn(core)
  .settings(
    name := "scala-cardinality-bench",
    publish / skip := true,
    coverageEnabled := false
  )

// The sbt plugin: reads each subproject's sources and hands them to `core`.
// Kept as thin as possible — logic that can live in `core` must live in
// `core`, where the coverage and mutation gates bite.
lazy val plugin = project
  .enablePlugins(SbtPlugin)
  .dependsOn(core)
  .settings(
    name := "sbt-cardinality",
    // The keys both sbt plugins offer: one source, compiled into each.
    Compile / unmanagedSourceDirectories += pluginKeys.value,
    description := "sbt plugin reporting the cardinality of a project's definitions and methods",
    // scripted boots a fresh sbt per test project and resolves the plugin by
    // version, so the tests pass it through a system property.
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false,
    // scripted resolves the plugin and its `core` dependency from the local
    // repository; publish both before any test project boots.
    scriptedDependencies := {
      val _ = (core / publishLocal).value
      publishLocal.value
    }
  )

lazy val pluginKeys = Def.setting(
  (ThisBuild / baseDirectory).value / "plugin-keys" / "src" / "main" / "scala"
)

// The sbt 1 plugin, for builds that cannot move to sbt 2 yet. sbt 1 runs on Scala 2.12 and cannot
// load `core`, so this plugin has no dependency on it: it resolves `scala-cardinality_3` at the
// plugin's own version and calls `cardinality.Entry` in a classloader apart from sbt's. Same
// artifact name as `plugin`; sbt's cross suffixes (`_2.12_1.0` vs `_sbt2_3`) keep them apart.
lazy val sbt1Plugin = (project in file("sbt1-plugin"))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-cardinality",
    // The keys both sbt plugins offer: one source, compiled into each.
    Compile / unmanagedSourceDirectories += pluginKeys.value,
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
      // Scala 3 syntax (`import sbt.*`) so scalafix's OrganizeImports rules hold here too.
      "-Xsource:3"
    ),
    coverageEnabled := false,
    Compile / sourceGenerators += Def.task {
      val out = (Compile / sourceManaged).value / "cardinality" / "BuildInfo.scala"
      IO.write(
        out,
        s"package cardinality\n\nprivate[cardinality] object BuildInfo { val version = \"${version.value}\" }\n"
      )
      Seq(out)
    }.taskValue
  )

// The Mill plugin: `CardinalityModule`, mixed into a `ScalaModule`. Mill 1.3 compiles builds with
// Scala 3.9.0, this build's version, so the plugin loads `core` directly. The `_mill1` suffix is
// Mill's convention for plugins built against its 1.x API.
lazy val millPlugin = (project in file("mill-plugin"))
  .dependsOn(core)
  .settings(
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
    // the run.
    Test / fork := true
  )

lazy val millVersion = "1.3.1"

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
  "-Werror"
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

// Full coverage sweep used by CI (`sbt coverageAll`). `clean` first so a
// rebuild starts from the sources: on a cold sbt cache this discards any stale
// instrumented classes. (sbt 2 may serve `clean` from its machine-wide task
// cache; see the README note on cold runs.)
addCommandAlias(
  "coverageAll",
  "clean; coverage; test; coverageReport"
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
