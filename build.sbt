// Bare settings are sbt 2 common settings: they apply to every subproject, so
// only cross-cutting values live here. Anything module-specific (name,
// dependencies, gates) sits on its project below.
version := "0.1.0-SNAPSHOT"
organization := "io.github.constructive-programming"

// Pinned to the Scala version sbt 2.0.x itself runs on (its metabuild is
// 3.8.4): the sbt plugin — and `core`, which it loads — must be binary-loadable
// inside sbt, and Scala 3 binary compatibility is backward only. Library users
// on 3.9+ can consume 3.8.4 artifacts unchanged, so nothing is lost by
// building everything at this version.
scalaVersion := "3.8.4"

// The root project only aggregates; it publishes nothing and has no sources.
lazy val root = (project in file("."))
  .aggregate(core, plugin)
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
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "4.17.4",
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

// The sbt plugin: reads each subproject's sources and hands them to `core`.
// Kept as thin as possible — logic that can live in `core` must live in
// `core`, where the coverage and mutation gates bite.
lazy val plugin = project
  .enablePlugins(SbtPlugin)
  .dependsOn(core)
  .settings(
    name := "sbt-cardinality",
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

// ----------------------------------------------------------------
// Compiler options
// ----------------------------------------------------------------
// A subset of the flag set the sister project `eo` enables through
// sbt-typelevel-settings, spelled out here so the small build does not need that
// plugin. `-Wunused:all` is the broadest unused-warning surface; unlike `eo` we
// do not turn warnings into errors yet.
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all"
)

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
// A regression floor, not an aspiration: at the multi-module restructure the
// `core` baseline measures ~84% statements / ~79% branches, so statements are
// gated just below that. The number should ratchet up as the report work lands,
// not be treated as a target. Report-only would let coverage rot silently, and
// an aspirational number here would be red on day one. The floor itself is set
// on `core` above.
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
