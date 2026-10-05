name := "scala-cardinality"
version := "0.1.0-SNAPSHOT"
scalaVersion := "3.9.0"

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "4.17.4",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.specs2" %% "specs2-core" % "4.23.0" % Test,
  "org.specs2" %% "specs2-cats" % "4.23.0" % Test
)

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
  // Scala 3.9's JVM optimizer; inline only this compilation's sources rather
  // than embedding dependency implementations in our published bytecode.
  "-opt",
  "-opt-inline:<sources>"
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
// A regression floor, not an aspiration: the suites cover the `Size` algebra and
// the arithmetic in `docs/type-arithmetic.md`, but not every escalation arm, so
// the current baseline is ~85% statements / ~82% branches. Statements are gated
// just below that; the number should ratchet up as the algebra gains tests, not
// be treated as a target. Report-only would let coverage rot silently, and an
// aspirational number here would be red on day one.
coverageHighlighting := true
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 80

// Full coverage sweep used by CI (`sbt coverageAll`). `clean` first so a
// rebuild starts from the sources: on a cold sbt cache this discards any stale
// instrumented classes. (sbt 2 may serve `clean` from its machine-wide task
// cache; see the README note on cold runs.)
addCommandAlias(
  "coverageAll",
  "clean; coverage; test; coverageReport"
)

// Mutation-testing sweep used on demand and at release (`sbt mutationAll`).
// Single module, so this is just the plugin's `stryker` task with the
// cross-cutting config from `stryker4s.conf`.
addCommandAlias("mutationAll", "stryker")

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
lazy val siteRender = taskKey[Unit]("Render docs/ into the static site under target/site")

// Uncached on purpose: writing the site is a side effect, so it must re-read `docs/` on
// every run rather than trust a cache entry that only tracks a directory path.
siteRender := Def.uncached {
  // Not `target.value`: sbt 2 nests that under `target/out/jvm/...`, and the deploy
  // workflow wants one path it can point at.
  val output = (ThisBuild / baseDirectory).value / "target" / "site"
  IO.delete(output)
  SiteRenderer.render((ThisBuild / baseDirectory).value / "docs", output)
  streams.value.log.info(s"Site written to ${output.getAbsolutePath}")
}
