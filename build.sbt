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
// A regression floor, not an aspiration: `SizeSpec` covers the `Size` algebra
// but not every escalation arm, so the current baseline is ~68% statements /
// ~56% branches. Statements are gated just below that; the number should
// ratchet up as the algebra gains tests, not be treated as a target.
// Report-only would let coverage rot silently, and an aspirational number here
// would be red on day one.
coverageHighlighting := true
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 65

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
