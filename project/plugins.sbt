// Formatting gate. CI runs `sbt scalafmtCheckAll scalafmtSbtCheck`; the
// `.scalafmt.conf` pin drives the formatter version the plugin downloads,
// so the plugin itself stays version-agnostic across scalafmt releases.
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")

// Scalafix wires the semantic rewrites declared in `.scalafix.conf`.
// Developers run `sbt scalafixAll` to auto-fix; CI runs
// `sbt scalafixAll --check` so rule drift fails the build without
// rewriting files.
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.9")

// Statement / branch coverage for the test suite. CI runs the `coverageAll`
// alias (defined in build.sbt); the HTML report is uploaded as an artifact.
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")

// Mutation testing. Heavy — a full sandbox build plus a test run per mutant —
// so it runs on demand and at release via `mutationAll`, never as a per-PR
// gate. Cross-cutting knobs live in `stryker4s.conf`.
addSbtPlugin("io.stryker-mutator" % "sbt-stryker4s" % "1.1.1")
