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

// Documentation site. Laika renders the markdown in `docs/` into a static site with
// its Helium theme; the `siteRender` task in build.sbt calls it, and `project/
// SiteRenderer.scala` is the wiring. Laika runs inside the build JVM, like it does for
// the sister project `eo`, which gets the whole pipeline (mdoc + Laika) from
// `sbt-typelevel-site`.
//
// Two things are deliberately not used here:
//   - `sbt-typelevel-site` and Laika's own sbt plugin, neither of which has an sbt 2 build.
//   - `sbt-mdoc`, for compiling `scala mdoc` fences in the docs. It puts mdoc 2.13 and
//     its `scalameta_2.13` on this project's classpath, and `Counter` depends on
//     `scalameta_3`: same package names, different artifacts, so neither sbt nor a
//     compiler classpath can hold both. Compiling docs examples needs either an mdoc
//     release that follows scalameta's Scala 3 artifacts, or a separate build. Until
//     then `docs/` examples are pinned by the test suite instead.
libraryDependencies +=
  "org.typelevel" %% "laika-io" % "1.3.2"
