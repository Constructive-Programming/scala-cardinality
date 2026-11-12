# Getting started

The calculator runs inside your build tool. Add the plugin, run one command, read the
number. Nothing is uploaded and no source leaves the machine.

## sbt

Add the plugin to `project/plugins.sbt`:

```scala
addSbtPlugin("dev.constructive" % "sbt-cardinality" % "0.0.6")
```

sbt 1.10 and later use the same line: sbt selects the sbt 1 artifact
(`sbt-cardinality_2.12_1.0`). Then run:

```bash
sbt cardinalityReport
```

`sbt cardinalityReport` measures every definition the project's `Compile` sources
introduce — nested definitions included — and logs a report. The same report is written to
`target/cardinality/report.txt` (`cardinalityReportFile`), so a build can keep it, diff
it, or post it as an artifact.

```
scala-cardinality — 1 source, 10 definitions
  stored-value estimates: constructor inputs only; finite capacities are upper bounds
  ?: unresolved, not a proof of infinity
  2 unresolved · 4 with more than one value · 2 with one value · 2 abstract
Stored-value estimates (constructor inputs; `?` = unresolved)

?  example.Holder[A]  class     Light.scala:11  unresolved: A
ω  example.Succ       class     Light.scala:17
ω  example.Timeline   class     Light.scala:19
2  example.Custom     class     Light.scala:5
1  example.Zero       object    Light.scala:16
—  example.Light      abstract  Light.scala:3
```

`cardinalityReportSelected` analyzes named signatures only:

```bash
sbt 'cardinalityReportSelected example.Accessor.get'
```

`cardinalityReportOf` runs the same report over sources the build does not compile itself
— a directory, a single file, or a published `-sources.jar`:

```bash
sbt 'cardinalityReportOf path/to/sources.jar'
```

## Mill

For Mill 1.3 or later, load the plugin in `build.mill`'s header and mix
`CardinalityModule` into a `ScalaModule`:

```scala
//| mvnDeps: ["dev.constructive::mill-cardinality_mill1:0.0.6"]
import millcardinality.CardinalityModule

object foo extends ScalaModule with CardinalityModule { def scalaVersion = "3.9.0" }
```

`mill foo.cardinalityReport`, `mill foo.cardinalityReportOf <path>...` and
`mill foo.cardinalityReportSelected <qualified-method-name>...` run the reports; each
writes `report.txt` under the command's `out/` directory.

## The calculator alone

```scala
"dev.constructive" %% "scala-cardinality" % "0.0.6"
```

The build tool may run on JDK 17, 21 or 25. The calculator reads dependencies compiled by
Scala up to the version its TASTy reader supports: 0.0.5 and later read Scala 3.0 through
3.10. A newer Scala's jars appear as `dependency stub not read: … TASTy signature has
wrong version` at the end of the report.

## Next

[Reading a report](reading-a-report.md) explains the rows, the tiers and the moves that
lower a number. [Assumptions](references/assumptions.md) states what every count assumes.
