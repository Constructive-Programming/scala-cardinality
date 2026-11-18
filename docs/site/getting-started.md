# Getting started

The calculator runs inside your build tool. Add the plugin, run one command, read the
number. Nothing is uploaded and no source leaves the machine.

The plugins compile the module first and read its compiled classes (TASTy). Every type, an
inferred one included, is the one the compiler gave it, so the module has to compile: a source
without compiled classes is refused. This page describes `main`. 0.0.6, the latest release,
still reads source syntax; reading compiled classes ships in the next release. To try it now,
run `sbt publishLocal` in a checkout and use `0.1.0-SNAPSHOT` in place of `0.0.6` below.

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

`sbt cardinalityReport` compiles the project's `Compile` sources and measures what they
introduce, nested definitions included. It logs a report and writes the same report to
`target/cardinality/report.txt` (`cardinalityReportFile`), so a build can keep it, diff it,
or post it as an artifact. The report has two parts: the implementation cardinality of each
generic method and constructor, then the stored-value estimate of each definition.

```
Generic method / constructor implementation cardinalities
  6 signatures: 2 finite · 3 countably infinite · 1 beyond countable · 0 unresolved

2  example.Custom.<init>  Custom(on: Boolean)  Light.scala:5  [constructor]
2^32  example.Holder.<init>  Holder[A](value: A, count: Int)  Light.scala:11  [constructor]
ω  example.Cursor.<init>  Cursor(at: Nat, open: Boolean)  Light.scala:16  [constructor]
ω  example.Timeline.<init>  Timeline(head: Boolean, tail: => Timeline)  Light.scala:18  [constructor]
ε₀  example.Reducer.<init>  Reducer(run: String => String)  Light.scala:20  [constructor]
ω  example.Succ.<init>  Succ(n: Nat)  Natural.scala:7  [constructor]
scala-cardinality — 2 sources, 11 definitions
  stored-value estimates: constructor inputs only; a finite capacity prints as the bound ≤2^b, marked (lossy) when it holds floating point
  ?: no single number here — the reason after the row says what would give one
  1 instantiation-dependent · 8 with more than one value · 2 with one value
Stored-value estimates (constructor inputs; `?` = unresolved)

?   example.Holder[A]  class     Light.scala:11  depends on its instantiation (A)
ε₀  example.Reducer    class     Light.scala:20
ω   example.Cursor     class     Light.scala:16
ω   example.Nat        abstract  Natural.scala:5
ω   example.Succ       class     Natural.scala:7
ω   example.Timeline   class     Light.scala:18
3   example.Light      abstract  Light.scala:3
2   example.Custom     class     Light.scala:5
2   example.Mode       enum      Light.scala:7
1   example.Red        object    Light.scala:4
1   example.Zero       object    Natural.scala:6
```

If a source does not compile, the task fails and the report names it:
`could not read Broken.scala: no compiled classes supplied`.

`cardinalityReportSelected` filters the same report to the methods you name:

```bash
sbt 'cardinalityReportSelected example.Accessor.get'
```

`cardinalityReportOf` runs the report over a library the build does not compile itself. It
takes the library's binary jar, whose compiled classes are read against the project's
dependencies, and its `-sources.jar`, which names and places the rows. It refuses either jar
on its own, and a loose source file too, since that has no compiled classes:

```bash
sbt 'cardinalityReportOf path/to/library.jar path/to/library-sources.jar'
```

`cardinalitySeedSkill` writes a cardinality-reduction skill for coding agents to
`.claude/skills/cardinality-reduction/SKILL.md` (`cardinalitySkillFile`). The skill is filled in
for your build. A second run refreshes the file, but it never overwrites a file the plugin did
not write.

## Mill

For Mill 1.3 or later, load the plugin in `build.mill`'s header and mix
`CardinalityModule` into a `ScalaModule`:

```scala
//| mvnDeps: ["dev.constructive::mill-cardinality_mill1:0.0.6"]
import millcardinality.CardinalityModule

object foo extends ScalaModule with CardinalityModule { def scalaVersion = "3.9.0" }
```

`mill foo.cardinalityReport`, `mill foo.cardinalityReportOf <path>...` and
`mill foo.cardinalityReportSelected <qualified-method-name>...` run the reports, and
`mill foo.cardinalitySeedSkill` writes the skill. Each report goes to `report.txt` under the
command's `out/` directory.

## The calculator alone

```scala
"dev.constructive" %% "scala-cardinality" % "0.0.6"
```

## Versions

The build tool may run on JDK 17, 21 or 25. The calculator reads code compiled by Scala up to
the version its TASTy reader (tasty-query) supports: 0.0.5 and later read Scala 3.0 through
3.10. Classes from a newer Scala show up as `compiled class not read: …` at the end of the
report.

## Next

[Reading a report](reading-a-report.md) explains the rows, the tiers and the moves that
lower a number. [Assumptions](references/assumptions.md) states what every count assumes.
