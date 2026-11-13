# cardinality

**Build development cycles that reduce complexity.**

A type is a state space. Objects and values fit into types, with simple rules.

`Unit` 1 · `Boolean` 2 · `Option[Boolean]` 3

`Long` 2^64 · `String` ω · `(String, String)` ω²

`Map[Char, Boolean]` 3^(2^16) · `Char => Boolean` 2^(2^16).¹

Sums add, products multiply, functions are exponentials. Every value a type admits is a
state the program must handle — or can wrongly produce. The calculator puts a number on
that space inside your build, so each change can be judged by whether it shrank the
space or grew it.

## The loop

### Measure

Add the plugin to `project/plugins.sbt`:

```scala
addSbtPlugin("dev.constructive" % "sbt-cardinality" % "0.0.6")
```

and run it:

```bash
sbt cardinalityReport
```

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

Every row is a definition, its number, and the reason when the number is not exact. The
same report is written to `target/cardinality/report.txt`, so a build can keep it, diff
it, or post it as an artifact.

### Narrow

Replace the type, not the test:

- `String` → an `enum` of the cases the domain actually has.
- Three `Boolean`s → a sum of the legal combinations.
- A sentinel value → `Option` or a dedicated case.
- A free-form record → a capability the code actually needs.

The number falls, and the compiler keeps it fallen: a state the type cannot represent
cannot be built, tested, or hallucinated.

### Enforce

Diff `report.txt` in review and CI. A change that widens a type shows the new number next
to the old one; a change that narrows it is the improvement, in one line. The
[reading a report](reading-a-report.md) page has the row anatomy and the moves that lower
a number.

## What the loop buys

- **For a person**: a type is a state space; smaller is easier to grapple with.
- **For a reviewer**: the diff is the argument, not a style opinion.
- **For an agent**: fewer representable states means fewer invalid states to invent. The
  count is a machine-checkable target, and the compiler enforces the result.

## Ergonomics

For one type, with no build plugin:

```scala
Counter.`type`(dialects.Scala3("Either[Boolean, Option[Boolean]]").parse[Type].get)
// TinySize(5)
```

For a source, `Counter.source` sums the value spaces of every definition it introduces;
`Counter.sourceSignature` sums what their members declare. See
[counting methods](arithmetic/counting-methods.md) for the other number a report gives:
the implementations a signature admits.

Current version: 0.0.6.

## Further Reading

- [Getting started](getting-started.md) — install for sbt 2, sbt 1 and Mill, and run the
  first report.
- [Reading a report](reading-a-report.md) — what each row, tier and `?` means, and the
  narrowing moves that lower a number.
- [Counting data](arithmetic/counting-data.md) — the stored-value reference: sums, products,
  exponentials, collections, recursion and polymorphism.
- [Counting methods](arithmetic/counting-methods.md) — implementations a method or
  constructor admits, including repeated arguments.
- [References](references/index.md) — the assumptions every count makes, the closed world, the
  approximations, and the papers and research behind the model.

---

¹ The report prints finite capacities as bounds (`≤2^65536`) and keeps products above the
finite tier coarse (`(String, String)` prints `ω`). The arithmetic above is the target; a
bound is not a licence to stop looking.
