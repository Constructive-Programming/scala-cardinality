# scala-cardinality

`scala-cardinality` parses Scala source and reports how many values the types in it can
hold.

That number is a design smell detector. A type with few inhabitants leaves a program
little room to be wrong: `Unit` has one value, `Boolean` two, `Option[Boolean]` three,
`Long` 2^64, and `String` as many as you can type. Large or unbounded cardinality is
where runtime checks and guesswork come from — see the repository
[README](https://github.com/constructive-programming/scala-cardinality#readme) for the
longer version of that argument.

## Counting a type

```scala
Counter.source("enum Color { case Red, Green, Blue }".parse[Source].get)
// TinySize(3)

Counter.`type`(dialects.Scala3("Either[Boolean, Option[Boolean]]").parse[Type].get)
// TinySize(5)

Counter.source("case class Pixel(shade: Byte, on: Boolean)".parse[Source].get)
// FiniteSize(9)   — 2^8 * 2, held as a 9-bit capacity

Counter.`type`(dialects.Scala3("Either[String, String]").parse[Type].get)
// 2ω              — two countable alternatives, kept as coefficients

Counter.sourceSignature("object S { def f(s: String): String = s; def g(s: String): String = s; val n: String = x }".parse[Source].get)
// 2ε₀ + ω         — two ε₀-tier methods and one ω-tier field
```

The first four counts are value spaces of types; the last is a member signature: what a
definition declares, summed per tier, so two large modules can still be compared. A definition
contributes its solved value once, and a recursive component that keeps growing is widened to
the tier it has grown into rather than counted round by round.

Lazy types have an additional approximation at completion: combine the finite-value μ
estimate and infinite contribution, then report one `ε₀` if present, otherwise one `ω`
if present, leaving purely finite totals unchanged. `LazyList[Unit]`, `LazyList[Boolean]`
and `Stream[Boolean]` therefore report `ω`; `LazyList[String]` and `Stream[String]`
report `ε₀`. Finite and infinite families still exist, but their breakdown is deliberately
discarded at this boundary, not equated by exact cardinal or ordinal arithmetic.
General addition is unchanged: `Either[LazyList[String], LazyList[String]]` reports
`2ε₀`, and source totals and class/object signatures still add contributions normally.
See [lazy recursion](type-arithmetic.md#lazy-recursion-greatest-fixed-points) for the
finite and empty boundary cases.

These are examples, not `mdoc` fences: this build cannot run `mdoc` (see
[the plugin notes](https://github.com/constructive-programming/scala-cardinality/blob/main/project/plugins.sbt)).
The behaviour they show is pinned by
[`ArticleCardinalitySpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/src/test/scala/ArticleCardinalitySpec.scala)
instead, which fails when a count changes.

## Where to go next

- [Type arithmetic](type-arithmetic.md) — the rules behind those numbers, condensed from
  Alex Knvl's *Counting type inhabitants*, together with the calculator's current limits
  and the tests that pin each rule down.
- [Source, tests and quality gates](https://github.com/constructive-programming/scala-cardinality)
  — the README documents the toolchain (`scalafmt`, `scalafix`, scoverage, stryker4s,
  CodeScene) and how to run it.
