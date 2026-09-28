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
```

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
