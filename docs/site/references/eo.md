# eo

[eo](https://eo.constructive.dev/) is the sister project: an **existential optics**
library for Scala 3, built on [cats](https://typelevel.org/cats/).

One `Optic[S, T, A, B, F]` trait, parameterised over a carrier `F[_, _]`, unifies every
optic family; composition crosses families through `Composer[F, G]` bridges rather than
N² hand-written `.andThen` overloads. A function can demand a capability
(`CanModify[T, String]`) instead of a concrete type, and the caller supplies the optic
that proves it.

`scala-cardinality` shares eo's site pipeline: Laika with the Helium theme, the same
Cloudflare Pages deployment, and the same brand overlay.

- [Documentation](https://eo.constructive.dev/)
- [Source](https://github.com/Constructive-Programming/eo)
