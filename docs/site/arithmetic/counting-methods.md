# Counting Methods

The second number a report gives is the count of canonical **pure, total, parametric
implementations** a method or constructor admits with everything in scope — what it can
access, capture or call. `def choose[A](x: A, y: A): A` has 2 (`x` or `y`). A constructor
is counted as the product of its parameters' value spaces: `Pair[A](x: A, y: A)` has 4
ways to build its product, `IntArrBuilder(initialCapacity: Int)` is `2^32`, and
`Slice(arr: Array[Any], offset: Int, length: Int)` is `ω`.

This is a different question from the stored-value estimate on the data type: `Pair[Int]`
still holds `2^64` values. A parameter whose space the estimate cannot read whole — a type
parameter nobody supplied, a name outside the sources — stays with the shape model, which
names it.

The rules below follow Alex Knvl's
[Counting type inhabitants](https://web.archive.org/web/20181221193229/https://alexknvl.com/posts/counting-type-inhabitants.html)
(24 November 2018). `ReferenceInhabitantsSpec` is the executable form of the examples.

## The rules

- Inhabitants are counted up to **observational equivalence**: renaming, beta/eta
  expansion and a redundant `case` analysis do not make two expressions different
  inhabitants.
- `Void ~ 0`, `() ~ 1`, `Bool ~ 2`, `Int ~ 2^32`. On the method side a ground type is a
  supplied one case-split into `|G|` identical branches (one goal raised to `|G|`), so
  `Int -> Int ~ 2^(2^37)` exactly. `String` is ω and its split saturates at 0, 1 or ω
  ([ground scalar types](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/ground-scalar-types.md)).
- A coproduct adds: `Either a b ~ |a| + |b|`, and `Option a ~ 1 + |a|`.
- A product multiplies: `(a, b) ~ |a| * |b|`; the empty product is `1` and an empty
  alternative is `0`.
- A function is an exponential: `a -> b ~ |b| ^ |a|`, with `1 -> a ~ a`, `0 -> a ~ 1` and
  `a -> 0 ~ 0` for inhabited `a`.
- Universal quantification counts a **free** type parameter as one atom, never as a type of
  unknown finite size. `∀a. a -> a ~ 1`, `∀a. (a, a) -> a ~ 2`, `∀a. (a, a) -> (a, a) ~ 4`,
  `∀a. (a, b) -> a ~ 1`.
- A polymorphic value's arguments are consumed as a product: `(a, b) -> c ~ a -> b -> c`
  and `a -> (b, c) ~ (a -> b) * (a -> c)`.
- A recursive type is a least fixed point: `∀a. (a -> a) -> a -> a ~ μx. 1 + x ~ ℵ₀`, and
  only when a starting inhabitant exists: `∀a. (a -> a) -> a ~ 0`, `∀a. (a, a) -> a ~ 2`.
- Negation: `x -> 0` has at most one inhabitant, and `Set a` is a predicate `a -> Bool`, so
  `|Set a| = 2^|a|`.

`ω` is a productive cycle: `def use[A](x: A, step: A => A): A` can return `x`, `step(x)`,
`step(step(x))`, … — countably many. A cycle with no starting inhabitant is `0`, not `ω`.
An enclosing value, a callable producer and a product projection all count as captures,
and a type parameter's identity is per binder, so a shadowed `A` is not an outer `A`.

What the analysis cannot read is `?`, with the reason. The section header sums the reasons
into a triage list:

```
Generic method / constructor implementation cardinalities
  53 signatures: 41 finite · 3 countably infinite · 9 unresolved
  8 unresolved on: unresolved type
  1 unresolved on: given environment not resolved

4  example.Pair.<init>  Pair[A](left: A, right: A)  Light.scala:11  [constructor]
1  example.Accessor.get  get[X, A](fa: (X, A)): A  Accessor.scala:18  [method]
    captures: tupleAccessor
```

## Repeated arguments

`Type.Repeated(T)` in a method or constructor parameter is a finite, possibly empty
sequence of `T`, not one supplied `T`: `Σ_{n≥0} Xⁿ`. Parameter clauses keep their ordering
and binder identities. This is a pure, total implementation count, not a stored-value
estimate for Scala's `Seq` representation.

- A sequence whose element shape is structurally empty has just the empty sequence, so it
  normalizes to `Unit`. Structural emptiness includes `Nothing`, sums of empty
  alternatives, and products with an empty field.
- A structurally singleton result (`Unit`, products of singleton results, one-alternative
  singleton sums, or functions returning singleton results) has one implementation even
  when repeated arguments are supplied.
- Sequence introduction has the canonical constructors `Nil` and `Cons`. Without a way to
  produce an element, only `Nil` is available. With a productive element construction, the
  productive-cycle proof gives `ω`: sequences of length 0, 1, 2, … are distinguishable.

| Signature or shape goal | Count | Reason |
|---|---|---|
| `def ignore[A](xs: A*): Unit` | `1` | Only the `Unit` result is observable |
| `def pick[A](x: A)(xs: Nothing*): A` | `1` | `xs` is necessarily empty |
| `case class Empty(xs: Nothing*)` constructor | `1` | The only field sequence is empty |
| Sequence-of-`A` shape goal with no `A` producer | `1` | Only `Nil` |
| Sequence-of-`A` shape goal with `x: A` | `ω` | Productive `Cons` cycle, distinct lengths |
| `def use[A](x: A)(fs: (A => A)*): A` | `?` | Arbitrary sequence elimination is outside the fragment |
| `case class Many[A](xs: A*)` constructor | `?` | Arbitrary input-sequence transformations are not enumerated |

The solver does not enumerate total length tests, safe element selection, mapping, folds or
recursive sequence elimination. When a supplied sequence can be present in a non-singleton
goal, it reports:

> Repeated-argument sequence elimination is unsupported (length tests, selection and folds)

Element types still use the ordinary resolver and environment checks. General `Seq[T]`
names, aliases and collection methods are not resolved by this feature.

## What the reference supports that the solver does not

- Rank-N quantification and Yoneda-style reasoning: a plain higher-kinded binder is read as
  a rigid, uninterpreted constructor (`F[A]` and `F[B]` are distinct base types; see
  [higher-kinded binders](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/higher-kinded-binders.md)),
  so `∀f. f A -> f B` counts 0 without evidence and 1 with `A =:= B`, but no functor law is
  assumed and variant or bounded constructor binders are refused rather than guessed.
- Laws: a functor or optic law is an extra condition on inhabitant counts and is never
  assumed here.
- Non-regular types such as `Set` beyond the `2^|a|` identity, and types whose inhabitants
  depend on instance methods the model cannot see.

## Where the solver is deliberately narrower

- It counts **canonical pure, total parametric constructions**: no divergence, effects,
  casts or unsafe equality.
- `Any`, `AnyRef`, `Matchable` and `AnyVal` are read by their place in their own arrow: `1`
  in an input, ω in an output. A union is a sum only where a class test is its eliminator
  ([top types and unions](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/top-types-unions.md)).
- An opaque callable is unconstrained, so applying it repeatedly is countable
  (`∀a. (a -> a) -> a -> a ~ ℵ₀`); a callable the source set defines is a specific
  inhabitant of its type, whose values a total parametric body can only draw from the
  environment.
- What the fragment cannot decide is `?` with a reason, never a finite guess and never an
  invented infinity.
