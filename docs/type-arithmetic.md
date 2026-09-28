# Type arithmetic for cardinality

An implementation-oriented condensation of Alex Knvl's
[“Counting type inhabitants” (November 24, 2018)](https://web.archive.org/web/20181221193229/https://alexknvl.com/posts/counting-type-inhabitants.html).
The rules below describe the mathematical target, not a claim that the current
calculator implements every rule. The final section maps them to code and tests.

## 1. What are we counting?

Count **observationally distinct, total, pure values**, not expressions, allocation
identities, or implementations. `1 + 1` and `2` are the same inhabitant; two
functions are the same when no permitted observation distinguishes their results.
Ignore execution time, exceptions, nontermination, side effects, and unsafe casts.
In particular, a throwing expression does not make `Nothing` inhabited.

Write `|A| = a` for the cardinality of type `A`. Write `A ≅ B` for an isomorphism:
functions in both directions whose compositions are identities. Isomorphic types
have equal cardinalities. In the formulas, `0`, `1`, and `2` denote types with
that many values, **not Scala literal types**: Scala's type `3` has one value.

The polymorphic rules additionally require **parametricity**: implementations
cannot inspect a type argument, reflect on it, or invent a value of an arbitrary
type. They must work uniformly for every instantiation.

Important boundaries:

- The article's `Any ≅ 1` uses an abstract existential with no operations that
  reveal its contents. Ordinary Scala `Any` permits observations such as pattern
  matching, so do not hard-code `|Any| = 1` for ordinary Scala semantics.
- Abstract and opaque types are not automatically singletons. Their observable
  cardinality depends on their exposed operations and representation access.
- Finite function spaces can be enumerated. For infinite types, distinguish all
  mathematical functions from functions expressible by finite programs; there
  are only countably many finite programs. This page counts the latter, so a
  finite base over an infinite domain is `ℵ₀`. That follows the constructivist
  approach of §3, and it is the practical reality: a running program only ever
  holds values that some finite program produced. One step is kept above it:
  `ℵ₀^ℵ₀`, a function space whose domain and codomain are both infinite, is `τ`
  (§4). Use these readings everywhere; do not switch silently.

## 2. Base types, sums, and products

| Type or construction | Cardinality | Reason |
| --- | --- | --- |
| `Nothing` | `0` | No total values |
| `Unit`, `EmptyTuple`, a singleton | `1` | One value |
| `Boolean` | `2` | `false` or `true` |
| `Byte`; `Short` or `Char`; `Int`; `Long` | `2^8`; `2^16`; `2^32`; `2^64` | Fixed-width integral values |
| `Either[A, B]` | `a + b` | Disjoint, tagged alternatives |
| `Option[A]` | `1 + a` | `None` plus each `Some(a)` |
| `(A, B)` | `a * b` | Independent choices of both fields |
| `(A, B, C)` | `a * b * c` | Extend the product to all fields |
| ADT with constructors `Cᵢ(fieldsᵢ)` | `Σᵢ Πⱼ \|fieldᵢⱼ\|` | Sum of constructor products |

An empty product is `1`: a constructor with no fields contributes one value.
An empty sum is `0`: a data type with no constructors contributes no values.
For example, `Bar | Baz(Boolean) | Baf(Int)` has `1 + 2 + 2^32` values.
Constructors with an uninhabited field contribute zero.

Useful normalization laws:

```text
a + 0 = a                  a * 0 = 0
a * 1 = a                  a * (b + c) = a*b + a*c
a + b = b + a              a * b = b * a
(a + b) + c = a + (b + c)   (a * b) * c = a * (b * c)
```

These laws reduce nested ADTs to sums of products. Constructor tags matter:
`Either[Boolean, Boolean]` has four values, not two.

**Scala unions are not tagged sums.** For finite types, the cardinality of the union
type `A | B` is `|A| + |B| - |A & B|`: overlapping members must not be counted twice.
Intersection cardinality is not generally `min(a, b)` either; that shortcut needs a
proven subtype relationship, and neither overlap nor subtyping can be inferred from the
two cardinalities alone.

## 3. Functions are exponentials

For fixed, non-polymorphic types:

```text
|A => B| = b^a
```

Choose one output from `B` independently for every input from `A`. This counts
extensional, total functions, not distinct function bodies. For a function with
multiple arguments, first multiply the argument cardinalities.

The zero and one rules are essential, not exceptional failures:

```text
|A => Unit|    = 1^a = 1
|Nothing => B| = 0             including 0^0 = 0
|A => Nothing| = 0^a = 0       when a > 0
|Unit => B|    = b^1 = b
```

The empty-domain rule departs from set-theoretic arithmetic, where `b^0 = 1`
counts the single empty function. Scala is eager: applying a function evaluates
its argument first, and no argument of type `Nothing` can ever be evaluated. A
function from `Nothing` can therefore never run, so it contributes no values,
`Nothing => Nothing` included. This is a constructivist approach: a value counts
only when it can be exhibited in use, and a function is exhibited only by
applying it to an argument someone has actually constructed. This applies to
function types only: `Set[Nothing]` and `Map[Nothing, V]` still hold their one
empty value.

Normalize function types with these isomorphisms:

```text
((A, B)) => C       ≅ A => B => C
A => (B, C)         ≅ (A => B, A => C)
Either[A, B] => C   ≅ (A => C, B => C)
```

Their cardinal forms are `c^(a*b) = (c^b)^a`, `(b*c)^a = b^a*c^a`, and
`c^(a+b) = c^a*c^b`. Do **not** distribute an arrow into a sum in its result:
`A => Either[B, C]` is not generally `Either[A => B, A => C]`.

### Worked finite cases

| Scala type | Calculation | Count |
| --- | --- | --- |
| `(Boolean, Unit)` | `2*1` | `2` |
| `(Boolean, Boolean, Boolean)` | `2*2*2` | `8` |
| `(Nothing, Int)` | `0*2^32` | `0` |
| `(Int, Int)` | `2^32 * 2^32` | `2^64` (same as `Long`) |
| `Either[Boolean, Option[Boolean]]` | `2+(1+2)` | `5` |
| `Boolean => Boolean` | `2^2` | `4` |
| `Option[Boolean] => Boolean` | `2^(1+2)` | `8` |
| `Boolean => Option[Boolean]` | `(1+2)^2` | `9` |
| `(Boolean, Boolean, Boolean) => Unit` | `1^8` | `1` |
| `Boolean => Boolean => Either[Boolean, Boolean] => Boolean` | `2^(2*2*(2+2))` | `65,536` |

The article's last calculation writes `2*2` for the `Either` subexpression.
It happens to equal `2+2` here; the general rule for `Either` is still addition.

## 4. Sets, lists, and infinity

For a **finite** element type with `a` values:

```text
|Setₖ[A]| = choose(a, k) = a! / (k! * (a-k)!)   for 0 <= k <= a
|Setₖ[A]| = 0                                  for k > a
|Set[A]|  = Σₖ choose(a, k) = 2^a
```

Sets ignore order and repeated elements. For a nonempty `A`, a subset is
equivalently represented by its membership predicate `A => Boolean`; for
`Nothing` the empty set still exists while the predicate does not (§3). Thus `Set[Nothing]`, `Set[Unit]`,
`Set[Boolean]`, and `Set[Option[Boolean]]` have `1`, `2`, `4`, and `8` values.
These are abstract sets with observational equality, not object-identity-based
sets.

Lists preserve order and repetition and permit every finite length:

```text
|List[A]| = Σₙ₌₀^∞ a^n
|List[Nothing]| = 1             only Nil
|List[Unit]| = ℵ₀               one list per natural-number length
```

Finite lists over a nonempty finite or countably infinite alphabet are countably
infinite (`ℵ₀`). This is a least-fixed-point/finite-value interpretation, not a
statement about potentially infinite lazy streams; those are greatest fixed points
(§8).

For infinite sets of values, cardinal arithmetic and this page part ways:

- In cardinal arithmetic the full powerset of a countably infinite type, and its
  full space of Boolean predicates, has `2^ℵ₀` values, strictly more than `ℵ₀`.
  So do `n^ℵ₀` for finite `n >= 2` and `ℵ₀^ℵ₀`.
- This page counts only what a finite program can produce (§1), and there are
  countably many of those. So `String => Boolean`, and every other finite base
  over a countably infinite domain, counts `ℵ₀`.
- `ℵ₀^ℵ₀`, as in `String => String`, counts `τ`: one step above `ℵ₀`. This is a
  deliberate distinction, not cardinal arithmetic (where `ℵ₀^ℵ₀ = n^ℵ₀ = 2^ℵ₀`)
  nor the finite-program reading (where it is `ℵ₀`). It keeps
  a function space between two infinite types apart from one into a finite type.
- The **finite subsets** of a countably infinite type are countable under either
  reading; Scala's finite `Set[String]` is `ℵ₀`.
- `ℵ₀^n = ℵ₀` for positive finite `n`, and finite sums/products of countable
  sets remain countable, apart from zero annihilating a product.

The article's predicate/powerset shortcut is safe for finite element types. For
infinite types the same exponentiation applies, with the two rules above.

## 5. Negation and inhabitance

Let `Not[A] = A => Nothing`. With the eager empty-domain rule of §3, negation
is uninhabited for every `A`:

| Condition | Cardinality of `Not[A]` | Cardinality of `Not[Not[A]]` |
| --- | --- | --- |
| `a = 0` | `0` (empty domain) | `0` (empty domain) |
| `a > 0` | `0` (no result) | `0` (empty domain) |

So negation does not detect inhabitance here: `Not[Not[Boolean]]` has no
inhabitants, and neither does `Not[Not[Nothing]]`. The set-theoretic reading,
where `0^0 = 1` makes double negation `1` exactly for nonempty `A`, is
deliberately not the model: under the constructivist approach of §3, a
negation would have to be applied to a constructed `A` to be exhibited, and it
has no result to give back. Decide whether a recursive constructor can produce a
finite value with the fixed-point rule of §8 instead.

## 6. Universal quantification and Yoneda

`∀ A. T[A]` denotes one implementation working uniformly for every type `A`
(Scala 3 polymorphic function syntax includes `[A] => A => A`).
Do not assign a guessed cardinality to `A` and then apply ordinary exponentiation.

Under totality, purity, and parametricity:

```text
∀ X. (F[X], G[X])       ≅ (∀ X. F[X], ∀ X. G[X])
∀ X. Either[F[X], G[X]] ≅ Either[∀ X. F[X], ∀ X. G[X]]
∀ X. A => F[X]          ≅ A => (∀ X. F[X])          if X does not occur in A
```

The sum rule relies on a uniform choice of constructor, not a choice that varies
with the runtime type argument.

**Yoneda rules** eliminate a quantified variable:

```text
∀ X. (A => X) => F[X] ≅ F[A]     if F is a covariant functor
∀ X. (X => A) => F[X] ≅ F[A]     if F is a contravariant functor
```

Variance is a precondition. Function arguments reverse variance; results preserve
it. An arbitrary expression using `X` is not automatically covariant.
Useful corollaries:

```text
∀ X. F[X] ≅ F[Nothing]          if F is covariant
∀ X. F[X] ≅ F[Unit]             if F is contravariant
∀ X. C    ≅ C                   if X does not occur in C (phantom)
```

### Polymorphic regression targets

Arrows below associate to the right and quantifiers cover the whole following type. The table
uses the article's `∀` notation; the Scala 3 spelling of `∀ A. A => A` is `[A] => A => A`.

| Type | Count | Interpretation |
| --- | --- | --- |
| `∀ A. A => A` | `1` | Identity only |
| `∀ A B. ((A, B)) => A` | `1` | First projection only |
| `∀ A. ((A, A)) => A` | `2` | Select either input |
| `∀ A. ((A, A)) => (A, A)` | `4` | Independently select each output |
| `∀ A B C. (A => C) => B => A => C` | `1` | Apply the function, ignore `B` |
| `∀ A B C. (A => B) => (B => C) => A => C` | `1` | Composition |
| `∀ A B. (A => B) => B => A => B` | `2` | Apply the function or return the supplied `B` |
| `∀ A B C. (A => B) => B => A => C` | `0` | Cannot manufacture `C` |
| `∀ A. (Nothing => A) => A => Nothing` | `0` | Cannot manufacture `Nothing` |
| `∀ A B. ((A => Nothing, B)) => (Boolean, B)` | `0` | `A => Nothing` is empty for every `A` (§5) |
| `∀ A. (A => Nothing) => Nothing` | `0` | No universally available `A` |
| `∀ A. (A => Nothing) => A => Nothing` | `0` | No contradiction can be supplied (§5) |
| `∀ A. ((A => Nothing, A => Nothing)) => (A => Nothing)` | `0` | Neither can a pair of them |
| `∀ A. Option[A] => Option[A]` | `2` | Identity or always `None` |
| `∀ A B. (A => B) => Option[A] => Option[B]` | `2` | Ordinary map or always `None` |

Instantiation is **not** a general bound on a polymorphic count: identity has
one polymorphic inhabitant but `Boolean => Boolean` has four; the two polymorphic
projections collapse to one at `A = Unit` or `A = Nothing`.

## 7. Representable functors, containers, and laws

A representable functor has `F[X] ≅ R => X`. For example, `Unit`, `X`, and
`(X, X, X)` have position types `R = 0`, `1`, and `3`. Yoneda gives:

```text
∀ X. F[X] => G[X] ≅ G[R]                     for covariant G
```

A container is a sum over shapes, with a position type for each shape:
`F[X] ≅ Σᵢ (Rᵢ => X)`. Generalizing:

```text
∀ X. F[X] => G[X] ≅ Πᵢ G[Rᵢ]                for covariant G
```

For `Option`, the shapes have zero or one positions, so
`∀ X. Option[X] => Option[X] ≅ Option[0] * Option[1]`, with `1*2 = 2` inhabitants.
For `List`, shapes are lengths `n` with `n` positions.

The article's list-map signature becomes `Πₙ List[Fin n]`: for each input length,
choose a finite list of input indices, allowing reordering, duplication, or
discarding. Its filter signature becomes
`Πₙ (Set[Fin n] => List[Fin n])`: predicate answers can also guide that choice.
The signatures alone do not require ordinary `map` or `filter` behavior.

Additional **laws** reduce the space of implementations. For example,
`map(identity) = identity` rules out the always-`None` implementation of Option
map. A lawful-functor count is a different question from a signature-only count;
do not silently impose such laws. The article does not give a general algorithm
for counting implementations subject to arbitrary laws.

## 8. Recursive types are fixed points: least when eager, greatest when lazy

The evaluation strategy of the recursive position decides which fixed point a
recursive type denotes. Do not merely solve the numeric equation `x = F(x)`;
that can have multiple solutions, and the least and greatest ones differ exactly
by the infinite values.

- **Eager recursion** (a strict field, the default): constructing a value
  evaluates its fields first, so construction must bottom out. Only finite values
  exist, and the type is the least fixed point `μ X. F[X]`.
- **Lazy recursion** (a by-name `=> A`, a `lazy val`, a `LazyList`, a thunk
  `() => A`): a field is evaluated only when observed, so a value can unfold
  forever. Infinite values exist too, and the type is the greatest fixed point
  `ν X. F[X]`.

A recursive type is coinductive only through its lazy positions: a cycle through
at least one lazy occurrence admits infinite values, while recursion through
strict positions alone stays inductive. `case class Rose(label: Int, kids:
LazyList[Rose])` has trees of infinite depth and width; `case class Tree(l: Tree,
r: Tree)` has no values at all.

### Eager recursion: least fixed points

Write `μ X. F[X]` for the least fixed point: values built by finitely many
constructor applications.

For the covariant inductive constructions considered in the article:

```text
μ X. F[X] ≅ ∀ X. (F[X] => X) => X
μ X. F[X] is inhabited iff F[Nothing] is inhabited
```

Examples:

| Construction | Equation / encoding | Count |
| --- | --- | --- |
| Wrapper that requires another wrapper | `μ X. X` | `0` |
| Peano naturals (`Zero` or `Succ`) | `μ X. (1 + X)` | `ℵ₀` |
| Finite lists of `A` | `μ X. (1 + A*X)` | `1` if `a=0`; `ℵ₀` if `0<a<=ℵ₀` |
| Church numerals | `∀ A. (A => A) => A => A` | `ℵ₀` |
| Endofunction without a seed | `∀ A. (A => A) => A` | `0` |
| Iteration with an extra argument | `∀ A B. (A => B => A) => A => B => A` | `ℵ₀` |

Church numerals include zero iterations (`identity`), then one application, two,
and so on. Without a seed or base constructor, recursion alone cannot make a
total finite value.

A productive recursive constructor plus a base constructor gives arbitrarily
large values. Concluding **countably** infinite additionally needs countably many
constructor/label choices and finite arity. Do not generalize the article's
informal infinity argument to infinitely branching trees or uncountable labels.
An impossible recursive branch, such as `Nothing * X`, adds no values.

### Lazy recursion: greatest fixed points

Write `ν X. F[X]` for the greatest fixed point: every value that can be observed
one constructor at a time, including values that never bottom out. Laziness does
not change the count of a non-recursive use: `=> A` behaves as `Unit => A`, whose
cardinality is `a^1 = a` (§3). It changes only what recursion can build.

For the covariant coinductive constructions:

```text
ν X. F[X] ≅ ∃ X. (X, X => F[X])      a seed and a step that unfolds it
ν X. F[X] is inhabited iff F[Unit] is inhabited
```

The inhabitance rule is the dual of the eager one: a constructor whose
non-recursive fields are all inhabited can be repeated forever, so the recursive
fields no longer need a base case.

| Construction | Equation | Count |
| --- | --- | --- |
| Lazy wrapper that requires another (`next: => Loop`) | `ν X. X` | `1`: the value `lazy val l: Loop = Loop(l)` |
| Conaturals (`pred: => Option[CoNat]`) | `ν X. (1 + X)` | `ℵ₀`: every finite depth, plus one infinite one |
| `LazyList[A]` | `ν X. (1 + A*X)` | `1` if `a=0`; `ℵ₀` if `a` is finite and `>=1`; `τ` if `a=ℵ₀` |
| Stream without an end (`head: A`, `tail: => Stream[A]`) | `ν X. (A*X)` | `0` if `a=0`; `1` if `a=1`; `ℵ₀` if `a` is finite and `>=2`; `τ` if `a=ℵ₀` |

Compare the eager rows: `μ X. X` is `0` but `ν X. X` is `1`, and a stream without
an end has no finite values at all. With `a>=2` an infinite stream is a function
`ℕ => A`, so it follows §4 exactly: `ℵ₀` for a finite `A` (only the streams a
finite program can produce), `τ` for a countably infinite `A` (`ℵ₀^ℵ₀`).

## 9. Higher kinds and rank-N types

The article only sketches this area. One target, for arbitrary type constructors
`F` with no covariance or other operations assumed, is:

```text
∀ F. F[A] => F[B] ≅ (A = B)
```

This requires type-equality evidence: for fixed equal types there is the identity;
for distinct types there is no general conversion. Nested quantifiers and
higher-kinded parameters require their own binding/variance-aware analysis.
Neither sampling instantiations nor treating an unknown constructor as infinite
implements these rules.

## 10. Implementation status and test contract

The current API and representation have important limits:

- `Counter.type` counts a single parsed type, through
  [`typeIn`](https://github.com/constructive-programming/scala-cardinality/blob/main/src/main/scala/Counter.scala).
  `Counter.source` instead sums the contributions of every definition in a source, so it is
  not a lookup of one chosen ADT. Tests of a single ADT use an isolated declaration so that
  unrelated definitions cannot inflate the count.
- [`Size`](https://github.com/constructive-programming/scala-cardinality/blob/main/src/main/scala/Size.scala)
  keeps counts up to 127 exactly, in `TinySize`.
  `FiniteSize(bits)` records only the binary width: it stands for a count in
  `(2^(bits-1), 2^bits]`, so `2^32 + 3` becomes `FiniteSize(33)` instead of an exact count,
  and algebraically equal expressions can round differently. `FloatSize` and `DoubleSize`
  are lossy stand-ins for the real types, not consequences of the integral-width rules.
- `EffectiveOmega` conflates unresolved and unsupported types, countable infinity, and some
  very large finite results; `EffectiveTau` (`τ`) is coarse in the same way. A result that
  equals either marker is not by itself a mathematical result.
- The traversal estimates unions by addition (deduplicating identical syntax only) and
  intersections by minimum, so it cannot see overlap or subtyping. It does not resolve
  forward references, generic definitions, or recursion, and its opaque-type singleton
  treatment is an approximation.
- `Size.pow` reports a finite base over an infinite exponent as `EffectiveOmega`, the
  finite-program reading of §1 and §4, and an infinite base over an infinite exponent as
  `EffectiveTau`.

[`ArticleCardinalitySpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/src/test/scala/ArticleCardinalitySpec.scala)
is the article-focused regression suite. Together with
[`SizeSpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/src/test/scala/SizeSpec.scala)
and [`CardinalitySpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/src/test/scala/CardinalitySpec.scala),
it separates implemented rules from executable **targets**:

| Area | Test contract |
| --- | --- |
| Base types, sums, products, and ADT normal forms | Asserted counts (or explicitly identified bit capacities) |
| Exponentials, currying, distributivity, the `0`/`1` rules, double negation | Asserted against independently known results |
| Finite powersets and finite-list boundary cases | Asserted for empty, singleton, and nontrivial element types |
| Parametric identity, projections, composition, contradiction, Option transformations | Pending: needs parametricity, which the type traversal does not model |
| Recursive types with no base constructor | Pending: needs least-fixed-point analysis |
| Lazy (coinductive) recursive types | Targets with nothing behind them yet; `LazyList` is counted like `List` today |
| Union and intersection overlap beyond identical syntax | Pending: needs overlap and subtyping information |
| Functions from countably infinite domains | Asserted at `ℵ₀` into a finite codomain and `τ` into an infinite one (§4) |
| Containers, rank-N and higher-kinded types, counts under extra laws | Targets with nothing behind them yet |

A pending example is a keyed expectation: it fails today, so specs2 reports it as pending and
prints the count the article derives next to the count the calculator returned. When the rule
is implemented the example starts holding, which specs2 turns into a failure, so support
cannot land behind a stale marker. A test that merely gets an infinity marker back for a
polymorphic or recursive type is not evidence that the rule is implemented: the same marker
also means "unresolved".

Implementation priorities are: keep exact, approximate, and unknown results distinct; apply
the finite algebra and the empty-type identities; resolve names and constructor structure;
analyze recursive dependencies as least fixed points, or greatest ones through lazy
positions; then add binding- and variance-aware polymorphic reductions.

Run the focused regressions or the full suite with:

```sh
sbt 'testOnly ArticleCardinalitySpec SizeSpec'
sbt test
```
