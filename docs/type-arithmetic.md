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
  holds values that some finite program produced. Above it sits one more tier: a
  function space whose domain and codomain are both infinite is `ε₀`, the least
  fixed point of `α ↦ ω^α`, which caps everything from `ω^ω` upward (§4). Sizes
  are reported as `a·ε₀ + b·ω + n` — contributions counted per tier, plus the
  finite part — so two countable families are plainly `2ω`. Use these readings
  everywhere; do not switch silently.

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

The calculator keeps those laws for the finite counts and reports every sum as a
polynomial `a·ε₀ + b·ω + n` over three tiers (§4). Addition is the **natural sum**:
coefficients are kept, so `Either[String, String]` is `2ω` rather than one
absorbed `ω`. That matches `Either[A, B] ≅ Either[B, A]` and lets two definitions
be compared more finely than "both infinite". Products and powers stay coarse
above the finite tier: `2 * ω = ω`,
so `Either[String, String]` (`2ω`) and `(String, String)` (`ω`) differ even
though the two types are isomorphic. Recursive widening and lazy-type completion
also deliberately lose precision (§8); neither changes general addition.

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

The identities hold for the infinite tiers too: `b^0 = 1`, `b^1 = b`, `1^b = 1`
and `0^b = 0` for every nonzero `b`, `b = ω` included. A finite base over an
infinite domain stays countable (`2^ω = ω` in the ordinal arithmetic of §4), and
an infinite base over an infinite exponent is `ε₀`.

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
  So does `ℵ₀^ℵ₀`, and in every model of ZFC the two are equal:
  `2^ℵ₀ ≤ ℵ₀^ℵ₀ ≤ (2^ℵ₀)^ℵ₀ = 2^(ℵ₀·ℵ₀) = 2^ℵ₀`, with Cantor–Schröder–Bernstein
  giving the bijection.
- This page counts only what a finite program can produce (§1), and there are
  countably many of those. So `String => Boolean`, and every other finite base
  over a countably infinite domain, counts `ℵ₀` — written `ω` in the ordinal
  arithmetic used below.
- It also keeps one **tier** above that, by the ordinal laws of §3 rather than by
  cardinal arithmetic: `ω^ω > ω`, and everything from there up to the least fixed
  point of `α ↦ ω^α` is reported as the `ε₀` tier. So `String => String` is `ε₀`,
  and so is `String => String => String`. `ε₀` is a countable ordinal, so this is
  a *ranking* of function spaces, not a claim that they have different
  cardinalities. It keeps a space between two infinite types apart from one into
  a finite type, and it never claims the two differ as cardinals.
- The **finite subsets** of a countably infinite type are countable under either
  reading; Scala's finite `Set[String]` is `ℵ₀`.
- `ℵ₀^n = ℵ₀` for positive finite `n`, and finite sums and products of countable
  sets remain countable, apart from zero annihilating a product.

Two arguments motivate keeping the distinction, neither of which makes it a
cardinal inequality:

- **Different structure.** With application in the signature, the sentence
  `∃ c₀ c₁. ∀ f ∀ x. f(x) = c₀ ∨ f(x) = c₁` holds for `ℕ => 2` and fails for
  `ℕ => ℕ`. The two spaces are not even elementarily equivalent, and injections
  exist both ways (`f ↦ 0^f(0) 1 0^f(1) 1 …` embeds `ℕ^ℕ` into `2^ℕ`), so any
  strict ordering between them is a convention of this analysis, not a theorem.
- **Constructive models.** In a continuous model — Brouwer's fan theorem,
  Kleene–Vesley function realizability — every function is continuous, `2^ℕ` is
  compact and `ℕ^ℕ` is not, and no bijection between them exists:
  Schröder–Bernstein is not constructively valid, which is exactly where the ZFC
  chain above breaks.

Sizes are therefore written as polynomials over the three tiers:

```text
a·ε₀ + b·ω + n        a, b, n >= 0
```

`n` is the finite component of §2: exact up to 127, then a rounded bit capacity.
Two countable families read `2ω`, one countable family beside three values reads
`ω + 3`, and comparison is lexicographic — the `ε₀` coefficient first, then `ω`,
then the finite part. `2ε₀ + ω` is larger than `ε₀ + ω`, which is larger than
`3ω + 100`.

The article's predicate/powerset shortcut is safe for finite element types. For
infinite types the same exponentiation applies, with the rules above.

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
  `() => A`, an `Option[A]` field, or a function field `D => A`): a field is
  evaluated only when observed, so a value can unfold forever. Infinite values
  exist too, and the type is the greatest fixed point `ν X. F[X]`.

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
| Peano naturals (`Zero` or `Succ`) | `μ X. (1 + X)` | `ω` |
| Finite lists of `A` | `μ X. (1 + A*X)` | `1` if `a=0`; `ω` if `0<a<=ω` |
| Church numerals | `∀ A. (A => A) => A => A` | `ω` |
| Endofunction without a seed | `∀ A. (A => A) => A` | `0` |
| Iteration with an extra argument | `∀ A B. (A => B => A) => A => B => A` | `ω` |

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
| Conaturals (`pred: => Option[CoNat]`) | `ν X. (1 + X)` | `ω`: finite depths and the single infinite one, normalized at completion |
| `LazyList[A]` | `ν X. (1 + A*X)` | `1` if `a=0`; `ω` if `a` is nonempty and finite; `ε₀` if `a=ω` |
| Stream without an end (`head: A`, `tail: => Stream[A]`) | `ν X. (A*X)` | `0` if `a=0`; `1` if `a=1`; `ω` if `a` is finite and `>=2`; `ε₀` if `a=ω` |
| Unfolding that branches per argument (`One(e: => E)`, `Two(i: Int => E)`) | `ν X. (X + (Int → X))` | `ω` under the §4 finite-program reading — a function field is also never demanded while constructing, so `lazy val e = One(Two(_ => e))` and per-`int` choices all unfold further. An empty per-lap label space stays `0`, and a label space that reaches `ε₀` is not demoted |

Compare the eager rows: `μ X. X` is `0` but `ν X. X` is `1`, and a stream without
an end has no finite values at all. With `a>=2` an infinite stream is a function
`ℕ => A`, so it follows §4 exactly: `ω` for a finite `A` (only the streams a
finite program can produce), `ε₀` for a countably infinite `A` (`ω^ω`).

**Lazy-type completion policy (B).** The counts in this table are approximations,
not exact cardinal or ordinal arithmetic. On completing each lazy/coinductive type,
combine its finite-value μ estimate with its infinite contribution. If the total
contains an `ε₀` term, return one `EffectiveEpsilon0`; otherwise, if it contains
an `ω` term, return one `EffectiveOmega`. Purely finite totals, including `0`
and `1`, are unchanged. Here “finite-value” describes finite unfoldings, whose
μ estimate can itself be infinite.

The finite and infinite families still exist; their breakdown is deliberately
discarded at this boundary. Conaturals therefore report `ω`, not `ω + 1`;
`LazyList[Unit]`, `LazyList[Boolean]`, and Scala's `Stream[Boolean]` report `ω`.
`LazyList[String]` and Scala's `Stream[String]` report `ε₀`, not `ε₀ + ω`.
`LazyList[Nothing]` is still `1`, a pure lazy self-wrapper `ν X. X` is still `1`,
and blocked or empty cycles whose total is `0` stay `0`.

This normalization is **only** for completion of each lazy/coinductive type.
It is not a rule of `Size.+`, enclosing sums, source totals, or class/object
signatures: `Either[String, String]` remains `2ω`, and
`Either[LazyList[String], LazyList[String]]` is `2ε₀`, not `ε₀`.
Source and signature aggregates add the completed contributions normally;
`sourceSignature` with two `String => String` methods and a `String` field
still reports `2ε₀ + ω`.

A recursive equation whose estimate keeps growing never reaches a fixed point
under coefficient-preserving addition: the estimate for `μ X. (1 + X)` climbs
`1, 2, 3, …` for ever. The calculator settles it by **widening** the component to
the tier it has grown into, once the growth is recognized as productive, so
`μ X. (1 + X)` is `ω`; `ν X. (1 + X)` also reports `ω` after lazy-type
completion normalizes the finite depths plus the infinite tower. Only a name that can reach
itself through the equations is widened, so however long a chain of forward
references is, it settles exactly, and a widening never turns an `ε₀` estimate
back into `ω`. Widening and lazy-type completion are separate approximations,
not rules of general addition: `+` keeps the infinite-tier coefficients.

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
  [`typeIn`](https://github.com/constructive-programming/scala-cardinality/blob/main/core/src/main/scala/Counter.scala).
  `Counter.source` instead sums the contributions of every definition in a source, so it is not a
  lookup of one chosen ADT, and a definition contributes its **solved** value rather than a fresh
  evaluation of the same syntax — so a recursive definition and a reference to it always agree.
- `Counter.sourceSignature` reports the other reading: the declared types of the members of a
  definition (`Counter.defnSignature` for a single definition). Every typed field, term and method
  contributes the size of its declared type, constructor and enum-case parameters included, so a
  module with two `String => String` methods and one `String` field is `2ε₀ + ω`, while
  `Counter.source` reports that the module itself is a single value. Both readings return the same
  `Size`; neither replaces the other.
- [`Size`](https://github.com/constructive-programming/scala-cardinality/blob/main/core/src/main/scala/Size.scala)
  is `a·ε₀ + b·ω + n`: contributions counted per tier and added componentwise, so
  `Either[String, String]` is `2ω`. Counts up to 127 are exact in the finite part, and
  `FiniteSize(bits)` records only the binary width above that — it stands for a count in
  `(2^(bits-1), 2^bits]`, so `2^32 + 3` becomes `FiniteSize(33)` instead of an exact count, and
  algebraically equal expressions can round differently. The finite coordinate is an estimate
  rather than a natural number: its addition is not associative. `FloatSize` and `DoubleSize` are
  lossy stand-ins for the real types, not consequences of the integral-width rules.
- Multiplication and exponentiation are coarse above the finite tier: after the zero and one
  identities (`p * 0 = 0`, `p * 1 = p`, `p^0 = 1`, `p^1 = p`), an infinite operand is projected to
  its dominant tier. So `2 * ω = ω` and `ω * ω = ω` even though `Either[String, String]` is `2ω`:
  coefficients count additive contributions, not repeated products.
- `EffectiveOmega` conflates unresolved and unsupported types, countable infinity, and some very
  large finite results; `EffectiveEpsilon0` (`ε₀`) is the tier marker above it and is coarse in the
  same way. A result that equals either marker is not by itself a mathematical result, and the
  `ε₀` tier is countable, not uncountable.
- The traversal estimates unions by addition (deduplicating identical syntax only) and
  intersections by minimum, so it cannot see overlap or subtyping. It does not resolve generic
  definitions, and its opaque-type singleton treatment is an approximation. Forward references and
  recursion within a single compilation unit are resolved: `Counter.source` solves the definitions
  as a system of equations by Kleene iteration from the empty type (§8), and abstract traits and
  sealed classes are folded from their concrete subtypes in the same unit. Cycles through
  recognized continuations — holes (`=> X`, `=> Option[X]`, `() => X`), strict `Option[X]` fields,
  and function fields `D => X` with inhabited `D` — also take the greatest fixed point: a
  deterministic cycle adds its per-lap label space raised to ω, any branch (two continuations in
  one arm, an `Option` field beside a hole, a domain with two or more inputs, a sealed parent with
  several continuing children) saturates at ω under the §4 finite-program reading, one program per
  unfolding; `LazyList` and `Stream` follow the same rule. A cycle demanded outright — strict self
  argument, tuple, `Set[X]`, function domain — blocks coiteration and is left at the sound
  least-fixed-point under-count.
- A growing recursive component is widened to its tier, as §8 describes: a name that can reach
  itself and keeps growing is replaced by the tier it has grown into, so the iteration terminates.
  Separately, completion of each lazy/coinductive type combines its μ estimate and infinite
  contribution, then returns one `EffectiveEpsilon0` if present, otherwise one `EffectiveOmega`
  if present, otherwise the unchanged finite total. General addition retains infinite-tier
  coefficients; enclosing sums, source totals, and signatures are not normalized this way.

[`ArticleCardinalitySpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/core/src/test/scala/ArticleCardinalitySpec.scala)
is the article-focused regression suite. Together with
[`SizeSpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/core/src/test/scala/SizeSpec.scala)
and [`CardinalitySpec`](https://github.com/constructive-programming/scala-cardinality/blob/main/core/src/test/scala/CardinalitySpec.scala),
it separates implemented rules from executable **targets**:

| Area | Test contract |
| --- | --- |
| Base types, sums, products, and ADT normal forms | Asserted counts (or explicitly identified bit capacities) |
| Exponentials, currying, distributivity, the `0`/`1` rules, double negation | Asserted against independently known results |
| Finite powersets and finite-list boundary cases | Asserted for empty, singleton, and nontrivial element types |
| Polynomial sums across the three tiers | Asserted: `ω + ω = 2ω`, lexicographic comparison, coarse products and powers, and the zero/one identities |
| Member signatures of definitions | Asserted: fields, methods, constructor and enum-case parameters, and nested definitions summed per tier |
| Parametric identity, projections, composition, contradiction, Option transformations | Pending: needs parametricity, which the type traversal does not model |
| Recursive types (eager) with no base constructor | Asserted: least fixed points of the equation system; `μX.X` counts `0` |
| Recursive types (eager) with a base constructor | Asserted: productive recursion widened to `ω`, and an `ε₀` payload never demoted |
| Lazy (coinductive) recursion through recognized continuations — holes, strict `Option[X]` fields, inhabited function fields `D => X`, sealed-parent pass-throughs | Asserted: deterministic cycles add their label space raised to ω, then lazy-type completion normalizes the combined total (`νX.X` = 1, conaturals = ω, endless `νX.(2·X)` = ω, `LazyList[String]` = `ε₀`); branching cycles saturate at ω (§4 finite-program reading), an empty label space stays `0`, and a chain of consumers sees the settled value |
| Coiteration blocked by a strict self argument (`x: X`) or a mention nested past the recognized forms (tuples, `Set[X]`, function domains, `Either`) | Kept at the least-fixed-point under-count (sound, deliberately not guessed) |
| Union and intersection overlap beyond identical syntax | Pending: needs overlap and subtyping information |
| Functions from countably infinite domains | Asserted at `ω` into a finite codomain and `ε₀` into an infinite one (§4) |
| Containers, rank-N and higher-kinded types, counts under extra laws | Targets with nothing behind them yet |

A pending example is a keyed expectation: it fails today, so specs2 reports it as pending and
prints the count the article derives next to the count the calculator returned. When the rule
is implemented the example starts holding, which specs2 turns into a failure, so support
cannot land behind a stale marker. A test that merely gets an infinity marker back for a
polymorphic or recursive type is not evidence that the rule is implemented: the same marker
also means "unresolved".

Implementation priorities are: keep exact, approximate, and unknown results distinct; apply
the finite algebra and the empty-type identities; resolve names and constructor structure
across compilation units; carry the greatest-fixed-point analysis through `Either`
and deeper-nested lazy continuations, and let a cycle's label space depend on its
own unfolding; then add binding- and variance-aware polymorphic reductions.

Run the focused regressions or the full suite with:

```sh
sbt 'testOnly ArticleCardinalitySpec SizeSpec'
sbt test
```
