# Ground scalar types and exact symbolic counts

The method-side solver reads `Byte`, `Short`, `Char`, `Int`, `Long`, `Float`, `Double` and
`String` as **transparent** types: closed enumerations of literal values. This page states the
rule, its derivation, the counts it produces and where it stops.

## The rule

| Type | Values | Size |
|---|---|---|
| `Byte`, `Short`, `Int`, `Long` | two's-complement words | 2^8, 2^16, 2^32, 2^64 |
| `Char` | UTF-16 code units, lone surrogates included | 2^16 |
| `Float` | binary32 modulo `Float.equals` | 2^32 − 2^24 + 3 |
| `Double` | binary64 modulo `Double.equals` | 2^64 − 2^53 + 3 |
| `String` | finite sequences of `Char` | ω |

Two operations make a ground type transparent: **(T1)** every value is a closed expression (a
literal, `Float.NaN`, `-0.0f`), and **(T2)** the type's own equality is total. Equality on a ground
type is not the "unrestricted runtime type/equality test" the [contract](../plans/v1.md) excludes —
that exclusion is about tests on abstract and top types. Every other total operation (arithmetic,
comparison, conversion, `toString`) is definable from T1 and T2 by case analysis, so no count below
depends on which of them a body uses. Reference identity, unguarded partial operations, locale- or
charset-dependent conversions and raw NaN bits stay excluded.

`Float` and `Double` are counted under `equals`: IEEE `==` is not reflexive (NaN ≠ NaN), and raw
NaN payloads may change on a copy on some platforms (the JDK documents this for
`Float.intBitsToFloat`; recalled, not re-inspected here), so the 2^24 − 2 (2^53 − 2) NaN patterns
are one value while the two zeros stay two. `String` is idealized as the free monoid over `Char`,
the same idealization as `List` and the stored-value side's `String ~ ω`; the JVM's `Int.MaxValue`
length bound would make every String count an unreadable finite number.

## Derivation

Write `#(Γ ⊢ τ)` for the implementations of goal `τ` in environment `Γ`.

- **Literal goal.** With no transparent value in `Γ` (they are split first) and no producer of a
  transparent result (those are refused, below), a `G`-valued expression can only be a literal:
  atoms have no observations. So `#(Γ ⊢ G) = |G|`, a rule weighted by the type's size.
- **Uniform split.** For `x : G` with `n` values, an implementation is a decision tree on `x` (T2);
  in the branch `x = c`, `x` is the literal `c`, which the empty environment builds (T1), so the
  branch is an implementation of `Γ ⊢ τ`. Every `n`-tuple of branch implementations is a finite
  program `x match { case c₁ => t₁ … }`, and tuples differing at `cᵢ` are told apart by the input
  `cᵢ`. Hence `#(Γ, x:G ⊢ τ) = #(Γ ⊢ τ)^n`. The branches are the same goal, so the solver makes one
  memoised child raised to `n`. For the countable `String`: `c^ω` is 0, 1 or ω — 0 when no branch
  exists, 1 when all agree, and for `c ≥ 2` the programs `if x == ℓₖ then u else v` give ℵ₀ while
  finite programs give at most ℵ₀ (register D2).
- **Closed values.** A value whose shape mentions no atom (a ground value, `Int => Int`, a declared
  `def n: Int`, a closed sum or sequence) only ever yields closed values, each a literal the empty
  environment builds. It splits the same way, with exponent its value space: products multiply, sums
  add, `|A => B| = |B|^|A|`, a sequence is ω (1 if its element is empty). So `def f(g: Int => Int,
  x: Int): Int` is the finite `(2^32)^(2^(2^37)·2^32) = 2^(2^(2^37 + 37))`, never the ω a producer
  cycle would claim. An empty closed value (`Int => Nothing`) makes the context absurd: 1.
- **One value per member.** The split multiplies exponents, so every closed value in the
  environment must be one independent supplied value. A declared `def n: Int` is one only while a
  subclass supplies it: in an object, a given or a final concrete class every inherited member is
  implemented (Scala requires it), so it is a value the template's own environment fixes and is
  dropped, as a concrete member written there is; a `val` or case-class field is the member it
  implements, already bound; a member restated by the template or reached along several parents is
  one member, bound once. An open template that implements an inherited declaration (a subclass
  may still override it) and one member read with two shapes (a `def` and a `val` of one name)
  are refused until capability records read them. Each enclosing template is its own receiver:
  `Outer.this.n` and `Inner.this.n` are two members, each bound under its own template's type
  arguments ([environment readings](environment-readings.md) §3), so `def f: Boolean` beside two
  free `n: Boolean` is `2^(2·2) = 16`. Callables are keyed by the declarations their parameter
  types resolve to (environment readings §3): `def g(x: Int): X` in two parents of `C[X]` is one
  member (two abstract members with one signature merge), so `def f(x: Boolean): X` there is
  `|Int|^|Bool| = 2^64`, not `(2 · 2^32)^2`, and so is a restatement two `extends` levels up, read
  under the whole chain's substitution; parameters that resolve differently are overloads, two
  members (`g(x: Int): Int` and `g(x: Long): Int` beside `def f: Boolean`: `2^(2^(2^37 + 2^69))`).
- **Corollary.** Over finite closed types only, `#(Γ ⊢ τ) = |τ|^Π|Γᵢ|`, the set-theoretic count
  (type-arithmetic §3): `def f(x: Int): Int` is `2^(2^37)`, `def f(c: Char): Boolean` is
  `2^(2^16)`.

## Exact symbolic counts

A count of up to 65,536 bits stays `Count.Finite(BigInt)`. Above that it is `Count.Symbolic`, an
exact `Natural`: literals, powers, products and sums in a normal form (literal bases reduced to a
power of two and an odd root, equal bases merged, coefficients absorbed into equal bases, like
terms merged, factors and terms sorted). Every symbolic node is at least 2, so the solver's
0 / 1 / ≥ 2 tests are exact. A power of two from 2^8 up renders `2^k`, a literal wider than 100
bits as its odd root's power times its power of two where that is shorter (`4294967297^4`,
`3·2^200`), and a compound operand is parenthesized: `2^(2^37)`, `4294967297^(2^32)`,
`2^(2^137438953509)`.

The representation is bounded, not the magnitude: a literal wider than 65,536 bits that no power
of two can carry, more than 256 nodes, or a power tower higher than 8 is refused with
`Inhabitation arithmetic budget exceeded (symbolic form: …)` (register D8). Two equal values built
through an odd base wider than 4,096 bits, or through a distributed product, may differ in form;
nothing the solver builds reaches either.

## Constructors

A constructor counts the product of its positions' value spaces. A **closed** position is a value
space, never a choice of supplied values: `P3(x: Int, y: Option[Int])` is exactly
`2^64 + 2^32` (the estimate's bound was `2^65`), and a position is `|T|`, never `|T|^|T|`, whatever
other packages declare under the same name (the estimate reads it where the constructor is
written — [stored-value-names.md](stored-value-names.md)). `Float` and `Double`
positions are exact too (`P(x: Double)` is `2^64 − 2^53 + 3`): the method side counts `equals`
classes, while the definition's stored-value row keeps the estimate's lossy capacity
(`2^64 (lossy)`) — two views of one type, documented apart.

An infinite closed position keeps the estimate's tier (`Reducer(run: String => String)` stays ε₀),
so `R(run: C => String)` with `C(n: String)` is ε₀ whether or not another package also declares a
`C`; the solver's finite programs would saturate the same space at ω, which under-states the tier.
Where the estimate gives no reading and names no reason, the factor is the size algebra's own
reading of the closed shape (products multiply, sums add, `|B|^|A|`, sequences ω). Where it
declines the position for a reason — an import it does not follow, a wildcard over a namespace no
source supplies, a family it does not sum — the row is `?` with that reason: the method side's
value space would have nothing to agree with.

The two readings must agree: the size algebra must read the method side's closed type as the
estimate reads its own — the same count where both are exact (up to 127), the same capacity width
above — and an infinite space needs an infinite estimate (and vice versa). A literal merely within
the estimate's capacity is not enough: in `Dep(q: Other)(val x: q.X)` the method side reads `q.X`
through an object `q` (`X = Boolean`, 2) while the estimate reads the parameter's `Other.X = Int`
(2^32), and in `object O extends Base { final case class P(x: Int) }` the method side reads the
inherited `Base.Int = Boolean` (2) while the estimate, which does not bind inherited type members,
reads the builtin (2^32) — the same two numbers, opposite readings right. Otherwise one side read
a different type and the position is refused (`constructor position readings disagree: x (value
space 2, estimate 2^32)`). Rounding is the only way one type reads to two capacities (sums round
by `max(bits) + 1`, not associatively), and it refuses rather than guesses. A symbolic exact space
is at least 2^65,536 (every smaller value is a literal), so it agrees only with an infinite
estimate, which may saturate a power that wide, or a capacity of at least 65,536 bits
(`P(p: Char => Boolean)` is `2^(2^16)` against `2^65536`).

Only the constructor's own positions are value spaces. The parameters of an enclosing template
are captures, bound like a method's: `Inner(a: A)` inside `Outer[A](val n: Int, val g: Int => A)`
fills its slot from `a` or `g(c)` for each `n`, `(1 + 2^32)^(2^32)`.

A position with no type parameter is a value space even when it is not closed. Beside a singleton,
evidence or existential (`p: (Inner.type, C)`) the closed algebra cannot read it, and the shape
model would count the functions of the position's own value (`(2^8)^(2^8)` for `|C| = 2^8`); it
takes the estimate's reading (`W(p: (Inner.type, C))` is the estimate's `2^8`), and the row is `?`
where the estimate declines it. A position whose name the method side refuses as a shadowed
builtin (below) is refused with that reason: the estimate reads the builtin, the same misreading.

The stored-value algebra counts a function from `Nothing` as 0 (it can never run, type-arithmetic
§3), while the method side counts its one function, so that a supplied `g: Nothing => String` does
not make a context absurd. A constructor position of that type has two readings and is refused
(`Y(f: Nothing => String)`: value space 1, estimate 0).

A position mixing a type parameter with ground values (`p: (A, Int)`) is refused: its value space
and its supplied choices disagree. An exact symbolic space beside a position only the estimate
reads as a finite capacity is refused too (`constructor space has no estimate form: …`): a bound
has no exact product with a value the capacity algebra cannot hold; beside an ω or ε₀ tier it is
that tier.

## Limits

- `def f[A](g: A => Int, a: A): Int` stays `?` — "Elimination of an opaque callable's transparent
  ground result is unsupported". Its true count is `(2^32)^(2^32)` (functions of `g(a)`); a
  producer reading would say `2^32 + 1` and a cycle reading ω, both wrong.
- A `var` stays `mutable capture`; a literal type (`x: 3`) stays `unsupported type`; a union with a
  ground alternative needs a discrimination proof; an exported or `given`-imported ground name is
  flagged like the other builtins.
- `Integer` and the other boxes, `BigInt` and `BigDecimal` are not modelled here.
- Ground types stay out of `ConcreteAtoms`: a concrete producer outside the lexical chain cannot add
  an implementation of a transparent goal (its result is a fixed function of the target's
  transparent values, which the split already counts), but removing `Boolean` from that set is the
  environment-completeness decision.
- A member of an enum or a sealed template reads `this`, one of the family's cases, and an enum's
  class parameters are fixed by the case; neither is bound yet. Its count stands only for a terminal
  goal (1 in every environment) and for a goal with infinitely many literals in an enum with a case
  built from nothing (ω: the solver's ω shows the rest of the context is not absurd, the receiver
  keeps it so, and finite programs bound it). Anything else is refused: `receiver of sealed family
  F is a sum this measurement does not split` (`enum E { case A(x: Int); case B; def f: Int }` is
  truly `(2^32)^(2^32 + 1)`; with `case A(n: Nothing)` only, 1).
- A builtin comes from the root import `scala.*`, the weakest binding there is, so any supplied
  declaration a reference reaches by its simple name shadows it. Lookup reads lexical
  declarations, package members, imports and (environment readings §3) what an enclosing
  template inherits from a supplied parent — `final class Sub extends Base { def f(x: Int): Int }`
  reads `Base.Int = Boolean`, 4 — refusing a name an unread parent (outside the sources, or an
  alias of a parent) may declare. It does not read a self type, an enum's cases, what a
  wildcard-imported object inherits or exports, a package object's parents, or export clauses.
  With `trait Base { type Int = Boolean }`, `Int` is `Boolean` in each of those (Scala 3.9.0
  rejects `val w: Int = 3` there), so the builtin is refused, `builtin name Int may be shadowed by
  w8.Base.Int (wildcard import of O)`, until those members are resolved where they are declared. A step the walk cannot follow is
  refused too when it may lead into the sources (`import o.*` of a supplied value: `… shadowed
  through o, which is not resolved`). The walk runs only for a name a supplied type, enum case or
  export spells, so it never runs on eo 1.0.0. A primary constructor's parameters are read outside
  the template's members (`class Sub(x: Int) extends Base` reads `scala.Int`).
- Ground names under a wildcard import of a foreign namespace (`import org.foo.*`) are refused
  (`builtin name Int may be shadowed through org.foo, which is not supplied …`): `org.foo` may
  declare `type Int = Unit`. The stdlib namespaces the model lists declare no root name
  ([stdlib-model.md](stdlib-model.md)), so on eo 1.0.0 `AvroWalk.normalisedName` (ω) keeps its
  count under `import scala.jdk.CollectionConverters.*`.
- Two anonymous `using` parameters of one ground type are ambiguous for `summon`, so neither is
  usable: `def f(using Int, Int): Int` is truly `2^32`, and the anonymous-evidence guard refuses it
  (`ambiguous anonymous evidence: Int`, `GroundEnvironmentSpec.anonymousGround`).

Executable: `GroundTypesSpec`, `GroundEnvironmentSpec`, `NaturalSpec`, `InhabitationSpec`.
