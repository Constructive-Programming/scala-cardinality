# Top types, one-valued goals and unions

The method-side solver reads `Any`, `AnyRef` / `Object`, `Matchable` and `AnyVal` **by their place
in their own arrow** (Rule P): `1` in an arrow's input, ω in its output. The stored-value side reads
`Any`, `AnyRef` / `Object` and `Matchable` as ω everywhere (Rule S). A goal with exactly one value
has one implementation in every environment (Rule O); a top result is not such a goal. A union
`A₁ | … | Aₙ` is the tagged sum `A₁ + … + Aₙ` only where a class test is its eliminator (Rule U).
This page states the rules, their derivations, the counts they produce and where they stop.

*Input* and *output* are used throughout: an input is a method's or a function's parameter, or a
constructor slot, read by the code receiving it; an output is a result.

## What a value of a top type can be used for

The [contract](../plans/v1.md) excludes unrestricted runtime type and equality tests, unsafe
casts, reflection, `null` and effects. On a value of an abstract, top or top-like type these
exclusions remove **every** universal member Scala offers, and the model names them:

| Member or construct | Excluded as |
|---|---|
| `==`, `!=`, `equals`, `hashCode`, `##` | unrestricted equality |
| `eq`, `ne` | reference identity |
| `isInstanceOf`, type patterns, `match` with a default case | unrestricted type test |
| `asInstanceOf` | unsafe cast |
| `getClass`, the default `toString` | reflection |
| `synchronized`, `wait`, `notify` | effects |

The solver already reads an unbounded type parameter this way (`def f[A](a: A, b: A): Boolean` is
2, not the 4 that `a == b` would add), and Scala 3 points the same direction: under
`-language:strictEquality` `==` on `Any` or an unbounded `A` is rejected, and under
`-source:future` a type pattern on either warns that the scrutinee is not `Matchable` (Scala 3.9.0
compiler fixtures). Ground scalars are the opposite case: their *own* equality is total and part of
the model ([ground-scalar-types.md](ground-scalar-types.md)), which is why `def f[A](a: A): Int` is
the `2^32` constants and never a function of `a.hashCode`. A source override of a universal member
(`override def equals(that: Any): Boolean`) is counted like any method: `that` is an input and
carries nothing, so its implementations ignore it, which is exact in the model
(`final class E(val n: Boolean) { override def equals(that: Any): Boolean }` is 2^2).

## Rule P: a top type is read by its place in its own arrow

**Statement** (the owner's decision, v1.md §2, register D38). Each function type, and each method,
is read on its own, innermost first.

1. **Input: 1.** A top in an arrow's input is one value: the code receiving it observes nothing of
   it (Rule T). `def p(x: Any): Boolean` is 2^1.
2. **Output: ω.** A top in an arrow's output is ω: the code building it may return any value a
   finite program builds, countably many — the tier a `String` has. `def r(b: Boolean): Any` is
   ω^2 = ω.
3. **Nothing flips through nesting.** In `f: Any => R` the top is f's input, 1, wherever f itself
   sits: `def call[R](f: Any => R): R` is 1 (`f(())`), and `Function1[Any, R]` is the same type.
   In `g: A => Any` it is g's output, ω, so g's space is ω. A wildcard stays what it was:
   `Function1[?, R]` is `∃X. (X → R)`, uncallable, 0 as a goal.
4. **A top under no arrow takes the shape's place.** A method parameter and a constructor slot are
   inputs, a result (a `val` included) an output. A product, a sum, a sequence, an existential or
   polymorphic body passes its place on, and so does a container: an `Option`'s value, a `Set`'s
   elements, a `Map`'s keys and values — the function a set or a map is tabulated as is no arrow of
   the program (`TopTypes.keyed`). `Option[Any]` is 1 + 1 = 2 as an input and 1 + ω = ω as an
   output. A source class applied to a top reads its fields, each arrow in them on its own
   (`final case class Fn[A](f: Boolean => A)`: `Fn[Any]` holds an output top, ω). A rigid
   constructor's arguments are keys, not values, and are not read.
5. **Set elements and map keys.** In an input, the code receiving a `Set[Any]` cannot tell its
   elements apart, so only its size could tell two sets apart, and the type fixes no size: a value
   whose size is known a priori is determined (one value per known size), and one whose size is
   unknown is empty or not, 2 values (`Set[Any] => Boolean` is 2^2). Scala's `Set[A]` fixes no
   size, so every input `Set[Any]` is 2, and an input `Map[Any, Boolean]` 3 (no key, or the one key
   with either value). In an output or a `val`, a `Set[Any]` is 2^ω = ω and a `Map[Any, Boolean]`
   3^ω = ω.
6. **Arguments before the method.** A method's count is its result's choices over its argument
   spaces. Where an argument's space holds a top in an output — a supplied function returning a
   top, ω values — the choices are counted with that top as one value, which the body cannot
   observe, and two or more choices over such a space are ω; one stays 1, none 0
   (`Inhabitation.count`). `def h(g: Unit => Any): Boolean` is ω; `def loop[A](a: A, f: Any => A,
   g: A => Any): A` is ω (two choices, `a` and `f(())`, over g's ω space); `def u[A](f: Any => A,
   g: A => Any): A` is 1 (one choice, `f(())`). No count is refused because a top-typed position
   cannot be enumerated.
7. **`AnyRef` / `Object` and `Matchable` are `Any`**, everywhere: on the method side, on the stored
   side (Rule S) and in unions (Rule U: `T | AnyRef` is `AnyRef`, `T & AnyRef` is `T`). Boxing
   refuses nothing. `AnyVal` keeps its reading: a top on the method side (this rule), unmodelled on
   the stored side (`unresolved[AnyVal]`), refused beside another alternative in a union.
8. **Evidence that a type is below a top carries nothing.** `A <:< Any` (`AnyRef`, `Object`,
   `Matchable`) imposes nothing on `A`: the proof is one value, factor 1, and refuses nothing
   (`def ev[A](x: A)(using e: A <:< Any): A` is 1). `Any <:< A` and `A =:= Any` stay refused.
9. **Inline methods.** An `inline` method counts as a `def` (AGENTS.md): a top-typed parameter is
   one value and refuses nothing (`inline def k(x: Any): Boolean` is 2); only a compile-time
   operation or another channel to the call site refuses ([syntax-features.md](syntax-features.md)).
10. **Repeated parameters.** `xs: Any*` as an input is one value per length: ω.

A shadowed `Any` (an alias, a binder, an import, an inherited member) keeps the shadowing
declaration's reading. A top behind a source alias (`type Top = Any`) is read through the alias:
the method side places it as above, the stored-value side reads it as ω (Rule S).

**Boundary.** ω over an argument space holding an output top is the rule's extensional reading: a
body that cannot observe the top has only its choices (2 for `h`, 2^(2^32) for a `conv(i: Int):
Boolean` beside a `def other: Any` capability, `ReentrySpec.literalSearch`), so the count is a tier
above the parametric count, never below it.

## Rule T: a top type in an input carries nothing

**Statement.** Where a top spelling reaches the builtin — no binder, declaration, import, export,
inherited member or wildcard-imported member gives it another meaning, and no channel around the
reference reaches declarations the sources do not supply (below) — a top in an input denotes
`Unit`'s shape `1`: an input of a top type carries no information, `Any => R ≅ R`, and `F[Any] ≅
F[Unit]` in an input for every `F` that passes its place on to its argument — functions' outputs
excepted, which Rule P reads as ω. An input `Set[Any]` is `Set[Unit]` too: Rule P (5).

**Derivation.** A type with no operations is the existential `∃X. X` (Mitchell and Plotkin's
"abstract types have existential type", **classical**). Encoded, `∃X. X ≅ ∀R. (∀X. X → R) → R`.
For `k : ∀X. X → R` with `R` independent of `X`, the abstraction theorem at the relation
`{(t, ())} ⊆ T × 1` gives `k[T](t) = k[1](())` (the free theorem `k[U] ∘ g = k[T]` at
`g = !ₜ : T → 1`; register D3), so `∀X. X → R ≅ R`: a function receiving a top observes nothing of
it. Directly: interpret the top type by the total relation; no term former of the model inspects a
value of it — it is only named, stored, passed and returned — so the fundamental lemma holds and no
context tells two of its values apart. It is always inhabited, by `()`; so `def f[A](g: Any => A):
A` is 1 where `def f[A, X](g: X => A): A` is 0. The argument concerns the code receiving a top; the
code building one is Rule P's, which counts a top-typed output as ω by decision (register D38).

`AnyRef`, `Matchable` and `AnyVal` add only unrestricted capabilities (`eq`, `synchronized`, an
open-ended type test that needs a default case), all excluded, so in an input they carry nothing
too. `xs: Any*` is not one value: its length is observable.

## Rule O: a goal with one value has one implementation

**Statement.** If the declared result resolves to a shape with exactly one value — `Unit`, a
singleton type, a product of those, a function into one, or a closed type of value space 1
(`Option[Nothing]`, `Nothing => Boolean`) — the count is 1 whatever else the environment leaves
unresolved: unreadable captures, inherited declarations, parameter types. A top result is an
output, ω (Rule P), not one value: an unreadable name beside it refuses the count
(`def encode(a: A): Any` beside an unsupplied `Schema`).

**Derivation.** Implementations are counted up to observational equality. If the environment is
empty, every goal has the unique absurd answer: 1. Otherwise every implementation returns a value
of the goal, and the goal has one value: 1. Neither case reads the unresolved part, so no omitted
capability can add a distinguishable implementation. Under the model's exclusion of effects, a
`Unit` method is "do nothing": exact in the model, not a claim about side effects.

**Exceptions.** The shortcut rests on the result being the type it spells, so it does not fire where
another meaning may hide behind a name:

- a clause that may rebind a builtin name (an unbound import selector or an export spelling it)
  keeps its `imported environment not resolved` reason;
- a channel name lookup does not walk may reach declarations the sources do not supply: an
  enclosing template's self type outside the sources (`self: ext.Ext =>`) or with an ancestor
  outside them, the parents of a wildcard-imported object or of a package object outside them
  (`object Defs extends ext.X; import Defs.*`), or a wildcard import or export of a namespace no
  supplied declaration provides (`import ext.*`, `export ext.defs.*`), or the outer clause of a
  chained `package a; package b` when no supplied unit declares a member of `a` (the clause opens
  all of `a`'s members, wherever they are written). Scala 3.9.0 compiles
  `def g(): Unit = false` in each case when the outside declaration is `type Unit = Boolean`; the
  never-shadowed assumption (v1.md §2, register D37) sets that case aside, so the top
  names and every other builtin and stdlib-model name keep their reading there, unless the channel
  leads into the standard library or the JDK ([stdlib-model.md](stdlib-model.md));
- a `transparent inline` result must also pass rule X.

Constructor rows are value spaces, not goals, and are untouched.

## Rule S: the stored-value side reads a top as ω

**Statement.** The stored-value estimate (the definitions table) asks what a value of a type may
hold, not what code may observe of it: `Any`, `AnyRef` / `Object` (`java.lang.Object` included) and
`Matchable` are ω wherever they appear, as `String` is — every value a finite program builds. A top
behind a source alias (`type Top = Any`) is ω too, with no refusal. `AnyVal` stays unmodelled
(`unresolved[AnyVal]`).

**Constructor and type are measured apart.** The methods table counts a constructor `<init>` like a
method: its parameters are inputs, so a top in a slot is one value (Rule K). The definitions table
gives the stored values of the type. For `final class Walk(val cur: Any, val n: Int)` the
constructor row is 1 · 2^32 = 2^32, and the type `Walk` is ω · 2^32 = ω, the value space of the
tuple `(Any, Int)` (`TopTypesUnionSpec.walkCtor`, `.walkType`).

## Rule X: `transparent inline` results callers may see narrowed

**Statement.** A `transparent inline` def is `?` with `transparent inline result: the call site
observes the expansion's type, not the declared <T>` unless its declared result is a singleton or
literal type.

**Why.** The declared type is only an upper bound; callers see the expansion's type, and two
expansions with the same values are told apart by it (overload resolution, member selection,
implicit search). In `transparent inline def sel(b: Boolean): Any = b` the expansion is a
`Boolean`, whose members callers may select (the Scala 3 reference on `transparent`), though the
declared `Any` says nothing of it.
Beside `class Sub(val n: Boolean) extends Foo()`, Scala 3.9.0 compiles
`transparent inline def mk(b: Boolean): Foo = new Sub(b)` and a caller's `mk(true).n`, and
`Some(new Sub(b))` under a declared `Option[Foo]` gives `mo(true).get.n` (**compiler fixture**), so
`Foo()`, `new Sub(true)` and `new Sub(false)` are three implementations the declared `Foo` counts
as one. Closed results are narrowed too (review of A6, **compiler fixtures** review-A6 h11–h14,
a6-fix1 t1, a6-fix1b dv): a case of a sealed family (`S2.type` beside a declared `S`: an overload
`h(S2.type)` / `h(S)` picks 1 for the body `S2` and 2 for `(S2: S)`), `None.type` beside
`Option[Boolean]`, a literal type beside `Boolean`, an opaque alias's representation outside its
scope (`Direct.apply`'s `a` is typed `A`, not `Direct[X, A]`: 1 against 2), and the singleton type
of a stable path the body returns (`id(v)` is `v.type`; eo's `dv.value` is `dv.type`). Since a
stable path of any type may be returned, only a declared singleton type leaves nothing narrower.
An anonymous class adds nothing: `new AnyRef { val n = true }` under a declared `AnyRef` is typed
`Object` at the call site (**compiler fixture**). This revises A2's reading, which counted results
naming only closed classes (`Direct.apply` / `Direct.value` = 1, a sealed `S` its 3 values): the
values bound the expansion, the types callers observe do not.

## Rule K: constructor positions are read in the implementation model

A constructor's count multiplies its positions' value spaces, consulting the stored-value estimate.
A slot is an input, so for a top type or `Null` the two views disagree by design (`Any` in a slot:
ω against 1; `Null`: 1 against 0; `X | Null`: |X| + 1 against |X|). Before the estimate is asked,
the position's type is rewritten into its implementation reading — a top in an input to `()`,
`Null` to `Nothing`, `Null` alternatives dropped — and a union or intersection left over is not
handed to the estimate (its sum is only a bound) but to the method side. Each function type in the
slot is read on its own: its parameters are inputs, its result an output, where a top stays a top,
which the estimate reads as ω, as the method side does (`final case class K(f: Unit => Any, b:
Boolean)` is ω). The rewritten type is a new tree with no position of its own, so the estimate
reads it in the scope of the slot it was rebuilt from — where the constructor is written
([stored-value-names.md](stored-value-names.md)): in `object O { final case class Red(); final case
class Box(f: (Any, Red)) }`, `Red` is `O`'s one value, whatever the package declares. A position
the estimate is not asked to size is still read through it as written, so a name it cannot bind
declines that position as it declines any other. The `Null` rewrites only drop values the null-free
model excludes.

The top rewrite identifies values no term of the model tells apart in an input, which holds under
every library constructor that passes its place on to its arguments: functions (by arrow, above),
tuples, `Option`, `Either`, the sequences, a `Set`'s elements and a `Map`'s keys and values (an
input `Set[Any]` is `Set[Unit]`'s 2 values, Rule P (5)). A top inside any other constructor — a
source class applied to it included, since its fields may hold it in an output — is not rewritten,
and the position is the method side's alone: `constructor position reads a top type inside <F>,
whose place in it this reading does not know: <param>` where that side cannot read it (`Box[Any]`
with `Box[A](a: A)` it reads whole: 1; `Fn[Any]` with `Fn[A](f: Boolean => A)`: ω). So `case class
AnyBox(x: Any)` = 1, `final class Walk(val cur: Any, val n: Int)` = 2^32 (its type is ω: Rule S),
`Set[Any]` in a slot = 2, `Map[Boolean, Any]` = 4 (its key sets), and a `WalkRes(cur: Any, parents:
Array[Any], parentsLen: Int)` is ω (1 × lengths × 2^32) — eo's own `WalkRes` too, now that both
sides read its file's `scala.jdk.CollectionConverters.*` through the shared table of listed
namespaces: `CollectionConverters.*` binds no root name ([stdlib-model.md](stdlib-model.md)).

Constructor parameters are typed outside their template — neither the body's declarations nor the
parents' members, nor the companion's, are in scope (Scala 3.9.0: `class C(x: T)` beside `object C
{ type T = Int }` is `Not found: type T`, and `final class C(val x: Any) { type Any = Boolean }`
takes `new C(1)`, **compiler fixture**). That holds for the constructor row and for a member that
reads the field as a capture: `final class S2(val a: Any, val b: Boolean) extends X { def get2:
Boolean = b }` with `X`'s `type Any = Boolean` is 2^2, its `a` one value.

The rewrite reads spellings, so a top or `Null` behind a source alias (`type Top = Any`) is not
handed to the estimate, which would read the alias's top as ω where the slot reads it as one value:
a closed position takes the method side's exact value space (`final case class TA(x: Top)` = 1,
`TC(f: Any => Boolean)` through an alias = 2), a type-parameter slot is the shape model's as usual,
and a position only the estimate could read (one holding a non-literal singleton beside the alias,
which the closed algebra does not size) is refused with `constructor position reads a top type or
Null through an alias: <param>`. The stored-value row of the same type reads the alias's top as ω
with no refusal (Rule S). The rewrite writes the empty type as `Nothing`, so a slot where that name
means a binder or a source declaration is not handed to the estimate.

## Rule U: unions

Exact type equalities apply first, before any alternative is resolved (rule L):

- a union alias is its alternatives, read in the alias's own scope, with a parameter standing alone
  replaced by the argument as written at the use (`RB | Green.type`, `Or[Red, Blue] | Green.type`,
  `ORB | Green.type` with `type ORB = Or[Red, Blue]`, and `Or[Red, Blue]` alone all read as
  `Red | Blue (| Green.type)`); a type parameter passed as an argument stays a type parameter;
- `T | Any = Any` — even when `T` is a foreign name (`Schema | Any` is a top: ω as a result) —
  and so `T | AnyRef`, `T | Object` and `T | Matchable`, the same type in the model (Rule P (7));
  `T & Any = T`, and `T & AnyRef`, `T & Matchable`;
- one class spelled twice, both unapplied, is one alternative (`Red | p.Red`; inside an opaque
  type's scope, `Age | Boolean` with `opaque type Age = Boolean`);
- `S | T = T` when `S`'s class is known to derive from `T`'s, both unapplied — so
  `Fail | Missing` reads as `Fail` although `Missing` alone is unreadable;
- `Nothing` and (null-free model) `Null` alternatives are empty.

Then every remaining alternative is resolved; the first failure is the reason
(`union <u>: <reason>`). Several alternatives of one free binder are that binder. Otherwise the
union is the sum `Shape.Sum(A₁ … Aₙ)` exactly when:

1. every alternative has a **runtime class**: a source class, trait, enum or module, after
   dealiasing; or a builtin from a fixed list — `String`, the boxed primitives, `BoxedUnit`,
   `TupleN` (`TupleXXL` past 22), `Option`, `Either`;
2. no class carries two alternatives (type arguments are erased: `Box[Boolean] | Box[Unit]` share
   `Box`; `(Int, Int) | (String, String)` share `Tuple2`);
3. the classes are pairwise **disjoint**: neither derives from the other, and one is final, both
   are classes (single inheritance), or one is sealed (an enum: its cases) and each subclass it
   enumerates is disjoint from the other;
4. every sealed family among them has only subclasses this reading holds as declarations — the
   sum adds the family's *shape*, which counts the subclasses it reads, so one it misses would
   leave the sum short even when the other side proves the pair apart
   (`sealed trait P; case object P1 extends P; enum E { case A extends E with P }` beside an
   unrelated sealed `Q`: `P | Q` is refused, not read as 1 + 1).

**Derivation.** The injections are the subsumptions `Aᵢ <: U`. The copairing `[f₁ … fₙ]` is
`u match { case x: κ₁ => f₁(x) … }`: total, because the static union lists every case and Scala
checks exhaustiveness; well defined, because disjointness makes exactly one test succeed; well
typed at `x : Aᵢ` with its type arguments, because one class carries one alternative. Every
`g : U → R` equals `[g ∘ ι₁ … g ∘ ιₙ]`, since each `u` is `ιᵢ(a)` for exactly one `i` and `a`: the
universal property of the coproduct, so `U ≅ ΣAᵢ`, which adds in result position and multiplies
(by the existing split) in input position. This class test is restricted — it decides membership
only among the alternatives the static type lists and returns each value at the type it was
injected from — unlike a test on `Any`, which needs a default case and recovers what an upcast hid.

Disjointness is the class-level half of Scala 3's `provablyDisjoint` (`TypeComparer`; SIP-56,
**classical**), which reduces a match type past a case only for a provably disjoint scrutinee:
final class vs unrelated trait, unrelated classes, sealed trait with final leaves vs open trait,
object vs `Tuple2` reduce; non-final class vs open trait, two open traits and type parameters stay
stuck (Scala 3.9.0 compiler fixtures). The deliberate deviation: Scala also separates `Box[Int]`
from `Box[String]` through type arguments, but runtime discrimination is by erased class, so here
one class admits one alternative.

**Ancestry.** Ancestor sets come from the supplied declarations, each parent clause read where it
is written. A parent that does not resolve leaves the set incomplete and the pair is refused —
unless no supplied declaration, enum case or export selector carries its name: a dependency's
classes cannot extend the sources that depend on it. The builtin classes listed above are final,
or sealed in the standard library, so no source class extends one and none extends a source class.
A sealed family's subclasses are read from its file: every template whose `extends` clause names
the family, an alias leading to it or a renaming selector of it; an anonymous class, a given
instance or an enum case among them, or a class the index does not hold, refuses that side's
enumeration — the other side's may still prove the pair apart. A parent clause that names the
family only in an argument (`type Id[X] = X; case class C() extends Id[S]`, which Scala 3.9.0 makes
a child of `S`: a match on `S1` alone warns that it misses `C()`) refuses too, unless its head is a
class of the sources that no alias or renaming spells: such a head cannot be dealiased to the
family, any other may.

## Counts

| Signature | Count | Why |
|---|---:|---|
| `def p(x: Any): Boolean` | 2 | input top: 2^1 |
| `def r(b: Boolean): Any` | ω | output top: ω^2 |
| `def anyOut[A](a: A): Any` | ω | output top |
| `def eqAny(x: Any, y: Any): Boolean` | 2 | no equality on `Any`, as on an unbounded `A` |
| `def call[R](f: Any => R): R` | 1 | f's `Any` is f's input: `f(())` |
| `def callTwo[R](f: Any => R, g: Any => R): R` | 2 | `f(())`, `g(())` |
| `def m[R](consume: Function1[?, R]): R` / `Function1[Any, R]` | 0 / 1 | uncallable / `consume(())` |
| `def loop[A](a: A, f: Any => A, g: A => Any): A` | ω | `a`, `f(())`: 2 choices over g's ω space |
| `def u[A](f: Any => A, g: A => Any): A` | 1 | one choice, `f(())` |
| `def h(g: Unit => Any): Boolean` | ω | 2 choices over g's ω space |
| `def oi(o: Option[Any]): Boolean` / `def oo(b: Boolean): Option[Any]` | 4 / ω | 2 inputs: 2^2 / 1 + ω |
| `def si(s: Set[Any]): Boolean` | 4 | empty or not: 2^2 |
| `def so(b: Boolean): Set[Any]`, `val sv: Set[Any]` | ω | 2^ω |
| `def mi(m: Map[Any, Boolean]): Boolean` / `def mo(b: Boolean): Map[Any, Boolean]` | 8 / ω | 3 inputs: 2^3 / 3^ω |
| `def f(xs: Any*): Boolean` | ω | one input per length |
| `override def equals(that: Any): Boolean` in `final class E(val n: Boolean)` | 4 | `that` carries nothing: 2^2 |
| `inline def k(x: Any): Boolean` / `inline def r(b: Boolean): Any` | 2 / ω | counted as a `def` |
| `def ev[A](x: A)(using e: A <:< Any): A` (or `<:< AnyRef`) | 1 | the proof carries nothing |
| `def t(x: AnyRef, i: Boolean): AnyRef \| Boolean` | ω | absorbed into `AnyRef`, an output |
| `def mt[A](a: A): A & AnyRef` | 1 | `A & AnyRef` is `A` |
| `def v(i: Boolean): AnyVal \| Boolean` | ? | `AnyVal` is not absorbing |
| `trait Codec[A] { def encode(a: A): Any }` (`Schema` foreign) | ? | an output top is not one-valued (rule O) |
| `case class AnyBox(x: Any)` (constructor row) | 1 | rule K |
| `final class Walk(val cur: Any, val n: Int)`: constructor row / type | 2^32 / ω | rule K / rule S |
| `final case class TC(f: Any => Boolean)` (constructor row) | 2 | f's `Any` is f's input: 2^1 |
| `final case class K(f: Unit => Any, b: Boolean)` (constructor row) | ω | f's output top |
| `type Top = Any; final case class TA(x: Top)`: constructor row / type | 1 / ω | rule K / rule S |
| `def out(r: Red, b: Blue): Red \| Blue` (`Red(level: Boolean)`, `Blue()`, final) | 9 | 3 results per value of `r.level` |
| `def in(u: Red \| Blue): Boolean` | 8 | 3 input values |
| `def t(p: (Boolean, Boolean)): Green.type \| (Boolean, Boolean)` | 625 | `(1 + 4)^4` |
| `def pick(): One \| Two` (two case classes) | 2 | single inheritance |
| `def a(f: Fail): Fail \| Missing` | 4 | `Missing` absorbed: `2^2` |
| `def a(r: Red): RB \| Green.type` (`type RB = Red \| Blue`) | 16 | the alias's alternatives: `4^2` |
| `transparent inline def s(b: Boolean): S` (`S1(b: Boolean)` final, `S2` object) | ? | rule X: `S2` is typed `S2.type` at the call site |
| `transparent inline def one(b: Boolean): 1` | 1 | singleton result: nothing narrower |
| `transparent inline def mk(b: Boolean): Foo` (`Foo` has a subclass) | ? | rule X |
| `def oe(x: Option[Boolean]): Option[Boolean] \| Either[Unit, Unit]` | 125 | `5^3` |

## What stays `?`

| Case | Reason |
|---|---|
| type parameter or alias argument alternative | `union alternatives require an overlap and discrimination proof: A \| Red — A has no runtime class (type parameter or alias argument)` |
| one class, two alternatives | `… — two alternatives share runtime class Box` |
| overlap | `… — PQ derives from Q`; `… — a case of Color derives from Q` |
| open classes | `… — X and Y may overlap (non-final class and open trait)` — defensive: the shape reading refuses an open trait or a plain class first (`abstract type or method-valued representation`), so no count reaches it; `TopTypesUnionSpec.openPair` pins the pair check directly |
| `AnyVal` beside other alternatives | `… — union with top-like alternative AnyVal is not a sum` (`AnyRef`, `Object` and `Matchable` absorb the union instead) |
| ancestry not resolved | `… — ancestry of P1 not resolved: Mixin` |
| sealed family with a subclass it does not hold (an enum case of another enum, an anonymous class, a given) | `… — sealed family P has a subclass this reading does not enumerate: …` (an anonymous instance in its file is usually refused first by the family reading: `sealed family has anonymous instances in its file`) |
| sealed family a parent clause may reach through a parameterised alias | `… — sealed family S may be extended through Id[S], which this reading does not dealias` |
| opaque alternative outside its scope, unresolved alternative | the alternative's own reason, `union <u>: …` |
| stable-value singleton alternatives | the alternative's own reason (no runtime class without ground equality analysis) |
| literal alternatives beside a class (`1 \| Int`) | `two alternatives share runtime class …` (a union of literals alone is the set of its constants: [syntax-features.md](syntax-features.md)) |
| callable returning a union | a disjoint one is a sum, eliminated once per argument ([D25](polymorphic-members-and-saturation.md): `TopTypesUnionSpec.callableSum` 27); an open one keeps the union's own reason |
| transparent result whose declared type is not a singleton | `transparent inline result: …` |
| self type, wildcard-imported object, package object, wildcard namespace or outer chained package clause outside the sources | the builtin (never-shadowed assumption); through the standard library or the JDK, `builtin name(s) … may be shadowed through a reference not supplied: … (…)` |
| a constructor slot holding a top inside a source class or an unmodelled library constructor, which the method side cannot read | `constructor position reads a top type inside <F>, whose place in it this reading does not know: <param>` |
| a constructor slot holding a top or `Null` behind an alias, which only the estimate could read | `constructor position reads a top type or Null through an alias: <param>` |
| an `inline` method whose body uses a compile-time operation or another channel to the call site | `inline method: <what failed>` ([syntax-features.md](syntax-features.md)) |
| `Any` shadowed (alias, binder, import, inherited member) | the shadowing declaration's reading or refusal, unchanged |
| evidence with a top on its left or in an equality (`Any <:< A`, `A =:= Any`) | `unsupported evidence relationship: … requires free-atom or rigid-application endpoints` |

Not done here: `A <: Any` as a vacuous bound and `A <: AnyRef` / `Matchable` / `AnyVal` bounds (the
bounded-parameter work owns bounds), absorption of classes into `AnyVal` (refused instead),
wildcards in variant slots as `Any` / `Nothing`, intersections other than `T & Any` (`AnyRef`,
`Object`, `Matchable`), and union aliases spelled infix (`Red Or Blue`) or reached through a
parameter (`type Id[A] = A` applied to a union reads the union as written, which is the same sum).

## Evidence

- Mitchell and Plotkin 1988, *Abstract types have existential type* (TOPLAS 10(3)) —
  **classical**.
- Reynolds 1983 and Wadler 1989 (register D3) — the abstraction theorem and free theorems behind
  `∀X. X → R ≅ R`.
- The universal property of the coproduct — **classical**.
- Scala 3.9.0 compiler fixtures — **compiler fixture**: strict equality rejects `==` on `Any` and
  on unbounded `A`; type patterns on them warn under `-source:future`; `(Red | Any) =:= Any`,
  `(Red & Any) =:= Red`, `(Fail | Missing) =:= Fail` hold; a missing union case fails
  exhaustiveness; `case l: List[Int]` against `List[Int] | Vector[String]` is unchecked-warning
  free; the match-type disjointness table above; a `transparent inline` result narrowed to a
  subclass exposes the subclass's members at the call site, an anonymous class's do not; a sealed
  case, `None`, an opaque representation and a returned stable path each pick a different overload
  than an equal-valued body ascribed to the declared result (review-A6 h11–h14, a6-fix1 t1, dv); a self
  type, a wildcard-imported object's parent, a package object's parent, a wildcard import or
  export and an outer chained package clause each rebind `Unit` and `Any` when the outside
  declaration says `type Unit = Boolean`; constructor parameters typed outside the template's body,
  parents and companion; `extends Id[S]` with `type Id[X] = X` makes a child of the sealed `S`.
- The output reading (ω), the set-size reading of Rule P (5), `AnyRef` / `Matchable` as `Any` and
  the no-op `<:< Any` proof are the owner's decisions (register D38), not derivations.
- **Executable**: `TopTypesUnionSpec` (`.polarity`, `.loop`, `.unseeded`, `.structural`,
  `.otherTops`, `.containers`, `.equalsOverride`, `.inlineTop`, `.referenceTops`, `.evidence`,
  `.walkCtor`, `.walkType`, `.outputSlot`, `.mergingCtor`, `.aliasedTopCtor`, `.topLike`),
  `StdlibModelSpec.keyed`, `MethodUnionNullabilitySpec`, `ReferenceInhabitantsSpec`; rule X in
  `TransparentInlineSpec`.
