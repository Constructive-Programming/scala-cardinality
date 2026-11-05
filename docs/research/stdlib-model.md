# The standard-library model, sequences and absent declarations


The method side reads library declarations the sources do not supply through one small table,
`StdlibModel` (`stdlib-model/1`: Scala 3.9.0's library API on 2.13 collections, so `Seq` is
`immutable.Seq`). Each entry is keyed by its fully qualified name and read one way. A name the
table does not hold, and no source declares, is reported as absent information — never guessed at.
This page states the readings, the sequence-elimination rule they need, where the table answers,
and what stays `?`. Executable: `StdlibModelSpec`, `SequenceEliminationSpec`,
`RepeatedParameterFragmentSpec`.

## Singleton terms

The table also holds the library's two parameterless root-imported **terms**, `scala.None` and
`scala.Nil`, keyed by both spellings (`None`, `scala.None`, `Nil`, `scala.Nil`). The rule is
Scala's own singleton-type rule, not a model choice: a stable identifier's type `p.type` has the
one value `p` (draft specification ch. 3), so a goal of it has exactly one implementation — the
one returning `p` — and a `p.type` input carries exactly one value. The table's entries give the
two objects a canonical identity, so `None.type` and `scala.None.type` are the same value.

The same terminal reading answers a `p.type` whose path resolves to nothing this reading knows
(`SingletonIntersections.module`): the sources typecheck, so the written `p.type` is a stable path
of some term — of an unsupplied dependency, a top-level value, an enum case — and that term is the
type's one value whatever it is. The two cases differ only in identity: a table term is the same
value everywhere, an opaque path is its own spelling's. What stays refused is everything the
singleton rule itself refuses, unchanged: a `var`'s path (E083), a forward reference, a path not
an enclosing capture, and a source module the use site may not reach — the reading there needs the
module's member environment, not just its existence. A call's singleton *result* keeps its own
rule: an object's singleton widens to itself, anything else to a type this reading does not
compute (`InferredResults`), so a method's result of `E.A.type` stays refused while a *declared*
goal of `E.A.type` is 1.

## Readings

| Reading | Entries | Shape |
|---|---|---|
| product | `Tuple1`…`Tuple22` (≡ tuple syntax), `EmptyTuple`, `Some[A]` ≡ `A`, `Left[A, B]` ≡ `A`, `Right[A, B]` ≡ `B`, `NamedTuple.NamedTuple[N, V]` ≡ `V` | `Product` |
| sum | `Option[A]` = `1 + A`, `Either[L, R]` = `L + R` | `Sum` |
| function | `Function0`…`Function22` ≡ the arrow they spell | `Function` |
| finite sequence | `List`, `Vector`, `IndexedSeq`, `Seq`, `immutable.ArraySeq`, `Array[E]` | `Repeated(E)` |
| type constructor | any of the above unapplied: in a higher-kinded position (`Box[Either]`) an operator of the higher-kinded reading, applied where it is read; read as a proper type (no Scala program) | operator ([higher-kinded binders](higher-kinded-binders.md) R3); refused: `higher-kinded argument: scala.util.Either (type constructor [L, R] =>> L + R, needs higher-kinded instantiation)` |
| excluded (final) | `Iterator`, `IterableOnce`, `java.util.Iterator`, `java.nio.ByteBuffer`, `java.io.InputStream` (a read changes them); `Map`, `Set` (built and read by key equality); `WeakReference`, `ReferenceQueue`; `scala.quoted.{Quotes, Expr, Type, …}`; `scala.reflect.{ClassTag, Manifest, TypeTest, Typeable}`, `scala.deriving.Mirror{, .Product, .Sum, .Singleton, .Of, .ProductOf, .SumOf}` (type-revealing evidence, [syntax-features.md](syntax-features.md)) | refused: `outside the counting model: <fqn> (<why>)` |
| transparent, open | the throwables (`Throwable` … `NumberFormatException`, `java.util.NoSuchElementException`): built from any message, observed through `getMessage`, `getLocalizedMessage`, `getCause` ([code without side effects](pure-code.md), register D26) | `Ground(<fqn>, ω)` |
| unmodelled, open | `Product`, `Equals`, `CharSequence`, `java.util.{List, Map, Set, Collection, ArrayList, HashMap}`, `ConcurrentHashMap` | refused: `open stdlib type with unmodelled members: <fqn>` |
| unmodelled, final | `java.lang.Integer` | refused: `stdlib type with unmodelled members: <fqn>` |

**Products and sums.** `TupleN`, `Some`, `Left` and `Right` are `final case class`es with a public
total constructor and public fields, so their values are exactly the products of their fields;
`Left`'s second parameter is phantom. `NamedTuple` is an opaque alias of its value tuple with
total conversions both ways (SIP-58), so it holds the values `V` holds. These are the sum
and product rules of [type-arithmetic.md](../type-arithmetic.md) §2 applied to declarations whose
representation the language fixes. Every spelling of one entry is one key —
`Tuple2[X, A]`, `scala.Tuple2[X, A]` and `(X, A)` in override identity, `Either` and
`scala.util.Either` in evidence identity, `List` and `scala.collection.immutable.List` as the
argument of a rigid constructor ([higher-kinded binders](higher-kinded-binders.md) R2) — so the
reviewed `Accessor.tupleAccessor.get = 1` keeps its value.

**Sequences.** A strict list is the least fixed point `μX. 1 + E × X ≅ Σₙ Eⁿ`
([type-arithmetic.md](../type-arithmetic.md) §4, §8; register D4). A pure, total program observes a
`Vector`, an `IndexedSeq`, an `ArraySeq` or an `Array` only by its length and its elements by
position — identity and mutation are outside the contract — so one `Repeated` shape loses nothing.
Introduction is Nil/Cons (in the solver already). An `Array` is built only with its element's
`ClassTag` (`new Array[E](0)`, `Array.empty[E]`, `Array(e)`), which Scala 3.9.0 synthesizes for
every element type but a type parameter, an abstract type member, or a singleton whose widened type
is one (`x.type` for `x: A`: E172 for all three spellings). `Nothing`, `Null`, an empty sealed
trait, an opaque type, `Box[A]`, an object's singleton and `x.type` for `x: String` or `x: Box[A]`
build one with `new Array[E](0)`; `F[A]` with `Array.empty[F[A]]` or `Array(x)` (fixtures — `new
Array[F[A]](0)` crashes the 3.9.0 back end, which changes nothing here). So `Array[A]` and
`Array[x.type]` for `x: A` are refused (`Array of an abstract element type is not read: scala.Array
(building one needs ClassTag evidence, and the reading has no polarity)` — an input array needs no
`ClassTag`, but the reading does not tell inputs from goals), `Array[Box[A]]` is a sequence like any
other, and an array of an empty type is only the empty one. `scala.Seq` admits a `LazyList`: as a
goal an infinite sequence needs an element, and with one the finite sequences already give ω; as an
input, a total program inspects a finite prefix and its length tests (`lengthCompare`), so two
programs that agree on every finite sequence agree on infinite ones too (continuity): counting over
finite inputs is exact.

**Closed sequences are transparent data.** A sequence of closed elements (`Array[Byte]`,
`List[Int]`, `Array[Any]` — `Any` is `1` on the method side, register D12) is a closed value like
any other ([ground-scalar-types.md](ground-scalar-types.md)): the solver splits it, and a goal of
one is ω by its lengths (`zigZagInt(n: Int): Array[Byte]`). An infinite closed input makes a
function space ω, never beyond (register D2).

## Rule SE: eliminating a supplied sequence by length

A sequence of opaque elements in the environment — a parameter, a field of one, a capture, a
vararg, the goal's own arrow parameter — is eliminated by length. With `Γ(n̄)` the environment
with each of the `m` sequences replaced by `nᵢ` fresh elements, and `c(n̄)` the solver's count in
it:

- `c(0̄) = 0` → **0**: the empty sequences are an input with no answer;
- the goal has one value → **1** (register D12);
- `c(0̄) ≥ 1` and `c(k̄) ≥ 2` for some `k̄ ≠ 0̄` with every `kᵢ ≤ 2` → **ω**;
- otherwise `?` (`sequence elimination: …`), and `?` past three sequences.

*Derivation.* `Seq[E] ≅ Σₙ Eⁿ`, and a function out of a sum is a product of functions
(type-arithmetic.md §7: for `List` the shapes are lengths), so an implementation is a family
`(f_n̄)` with `f_n̄` an inhabitant of `Γ(n̄)`, selected by a total length test and positional
patterns. (1) The empty input is admissible, so `c(0̄) = 0` leaves no total family — which is why
the fields of a receiver no public constructor builds are refused outright
(`receiver has no public constructor: NonEmpty`): its factories decide which
field values exist, and `NonEmpty.of(a, rest*)` never builds `Nil`. (2) Weakening
carries existence to every length: an inhabitant of `Γ(0̄)` is one of every `Γ(n̄)` that ignores the
elements. Distinctness needs more: two inhabitants of `Γ(k̄)` told apart by some valuation stay
apart in `Γ(k̄ + eᵢ)` when `kᵢ ≥ 1`, because the valuation extends by repeating an element `i`
already has. So every length `k̄ + j·eᵢ` has two choices, and the families "the other choice at
length `k̄ + j·eᵢ`, the weakened length-0 answer elsewhere" are pairwise distinct — infinitely many.
Finite programs bound them by ℵ₀ (register D2): **ω**.

*Why not `c(0̄) ≥ 2`.* Weakening does not preserve distinctness through the empty type: a new
element can make the context absurd and merge every answer. `def f[A](n: A => Nothing, xs: A*):
Boolean` has `c(0) = 2` but `c(1) = c(2) = 1`, and only the empty sequence can be passed, so its
true count is 2 — the rule answers `?` (`two answers only for empty sequences …`), never ω. With a
branch that survives (`y: Either[A => Nothing, Unit]`), `c(1) = 1 · 2 = 2` and the count is ω.
That an absurd element merges the answers is this page's own derivation, pinned by `varAbsurd`.
Related work: Scherer & Rémy 2015 report that the simply typed λ-calculus with sums has 0, 1 or ≥ 2
inhabitants under βη, decided by saturating focused search; Scherer 2017 reports that
βη-equivalence with sums and the empty type is decidable and coincides with contextual
equivalence (both **Metadata-verified**).

A singleton alias of a value is that value (`q: p.type` beside `p: (List[B], A)` holds no second
`A`): the sequences under it are the referent's own, eliminated once.

A sequence the rule cannot reach stays `?` with its place: inside a supplied sum, as a supplied
callable's result, as a parameter of a function the goal builds, under an existential or a proof
(`Repeated-argument sequence elimination is unsupported: <place>`); a callable returning a closed
sequence is not eliminated either (`Elimination of an opaque callable's sequence result is
unsupported`).

## Where the table answers

Scala's root imports — `java.lang.*`, `scala.*`, `Predef.*` — have the lowest precedence of all
bindings (Scala 3 draft specification ch. 2, **Inspected (full text)**: they "are taken as lowest
precedence, so that they are always shadowed by user code"). So the table answers only after every
source declaration, named import, enclosing package and wildcard, under these spellings: the
simple names the root imports bind (`Map` and `Set` through `Predef`, `IllegalStateException`
through `java.lang`, `NoSuchElementException` as `java.util`'s), `scala.`-qualified ones, fully
qualified ones, and named imports of a model path (`import java.util.List` reads the open
`java.util.List`, never a Scala sequence).

**Wildcard refusal.** A wildcard import of a namespace the sources do not supply may declare any
name. A `scala.Dynamic` parent is registered in the same table as a marker that declares no member
a count reads: the structural calls its subclasses enable (`selectDynamic`, `applyDynamic`, …)
are compile-time-generated members outside the model, and a run over sources that use them logs a
notice saying so — the assumption is that no counted implementation calls a structural member. — `import java.util.*` brings `java.util.List` over `scala.List`. Every builtin and model
name is refused there (`builtin name List may be shadowed through a reference not
supplied: java.util (wildcard import of java.util)`), as is any other channel to undeclared names (an unsupplied
parent, self type or package clause). A `_root_`-anchored path is exempt. One table,
`ListedNamespaces`, which the stored-value estimate reads too, lists the stdlib namespaces whose
wildcards both readings know, with what each declares: `scala.quoted.*` (the types `Expr`,
`ExprMap`, `FromExpr`, `Quotes`, `ToExpr`, `Type`; the terms `Expr`, `Exprs`, `FromExpr`,
`ToExpr`, `Type`, `Varargs`, `quotes`, `runtime`), `scala.jdk.CollectionConverters.*` (its twenty
conversion classes, each with the implicit conversion of its name) and `quotes.reflect.*` (members
not listed) — scala-library 3.9.0 class files, read with `javap` and the jar's entries, and for
`quotes.reflect` the TASTy of `scala.quoted.Quotes` (its type members are abstract, which `javap`
cannot show). The same listing covers the dependency namespaces the analyzed corpora's wildcards
open, taken from their artifacts: cats-core 2.13.0's syntax objects (`cats.syntax.functor`,
`either`, `flatMap`, `coflatMap`, `applicative` — the ops converters their sources declare, the
syntax traits and their `To*Ops` parents carrying no type members) and its instances object
(`cats.instances.function` — its given methods), and the `kyo` package of kyo 1.0.0-RC7 (its
classes, traits, objects, top-level definitions and aliases, across the artifacts the corpus
pulls, from the sources jars and the entries). None declares a root spelling, so the 283 rows the
blanket doubt had stopped — `builtin name Unit may be shadowed …: cats.syntax.functor` and kin —
read the names behind it (`register D33`). A namespace no source supplies and no listing covers
(`hearth.fp.instances`, whose package object's surface the entries cannot complete) keeps the
blanket refusal. None of them declares one of the 93 root spellings the analyzer reads through the
model — the stdlib namespaces checked with Scala 3.9.0 fixtures (under each, `summon[N[…] =:=
_root_.<fqn>[…]]` compiles for all 93), the dependencies from their sources' declarations.
`quotes.reflect` does declare two names the root imports bind: of its 323 type members, `Singleton`
and `Symbol` are the only ones a `scala.*`, `scala.Predef` or `java.lang.*` type also spells (scalac
compiles `summon[Symbol =:= quotes.reflect.Symbol]` there), so the table records both as reflect's,
and the `Singleton` marker of a singleton intersection is not read under it (below). None declares
a term `scala` or `java`.

**Listed and unsupplied wildcards against the sources' names.** Scala 3 ranks the bindings of a
name (Scala 3 draft specification ch. 2, **Inspected (full text)**; register D11): a definition of
the reading file (or an inherited or exported member) > an explicit import > a wildcard import > a
package member of another file > the root imports and root packages, an inner binding shadowing
outer ones of the same or lower rank. So a wildcard outranks a package member of another file and
a root package, and an inner wildcard outranks an outer one. A wildcard of a listed namespace binds
exactly what the table lists: under `import scala.quoted.*`, `Expr` is the library's excluded
`scala.quoted.Expr` even where another file of the package declares an `Expr` (Scala 3.9.0
fixture: that `Expr`'s field is not a member there), and a name it does not declare is still the
package's. A wildcard over a namespace the sources do not supply — or one whose members the table
does not list, `quotes.reflect` — may bind any name, so the method side reads none it may bind
(the stored side already did, [stored-value names](stored-value-names.md)):

- a type name the reading would otherwise take from another file's package member or from an
  outer scope's wildcard: `unresolved type: Fst (may be imported by cats.syntax.functor.*)`;
- a path's first name — `compose` in a later `import compose.*`, `q` in `import q.Two` or in the
  type `q.Two` — which would otherwise be a package or object of another file or a root package:
  `unresolved type: Fst (may be imported by compose.*: compose may be imported by
  cats.instances.function.*)`, `unresolved type: Two (imported from q.Two, q may be imported by
  zzz.*)`, `unresolved type: q.Two (q may be imported by zzz.*)`. A wildcard whose own prefix is in
  doubt passes that doubt on, as the stored side words it.

What keeps its reading: a definition of the reading file (in its scope or an outer one, or in
another block of the same package in that file — a definition of the compilation unit, Scala 3.9.0
fixture `probes/review-A7/r9`), an inherited member, an explicit import, a wildcard of the same
scope over a supplied namespace that binds the name, and a `_root_` path. Scala rejects (E049) a
reference that one of them and an inner or same-scope wildcard both bind, so where the program
compiles the
wildcard does not bind it — a fact about the reference, not the whole-program argument that a
rebinding would not typecheck elsewhere, which no rule adopts. A name the sources do not declare is
left to the root imports' reading, which refuses it through the same wildcard — a wildcard whose
prefix's first name is itself in doubt included: that reading asks the lookup about the prefix
(`import zzz.{Boolean as _, *}; import compose.*` refuses `Boolean`, which `zzz.compose.*` may bind:
`builtin name Boolean may be shadowed through a reference not supplied: compose (wildcard import of
compose: compose may be imported by zzz.*)`, fixtures r8 and r5). So does the `Singleton` marker of
`O.type & Singleton`: it is `scala.Singleton` only where the lookup finds no declaration of the name
and no channel may rebind it (under `import zzz.*` a `zzz.Singleton = Nothing` empties the type;
under `quotes.reflect.*` it is reflect's own).

The same reading covers every clause that may bind a name besides a wildcard over a path:

- a `given` selector (`import zzz.given`, `import zzz.{given T}`) binds the given instances of its
  prefix — terms of any name, no type: over a namespace no source supplies it may bind a path's
  first name (`unresolved type: C (may be imported by compose.*: compose may be imported by
  zzz.given)`, fixture r11); over a supplied one it binds that namespace's given or implicit
  members by name, values a path through which is not followed;
- an importer whose prefix is no path (`import Outer.this.o.*`, `import Outer.this.o.Two`) binds
  members of a value: its wildcard may bind any name (`unresolved type: Two (may be imported by
  Outer.this.o.*)`) and its selectors bind theirs to members not followed (`path through a value not
  followed: Outer.this.o`), fixture r12;
- a supplied wildcard binds what its namespace holds, a value (`import Lib.*` binds `val
  Lib.compose`: `path through a value not followed: compose`, fixture r15) and an enum
  companion's class cases (`import E.*` binds `E.A`: `enum case type not read: p.E.A`, fixture
  r16) included; a singleton case (`case B`, `case D extends E`) is a value, no type.

A path's first name is a term, so a clause over a supplied namespace binds it to a term member
the clause imports (`TermMembers`; Scala 3 reference, given imports, and the fixtures
`probes/review-A7/round2/*`, `probes/a7fix2/spec`): a wildcard binds every accessible member but a
given, a `given` selector the given and implicit ones, and neither binds a private member outside
where it is accessible. The members are the namespace's own definitions — a package's
subpackages among them, never a type or a class of the name (`import Lib.*` with `class two`
leaves `two.C` the package `p.two`'s) —, the aliases its export clauses define (followed into
the supplied namespace a named or wildcard export forwards), and what an object's or a package
object's parents pass on (`object Lib extends Base`: Base's object `two` is read, its value
`compose` is not followed; a parent outside the sources may declare any name, so the head is
refused: `inherited type member not resolved: compose (unsupplied parent zzz.Base)`). A
package object's parents pass type members on to its package's wildcard the same way. A first
name nothing around the reference binds is the nearest package the scope opens that has that
member among the sources (`c` in package `a.b` is `a.b.c` when the sources declare it, whatever
the root `c` holds), not every package that holds the selected name.

**The closed-package assumption.** A package some supplied file declares a member in (a definition,
not only a nested package clause or an import) is read as complete: a wildcard over it binds its
supplied members and nothing a dependency might add (`import zzz.*` with `package zzz; object Mine`
supplied binds no `Two`). A package the sources spell only as the prefix of a deeper one (`zzz` for
`package zzz.inner`, `dev` for `dev.constructive.eo`) is not: Scala binds a member another
compilation unit declares there (fixture r14 binds a `zzz.Two` of a separate unit), so its wildcard
binds its supplied subpackages and may bind any other name, like a namespace no source supplies. A
package object counts as a member of its package (both sides share `Trees.declaresPackageMember`).

Every reading goes through this lookup: a signature's goal and inputs, a capability record's
fields (read where the field is written), a constructor's slots, a projection's prefix (`H#M`), a
path through a parameter whose declared type it refuses (that refusal is the path's reason), and
the conversions an inline body may meet
— there the supplied declaration a wildcard may hide is what is read, the alternative being a
declaration outside the sources, which is not read as a conversion (register D27's boundary).
Scala 3.9.0 fixtures (`probes/a7/prec`, `probes/a7/plain`): `import zzz.w.*` binds `zzz.w.Fst` over
another file's `p.Fst`; `import zzz.v.*; import compose.*` binds `zzz.v.compose.Fst`; `import
zzz.v.*; import dev.other.Y` binds `zzz.v.dev.other.Y`, and the type `dev.r.optics.BijectionIso`
under `import zzz.syntax.functor.*` is `zzz.syntax.functor.dev.r.optics.BijectionIso` while the
`_root_` spelling is not; an inner `import zzz.w.*` shadows an outer `import a.*`; a same-file
definition, a named import and a supplied wildcard of the same scope win; and an outer definition,
an outer named import, a same-scope supplied wildcard or an inner package's member of another file
against a wildcard that binds the name are each E049.

*Future work, not adopted:* a per-version index of the NAMES a dependency exports (no sources)
would let a wildcard over a known library bind only those names, as the table does for the three
namespaces above.

**Absence.** A name no source declaration and no table entry answers is `declaration not
supplied: <fqn> (<how it was bound>)` — `(named import Schema)`, `(renamed import JList)`,
`(selected through named import Schema)`, `(selected through wildcard import)` for a member a
supplied package lacks, `(not declared in the sources or stdlib-model/1)`,
with `may be bound through <namespace>, which is not supplied (…)` when a channel exists or `may
be a member of quotes.reflect …` for a namespace the table does not enumerate. `unresolved type:`
is kept for gaps in reading the sources: a name some supplied declaration spells, a path through a
supplied term, an applied callee.

**Named imports and heads.** A named import of a builtin path reads the builtin (`import scala.Unit`
is `Unit`, `import java.lang.String` is `String`). Its own first name is read like any other: after
`import lib.scala`, `import scala.collection.immutable.List` names
`lib.scala.collection.immutable.List` (`declaration not supplied: … (selected through named import
scala)`), and a channel that may bind `scala` or `java` refuses the reading as for a qualified
spelling. A listed namespace counts only through the library's first name: `scala` not rebound by an
import or a source term, and `quotes` bound to `scala.quoted.quotes` — by a named import or a
`scala.quoted` wildcard before the clause. `import lib.quotes; import quotes.reflect.*` (and `import
quotes.reflect.*` alone, a top-level package `quotes` outside the sources) refuse every builtin as
an unsupplied wildcard does.

**Assumption.** No package outside the sources is named `scala` or `java` inside a package of the
sources: inside `package r7`, `scala.Option` is read as the library's, though an unsupplied
`r7.scala` would outrank the root package (Scala's reason for `_root_`). A package `r7.scala` the
sources do supply refuses the reading (`builtin name scala.Option may be shadowed by a
declaration: package r7.scala (a package of the sources)`). A relative path (`util.Either`) is refused without such
evidence, since top-level packages of those names are commonplace.

## Parents outside the sources

A method's environment includes what its owners inherit. An **open** owner — a trait, an
abstract or non-final class — may leave a foreign parent's abstract members to a subclass, and a
subclass may override its concrete ones: capabilities this reading cannot list. Such an owner's
rows are refused (`inherited parent declaration not supplied: org.typelevel.discipline.Laws (named
import Laws)`; a stdlib parent the model does not summarize, such as `Product`, reads alike since
the corpus-coverage prune: `inherited parent declaration not supplied: Product (not declared in the
sources or stdlib-model/1)`). A receiver no subclass
replaces — an object, a given, a final class — has implemented every inherited member, and
concrete code no subclass can override cannot change a count whose inputs and result are read (the
environment reading's rule for concrete code). Constructors are exempt: a constructor
chooses only its parameters. A sealed template whose subtypes are all fixed receivers in turn
(final classes, objects, sealed templates of such, with no anonymous or given instance) is one too:
its own receiver's family is what stays unread (`receiver is a sealed family this measurement does not split: P`). The table
summarizes the parents eo extends: the root and marker types (`Any`, `AnyRef`, `Object`,
`Matchable`, `AnyVal`, `Serializable`, `Dynamic`) declare no member a count reads; `Product` and
`Equals` declare no type member, but term members the table does not hold; `IterableOnce`
declares no type member, leaves `iterator` abstract and lets a subclass override `knownSize` and
`stepper` (scala-library 2.13 class file), so an open owner must define all three (`inherited
parent declaration not implemented: …iterator`, `inherited stdlib member a subclass may override:
…stepper`). A summary holds only where the root imports bind the parent's name: under `import
lib.*`, `extends Serializable` may name `lib.Serializable` (`inherited parent declaration not
supplied: Serializable (may be bound through lib, which is not supplied (wildcard import of
lib))`), and its type members are unknown where the subclass reads `Int`.

## Counts

| Signature | Count | Why |
|---|---:|---|
| `widenLeft[L, R, L2](r: Right[L, R]): Either[L2, R]` | 1 | `Right(r.value)`; no `L2` |
| `both[L, R](r: Right[L, R]): Either[R, R]` | 2 | `Left(r.value)`, `Right(r.value)` |
| `pick[A](p: Tuple2[A, A]): A` | 2 | either field |
| `none[A]: List[A]` / `nothing: List[Nothing]` | 1 / 1 | Nil only |
| `some[A](a: A): Vector[A]` | ω | every length |
| `wrap[A](a: A): Array[A]` / `single[A](x: A): Array[x.type]` | ? / ? | no `ClassTag` (true 0 for both) |
| `wrap[A](x: Box[A]): Array[Box[A]]` / `empty: Array[Null]` | ω / 1 | every length of x / the empty array |
| `total[A](xs: List[A]): A` | 0 | `c(0) = 0` |
| `headOption[A](xs: List[A]): Option[A]` | ω | `c(0) = 1`, `c(1) = 2` |
| `f[A](xs: List[Nothing], b: Boolean): Boolean` | 4 | the list is Nil |
| `varAbsurd[A](n: A => Nothing, xs: A*): Boolean` | ? | true 2; `c(1) = c(2) = 1` |
| `pair[A](p: (List[A], A)): A` | ω | a field is a sequence of its own |
| `pairAlias[A, B](p: (List[B], A), q: p.type): A` | ? | true 1: unique at every length counted |
| `NonEmpty.head` (`private` constructor over `xs: List[A]`) | ? | true ω: only the factory builds receivers |
| `head` of a `private`, `private[p]`, protected, block-local class, or one nested in a private object | ? | only that scope's code builds receivers |
| `fromList[A](xs: List[A]): List[A]` (given over `List`) | ω | identity, tail, … |
| `zigZagInt(n: Int): Array[Byte]` | ω | closed sequence goal |

## What stays `?`

Excluded and unmodelled entries (above; neither is a nominal key of a rigid constructor's argument
either, since the table holds no parameters for them — `scala.quoted.Type` takes any kind); an
unapplied constructor read as a proper type; an `Array` of a type parameter or of a singleton of
one, even as an input (the reading has no polarity); the members of a class only its own scope
builds (a non-public constructor or class, a non-public enclosing template, a block) until its
factories are read; `Map`/`Set` though the stored-value side keeps its mathematical estimate; names
under an unsupplied wildcard (a `given` selector, a package spelled only as a prefix and an
importer whose prefix is no path included), and the source names and paths' first names it may
bind; a definition in another block of the reading file's package on the stored side and in the
constructor slots it reads (the method side reads it); a foreign
parent of an open owner; SE beyond its three sequences and
lengths ≤ 2, and the places listed under Rule SE; every name the sources and the table do not
declare — a written `p.type`'s path excepted, which its own spelling decides
([Singleton terms](#singleton-terms)). Not modelled: `PartialFunction`, `LazyList`,
`collection.Seq`, `Iterable`, `scala.Symbol`; dependency declarations are not read (no
dependency-sources mode).

## Evidence

- Scala 3 draft specification, ch. 2, `docs/_spec/02-identifiers-names-and-scopes.md` (main) —
  **Inspected (full text)** by the literature verifier (a draft, not versioned for 3.9.0): root
  imports have the lowest precedence; binding precedence (register D11).
- Odersky 2024, SIP-58, and the reference page *Named Tuples* — **Inspected (full text)** by the
  verifier: `NamedTuple` is an opaque alias of its value parameter, with `toTuple`.
- Knvl 2018, *Counting type inhabitants* — **Executable** (register D3, D4): sums, products,
  `μx. 1 + x ~ ℵ₀`.
- Smyth & Plotkin 1982 — **Bibliography-verified** (register D4): least fixed points.
- Finite texts over a finite alphabet are countable — **Classical** (register D2): at most ℵ₀
  finite programs.
- Scherer & Rémy 2015, ICFP — **Metadata-verified**: reports that the simply typed λ-calculus
  with sums has 0, 1 or ≥ 2 inhabitants under βη (related work).
- Scherer 2017, POPL — **Metadata-verified**: reports that βη-equivalence with sums and the empty
  type is decidable and coincides with contextual equivalence (related work).
- Scala 3.9.0 compiler fixtures — **compiler fixture**: the 93 root spellings under each listed
  namespace and inside an `IterableOnce` subclass; `scala.NoSuchElementException =:=
  java.util.NoSuchElementException`; no `scala.IllegalStateException`, `scala.Map` or
  `scala.Set`; `Array.empty[A]`, `new Array[A](0)` and the same for `x.type` with `x: A` need a
  `ClassTag` nobody supplies, `new Array[E](0)` builds for `Nothing`, `Null`, an empty sealed
  trait, an opaque type, an object's singleton and `x.type` with `x: String` or `x: Box[A]`, and
  `Array.empty[F[A]]` / `Array(x)` for `x: F[A]` (not `new Array[F[A]](0)`, which crashes the back
  end); under `import scala.quoted.*` another file's `q1.Expr` is not what `Expr` names;
  `ArraySeq.untagged` builds without one; two anonymous `using Either[A, A]` /
  `scala.util.Either[A, A]` are ambiguous for `summon`; an override may respell `Unit => Unit` as
  `Function1[Unit, Unit]`; `lib.quotes.reflect.Int` and `lib.Serializable`'s members shadow the
  builtins as the rules above say.
- scala-library 2.13 class file (`javap scala.collection.IterableOnce`) — **Inspected**:
  `iterator` abstract, `stepper` and `knownSize` default methods.
- **Executable**: `StdlibModelSpec`, `SequenceEliminationSpec`, `RepeatedParameterFragmentSpec`.
