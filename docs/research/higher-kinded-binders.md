# Higher-kinded and bounded type parameters

How the method-side count reads a binder of any kind (`F[_]`, `F[_, _]`, `C[_[_]]`), what a
higher-kinded argument (`Either`, `[x] =>> …`, `Forget[F]`) denotes, and what a context bound or
a subtype bound adds to an environment. Register entry: [D14](code-cardinality-foundations.md#d14-a-rigid-constructor-is-an-uninterpreted-base-type).
Regressions: `HigherKindedBindingSpec`, `HigherKindedRefusalSpec`, `BinderBoundsSpec` and
`KindedIdentitySpec` (every valid fixture compiled with Scala 3.9.0; every fixture marked invalid
checked to fail).

## Rules

**R1 — rigid constructors.** A higher-kinded binder with no subtype bound and only plain holes (no
variance, bound or context bound on a hole, recursively) is rigid. An application `F[τ₁…τₙ]` is an
uninterpreted base type: `Shape.Applied(Rigid(F), keys)`, with no constructor and no eliminator,
produced only by a value of the same head over the same argument keys. A binder that is never
mentioned contributes nothing — the old blanket reason, which named every refused binder of the
frame chain, is gone; a refused binder's reason counts only where the binder is read.

*Derivation.* (≤) System Fω is strongly normalising (Girard 1972); a β-normal term has no
introduction or elimination form for an abstract `F[τ]`, and the contract excludes type tests,
casts and reflection, so `F[τ]`-values in a normal form are variables and results of
environment callables — a base type of the simply typed calculus the solver already counts.
Relational parametricity for higher kinds (Atkey 2012) is the semantic form of the same
statement. (≥) Instantiate each rigid head as a constant constructor over ℕ: term syntax and
η-long normal forms are unchanged, and the full type structure over ℕ is complete for βη
(Friedman 1975), with coproducts (Dougherty & Subrahmanyam 2000), so distinct normal forms are
distinct programs — the premise the solver already rests on for proper binders (D3). Distinct
applications are distinct types (`F = [x] =>> x` with disjoint arguments separates them).

**R2 — nominal keys.** The arguments of a rigid head are keys, never shapes: `Option[A]` and
`Either[Unit, A]` share the shape `1 + A`, yet no program converts `F[Option[A]]` to
`F[Either[Unit, A]]`. A key is the declaration each name resolves to (`TypeIdentities.key`, or the
canonical key `BuiltinTypes.canonical` gives a builtin or a [stdlib-model](stdlib-model.md) entry —
`scala.Unit`, `scala.util.Either`, `scala.collection.immutable.List`, one per declaration whatever
its spelling — which override and evidence identity share; an excluded or unmodelled entry, whose
parameters the model does not hold, is no key) applied to its arguments' keys, each binder its own
atom. Keys respect Scala's equalities: a transparent alias expands in its own scope, an opaque type
only where it is visible, `(A, B)` is `Tuple2[A, B]` and `A => B` is `Function1[A, B]`, `A | A` is
`A`, and an operator argument is its η-long lambda with parameters numbered by position, so
α-equivalent operators share a key. Outside its scope an opaque type is no key at all: its API may
relate it to other types (`inside: F[W[A]] => F[A]`), which a key cannot record, and concrete code
is only irrelevant to the count when every input is readable. `=:=` evidence rewrites atoms inside
keys (Leibniz: `substituteCo` exists for every `F`); `<:<` evidence never lifts through an invariant
head. Rewriting replaces an occurrence by its equality class's representative, which depends on the
class alone, so a rewriting that ends unfolds each class at most once along any chain: one still
changing after as many steps as there are classes has met a class again and never ends
(`A =:= F[A]`, an equation no finite type solves for every `F`) — refused, never cut off by a fixed
budget. The builtin vocabulary is also what an import or an export must not shadow: an
`export foo.Function1` leaves every reading of `Function1` unknown.

**R3 — operator closures.** A callee is resolved first and its declaration's kinds direct its
arguments: a higher-kinded argument is an operator — a rigid binder passed through, anything else
a closure over the scope it was written in (`TypeOperators`), read only when applied. Applying a
closure β-reduces in that scope, so free names resolve where the constructor was written and its
parameters shadow outer binders (hygiene by environments, no syntactic substitution). A partially
applied type-lambda alias (`Forget[F]`) opens its lambda with the alias's parameters bound. A
callee with no supplied declaration reports only its own reason: nothing says whether its
arguments are types or constructors. Unapplied stdlib constructors reach the structural reading
through `BuiltinTypes` (`TupleN`, `FunctionN` added beside `Option` / `Either`), the hook the
stdlib model extends. An argument whose declaration fixes its kind — a class, trait, enum,
abstract member or builtin by its parameters, an explicit lambda by its own, a full application as
proper — is checked against the kind its position expects, applied or not: Scala 3.9.0 rejects
`Phantom[Option]` for `Phantom[F[_, _]]` ("does not have the same kind as its bound"). A builtin
applied at the wrong arity (`Either[A]`) says so in the shape reading as in the key reading.

**R4 — context bounds.** `[A: C₁: C₂]` is `[A](using C₁[A], C₂[A])` (`[G[_]: C]` is `using C[G]`;
SIP-64's `A: C as c` names it `c`): one more contextual parameter of the frame — for a class, of
its constructor too — counted exactly like an anonymous `using` (D10): reachable only through an
implicit search that provably selects it. Two same-typed evidences make every search ambiguous
(3.9.0: E172) and are refused. As a constructor position a class's bound is read where the `using`
parameter it stands for would be — inside the class, beside its binder — so `[A: Tag]` and
`(using Tag[A])` give one number (D11 reads a position where it is written; the evidence is built,
not parsed, so it borrows its binder's place).

**R5 — subtype bounds.** `A <: U` / `A >: L` are ambient proofs `A <:< U` / `L <:< A`: always
available, never values (`summon[A <:< U]` compiles; a proof's only behaviour is the upcast).
*Phase B1*: a bound to a binder or a rigid application is a directed view between nominal
endpoints, a cycle of them an equality (`A >: B <: B` is `A = B`). *Phase B2*: a bound to a
closed structural type — a sealed family, a case or final class, an enum, a builtin — views each
of the binder's values as the bound for elimination only (upper), or builds the binder's goal as
the bound (lower: split-and-rebuild already reaches the bound's values, so nothing is counted
twice). The fields a view is flattened into are values of their own, read through their own bounds
in the same pass: with `A <: P[B]`, `B <: Flag`, `a.x` is split by case like any Flag, so
`[A <: P[B], B <: Flag](a: A): Flag` is |Flag → Flag| = 4 whatever the goal records later. A
value's reading — unread, read with its views beside it, or a view — is a mark of the value, not a
step of its path (a path starts with a binding id, which any source path may spell). `A <: Any`
and `A >: Nothing` say nothing — where the name is the builtin's as the shape reading reads it: a
channel that may rebind it (for a top name, a wildcard over a namespace no source supplies, D12)
leaves a bound to read, refused with that channel. `AnyRef`, `AnyVal` and `Matchable` above say
nothing of the binder's values (the contract excludes the runtime type tests `Matchable` permits),
read past the same channels, but they are not top types: a structural lower bound below one — on
the binder itself, or on a binder a bound or a proof puts below it — must be an instance of that
class, read from its declaration (a class or enum is an `AnyRef` unless it extends `AnyVal`, a
trait without parents is an `AnyRef`, tuples and functions are, value builtins are `AnyVal`s).
Otherwise the binder is refused: Scala 3.9.0 accepts `[A >: Flag <: AnyVal]` and rejects every
call (E057). A bounded binder's evidence is never provably unrelated to another candidate.
Subtyping with bounded quantification is undecidable in full F<: (Pierce 1994); this fragment is a
finite graph of binder edges plus closed views.

Bounds that refer back to their binder are never read. Where the compiler follows bounds alone
back to the binder — a binder an upper bound names, itself or as a union or intersection
component, and every binder a lower bound mentions — the clause is no program (E140: `[A <: B, B <:
A]`, `[A >: B, B >: A]`, `[A >: Option[A]]`), refused as invalid whether or not the binder is read
— and whatever the result: a goal with one value (D12) has one implementation only in a signature
that is a program.
Inside an upper bound's applied type the compiler stops, so an F-bound `A <: P[A]` is legal; it
describes A as a fixpoint — every value of A holds a smaller one — which for an invariant final
`P` has no value at all (a vacuous context) while `A <: Option[A]` admits `None`. The shapes do not
carry that inhabitation argument, so a bound whose structure holds its binder again, directly, via
other bounds (`[B <: Option[A], A <: B]`) or via a proof (`using A <:< B` beside `B <: P[A]`), is
refused; a binder held only as an application's argument (`A <: F[A]`) is a view and stays read.

Structural lower and upper bounds are never read together. Where a binder bounded below by a
structural type and one bounded above by one share a supertype in the graph (either may be it),
a value viewed through the upper bound and a construction through the lower bound may be the same
value: `[A >: Flag <: Flag](x: A): A` pins `A` to `Flag`, so the count is |Flag → Flag| = 4, but
reading `x` beside the constructions `On`, `Off` in each case of `x` gives 3² = 9; with
`Big = {One, Two : Sub; Three}`, `[A >: Sub <: Big](x: A): A` is 2·2·3 = 12, not 27. And the bounds
may admit no type at all: Scala 3.9.0 accepts `[A >: Flag <: Boolean]` and rejects every call
(E057), a vacuous context whose count is 1. Shapes erase the nominal identity that would decide
either, so the meeting is refused.

## Boundary (each stays `?` with the reason named)

| Case | Reason |
|---|---|
| variance on a rigid hole (`F[+_]`) | `variant higher-kinded parameter requires subtyping between applications` |
| bound on a higher-kinded binder or a hole | `bounded higher-kinded parameter requires its bound's capabilities` |
| upper bound to an open trait or abstract class | `upper bound requires capability analysis` |
| bound to a type the solver cannot eliminate (paths, proofs, packages) | `bound to a structural type requires coercion analysis` |
| foreign bound or evidence type | the foreign name's own reason |
| a bound whose structure holds its binder again (`A <: P[A]`, `[B <: Option[A], A <: B]`) | `recursive bound not read` |
| the same through a subtype proof (`using A <:< B`, `B <: P[A]`) | `… a structural view holds a value of its own type` |
| bounds referring back through bounds alone (invalid: E140), used or not, a `Unit` result too | `cyclic type bound (an invalid program)` |
| `AnyRef` / `AnyVal` / `Matchable` above a structural lower bound whose class is not fixed or differs (`A >: Flag <: AnyVal`: every call E057) | `structural lower bound under a class bound requires class analysis` |
| a view bound `A <% T` (invalid: removed in Scala 3), used or not, a `Unit` result too | `view bound is not Scala 3 (an invalid program)` |
| `<: Any` / `AnyRef` / `Object` / `AnyVal` / `Matchable` or `>: Nothing` where a channel may rebind the name (`import foreign.*`) | `builtin name … may be shadowed through …` (the channel's reason) |
| any builtin or model name under `import java.lang.*` (`Boolean`, `Byte`, `Short`, `Long`, `Float`, `Double` are the boxed classes there; the model does not list `java.lang`'s other members, so `Int` is refused too — D16) | `builtin name … may be shadowed through a reference not supplied: java.lang (wildcard import of java.lang)` |
| a subtype proof beside a callable or a sum value | `unsupported subtype evidence interaction` (contravariant uses are not read) |
| a callable producing a structurally bounded value | `… a callable produces a value its bound views as a structural type` |
| two values a structural bound views producing their own type | `… values a bound views as a structural type may be the same case` |
| structural lower and upper bounds sharing a supertype (`A >: Flag <: Flag`, `A >: Flag <: Boolean`) | `… a structural lower bound meets an upper one` |
| an opaque type outside its scope in a key | `opaque representation is not visible at use site` |
| an equality proof that never settles (`A =:= F[A]`) | `unsupported evidence relationship: an equality proof keeps unfolding` |
| a rigid binder applied to a wildcard, `F[?]` (invalid: E043) | `unreducible application of a higher-kinded binder to a wildcard` |
| a structurally substituted argument under a rigid head | `nominal identity of substituted argument not tracked` (the shape lost the identity; exact counts need the argument's syntax) |
| unions of distinct keys, nested wildcards, refinements | `nominal identity of argument not established` |
| abstract type members, path types, inherited members in keys | `nominal identity of … not established` (members cluster) |
| a wildcard where a constructor is expected | `existential type constructor requires witness-kind analysis` |
| arity or kind mismatch (invalid programs), a constructor argument of the wrong kind applied or not | `type argument arity` / `kind mismatch` / `higher-kinded parameter used as a proper type` |
| an unapplied type lambda where a type is expected | `unapplied type lambda used as a proper type` |
| operators whose holes are themselves higher-kinded, in keys | `nominal identity of argument not established` |
| higher-kinded binders of polymorphic function types | `constrained polymorphic function` |

## References

- J.-Y. Girard, strong normalisation of System Fω (1972) — **classical**.
- R. Atkey, *Relational Parametricity for Higher Kinds*, CSL 2012, DOI 10.4230/LIPIcs.CSL.2012.46 —
  **metadata-verified** (design memo session); a model, not a counting algorithm.
- D. Vytiniotis & S. Weirich, *Parametricity, Type Equality and Higher-order Polymorphism*, JFP
  20(2), 2010, DOI 10.1017/S0956796810000079 — **metadata-verified**; equality is inhabited only by
  the identity, which justifies rewriting under `=:=`.
- H. Friedman, *Equality between functionals*, LNM 453, 1975 — **metadata-verified via secondary
  sources**; D. Dougherty & R. Subrahmanyam, *Equality between functionals in the presence of
  coproducts*, Inf. Comput. 157, 2000 — **metadata and abstract verified**; completeness of the
  set-theoretic model for βη (with sums).
- B. C. Pierce, *Bounded quantification is undecidable*, Inf. Comput. 112(1), 1994 —
  **metadata-verified**.
- Scala 3.9.0 compiler fixtures (this session): context bounds desugar to `using` parameters and
  two same-typed ones are ambiguous (E172); `summon[B]` with `A <: B` evidences of both selects the
  `B` parameter (run); kind-projector placeholders, named context bounds and every positive
  fixture of the spec compile. `F[?]` for an abstract `F` (method or class binder, variant or
  bounded) is E043; `[A >: Flag <: Boolean]` compiles and every call is E057; the companion object
  of a top-level type alias compiles into the file's package object (`f$package$MF`) and sees the
  file's opaque types, where another top-level object does not (E007). Bounds referring back
  through bounds alone are E140 (`[A <: B, B <: A]`, `[A >: Option[A]]`, `[A <: B | Int, B <:
  A]`); `[A <: P[A]]`, `[A <: P[B], B <: P[A]]`, `[B <: Option[A], A <: B]` and `[B, A >: B <: B]`
  compile, while `[A <: B, B <: P[A]]` is E046 in that order only. `[A >: Flag <: AnyVal]` and `[A
  >: Boolean <: AnyRef]` compile and every call is E057. `A <% Int` is rejected ("view bounds are no
  longer supported"); `Phantom[Option]` for `Phantom[F[_, _]]` and `Box[[x, y] =>> x, A]` for
  `Box[F[_], A]` are rejected ("does not have the same kind as its bound"). Under `import
  java.lang.*`, `val b: Boolean = null` compiles and `F[Boolean]` is not an `F[scala.Boolean]`
  (E007).
