# Higher-kinded and bounded type parameters

How the method-side count reads a binder of any kind (`F[_]`, `F[_, _]`, `C[_[_]]`), what a
higher-kinded argument (`Either`, `[x] =>> …`, `Forget[F]`) denotes, and what a context bound or
a subtype bound adds to an environment. Register entry: [D14](code-cardinality-foundations.md#d14-a-rigid-constructor-is-an-uninterpreted-base-type).
Regressions: `HigherKindedBindingSpec`, `HigherKindedRefusalSpec`, `BinderBoundsSpec` and
`KindedIdentitySpec` (every valid fixture compiled with Scala 3.9.0; every fixture marked invalid
checked to fail).

## Rules

**R1 — rigid constructors.** A higher-kinded binder with no subtype bound and only plain holes (no
bound or context bound on a hole, recursively; a hole's variance is not read) is rigid. An application `F[τ₁…τₙ]` is an
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
`F[Either[Unit, A]]`. A key is the canonical key `BuiltinTypes.canonical` gives a builtin or a
[stdlib-model](stdlib-model.md) entry (`scala.Unit`, `scala.util.Either`,
`scala.collection.immutable.List`, one per declaration whatever its spelling, which override and
evidence identity share; an excluded or unmodelled entry, whose parameters the model does not
hold, is no key) applied to its arguments' keys, each binder its own atom; a class, trait or enum
of the sources is no key. Keys respect Scala's equalities: a transparent alias expands in its own
scope, an opaque type only where it is visible, `(A, B)` is `Tuple2[A, B]` and `A => B` is
`Function1[A, B]`, and an operator argument is its η-long lambda with parameters numbered by
position, so α-equivalent operators share a key. Outside its scope an opaque type is no key at all:
its API may relate it to other types (`inside: F[W[A]] => F[A]`), which a key cannot record, and
concrete code is only irrelevant to the count when every input is readable. `=:=` evidence rewrites
atoms inside keys (Leibniz: `substituteCo` exists for every `F`); `<:<` evidence never lifts through
an invariant head. Rewriting replaces an occurrence by its equality class's representative, which
depends on the class alone, so a rewriting that ends unfolds each class at most once along any
chain: one still changing after as many steps as there are classes has met a class again and never
ends (`A =:= F[A]`, an equation no finite type solves for every `F`) — refused, never cut off by a
fixed budget. The builtin vocabulary is also what an import or an export must not shadow: an
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
This reading constructs none of them: a binder the environment mentions is refused when it has a
lower bound, or an upper bound other than `Any`, `AnyRef`, `AnyVal` or `Matchable`. `A <: Any`
says nothing — where the name is the builtin's as the shape reading reads it: a channel that may
rebind it (for a top name, a wildcard over a namespace no source supplies, D12) leaves a bound to
read, refused with that channel. `AnyRef`, `AnyVal` and `Matchable` above say nothing of the
binder's values (the contract excludes the runtime type tests `Matchable` permits), read past the
same channels.
Subtyping with bounded quantification is undecidable in full F<: (Pierce 1994).

Bounds that refer back to their binder are never read. Where the compiler follows bounds alone
back to the binder — a binder an upper bound names, itself or as a union or intersection
component, and every binder a lower bound mentions — the clause is no program (E140: `[A <: B, B <:
A]`, `[A >: B, B >: A]`, `[A >: Option[A]]`). Such a clause is not detected as invalid here.

## Boundary (each stays `?` with the reason named)

| Case | Reason |
|---|---|
| bound on a higher-kinded binder or a hole | `bounded higher-kinded parameter requires its bound's capabilities` |
| upper bound to an open class | `upper bound requires capability analysis` |
| any other upper bound the shape reading resolves | `bound to a structural type requires coercion analysis` |
| foreign bound or evidence type | the foreign name's own reason |
| any lower bound | `lower bound not read` |
| a subtype proof or a binder bound the solver reaches | `unsupported evidence relationship: only equality proofs between endpoints are read` |
| `<: Any` / `AnyRef` / `Object` / `AnyVal` / `Matchable` where a channel may rebind the name (`import foreign.*`) | `builtin name … may be shadowed through …` (the channel's reason) |
| any builtin or model name under `import java.lang.*` (`Boolean`, `Byte`, `Short`, `Long`, `Float`, `Double` are the boxed classes there; the model does not list `java.lang`'s other members, so `Int` is refused too — D16) | `builtin name … may be shadowed through a reference not supplied: java.lang (wildcard import of java.lang)` |
| an opaque type outside its scope in a key | `opaque representation is not visible at use site` |
| an equality proof that never settles (`A =:= F[A]`) | `unsupported evidence relationship: an equality proof keeps unfolding` |
| a rigid binder applied to a wildcard, `F[?]` (invalid: E043) | `unreducible application of a higher-kinded binder to a wildcard` |
| a structurally substituted argument under a rigid head | `nominal identity of substituted argument not tracked` (the shape lost the identity; exact counts need the argument's syntax) |
| unions, a class, trait or enum of the sources, nested wildcards, refinements | `nominal identity of argument not established` |
| abstract type members, path types, inherited members in keys | `nominal identity of … not established` (members cluster) |
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
