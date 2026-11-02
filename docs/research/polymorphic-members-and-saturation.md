# Polymorphic capability members and callable sums

How the method-side count reads a supplied member with its own type parameters, how it eliminates a
supplied callable whose result is a sum or a ground value, and when a productive cycle answers ω
although other producers carry reasons. Register entries:
[D24](code-cardinality-foundations.md#d24-a-polymorphic-capability-member-is-read-up-to-its-free-theorems)
and
[D25](code-cardinality-foundations.md#d25-a-supplied-sum-is-eliminated-once-per-argument-and-path).
Design memo: capabilities.md C8, C10 and C11 (restricted as SYNTHESIS D-capabilities and crosscheck
X3 require; X7 and X8 are answered below). Regressions: `PolymorphicMemberSpec`,
`NeutralSaturationSpec` (fixtures compiled with scalac 3.9.0; the C11 fixture is the crosscheck's
`c11`), and the updated `OpaqueSumForwardingSpec`, `InhabitationSpec`, `GroundTypesSpec`.

## Rules

**PB0 — phantom binders.** A member binder its type never mentions is irrelevant: every
instantiation gives the same monomorphic type — provided one exists. A one-sided bound is always
met, whatever other binders it mentions (`Nothing` below an upper bound, `Any` above a lower one);
a two-sided bound need not be: no `M` has `Int <: M <: String`, and none provably has
`X <: M <: String` for a rigid `X`, so scalac rejects every call of such a member and the
environment has one value fewer than a callable `f` would give. A binder with both bounds is
therefore refused (`polymorphic capability binder: M (bounded)`), never dropped.

**PB1 — Yoneda.** After uncurrying (tuple parameters spread), a binder `M` met only in generators
`Pᵢ → M` (`Pᵢ` free of the member's binders; a bare `M` parameter is `1 → M`) and otherwise only at
positive positions of the rest `G(M) = Rest → Result` (functions, products and sums; any other
constructor mentioning `M` — a rigid application, a sequence, a proof, a package — fails the test)
is the sum of its generators' domains:

    ∀M. (P₁ → M) → … → (Pₖ → M) → G(M)  ≅  G(P₁ + … + Pₖ)

`∀M. (A → M) → S → M ≅ S → A` (a fold is its focus), `∀C. C → C ≅ 1` (a known identity),
`∀M. M → M → M ≅ 1 + 1` (a tag), `∀M. M → Boolean ≅ Boolean` (a constant), and with no generator
`∀C. C ≅ 0`: a supplied member of that type is a value of the empty type, so the environment is
vacuous (count 1), and a goal of it has none (count 0). Unit parameters are dropped afterwards
(`1 → A ≅ A`). The reading is a type isomorphism, so it applies to an input and to a goal alike.

A member's type is read where its level declares it, at the record's arguments: a template alias
`type T = A` is the argument the record gives `A`, never the template's own binder nor a member
binder that shadows `A` (on a `Box[X]`, `trait Box[A] { type T = A; def get[A](t: T): A }` has
`get ≅ ∀A'. X → A'`, the empty type once an `X` exists; scalac types `b.get[String](1)` on a
`Box[Int]`). Two records of one template at two arguments are read each at its own.

**PB2–PB4 — the rest.** A member PB1 does not reduce stays polymorphic (`Shape.Poly`). As a goal,
its binders are fresh rigid atoms. As an input, each producer path is matched against each goal
(first-order matching; the binders are the variables):

- the path mentions none of the member's binders: an ordinary producer (PB0), cased and applied like
  any callable's path (`f[M](a: A): (M => M, Boolean)` has the path `A → Boolean`);
- the match fixes every binder the path mentions: an application at those types, whose count is
  reported only when it is 0 or 1 (PB2), and refused above with `polymorphic capability binder: M
  (free-theorem quotient not modelled for C.m)`;
- no match: the path builds nothing for that goal (PB3);
- a binder the match leaves free, or a result eliminated rather than matched (a sum, a ground
  value, a sequence, a package): `polymorphic capability binder: M (not determined by the result of
  C.m)` wherever the path is relevant (PB4). An instantiation the goal fixes at an eliminated type
  keeps the quotient reason.

A bounded binder that occurs, any context- or view-bounded one and any with two bounds is
`polymorphic capability binder: M (bounded)`; a higher-kinded one `polymorphic capability binder:
G[_] (higher-kinded)`.

**PB2's boundary.** A path whose result is a bare binder `M` can be instantiated at a compound type
whose value is then applied, projected or cased (`run[(B, B)](swap, (b1, b2))._1`), a term no
instantiation at a goal represents (derivation below). With such a member in the context only ω,
which such terms cannot lower, and a context proved absurd, which they cannot make consistent, are
reported; any other count is `polymorphic capability binder: M (instantiations at compound types
not searched for C.m)`.

Cats evidence (`Monoid`, `Functor`, `Applicative`) is never reduced by laws: it is a foreign
declaration, and a source algebra (`combine: (M, M) → M`) fails PB1's polarity test.

**Deferred inline members.** An abstract `inline` (or `transparent inline`) member — a method or a
value — can be called only from an inline body: scalac rejects the call anywhere else ("Deferred
inline method … cannot be invoked"), and an inline body's expansion calls the override of the
receiver's static type at that call site. So for a target that is not inline the member is no term;
its type only says whether an instance can exist. A type built from nothing (its atoms rigid: a
constant, a tag, `Boolean ⇒ Boolean`) is inhabited however the atoms are chosen, so the member
constrains no instance and is dropped: `def t(c: T): Boolean` with `inline def f[M](x: M): Boolean`
in `T` is 2. Any other type might be empty together with the rest of the environment (`inline def
neg: A => Nothing` beside an `a: A` admits no instance, count 1), which this reading does not decide:
`deferred inline member whose type may be empty: C.m`. For an inline target the member is `deferred
inline member resolved at each expansion: C.m`.

**C10 — callable sums.** For every supplied producer path `p : P̄ → R` with `R` a nonempty sum or a
ground type, the solver adds each new neutral `p(t̄)` to the context once per argument tuple and path
(an eliminated neutral leaves a marker, so a branch never cases on it again) and its existing split
— the uniform closed split for a ground result — counts the cases. Arguments range over the
neutral terms of first-order argument types: variables, and applications of producers that are not
themselves eliminated, enumerated per type. A sum-valued neutral is never also an answer: that is
the η-case of its split. A path is refused when:

- what its cases bind leads back, through any producer, to its own argument types, or an argument
  type is built by a cycle of producers: `sum elimination over an unbounded argument space: p`;
- an argument type is ground, a sum, a sequence, has a structural lower bound or may be built by a
  polymorphic input: `sum elimination over an argument space not enumerated: p`;
- an argument is higher-order or a package: the existing `Elimination of an opaque … unsupported`.

A reason names the callable by the source name of the binding it is read from (`g:h` stays `g:h`),
and by the fields that lead to it when it is a member of a record or a field of a product:
`inner#0` is the first field of `inner`'s record (records are anonymous products, so the member's
own name is not kept). Neutrals are
identified by their structure — the callable, its path index and its argument terms — never by a
rendering of names, so two distinct neutrals never merge however their values are named.

The forwarding certificate (`OpaqueSumForwarding`) still answers where nothing was eliminated yet;
on every case it covered, saturation agrees with it.

**C11 — ω beside reasons (restricted).** A root whose rules carry reasons is ω when it reaches a
cycle along rules with no reason and no polymorphic instantiation, every child of every such rule
is built by rules with no reason, and at every goal on the way (the cycle included) the empty goal
is not built even when reason-carrying rules are allowed. Eager (resolution-time) reasons never
reach the solver.

## Derivations

*PB0, PB1.* Supplied members are parametric (register D3). For covariant `G`, the Yoneda lemma
(classical, C-6) read in a relationally parametric model gives the isomorphism above: the maps
`m ↦ m[ΣP](inj₁, …, injₖ)` and `m' ↦ ΛN. λḡ. G([ḡ])(m')` are definable and mutually inverse up to
parametricity. A count of implementations compares programs by their behaviour over every supply of
the environment; the isomorphism is a bijection between supplies of `∀M.τ` and of `G(ΣP)` that
preserves every program's behaviour, so the two environments have the same count. As a goal the
same maps carry implementations both ways. Why not instantiate naively: `foldMap(identity)(s)`,
`foldMap(a => foldMap(_ => a)(s))(s)`, … are pairwise distinct terms but one value under
parametricity (`PolymorphicMemberSpec.fold`: naive ω, true 1).

*PB2.* Instantiation is restricted to binders the goal fixes (crosscheck X7's alternative).
Coverage holds for a member none of whose paths ends in a bare binder. A path the solver
instantiates then ends in an application of a rigid constructor (`F[M]`), which no long normal form
eliminates further: every application of it stands at a goal of the solver's graph whose match fixes
its instantiation (any other path is refused by PB4 or mentions no binder), so every long normal
form has its representative among the enumerated derivations. Quotienting by free theorems
only merges terms, so the naive count bounds the true one from above; a found derivation is a real
term, so a naive 0 or 1 is exact (a derivation of the empty type proves the context absurd: 1). A
larger naive count is refused, never reported.

Coverage fails for a bare-binder result, which a normal form may instantiate at a compound type and
eliminate afterwards: at a pair and project, at a function and apply, at a sum and case. With
`trait Church { def run[M](f: M => M, x: M): M }`, the term `c.run[(B, B)](p => (p._2, p._1), (b1,
b2))._1` is b1 for even numerals and b2 for odd ones. The search never has a goal `(B, B)` (it
builds products field-wise), and no term instantiating `run` at `B` denotes that function: every
`B → B` definable from `b1`, `b2` and `run[B]` is, at a fixed numeral, the identity or a constant,
so `fⁿ(x)` depends only on whether n is 0. So the enumeration misses terms, the naive count is no
bound, and a 0 or 1 found there is not exact; hence PB2's boundary. Two answers survive it: ω, since
missed terms only add to a set that is already infinite, and a context proved absurd, since a found
derivation of the empty type is a real term. (Round 1 claimed coverage for every member; that
lemma was false.)

*C10.* Pure and total, a case commutes with every context, and casing twice on one scrutinee is
redundant (β and η for sums), so every term equals one that cases on each neutral of its context
exactly once, as soon as its arguments exist: the saturated normal forms of focused proof search.
Distinct neutral terms are distinct values for some supply of the free producers, so their cases
combine independently; a case that makes the context absurd is collapsed by the solver's per-node
absurd rule (crosscheck X8: the empty-type part rests on that rule, never on an at-most-two
cutoff). Finiteness is checked, not assumed: the argument enumeration and the case-to-argument
graph refuse every unbounded family.

*C11.* Lower bound: the cycle's terms are pairwise distinct terms of every rule along the path, and
each sibling has a real term, so there are infinitely many. They stay distinct: further producers
can merge terms only by making a context absurd (crosscheck X3's `branchAbsurd`) or a sibling empty
(`siblingZero`), and both are excluded at every goal on the way. Upper bound: a program text is
finite (register D2).

## Limits (each stays `?` with the reason given)

- Non-reducible polymorphic members whose naive count exceeds 1, members whose binders the goal
  does not fix, bounded, context-bounded and higher-kinded binders (reasons above).
- PB1 does not see through rigid constructors (no functorial action), sequences, proofs or other
  members' binders, and does not uncurry a curried generator (`A => B => M`): those stay PB2–PB4.
- Callable sums over ground, sum, sequence or lower-bounded arguments, over arguments a polymorphic
  input may build, and over unbounded families (reasons above); ω for them needs C11's conditions,
  which a cycle behind a reason-carrying rule never meets (`sums1.U.first` stays `?` though its
  model count is ω).
- C11 never fires through a polymorphic instantiation, a reason-carrying rule, or a goal whose
  context may be absurd.
- A context holding a member whose result is a bare binder has no finite count, even where the true
  count is 0 (`def zero[A](c: Church): A`).
- A member whose result is a bare binder matches every goal; beside a type constructor it grows the
  goals without end (`trait W[F[_]] { def wrap[A](a: A): F[A]; def unwrap[A](fa: F[A]): A }`: at
  the goal `F[B]`, `unwrap` at `A := F[B]` needs `F[F[B]]`, …), so `cyc[F[_], B](w: W[F], b: B):
  F[B]` (true count 1: `unwrap ∘ wrap` is `∀A. A → A`, the identity) and `W.wrap` (true 0) answer
  `Inhabitation state budget exceeded (256)` rather than the quotient reason.
- A deferred inline member whose type is not built from nothing is refused even when the count
  without it is 0 (`def t3[A](c: T3[A]): A` with `inline def get: A`, true 0).

## References

- Reynolds 1983 [9] (**bibliography-verified**) and Wadler 1989 [37] (**metadata-verified**):
  parametric uniformity, register D3. The Yoneda lemma (C-6, **classical**); its
  parametric-model form here is a derivation, not a citation.
- Pistone & Tranchini 2021, CSL, DOI 10.4230/LIPIcs.CSL.2021.35 (**metadata-verified**): the abstract
  reports that the Yoneda type isomorphisms induce a rewriting on types that eliminates quantifiers.
  PB1's exact condition and the counting use are not shown there.
- Plotkin & Abadi 1993, TLCA (**metadata-verified**): the abstract reports encodings of data types
  under parametricity. Hasegawa 1994 and Bainbridge, Freyd, Scedrov & Scott 1990
  (**metadata-verified**): background only.
- Urzyczyn 1997, TLCA (**metadata-verified**): the abstract reports that inhabitation is undecidable
  for the polymorphic second-order λ-calculus (C-7, **classical**) — why counts above 1 for
  polymorphic members need an exact reduction. Scala 3 reference *Polymorphic Function Types* [23]
  (**inspected**): a binder may be instantiated at a polymorphic function type.
- Wadler 1990, *Recursive types for free!* (unpublished draft; **metadata-verified**): cited for
  the exclusion of algebra premises (`Φ(M) → M`, an initial-algebra encoding) from PB1.
- Scherer 2017, POPL (**metadata-verified**): the abstract reports that βη-equivalence with sums and
  the empty type is decidable and coincides with contextual equivalence. Scherer & Rémy 2015, ICFP
  (**metadata-verified**): the summary reports 0, 1 or ≥ 2 inhabitants decided by saturating
  focused search; for "cut only new sums" background only. Scherer 2016, thesis
  (**metadata-verified**). No source states the per-path elimination or the restricted ω
  conjunction: both are this register's derivations, backed by the executable regressions.
- Espírito Santo, Matthes & Pinto 2021, TYPES 2020, DOI 10.4230/LIPIcs.TYPES.2020.4
  (**metadata-verified**): the summary reports finiteness of focused inhabitants decidable with
  disjunction. David & Zaionc 2009 (**metadata-verified**): background for counting normal proofs.
