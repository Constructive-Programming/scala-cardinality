# Abstract type members, paths and stuck projections

How the method-side count reads a type member (`type X`, `type X = …`) of a template or of a
stable path, a `{ type X = … }` refinement, and a match type that stays stuck. Register entry:
[D18](code-cardinality-foundations.md#d18-an-abstract-member-of-a-stable-path-is-a-rigid-atom).
Regressions: `TypeMemberSpec`, `TypeMemberBoundarySpec` and `TypeMemberScopeSpec` (positive and
adversarial; compiler facts pinned against `memberfixture`), `MethodMatchProjectionSpec`,
`MethodRefinementSpec`.

## Rules

**M0 — precedence.** Inside a template body a simple type name means, in order: a binder of a
method or local scope, a type parameter of the template, a type member of the template's
linearization (declared or inherited from a supplied parent), then enclosing scopes, packages and
imports (environment readings, rule 10, already reads inherited names first and refuses a name an
unsupplied parent may declare).

**M1 — members of a path.** A stable path is a self alias `outer =>` or the implicit `this` of a
member name, a parameter that is neither by-name nor `var`, or a local `val` with a declared type (a
template's field is not followed). A name is its innermost binding: a parameter or value of a nearer
scope hides an enclosing template's self alias (`def f(self: R)` inside `trait T { self => … }`
makes `self.X` R's member). Along the path's linearization — its refinement, its template, the
template's supplied parents, each read with the arguments the path gives it — the first alias is the
member's meaning (Scala lets an alias be overridden only by an equal one), read where it is declared
with `this` still the path. A member every level leaves abstract, with no bounds and no type
parameters, is the rigid atom `member:<path>#N`.

**M2 — members of the receiver.** A parent's declaration is a member of the template that inherits
it, so its type members are read on that receiver: Optic's `to: S => F[X, A]` read for
`PickFold` is `S => Affine[(Unit, Unit), A]`, and an alias body (`X = (T, M)` in `Optional`) reads
the receiver's `M`. Only `this` of the innermost template being read is the receiver: an outer
`O.this` (or an outer self alias) in a nested parent `P` is the enclosing instance, even where the
receiver `C` extends both `P` and `O` (scalac types `get: O.this.X` in `C` as `O.this.X`, not
`C.this.X`). Override identity keys a member the same way on both signatures.

**M3/M5 — values.** A trait or plain class with no parent, parameter, constructor modifier or self
type whose only members are types has the empty record (one value; universal members are outside the
model), when it has an instance: not a `final abstract` class. A stable value's type-only refinement
is read as its base when some value of the base has the refined members: the base is a supplied
template a subclass may extend anywhere (not final, a value class, an object, sealed, privately
constructed, or below an unsupplied parent), and no member the refinement pins is already fixed or
bounded along the base. Otherwise the refined type may have no value (`Boolean { type X }`,
`F { type X = String }` for a final `F` with `X = Int` — both rejected by scalac) and the input is
refused: every implementation would be vacuously one, never the base's count. A refinement with a
term member keeps `refined representation unavailable`.

**MT1′ — arguments as written.** A binder bound by an application keeps the argument as written
and its scope; a projection reads the argument again, so `Affine[(A, A), A]` reaches its fields'
`Fst[(A, A)] = A`.

**MT2 — irrefutable first case.** `T match { case _ => B; … }` is `B` for every `T`.

**MT3 — stuck projections.** A single-case match type whose pattern is a tuple of `n` distinct
binders and whose body is the `i`-th binder, applied to a rigid atom `α` (a binder without bounds
or a member atom), in a match type with no written upper bound (even `<: Any` is refused, which is
conservative), is the atom `proj:n:i:(α)`. A signature whose projections on one `α` disagree on
`n`, where a bound mentions `α`, or where type evidence (`α =:= β`, `<:<`) mentions `α`, is
refused: `ev.substituteCo[Fst](x)` turns a `Fst[α]` into a `Fst[β]` inside the stuck type, where the
atom hides `α`.

**G1 — implemented declarations.** A final root's inherited declaration that another level of its
linearization implements (same name and clause shape; a parameterless one also by a field, a
`val` or a `var`, which holds its initializer since mutation is excluded) is that level's concrete
code, not a free capability: dropped, or refused where the implementing template is local (its
captures are not read). An open root's implemented members stay the capabilities a subclass may override
(D-capabilities). **G2** (a member of a sealed family reads an unsplit receiver) is the
`SealedReceivers` gate, unchanged.

## Derivations

*Rigid atoms (M1).* In Scala an abstract member seen through a path is an abstract type: the typer
neither reduces nor refines it, two paths are distinct (`x: p.X` does not type as `q.X`, even for
`val q: R = p`) and a singleton alias is the same path (Scala 3.9.0 fixtures, `TypeMemberSpec
.pathFacts`; draft specification ch. 3 \[29\]). So the well-typed bodies are those of the rigid
reading. **Parity:** for a template whose member `X` no level fixes, the implementations of a member
`m` are those of the same template with `X` as a fresh type parameter — a subclass picks `X` as a
caller picks an argument, and that choice is free exactly when nothing constrains it — so the
count is the binder's (D3; executable: `TypeMemberSpec.parity`). A parameter path is the opening of
an existential package whose witness the caller picks (Mitchell & Plotkin 1988, C-10), named by
the path (dot notation, Cardelli & Leroy 1990, background).

*When the choice is not free.* The parity argument fails where no one supplying the path can pick
the member: a final class (a value class is one: "cannot extend final
class"; a `final abstract` class has no instance at all), object or given never instantiates it (a
concrete class may leave it undefined — `memberfixture.Never` compiles — and then no value of the
member exists, so every implementation is vacuously one); a private constructor, the template's
own or an ancestor class's, admits subclasses only in that class's body and companion (E173), and
a private or local template only in its enclosing scope, neither of which this reading enumerates
(a `private[pkg]` template stays open: any file of the package may extend it); a sealed family's
cases fix it when every case is closed (inside the family the receiver gate G2 answers; an open
trait or class among the cases lets any file pick the member, and only its own reason is true); a
`val` fixed by its own definition (a local value, a member of a closed template or object, a private
or final field) has one value whose member nobody outside sees; a parent outside the source set may
define it. This reading refuses the sealed family, the `val` (a local one as fixed, a field as a
path it does not follow) and the parent outside the source set; a final class, a private constructor
and a private or local template are not refused since the corpus-coverage prune. A bounded member
stays `?`: bounds
on abstract members are where Scala's subtyping loses transitivity (\[29\]: "known to break
transitivity"; the abstract of Hu & Lhoták 2020 (Metadata-verified) reports undecidable `D<:`
subtyping and decidable fragments without bad bounds). A type projection `P#X` of an abstract
member denotes no path's member — each value of `P` has its own (`∃p. p.X`) — so it is refused as
such, not read through the member's bounds.

*Refinements (M5).* Reading a stable input `p: B { type X = T }` as a `B` is sound only when some
`B` value has `X = T`: then the inputs are exactly the `B` values of the subclasses that pick it,
and the refinement only names `p.X`. Where none can — `B` cannot be extended, already fixes `X`
(scalac: "type X, which equals String has incompatible type", even with `override`), or the
refinement's bounds are bad ("cannot be instantiated since it has a member X with possibly
conflicting bounds") — the parameter type is empty and the base's count would overcount the
vacuous one. The check is syntactic and conservative: an alias the base already fixes to the same
type is refused too.

*Stuck projections (MT3).* (≤) With `α` abstract, a value of `proj(n,i,α)` comes only from a value
of the same stuck type: Scala reduces a match type only past provably disjoint cases and otherwise
leaves it stuck, "just an abstract type" (\[18\], \[1\], \[29\]), and nothing destructures an `α` (the
body `x` for `Fst[T]` from `x: T` does not type: `TypeMemberSpec.projectionFacts`). Type equality
of stuck applications is equality of the normalized match type, so the key is the projection, not
the alias that spells it (`First[T]` is `Fst[T]`). (≥) One instantiation `α = (β₁, …, βₙ)` with
fresh βs reduces every stuck projection of the signature to an independent variable at once, and
rigid terms never destructure `α`-values, so distinct rigid terms stay distinct (D3's free-atom
argument). That instantiation exists only when every projection on `α` has one arity: with `Fst[T]`
and a three-way `Fst3[T]` no caller supplies both, every implementation is vacuously equal, and
the atoms' 2 would overcount — refused.

*Implemented declarations (G1).* No subclass replaces a member of a final template, so an inherited
declaration a level implements is concrete code computing from the environment the template is
given; under the contract's model assumption on bodies it neither adds nor hides a value (D10, the
environment's concrete-code result). Left as a free capability it adds values the program cannot
produce (`D.other` would read ω, true 1).

## Limits (stay `?`, with the reason)

- `bounded abstract type member p#X (<bounds>)`; `higher-kinded abstract type member p#F` (also for
  an applied path member `rc.Out[S, C]`); `unavailable refined member: P#X` for a type projection
  or a member selected on a refinement.
- `… of p is fixed by the cases of sealed S` (only where every case is closed); `abstract type
  member p#X is fixed: p is a local value`; `path through the field p not followed`; `type member
  X of C.this may be defined by unsupplied parent P`.
- `refinement not shown satisfiable: B { … } (<why>)` — `B is not extended freely`, `already
  defines X`, `B is not a supplied template`, `refined members not read`.
- a refinement outside a stable input position keeps `refined representation unavailable
  (constraints not erased)`.
- `stuck match types on α have no common reducing instantiation: arities …`; `… meet a bound that
  mentions it`; `… meet type evidence that mentions it`; `inert match type: a bounded match type
  stays stuck on α`; a nominal, opaque or
  bounded-binder scrutinee keeps `inert match type`; a nested stuck projection is not read.
- `self type not resolved: T` for a self alias with a declared self type.
- `inherited declaration n is implemented in local C, whose captures are not read`.
- Opaque types outside their scope are not atoms (members.md §3.3, rule O1, is not adopted here).
- A path through a `val` of another file's package is read by the lookup as before (it may still
  disagree with the stored-value estimate, which refuses the position).

## References

Verification levels as in the evidence register. \[29\] Scala 3 draft specification ch. 3 —
**Inspected**; \[18\] Scala 3 reference, *Match Types* — **Inspected**; \[1\] Doeraene, SIP-56 —
**Inspected**; Mitchell & Plotkin 1988, TOPLAS 10(3) — **Metadata-verified**, classical record for
C-10 (the abstract reports abstract data type declarations typed as existential types); Hu &
Lhoták 2020, PACMPL 4(POPL), DOI 10.1145/3371077 — **Metadata-verified** (the abstract reports the
undecidability of `D<:` subtyping and decidable fragments without bad bounds); Blanvillain,
Brachthäuser, Kjaer & Odersky 2022, PACMPL 6(POPL), DOI 10.1145/3498698 — **Metadata-verified**
(the abstract reports match types formalized in a calculus based on F<: and proved sound); Rompf &
Amin 2016, OOPSLA — **Metadata-verified** (the abstract reports DOT sound); Cardelli & Leroy 1990,
*Abstract types and the dot notation*, DEC SRC RR 56 — **Metadata-verified**, background only.
Compiler fixtures: Scala 3.9.0, `TypeMemberSpec` (`typeChecks`) and `memberfixture`, plus the
members design session's `c01`–`c08`.
