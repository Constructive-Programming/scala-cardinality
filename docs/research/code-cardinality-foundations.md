# Evidence register: defending the counting decisions

**Purpose.** This is the body of material behind the [v1 counting contract](../plans/v1.md#_2-agreed-counting-contract):
for each decision the analyzer makes — and will keep making as it audits codebases — the sources
that justify it, how far each source was verified, and the boundary beyond which it must not be
stretched. When a review questions a number, this is the register to answer from.

**Verification levels.** Every source states one:

- **Executable** — a runnable artifact in this repository (a spec, the analyzer itself).
- **Inspected** — the text (abstract, page, or PDF) was read in the session cited.
- **Bibliography-verified** — the item's reference list was inspected (via Crossref), but the full
  text was not.
- **Metadata-verified** — publication metadata was confirmed via Crossref; content not read.
- **Classical** — a foundational result stated as such, not re-verified this session.

No entry below claims more than its level. The [Russell citation inventory](russell-citation-inventory.md)
records the database-reported line of descent in full.

## D1. Fix the collection before counting

**Decision.** A count is only meaningful once the language, the accessible environment, the
equivalence, and the model assumptions are fixed ([plan §2](../plans/v1.md#_2-agreed-counting-contract)).
A free type parameter is one atom per binder, never "a type of unknown finite size"; binder
identity is per declaration.

**Evidence.**

- Russell 1907, *On Some Difficulties in the Theory of Transfinite Numbers and Order Types*
  ([DOI](https://doi.org/10.1112/plms/s2-4.1.29)) — **metadata-verified**; the paper's argument is
  documented via the [Stanford Encyclopedia's Russell-paradox entry](https://plato.stanford.edu/entries/russell-paradox/)
  (**inspected**), which places this paper in Russell's struggle over which collections a
  definition may form: the zigzag theory, limitation of size, and the no-classes theory that
  preceded the vicious-circle principle. The lesson the analyzer takes is not any one of those
  systems but the shared constraint: **a definition does not automatically license a collection**,
  and unrestricted comprehension fails.
- Luna & Taylor 2010, *Cantor's Proof in the Full Definable Universe*
  ([DOI](https://doi.org/10.26686/ajl.v9i0.1818)) — **abstract inspected**. Cantor's powerset
  argument restricted to the definable universe "seems to be countable on one account and
  uncountable on another"; the resolution is that **definitional contexts restrict the scope of
  quantifiers**. That is precisely our rule: the same type expression counted in different scopes
  (different environments, different languages) is a different question.
- Fan 2020, *Hobson's Conception of Definable Numbers*
  ([DOI](https://doi.org/10.1080/01445340.2020.1731784)) — **bibliography-verified** (cites
  Russell 1907 directly); language-relative definability in the Hobson–Richard tradition, with
  the diagonal generation of definitions connected to computability. Full text paywalled.

**Boundary.** None of these sources says *which* scope rules Scala has; they justify requiring
explicit scope rules at all. Our particular rules (lexical chain, package peers, `this.x`
surviving shadowing) are defended by our own regressions, not by 1907.

## D2. Mathematical inhabitants are not expressible implementations

**Decision.** The analyzer reports `Finite`, `Countable` (ω), or `Unresolved` — never an
uncountable implementation count. Stored-value estimates are kept in a separate report section.

**Evidence.**

- The set of finite program texts over a finite alphabet is countable (an elementary
  enumeration argument; **classical**). Restricting to well-typed, pure, total programs and
  quotienting by observational equivalence cannot increase that cardinality. So for any fixed,
  finitely-described environment, the expressible implementations of a signature form a set of at
  most countably many observation classes — even where the *mathematical* function space the
  signature ranges over is uncountable (`2^ℵ₀` functions exist; only countably many are
  finitely expressible).
- Reynolds 1984, *Polymorphism is not set-theoretic*
  ([DOI](https://doi.org/10.1007/3-540-13346-1_7)) — **abstract inspected**: "we will prove that
  the standard set-theoretic model of the ordinary typed lambda calculus cannot be extended to
  model this \[polymorphic\] language extension." A naive reading of types as sets of *all* values
  breaks down exactly where our analyzer works — at polymorphism. This is why our generic-method
  counter never treats a type variable as "all values of some unknown set".
- Pitts 1987, *Polymorphism is set theoretic, constructively*
  ([DOI](https://doi.org/10.1007/3-540-18508-9_18)) — **metadata-verified**, bibliography
  inspected: a constructive model of the same calculus exists. Together with Reynolds 1984 this
  says the choice of semantic model is a real decision with consequences — which is why the plan
  states the model assumptions explicitly rather than inheriting them from the `Size` algebra.
- Russell 1907 (D1) — the definability paradoxes are the historical root of the same separation:
  "all definable" and "all" are different collections.

**Boundary.** Countability does not mean effective enumerability with decidable equality or
totality (D7). Reynolds and Pitts argue about specific idealized calculi, not Scala; they
justify our *shape* of answer, not any particular number we emit.

## D3. Parametricity constrains generic implementations

**Decision.** A universally quantified signature is counted uniformly across its admissible
instantiations: `∀a. a -> a ~ 1`, `choose[A](x: A, y: A): A ~ 2`, `Pair`'s four-construction
count. Concrete types mention concrete sizes.

**Evidence.**

- Reynolds 1983, *Types, Abstraction and Parametric Polymorphism* (Information Processing 83,
  pp. 513–523) — **bibliography-verified** via Reynolds 1984's reference list; the origin of
  relational parametricity for the polymorphic λ-calculus.
- Wadler 1989, *Theorems for free!*
  ([DOI](https://doi.org/10.1145/99370.99404), FPCA '89, pp. 347–359) — **metadata-verified**:
  from parametricity alone, a polymorphic function satisfies free theorems (e.g. `r (map f as)
  = map f (r as)`), which is the formal statement of the uniformity our counts depend on.
- Milewski 2014, *Parametricity: Money for Nothing and Theorems for Free*
  ([blog](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/))
  — **inspected** (earlier session): the accessible statement of the same argument that motivated
  this project's scope-relative reading.
- Knvl 2018, *Counting type inhabitants*
  ([archived](https://web.archive.org/web/20181221193229/https://alexknvl.com/posts/counting-type-inhabitants.html))
  — **executable**: the worked examples our `ReferenceInhabitantsSpec` encodes, including
  `∀a. (a,a) -> (a,a) ~ 4` — the pair-construction count on the type level.

**Boundary.** Free theorems hold for *ideally parametric* languages. Scala has `asInstanceOf`,
reflection, and side effects; our model excludes them explicitly, and a signature whose result
depends on such features must read `?`, not a parametric count. Laws (functor, optic) are
*additional* constraints on top of parametricity, never consequences of it — counting
law-abiding implementations is a separate, unsolved mode ([plan §8](../plans/v1.md#_8-boundaries-and-decisions-still-open)).

## D4. Recursion requires productivity

**Decision.** A supplied endomorphism with a seed admits `x`, `step(x)`, `step(step(x))`, … —
countably many (ω); without a seed, the same cycle supplies nothing (0). Recursive type
equations are read as least fixed points. A declaration may not supply its own implementation.

**Evidence.**

- Knvl 2018 (**executable**, D3): `∀a. (a -> a) -> a -> a ~ μx. 1 + x ~ ℵ₀` and
  `∀a. (a -> a) -> a ~ 0` — the seeded/unseeded distinction with the least-fixed-point reading.
- Smyth & Plotkin 1982, *The Category-Theoretic Solution of Recursive Domain Equations* (SIAM
  J. Computing 11(4)) — **bibliography-verified** via Reynolds 1984's reference list: the
  standard machinery giving recursive type equations their least-fixed-point semantics. A
  schematic `X ≅ (X -> Bool)` has no set-theoretic solution with unrestricted functions
  (`|X| = 2^|X|` contradicts Cantor); the least-fixed-point reading in a restricted model is
  what makes "recursive type" a well-posed question at all.
- Russell 1907's road to the vicious-circle principle (D1, SEP **inspected**): no collection may
  be defined in terms of itself. Our analogue is narrow and practical — the signature being
  measured is excluded from its own supplied slots — but it is the same discipline: circular
  "definitions" do not create inhabitants.

**Boundary.** Least-fixed-point semantics justifies *reading* a recursive type; it does not
justify assuming a program terminates. Divergence is excluded by model assumption, not proven
away, and any recursive signature our fragment cannot decide stays `?`.

## D5. Cardinality is not order type

**Decision.** The analyzer's `ω` means countably infinite cardinality (ℵ₀) and nothing else:
no claim about ordering, construction depth, or evaluation cost.

**Evidence.**

- Russell 1907 — **metadata-verified**: the paper's own title distinguishes transfinite
  *numbers* (cardinals) from *order types* (ordinals). Ordinally `ω + 1 > ω`; cardinally
  `ℵ₀ + 1 = ℵ₀`. A count of implementations is a cardinal statement, so our iteration family is
  "countably many", not "at depth n" for any n.

**Boundary.** If we ever report construction depth or ordering — for instance, to bound the
canonical forms counted in an `ω` family — that is a new quantity requiring its own evidence,
rendered separately from the cardinality.

## D6. Approximate counting must be labeled approximate

**Decision.** Estimates from probabilistic counting (HyperLogLog) would live in a separate,
opt-in mode, never mixed into exact or unresolved rows.

**Evidence.**

- Ertl 2017, *New cardinality estimation algorithms for HyperLogLog sketches*
  ([paper](https://oertl.github.io/hyperloglog-sketch-estimation-paper/paper/paper.pdf)) —
  **inspected** (PDF read this session): the estimator's error characterizes uncertainty about
  *the elements supplied to the sketch*, not about inhabitants the generator or workload failed
  to produce. Even Ertl's improved joint estimator for overlapping sets operates on recorded
  elements. Recorded in [plan §10](../plans/v1.md#_10-future-exploration-hyperloglog-measurement)
  as deferred exploration.

**Boundary.** A sketch estimate is evidence about a sample, never about the full inhabitant
set; it cannot turn `?` into a number, prove ω, or prove a count impossible.

## D7. `?` is a first-class answer, not a failure

**Decision.** Unsupported syntax, incomplete scope, or an exhausted analysis budget yields
`Unresolved` with a reason — never a finite guess, never an invented infinity.

**Evidence.**

- The halting problem (Turing 1936; **classical**, not re-verified this session): for any
  Turing-complete language there is no general decision procedure for which programs terminate.
  Totality — a precondition of our pure, total fragment — is therefore undecidable in general,
  which is why the *fragment* is explicitly restricted and why the fragment's boundary is a
  report row rather than an error.
- Luna & Taylor 2010 (D1, **abstract inspected**) and Fan 2020 (**bibliography-verified**):
  the definability literature's repeated conclusion — quantifier scope must be fixed before
  a count means anything — is the same reason our report names the scope it did *not* resolve,
  instead of printing a number over an environment it did not fully read.

**Boundary.** "Undecidable in general" does not mean "undecidable here": specific fragments
(pure, first-order, finite sums and products) are decidable and we count them exactly. `?` is
for the cases outside the fragment, and the reason recorded with it is the work item.

## D8. Resource limits report, never approximate silently

**Decision.** The solver's state and arithmetic budgets exhaust into `Unresolved` with the
budget's name, rather than returning a partial count.

**Evidence.**

- **Executable**: `InhabitationSpec` pins the budget behaviors, and `ReferenceInhabitantsSpec`
  pins the algebra they guard. The design is the report's honesty rule applied to computation
  itself: an incomplete computation is an incomplete answer.

**Boundary.** A budget hit is an obligation to raise the limit or restructure, not evidence
about the signature.

## D9. Ground scalar types are transparent

**Decision.** `Byte`, `Short`, `Char`, `Int`, `Long`, `Float`, `Double` and `String` are closed
enumerations of literals on the method side: a supplied one is split by case analysis into
`|G|` identical branches (one memoised goal raised to `|G|`), a goal of one is its `|G|` literals,
and any value whose type mentions no type parameter splits the same way with exponent its value
space. Counts above 65,536 bits are exact symbolic naturals (`Count.Symbolic`).

**Evidence.**

- Knvl 2018 (D3, the [solver's reference](../counting-type-inhabitants.md)) — **executable** as
  far as `ReferenceInhabitantsSpec` encodes it; its `Int ~ 2^32` and `a -> b ~ |b| ^ |a|` are what
  the split reproduces for closed signatures (`def f(x: Int): Int` = `2^(2^37)`).
- [type-arithmetic.md](../type-arithmetic.md) §2–3 — the repository's own fixed-width sizes and
  extensional function spaces; the stored-value side reads `Int => Int` as the same `2^37`-bit
  capacity.
- Decision-tree definability of every function on a finite domain from literals and equality
  (**classical**), and D2 for the countable `String`: `c^ω` is 0, 1 or ω under the finite-program
  reading, never `2^ℵ₀`.
- JLS §4.2 for the integral and `char` value sets, and the JDK's `Float.equals` /
  `floatToIntBits` for one NaN class and two zeros — **classical**, recalled, not re-inspected
  (docs.oracle.com is outside this session's network allow-list).
- Scala 3.9.0 compiler fixtures (**compiler fixture**): an object or final class must implement
  every inherited abstract member, a class has one member per name and signature (two abstract
  parents' `def g(x: Int): X` are one member), and a member type reached through a parent, a self
  type, an enum's cases, a wildcard-imported object's parents, a package object's parents or an
  export shadows the builtin name, while a primary constructor's parameters do not see the
  template's members (`GroundEnvironmentSpec` reproduces each).
- **Executable**: `GroundTypesSpec`, `GroundEnvironmentSpec`, `NaturalSpec`, `InhabitationSpec`.

**Boundary.** Transparency licenses equality on a ground type, never on an abstract or top type.
A ground *result* of an opaque callable (`A => Int`) is not eliminated: it stays `?`. The split
multiplies exponents, so it is sound only where each closed value is one independent supplied
value: inherited declarations a template implements or restates are dropped or refused (a parent
two enclosing templates both inherit is two members, one per receiver — D10), and a member of an
enum or sealed template, whose receiver is not bound, keeps only a terminal or (inhabited enum) ω
count. A builtin name a channel lookup does not walk may bind to a supplied member is refused,
constructor positions resting on it included. `String`'s ω is an idealization (the JVM bounds its length). See
[ground-scalar-types.md](ground-scalar-types.md).

## D10. The environment holds what a body can reach

**Decision.** A value joins a signature's environment only when the body can reach it the way
Scala binds it: by name, as an inherited member under the substitution its `extends` chain gives,
or — for an anonymous contextual parameter — through an implicit search that provably selects it.
A name means the declaration Scala binds it to — an explicit import before a wildcard, a wildcard
before a package member of another file, an inherited member or an export (which defines one,
D19) before both, for a path's first segment as for a simple name — and a first segment bound to a
value is no object (D19). A family is a sum only when its named summands are all its values.
Anything the reading cannot settle (an overlapping candidate, a parent instantiated twice, a self
type, an anonymous, inner or local summand, a name an unsupplied parent may declare) is `?` with
that reason. Details: [environment readings](environment-readings.md).

**Evidence.**

- **Executable**: `EnvironmentSoundnessSpec` and `NameLookupSoundnessSpec` pin each reading,
  positive and adversarial.
- **Compiler fixtures** (Scala 3.9.0, compiled — and where marked run — this session): anonymous
  evidence of one type declared by one method (an extension's clauses included) or one template
  (its own givens, named imports and exports included) is an ambiguity error; nested evidence beats
  an enclosing one; a block's later given is selected (run); a parent's given loses to the class's
  own evidence (run); an inherited alias beats a package-level one and an import, and is ambiguous
  against an enclosing class's, object's, method's or type parameter's (E049); `package a.b` does
  not open `a`; a covariant diamond gives an intersection member type; enum cases take the enum
  parameters their fields or `extends` clause mention; an earlier import binds a later import's
  prefix; a wildcard import beats a package member of another file (an outer wildcard against an
  inner package member is E049); an inherited member beats an import inside the body; two
  same-shaped classes of two scopes are two overloads (E231 when only one is implemented); evidence
  of two distinct final classes is selected by type (run).
- The Scala Language Specification's binding precedence (ch. 2) and base-type / matching-member
  rules (§5.1) (**classical**, not re-read this session; the fixtures pin the behaviour used).

**Boundary.** Only lexical contextual candidates and supplied parents are read: the implicit scope
of companions matters only when the lexical search fails, which a counted parameter rules out, but
members of a parent outside the source set stay unknown (a name such a parent may declare is
refused where an inherited member would shadow its outer meaning), and same-level ranking
(specificity, derived-over-base, which Scala has changed between versions) is never relied on.

## D11. A name denotes the declaration Scala binds it to

**Decision.** The stored-value estimate, and the constructor-slot reading it lends the method side,
resolve a type name as Scala binds it: types and terms are separate (an object never stands in for a
type), `O.T` is the member of the object or package `O` and nothing else (no last-segment fallback),
a qualifier's first name is the innermost term binding (pattern variables and self aliases
included), bare names follow the precedence local, inherited or exported definition > explicit
import > wildcard import > another file's package member > root import (an export defines members of
its template, D19), an import binds only the statements after it and an export its whole template, a
wildcard over a namespace no source supplies may bind any name the reading file does not define,
package clauses open exactly the packages they name, each file reads with its own clauses, a binder
(an abstract type member too) is innermost, a match type's case body reads where the match type is
written, and a slot is read where its constructor is written — a ground slot it refuses leaves no
number on the method side either. A name whose binding is outside the sources, or a clause the
calculator does not follow, is a reason naming that clause, never a same-named declaration found
elsewhere. Rows that would rest on an uncomputed value say why: `sealed family not summed`, `sealed
child parameter not pinned`, `lazy cycle not completed`, `cross-package cycle not solved`, `type
constructor`. See [stored-value names](stored-value-names.md).

**Evidence.**

- Scala Language Specification, chapter 2, "Identifiers, Names and Scopes" (**classical**, not
  re-verified this session; scala-lang.org was not reachable): separate type and term
  namespaces, qualified identifiers as members of stable paths, package-clause visibility, and
  binding precedence with inner-scope shadowing.
- **Compiler fixture** (Scala 3.9.0, this session): scalac rejects `Foo.B` for a trait `Foo` with
  no companion, rejects `W` under `package a.b` when only `a` defines it, rejects a sealed
  subtype in another file, and accepts the readings the rules give for chained clauses, an inner
  subpackage over an outer object, wildcard imports over another file's package member, a
  package's own `Int` over `scala.Int`, inner terms over outer objects, class members read in
  their class's body, local classes in method bodies, and a lazy knot through a blocked cycle;
  it rejects an outer definition and an inner wildcard that both bind a name as ambiguous, and
  accepts the readings for positional imports, object wildcards, `import x.*` naming a package's
  own object `x`, wildcard exports, package values and pattern variables as first names, self
  aliases, match-type case bodies in their own scope, and abstract type members.
- Bekić's lemma (**classical**): solving an object's members jointly with the enclosing body
  gives the same least fixed point as solving the body and then the object.
- **Executable**: `NamespaceSpec`, `NamespaceScopingSpec`, `ReportSpec`.

**Boundary.** This decides only *which* declaration a name denotes, and refuses where the value
is not computed; it adds no counting rule. It does not read enums as namespaces and does not read
a namespace no source supplies (the standard library's included) — each stays `?` with its
reason. Explicit imports and sealed families are D15's.

## D12. Top types are terminal; a goal with one value has one implementation

**Decision.** On the method side `Any`, `AnyRef` / `Object`, `Matchable` and `AnyVal` are the
terminal type `1`: results of them have one implementation, inputs of them carry no information,
`Any => R ≅ R`. Every universal member — `==`, `!=`, `equals`, `hashCode`, `##`, `eq`, `ne`,
`isInstanceOf`, type patterns, `asInstanceOf`, `getClass`, the default `toString`, `synchronized` —
is outside the model on a value of an abstract, top or top-like type. A declared result with exactly
one value (`Unit`, a top type, a singleton type, products of those, a function into one, a closed
type of value space 1) counts 1 whatever the environment leaves unresolved. The stored-value side
keeps `Any` as everything (ε₀); constructor positions are read through the method-side model.

**Evidence.**

- Mitchell and Plotkin 1988, *Abstract types have existential type* (TOPLAS 10(3)) —
  **classical**: a type with no operations is `∃X. X`.
- Reynolds 1983 / Wadler 1989 (D3) — the free theorem `k[U] ∘ g = k[T]` gives
  `∀X. X → R ≅ R`, hence `∃X. X ≅ ∀R. R → R ≅ 1`; `ReferenceInhabitantsSpec` encodes
  `∀a. a → a ~ 1` (**executable**).
- Observational equivalence, the counting rule of the
  [reference](../counting-type-inhabitants.md) — the contract's own rule, **executable** through
  `ReferenceInhabitantsSpec`: a goal with one value admits one implementation up to observation,
  and an empty context the unique absurd one.
- Scala 3.9.0 compiler fixtures (**compiler fixture**): `-language:strictEquality` rejects `==` on
  `Any` and on an unbounded `A`; under `-source:future` a type pattern on either warns that it is
  not `Matchable`; `(Red | Any) =:= Any` and `(Red & Any) =:= Red` hold; four `Set[Any]` values
  have sizes 0–3; an `inline match` on an `Any` parameter separates `isInt(1)` from `isInt("s")`;
  constructor parameters are typed outside the template's body, parents and companion.
- **Executable**: `TopTypesUnionSpec`.

**Boundary.** Only inside the stated model: an idealized terminal top type is not unrestricted
Scala `Any`, and the exclusions above are what make it terminal. A `transparent inline` result that
names a top type, a function type or a class a subclass may extend stays `?`: the call site
observes the expansion's type (Scala 3.9.0 compiles `mk(true).n` against a declared `Foo` whose
expansion is `new Sub(true)` — **compiler fixture**); so does a top-typed parameter of an `inline`
method, which an `inline match` may dispatch on. A clause that may rebind a builtin name keeps its
reason, and so does a channel name lookup does not walk that reaches declarations the sources do
not supply (a self type, the parent of a wildcard-imported or package object, a wildcard namespace,
the outer clause of a chained package no supplied unit declares into): there every builtin and
model name is refused (D16) and the one-value rule does not fire. `Any*` is not terminal (its
length is observable), and neither is a top inside a constructor that merges equal elements:
`Set[Any]` has a value per size, so a constructor position reading one is refused rather than read
as `Set[Unit]`. See
[top-types-unions.md](top-types-unions.md).

## D13. A union of disjoint, discriminable classes is a coproduct

**Decision.** `A₁ | … | Aₙ` is the tagged sum `A₁ + … + Aₙ` when each alternative has a runtime
class, no class carries two alternatives, and the classes are pairwise provably disjoint (neither
derives from the other, and one is final, both are classes, or one is sealed and its enumerated
subclasses are each disjoint from the other). Exact type equalities (`T | Any = Any`, absorption of
a known subclass, idempotence, `Nothing` / `Null` empty) apply first.

**Evidence.**

- The universal property of the coproduct — **classical**: the class-test `match` is a total,
  well-defined, well-typed copairing, unique for every map out of the union.
- Scala 3's `TypeComparer.provablyDisjoint` (SIP-56) — **classical**, not re-read this session;
  the fixtures pin the behaviour used (**compiler fixture**): final class vs unrelated trait,
  unrelated classes, sealed trait with final leaves vs open trait, object vs `Tuple2` are disjoint;
  non-final class vs open trait, two open traits and type parameters are not; `case _: Box[Unit]`
  after `case _: Box[Boolean]` is unreachable (erased type arguments).
- **Executable**: `TopTypesUnionSpec`, `MethodUnionNullabilitySpec`.

**Boundary.** Type arguments never separate alternatives (runtime tests are by erased class).
Ancestry rests on supplied declarations: a parent that does not resolve refuses the pair unless no
supplied declaration carries its name (a dependency cannot extend the sources depending on it).
A sealed family whose file holds a subclass this reading cannot enumerate — or a parent clause that
names it only in an argument (`extends Id[S]`), which a parameterised alias may dealias to it —
refuses the union even when the other side proves the pair apart, since the sum adds the family's
shape. Union aliases
are read as their alternatives at the use. Builtins are a fixed list of final or
standard-library-sealed classes. A callable that returns a union is still not eliminated.

## D14. A rigid constructor is an uninterpreted base type

**Decision.** An unbounded higher-kinded binder with plain holes is rigid: `F[τ…]` is a base type
over its arguments' nominal keys, equal only to the same head over the same keys, with no
constructor or eliminator. A higher-kinded argument is an operator read where it is written (a
closure, applied by β-reduction in that scope). A context bound is one more contextual parameter;
a subtype bound an ambient `<:<` proof — between binders or rigid applications a directed view, to
a closed structural type an elimination-only view (upper) or a construction (lower). Details:
[higher-kinded binders](higher-kinded-binders.md).

**Evidence.**

- Strong normalisation of System Fω (Girard 1972) and the absence of eliminators for an abstract
  application in a β-normal term (**classical**); completeness of the set-theoretic model over ℕ
  for βη (Friedman 1975, **metadata-verified via secondary sources**) and with coproducts
  (Dougherty & Subrahmanyam 2000, **metadata and abstract verified**) — instantiating each rigid
  head as a constant constructor separates distinct normal forms.
- Relational parametricity for higher kinds (Atkey 2012, **metadata-verified**) and the
  identity-only inhabitation of an equality type (Vytiniotis & Weirich 2010, **metadata and
  abstract verified**): the semantic form of the rule, and the licence for rewriting under `=:=`
  through any `F`.
- Undecidability of F<: subtyping (Pierce 1994, **metadata-verified**) bounds the fragment: a
  finite graph of binder bounds plus closed structural views.
- **Compiler fixtures** (Scala 3.9.0, this session): context bounds are `using` parameters, two of
  one type ambiguous (E172); for `A <: B` with evidence of both, `summon[B]` selects the `B`
  parameter (run); every positive fixture of the spec compiles; `F[?]` for an abstract `F` is
  E043; `[A >: Flag <: Boolean]`, `[A >: Flag <: AnyVal]` compile and every call is E057; bounds
  referring back through bounds alone are E140, F-bounds compile; view bounds and constructor
  arguments of the wrong kind are rejected.
- **Executable**: `HigherKindedBindingSpec`, `HigherKindedRefusalSpec`, `BinderBoundsSpec`,
  `KindedIdentitySpec` (positive and adversarial: nominal keys separate `Option[A]` from
  `Either[Unit, A]`, hygiene, closure scope, views of views, bounds, refusals).

**Boundary.** Nothing structural is assumed of a rigid `F` — no law, no variance (a variant hole is
refused), no bound (a bounded higher-kinded binder is refused). A key never identifies two types
by their shapes: a structurally substituted argument, a union of distinct keys, a path, an
abstract member or an unsupplied name has no key and stays `?`. A subtype proof beside a callable
or a sum value is refused (contravariant uses are not read); a callable producing a structurally
bounded value is refused (its output would be eliminated). Structural lower and upper bounds
that share a supertype are refused: a viewed value and a lower-bound construction may be one value,
and the bounds may admit no type (a vacuous context); so is a structural lower bound under
`AnyRef` / `AnyVal` / `Matchable` whose declared class does not fit. A bound whose structure holds
its binder again (an F-bound through a structure, directly, via bounds or via a proof) is refused:
its inhabitation is a fixpoint question; bounds referring back through bounds alone, and view
bounds, are invalid programs, refused whether or not the binder is read and whatever the result
(D12's one-valued goal presumes a program). A top or `Nothing` bound is a no-op only past every
channel that may rebind the name (D12). An opaque type outside its
scope is no key (its API may relate it to other types). Bounds to open traits wait for the
capability records.

## D15. Sealed families are tagged sums; a number rests on everything it reads

**Decision.** In the stored-value estimate (and the constructor-slot reading it lends the method
side):

- A sealed trait or abstract class is the sum of the direct subtypes its file declares in its body
  and nested objects: each case or final class, enum, object or sealed intermediate a member of
  every instantiation. Its parameters bind the parent's slots exactly once, or slots are pinned
  bottom-up with the standard `Nothing`/`Any` where the clause is written; its bounds are the
  slot's, read alike (beside the parent); it varies as the slot does, holding a parameter through
  a covariant slot only by a constructor its name denotes or a standard one no source binds.
- Otherwise the family is a refusal naming why: an anonymous or given instance; an open, inner,
  aliased or renamed subtype; a subtype whose parent name a scope may rebind; a GADT pin; an
  unpinned, bounded or wrongly varying parameter (the reason names the subtype and the parameter);
  an overlap. The parent's name is read where each clause is written, inherited members and
  exports included.
- An instantiation that recurses on its own parameters through positive positions is its own
  least fixed point. A cycle through a function's domain, a constructor no reading knows (a
  constructor's slots are its declaration's, wherever its name is written) or a type member of a
  value has no count.
- A match type in a subtype reduces on the parent's arguments as written, matching a pattern by
  spelling only where its names mean one declaration, and passing a case only when provably
  disjoint from it.
- Explicit imports and exports are followed into the supplied sources, an export outranking an
  import and reading its path with the imports before it; a selector binds the namespaces its
  member has, a body's own definition outranking it in that namespace only.
- A row carries the non-parameter reasons of every definition it reads, transitively. Context
  bounds are stored evidence. A finite collection is at least its element's tier. An enclosing
  template's abstract type member and a match type stuck only on the reading's free parameters are
  instantiation-dependent. A lossy width survives `+` and `×`. Reason classes rank unresolved >
  open > instantiation, all rendered.

See [stored-value families](stored-value-families.md).

**Evidence.**

- [30] Scala 3 draft specification, ch. 5, and [33] SLS 2.13, ch. 5 (**Inspected (full text) —
  bibliographer**, bibliography T5): `sealed` and `final` inheritance. [24] Scala 3 reference,
  *Translation of Enums and ADTs* (**Inspected (full text) — bibliographer**, T5): enums as sealed
  abstract classes, cases as final case classes, explicit `extends` pins. [22] Scala 3 reference,
  *Open Classes* (**Inspected (full text)** (verifier), T5): a sealed class is extended only in its
  source file. [29] Scala 3 draft specification, ch. 3 (**Inspected — bibliographer**, T5): a
  sealed class is disjoint only "without anonymous child".
- [15] Scala 3 reference, *Context Bounds* (**Inspected (full text) — bibliographer**, T2): a
  context bound is a `using` parameter of the bound applied to the type parameter.
- Sum of products and the least fixed point for recursive types (type-arithmetic §2, §8;
  **classical**); `|List[A]| = Σₙ aⁿ` ≥ `a` (type-arithmetic §4; **classical**); no set X ≅ 2^X
  (Cantor; **classical**), so a negative recursion has no count.
- Scala 3 reference, *Match Types*, and the Scala 3 draft specification, ch. 3: a case is passed
  only when provably disjoint (**Inspected (full text) — bibliographer**, bibliography T4).
- **Compiler fixture** (Scala 3.9.0, this session): every fixture of `StoredValueSoundnessSpec`
  compiles; scalac accepts a subtype and an anonymous instance through an alias of a sealed
  parent and a bounded subtype parameter, rejects an inner import or wildcard rebinding an outer
  family's name in the family's file as ambiguous, and rejects an enum case relying on the enum's
  context bound (cases do not inherit it); it types a subtype under an object that inherits or
  exports the parent's name as that other type, accepts `C[Any]` in a contravariant slot of
  `P[Boolean]`, rejects a GADT enum case and a shadowed `Nothing` pin as members of other
  instantiations, leaves `H[Any]` unreduced past an undecided case, and binds an exported name
  over an imported one; it rejects a subtype's bound read in an object that rebinds its names
  (E057), accepts a subtype through a renaming import, distinguishes closed values of a negative
  recursion through a source `List` or a value's type member, reduces a spelled `case Int` only
  for the same `Int`, and scopes renamed, term-only and body-shadowed selectors as the page says.
- **Executable**: `StoredValueSoundnessSpec`, `NamespaceSpec`, `NamespaceScopingSpec`,
  `ReportSpec`, `CounterSpec`, `SizeSpec`.

**Boundary.** A model-level rule over constructor inputs; it does not decide `|Any|` or
`|Double|` (D12, D9) and leaves the method side's own collection and family readings to it.
Mutual recursion between generic families, a match type over another match type's result,
absorption by an exact 0, an instantiation's recursion through `&` that settles late, negative
recursion, recursion through an unread constructor or a value's type member, and a match case only
subtyping would match stay `?`, as the page's limits state.

## D16. Library declarations are read from a versioned model; absent ones are reported

**Decision.** Declarations the sources do not supply are read only through a small table keyed by
fully qualified name (`stdlib-model/1`, Scala 3.9.0 on 2.13 collections): tuples, `Some`, `Left`,
`Right`, `EmptyTuple`, `NamedTuple` as products; `Option`, `Either` as sums; `FunctionN` as
arrows; `List`, `Vector`, `IndexedSeq`, `Seq`, `ArraySeq` and `Array` of a class as finite
sequences `Σₙ Eⁿ`; single-pass, equality-built, reflective and type-revealing (`ClassTag`,
`TypeTest`, `Mirror`) types excluded; classes and Java
interfaces whose members it does not hold (open ones, and the final `Integer`) refused. The table
answers after every binding of the sources, under the root imports' spellings only; a wildcard of an
unsupplied namespace refuses every builtin and model name, and a wildcard of a listed namespace
binds the names it lists ahead of another file's package member. A name nobody declares is
`declaration not supplied: <fqn> (<binding>)`. An open owner's foreign parent is reported, not
ignored.

**Evidence.**

- Scala 3 draft specification, ch. 2, `docs/_spec/02-identifiers-names-and-scopes.md` (main) —
  **inspected (full text)** by the literature verifier (a draft, not versioned for 3.9.0): the
  root imports "are taken as lowest precedence, so that they are always shadowed by user code"; a
  wildcard import outranks them.
- Odersky 2024, SIP-58, and the reference page *Named Tuples* — **inspected (full text)** by the
  verifier: "`NamedTuple` is an opaque type alias of its second, value parameter", with
  `toTuple`.
- Knvl 2018 (D3, D4) — **executable**: sums, products and the list fixed point; Smyth & Plotkin
  1982 (D4) — **bibliography-verified**: least fixed points.
- Scala 3.9.0 compiler fixtures — **compiler fixture**: the 93 root spellings the analyzer reads
  mean the library's under `scala.quoted.*`, `quotes.reflect.*`, `scala.jdk.CollectionConverters.*`
  and inside an `IterableOnce` subclass; `NoSuchElementException` is `java.util`'s;
  `new Array[A](0)`, `Array.empty[x.type]` and `Array(x)` for `x: A` (or a member type `h.T`) need a
  `ClassTag` nobody supplies (E172), while an array of every non-rigid element builds — `new
  Array[E](0)` for `Nothing`, `Null`, `Box[A]`, `List[A]`, an opaque type, an object's singleton and
  `x.type` for `x: String` or `x: Box[A]`, `Array.empty[F[A]]` and `Array(x)` for `x: F[A]` (`new
  Array[F[A]](0)` itself crashes the 3.9.0 back end, so it is not the fixture); a listed namespace
  reached through a rebound head (`import lib.quotes; import quotes.reflect.*`) is `lib`'s; under
  `import scala.quoted.*` a package member `Expr` of another file is not what `Expr` names (E008 on
  its field). The scala-library 2.13 class file of
  `IterableOnce` (**inspected**, `javap`): `iterator` abstract, `knownSize` and `stepper` default.
- **Executable**: `StdlibModelSpec`.

**Boundary.** The table describes one library version and must be refused for others. Readings hold
under the contract's exclusions (identity, mutation, runtime equality): an `Array` is its contents,
a `Map` or `Set` is not counted at all. An unapplied entry is an operator of the higher-kinded
reading (D14) and is read only where applied; every entry the model reads is a nominal key there,
one per declaration whatever its spelling. An `Array` of a type parameter, or of a singleton of one,
is refused even as an input (the reading has no polarity). The table is consulted only where the
root imports bind the name — a model parent or a listed namespace under a rebound head or an
unsupplied wildcard is not the model's — and under one **assumption**: no package outside the
sources is named `scala` or `java` inside a source package (a supplied one refuses). Dependency
declarations are not read; absence stays `?`. See [stdlib-model.md](stdlib-model.md).

## D17. A supplied sequence is eliminated by length

**Decision.** With `c(n̄)` the count when each of at most three opaque-element sequences is
replaced by `nᵢ` fresh elements: `c(0̄) = 0` gives 0; a one-valued goal 1; `c(0̄) ≥ 1` and
`c(k̄) ≥ 2` for some nonzero `k̄` with every `kᵢ ≤ 2` gives ω; anything else is `?`. A sequence the
rule cannot reach (in a supplied sum, a callable's result, a built function's parameter) is `?`.

**Evidence.**

- [type-arithmetic.md](../type-arithmetic.md) §4, §7 — `List[A] ≅ Σₙ Aⁿ`, and a function out of a
  sum is a product of functions (shapes are lengths): an implementation is a family indexed by
  length, chosen by a total length test.
- D2 (**classical**): at most ℵ₀ finite programs, so infinitely many distinct families are ω.
- Weakening (an inhabitant ignores a new element) carries existence to every length; distinctness
  needs a nonzero witness `kᵢ ≥ 1`, whose valuation extends by repeating an element; an absurd
  element merging the answers is the analyzer's own derivation (`varAbsurd`).
- Related work, **metadata-verified**: Scherer & Rémy 2015 (ICFP) report that the simply typed
  λ-calculus with sums has 0, 1 or ≥ 2 inhabitants under βη, decided by saturating focused search;
  Scherer 2017 (POPL) reports that βη-equivalence with sums and the empty type is decidable and
  coincides with contextual equivalence.
- **Executable**: `SequenceEliminationSpec` (`varAbsurd` is never ω;
  `varBranch` is ω), `RepeatedParameterFragmentSpec`.

**Boundary.** Lengths up to 2 decide only ω or 0; a count that stays finite and above 1 (the true 2
of `varAbsurd`) is `?`. Premise (1) needs the empty sequence to be an admissible input: the fields
of a receiver whose class has a non-public constructor are refused, since its factories may never
build one (`NonEmpty.head`, true ω). Infinite `Seq` inputs are covered by continuity (a total
program reads a finite prefix), not by a separate rule. See [stdlib-model.md](stdlib-model.md).

## D18. An abstract member of a stable path is a rigid atom

**Decision.** A type member is a member of a stable path (`this`, a parameter, a declared `val`, a
singleton alias of one). Along the path's linearization the first alias is its meaning, read where
it is declared with `this` still the path — so a parent's declaration reads its receiver's members
(`Optic.to` in `PickFold` is `S => Affine[(Unit, Unit), A]`). A member every level leaves abstract
and unbounded is the atom `member:<path>#N`, counted as a class type parameter is; a type-only
trait or class has one value; a single-case tuple projection on a rigid atom is the atom
`proj:n:i:(α)` when one arity per scrutinee lets one instantiation reduce them all; an irrefutable
first case reduces. A final root's inherited declaration another level implements is that level's
code (members gate G1). Details: [type members](type-members.md).

**Evidence.**

- Mitchell & Plotkin 1988 (C-10, **metadata-verified**: the abstract reports abstract data type
  declarations typed as existential types); reading a parameter path as the opening of that
  package, whose witness the caller picks, is this register's derivation. Cardelli & Leroy 1990
  (**metadata-verified**, background only): dot notation.
- Scala 3 draft specification ch. 3 ([29], **inspected**): abstract types have bounds, and the
  lower-bound rule breaks transitivity, so only unbounded members are free atoms; Hu & Lhoták 2020
  (**metadata-verified**: the abstract reports undecidable `D<:` subtyping and decidable
  fragments without bad bounds) is why bounded members stay `?`.
- Match types: Scala 3 reference *Match Types* ([18], **inspected**), SIP-56 ([1], **inspected**)
  and [29]: reduction only past provably disjoint cases, otherwise stuck, "just an abstract type";
  Blanvillain et al. 2022 (**metadata-verified**: the abstract reports match types formalized in
  a calculus based on F<: and proved sound).
- **Compiler fixtures** (Scala 3.9.0): two paths do not share a member, `q: p.type` does; a final
  class may leave a member undefined; an inherited alias beats a package alias inside the body;
  `Fst[T]` does not type from a `T` nor as `Snd[T]`, `First[T]` is `Fst[T]`, `case _` reduces on
  an abstract scrutinee (`TypeMemberSpec`, `memberfixture`; members design session `c01`–`c08`);
  `Boolean { type X }` and a final class refined against its own alias have no value, an alias
  cannot be overridden by another, a value class or a private constructor admits no outside
  subclass (`TypeMemberBoundarySpec.refinementFacts`); an inherited `O.this.X` stays the outer
  instance's in a receiver extending `O`, a parameter hides a self alias, a parent's private alias
  is not the receiver's, a class member cannot be overridden (B2 review fixtures, compiled with
  scalac 3.9.0); inside a refinement `this` and a member the refinement or its base declares are
  the refined value's, a subclass may narrow a field an alias reads, `=:=` rewrites a stuck
  projection (`substituteCo[Fst]`), and a `final abstract` class, a member with two bounds, a
  private constructor below a trait and a private template's scope admit no instance or no outside
  subclass (`TypeMemberScopeSpec.facts`).
- **Executable**: `TypeMemberSpec` (parity with a type parameter, distinct paths, singleton and
  self aliases, refinements, receivers, projections and their arity refusal, G1/G2),
  `TypeMemberBoundarySpec` (outer paths, shadowing, class members, unsubclassable templates,
  private aliases, unsatisfiable refinements, X6), `TypeMemberScopeSpec` (refinement bodies, a
  field read for its receiver, closed scopes, bounded templates, evidence on stuck projections),
  `MethodMatchProjectionSpec`, `MethodRefinementSpec`.

**Boundary.** Only a member someone may pick is an atom: a final class (abstract or not, a value
class too), object or given never instantiates it; a private constructor (the template's own or an
ancestor class's), a private or local template's scope, and a sealed family whose cases are all
closed keep every subclass where none is enumerated (inside the family the receiver gate answers;
an open case lets any file pick); a `val` fixed by its definition (local, private or final, or of
a closed template) is no choice; and an unsupplied parent may define it — each refused with its
reason; a member some level declares as a class is no atom. A field an alias reads is the
receiver's field, at its most derived declaration. Inside a refinement `this` and the members it or
its base declares are the refined value's; a projection `R#X` whose body reads either is refused.
A stable input's type-only refinement is read as its base only where some value of the base may
have its members (open supplied template, nothing it pins already fixed or bounded, bounds exact
or one-sided and uncontradicted); otherwise the input may be empty and is refused. Bounded and higher-kinded members, `R#X`
projections of abstract members, structural refinements with term members, nested or bounded
stuck projections, projections on nominal, opaque or bounded scrutinees and projections whose
scrutinee type evidence mentions stay `?`. Opaque types outside their scope are not atoms. The
empty record covers only templates with no parent, parameter, constructor modifier or self type (a
constructor argument must exist for an instance to exist), that are not `final abstract` and whose
members have at most one bound each (Nothing meets an upper bound, Any a lower one).

## D19. Concrete code nothing can override adds no implementation

**Decision.** Over readable inputs and goal, a closed helper the target can call — a module or
package member, a local definition, a given, an export forwarder, a member of a final template, a
concrete `var` — denotes one of the parametric inhabitants the solver already counts, so it is
neither bound nor reported; the `qualified member environment not resolved` diagnostic is gone.
This rests on the contract's assumption that helper bodies obey the model (no effects, casts,
runtime type tests, `null`). Overridable concrete members of open classes are capabilities, not
fixed code (cross-check X5; the environment memo's "closed-world dispatch" is superseded; the
capability records classify them, D20, so `ow.Box.target` reads 3 and the final `FBox.target` 2). The
model excludes mutation, so a `var` reads as the value it was given. An export defines members of
its template (they rank with its own declarations, an object's inherited members included), a
path whose first segment is a value is no object, a protected inherited member is not selected
from outside, a `given` selector binds no type name, an unimport leaves its name out (under a
foreign wildcard too), `=> A` reads as `A`. A local capture another binding of an inner scope
makes ambiguous (a definition — every variable of a pattern definition included —, extension
method, export, import or inherited member) is out of reach. An inferred result is typed only where
syntax fixes it: the overridden declaration (a method, an abstract `var`'s getter or setter, a
given), matched by name, clause shape and parameter types and read with the subclass as `this`,
when no base class outside the sources (`Any`, `Object`, `Product`, `Equals`, an enum's, a stdlib
or unsupplied parent) may declare the name, since Scala intersects every one; a literal; an
ascription to no singleton (written or aliased); explicit construction; a call of the one declared
method the nearest binding of the name reaches, in Scala's precedence, with explicit type
arguments, its declared result widened as Scala widens an inferred one (a literal type to its
class, an object's singleton kept, any other singleton refused). Details:
[environment completeness](environment-completeness.md).

**Evidence.**

- Reynolds 1983 ([9], **bibliography-verified**) and Mitchell & Plotkin 1988 (C-10,
  **classical**): a closed term's denotation is parametric and its internals are unobservable
  through readable types. Pitts & Stark 1998 (**metadata-verified**: the abstract reports that
  local state must be handled for representation independence) is why mutation must be excluded,
  not the `var`-as-value reading itself, which is the contract's.
- Scala 3 reference, *Export clauses* (**inspected**, extraction — cross-check): export clauses
  define aliases for selected members; Scala 3 draft specification ch. 2 ([28], **inspected**):
  the precedence list, read with exports among the definitions.
- **Compiler fixtures** (Scala 3.9.0, this session): no member overrides a `var` (E164); an
  overriding member without a declared result is typed by the overridden one (`Some(1)` overriding
  `Option[Int]` is no `Some[Int]`), but not across differing parameter types or a private parent
  member, and by `Any`'s, `Object`'s, `Product`'s or `Equals`' member where those declare it
  (`clone()` is an `Object`, a case class's `productPrefix` a `String`), and by an abstract `var`'s
  getter or setter or an abstract given it implements (`Any`, `Unit`); a singleton ascription is
  widened through an alias, and so is a call's singleton result (`1` and `1 | 2` to `Int`,
  `this.type` to the class, `E.A.type` to `E`; `Int | String` is kept); a pattern definition's
  variables are members that hide an enclosing parameter, and a pattern-bound function value is
  what a call of its name applies; a call resolves to an inherited overload or to an explicit import over
  another file's package member; `import p.{given T}` and `import p.given` bind no type; an
  unimport and a rename leave the standard `Option`; an export beats an outer definition and an
  import of its body, a rename exports only the new name, an export of an inherited member is
  read; a protected inherited member is not importable; an inherited export beats another file's
  package member and is ambiguous against an enclosing definition (E049), as an inner import or an
  inherited member is against a local capture; a declaration and an export of one name is E161.
- **Executable**: `EnvironmentCompletenessSpec`, `InferredResultsSpec`;
  `MethodAnalysisSpec.concreteProducers`,
  `EnvironmentSoundnessSpec.varParameter`, `GroundEnvironmentSpec` (exports and inherited members
  through imports).

**Boundary.** The premise is readability: opaque types outside their scope, classes with
non-public constructors, opaque callables with transparent results and names outside the sources
keep their own reasons, which is where a concrete producer could matter. Exported or imported
terms that may hide a local capture, the names an ancestor's exports define, a foreign wildcard
export, a
package-level export of another file, a path through a value, an override a base class outside
the sources may intersect, a call an import, export or inherited member may bind, and inference
that matches argument types stay `?` with their reasons.

## D20. A value of a source class is the record of its observations

**Decision.** A value is observed only through its accessible members, the universal ones (runtime
equality and reflection: `Any`'s `toString`, `hashCode`, `equals(Any)`) excluded. A member an
instance may replace — abstract, or concrete and not effectively final, a parent's constructor `val`
included — is a free capability of its type; a member none may replace (`final`, `private`,
concrete `inline`, a given or member object, any member of a final class, object or given) is
concrete code over the receiver's observable state and adds no choice. So an open trait, or an open class whose constructor
parameters are all public `val`s, is the product of its open members and fields, as input and as
goal; a final class with a public constructor whose every parameter is a public `val` or exposed by
an η-alias member is the product of its parameters; a `final abstract` class is the empty sum. On
the measured declaration's own receiver (and on enclosing ones) the same classification holds for
its own and inherited concrete members (a member is overridden by the levels before it in the
receiver's linearization). A member whose default evaluates the declaration whenever it is
evaluated (the one member its arguments select, every value clause applied, at a position the
compiler wraps in nothing that defers it: no extension, conversion or `==` overload taking it by
name, no context-function parameter, no implicit search) is a view: dropped when definite, a reason
when it depends on arguments, on a by-name operand, on a nested template's body, on `super`, on an
η-expansion or on such an insertion. An inherited member is the
measured declaration when parameters match as seen from the receiver, through every level, keyed by
the declarations the scope-correct lookup binds; results are not compared. Details: [capability
records](capability-records.md).

**Evidence.**

- Scala 3 reference *Open Classes* ([22], **inspected**): traits and abstract classes are always
  open and any non-final class may be extended; Scala 3 draft specification ch. 5 and SLS 2.13
  ch. 5 ([30], [33], **inspected**): a final member may not be overridden, members of final classes
  or objects are implicitly final, a concrete definition overrides an abstract one; *Inline* ([17],
  **inspected**): inline methods are effectively final; *The Matchable Trait* ([19],
  **inspected**): the universal members excluded. Member matching by parameters (SLS §5.1.3) is
  **classical**.
- Cook 2009 (**metadata-verified**: the abstract reports abstract data types and objects as two
  distinct forms of data abstraction); Cook & Palsberg 1989 (**metadata-verified**: the abstract
  reports self-reference as a fixed point, so late binding leaves overridable members open); Pierce
  & Turner 1994, Bruce, Cardelli & Pierce 1999 (**metadata-verified**, background only). The
  record reading, the coincidence condition and the view rule are this register's derivations.
- Environment readings' §3.0(1) "closed-world dispatch" is **superseded** for overridable members
  (crosscheck X5): an open class's concrete member is a capability (`ow`: `Box.target` 3, `FBox`
  2).
- **Compiler fixtures** (Scala 3.9.0, B3 probes): every `CapabilitySpec` and `TargetViewsSpec`
  fixture compiles; a class leaving an inherited abstract member undefined must be abstract; a
  given or member object cannot be overridden; a parent's `val` parameter can (a warning), and the
  override is what the parent's code reads (run); a by-name left operand of a right-associative
  operator is not evaluated (run); review round 2 (Scala 3.9.0 runs): a diamond `R extends C with
  B` reads B's final `m` though a breadth-first walk meets A first; a by-name extension receiver,
  `==` overload or conversion, a context-function parameter and `summon[T]` of `given self: T =
  this` each behave as the view rule says; at landing (Scala 3.9.0 runs, `probes/b3land/fix`): an
  extension with a by-name receiver imported by the file, the template or a block, or exported by
  the template, never runs its receiver.
- **Executable**: `CapabilitySpec` (open siblings, fixed siblings, definite and possible views,
  open implementations, outer receivers, Unit members, the reviewed views; multi-level, imported,
  covariant and arity identity; trait inputs and goals, restated members, final records, the
  composed-getter laws; hidden members, unexposed fields, X6, `final abstract`, existentials, self
  views; file-closed templates, givens, universal members, overloads, parent fields and unpassed
  parent state; linearization diamonds, refined restatements, body `var`s, file-closed mixins,
  tentative keys), `TargetViewsSpec` (arity, η-expansion, operators, `using` clauses, local
  bindings, `Outer.this`, enclosing and nested templates; every definite position; by-name
  `==`, extensions and conversions, context functions, SAM lambdas, implicit search, `super`,
  imported or exported extensions, block imports),
  `MethodAnalysisSpec.openMembers`,
  `OverrideIdentityImplementationSpec`,
  `ContextualOverrideIdentitySpec`, `GroundEnvironmentSpec.openImplemented`.

**Boundary.** Records with abstract type members, self types, trait parameters, `var`, extension
or pattern-defined members, members hidden at the use site, hidden state of an open class or of a
parameterized parent no class passes arguments to (no instance may exist: X6), unexposed or
non-injectively exposed final fields, non-public or scoped constructors, recursive records (the
type reading's structural-proof guard) and polymorphic open members are refused with their reasons; case
classes, sealed families and enums keep their own readings. A possible view, a record member that
may be the declaration on its receiver, a view through code the compiler inserts, and an
override identity resting on foreign or opaque declarations stay `?`. The view rule assumes no
unsupplied *named* import brings a by-name implicit conversion and no unsupplied class overloads
`==` by name; every other unsupplied channel (wildcard import, parent, self type, an import or
export of the selected name, a block's import) refuses. Open: a by-name conversion or extension in
the companion of a type outside the sources is not seen (a definite view read where none exists:
fixture `fc`, 4 for true 16). Only a member that builds Unit is dropped regardless of unreadable
parameters; any other unreadable member type fails the environment eagerly.

## D21. Named tuples are products; a literal type has one value

**Decision.** `(x: A, y: B)` reads as `A × B` (a match type is stuck on it; names are part of a
rigid argument's key). A literal type is a singleton without a widened binding: one value as a
goal, nothing as an input; a union of literal types is the set of its distinct constants; both
are closed spaces (one point, n points), so callables over them are counted as over `Unit` or a
sum; a key is the literal's class and constant. A call's result that its type argument makes a
literal type is widened to the literal's class. A floating zero is refused (the parser drops its
sign).

**Evidence.**

- Odersky 2024, SIP-58, and the reference page *Named Tuples* — **inspected (full text)** by the
  verifier: "`NamedTuple` is an opaque type alias of its second, value parameter"; "The order of
  names in a named tuple matters."
- Scala 3 draft specification, ch. 3 ([29]) — **inspected (full text)**: "A literal type `lit`
  denotes the single literal value `lit`." SIP-23 ([3]) — **inspected (full text)**: literals
  designate "the corresponding singleton type".
- **Compiler fixtures** (Scala 3.9.0): a match on `(x: Int, y: String)` reduces neither `case (a,
  b)` nor `case Int` and proves neither disjoint (`m1`, `m4`); `def nul: "a" = null` is rejected
  (`m3`); `case 1` matches `1L` and `case 97` matches `'a'` (cooperative equality, `eq`), so
  constants of different classes are told apart by a class test; `idc[1](1)` of `def idc[T](t:
  T): T` types an inferred result as `Int` and `opt[1](1)` as `Option[1]` (`review-A5 r2 inf3`).
- **Executable**: `SyntaxFeaturesSpec`.

**Boundary.** Dependent function types and named-tuple patterns are not read; `1 | Int` is
refused; a call instantiated with a value's singleton or a union of literals is refused. See
[syntax-features.md](syntax-features.md).

## D22. A context function is a function whose binders are givens around the body

**Decision.** `T ?=> R` reads as `T => R`. The binders of the context-function chain a
definition's result starts with — followed through aliases and their arguments, applied lambdas,
members as seen from the receiver and an overridden declaration's result — are contextual
candidates nested inside it, each `?=>` one level deeper, which the anonymous-evidence rule (D10)
checks: for the target they are its result's arguments; for an enclosing definition they are
anonymous evidence of everything nested in its body, refused where a deeper candidate may shadow
them and ambiguous within one clause. A candidate of type `A ?=> R` also answers searches for
`R` (and a search for `A ?=> R` finds a plain `R`), so the rule compares every type a search may
select a candidate for. A call or ascription yielding a context function is not an inferred
result (Scala applies it).

**Evidence.**

- Scala 3 reference, *Context Functions* and *More Details* ([16]) — **inspected (full text)**:
  context function types "erase to normal function types"; the literal's parameters "are available
  as givens in E". No source states `?=>` ≅ `=>` as a theorem; the reading rests on these.
- **Compiler fixtures** (Scala 3.9.0, cross-check `cf`, `cf2`; design `cf3`): `g(using 1)(using 2)`
  prints 2 for a block body, a helper call, a local definition, an alias and an override's
  inferred result; a value read before a nested wrapper (`hoisted`) prints the outer 1. Review
  fixtures (a5-fix1 `r1`–`r8`, `w1`–`w4`; review-A5 `o1`–`o3`, `i3`): a deeper binder, a nested
  `using`, a body's given and a nested context-function result each win over an enclosing binder;
  `(A, A) ?=> A` is ambiguous inside; unions, intersections and singletons are not wrapped,
  projections, match types and refinements declaring `apply` are; an ascription to a context
  function is applied. Review fixtures `review-A5 r2` (`cf4`, `cf6`, `amb2`–`amb5`): `using A ?=>
  B` and `using B` make `summon[B]` and `summon[A ?=> B]` ambiguous; a binder `k: A ?=> B` wins
  `summon[B]` over an outer `B`.
- Related work: Odersky et al. 2018, *Simplicitly* (PACMPL 2, POPL) — **metadata-verified**.
- **Executable**: `SyntaxFeaturesSpec`, `ContextBindersSpec`.

**Boundary.** `def g[A](using A): A ?=> A` reads `?` (true 1): the overlapping outer evidence is
refused, not dropped, and so is a shadowed enclosing binder; likewise `a4[A, B](using A ?=>
B)(using B)(a: A): B` (true 1) and `b1[A, B](using A)(using B): (A ?=> B) ?=> B` (true 1). A chain the reading cannot follow —
match type, projection, path, refinement of a context function, applied inherited alias, a
parameter an `extends` clause substituted with a function, an unsupplied or undeclared name —
refuses nested definitions with its reason. See [syntax-features.md](syntax-features.md).

## D23. Compile-time and type-revealing evidence is not parametric

**Decision.** An `inline` method, and every method defined inside one, is refused unless its goal
has one value (D12): an expansion may inspect its call site's argument types, constants, type
arguments and givens. `ClassTag`, `Manifest`, `TypeTest`, `Typeable` and every `Mirror` are
excluded from the stdlib model as type-revealing evidence, as `scala.quoted` is as compile-time
reflection.

**Evidence.**

- Scala 3 reference, *Inline*, *Compile-time operations*, *Macros* ([17]) — **inspected (full
  text)**: "The scrutinee `x` is examined statically and the inline match is reduced
  accordingly"; `erasedValue`, `summonFrom`, quoted type patterns.
- Scala 2.13 library, `ClassTag` ([11]) — **inspected (full text)**: "can serve as an extractor
  that matches only objects of type `T`"; *TypeTest* ([25]) and `scala.deriving.Mirror` ([14]) —
  **inspected**.
- **Compiler fixture** (Scala 3.9.0, `il`): `byVal(1)` is 10 and `byVal(a)` 20 with `a == 1`;
  `pick[Int]` and `pick[String]` choose different arguments; `summonInline` applies a call-site
  given. Fixture a5-fix1 `r3`: a local definition's `summonFrom` prints 1, and 7 under a call-site
  given.
- Related work, **metadata-verified**: Washburn & Weirich 2005 (LICS; the abstract reports that
  run-time type analysis breaks standard parametricity); Neis, Dreyer & Rossberg 2009 (ICFP; the
  abstract states that "type abstraction and intensional type analysis are features seemingly at
  odds"); Stucki, Brachthäuser & Odersky 2021 (GPCE), background.
- **Executable**: `SyntaxFeaturesSpec`, `TopTypesUnionSpec.inlineTop`.

**Boundary.** The refusal does not read the body, so it also covers inline methods that inspect
nothing. `ValueOf` is not excluded (it supplies a value). See
[syntax-features.md](syntax-features.md).

## How to extend this register

A new source earns an entry only with: the decision it defends, the exact claim relied on, its
verification level as defined above, and the boundary past which it must not be cited. A new
*decision* earns an entry only when it can name at least one source or one executable artifact
in this repository as evidence. Assertions that cannot meet either bar do not belong in the
register; they belong in the [plan's open-decisions list](../plans/v1.md#_8-boundaries-and-decisions-still-open).
