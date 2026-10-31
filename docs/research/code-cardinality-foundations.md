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
before a package member of another file, an inherited member before both, for a path's first
segment as for a simple name. A family is a sum only when its named summands are all its values.
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

**Decision.** The stored-value estimate, and the constructor-slot reading it lends the method
side, resolve a type name as Scala binds it: types and terms are separate (an object never stands
in for a type), `O.T` is the member of the object or package `O` and nothing else (no
last-segment fallback), a qualifier's first name is the innermost term binding (pattern
variables and self aliases included), bare names follow the precedence local definition >
explicit import or export > wildcard import > another file's package member > root import, an
import binds only the statements after it and an export its whole template, a wildcard over a
namespace no source supplies may bind any name the reading file does not define, package clauses
open exactly the packages they name, each file reads with its own clauses, a binder (an abstract
type member too) is innermost, a match type's case body reads where the match type is written,
and a slot is read where its constructor is written — a ground slot it refuses leaves no number
on the method side either. A name whose binding is outside the sources, or a clause the
calculator does not follow, is a reason naming that clause, never a same-named declaration found
elsewhere. Rows that would rest on an uncomputed value say why: `sealed family not summed`,
`sealed child parameter not pinned`, `lazy cycle not completed`, `cross-package cycle not
solved`, `type constructor`. See [stored-value names](stored-value-names.md).

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
is not computed; it adds no counting rule. It does not follow explicit imports or exports, does
not read enums as namespaces, does not read a namespace no source supplies (the standard
library's included), and does not sum generic sealed families or families with a subtype outside
the body — each stays `?` with its reason.

## How to extend this register

A new source earns an entry only with: the decision it defends, the exact claim relied on, its
verification level as defined above, and the boundary past which it must not be cited. A new
*decision* earns an entry only when it can name at least one source or one executable artifact
in this repository as evidence. Assertions that cannot meet either bar do not belong in the
register; they belong in the [plan's open-decisions list](../plans/v1.md#_8-boundaries-and-decisions-still-open).
