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
  of two distinct final classes is selected by type (run), and so is evidence of two unrelated
  traits (`probes/b4fix2/overlap/o3`, compiled).
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
number on the method side either. The method side's own lookup reads wildcards the same way:
a name, or a path's first name, that a wildcard over an unsupplied namespace may bind is refused
wherever it would otherwise be another file's package member, an outer scope's wildcard member or
a root package (`unresolved type: Fst (may be imported by compose.*: compose may be imported by
cats.instances.function.*)`) — in a goal, an input, a capability record's field, a constructor
slot or a projection's prefix — while a definition of the reading file (another block of its
package in that file included), an inherited member, an explicit import, a same-scope supplied
wildcard and a `_root_` path keep the name (Scala rejects a reference they and the wildcard both
bind, E049). A `given` selector binds the given instances of its prefix, terms of any name (no
type); an importer whose prefix is no path binds a value's members, not followed; a supplied
wildcard binds what its namespace holds, a value or an enum case class included, neither read; the
root imports' reading refuses a builtin a wildcard behind a doubted prefix may bind, and the
`Singleton` marker is read only where nothing may rebind the name. A path's first name is a term:
a clause over a supplied namespace binds it to a member that namespace has under the name and the
clause imports — a wildcard every accessible member but a given, a `given` selector the given and
implicit ones; no private member outside where it is accessible — among its own definitions (a
package's subpackages included; a type or a class of the name is none), the aliases its export
clauses define, and what an object's or a package object's parents pass on; a parent outside the
sources may declare any name, so the head is refused. An object, a subpackage or a companion is
read on; a value is not followed. A first name nothing around the reference binds is the nearest
package the scope opens that has that member among the sources, not every package that holds the
selected name. An enum's class case is a type its companion's wildcard binds; a singleton case is
a value. A package some supplied file declares a member in — a package object counts — is read as
complete (the closed-package assumption: no dependency adds to it), while one the sources spell
only as a prefix of a deeper package holds what a dependency declares there, so its wildcard may
bind any name but its subpackages. The stored side refuses a name a wildcard over an object or
package object with parents or export clauses may bind and a private member, which it does not
follow. Both sides read the stdlib namespaces whose members are known from one table
(`ListedNamespaces`, D16). A name whose binding is outside the sources, or a clause the calculator
does not follow, is a reason naming that clause, never a same-named declaration found elsewhere.
Rows that would rest on an uncomputed value say why: `sealed family not summed`, `sealed child
parameter not pinned`, `lazy cycle not completed`, `cross-package cycle not solved`, `type
constructor`. See [stored-value names](stored-value-names.md).

**Evidence.**

- Scala 3 draft specification, ch. 2 \[28\], and SLS 2.13, ch. 2 \[32\] — both **Inspected (full
  text)** by the literature verifier: "There are two different name spaces, one for types and one
  for terms"; the precedence list (local, inherited or package-clause definitions of the same unit,
  then explicit imports, then wildcard imports, then other units' package members and the root
  imports); inner-scope shadowing ("Scopes are nested").
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
- **Compiler fixture** (Scala 3.9.0, `probes/a7/prec`, `probes/a7/plain`, `probes/a7/spec`): an
  unsupplied wildcard's member beats another file's package member, rebinds a later wildcard's
  prefix (`import zzz.v.*; import compose.*`), a named import's first name (`import dev.other.Y`)
  and a qualified type's (`dev.r.optics.BijectionIso`, but not its `_root_` spelling), and an inner
  one shadows an outer supplied wildcard; a same-file definition, a named import and a supplied
  wildcard of the same scope win; an outer definition, an outer named import, a same-scope
  supplied wildcard or an inner package's member of another file against a wildcard binding the
  name are each E049.
- **Compiler fixture** (Scala 3.9.0, `probes/review-A7/r5`, `r8`, `r9`, `r11`, `r12`, `r14`–`r16`,
  `probes/review-A7q/sc`): a definition in a sibling `package p { }` block of the same file beats
  `import zzz.*`; `import zzz.given` binds a given `zzz.compose` over another file's package
  `p.compose`; `import zzz.{Boolean as _, *}` then `import compose.*` (or
  `scala.jdk.CollectionConverters.*`) binds a `zzz.compose.Boolean` (`zzz.scala.jdk…`); a member of
  `zzz` from a separate compilation unit is bound through `import zzz.*` when the sources supply
  only `package zzz.inner`; `import Lib.*` binds `val Lib.compose`, `import E.*` the case class
  `E.A`, `import Outer.this.o.*` the value's `o.Two`, each over another file's declaration; under
  `import zzz.*` a `zzz` declaring `type Singleton = Nothing` makes `O.type & Singleton` empty.
- **Compiler fixture** (Scala 3.9.0, `probes/review-A7/round2/*`, `probes/a7fix2/spec`,
  `probes/a7fix2/stored`): `import Lib.*` binds an object, a value or a type alias `Lib` inherits
  or exports, and a package object's parents' members through its package's wildcard; it binds no
  given, no private member, no type or class as a path's first name, and no singleton enum case as
  a type; `import Lib.given` binds an inherited given; `c` in package `a.b` is `a.b.c` when the
  sources declare it, whatever the root `c` holds.
- Bekić's lemma (**classical**): solving an object's members jointly with the enclosing body
  gives the same least fixed point as solving the body and then the object.
- **Executable**: `NamespaceSpec`, `NamespaceScopingSpec`, `ReportSpec`, `ImportClausesSpec`,
  `UnsuppliedWildcardSpec`.

**Boundary.** This decides only *which* declaration a name denotes, and refuses where the value
is not computed; it adds no counting rule. It reads an enum as a namespace only through an
explicit companion, whose class cases it refuses as types (D29), and does not read
a namespace no source supplies — the standard library's included, beyond what `ListedNamespaces`
lists (D16) — each stays `?` with its reason. It does not argue that a rebinding would make the
rest of the program fail to typecheck (no rule licenses that; the B3 delta audit's dissent on
`ModifyF.map.map`). The closed-package assumption is an assumption, not a fact Scala checks: a
dependency may declare members in a package the sources also declare members in; the decision reads
such a package as the sources give it. The stored side, and the constructor slots it reads, read
each package block with its own definitions only (another block of the file is refused through a
wildcard there). Explicit imports and sealed families are D15's.

## D12. Top types carry nothing in an input; a goal with one value has one implementation

**Decision.** On the method side `Any`, `AnyRef` / `Object`, `Matchable` and `AnyVal` in an input
are the terminal type `1`: inputs of them carry no information, `Any => R ≅ R`. D38 supersedes the
reading of results: a top result is ω, not one implementation. Every universal member — `==`, `!=`, `equals`, `hashCode`, `##`, `eq`, `ne`,
`isInstanceOf`, type patterns, `asInstanceOf`, `getClass`, the default `toString`, `synchronized` —
is outside the model on a value of an abstract, top or top-like type. A declared result with exactly
one value (`Unit`, a singleton type, products of those, a function into one, a closed type of value
space 1) counts 1 whatever the environment leaves unresolved; a top result is not one (D38). The
stored-value side reads `Any` as every value, ω (D38); constructor positions are read through the
method-side model.

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
Scala `Any`, and the exclusions above are what make it terminal. A `transparent inline` result
whose declared type is not a singleton stays `?`, even a one-valued one: the call site observes the
expansion's type (Scala 3.9.0 compiles `mk(true).n` against a declared `Foo` whose expansion is
`new Sub(true)`, and gives a returned stable path its singleton type — **compiler fixtures**, D27);
a top-typed parameter of an `inline` method refuses nothing by itself (D38): only a compile-time
operation such as an `inline match` does. A clause that may rebind a builtin name keeps its
reason, and so does a channel name lookup does not walk that reaches declarations the sources do
not supply (a self type, the parent of a wildcard-imported or package object, a wildcard namespace,
the outer clause of a chained package no supplied unit declares into): there every builtin and
model name is refused (D16) and the one-value rule does not fire. `Any*` is not one value (its
length is observable); an input `Set[Any]` is `Set[Unit]`'s 2 values, since the type fixes no
size (D38). See
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
scope is its own declaration's key (D35). Bounds to open traits wait for the
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

- \[30\] Scala 3 draft specification, ch. 5, and \[33\] SLS 2.13, ch. 5 (**Inspected (full text) —
  bibliographer**, bibliography T5): `sealed` and `final` inheritance. \[24\] Scala 3 reference,
  *Translation of Enums and ADTs* (**Inspected (full text) — bibliographer**, T5): enums as sealed
  abstract classes, cases as final case classes, explicit `extends` pins. \[22\] Scala 3 reference,
  *Open Classes* (**Inspected (full text)** (verifier), T5): a sealed class is extended only in its
  source file. \[29\] Scala 3 draft specification, ch. 3 (**Inspected — bibliographer**, T5): a
  sealed class is disjoint only "without anonymous child".
- \[15\] Scala 3 reference, *Context Bounds* (**Inspected (full text) — bibliographer**, T2): a
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
subtyping would match stay `?`, as the page's limits state. In the closed world (D39) a leaf no
supplied template extends is final. A case-class leaf a supplied class extends is no closed
summand when a subclass may override a member it has (its own — an extension method
included —, a source parent's body member or `val` parameter, or one the stdlib model lists): both
sides refuse the family (`sealed family leaf may be extended: S1 (overridable member m)`); the
compiler's synthesized case-class members and the leaf's own fields (its first clause and `val`
parameters, not a later clause's plain parameter) are outside the reading (an override of a field
is another value of its type). A parameterless member fixed anywhere in the ancestry (final,
inline, a given or a `var`; a private one is no member of a subclass) fixes its name for every
subclass, so a final override in a parent between the leaf and the declaration closes it (scalac
rejects overriding it again, E164); a member with parameters fixes nothing, since the member it
overrides depends on the types its parameters resolve to (a final `d(i: Int)` leaves an inherited
`d(s: String)` open). Every parent is read before a member is: one not read is the reason. Each parent is the
declaration its name denotes where the clause is written: the method side resolves it as any
parent (imports, enclosing scopes), the stored side reads a class or trait of the file the name
denotes lexically there, or the stdlib model's parent (one table for both sides,
`StdlibModel.leafParent`) only where no source binds the name at the clause — an imported source
`Product` is `(parent not read: Product)`, another package's `Exception` no import brings is not the
one a leaf extends. A case class a supplied class extends (D39), with more than its product, is
refused for what it is (`case class open to extension: NC (overridable member hit)`).

## D16. Library declarations are read from a versioned model; absent ones are reported

**Decision.** Declarations the sources do not supply are read only through a small table keyed by
fully qualified name (`stdlib-model/1`, Scala 3.9.0 on 2.13 collections): tuples, `Some`, `Left`,
`Right`, `EmptyTuple`, `NamedTuple` as products; `Option`, `Either` as sums; `FunctionN` as
arrows; `List`, `Vector`, `IndexedSeq`, `Seq`, `ArraySeq` and `Array` of a class as finite
sequences `Σₙ Eⁿ`, and so the single-pass `Iterator`, `IterableOnce`, `ByteBuffer` and
`InputStream` (D36); `Map` and `Set` as the finite functions they tabulate (D36);
reflective and type-revealing (`ClassTag`, `TypeTest`, `Mirror`) types excluded; classes and Java
interfaces whose members it does not hold (open ones, and the final `Integer`) refused. The table
answers after every binding of the sources, under the root imports' spellings only; a wildcard of an
unsupplied namespace rebinds no builtin or model name (D37), and a wildcard of a listed namespace
binds the type and term names it lists ahead of another file's package member, and nothing else
(`quotes.reflect`, whose members are not listed, any name but a root spelling the reading side
reads — of the root imports' names it declares `Singleton` and `Symbol`, which the table records);
one table, `ListedNamespaces`, holds them for both the method side and the stored-value estimate.
A name nobody declares is `declaration not supplied: <fqn> (<binding>)`. An open owner's foreign
parent is reported, not ignored.

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
  The scala-library 3.9.0 jar (**inspected**, `javap` and its entries): `scala.quoted` holds
  `Expr`, `ExprMap`, `Exprs`, `FromExpr`, `Quotes`, `ToExpr`, `Type`, `Varargs`, the top-level
  `quotes` and the subpackage `runtime`; `CollectionConverters` the twenty conversion methods of
  its implicit classes; neither, nor `Quotes#reflectModule`, a member `scala` or `java`. The TASTy
  of `scala.quoted.Quotes` (**inspected**, decompiled with the 3.9.0 compiler): `reflectModule`
  declares 323 type members, abstract ones among them (which `javap` cannot show); `Singleton` and
  `Symbol` are the only ones a `scala.*`, `scala.Predef` or `java.lang.*` type also spells — a
  compiler fixture accepts `summon[Symbol =:= quotes.reflect.Symbol]` and `Singleton <:< TypeTree`
  under `import quotes.reflect.*` (`probes/review-A7/r4/m.scala`).
- **Executable**: `StdlibModelSpec`, `ImportClausesSpec.listedWildcards`,
  `UnsuppliedWildcardSpec`.

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
- Scala 3 draft specification ch. 3 (\[29\], **inspected**): abstract types have bounds, and the
  lower-bound rule breaks transitivity, so only unbounded members are free atoms; Hu & Lhoták 2020
  (**metadata-verified**: the abstract reports undecidable `D<:` subtyping and decidable
  fragments without bad bounds) is why bounded members stay `?`.
- Match types: Scala 3 reference *Match Types* (\[18\], **inspected**), SIP-56 (\[1\], **inspected**)
  and \[29\]: reduction only past provably disjoint cases, otherwise stuck, "just an abstract type";
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

- Reynolds 1983 (\[9\], **bibliography-verified**) and Mitchell & Plotkin 1988 (C-10,
  **classical**): a closed term's denotation is parametric and its internals are unobservable
  through readable types. Pitts & Stark 1998 (**metadata-verified**: the abstract reports that
  local state must be handled for representation independence) is why mutation must be excluded,
  not the `var`-as-value reading itself, which is the contract's.
- Scala 3 reference, *Export clauses* (**inspected**, extraction — cross-check): export clauses
  define aliases for selected members; Scala 3 draft specification ch. 2 (\[28\], **inspected**):
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

**Boundary.** The premise is readability: classes no public constructor builds (D35), opaque
callables with transparent results and names outside the sources
keep their own reasons, which is where a concrete producer could matter. The premise also needs
the values: a capture the target cannot name (hidden by a member, an inherited member or its own
parameter) that a member nothing overrides still holds is put back when the member is the capture
itself (a parent's field the template can read passed exactly it, `def seed = b`) at the
capture's own type, adds nothing when the member's type has one value (`def seed: Any = b`, a field
of a binder the template gives `Any`: D-top), and is refused otherwise — a wider type, a field a
nearer level overrides (the parent's constructor still read the argument: a 3.9.0 run shows a
strict val of the parent keeping it), any other reading (`local capture held by a member that hides
it: n`; `mk`: 16, it read 4). The same holds for a local definition around the target — a method, a
lazy value or a given of an enclosing block, before or after the target (`def seed = b` beside
`def target(b: Boolean)`: 16, it read 4) — and a local object or class reading the capture is
refused (`class H { def seed = b }`, which the target may build). No subclass overrides a given (a deferred one, `= deferred`, is
abstract: each class extending its trait supplies it) or a `var`, nor a final, inline or
unqualified-private member (E164, `Overrides`); in an
open template such a member implements an inherited declaration for good only when the
declaration is parameterless (a same-shaped overload is not an implementation). Exported or imported
terms that may hide a local capture, the names an ancestor's exports define, a foreign wildcard
export, a
package-level export of another file, a path through a value, an override a base class outside
the sources may intersect, a call an import, export or inherited member may bind, and inference
that matches argument types stay `?` with their reasons.

## D20. A value of a source class is the record of its observations

**Decision.** A value is observed only through its accessible members, the universal ones (runtime
equality and reflection: `Any`'s `toString`, `hashCode`, `equals(Any)`) excluded. A member an
instance may replace — abstract, or concrete and not effectively final, a parent's constructor `val`
included — is a free capability of its type; a member none may replace (`final`, `private`, concrete
`inline`, a given or member object, any member of a final class, object or given) is concrete code
over the receiver's observable state and adds no choice. So an open trait, or an open class whose
constructor parameters are all public `val`s, is the product of its open members and fields, as
input and as goal; a final class with a public constructor whose every parameter is a public `val`
or exposed by an η-alias member is the product of its parameters; a `final abstract` class is the
empty sum. On the measured declaration's own receiver (and on enclosing ones) the same
classification holds for its own and inherited concrete members (a member is overridden by the
levels before it in the receiver's linearization). A member whose default evaluates the declaration
whenever it is evaluated (the one member its arguments select, every value clause applied, at a
position the compiler wraps in nothing that defers it: no extension, conversion or `==` overload
taking it by name, no context-function parameter, no implicit search) is a view: dropped when
definite, a reason when it depends on arguments, on a by-name operand, on a nested template's body,
on `super`, on an η-expansion or on such an insertion. An inherited member is the measured
declaration when parameters match as seen from the receiver, through every level, keyed by the
declarations the scope-correct lookup binds; results are not compared. Details: [capability
records](capability-records.md).

**Evidence.**

- Scala 3 reference *Open Classes* (\[22\], **inspected**): traits and abstract classes are always
  open and any non-final class may be extended; Scala 3 draft specification ch. 5 and SLS 2.13
  ch. 5 (\[30\], \[33\], **inspected**): a final member may not be overridden, members of final classes
  or objects are implicitly final, a concrete definition overrides an abstract one; *Inline* (\[17\],
  **inspected**): inline methods are effectively final; *The Matchable Trait* (\[19\],
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

**Boundary.** Records with abstract type members, self types, trait parameters, extension or
pattern-defined members (a `var` reads as its value: D26), members hidden at the use site, hidden
state of an open class or of a parameterized parent no class passes arguments to (no instance may
exist: X6), unexposed or non-injectively exposed final fields, non-public or scoped constructors,
recursive records (the type reading's cycle guard) are refused with their reasons, and
polymorphic open members are read by D24; a member's type is read at the record's arguments, its
template's aliases included (`type T = A` is the record's argument, never the template's binder or a
member binder that shadows it), its proper parameters remembering their arguments as written so a
match-type field reduces on a tuple argument (eo's `pickFoldMiss`: 1); case classes, sealed
families and enums keep their own readings, except that a final case class with more than its
product (members, parents, a later clause, a constructor modifier) is a final class's record (eo's
`JsonPathScanner.Span`: 2^64 values). A possible view (but see D31), a record member that may be
the declaration on its receiver, a view through code the compiler inserts, and an override
identity resting on foreign or opaque declarations stay `?`. The
view rule assumes no unsupplied *named* import brings a by-name implicit conversion and no
unsupplied class overloads `==` by name; every other unsupplied channel (wildcard import, parent,
self type, an import or export of the selected name, a block's import) refuses. Open: a by-name
conversion or extension in the companion of a type outside the sources is not seen (a definite view
read where none exists: fixture `fc`, 4 for true 16). Only a member that builds Unit is dropped
regardless of unreadable parameters; any other unreadable member type fails the environment eagerly.

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
- Scala 3 draft specification, ch. 3 (\[29\]) — **inspected (full text)**: "A literal type `lit`
  denotes the single literal value `lit`." SIP-23 (\[3\]) — **inspected (full text)**: literals
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

- Scala 3 reference, *Context Functions* and *More Details* (\[16\]) — **inspected (full text)**:
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

**Decision.** `ClassTag`, `Manifest`, `TypeTest`, `Typeable` and every `Mirror` are excluded from
the stdlib model as type-revealing evidence, as `scala.quoted` is as compile-time reflection. An
`inline` method is refused only when its body uses a compile-time operation (D27, which revises
the refusal of every inline method this entry first recorded).

**Evidence.**

- Scala 2.13 library, `ClassTag` (\[11\]) — **inspected (full text)**: "can serve as an extractor
  that matches only objects of type `T`"; *TypeTest* (\[25\]) and `scala.deriving.Mirror` (\[14\]) —
  **inspected**.
- Related work, **metadata-verified**: Washburn & Weirich 2005 (LICS; the abstract reports that
  run-time type analysis breaks standard parametricity); Neis, Dreyer & Rossberg 2009 (ICFP; the
  abstract states that "type abstraction and intensional type analysis are features seemingly at
  odds").
- **Executable**: `SyntaxFeaturesSpec`.

**Boundary.** `ValueOf` is not excluded (it supplies a value). See
[syntax-features.md](syntax-features.md).

## D24. A polymorphic capability member is read up to its free theorems

**Decision.** A supplied member's own type parameters are not free choices of type: supplied
members are parametric (D3), and free theorems identify terms that instantiate them differently. A
binder the member's type never mentions is irrelevant (PB0) when some type meets its bounds: a
one-sided bound always is met (`Nothing` below an upper bound, `Any` above a lower one), a two-sided
one need not be (`M >: Int <: String` admits no type, so no call typechecks) and is refused as
`polymorphic capability binder: M (bounded)`. A binder met only in generators `P → M`
and at positive positions of the rest is the sum of the generators' domains — `∀M. (P₁ → M) → … →
(Pₖ → M) → G(M) ≅ G(P₁ + … + Pₖ)` for covariant `G` — for an input and a goal alike (PB1): a fold is
its focus, a known identity is `1`, `∀M. M → M → M` a tag, and `∀C. C` the empty type, which makes
an environment holding it vacuous (count 1). Any other member stays polymorphic: a goal of it gets
fresh rigid binders; as an input, a path that mentions none of its binders is an ordinary producer,
and any other is instantiated only where matching its result against a goal fixes every binder it
mentions (PB2), and then only a count of 0 or 1 is reported; a result no goal meets builds nothing
(PB3); a binder no goal fixes, or a result eliminated by cases, keeps `polymorphic capability
binder: M (not determined by the result of C.m)` (PB4). A binder that occurs in an applied
parameter is fixed by the applied values the environment supplies — its values and fields, its
monomorphic producers' results and the instances' own results — each distinct instantiation one
monomorphic callable, counted like any other (PB5, owner decision 2026-11); a producer that can be
applied and builds such a value at a type it chooses keeps the path refused (`… (instantiations not
closed: a polymorphic producer builds an argument of C.m)`). A member with a path whose result is a
bare binder PB5 does not fix leaves only ω and absurd contexts reported (`… (instantiations at
compound types not searched for C.m)`): a normal form may instantiate it at a compound type and
eliminate the value. A
context bound is the `using` parameter it stands for (D14; higher-kinded binders R4), spelled into
the member's callable; bounded, view-bounded and higher-kinded binders keep their own reasons. A deferred (abstract) `inline`
member is no term of a body that is not inline — the compiler rejects the call — so it is dropped
when its type is built from nothing (it then constrains no instance), and refused otherwise (`deferred
inline member whose type may be empty: C.m`); in an inline body each expansion calls the override
of its receiver's static type (`deferred inline member resolved at each expansion: C.m`). Details:
[polymorphic members and saturation](polymorphic-members-and-saturation.md).

**Evidence.**

- Reynolds 1983 \[9\] (**bibliography-verified**), Wadler 1989 \[37\] (**metadata-verified**): D3. The
  Yoneda lemma (**classical**); its parametric-model form and PB1's admissibility condition
  (generators plus a syntactically covariant rest) are this register's derivation. Pistone &
  Tranchini 2021, CSL, DOI 10.4230/LIPIcs.CSL.2021.35 (**metadata-verified**: the abstract reports
  that the Yoneda isomorphisms induce a quantifier-eliminating rewriting of types); Plotkin & Abadi
  1993 (**metadata-verified**: the abstract reports data-type encodings under parametricity);
  Hasegawa 1994, Bainbridge, Freyd, Scedrov & Scott 1990 (**metadata-verified**, background only);
  Wadler 1990, unpublished draft (**metadata-verified**), cited for excluding algebra premises.
- Urzyczyn 1997 (**metadata-verified**: the abstract reports System F inhabitation undecidable;
  C-7 **classical**): no instantiation bound computable from the goal is complete, so counts above
  1 need an exact reduction. Scala 3 reference *Polymorphic Function Types* \[23\] (**inspected**).
- PB2's upper bound (crosscheck X7) rests on coverage, which holds for members with no bare-binder
  result: every application of a path ending in a rigid constructor `F[M]` stands at a goal whose
  match fixes it, so every long normal form has a representative; quotienting only merges. Round 1
  claimed coverage for every member, which is false (review round 2, `probes/review-B4/r2/i1`:
  `c.run[(B, B)](swap, (b1, b2))._1` has no representative instantiating `run` at `B`); a bare-binder
  result now leaves only ω and absurd contexts reported. A record member's type is read at the
  record's arguments, template aliases included (`probes/review-B4/r2/a1check`: scalac types
  `b.get[String](1)` on a `Box[Int]` whose `type T = A`).
- **Compiler fixtures** (Scala 3.9.0, `probes/b4/spec` poly1–poly4, `probes/b4fix/spec` inl and
  bounds): every `PolymorphicMemberSpec` fixture compiles. `probes/review-B4/pb0c`: scalac rejects
  every call of `f[M >: Int <: String]` and of `f[M >: X <: String]` (E057). `probes/review-B4/inlc`,
  `inlc2`, `inl2c`: a call of a deferred inline member from a body that is not inline is rejected
  ("Deferred inline method … cannot be invoked"); `probes/b4fix/inl`: an inline body may call one,
  and its expansion at a receiver whose static type has an inline override compiles.
- **Executable**: `PolymorphicMemberSpec` (phantom, fold 1 where naive is ω, chooser 4 and its goal
  2, constant Boolean 4, `Option[M]` 2, known identity 1, `∀C. C` 1 and its goal 0, a two-sided
  phantom refused and a one-sided one ω, a tuple generator `(2^33 + 1)^(2^33 + 1)` and a spread
  tuple parameter 4; PB2 1 and 0, a naive 2 refused, PB3 irrelevance, PB4, the `Monoid`-like
  algebra, Church goal ω, bounded, context-bounded and higher-kinded; deferred inline members 2, 4,
  2, 2, 2 and their refusals; template aliases at the record's arguments 4 and 1, a monomorphic
  path ω and its refused Boolean twin, a bare-binder result refused at 0, records of unrelated
  traits as evidence), `InheritedOverrideSpec.otherGeneric` and
  `OverrideIdentityImplementationSpec.foreignBinder` (`∀C. C`: 1),
  `OverrideIdentityImplementationSpec.nominal` (`∀X A. X → A → A ≅ 1`).

**Boundary.** PB1 does not read through rigid constructors, sequences, proofs, packages or another
member's binders; a reduction for higher-kinded binders needs type-lambda shapes; algebra premises
(`Monoid`, `Functor`, `Applicative` evidence, foreign or source) are never reduced by laws. PB2
never reports a count above 1, and never instantiates at a type a goal does not fix; beside a
member with a bare-binder result it reports no finite count at all (`zero[A](c: Church): A`, true
0). A member whose result is a bare binder matches every goal, so beside a constructor it can grow
(`unwrap[A](fa: F[A]): A` at `A := F[B]` needs `F[F[B]]`, …) the goals never repeat and the state
budget answers (`Inhabitation state budget exceeded`). A deferred inline member whose type is not
built from nothing is refused even where the count is known to be 0 (whether an instance implements
it together with the rest is not decided).

## D25. A supplied sum is eliminated once per argument and path

**Decision.** A supplied callable whose result is a nonempty sum or a ground value is cased on once
for each tuple of arguments the context can build, as soon as they exist, and never again on that
path; its cases are counted independently by the solver's split, and answering with the call itself
is the η-case of that split. Arguments range over the neutral terms of first-order argument types —
variables and applications of producers that are not themselves eliminated. A callable whose cases
lead back to its own argument types, or whose argument types a producer cycle builds, is `sum
elimination over an unbounded argument space: p`; ground, sum, sequence or lower-bounded argument
types, and types a polymorphic input may build, are `sum elimination over an argument space not
enumerated: p`; higher-order arguments keep the elimination reason. A root whose rules carry
reasons is ω when it reaches, along rules with no reason and no polymorphic instantiation whose
every child is built without reasons, a cycle, and the empty goal is not built even optimistically
at any goal on the way (C11 as restricted by crosscheck X3). Details: [polymorphic members and
saturation](polymorphic-members-and-saturation.md).

**Evidence.**

- Scherer 2017, POPL (**metadata-verified**: the abstract reports βη-equivalence with sums and the
  empty type decidable and coinciding with contextual equivalence); Scherer & Rémy 2015, ICFP
  (**metadata-verified**: the summary reports 0, 1 or ≥ 2 inhabitants decided by saturating focused
  search; background for "cut only new sums"); Scherer 2016, thesis (**metadata-verified**). The
  once-per-path elimination with argument enumeration and the restricted ω conjunction are this
  register's derivations; the empty-type part rests on the solver's per-node absurd rule, never on
  an at-most-two cutoff (crosscheck X8).
- Espírito Santo, Matthes & Pinto 2021, TYPES 2020, DOI 10.4230/LIPIcs.TYPES.2020.4
  (**metadata-verified**: the summary reports finiteness of focused inhabitants decidable with
  disjunction); David & Zaionc 2009 (**metadata-verified**, background). D2 (finite texts) bounds ω
  from above; D4 (productivity) supplies the cycle.
- **Compiler fixtures** (Scala 3.9.0, `probes/b4/spec` sums1 and c11, `probes/b4fix/spec` sat,
  low and keys; `research/xprobe/c11`): every `NeutralSaturationSpec` fixture compiles.
- **Executable**: `NeutralSaturationSpec` (composed getters 2, `FromFunctions` 2, prism modify 1 and
  replace 2, built arguments 16, Boolean results 16 and 4, an absurd case 1; unbounded, payload
  cycle, literal, sum, lower-bounded, polymorphically built and higher-order refusals, a record's
  callable named by its field; neutrals identified by structure, not by spelled names: 2^(2^16)
  however the values are named; `branchCycle` ω, `branchAbsurd` and `reasonedAbsurd` never ω,
  `branchAbsurdPlain` 1, `siblingZero` never ω, `siblingEmpty` 0, `reasonedCycle` ω, a cycle
  through a polymorphic instantiation never ω),
  `OpaqueSumForwardingSpec` (saturation agrees with the certificate where it applies: 2, 1, 1, 2;
  12, 12, 4, 1, ω, 6, 6, 2, 2, 2 beyond it), `InhabitationSpec`, `GroundTypesSpec.groundCallable`,
  `TopTypesUnionSpec.callableSum` (27), `EnvironmentCompletenessSpec.opaqueCallable` (4).

**Boundary.** Arguments over ground (literal) spaces, sums, sequences, lower-bounded types or
polymorphic producers are not enumerated, and unbounded families are not counted; ω for them is
claimed only under C11's conditions, which a cycle behind a reason-carrying rule never meets.

## D26. Code has no side effects: a `var` is its value, a throw supplies nothing, a throwable is data

**Decision.** The analysis assumes the code it measures has no side effects (the owner's model
decision, v1 §2), and every reading obeys it.

1. **A `var` reads as a `val`.** A `var` holds the value it was given:
   - a public `var` constructor parameter is a field (an open record's, a final record's, a case
     class's or a sealed leaf's); a plain class's parameter with neither `val` nor `var` is no field
     (E173), so a sealed leaf with one is refused;
   - an abstract `var` is its getter (its setter is an effect);
   - a concrete `var` is derived code (no override exists, E164).

   No reason cites mutation. Only Scala's typing rules still refuse: a `var` is no stable path
   (`not a stable path: x is a var`).
2. **A throw supplies nothing.** A term that only throws is typed `Nothing`, as Scala types it:
   `throw e`; the root imports' `???` or `sys.error(…)`; a block whose result throws; a conditional
   or match whose every branch throws. In particular:
   - an inferred result from such a body is `Nothing`, and a conditional's throwing branch joins
     nothing to the other branch;
   - concrete code nothing can override is never a producer (D19); its throw supplies nothing, so
     the template around it is built;
   - an overridable member whose default only throws reads exactly as its abstract twin: a free
     capability of its declared type, the default being none of its behaviors (`class C { def get:
     Boolean = ??? }` keeps every Boolean an override gives it). Where that type is empty, no
     override implements it either, so the record or receiver has no instance and is vacuous, as
     D20 reads the abstract twin. The default instance is no value: were it one, every member
     calling a throwing `get` would throw on it and the open reading would undercount. The reading
     is taken at the record's or receiver's type arguments (`K2[Int]`, `K4 extends K2[Int]`), and
     a subclass whose member only throws counts as its parent with the abstract one;
   - a template whose primary constructor evaluates a parent argument, a strict `val` or `var`
     initializer or a statement that only throws is never built, by any subclass either: its record
     is empty (an input vacuous, a goal 0), and a declaration that runs in it — or in a template
     its instances carry (an outer instance, a local template's receiver, a member object's owning
     instance, and for a top-level definition its file's synthetic object; not a static object
     around a template) — is never called: 1. A name there is read in its own level's scope (its
     parameters, members and inherited members, never a subclass's; the imports before it; the
     exports of its template); one an unread scope may bind leaves the construction unresolved
     (`construction not resolved: C`). The measured declaration's own body is excluded, and
     constructor rows keep their convention. A sealed case, family or enum whose construction
     (every template it extends, and for singleton enum cases the companion) would only throw is
     refused, since that reading resolves no term names (`sealed family construction not
     resolved`, `enum construction not resolved`);
   - a throw's operand reads nothing that a result is built from (a final implementation's hidden
     capture). A re-entry inside it still counts as a view, because the operand is evaluated.
3. **A throwable is data.** Every root-import throwable is an ω transparent value on the method
   side:
   - as a goal, `new T(s)` gives a value per string, and `getMessage` tells them apart;
   - as an input, it is observed only through strings and throwables, which are closed data built
     from nothing, so it is eliminated by the uniform split;
   - subtyping adds only producers of types that are already inhabited;
   - a `null` message is read by the `T | Null` rule.

   On the stored side a throwable is an open class: `open to implementations (Throwable)`, never a
   size. Its stack trace (reflection), `initCause`, `addSuppressed`, `printStackTrace` (effects)
   and `getSuppressed` are outside the model. Details: [code without side
   effects](pure-code.md).

**Evidence.**

- \[29\] Scala 3 draft specification, ch. 3 (**Inspected (full text) — bibliographer**): "´p.x´ is
  a stable type if an only if `t` is a `val` or `object` definition". \[30\] Scala 3 draft
  specification, ch. 5 (**Inspected (full text) — bibliographer**): a `var` class parameter adds a
  getter and a setter `x_=`; "A stable member can only be overridden by a stable member".
- **Compiler fixtures** (Scala 3.9.0, `$S/probes/c4/fixtures`): `throw`, `???`, `sys.error` and a
  block ending in a throw are `Nothing` (each conforms to `Int`, `String`, `Boolean`, `Unit`);
  `if (c) 1 else throw …` is an `Int`; a source `???` is the member called; neither a `def` nor a
  `val` overrides a `var` (E164); a `var` has no singleton type (E083).
- **Tool output**: `javap -public` (OpenJDK 21) on the twelve root-import throwables gives the API
  the model reads: constructors from `()`, a message, a message and a cause, or a cause; the
  overridable `getMessage`, `getLocalizedMessage` and `getCause`; and the excluded members.
- **Compiler runs** (Scala 3.9.0, `$S/probes/c4-fix1/run`, `$S/probes/review-C4/run*`,
  `$S/probes/review-C4/r2`, `$S/probes/c4-fix2`): `def get = ???` overriding `get: Boolean` is
  overridden with `true` and with `false`; a strict `val` initializer (a trait's included), a body
  statement, a parent's argument and a `var` initializer throw in every constructor, an overriding
  subclass's included, and a `lazy val` does not; an object's nested class and object run while
  the object's initializer throws, a member object's class needs its owning instance, and calling
  a top-level `def` runs its own file's top-level `val`s; a parent's body and arguments bind `???`
  lexically, never to a subclass's member, and an import only after its position; a singleton
  enum case needs its companion, a class case does not.
- The tier argument for throwables (ω as goal; uniform split as input; subtyping and `null`
  change no tier), the throw rule's case split and the emptiness and construction readings are
  this register's derivations. The no-side-effects assumption is a model decision, not a cited
  theorem; reading a `var` as its value remains a model decision. Related work: Pitts & Stark 1998
  (**metadata-verified**: the abstract reports that local state must be handled for
  representation independence and for memoising functions) and Ahmed, Dreyer & Rossberg 2009
  (**metadata-verified**: the abstract reports that representation independence holds even when
  representations depend on state) show why the exclusion is load-bearing; Banerjee & Naumann 2005
  (**metadata-verified**, background).
- **Executable**: `PureCodeSpec`, `ThrowableSlotsSpec`, `StdlibModelSpec.open`,
  `KindedIdentitySpec.modelKeys`, `CapabilitySpec.unreadMembers`,
  `MethodRefinementSpec.mutablePath`, `StoredValueSoundnessSpec.taint`,
  `TypeMemberSpec.valuePaths`.

**Boundary.** These stay `?`, each with a named reason:
- a union with a throwable (no runtime class the class test tells apart yet);
- an open subclass of a throwable (its parent's overridable reads named);
- a wildcard import that may bind a throwable's name;
- a `try`;
- a match mixing throwing and returning cases;
- `Try`, `NonFatal` and `MonadThrow` (not modelled or not supplied);
- an open member without a result type (`overridable member's result type is inferred`);
- a polymorphic open member, abstract or with a throwing default, where D24 leaves it so (its
  bounded, higher-kinded or undetermined binder named); a throwing default is read by D24 exactly
  as its abstract twin;
- a sealed case, family or enum whose construction would only throw (its names unresolved).

`Throws` recognizes its forms by syntax and never follows a body into a callee. An object whose
initializer throws is still one value as a singleton type, and a class a static object encloses
is built without it. An inline member of a static object or of a file's top level is counted as
in a built owner: its expansion does not build the owner (Scala 3.9.0 runs print no `K init` for
`K.f(true)`), and the owner's own members supply nothing — an object or a file declares no slot
and has no open member, so none is an input; a class's, a trait's or a member object's inline
member keeps 1. A parameterless `given … with` instance is such an object (Scala 3.9.0 compiles it
to a module, which `G.f(true)` does not initialize either). These runs were session fixtures (Scala
3.9.0, not retained). The sealed-family reading reads no leaf member: an abstract empty member of
a family's root does not empty it (predating D26, it belongs to D15); a non-final case-class leaf a
subclass may extend with an override is no closed summand (D15). Open (owner): a member nothing
overrides that implements a declaration of an empty type (`object OQ extends P { def k(x: Int): Nothing = ??? }`,
eo's `PSVec.Empty.apply`) is code no pure program contains; its template is read as built (`OQ.f`
4) while its parent's record has no instance (1).

## D27. An inline method free of compile-time operations is a method

**Decision** (owner decision D-inline). An `inline def` counts exactly as a `def` when its body —
its parameter defaults, its nested definitions, and, transitively, every inline method it calls —
uses no `inline if`, no `inline match`, no `scala.compiletime` member (however its name is bound),
no quote or splice. `transparent` and `inline` parameters do not disqualify it. An `inline`
parameter is its argument substituted by name, which with no side effects (D26) has its value.
A2's rule X still refuses a `transparent` result callers may see narrowed, which is every declared
result but a singleton type. Every name the body calls must resolve, as Scala binds it, to:

- a supplied method that is not inline,
- a supplied inline method that passes, or
- a member of the library table (methods the compiler reads as regular).

Refused besides, because each reaches the call site without a compile-time operation in the
source:

- a member an expansion may select on a narrower class: one a supplied subclass overrides (the
  closed world, D39) while an inline method of its name in a supplied template that may extend
  the receiver's class (never an extension method, nor a member of an unrelated template) is not
  one function of its values, selected on a parameter of the expanded method, on `this` or `C.this` of a template a
  class may extend, on a lambda's parameter, or on a field or result selected on one of those
  (every function type's `apply` included); an argument clause a callee does not take selects
  `apply` on its result, which is checked the same way (an unread result refuses);
- a runtime class an expansion reifies from a type variable: a type test or `classOf` whose
  erased class depends on one, `Array[A]` included;
- a contextual clause left to implicit search — a callee's, its extension group's, a context
  bound, an eta-expanded method's, a local method's or given's — a value of a context function
  type read without an explicit `using`, `summon` and `implicitly`;
- an applied selection whose name an inline or contextual extension the body may find also
  declares, or one a conversion the body may find may answer when an inline conversion it may
  find, or a supplied inline method of the name, fails itself;
- a body when an inline conversion the body may find fails itself (or is an instance whose inline
  `apply` this reading does not place), or when a channel outside the sources is open where the
  method is written, unless it returns a parameter of the result's own type.

A conversion is typed where the body is written, as any call is, and the closed world (D39)
supplies every one: a conversion that is not inline, or an inline one whose own body passes, is
the same function at every call site. Scala inserts only an accessible implicit, so a conversion
`private[p]` to a package `p` is no candidate outside `p`. A qualifier `e` of an applied selection
is converted, or answered by an extension, only with an implicit of the scopes around the body
(declared, inherited, imported, exported) or of the implicit scope of `e`'s static type (Scala 3
reference, *Changes in Implicit Resolution*: the companions of its anchors and the objects those
are members of): where `e` is a path whose declared type the reading resolves — its parts, their
supplied base classes, a type parameter's upper bound — only those scopes' candidates count; any
other qualifier, or a part not resolved, keeps every visible candidate. A body's own expressions,
whose types this reading does not compute, keep every visible conversion.

Definitions written in the body are read as supplied ones, under their own frames: a local
method's or given's clauses, defaults and declared types; a local object's members; `this` and the
inherited names of an anonymous class, a local class or a local given instance (their template,
not the enclosing one). A local type whose name the reading resolves outside the body refuses.
An abstract inline declaration counts as an abstract `def` does: an inline body that may call it
reads it as a capability, as any body reads an abstract member, and an implementation is read
through its own body (owner decision, revising the earlier refusal of both). A definition nested
in a refused inline method stays refused. With those refused, the body is at
every call site the same function of its signature's values as the method's own body, up to the
universal members and reflection the model excludes (D12): boxing lets an expansion read
`getClass` and `==` on a type variable's values differently from the `def`.

**Evidence.**

- Scala 3 reference, *Inline*, *Compile-time operations*, *Macros* (\[17\]) — **inspected (full
  text)**: the inline match "is examined statically and … reduced accordingly"; `erasedValue`,
  `constValue`, `summonFrom`, `summonInline` and quoted patterns read the call site; inline methods
  are effectively final. The decision rests on this enumeration plus the channels the fixtures
  below found (an inline override of a non-inline member among them): nothing else in a body is
  resolved at the call site.
- Scala 3 draft specification, ch. 2 (\[28\]) — **inspected (full text)**: name binding and
  precedence, which the resolution follows (D11).
- **Compiler fixtures** (Scala 3.9.0):
  - a6 `b1`: an overload picked in the body is the same at every call site, and a plain `if` is a
    function of the value; `isInstanceOf[A]` is reified in an expansion; an inline given the
    body's search chose is expanded at the call site.
  - a6 `r`: a by-value parameter's constant reaches a callee's `inline match`; a passing-looking
    `wrapper` dispatches through its `summonFrom` leaf; renamed, qualified, rooted, aliased and
    relative spellings of `constValue` compile.
  - a6 `fb`: an inline extension answers `r.m(5)` beside an inapplicable member `m(String)`.
  - a6 `inventory`: the compiler, reading scala-library's TASTy, finds no inline member in the
    table's classes and objects, and `Predef`'s and `Tuple`'s inline members, which are left out.
  - review-A6 `h3`, `h7`, `h8`, `h9`: an inline override is expanded when the body selects the
    member on a parameter or `this` (5, then 9 under a call-site `given Int = 9`; the `def` twins
    stay 5), eo's `BijectionIso.modify`, `SimpleLens.transfer` and `JsoniterPrism.scanRange` shapes
    included; a6-fix1 `d1` maps the receivers re-selected (parameters, `this`, members and results
    selected on them, a lambda applied in place) and those not (a local `val`, an ascription).
  - review-A6 `h4`, `h7`: an extension group's `using` clause, a context function read as its
    result and an eta-expanded method's `using` clause each expand an inline given at the call
    site. `h5`: `isInstanceOf[Array[A]]` reads `A`. `h10`, a6-fix1b `ex2`: an inline conversion
    reached through a `given … with`, an alias of `Conversion`, an export or a given local to the
    body is expanded. `h1`: boxing (`getClass`, `NaN == NaN`) differs between expansion and `def`.
  - review-A6 r2 (`l4`–`l6`, `m1`–`m3`, `o1`, `a1`, `c2`; a6-fix2 `locals`, `clauses`,
    `templates`, `reified`, `aliases`): a local method's `using` clause, context bound or default,
    the inline `apply` a further clause selects on a callee's result, `Outer.this.f` re-selected on
    a subclass, `this.h` in an anonymous or local class or a local given, `classOf[Array[A]]`, and
    a conversion nine aliases deep each make an expansion read the call site (5, then 9 under a
    call-site `given Int = 9`; `intArr[Int]` true, `intArr[String]` false); the `def` twins do not.
  - a6-land `dip`: an expansion of `t.f` calls the static type's implementation of a deferred
    inline `f` (`g(new A)` true, `g(new B)` false; `g` at a `T` does not compile).
  - review-A6 `h11`–`h14`, a6-fix1 `t1`, a6-fix1b `dv`: a transparent expansion is typed by what
    it returns — a sealed case, `None`, a literal, an opaque representation, a returned stable
    path's singleton type — and an overload tells it from an equal-valued body ascribed to the
    declared result.
- Scala 3 reference, *Implicit Conversions* — **classical** (not re-verified here): a conversion is
  inserted only where an expression's type does not conform, a member is missing, or no member
  applies.
- Related work: Stucki, Biboudis & Odersky 2018; Stucki, Brachthäuser & Odersky 2021 (GPCE) —
  **metadata-verified**, background.
- **Executable**: `InlineMethodsSpec`, `InlineScopesSpec`, `TransparentInlineSpec`,
  `SyntaxFeaturesSpec`.

**Boundary.** An extension or conversion in the companion of a type outside the sources, or brought
by a named import of a declaration outside them, selected because a member does not apply, is not
seen (the view rule's assumption, D20); likewise a given of the sources whose class is declared
outside them is not read as a conversion (its parents are not supplied). Which conversions and
extensions the body may find is over-approximated, never narrowed: every scope Scala may search,
an import matched by its last segment; aliases and export chains are followed to their end. An
extension that restates the selected member (an invariant base class receiver, the same parameter
names and resolved types under the qualifier's final class's base type, or a receiver no base class
of it) is never taken, since the member applies wherever it would (eo's `Getter.andThen`,
`BijectionIso.andThen`, `Review.andThen`: 1); anything less keeps the fallback reason. The
library table is what the check needs, not the library:
any member outside it refuses. Untyped lambda parameters, pattern variables and most library
results are not typed, so a selection on them refuses. A local class, trait or enum is read through its
companion and its own body only, so a value of its type has no members the reading resolves. A
passing body is then read by the solver exactly as its `def` twin, polymorphic members (D24) and
sum elimination (D25) included; a deferred inline member of a receiver or record is a capability
of an inline target, as an abstract member is (a body that is not inline still has no term of it,
D24). See [syntax-features.md](syntax-features.md).

## D28. Rows beyond defs, each measured by the rule its translation has

**Decision.** The report has rows for the definitions beyond defs and concrete classes' primary
constructors: given aliases (a method of their clauses, nullary without one) and abstract givens,
member `val`s and `var`s (a nullary signature of their type; a `final` or `inline` `val` of a
literal keeps its literal type, a `var` never does), members of anonymous classes (a final
class's), definitions in a body's nested blocks, function literals, case clauses and
comprehensions, parameterized enum cases (the case class constructor, typed where the enum is
declared, with the enum's binders when the case has none of its own, `extends` clause or not), the
primary constructors of abstract classes, traits with a parameter clause and conditional
structural givens (a class's), secondary constructors (a choice of the primary constructor's
arguments over their parameters: the tuple of the primary's parameter types, typed in the class's
enclosing scope with its binders, never a value of the class) and the definitions of their bodies
(local definitions of a method of the class, under `<init#k>`). A value or given that implements a
declaration of its name is that declaration, never its own slot — an anonymous given under the
name 3.9.0 synthesizes from its type (`given A = a` is `given_A`); of a definition of several
names each is a row and the others are its slots. A nested definition's environment is its typing
context, the parameters and captures of every enclosing owner (D19's Γ), each ranging over its
type — the signature lambda lifting gives it. Control flow narrows nothing: a branch, a loop, a
short-circuit operand, a case arm, a guard, a by-name argument or a dead branch leaves the
admissible inputs those of the types, as a method's callers leave its parameters' (a reading by
the inputs that reach a definition at run time would narrow a local def's own parameters by its
call sites, and a function literal's or by-name argument's by code the reading does not have). A
case clause's or comprehension's binders, given binders included, are refused, since one may alias
a value the scope has; a pattern that may add a type equation (a constructor, typed or object
pattern with a type parameter of an enclosing method or class in scope) refuses every count but a
one-valued result's; a context-function literal's parameters are contextual parameters of its
scope and bind its clause once (the definition's result does not wrap the literal again).
Inherited concrete code reading a parent's constructor parameter no public field exposes is
refused unless the template passes the argument itself and no member, field or import of its body
rebinds a name the argument reads (`open class with hidden constructor state: P.h`). An `inline
given` alias is read by D27 as an inline method, and a row in a scope of an inline method's body
shares that method's verdict (counted when it passes, refused as `definition inside an inline
method` when it does not). Local block values and local nullary given aliases, singleton enum
cases and unconditional structural givens are no rows. Details:
[definition inventory](definition-inventory.md).

**Evidence.**

- \[24\] Scala 3 reference, *Translation of Enums and ADTs* (**Inspected (full text) —
  bibliographer**, T5): rules 6 and 9, a class case expands "analogous to a final case class in
  `E`'s companion object", with the enum's type parameters when it has none and no `extends`;
  rule 7, with an explicit `extends` it takes them only if "at least one of the parameters `Ts` is
  mentioned in a parameter type in `<value-params>` or in a type argument in `<parents>`".
- Parreaux, Boruch-Gruszecki & Giarrusso 2019, *Towards improved GADT reasoning in Scala*, Scala
  Symposium (**Metadata-verified**; the abstract reports both scalac and Dotty have GADT-related
  type soundness holes): why a pattern that may refine a type is refused, not reasoned about.
  Kennedy & Russo 2005, *Generalized algebraic data types and object-oriented programming*, OOPSLA
  (**Metadata-verified**; the abstract reports GADTs are expressible through generics and
  subclassing) is background: reading a subclass that fixes its parent's type argument as a GADT
  case is this register's inference, not the abstract's claim.
- \[30\] Scala 3 draft specification, ch. 5 (**Inspected (full text) — bibliographer**, T5):
  *Constructor Definitions* — a secondary constructor's "signature and the self constructor
  invocation … are type-checked and evaluated in the scope which is in effect at the point of the
  enclosing class definition, augmented by any type parameters of the enclosing class", and the
  invocation "must construct a generic instance of the class"; *Overriding* — "a stable member can
  only be overridden by a stable member"; *`final`* — "constant value definitions … require an
  explicit `final` modifier".
- **Compiler fixtures** (Scala 3.9.0, this session; `$S/probes/b5`): a given alias is a `given def`
  (a `lazy given val` when nullary), a conditional structural given a `given class` with a `given
  def` building it, an unconditional one a module (`-Xprint:typer`); an enum case finds neither the
  enum's members (E006) nor its companion's ("illegal reference … from enum case") and needs a
  parameter list (E004); a `val` implements a parameterless `def`; an untyped `val` overriding a
  context-function `val` sees its binder; `final var x = 3` is an `Int` (E007 against `def useX:
  3 = x`) while `final val` and `final lazy val` keep `3`; `case D(y: A) extends E[Int]` of `enum
  E[A, B]` prints `final case class D[A, B](y: A)` (`$S/probes/b5-fix1`, `-Vprint:typer`); a
  secondary constructor's body reads the class's members and parameters; a context-function
  literal under a context-function result is one closure (no second wrapper); an anonymous given's
  name is `given_` and its type's names (`given_A`, `given_Ord_List`, `given_Int_String`,
  `given_String_to_Int`, `given_&_Ord_Show`, …, `$S/probes/b5-fix2/gname`), and `new C[A](x,
  y).given_A` selects one; matching `case TInt()` makes a method's or a class's type parameter `A`
  Int in the arm (`def h: A = 42` compiles), while abstract type members, path-dependent types,
  literal, binder and wildcard patterns refine nothing (`$S/probes/b5-fix2/refine`).
- **Executable**: `DefinitionInventorySpec`; `MethodAnalysisSpec.abstractCase`,
  `ReportHygieneSpec.operatorNames` (abstract-class constructor rows).

**Boundary.** Not a counting rule: each row is measured by the existing rules, and its `?` keeps
their reasons. Control flow is not part of the reading by decision (the typing context is); a
binder of a pattern is refused rather than read, and so is any count under a pattern that may
refine a type (whether the pattern's class pins an argument is not read); an intermediate level
passing a parent's hidden argument is not followed; definitions in default arguments and in a
named template's parent arguments are not indexed.

## D29. A stored read binds what templates, packages and enum companions pass on

**Decision.** The stored-value estimate, and the constructor slots it lends the method side, bind
three kinds of members D11 did not read. (1) *Template members*: inside the body of a class, trait,
enum or object, a bare type name or a path's first name binds the template's members — those its
parents and its self type pass on — before any enclosing package member, import or root import; a
declaration at or inside the body keeps its meaning (an enclosing class, object, method or block
declaring it too is E049, so a valid program never asks); parent clauses and primary constructor
parameters read outside the body. A member is read in the body of the supplied parent that declares
it, through every supplied ancestor — a body member, or a `val` or `var` parameter (a case class's
first clause included), which is a member too; a plain parameter is not. A `private` one is not
inherited, a qualified-private one (`private[q]`) is — Scala binds the name to it from anywhere and
rejects an access from outside `q` (E173). An abstract one is the implementing instance's (`member
T`); an opaque one is outside its scope (D30); one under the `extends` clause's type arguments is
refused (`inherited type member not resolved: X (type arguments of P)`). A parent's export clauses
define alias members it passes on like its declarations; they are not followed, so a name one may
define — a selector that names it in a namespace that has one, a wildcard whose namespace binds it,
or a namespace not read — is refused (`inherited type member not resolved: Boolean (exports of B)`).
A parent outside the sources may declare any name, so where the meaning would otherwise come from a
package, an import or the root imports the name is refused (`inherited type member not resolved: X
(unsupplied parent P)`; for a path's first name, a term, `inherited term member not resolved: x
(…)`), unless the stdlib model knows the parent declares no type member (D16), spelled where no
source binds it — the method side's guard (`InheritedTypeMembers`), with the same reasons, so a lent
slot refuses where it does (before D29 the estimate read the builtin there). (2) *Package members*:
a package holds what its package object's parents pass on and what its files' top-level exports
define; a read inside the package that finds a name nowhere nearer binds such a member over the root
imports. An export over an object the package defines, written where its file imports nothing before
it, is followed, for a type name and a path's first name alike (a renamed selector binds only its
new name, a wildcard no given, a private member is refused); a parent or any other export may bind
any name the files do not define, which is refused (`X (may be a member of package lib, an inherited
member not followed)`, or `of the empty package`), a path's first name included. (3) *Enum
companions*: `enum E`'s cases are members of its companion — an explicit `object E` or the one Scala
synthesizes — a class case as a type and a term, a singleton case as a term. Through an explicit
companion they bind for `import E.*`, `export E.*`, `E.A` and a bare name in `object E`'s body; with
or without one, the cases' parameters read in the companion (its members, its parents' and the
cases, under the case's own copy of the enum's type parameters), and the enum's own body imports the
cases by name. A case class is not read as a type of its own, so each such name is refused (`enum
case type not read: p.E.A`), as the method side refuses it. Details: [stored-value
names](stored-value-names.md).

**Evidence.**

- \[28\] Scala 3 draft specification, ch. 2 (**Inspected (full text)** (verifier)): the precedence
  list puts "same-unit local, inherited or package-clause definitions" first, then explicit
  imports, wildcard imports, and other-unit package members and compiler-supplied imports; "A
  binding in some inner scope shadows bindings of lower precedence in the same scope as well as
  bindings of the same or lower precedence in outer scopes."
- \[24\] Scala 3 reference, *Translation of Enums and ADTs* (**Inspected (full text) —
  bibliographer**): a class case expands "analogous to a final case class in `E`'s companion
  object" — the companion holds the cases. That the enum's body imports them by name is the
  compiler's (fixtures below), not the reference's.
- **Compiler fixtures** (Scala 3.9.0, this session: `$S/z1b/fx/inh`, `$S/z1b/fx/chk`,
  `$S/z1b/fx/spec`; the integrated review's `probes/stored/{inh,self,inst2,inhterm,unsup,t3,en,
  en2,en3,en4,pkgobj,pkgobj2}` and `probes/indep-*`): an inherited alias binds over a same-unit
  package member and the root imports, and an outer object's definition beside it is E049 ("It is
  both defined in object Outer and inherited subsequently in object O"); a nested object's own
  definition is nearer than what its owner inherits; inherited `val p: H.type` and `object Inner`
  bind a path's first name over the package's objects; a primary constructor's parameter reads
  `scala.Boolean` while the body reads the inherited alias; a self type's alias binds in the body;
  `import G.*` binds no type for `case Boolean`; `package object lib extends basep.Base` and
  another file's `export Src.*` (or `export Src.{Boolean as Flag}`) bind `Boolean` (`Flag`) inside
  the package as `Int`; `import E.*` over an explicit companion binds the case class `E.A`. Fix
  round (`$S/probes/review-Z1b/{a,a2,en9,en10,en5n,x6,x8}`, `$S/z1bfix/{qp,en,t3,t4}`):
  `private[pa] type Boolean = Int` binds `Boolean` in `O extends Base` (`O.U(42)`), and from outside
  `pa` it is E173, not the builtin; `case C(a: A)` builds `E.C(E.A(true))` beside another file's
  `type A = Unit`, with or without an explicit companion, and `e.In(())` is rejected for an
  enum-body `In(a: A)`; a companion's own alias is "illegal reference to type T from enum case",
  its parent's is read (`G.K(true)`); a followed `export Src.*` passes on no `scala`, so
  `scala.Boolean` stays the root's. Second fix round (`$S/z1bfix2/fx/{ex,vp}`, the review's
  `probes/review-Z1b/r2/{inh,inh2}`): a parent's `export Src.Boolean`, `export Src.q` and `export
  Src.*` bind `Boolean`, `q` and `Flag` in a subclass's body (`O.U(42).b: Int`, `OW.UW(42L).f:
  Long`), a self type's export alike; a `val` parameter and a case class's parameter are inherited
  (`OP.UP(42).x: Int`), a plain or private one is not (`OC.UC(()).x: Unit`).
- **Executable**: `StoredScopesSpec` (positive and adversarial; `inheritedTerms` and
  `inheritedExports` for the parameters and exports), with the expectations it moved:
  `GroundEnvironmentSpec.shadowedPosition`, `constructorScope`, `symbolicDisagreement` (the lent
  slot now agrees with the method side: 2^64, 2, 2^(2^16)), `StdlibModelSpec.constructorExempt`
  (`In(b: Boolean)` in `trait Outer extends lib.Base`: refused, not 2), and the method side's
  term-head wording (`NameLookupSoundnessSpec.unsuppliedObjectPath`,
  `UnsuppliedWildcardSpec`: `inherited term member not resolved: c`).

**Boundary.** Not a counting rule: it decides which declaration a name denotes. Members of a
parent applied to type arguments, export clauses that read imports or name anything but a plain
object of the package, package objects' parents and a template's inherited members met past an
unread parent where a method parameter or block outside every template declares the name are
refused, never followed; class cases are never read as types; a companion's own members, which
Scala forbids a case to name, are read as the companion's; a parent's export clauses are refused,
never followed. No eo 1.0.0 count moves; four eo rows keep `?` with a reason naming the unread
parent.

## D30. An opaque alias is its representation only where it is visible; a value row is min(1, |T|)

*Superseded in part by D35:* a reference outside the scope is one value, no longer refused.

**Decision.** (1) A reference to an opaque alias reads its representation's size where the
representation is visible: inside the template that declares it, or, for a top-level alias, inside
its file's synthetic package object — that package block's top-level definitions (a class nested in
one of them included), its implicit classes and implicit objects and its `given` instances, which
Scala 3 wraps there too, and the companion objects of its top-level types; not its other top-level
classes, traits, enums or objects, nor another file. Anywhere else, and for a type rebuilt with no
source position, the reference is refused (`opaque type read outside its scope: p.Ids.Id`), and so
is a constructor slot lent to the method side, whose own reading follows the same scope; the opaque
type's own row still reads its representation. This replaces the reading of an opaque reference as
one value, which made `U(id: Ids.Id)` 1. (2) A stored value row is `min(1, |T|)` for its declared
type `T`, read as inhabitation: 0 when `T` is empty, 1 when it has a value or none is declared,
`T`'s reasons when neither is decided; the initializer is never read. A function type has a value
when its result type has one or a parameter type is empty (the absurd function), although the
algebra counts it 0; a witness — a standard-library type the library gives a value of, an enum's
parameterless case, a sealed family's module that is a member of every instantiation — decides the
rest of the type. A value of a class, enum or module exists only if one is built, which runs every
ancestor's primary constructor (D26, CROSSCHECK X6): the arguments of its `extends` clauses, the
defaults its parents fall back to, the strict members and statements of its body, and an enum's
companion for its singleton cases. Where one of them throws, where the constructor is private or
protected, where implicit search supplies a parent's context argument, or where a parent is not
read, the reading is refused (`value not decided: K holds K, whose construction evaluates a throw`)
— never the 1 the algebra's count of what `K` stores gives, nor the 0 that would follow for `K =>
Nothing`. A singleton type `p.type` holds what `p` denotes: an object with no parents whose body
throws nothing is built, a standard-library object is; a value's singleton and any other path are
refused (`value not decided: n.type, a value's singleton type`), and so is a class holding a
singleton type. Adopted on a 2/3 vote of the integrated review: the dissent held that a value row is
documented as one inhabitant with its declared type unmeasured. Details: [type arithmetic
§10](../type-arithmetic.md), [pure code §2](pure-code.md).

**Evidence.**

- \[21\] Scala 3 reference, *Opaque Type Aliases* and *More Details* (**Inspected (full text)**
  (verifier)): the alias is transparent inside its defining scope and abstract with its bounds
  outside it; the scope is the whole defining scope, not a companion.
- **Compiler fixtures** (Scala 3.9.0, `$S/z1b/fx/opq`; the integrated review's
  `probes/stored/opq`, `probes/indep-opq`, `probes/model-opq`): a top-level `def`, a class nested
  in one, `object Flag` beside `opaque type Flag` and `object Al` beside `type Al` read `Flag` as
  `Boolean`; a top-level class, another object, another file and a subclass of the declaring trait
  reject it (E007). Fix round (`$S/z1bfix/op`, the review's `review-z1b-quality/p2`): `implicit
  class R(f: Flag)`, `implicit object IO` and a `given … with` body read `f` as `Boolean`; a plain
  `class R(f: Flag)` does not. Scala 3's desugaring wraps "implicit classes and objects" into the
  package object (compiler behaviour, not a reference text). Second fix round
  (`$S/z1bfix2/fx/{ub,sg}`, the review's `probes/review-Z1b/r2/x6`): `final case class K() extends
  Base(???)`, `case object E extends V(???)`, `enum En extends Base(???) { case X }`, `final class P
  private (i: Int)` and `val sn: n.type = n` for `val n: Nothing` all compile; none of them
  builds a value (`???` throws, `P` has no caller, `n` holds none).
- **Register D26** (this file): building an instance runs each level's parent arguments, strict
  initializers and statements, and one that only throws leaves no instance — the method side's
  `Construction`, read here by `Builds` with every simple name taken for the root imports'
  (refused, never read as empty).
- **Classical** (set theory): a value of type `T` exists exactly when `T` is inhabited; `B^∅` has
  one element (the empty function), so the stored algebra's `0` for a function from an empty
  domain ([type arithmetic](../type-arithmetic.md) §3: it never runs) is a capacity, not
  emptiness. Its intersection is the smaller side, an upper bound (`Int & String` is empty, its
  bound 2^32), and a refinement is the type it refines. Elsewhere its numbers are counts or
  rounded-up counts of at least 128: its 0 is emptiness and any other number inhabitation (sums,
  products, powers and least fixed points of inhabited parts are inhabited).
- **Executable**: `StoredScopesSpec.opaqueMembers`, `opaqueTopLevel`, `opaqueLending`;
  `ValueRowsSpec.valueReasons`, `sumDropsEmptySummand`; `OpaqueTypeScopeSpec` (unchanged); moved
  expectations `ReportSpec.crossSourceOpaque` and `CardinalitySpec` (a reference from another file
  or a top-level class: refused, not one value). Since pruned to what the corpora exercise: a
  value row is 1 only for a nonzero capacity read with no reason; every other reading is refused
  (no witnesses, no 0).

**Boundary.** A value row reads inhabitation, not the algebra's capacity: where the algebra's 0 for
a type rests on a function from an empty domain met inside a definition (`P(f: Nothing => Int)`),
the row is refused (`value not decided: P`), and so is a number read through an intersection or
refinement (`value not decided: Int & String`), a template whose construction is not read, or a
singleton type. A value of an unresolved type keeps its reasons (`Box[Json]` for a case class
`Box`), and an opaque alias read outside its scope is one value here too (D35). A construction step that
does not throw is read as yielding a value of its type (concrete code obeys the model,
D-env-concrete); one that throws only if a simple name is the root imports' is refused, whatever
binds the name. An opaque alias inside a template is not visible in a subclass's body; a reference
rebuilt without a position (a substituted slot) is one value even inside the scope (D35).

## D31. A count refused only by possible views is read between two bounds

**Decision.** C6's views see every way the receiver's code may run the measured declaration: a
local definition is mentioned by the name its block binds to it (a name bound again is another
binding, and its own body is the choice being counted), so the enclosing member whose body runs it
at a definite position is a definite view (`C.m.h`: 16, not 2^32) — a named argument of an eager
clause too — and one that mentions it anywhere this reading enters loosely (a match arm, a `try`,
an interpolation, a partial function, a loop) a possible view; constructing a class (`new C(…)`,
`extends C(…)`, a secondary constructor's `this(…)`, a creator application `C(…)` or `C.apply(…)`
where C names a supplied class, `copy(…)`) may run the secondary constructors, template statements
and strict initializers of every receiver level among the class's supplied base classes — a
subclass of the receiver's own included — a possible mention of what they run; a level of the
class outside the receiver's that runs code of its own (statements, strict initializers, parent
arguments, secondary constructors) may run any member it inherits, the measured one too (`new
Sub().x` with `class Sub extends C { val x = h(true) }`); a class outside the sources extends
none; one this reading cannot follow (an unread parent, `q.copy(…)` on another value, a name a
block import may rebind) may be any. Implicit search may select the measured declaration itself
when it is a given or implicit member, abstract or not (`given g: Boolean` beside `def other =
summon[Boolean]`: `?`, not 4). A body that only returns one of its parameters, declared with
exactly its result type, searches nothing, nor does a literal of the declared result type (or of
`Any`, `Matchable`, `AnyVal`/`AnyRef` above it); a literal at another type may be converted
(`def other: Boolean = 5` is `conv(5)`). Where the only reasons left are possible views P, two
more readings bound the count R of recursion-free implementations: P bound as free capabilities
(upper) and P left out (lower). If the upper reading is 0 the count is 0; otherwise the reasons
stay (the lower reading's 1 was dropped in the corpus-coverage prune).

**Derivation.** Every recursion-free implementation is a term over the environment that, in every
admissible receiver, never re-enters the declaration; reading each member of P as a free
capability of its type admits every behavior its default or an override has, so every such term is
among the upper reading's terms and is told apart from another no less finely: R ≤ upper. A term of
the lower reading uses no member of P (and no definite view), so it re-enters nothing: lower ≤ R.
Hence upper = 0 gives R = 0, and upper = 1 with lower ≥ 1 gives R = 1. One proviso: a member of P
whose type is empty makes the upper reading vacuous (every goal has its one absurd answer), so it
is no bound then — but no implementation can use that member either (it never returns), so R =
lower, and both cases stand (a vacuous reading is 1, never 0). Agreeing bounds past 1 fix R as
well once the upper reading is known not to be vacuous; the rule decides only 0 and 1 as a
deliberately narrow choice, not because agreeing bounds could miss the count. A member of P whose
binding the upper reading refuses refuses the row.

**Evidence.**

- First principles (the derivation above), over D4's recursion-free terms and D20's free
  capabilities.
- **Compiler fixtures** (session fixtures, Scala 3.9.0, not retained): a run overflows the stack when
  `C.m.h` delegates to `other`, and one for `given g` read through `summon`; one with a constructor
  re-entering its helper does not terminate; twin programs with the possible view left out (lower)
  and bound to an arbitrary function (upper) print the two bounds.
- **Executable**: `ReentrySpec`, `TargetViewsSpec`, `CapabilitySpec.openSteps` (an open step
  distinguishes every iterate).

**Boundary.** A lambda applied in place, a local definition calling the declaration, an
η-expansion and a constructor run are possible views, not definite ones. The sandwich decides
only 0 (`ReentrySpec`'s `E3.to`, with both bounds 4, stays `?` by that choice), and never
substitutes a reachability test for a view (`Opt.to` stays `?` although every instance the sources
declare fixes the dependent member away from the goal: the closed-world reading is unsound for an
open class, X5). eo's `ComposedGetOptionLaws.getOptionInner` is 1.

## D32. An open record's unbounded abstract member opens as the witness of each value's package

**Decision.** A capability record (C0–C5) whose template's linearization leaves an abstract type
member `X` — unbounded, first order, defined by no level — no longer refuses: the record's members
are read over a witness named by the member, and the record is the package ∃X. members-over-X
(`Shape.Existential`), which the solver opens per supplied value with a witness fresh to that
value, exactly as it opens a wildcard input (ExistentialInputs). Two supplied values never share a
witness, and one value's members share theirs. The witness itself is unobservable: a record whose
members return only the witness contributes no input factor of its own. A *goal* of the record is a
result construction — the implementation would pick the witness — and stays at the existential
result boundary (`existential wildcard result construction and repackaging requires witness-choice
analysis`). Bounded or higher-kinded members keep
`capability record with abstract type member not read: C.X`, and so does any member the measured
signature names through a path (`p.X`, `T#X`): the projection reads as an atom keyed by the path
(TypeMembers), and the opened witness and that atom would have to be one — a sharing this reading
does not yet agree on.

**Derivation.** A value of C is the existential package ∃X. members-over-X (Mitchell–Plotkin 1988;
Cardelli–Leray dot notation, both already the register's for parameter paths). Eliminating a
*supplied* package is parametric in the witness: the body cannot inspect it, choose it, or compare
two witnesses for equality, which is precisely a fresh rigid atom per value — the opening's
soundness is the same argument that makes the wildcard fragment's opening sound. The *goal*
direction packages a witness (the anonymous subclass the count realizes picks X), which is result
construction, outside the supported fragment (existential-inputs boundary 1). The observable
content of an input record is its members' results over the witness: `sub(c: Sub): Boolean` with
`Sub extends Base { def b: Int; type X }` is Boolean^Int = 2^(2^32) — implementations are the
functions from `c.b`'s value to the result, and the witness adds nothing.

**Evidence.**

- **Executable**: `CapabilitySpec.existentialRecords` (`run(p: Pair): Boolean` = 4 — the four
  Boolean functions of consume's behavior at the value the body can supply; `solo(p: Source)` = 2 —
  a record of one witness-returning member leaves the literals alone; `both(p, q)` = 2^4 — two
  independent packages, never crossed; a goal and a `p.X` mention refused),
  `CapabilitySpec.existentials` (the `Opt` goal at the result boundary),
  `TypeMemberBoundarySpec.typesOnly` (`sub` = 2^(2^32)), `TypeMemberScopeSpec.boundedTemplates`
  (bounded members keep the refusal).

**Boundary.** A signature that names the member through a path stays refused; so do bounded and
higher-kinded members; witnesses of two values never unify. Lifting the path case needs the two
readings to agree on one atom for `p.X` — the next slice of the same feature.

## D33. A listed dependency namespace binds only the members its artifacts declare

**Decision.** The wildcard-shadow guard (builtins D11's rank rules) treats a wildcard over a
namespace no source supplies as binding any name. For a namespace whose surface the artifacts
give, the guard instead reads its member list: a wildcard over it binds exactly the listed names,
so a root spelling that is not one survives (`import cats.syntax.functor.*` beside `def f(i: Int,
b: Boolean): Boolean` counts — the previous run refused it with `builtin name Unit may be shadowed
through a reference not supplied`), a member the namespace declares is refused as the import it is
(`import _root_.kyo.*` beside `def f(s: Stream): Boolean` names `kyo.Stream`), and a source name's
doubt chain through the wildcard dissolves when no link declares the name (`Fst (may be imported by
compose.*: compose may be imported by cats.instances.function.*)` reads past the doubt). The
listed namespaces are the corpus's: cats-core 2.13.0's syntax objects (`functor`, `either`,
`flatMap`, `coflatMap`, `applicative`) and its `instances.function`, and kyo 1.0.0-RC7's `kyo`
package. A namespace no artifact covers keeps the blanket refusal (`hearth.fp.instances`, whose
package object's members the class entries cannot complete).

**Derivation.** Shadowing is a name question, and the artifacts answer it completely: a wildcard
binds the namespace's own and inherited members, and the sources jars (cats-core) and the class
entries plus sources (kyo — whose package has no package object or `package.tasty`, so its
top-level surface is wholly enumerable) enumerate them. The listed sets are supersets checked
against the analyzer's vocabulary: no member of any listed namespace is a root spelling, so a root
spelling under such a wildcard keeps the model's reading — which is the same reading the compiler
gives, since nothing in the namespace can shadow it. The recovery this unlocks is the one the
integrated review deferred to "the dependency name index": eo's `Optic.id` and `ModifyF.map.map`
count 1 — `id`'s `BijectionIso` fixes `X = Nothing` and its two `A => A` fields are the identity
each (parametricity over the rigid binder), and `map`'s continuation is uniquely `f ∘ inner` with
`fa.modifier._1` the only `Fst[X]` — together with `ModifyF.<init>`, its two `coerceToModify`
members and `Modify.from`'s pair.

**Evidence.**

- **Inspected**: cats-core 2.13.0 sources jar (`cats/syntax/package.scala` — objects only;
  `functor.scala`, `either.scala`, `flatMap.scala`, `coflatMap.scala`, `applicative.scala` —
  methods and the one `private[syntax]` nested class; `Functor.scala`'s `ToFunctorOps` — no type
  members); kyo 1.0.0-RC7 sources jars (prelude, kernel, data, schema, schema-json) and the class
  entries of all ten cached artifacts — 397 types, 82 terms, none a root spelling.
- **Executable**: `StdlibModelSpec.dependencyNamespaces` (the root spellings survive
  `cats.syntax.functor.*` and `_root_.kyo.*` at Boolean^(2^33); `kyo.Stream` refused as the
  import it is), `StdlibModelSpec.nowhere` (the kyo wildcard's doubt names `kyo.Frame`).

**Boundary.** A namespace the artifacts do not cover keeps the blanket refusal. The listed sets
are the artifacts': a dependency upgrade re-checks them (the table names the versions). Terms and
types are listed separately — a term name the namespace declares does not shadow the same spelling
as a type.

## D34. A capability member's higher-kinded binder is a rigid constructor, gated on the result

**Decision.** A capability member's own higher-kinded type parameter (`Optic.andThen[C, IB,
G[_, _]]`) no longer refuses the record that holds it: the binder is read as the rigid constructor
`TypeOperators.rigid` binds, and the member's type applies it uninterpreted — the same reading the
method side gives its own higher-kinded binders, and the same one the compiler's typechecker
gives (an abstract constructor applied to arguments is unreducible but well-kinded). The member
stays polymorphic in its first-order binders (PB0–PB4 unchanged: the rigid applications sit
outside the atom-based Yoneda and matching, so neither reaches them). A member whose *result*
instantiates its own binder refuses — `polymorphic capability binder: G (the result instantiates
the binder)` — because there the caller instantiates the binder, and the solver's terms from such
a member never search the instantiation: counting without the search could report a 0 where an
andThen-composition is the only implementation.

**Derivation.** As an input, the member's terms exist only where a producer path's output meets a
goal. The binder stands in the member's parameters — the caller fills them from its own
environment, which holds no value of a rigid binder's application — so every path is either
irrelevant (its result reaches no goal: the result is an uninterpreted application over atoms and
constructors, which no eliminator of the model takes apart) or refused where the goal matches and
the binder is undetermined (PB4). Both are exact: an implementation that calls the member must
choose the binder's instantiation, and the choice's evidence would have to come from the same
fixed environment — the enumeration of those choices is the open boundary, not a silent
assumption. As a goal (the record's anonymous-subclass realization), the override is itself
universally quantified in the binder, so the rigid reading is the parametric one and exact.

**Evidence.**

- **Executable**: `KindedIdentitySpec.kindOverloads` (the un-overridden `K[_, _]` overload refuses
  with the result-instantiation reason), `PolymorphicMemberSpec.boundedAndKinded` (`Kinded.m[G[_]]
  (x: G[Int]): G[Int]` — the result instantiates G), `TypeMemberSpec.boundedMember` (`def g(p: H,
  x: p.F[Int]): p.F[Int]` = 1 — the higher-kinded abstract member `p.F` reads as the path's rigid
  constructor applied at its arity, and the only `p.F[Int]` value the environment builds is `x`).
- The cats-eo run: 291 rows reason-only, zero counts moved — the corpus's `G[_, _]` blocker
  (297 rows) dissolves into the per-member reasons behind it.

**Boundary.** A member whose result instantiates the binder stays refused; so do bounded,
context-bounded and view-binded binders. The instantiation search — enumerating the constructors
the environment can witness for the binder (`witness-kind analysis`) — is the feature's open half.


## D35. Public constructors build the receiver; an opaque type outside its scope is one value

**Decision.** Two modelling conventions the user decided (2026-11-05; [v1.md §2, "Modelling
conventions"](../plans/v1.md)). (B) The values a receiver's fields hold together — and a final
class's record, a sealed summand — are what its public constructors build: the largest space any
one of them builds, not their sum. A constructor is public where the measured declaration may
call it (`private[p]` inside `p`, `private` inside its class or companion), else ignored; a
restriction on the class or trait itself, on a template around it, or a class local to a block is
disregarded (such a class has no use site outside its scope), so its constructors are public. A
public primary builds every field value; otherwise a public secondary builds at most
`min(|its parameters|, |the fields|)`, read where both are closed, and the receiver is one value of
the largest such space (`ConstrainedReceivers`). No public constructor refuses (`receiver has no
public constructor: C`); a public secondary over a non-closed space refuses (`receiver space of its
public secondary constructors not compared: C`). (F) Outside its scope an opaque type counts 1: the
method side reads it as `Unit` (`OpaqueTypes.single`) and keys it as its own declaration
(`NominalKeys`), the stored side as `UnitSize` (`Counter.hidden`). This supersedes D30 (1)'s
refusal of a reference outside the scope, and the refusals `receiver fields constrained by …`,
`sealed subtype (constructor) not visible at use site` and `opaque representation is not visible
at use site`.

**Derivation.** (B) Every secondary constructor ends in a call of the primary, so what a public
secondary builds is a subset of the primary's space: with a public primary the largest space is the
fields' own, the reading before this rule. A secondary is a pure function of its arguments, so it
builds at most as many receivers as it has arguments, and at most the fields' space; a method of a
receiver with closed fields is a function of the receiver's value alone (a closed value is
eliminated by a uniform split, `ClosedSpaces`), so binding one value of that space
(`Shape.Ground`) in place of the fields counts it. (F) is a convention, not a derivation: two
values of an opaque type are identified, and an upper bound (`opaque type T[A] <: A`) is not read.

**Evidence.**

- **Executable**: `ReceiverConstructorsSpec` (two public constructors: 4; a private primary and a
  public `this(u: Unit)`: 2; two public secondaries building 1 and 2 of 4: 4, not 16; a
  `private[rc]` class and trait: 4; a factory-only class refused; a private sealed leaf summed: 8);
  `SequenceEliminationSpec.scopedReceivers` (non-public and local classes read as public: 0, ω);
  `OpaqueTypeScopeSpec.oneValue` (`f(a: Id, b: Id): Id` 1, `g(a: Id): Boolean` 2);
  `StoredScopesSpec.opaqueMembers`, `opaqueTopLevel`, `opaqueLending`; `ReportSpec.crossSourceOpaque`.

**Boundary.** Companion factories are not read, so a class built only by one stays `?`. A public
secondary's space is compared only where it and the fields are closed. (F) gives counts the
representation would not: `def get[A](t: Box[A]): A` outside `Box`'s scope is 0 even where
`Box`'s companion offers an unwrapping member, as for any member of another object that is not in
the environment.

## D36. Key equality is lawful, and a single-pass read is replayable

**Decision.** Two idealizations the owner adopted (v1 §2). (1) The equality and hashing a `Map`
or `Set` uses on its keys is lawful — structural, consistent with the value identity the model
gives the key type — so a `Map[K, V]` is its abstract value, a finite partial function, and a
`Set[E]` a finite subset; iteration order and hash structure are not observed. Over closed
arguments the value space is `(|V| + 1)^|K|` (`2^|E|`), ω over an infinite key, exactly the
closed space of `K => 1 + V` (`E => Boolean`) under the saturating power of D2, so the method
side reads it as that function (`StdlibModel`, reading `Tabulated`); the stored-value side
already read the same formulas (`Counter`). An open argument is refused: no parametric program
has the key equality. A top type among the keys takes the container's place (D38): one value in
an input, where only the set's size could tell two sets apart and the type fixes none, ω in an
output. (2) Reading a single-pass `Iterator`, `IterableOnce`, `java.util.Iterator`,
`ByteBuffer` or `InputStream` changes it, an effect D26 excludes, so each is the sequence it
yields: `Σₙ Eⁿ`, bytes for the last two (a buffer's position and capacity are more state of one
countable value). An infinite iterator changes no count, by D16's continuity argument for `Seq`.

**Evidence.** **Executable**: `StdlibModelSpec.singlePass` (`List[A] => Iterator[A]` ω, an
`Iterator[Nothing]` 1, `ByteBuffer => Boolean` ω), `.keyed` (`Set[1 | 2]` 4, `(1 | 2) =>
Set[1 | 2]` 16, `Map[1 | 2, Boolean]` 9, `Map[1 | 2, 1 | 2] => Boolean` 2^9, `Set[Nothing] =>
Boolean` 2, `Map[String, Nothing]` 1, `F[Set[String]] => F[List[String]]` 0, `Set[Boolean]` 4,
an enum-keyed `Map` 9, `Set[Any]` / `Map[Any, Boolean]` by place), `.openKeys`;
`KindedIdentitySpec.modelKeys` (`F[Map[Int, Int]]` keyed); `TopTypesUnionSpec.mergingCtor`. Knvl 2018 (D3) for the powerset
and partial-function arithmetic.

**Boundary.** It is an assumption about the measured code, not checked: a key type with a
reference `equals` (a plain class, a function) is read as if its equality were its value's. Open
keys stay refused.

## D37. No declaration outside the sources rebinds a builtin name

**Decision.** The owner's idealization (v1 §2): a channel the reading cannot follow or that leaves
the sources — a wildcard import or export of an unsupplied namespace, an unsupplied parent or self
type, a package clause no supplied unit declares members of, a wildcard whose prefix's first name
such a wildcard may rebind, a search that meets itself, a top-level package outside the sources
named like a root import's member (`util.Either`, `Predef.String`) — binds none of the names the
root imports bind (the builtins, the model's spellings, the top types). A declaration a channel
does reach still shadows one (a supplied member, an enum case, an export selector, a listed
namespace's member such as `quotes.reflect.Singleton`), and so does a channel into the standard
library or the JDK binds the names its package declares by their full rooted names:
`ListedNamespaces` lists every JDK 25 and scala-library 3.9.0 type whose simple name the root
imports bind to another declaration (generated from `jimage list` and the jars' entries), so
`import java.util.*` makes `List` mean `java.util.List`, and a wildcard into a library package
binds no other root name — on both sides. A rooted name whose values are a modelled type's counts
as that type under its own identity (`StdlibModel`'s equivalent entries: the Java and mutable
collections, the boxes, `Optional`); one the model does not read is its rooted declaration, read
like any supplied class through the dependency stubs, which render every JDK module's packages a
source or stub names; only a declaration no stub supplies is refused by name
(`declaration not supplied: java.awt.List`). The stored-value estimate still refuses a name an
unsupplied non-library wildcard may bind (`unresolved type: Int (may be imported by zz.*)`): its
namespace reading answers `Absent` both for a package no source supplies and for a supplied enum
or companion it does not follow, and taking the first for the second read `scala.Boolean` where
an enum case `Boolean` was meant (`StoredScopesSpec.enumTerms`). A channel's reach for anything else — an
implicit conversion a body may meet, a source name — is unchanged.

**Evidence.** **Executable**: `StdlibModelSpec.wildcardRefusal`, `.relativePaths`,
`.wildcardParent`, `.selfReachingParent`, `.javaWildcard`, `.listedHead`;
`GroundEnvironmentSpec.wildcard`, `.foreignWildcard`, `.nestedParentName`;
`UnsuppliedWildcardSpec.doubtedBuiltins`, `.singletonMarker`; `TopTypesUnionSpec.foreignSelf`,
`.packageObject`, `.chainedPackage`; `BinderBoundsSpec.rebindableTops`;
`HigherKindedRefusalSpec.boxedNames` (java.lang's boxed names read by their rooted names: their
scalars' counts, their own keys), `StdlibModelSpec.javaWildcard` (`java.util.List` a sequence,
`java.awt.List` refused unsupplied, 4 through a supplied declaration),
`TastyStubsSpec` (a `java.sql` enum 16 and `java.awt.List` resolved through rendered JDK-module
stubs), `.finalUnmodelled` (`Integer`, `Optional`),
`NamespaceScopingSpec.unsuppliedWildcard` (the stored side under `scala.collection.mutable.*`); the Shadowed-builtin-names group of `GroundEnvironmentSpec` (declarations still shadow).
Scala 3.9.0 fixtures r5, r8 and review-A7q/sc show a rebinding the assumption sets aside.

**Boundary.** A count under an unsupplied wildcard is exact only if that namespace declares no
root spelling; the report does not check it.

## D38. A top type is read by its place in its own arrow

**Decision.** The owner's decision (v1 §2), superseding D12's terminal reading in outputs,
ideal-dc's polarity flip and the ε₀ output tier: each function type and each method is read on its
own, innermost first. *Input* is a method's or function's parameter or a constructor slot, read by
the code receiving it; *output* is a result. `Any`, `AnyRef` / `Object`, `Matchable` and `AnyVal` in
an input are `1` (D12's argument: the code receiving it observes nothing of it) and in an output ω
(any value a finite program builds, the tier `String` has). Nothing flips through nesting
(`TopTypes.polarized`); a container's contents — an `Option`'s value, a sequence's elements, a
`Set`'s elements, a `Map`'s keys and values — take its place (`TopTypes.keyed`): `Option[Any]` is 2
as an input and ω as an output. A `Set[Any]` in an input is 2 values (its size is the only thing
that could tell two apart, and the type fixes none; a size known a priori determines the set), ω in
an output or a `val`; a `Map`'s keys alike. Arguments are read before the method: where an
argument's space holds an output top, the result's choices are counted with it as one value and two
or more of them are ω, one stays 1, none 0 (`Inhabitation.count`); no count is refused for a
top-typed position the solver cannot enumerate. A top result is not one-valued (rule O does not
apply). `AnyRef` / `Object` and `Matchable` are read exactly as `Any` everywhere — on the method
side, on the stored side, and in unions and intersections (`T | AnyRef` is `AnyRef`, `T & AnyRef`
is `T`); `AnyVal` keeps its reading (a top on the method side, unmodelled on the stored side,
refused in a union). `A <:< Any` (`AnyRef`, `Object`, `Matchable`) is a proof that carries nothing:
one value. An `inline` method's top-typed parameter refuses nothing. On the stored-value side `Any`,
`AnyRef` / `Object` and `Matchable` are ω everywhere (it was ε₀ for `Any`, unresolved for the
others), a top behind a source alias too. Constructor = a method, type = a value whose accessible
state is observed: a constructor row reads its parameters as inputs on the method side and never
hands a top (aliased or not) to the estimate, while the type's definitions row is the stored
side's: `final class Walk(val cur: Any, val n: Int)` is 2^32 as a constructor and ω as a type. A
repeated parameter is a possibly empty list (`first[A](xs: A*): A` is 0). The solver's `Card` has no ε₀ tier any more: it was added only for tops.

**Evidence.** **Executable**: `TopTypesUnionSpec.polarity` (`r(b: Boolean): Any` ω), `.anyOut`,
`.consumer` (`call[R](f: Any => R): R` 1), `.twoConsumers` (2), `.feed` (1), `.loop` (ω; `h(g: Unit
=> Any): Boolean` ω), `.unseeded` (1), `.existential`, `.structural`, `.otherTops`, `.containers`,
`.equalsOverride`, `.inlineTop`, `.referenceTops`, `.evidence`, `.topLike`, `.walkCtor`, `.walkType`,
`.outputSlot`, `.aliasSingletonCtor`, `.repeated`, `.mergingCtor`, `.nullCtor` (4), `.aliasedTopCtor` (`TC(f: Any => Boolean)` 2);
`ReentrySpec.literalSearch` (`V.conv` ω over a `def other: Any` capability); `StdlibModelSpec.keyed`
(`Set[Any]` and `Map[Any, Boolean]` as input and output); `ReportSpec` (`Anything(x: Any)` and
`Elements(xs: Array[Any])` ω); `StoredValueSoundnessSpec.containers`.

**Boundary.** The output tier is a decision, not a derivation. Over an argument space holding an
output top it is the extensional reading of the rule, above the parametric count a body that cannot
observe the top would have (2 for `h`).

## D39. The supplied sources are the whole program (closed world)

**Decision** (owner decision, an idealization like D26). No subclass, override or implementation
exists outside the supplied sources and dependency declarations. `ClosedWorld` reads the sources
with a `final` written on every concrete class no supplied template extends (an anonymous `new C
{ … }` and a given instance included) and on every concrete member of a class or trait no supplied
template overrides; every rule about final classes and members then applies (D19's derived
concrete code, final records, closed sealed leaves, fixed receivers). A member a supplied template
does override varies across receivers, so it stays a free capability, as an abstract member is
(C5), and its possible re-entry stays a view (C6). Two templates share an instance only where one
extends the other or a supplied template extends both. Abstract members stay capabilities (plan
§2), and a goal of an abstract template is still any tuple of them: the counted body is the one
new piece of code, and it may implement an abstract template, never override concrete code.

**Derivation.** Every receiver and every input is an instance of a supplied class. A member no
supplied class overrides runs its own body on all of them, so a call of it computes from the
environment the receiver shares: concrete code, which adds no choice (D19). A class no supplied
class extends has no instance of another class, so its value is its constructor arguments.

**Evidence.**

- **Executable**: `ClosedWorldSpec` (an override in another file, through an alias, by a mixin;
  braceless and annotated members; literal and constructor `val`s; a class's value; a compiled
  stub's parent read by its qualified name), and the
  closed twins beside the open fixtures of `CapabilitySpec`, `OwnersAndRecordsSpec`,
  `PureCodeSpec`, `MethodAnalysisSpec`, `EnvironmentSoundnessSpec`. `ReentrySpec` and
  `TargetViewsSpec` read the open world (`MethodAnalysis.analyzeOpen`): their open members stand
  for members a supplied template overrides.

**Boundary.** Templates are matched by the simple name their declaration or parent clause spells,
aliases followed. A compiled dependency stub names the parent the compiler resolved: its full
path, or the simple name of a top-level template of its own package (`StubDeclarations`), so a
stub's parent that some stub declares under that qualified name is that template alone, never a
same-named one of another package. A parent outside the sources whose members are not known (all but the markers
and `Product`, `Equals`) may override anything: a member only such a parent may override is refused
(`member may be overridden through a parent outside the sources: C.m`). Same-named templates of
different packages share their subclasses, so a member one of them overrides stays open — the open
world's reading, which may overcount (open: resolving parent clauses before the index exists). A class with a non-public constructor stays as written (its scope's
constructions are not enumerated), and so does the refusal of a record only its scope
instantiates. A constructor `val` and an untyped literal `val` keep their spelling (`final` would
change the signature or the type): a receiver asks the closed world for them instead
(`Resolver.overriding`). The closed world does not settle a name the analysis cannot resolve:
unresolved override identity, a sealed family extended through an alias, and the stored-value
estimate's open abstractions (a trait any implementation may extend) keep their reasons.

## D40. A value built before the receiver is not the receiver

**Decision.** A record value whose template may share an instance with the measured declaration's
keeps the reason `capability record member may be the measured declaration on its receiver` only
where the value may be that receiver (`this`). A value built before the receiver is not: a by-value
parameter of the receiver template's constructor that is no field, or of any definition lexically
around the template. Its member of the measured name is the declaration run on another, earlier
value, read as the closed world reads every member a supplied template overrides (D39): a free
capability. eo's anonymous optics (`outerProfunctor.dimap`, `innerProfunctor.dimap`, `reverse`,
the `Composer`, `Affine` and `MultiFocusK` conversions) lose the reason; their `Optic` record is then
refused as recursive (`recursive type not read`, parked).

**Derivation.** Evaluation is strict and effect-free (plan §2): a by-value argument is a finished
value when the call or `new` that receives it starts, so it was built before the instance that call
creates, and a finished value never contains an instance built after it (values are acyclic without
mutation or laziness). A class nested in a definition is instantiated while the definition runs, after
its parameters are evaluated; a template's constructor arguments are evaluated before it runs, and
Scala rejects `this` in a superclass constructor's arguments ("this can be used only in a class,
object, or template", scalac 3.8.3 on `class Sub2 extends Wrap2[Int, Int](this)`). A call of the
measured member on such a value runs on a receiver other than `this`, with the receiver's arguments:
no evaluation of the measured body re-enters itself on the same receiver through it. A by-name or
inline parameter is evaluated where it is read and may denote the receiver (`lazy val r: Opt[Int,
Int] = Ops.later(r, identity)` compiles, and `r.to` then calls itself); a constructor `val` is a
member a subclass may override with `this`; a parameter of the measured member itself may be `this`
at a call site. Those keep the reason.

**Evidence.**

- First principles (above), over D4's exclusion of a declaration's own implementation and D39.
- **Compiler fixtures** (scalac 3.8.3, `/tmp` fixtures, not retained): the `ReentrySpec`
  `builtBefore` source compiles, `Loop.r` included; `this` passed to a superclass constructor is
  rejected.
- **Executable**: `ReentrySpec.builtBefore` (dimap's and `Wrap`'s `to`: 1; a by-name `o`, a `val`
  field and the member's own parameter keep the reason), `CapabilitySpec.selfView`,
  `RecursionProbeSpec`.

**Boundary.** Only the record the value's declared type spells at its own reference is exempt: a
record reached through it (a member's result, a type argument, a function's result, an alias's body)
may be built from the receiver and keeps the reason. A captured strict local `val` and an enclosing template's
`this` are built before too, but this reading does not claim them; an enclosing template's member
may be overridden with the receiver. The rule does not
touch possible views of the receiver's own members (C6, D31): eo's `Optic.to` and `Optic.from` stay
`?` through `andThen`, whose supplied `ReadCompose` instances do run `outer.to`.

## How to extend this register

A new source earns an entry only with: the decision it defends, the exact claim relied on, its
verification level as defined above, and the boundary past which it must not be cited. A new
*decision* earns an entry only when it can name at least one source or one executable artifact
in this repository as evidence. Assertions that cannot meet either bar do not belong in the
register; they belong in the [plan's open-decisions list](../plans/v1.md#_8-boundaries-and-decisions-still-open).
