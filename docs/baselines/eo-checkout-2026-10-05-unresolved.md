# eo checkout: refreshed language-feature coverage

The first sections record the pre-implementation survey. Later sections record
the same checkout after the nine feature assignments and the subsequent
existential-input follow-up.

## Run and scope

- Date: 2026-10-05.
- Analyzer: `feat/sbt-plugin-skeleton`, commit
  `70b09ac8ea8778a3becdcdeaef964ecbbb6d3d8c`.
- eo: clean `main` checkout at
  `5a2802214e7c3f0caf08b6a4852edaf3c62666d0`.
- Runtime: sbt 2.0.9, Scala 3.8.4, Java 27; scalameta 4.17.4.
- Input: Git-tracked `.scala` files with `/src/main/` in their paths, excluding
  `benchmarks/`. Tests, build definitions, generated/untracked sources and `.delta`
  worktrees were not included. The site examples and law libraries were included.
- Production scope: avro (23 files), circe (9), core (54), generics (5),
  jsoniter (7), kyo (5), laws (61), schemes-laws (3), schemes (2), site (3), zio (6).
- Analysis: `Report.of` over the selected paths, once for the combined source set
  and once for core alone. No plugin was installed in eo and no eo files changed.
  This is a source-only analysis, not compiler/SemanticDB analysis; dependencies'
  source jars were not supplied. Combining modules is a coverage survey, not an
  emulation of each module's compiler classpath.

The local run artifacts are under `target/eo-checkout/`: `sources.txt`,
`production.txt`, `core.txt`, and the corresponding `*-blockers.tsv` files.
These are generated, ignored artifacts, not checked-in snapshots. The TSV records
each unresolved reason with its source path, per-source entry index, method name
and line. Counts below deduplicate by source path and entry index, not by name or
diagnostic text.

## Results

All selected sources were read and parsed: **zero errors** in both scopes.

| Scope | Sources | Definitions | Signatures | Finite | Countably infinite | Unresolved |
|---|---:|---:|---:|---:|---:|---:|
| Production source set | 178 | 392 | 1,520 | 23 | 2 | 1,495 |
| Core alone | 54 | 138 | 486 | 17 | 2 | 467 |

These implementation counts concern canonical pure, total, parametric
implementations in the analyzer's model. An unresolved result is not a proof of
infinity, impossibility, or a defect in eo.

For context, the published eo-core 0.16.0
[snapshot](eo-core-0.16.0.txt) had 53 sources, 134 definitions and 480 signatures:
15 finite, 2 countably infinite, 463 unresolved. The checkout is a different input;
the comparison is not a controlled analyzer regression test.

## Method/constructor blockers

Counts are **distinct signatures affected by each category**. A signature can
have several reasons, so rows do not add up to the unresolved total. In particular,
all bounded/higher-kinded parameter diagnostics are grouped together here, unlike
the report renderer's separate rows for each parameter spelling.

| Blocker | Production | Core | What is missing |
|---|---:|---:|---|
| Unresolved type | 1,245 | 341 | A richer method-shape vocabulary and resolution of source/dependency names; examples include `String`, `Int`, `Schema`, `Json`, `Quotes`, and path-dependent names such as `rc.Out` |
| Abstract type or method-valued representation | 515 | 242 | Models for abstract/member types, traits and non-plain products; prominent examples are `Optic.X`, `PSVec`, `Direct`, and optics capability traits |
| Qualified member environment not resolved | 418 | 57 | Resolve callable/captured members and their environments across owners, instances and qualified paths |
| Bounded or higher-kinded parameter | 337 | 163 | Higher-kinded application (`F[_]`, `F[_, _]`, `C[_[_]]`), upper/lower bounds, and context-bound evidence (`Type`, `Functor`, `Traverse`, `Monoid`, etc.) |
| Unsupported type syntax | 144 | 47 | The concrete syntax families listed below |
| Mutable capture | 30 | 16 | A sound treatment of state; mutation is outside the current pure implementation-counting contract |
| Inferred result type not resolved | 13 | 12 | Term-level result-type inference or compiler-provided type information |
| Opaque sum-producing callable elimination | 4 | 4 | Branch on a sum returned by a callable whose implementation is unavailable |
| Higher-order application | 2 | 2 | Apply function values obtained through higher-order calls in the inhabitation search |

`Int` and `String` appearing here do **not** mean the stored-value calculator
cannot count primitive types. The method-shape model has a much narrower builtin
vocabulary: `Unit`, `Nothing`, `Boolean`, `Option`, and `Either`.

### Unsupported syntax observed

These are distinct signatures whose `unsupported type:` reasons contain the
indicated syntax. Counts overlap; this is a classification of observed diagnostics,
not a claim that every occurrence of the feature fails in every analysis.

| Feature | Production | Core | Examples |
|---|---:|---:|---|
| Union types, including explicit nullability | 58 | 3 | `IndexedRecord \| Array[Byte] \| String`, `Json \| String`, `Schema \| Null` |
| Structural refinements and refined member bounds | 32 | 24 | `Optic[...] { type X = Xi }`, `Optic[...] { type X <: Tuple }` |
| Wildcards / type placeholders, including nested occurrences | 17 | 5 | `?`, `Function1[X0, *]` |
| Match types over unresolved parameters | 13 | 13 | `T match case (f, s) => f` and the second-component projection |
| Equality and subtyping evidence | 10 | 8 | `A =:= B`, `Tuple.Union[T] <:< A` |
| Singleton and intersection types | 10 | 0 | `Var.type`, `Record.type`, `name.type`, `String & Singleton` |
| Polymorphic function types and type lambdas | 8 | 0 | `[b] => ... => Type[b] ?=> Expr[Any]`, `[x] =>> ZIO[R, E, x]` |
| Repeated parameter types | 5 | 0 | `(A => Any)*`, `(S => Any)*` |
| Infix effect type applications | 5 | 0 | `A < Env[R]`, `Unit < Var[S]` |

The macro-heavy modules expose `Quotes`, `Expr`, dependent reflection types,
`Type` context bounds and polymorphic/context-function signatures. This run does
not expand macros. There was no standalone context-function diagnostic in this
run: the observed `?=>` types occur inside polymorphic function types rejected as
a whole. Supporting a syntax family can therefore expose further blockers rather
than immediately resolve every affected signature.

The four sum-elimination diagnostics occur at `PickFold.<init>`
(`AffineFold.scala:40`), `Optional.<init>` (`Optional.scala:73`),
`MendTearPrism.<init>` (`Prism.scala:79`) and `PickMendPrism.<init>`
(`Prism.scala:237`). The higher-order application diagnostics occur at
`CanModifyP.replace` (`CanModify.scala:18`) and `Modify.<init>`
(`Modify.scala:54`).

## Stored-value estimates are a separate analysis

The production definitions classify as:

```
83 unresolved · 25 instantiation-dependent · 9 unbounded by an open abstraction ·
2 type constructors, with no value space · 30 with more than one value ·
150 with one value · 7 with no values · 86 abstract
```

Core alone classifies as:

```
7 unresolved · 25 instantiation-dependent · 6 unbounded by an open abstraction ·
2 type constructors, with no value space · 3 with more than one value ·
84 with one value · 5 with no values · 6 abstract
```

Do not turn the method blocker list into a blanket unsupported-language list.
The stored-value side already handles cross-file source resolution and sealed
families, concrete generic substitution, some match-type reduction, and refinements
read as their base type. Its remaining symbolic/instantiation and member-inference
boundaries are described in the
[existing definition ledger](eo-core-0.16.0-unresolved.md).
An open abstraction has no closed value-space bound without an additional
assumption; a generic template needs an instantiation or symbolic answer. Neither
should be forced into a guessed number.

## Recommended next steps

1. **Resolve types and callable environments first.** Distinguish missing
   dependencies, unsupported builtin shapes, and unresolved instance/member paths.
   The largest diagnostic categories currently mix these causes.
2. **Model abstract/member representations and higher-kinded evidence together.**
   Optics capabilities, `Optic.X`, constructor applications and context bounds
   are coupled in eo; adding syntax alone will not make most signatures countable.
3. **Extend the method type model with unions, refinements and evidence.**
   These are concrete, frequently observed gaps. Preserve the distinction between
   overlapping Scala union types and disjoint tagged sums.
4. **Extend the inhabitation search for higher-order application and callable-sum
   elimination.** The six reported signatures give small, named regression targets.
5. **Keep mutation and macro semantics explicit.** Compiler-provided typing may
   resolve inferred/dependent types, but it does not by itself justify counting
   impure implementations or executing arbitrary macros as part of a report.

## After the nine feature assignments

The integrated, uncommitted working-tree changes on top of analyzer commit
`70b09ac8ea8778a3becdcdeaef964ecbbb6d3d8c` were run over the **same source manifest
and unchanged eo checkout**. Both reports again had zero read/parse errors, and
neither had an `analysis failed` diagnostic.

### Exact coverage did not increase on eo

| Scope | Finite before → after | Countably infinite before → after | Unresolved before → after |
|---|---:|---:|---:|
| Production | 23 → 23 | 2 → 2 | 1,495 → 1,495 |
| Core | 17 → 17 | 2 → 2 | 467 → 467 |

The identities of the unresolved signatures are unchanged, not just the totals.
The stored-value classification summaries are unchanged too. Generic
`unsupported type:` diagnostics are now absent (previously affecting 144
production signatures and 47 core signatures), but **this is not 144 resolved
signatures**. The new supported fragments and more precise diagnostics expose
other unresolved types, constraints and representations.

Local after-artifacts are `target/eo-checkout/production-after.txt`,
`core-after.txt`, and the corresponding `*-after-blockers.tsv` files.

### Implemented fragments and remaining boundaries

| Family | Supported now | Still unresolved |
|---|---|---|
| Unions/nullability | Duplicate/empty alternatives, aliases of the same free binder; `Null` is empty under the null-free method contract | Distinct alternatives needing overlap/discrimination proofs; unknown payload types |
| Refinements | Concrete member equalities, sibling equations, exact bounds, proven-bottom upper bounds, stable path projections | Whole trait/capability representations; non-exact bounds such as `X <: Tuple` |
| Wildcards/placeholders | Hygienic constructor-hole normalization and first-order beta application, including `Function1[A, *]` | Existential witnesses/bounds and general higher-kinded arguments |
| Match types | Proven tuple-pattern projections, nested projections and transparent alias substitution before shape erasure | Free/non-tuple scrutinees, uncertain patterns and tuple identity lost across shape-only substitution |
| Equality/subtyping evidence | Available free-atom equality, directed atomic subtype transport, provenance-preserving views | Structural/higher-kinded endpoints, variance and nonreflexive subtype interactions with functions/sums |
| Singletons/intersections | Accessible stable identities, widening with provenance, idempotence and proven singleton relationships | Callable module environments, unknown stable paths and general nominal/trait overlaps |
| Polymorphic functions/type lambdas | Hygienic first-order beta application; the closed total parametric identity `[A] => A => A` | General rank-polymorphic/evidence-bearing functions and unapplied higher-kinded lambdas |
| Repeated parameters | Explicit finite-sequence shape; exact empty-element collapse, sequence introduction and singleton-result goals | General supplied-sequence elimination, unknown elements and general collection models |
| Infix applications | The same application rule as prefix syntax, including qualification, argument order and arity | Missing external effect constructor representations; effects are not erased |

The fragments have focused regression tests; the table does not claim unrestricted
support for Scala's full semantics. Some eo syntax now gets past its previous
rejection, while other instances get a specific explanation for why their count
still cannot be determined.

### Remaining production blockers

Counts below again deduplicate signatures per category and overlap. Union
diagnostics are grouped by their common `union ` prefix; they include unknown
payloads as well as overlap/discrimination issues.

| Category | Production | Core |
|---|---:|---:|
| Unresolved type | 1,167 | 280 |
| Abstract/member or method-valued representation | 515 | 242 |
| Qualified callable/capture environment | 418 | 57 |
| Bounded/higher-kinded parameter | 337 | 163 |
| Union resolution | 56 | 1 |
| Whole refined representation unavailable | 32 | 24 |
| Mutable capture | 30 | 16 |
| Inert match type | 15 | 15 |
| Existential wildcard witness analysis | 14 | 2 |
| Inferred result type | 13 | 12 |
| Unavailable refined member | 8 | 6 |
| Unresolved singleton path | 8 | 0 |
| Constrained refined constructor | 4 | 4 |
| Callable sum elimination | 4 | 4 |
| Higher-order application | 2 | 2 |
| Unapplied type lambda | 2 | 0 |
| Unavailable stable path type | 2 | 2 |
| Unapplied constructor placeholder | 1 | 1 |

The lower unresolved-type count does not prove more complete resolution: some
formerly generic reasons are now wrapped or classified under another category.
The per-signature comparison above is the meaningful coverage check.

### Integration verification

- `core/testFull`: **550 passed, 19 pending, zero failures/errors**.
- Both plugin scripted fixtures, `cardinality/library` and `cardinality/report`,
  passed end to end.
- Core main/test formatting passed; `git diff --check` passed.
- Independent review findings were addressed with regression tests: nested
  binder-aware substitution, fixed-pattern substitution for the stored-value
  reducer, and use-site singleton accessibility checks on refined projections.
- Cross-feature tests also cover singleton-wrapped sequences, evidence rewriting
  through the new shapes, and collision-free internal substitution tokens.

For the implementation details, see the focused tests and the documents on
[unions/nullability](../research/method-unions-nullability.md),
[atomic evidence](../research/atomic-evidence-cardinality.md),
[polymorphic/type-lambda support](../research/poly-lambda-fragment.md), and
[repeated arguments](../repeated-arguments.md).

The next high-leverage work remains **type/callable-environment resolution,
abstract capability representations and higher-kinded evidence**. Broadening
syntax without those models is insufficient to turn these eo signatures into
exact numbers.

## Subsequent existential-input follow-up

The agreed input-side existential fragment now opens unbounded wildcard packages
as scoped rigid witnesses. Independent packages remain independent; fields of
one package and validated singleton aliases preserve shared identity. Bounds,
result witness choice/repackaging and fresh callable-produced packages remain
explicit boundaries. See [the model and examples](../research/existential-inputs.md).

The structural review changes were also applied: binder-aware substitution lives
in `TypeSubstitution`, singleton/intersection spellings share one extractor, and
match/transparent-alias projection normalization lives in `MethodProjections`.
Constructor spelling and lambda/binder classification share `TypeApplications`.

Verification: **573 tests passed, 19 pending**, zero failures/errors; both plugin
scripted fixtures passed. The 23 existential cases include real compiled Scala
opening, productive/unseeded cycles, witness independence, aliases, and
capture-avoiding substitution. Independent review's nested-binder collision case
is covered by a passing regression.

The same eo manifest was rerun with zero source errors and zero internal-error
diagnostics. Exact totals remain **23 finite, 2 countably infinite, 1,495
unresolved** for production, and **17 finite, 2 countably infinite, 467 unresolved**
for core. Previously generic wildcard-capture diagnostics give way to the
remaining constructor/representation/dependency reasons; these changes do not
yet unlock an additional exact eo count.

The local follow-up artifacts are `target/eo-checkout/production-existentials.txt`
and `target/eo-checkout/core-existentials.txt`.

## Definition-scope opaque representation follow-up

The next extension unfolds source-defined opaque aliases only where their
representation is visible to the observing use site. Alias declarations retain
their own lexical names and binders, but do not grant their caller visibility.
The same manifest and unchanged eo checkout were analyzed again.

| Scope | Finite before → after | Countably infinite | Unresolved before → after |
|---|---:|---:|---:|
| Production: 1,520 signatures | 23 → 29 | 2 | 1,495 → 1,489 |
| Core: 486 signatures | 17 → 23 | 2 | 467 → 461 |

**Exactly six previously unresolved rows now have `Finite(1)`**, all in
`dev.constructive.eo.data.Direct`: `apply` (line 35), extension `value` (38),
`accessor.get` (43), `reverseAccessor.reverseGet` (48), `applicative.map` (63),
and `applicative.pure` (66). All previously resolved row identities and counts
were retained unchanged. Stored-value summaries and source/signature inventories
are unchanged, with zero source errors or internal-error diagnostics.

The independent derivations and the supported/hidden visibility boundaries are
recorded in [opaque method representations](../research/opaque-method-representations.md).
The source's SHA-256 is
`788713d2536c7db0ff73d8cc0352da3ac4c063280247e821191d47682effd239`.
Its `foldMap`, higher-kinded `traverse`, and refined composition methods remain
unresolved for their remaining evidence/representation obligations.

Verification: **592 tests passed, 19 pending**, both plugin scripted fixtures
passed, including 19 opaque-scope examples and real compiled visibility/binder
fixtures. Core statement coverage is **90.57%** (branch coverage **83.94%**).
Review's inherited-substitution failure case now retains its diagnostic
instead of substituting an unrelated free binder and fabricating zero.

Local report artifacts: `target/eo-checkout/production-opaque.txt`,
`core-opaque.txt`, and the matching `*-opaque-counts.tsv` row inventories.

## Inherited override identity follow-up

An inherited declaration that the target implements is not a supplied callable
capability. The analyzer now proves direct override identity using nominal
constructor identities, parent type-argument substitution, and alpha-renamed
method binders. Proven different overloads remain available; uncertain aliases,
imports, dependent signatures and indirect overrides remain unresolved.
Source declaration identity uses the parsed tree, not a source offset shared by
unrelated files.

The unchanged manifest and eo checkout were rerun:

| Scope | Finite before → after | Countably infinite | Unresolved before → after |
|---|---:|---:|---:|
| Production: 1,520 signatures | 29 → 33 | 2 | 1,489 → 1,485 |
| Core: 486 signatures | 23 → 27 | 2 | 461 → 457 |

Exactly four additional rows have `Finite(1)`:

- `Accessor.tupleAccessor.get` (`Accessor.scala:18`): project the tuple's `A`.
- `ReverseAccessor.eitherRevAccessor.reverseGet`
  (`ReverseAccessor.scala:17`): inject the supplied `A` into `Right`.
- `ForgetfulFunctor.directTuple.map` (`ForgetfulFunctor.scala:24`):
  retain `X` and apply the supplied function to `A`.
- `ForgetfulFunctor.directEither.map` (`ForgetfulFunctor.scala:28`):
  retain the left `X`, or map the right `A` with the supplied function.

Each derivation uses distinct parametric binders and the complete relevant
environment. None depends on functor laws or treating the target itself as an
opaque recursive capability. All previously resolved rows retain their counts.
Source/signature inventories are unchanged, with zero source errors and zero
internal-error diagnostics. Exact coverage is now **35 / 1,520** production
signatures and **29 / 486** core signatures; parsing coverage is not counting
coverage.

Verification: **613 tests passed, 19 pending**, zero failures/errors; both plugin
scripted fixtures passed. The new tests include compiled Scala implementations
of all four signature families, nominal-product overloads, binder shadowing,
and conservative alias/import/return-type boundaries. A compiled parameter
annotation regression prevents a real override from becoming a false supplied
capability. Formatting and scalafix also passed. Cold-cache core coverage is
**90.66% statements** and **83.41% branches**.

Local report artifacts: `target/eo-checkout/production-overrides.txt` and
`core-overrides.txt`.
