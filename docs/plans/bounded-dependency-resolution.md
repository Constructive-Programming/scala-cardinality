# Bounded dependency resolution and caching

**Status:** first source-query foundation slice implemented; later phases remain proposed.
This is the next environment-resolution milestone,
not a promise to resolve every library type or count every method in a large application.

## Goal and boundary

Resolve the identities and relevant capabilities of foreign types without recursively analyzing
the entire classpath. Support selected targets in a million-line project, with bounded work,
incremental reuse, and explicit unresolved obligations when completeness cannot be established.

The landed EO baseline has 183 rows mentioning unresolved types, often alongside other reasons.
The 251-row figure came from the reverted capability-binder experiment. Neither bucket size is
a forecast of how many numeric answers dependency metadata will unlock.

Separate three questions:

1. **Identity:** which declaration does a reference denote?
2. **Available operations:** what values, constructors, projections, or callables are accessible
   to this target, and what do their signatures mean?
3. **Counting:** does the supported implementation model prove a count in that environment?

Finding `cats.Monoid` answers the first question, not the other two. An arbitrary foreign type
must not become a parametric `Shape.Atom`, an empty type, or a public product merely because its
declaration was found. Unknown representations and polymorphic capabilities remain obligations.
Library laws require explicit model assumptions; they are not inferred from names.

## Architectural choice

Use **immutable declaration summaries plus demand-driven queries**, rather than adding every
dependency source to the current report input list.

| Approach | Cost or limitation | Decision |
|---|---|---|
| Parse and analyze all dependency sources | Transitively unbounded; adds library methods as report targets; sources can be absent or differ from compiled artifacts | Do not use as the default |
| Guess models from simple library type names | Confuses shadowing, versions, visibility, and unknown producers | Reject |
| Use compiled symbol metadata and explicit semantic models | Adds a versioned compiler adapter, but supplies actual identities/signatures without reading bodies recursively | Preferred build-integrated direction |
| Explicit source dependency inputs | Works offline with the current parser; lacks full compiler name resolution | Bounded standalone fallback, with conservative guards |

Core consumes a small, versioned declaration-summary format, not compiler objects. Start with
explicitly selected metadata artifacts and an empty foreign-environment default. Keep the foreign
symbol environment separate from the source-derived `Library`; do not fabricate source trees to
feed compiled declarations into its existing equations.

A later build-integrated **metadata producer** should use Scala 3 compiler/TASTy information in the
analyzed project's compiler context, isolated from the plugin's own compiler/runtime classloader.
The report consumes its output; it does not run an arbitrary compiler inside resolution queries.
Prototype extraction costs and version compatibility before adopting an API. Reflection/classloading
must not execute dependency initialization. Java/Scala 2 metadata and unsupported TASTy versions
return explicit obligations until adapters exist.

The sbt producer obtains explicitly selected dependency paths from the chosen build configuration,
not by downloading the transitive universe itself. It must not silently enable `fullClasspath`
traversal. Compilation is an explicit prerequisite for compiled metadata mode, not a hidden effect
of standalone report generation. No network access is needed during analysis.

## Analysis request and index

An analysis request contains:

- selected project targets, separately from supporting project sources;
- an ordered, explicit dependency manifest, including artifact content identities;
- dialect/compiler/configuration identities and supported semantic-model versions;
- deterministic limits and cache policy.

Preserve the current sources-only mode. Add a separate opt-in mode instead of silently changing
`Report.of(paths)` to include classpath declarations or adding dependency methods to output totals.
The default whole-project report still necessarily scales with the number of reported targets.

Build an owner/name and qualified-symbol index once per project snapshot. Current lookup repeatedly
filters the full type list, and measurements scan package/module lists; caching those scans is not
a substitute for replacing them with indexed queries. Keep lexical scope, ordered classpath,
overload identity, access modifiers, and import/export rules explicit.

Project summaries come from one source pass per changed file. Dependency artifact directories are
indexed once, without analyzing bodies. Load declaration summaries only for requested symbols and
their relevant owners/parents/aliases. A selected-target query must not analyze all unrelated
methods in either the project or its libraries.

Source-summary extraction must also be memory-bounded: parse files individually, retain compact
declarations rather than every project's AST, and enforce a maximum individual input size. A
summary records body/capture obligations and provenance instead of pretending that omitted bodies
are harmless. Load a selected source body's AST only when an existing supported rule needs it.
Metadata-only extraction does not certify that concrete methods or initializers obey the model.

Stable symbol IDs describe artifact/module, owner, namespace, and overload signature. Parameter
binders have scoped identities, not just names. Positions are provenance, not durable cache identity;
moving a declaration cannot equate different symbols or corrupt a cached binder.
Origin-qualify archive entry identities: two jars containing `pkg/Types.scala` must not collide
in scopes, report grouping, or cache keys. Display paths can remain human-readable.

Snapshot acquisition is part of correctness. Hash and parse/decode the same captured bytes, not
separate reads of a mutable path. Freeze a directory/file manifest and revalidate its membership
and generations before publishing results; concurrent edits/builds require a bounded retry or a
`snapshot changed` diagnostic. For compiled mode, source summaries and compiler outputs must belong
to the same successful build generation. Otherwise reject the mixed snapshot rather than caching
it as complete. Adding a file during acquisition must not escape negative-lookup invalidation.

## Relevant environment closure

Starting at each selected signature, follow:

- declared input/result types and binder constraints;
- receiver, lexical captures, and accessible members;
- alias targets, parent substitutions, constructors and projections actually admitted by the model;
- explicit imports/exports and relevant companion/evidence lookup domains.

Do not enumerate all public functions as possible producers merely because their jar is present.
Conversely, do not ignore a potentially relevant accessible producer merely because it was not
loaded yet. Every query must track both positive dependencies and the namespace/member sets whose
completeness it relied on. Wildcard imports, exports, or unsupported implicit search can therefore
remain unresolved instead of pretending that only already-discovered values exist.

Decode a foreign declaration into a semantic shape only through an existing sound representation
rule or an explicit versioned library model. Prefer facts about identity, visibility, and member
signatures first. Initially leave collections' recursive representations, arbitrary typeclass
instances, and polymorphic capability instantiation unresolved. Loading `Monoid` does not solve
`foldMap[M]` specialization.

Summaries distinguish supplied abstract capabilities from concrete implementations and binding
aliases. A concrete identity method is not an unconstrained `A => A` producer establishing `ω`,
and a second name for a binding is not another choice. If a concrete body's supported semantics or
irrelevance is not established by a sound rule, preserve an obligation. Signature-only metadata
must not silently upgrade it to a freely supplied callable.

## Bounded work

Use a work queue and cycle/SCC detection, not unbounded recursive descent. Enforce independent limits
on:

- input files/bytes and archive entries inspected;
- symbols loaded, member signatures decoded, and dependency edges followed;
- alias/inheritance expansion and solver states;
- resident metadata, worker concurrency, and total cache storage.

Per-target limits isolate difficult signatures. Request-wide limits prevent a report with many
targets from multiplying those bounds indefinitely. A large report can be processed in explicit
batches, but batch boundaries must not change the declared environment.

Initially allocate deterministic per-target quotas from the request-wide budget **before** analysis.
Canonicalize the selected target set by stable symbol ID, bound its size, and record the resulting
allocation. Do not redistribute unused quotas according to completion order. This is deliberately
less efficient than work-stealing shared fuel, but makes cache warmth and parallel scheduling unable
to change which target exhausts its budget. A retry with a smaller selected set can receive a larger
quota; that is a different, visible request, not a cache discrepancy. Ingestion has its own limits;
an incomplete snapshot cannot certify target environments. Physical memory/IO limits remain bounded
independently and operational cancellation must not be reported as semantic completion.

Counters are deterministic logical work units. Cache hits still debit the semantic work represented
by a cached fragment when enforcing query limits, so a warm cache does not change which targets
resolve at the same limits. A fully certified final result can be reused only under a compatible
request key, including budgets. Wall-clock cancellation is an operational fallback, not a new
cardinality claim.

Each reusable result carries its logical work certificate and effective allocated quotas. Canonical
query traversal and certificate replay must use the same accounting rules as an uncached run. The
request key includes the canonical selected-target set, allocation policy/version, and budget profile;
the target key includes its effective allocation. A hit cannot bypass request admission or supply a
completed result that would exceed that target's quota.

On exhaustion, report `?` with the exhausted budget and resolution frontier. Preserve completed
independent rows. Never cache an incomplete environment as a complete one. Malformed, oversized,
or incompatible artifacts are diagnostics; archive readers must bound decompression and avoid
extracting paths or loading arbitrary code.

Choose numerical defaults from cold/warm measurements on EO and a large generated corpus, rather
than claiming a universal bound now. Every limit must be configurable and visible in provenance.

## Cache design

### 1. Content-addressed declaration facts

Cache immutable per-file/per-artifact declaration summaries and artifact indexes. Keys include
content digest, dialect/compiler metadata version, extractor/schema version, and extraction options.
Identical verified artifacts can share summaries across projects; project-private summaries stay
local. Do not persist mutable resolver instances or scalameta/compiler object graphs.

Cold discovery must inspect/hash declared inputs; it is not sublinear in unknown input size.
Build change information can avoid needless work, but paths, timestamps, or coordinates alone are
not correctness keys. Reusing a coordinate for changed jar bytes must invalidate its summaries.
If reliable incremental change information is absent, revalidate inputs before reusing results.

### 2. Scoped resolution fragments

Within an immutable request snapshot, memoize lookup and normalization by symbol/type expression,
binder substitutions, observer/access scope, relevant namespace versions, and model version.
The use-site observer matters for opaque/protected access; declaration-site lookup alone is
insufficient. Visitation/cycle state and truncation must not turn a context-specific failure into a
globally memoized result. Start with request-local memoization; persist fragments only after this
dependency contract is tested.

Negative lookups depend on namespace/member-set digests, not just the absence of a file.
A newly added declaration, overload, import, export, or given must invalidate affected queries.
Until precise dependency tracking is established, invalidate against the entire environment snapshot.
Conservative invalidation costs time; under-invalidation produces wrong answers.

### 3. Per-target results

Initially key results by target semantic digest, full environment snapshot, analyzer/model versions,
configuration, and limits. This is conservative but reviewable. Later use recorded dependency
footprints to narrow invalidation, including negative lookup and producer/evidence-domain witnesses.

Persist supported and genuinely unsupported results, but distinguish budget exhaustion and
cancellation. A budget-limited result must not prevent a retry with higher limits. A missing dependency
must invalidate when the manifest changes. Never reuse results of the reverted binder experiment.

Cache storage is local, bounded, atomically written, and disposable. Validate schema/digests on read;
corruption is a cache miss, not an analysis failure or proof. Use per-key coordination for concurrent
requests, bounded eviction, and no remote upload of private source or metadata. A fresh checkout
must still work without any cache.

## Delivery sequence

### A. Request boundary and scalable source index

Separate report targets from support inputs, add selected-target queries and budgets, and replace
whole-index lookup scans with symbol/owner indexes. No new semantic claims. Preserve legacy report
ordering/counts. Measure existing package/module environment scans as well as type lookup.

**Exit:** EO equivalence; selected-target work does not measure unrelated methods; explicit budget
failures instead of recursion overflow; counters for parsing, lookup, expansion, and solver work.

### B. Declaration cache and invalidation contract

Add versioned immutable summaries, artifact manifests, request-local memoization, and conservative
snapshot-keyed result reuse. Keep report tasks uncached at the sbt task level: the analyzer's verified
content cache owns freshness, so sbt must not serve reports keyed only by transient path inputs.

**Exit:** cold/warm/cache-disabled equality; unchanged inputs avoid repeat parsing/metadata decoding;
changes to jar bytes, declarations, imports, visibility, and models invalidate; corrupt/concurrent
cache writes recover safely. Budget outcomes agree cold and warm.

### C. One explicit dependency and import slice

Consume selected, versioned metadata from a small fixture library; compare its declarations against
compiled Scala fixtures. Begin with explicit qualified references and supported public aliases/plain
products. Then add named imports/renames and supplied abstract monomorphic callables under explicit
environment-completeness checks. Concrete callables retain the semantics guard above.
Keep wildcard/evidence/unsupported representation domains guarded. Do not initially promise full Cats
or Scala collections semantics.

**Exit:** source and dependency versions/shadowing/overloads/opaque visibility behave correctly;
dependency declarations do not add report rows; libraries are not executed or downloaded; an
independently derived full-environment case gains a numeric answer without losing unknown producers.
Include an abstract endomorphism, a concrete identity, and a binding alias as distinct adversarial
fixtures. Prototype the isolated compiler/TASTy producer only after the input/schema boundary is
stable; unsupported producer versions must not affect sources-only mode.

### D. EO case-directed expansion

Choose a concrete EO signature and load only its required library declarations, pinned to the actual
dependency versions. Publish the resolution trace, model assumptions, independent count or remaining
obligation, and full report delta. Extend typeclass/collection/capability models only when that case
justifies them. Investigate producer/evidence-domain completeness before claiming a number.

## Scale and soundness acceptance

- All retained EO numeric rows are checked, not just newly resolved ones.
- Missing metadata or unsupported semantics stays `?`, even when identity is known.
- Adding a reachable producer can change a count; adding an unrelated artifact need not rerun target
  reasoning once precise footprints exist. Both cases have adversarial tests.
- Equal results across input orders, cold/warm/cache-disabled runs, concurrency levels, and cache
  eviction. Environment ordering that is semantically significant remains part of the key.
- Two competing targets exercise request allocation and cache-certificate replay. Concurrent source
  edits, added files, and rebuilds exercise snapshot acquisition and source/output generation checks.
- Benchmarks include EO, generated million-line projects, many irrelevant dependencies, wildcard
  imports, and adversarial cyclic/high-fanout types. Record cold indexing/hash cost separately from
  warm selected-target latency and peak memory. Do not claim million-line whole-report latency from
  a selected-target benchmark.
- The report exposes input/manifest identity, cache/schema/model versions, limits, completed work,
  cache hits, and the frontier for unresolved queries without leaking secrets.

## First implementation PR

Start with **A**, plus a cache-key/snapshot contract exercised by tests. Keep it independent of new
foreign-type semantics. Then deliver **B** before advertising dependency-enabled analysis. This pays
the scaling and freshness cost up front, while leaving the existing core solver and output meaning
unchanged. Adapter feasibility evidence may adjust phase C; it must not weaken the semantic boundary.

## Implementation checkpoint: selected source queries

The first slice adds `Report.query`, `AnalysisQuery`, and `cardinalityReportSelected`, with explicit
target/support separation, origin-qualified snapshots, indexed declaration/package lookup, fixed
target fuel allocation, charged resolution/capture/solver work, and content/environment/request keys.
Capture bounds file/root counts, raw/decompressed bytes, directory/archive entries and compressed
archive size; read or parse failures reject the entire environment. Two acquisitions detect observed
drift but do not provide filesystem transactions or compiler-generation coherence. Acquisition
limits apply to each pass, so verification reads the admitted inputs a second time.

This is not completion of A or B: compact declaration summaries, bounded resident AST storage,
compiler/build snapshots, persistent caches/certificate replay, and million-line benchmarks remain
open. ASTs are still retained for the admitted source set. The lookup regression adds 2,000 unrelated
types and verifies unchanged lookup bucket/candidate work, not whole-run sublinear cost. Ingestion
and indexing are charged separately from selected-target work; default limits are admission policy,
not empirical promises of throughput. Legacy whole-source reports retain their previous behavior.
