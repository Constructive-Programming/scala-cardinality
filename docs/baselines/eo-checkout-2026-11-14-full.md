# cats-eo full-checkout run: every feature measured

This is the ledger of the cats-eo v1.0.0 full-checkout run at main `36cd3cd`. It records the "measure
every Scala feature cats-eo 1.0.0 uses" work, merged by PR #25, and the main it reached. The
[2026-10-27 ledger](eo-checkout-2026-10-27-full.md) records the state before that work and stays as
the historical record. The counting contract every number here rests on is [plan §2](../plans/v1.md#2-agreed-counting-contract);
the evidence for each rule is the [register](../research/code-cardinality-foundations.md). The chronological
[work log](../plans/eo-1.0.0-work-log.md) of the session that did the work opens with a summary for
review.

## Run and scope

- Date: 2026-11-14.
- Analyzer: main `36cd3cd`: this work through `7e3e306` (= main `4eabd23`, merged by PR #25) and
  the commits on main since, summarised under [How the run got here](#how-the-run-got-here). The
  commit that brings this ledger changes documentation only.
- cats-eo: tag `v1.0.0`, commit `570460441ab3fc1e118de3934a2aed2c206b64f1`.
- Runtime: sbt 2.1.0-RC1, Scala 3.10.1, scalameta 4.17.4 (the run header).
- Input: the same 189 Git-tracked `*/src/main/*.scala` across thirteen modules as the previous
  ledgers (benchmarks, tests and demos excluded; site examples and law libraries included), read as
  one scope, from the **sources only**: the runner's three-argument form, as in the earlier ledgers.
  Its optional classpath argument (and the build plugins) would add the dependencies' compiled
  declarations as TASTy and Java signature stubs; this run has none, so every foreign name reads as a
  declaration not supplied.
- Runner: `cardinality.EoCheckout` (test scope, opt-in). It writes `eo-checkout.txt` (the complete
  report), `eo-checkout-core.txt` (core alone), `eo-checkout-methods.tsv` (one row per signature:
  module, source, qualified name, kind, enclosing signature, signature, count, reasons),
  `eo-checkout-definitions.tsv` (one row per stored-value definition: module, source, qualified
  name, kind, parameters, line, size, class, reasons, reason kinds) and `eo-checkout-blockers.tsv`
  (distinct signatures per blocker category, combined and core). `cardinality.EoDelta` prints the
  per-row delta between two runs. Generated artifacts, not checked in; the commands under
  [Reproducing](#reproducing) regenerate them.

Every selected source was read and parsed: **zero errors**. The run checks that forward and
reversed input order give identical results on both sides, that no two method or definition rows
share an identity, and that the production `Report.of` path renders the same report.

## Results

| Scope | Sources | Definitions | Signatures | Finite | Countably infinite | Beyond countable | Unresolved |
|---|---:|---:|---:|---:|---:|---:|---:|
| Production source set | 189 | 435 | 2,074 | 354 | 185 | 0 | 1,535 |
| Core alone | 55 | 146 | 668 | 206 | 48 | 0 | 414 |

| Module | Signatures | Finite | Countable | Unresolved | Definitions |
|---|---:|---:|---:|---:|---:|
| avro | 464 | 47 | 36 | 381 | 74 |
| chimney | 33 | 0 | 0 | 33 | 3 |
| circe | 147 | 2 | 7 | 138 | 18 |
| core | 668 | 206 | 48 | 414 | 146 |
| generics | 40 | 4 | 0 | 36 | 8 |
| iron | 8 | 0 | 0 | 8 | 1 |
| jsoniter | 132 | 13 | 50 | 69 | 23 |
| kyo | 53 | 2 | 0 | 51 | 3 |
| laws | 331 | 59 | 0 | 272 | 80 |
| schemes | 19 | 5 | 4 | 10 | 2 |
| schemes-laws | 8 | 1 | 0 | 7 | 2 |
| site | 90 | 15 | 40 | 35 | 67 |
| zio | 81 | 0 | 0 | 81 | 8 |

The 2,074 rows are 1,621 methods, 254 constructors and 199 declarations
(abstract members: abstract defs, givens and values; given aliases and member values with bodies are
among the methods). 1,639 of them are the rows the 2026-10-27 ledger had; the other
435 are the definitions beyond `def`s that got rows in this work (register
[D28](../research/code-cardinality-foundations.md#d28-rows-beyond-defs-each-measured-by-the-rule-its-translation-has)):
given aliases and abstract givens, member values, members of anonymous classes, definitions in
nested blocks, function literals, case clauses and comprehensions, enum-case, abstract-class and
secondary constructors. "Countably infinite" includes the constructor rows whose value-space reading
is a tier sum such as `2ω + ≤2^32` (avro `AvroFailure.NotARecord.<init>` and its siblings); see
[plan §2, output meanings](../plans/v1.md#assumptions-and-output-meanings).

Stored-value side (435 definitions): 66 unresolved · 36 instantiation-dependent · 16 unbounded by an open abstraction · 3 type constructors, with no value space · 66 with more than one value · 125 with one value · 4 with no values · 119 abstract. The `?` rows are the
unresolved, instantiation-dependent and unbounded ones: 118.

The eo-core 0.16.0 archive, run by `cardinality.EoBaseline` at the same analyzer, reads
644 signatures: 212 finite, 45 countably infinite, 387 unresolved, and its five independently
reviewed model counts are unchanged (the runner checks them). Its checked-in artifact stays the
`7f8bcff` run of 480 signatures ([review ledger](eo-core-0.16.0-review.md)).

## How the run got here

One row per commit of this work as it was integrated, in order, then main as measured here.
Methods: the headline of `eo-checkout-methods.tsv`; stored: definitions whose size is `?`. Every count
change of the work's commits, `e575d01` to `7e3e306`, was explained row by row when it landed, and its
commit message records the measured delta; every new number from `b5045b8` to `7e3e306` was
audited (see [Independent verification](#independent-verification)).

| Commit | Change | Signatures | Finite | ω | Beyond countable | `?` | Stored `?` |
|---|---|---:|---:|---:|---:|---:|---:|
| `613f655` | main before this work (PR #23 merged) | 1,639 | 46 | 38 | 1 | 1,554 | 95 |
| `e575d01` | Count ground scalars as transparent values with exact symbolic sizes | 1,639 | 47 | 46 | 1 | 1,545 | 95 |
| `58a217b` | Fix environment and family readings that miscounted | 1,639 | 63 | 46 | 1 | 1,529 | 95 |
| `c136109` | Resolve stored-value names through the declarations they denote | 1,639 | 60 | 47 | 0 | 1,532 | 110 |
| `4db1023` | Read top types as terminal and disjoint class unions as sums | 1,639 | 96 | 47 | 0 | 1,496 | 110 |
| `bda05e1` | Read higher-kinded binders as rigid constructors and bounds as proofs | 1,639 | 108 | 47 | 0 | 1,484 | 110 |
| `7ab0a74` | Sum sealed families and carry every reason a stored value reads | 1,639 | 108 | 47 | 0 | 1,484 | 117 |
| `2cc8de2` | Read stdlib declarations from a model and eliminate sequences by length | 1,639 | 144 | 91 | 0 | 1,404 | 117 |
| `ceccb6d` | Read abstract type members as path-keyed atoms and stuck projections | 1,639 | 152 | 95 | 0 | 1,392 | 117 |
| `b5045b8` | Keep report rows on one line and add a per-row run delta | 1,639 | 152 | 95 | 0 | 1,392 | 117 |
| `e039cd6` | Bind exports and read closed code, vars and inferred results exactly | 1,639 | 158 | 98 | 0 | 1,383 | 117 |
| `ad931e2` | Read source classes as capability records and classify their members | 1,639 | 183 | 98 | 0 | 1,358 | 117 |
| `382191a` | Read named tuples, literal and context function types; refuse inline | 1,639 | 159 | 93 | 0 | 1,387 | 117 |
| `fb5d936` | State the no-side-effects assumption in the README and a new AGENTS.md | 1,639 | 159 | 93 | 0 | 1,387 | 117 |
| `cd2c845` | Read polymorphic members by free theorems and eliminate callable sums | 1,639 | 172 | 93 | 0 | 1,374 | 117 |
| `b1eefb9` | Read vars as values, ignore throws and count throwables as data | 1,639 | 172 | 97 | 0 | 1,370 | 117 |
| `ecc47dd` | Count inline methods free of compile-time operations as methods | 1,639 | 188 | 100 | 0 | 1,351 | 117 |
| `08dcde1` | Report a row for definitions beyond defs: givens, values, nested scopes | 2,074 | 289 | 126 | 0 | 1,659 | 117 |
| `83f5e76` | Refuse names an unsupplied wildcard may bind on the method side | 2,074 | 289 | 127 | 0 | 1,658 | 112 |
| `280ab80` | Bind stored-side members, scope opaque aliases, read value rows | 2,074 | 289 | 127 | 0 | 1,658 | 112 |
| `7e3e306` | Close the integrated review's method-side gaps and recover precision (= main `4eabd23`, merged by PR #25) | 2,074 | 295 | 133 | 0 | 1,646 | 112 |
| `36cd3cd` | main: the commits since `4eabd23`, not audited by this work | 2,074 | 354 | 185 | 0 | 1,535 | 118 |

Numbers that went back to `?` are corrections, each explained in its commit message: `c136109`
(names resolve to the declarations Scala binds them to, D11: 5 method rows and 17 stored rows had
read a name wrongly), `7ab0a74` (7 stored rows now carry every reason they read, D15), `382191a`
(every inline method refused until `ecc47dd` read those free of compile-time operations as methods,
D27) and `83f5e76` (3 method rows and 1 stored row read names an unsupplied wildcard may bind,
D16). `08dcde1` added the 435 rows of definitions beyond `def`s (D28).

The last row is main `36cd3cd`: the commits since `4eabd23` (Java and TASTy dependency stubs, the
closed world, the modelling conventions for receivers and opaque types, lawful key equality, the
top-type, factory and conversion rules; register D32–D43), which this work did not audit. Against
`7e3e306`, per row: 120 method rows went from `?` to a number, 9 from a number to `?` and
6 changed number; 910 changed reasons only. Of the stored rows 6 went from a size to `?`, 4
changed size and 2 changed reasons only. The signature set is the same (2,074 rows on both sides).

## Unresolved rows: by design or analyzer gap

Every `?` row of both sides is classified by its reasons into one of two classes.

- **By design**: under the documented model the honest answer is not computable from the supplied
  sources, or the code is outside the model. Declarations not supplied (this run reads no
  dependency declarations), names an unsupplied wildcard import may bind, compile-time code (quotes,
  `summonInline`, call-site dispatch of inline bodies, `transparent` results), garbage-collector
  state and keys of an open type, a receiver no public constructor or factory builds and other
  hidden state, what the closed world leaves open (members a supplied template overrides, which
  may re-enter the measured declaration), and an open class in a slot the value-space convention
  reads (a constructor row's `Throwable` slot, the stored side's open abstractions). On the stored
  side also "depends on its instantiation": a generic template has no single number.
- **Analyzer gap**: the model has an answer the analyzer cannot compute yet: recursive types,
  capability records with abstract type members, polymorphic capability binders, callable-sum
  elimination, higher-order application, inference from argument types, other representation
  readings, evidence relationships, case-clause context, and possible views whose re-entry cannot
  reach the goal (only where an audit derived the true number). Two gap families also need a model
  decision, not only code: inference from argument types (not adopted) and polymorphic binders the
  result does not determine (a count above 1 needs coverage, register D24).

**Counting a row with several reasons.** The analyzer prints a row's reasons alphabetically (1,533 of
1,535 rows), so the first one printed is arbitrary: taking it would give 1,207 by design /
328 gap. Instead each row counts under the first **group** in the precedence BD1 > … > BD7 > G1 >
… > G10, and inside it under the fine category carried by the most `?` rows (ties alphabetical). By
design groups come first because one by-design reason keeps the row `?` after every gap is closed.
So the gap-first rows are exactly the rows whose **every** reason is a gap: an upper bound on what
closing gaps could number, since a reading that stops early can hide a further reason. Categories
that mix causes are split by their detail text, shown in parentheses below.

| Side | `?` rows | By design (first blocker) | Analyzer gap (every reason a gap) | Rows with at least one gap reason |
|---|---:|---:|---:|---:|
| Methods | 1,535 | 1,217 (79.3%) | 318 (20.7%) | 468 |
| Stored values | 118 | 112 | 6 | 6 |

Groups:

- **BD1** (by design): compile-time code: quotes and macros, compile-time operations, call-site dispatch of inline bodies (a member a subclass may override inline, an extension or conversion, a given left to implicit search, a selection on a value of an abstract type), `transparent` results ([D23](../research/code-cardinality-foundations.md#d23-compile-time-and-type-revealing-evidence-is-not-parametric), [D27](../research/code-cardinality-foundations.md#d27-an-inline-method-free-of-compile-time-operations-is-a-method)).
- **BD2** (by design): outside the model: garbage-collector state (`java.lang.ref.ReferenceQueue`), and a `Map` or `Set` over an open key type, whose key equality no parametric program has ([D36](../research/code-cardinality-foundations.md#d36-key-equality-is-lawful-and-a-single-pass-read-is-replayable)).
- **BD3** (by design): a declaration not supplied: foreign names, unmodelled standard-library names, their parents and bounds ([D16](../research/code-cardinality-foundations.md#d16-library-declarations-are-read-from-a-versioned-model-absent-ones-are-reported)), and a member only a parent outside the sources, whose members are not known, may override ([D39](../research/code-cardinality-foundations.md#d39-the-supplied-sources-are-the-whole-program-closed-world)).
- **BD4** (by design): a name an unsupplied wildcard import may bind ([D16](../research/code-cardinality-foundations.md#d16-library-declarations-are-read-from-a-versioned-model-absent-ones-are-reported), [D33](../research/code-cardinality-foundations.md#d33-a-listed-dependency-namespace-binds-only-the-members-its-artifacts-declare)).
- **BD5** (by design): a constrained receiver or hidden state: a class with neither a public constructor nor a public factory, a hidden parameter or field, an open class with hidden constructor state ([D19](../research/code-cardinality-foundations.md#d19-concrete-code-nothing-can-override-adds-no-implementation), [D35](../research/code-cardinality-foundations.md#d35-public-constructors-build-the-receiver-an-opaque-type-outside-its-scope-is-one-value), [D42](../research/code-cardinality-foundations.md#d42-a-class-no-callable-constructor-builds-is-counted-by-its-public-factories)).
- **BD6** (by design): what the closed world leaves open: a member a supplied template overrides stays a capability and may re-enter the measured declaration, a record may be the measured declaration on its receiver, an override identity not resolved ([D20](../research/code-cardinality-foundations.md#d20-a-value-of-a-source-class-is-the-record-of-its-observations), [D39](../research/code-cardinality-foundations.md#d39-the-supplied-sources-are-the-whole-program-closed-world)); and an open class in a slot the value-space convention reads: a `Throwable` slot of a constructor row (`open to implementations: Throwable`; [plan §2, output meanings](../plans/v1.md#assumptions-and-output-meanings)) and the stored side's open abstractions.
- **BD7** (by design): stored side: depends on its instantiation.
- **G1** (gap): capability record with an abstract type member not read, higher-kinded members, refinements and paths included ([D18](../research/code-cardinality-foundations.md#d18-an-abstract-member-of-a-stable-path-is-a-rigid-atom), [D20](../research/code-cardinality-foundations.md#d20-a-value-of-a-source-class-is-the-record-of-its-observations)).
- **G2** (gap): polymorphic capability binders: higher-kinded, bounded, or not determined by the result ([D24](../research/code-cardinality-foundations.md#d24-a-polymorphic-capability-member-is-read-up-to-its-free-theorems)).
- **G3** (gap): callable-sum elimination over unenumerated or unbounded spaces, opaque sum- and sequence-producing callables ([D25](../research/code-cardinality-foundations.md#d25-a-supplied-sum-is-eliminated-once-per-argument-and-path)).
- **G4** (gap): higher-order application.
- **G5** (gap): inferred result types beyond the syntactic forms ([environment completeness §5](../research/environment-completeness.md)).
- **G6** (gap): representation readings: recursive types ([D4](../research/code-cardinality-foundations.md#d4-recursion-requires-productivity); the reading is parked, [D40](../research/code-cardinality-foundations.md#d40-a-value-built-before-the-receiver-is-not-the-receiver)), a receiver space its constructors and factories leave uncompared ([D42](../research/code-cardinality-foundations.md#d42-a-class-no-callable-constructor-builds-is-counted-by-its-public-factories)), sealed receiver splits, match types, arrays, anonymous instances.
- **G7** (gap): evidence relationships: `=:=`, overlapping anonymous evidence, existential constructors, singleton intersections.
- **G8** (gap): case-clause context of nested definitions.
- **G9** (gap): unions needing discrimination.
- **G10** (gap): a possible view that cannot reach the goal, where an audit derived the true number.

### Methods per module (first blocking group)

| Module | `?` | By design | Gap | BD1 | BD2 | BD3 | BD4 | BD5 | BD6 | G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8 | G10 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| core | 414 | 252 | 162 | 34 | 2 | 193 | 5 | 11 | 7 | 13 | 1 | 7 | 8 | 9 | 120 | 1 | 1 | 2 |
| avro | 381 | 378 | 3 | 37 | 4 | 329 |  | 1 | 7 |  |  |  |  |  | 3 |  |  |  |
| laws | 272 | 145 | 127 |  |  | 145 |  |  |  | 5 | 4 |  |  |  | 118 |  |  |  |
| circe | 138 | 138 | 0 | 21 |  | 117 |  |  |  |  |  |  |  |  |  |  |  |  |
| zio | 81 | 81 | 0 |  |  | 81 |  |  |  |  |  |  |  |  |  |  |  |  |
| jsoniter | 69 | 58 | 11 | 13 |  | 45 |  |  |  |  |  | 2 |  |  | 9 |  |  |  |
| kyo | 51 | 51 | 0 | 12 |  | 39 |  |  |  |  |  |  |  |  |  |  |  |  |
| generics | 36 | 36 | 0 | 36 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| site | 35 | 35 | 0 |  |  | 34 |  | 1 |  |  |  |  |  |  |  |  |  |  |
| chimney | 33 | 33 | 0 | 1 |  | 30 | 2 |  |  |  |  |  |  |  |  |  |  |  |
| schemes | 10 | 0 | 10 |  |  |  |  |  |  |  |  | 7 |  |  | 3 |  |  |  |
| iron | 8 | 8 | 0 |  |  | 8 |  |  |  |  |  |  |  |  |  |  |  |  |
| schemes-laws | 7 | 2 | 5 |  |  | 2 |  |  |  |  |  |  |  |  | 5 |  |  |  |
| **total** | **1,535** | **1,217** | **318** | **154** | **6** | **1023** | **7** | **13** | **14** | **18** | **5** | **16** | **8** | **9** | **258** | **1** | **1** | **2** |

### Methods per category

"First" counts the rows filed under the category; "any" counts every row carrying it (the
`eo-checkout-blockers.tsv` figure where the category is not split).

| Class | Group | Category | First | Any |
|---|---|---|---:|---:|
| design | BD1 | outside the counting model (quoted Quotes/Type/Expr) | 95 | 95 |
| design | BD1 | inline method (compile-time operation: quotes / summonInline) | 34 | 34 |
| design | BD1 | inline method (call-site dispatch: extension / conversion) | 8 | 8 |
| design | BD1 | transparent inline result | 7 | 28 |
| design | BD1 | inline method (call-site dispatch: a given left to implicit search) | 4 | 4 |
| design | BD1 | inline method (call-site dispatch: selection on a value of an abstract type) | 4 | 4 |
| design | BD1 | definition inside an inline method | 2 | 2 |
| design | BD2 | outside the counting model (garbage-collector state) | 4 | 4 |
| design | BD2 | _ over an open type is not read (its keys are tested for an equality no parametric program has) | 2 | 2 |
| design | BD3 | declaration not supplied | 870 | 919 |
| design | BD3 | inherited type member not resolved (unsupplied parent) | 145 | 196 |
| design | BD3 | union _: declaration not supplied | 4 | 51 |
| design | BD3 | inline method (callee or type not supplied) | 2 | 9 |
| design | BD3 | bounded higher-kinded parameter requires its bound's capabilities | 1 | 1 |
| design | BD3 | inferred result type not resolved (overrides an unsupplied declaration) | 1 | 1 |
| design | BD3 | inherited parent declaration not supplied | 0 | 109 |
| design | BD3 | member may be overridden through a parent outside the sources | 0 | 25 |
| design | BD3 | inherited term member not resolved (unsupplied parent) | 0 | 3 |
| design | BD3 | unavailable refined member | 0 | 1 |
| design | BD3 | union _: inherited type member not resolved (unsupplied parent) | 0 | 1 |
| design | BD4 | inline method (callee or type under an unsupplied wildcard) | 4 | 4 |
| design | BD4 | unresolved type (may be bound by an unsupplied wildcard) | 3 | 21 |
| design | BD4 | inherited type member not resolved (parent under an unsupplied wildcard) | 0 | 6 |
| design | BD4 | inferred result type not resolved (callee an import may bind) | 0 | 1 |
| design | BD4 | local capture may be hidden by an import | 0 | 1 |
| design | BD5 | sealed subtype hides a parameter | 12 | 17 |
| design | BD5 | receiver has no public constructor | 1 | 1 |
| design | BD5 | open class with hidden constructor state | 0 | 4 |
| design | BD5 | final class field not observable injectively | 0 | 2 |
| design | BD6 | open to implementations | 7 | 7 |
| design | BD6 | capability record member may be the measured declaration on its receiver | 5 | 6 |
| design | BD6 | inherited override identity not resolved | 2 | 3 |
| design | BD6 | inherited declaration identity not resolved | 0 | 11 |
| design | BD6 | local capture may be hidden by an inherited member | 0 | 4 |
| design | BD6 | overridable member may re-enter the measured declaration | 0 | 4 |
| gap | G1 | refined representation unavailable (constraints not erased) | 12 | 17 |
| gap | G1 | capability record with abstract type member not read | 4 | 13 |
| gap | G1 | unavailable stable path type | 2 | 4 |
| gap | G1 | path through a value not followed | 0 | 14 |
| gap | G2 | polymorphic capability binder (not determined) | 4 | 4 |
| gap | G2 | polymorphic capability binder (instantiations) | 1 | 1 |
| gap | G3 | sum elimination over an unbounded argument space | 13 | 13 |
| gap | G3 | sum elimination over an argument space not enumerated | 2 | 2 |
| gap | G3 | Elimination of an opaque callable's sequence result is unsupported | 1 | 1 |
| gap | G4 | Higher-order application is unsupported | 8 | 8 |
| gap | G5 | inferred result type not resolved (tier 2: argument-type matching / chained inference) | 7 | 7 |
| gap | G5 | inline method (type of a local val not read) | 2 | 3 |
| gap | G6 | recursive type not read | 241 | 352 |
| gap | G6 | sealed family has anonymous instances in its file | 8 | 13 |
| gap | G6 | receiver space of its public constructors and factories not compared | 5 | 14 |
| gap | G6 | receiver is a sealed family this measurement does not split | 2 | 2 |
| gap | G6 | constructor position mixes type parameters with transparent ground values | 1 | 3 |
| gap | G6 | Array of an abstract element type is not read | 1 | 1 |
| gap | G6 | union _: recursive type not read | 0 | 2 |
| gap | G6 | constructor space has no estimate form | 0 | 1 |
| gap | G7 | inline method (=:= evidence application not read) | 1 | 1 |
| gap | G7 | anonymous evidence selection not resolved | 0 | 3 |
| gap | G7 | existential type constructor requires witness-kind analysis | 0 | 1 |
| gap | G7 | intersection with _ requires a proven singleton identity | 0 | 1 |
| gap | G7 | unsupported evidence relationship | 0 | 1 |
| gap | G8 | binder of an enclosing case clause not read | 1 | 8 |
| gap | G8 | pattern of an enclosing case clause may refine a type | 0 | 7 |
| gap | G9 | union alternatives require an overlap and discrimination proof | 0 | 2 |
| gap | G10 | overridable member may re-enter (view cannot reach the goal; true 0, audited at b5045b8) | 2 | 2 |

### Stored values

| Module | `?` | By design | Gap | BD3 | BD4 | BD5 | BD6 | BD7 | G1 | G6 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| core | 48 | 46 | 2 | 1 | 2 |  | 8 | 35 | 1 | 1 |
| avro | 41 | 41 | 0 | 33 |  |  | 8 |  |  |  |
| circe | 9 | 9 | 0 | 9 |  |  |  |  |  |  |
| jsoniter | 8 | 6 | 2 | 6 |  |  |  |  |  | 2 |
| site | 4 | 2 | 2 |  |  | 2 |  |  |  | 2 |
| generics | 3 | 3 | 0 | 3 |  |  |  |  |  |  |
| zio | 3 | 3 | 0 | 3 |  |  |  |  |  |  |
| chimney | 1 | 1 | 0 | 1 |  |  |  |  |  |  |
| schemes | 1 | 1 | 0 |  |  |  |  | 1 |  |  |
| **total** | **118** | **112** | **6** | **56** | **2** | **2** | **16** | **36** | **1** | **5** |

| Class | Group | Category (reason kind, refined) | First | Any |
|---|---|---|---:|---:|
| design | BD3 | imported name not supplied | 53 | 53 |
| design | BD3 | inherited type member not resolved (unsupplied parent) | 2 | 2 |
| design | BD3 | unknown name (chimney partial.Result via a foreign package import; reason omits it) | 1 | 1 |
| design | BD4 | imported name an unsupplied wildcard may bind | 1 | 1 |
| design | BD4 | inherited type member not resolved (parent an unsupplied wildcard may bind) | 1 | 1 |
| design | BD5 | receiver has no public constructor | 2 | 2 |
| design | BD6 | open to implementations | 16 | 20 |
| design | BD7 | depends on its instantiation | 36 | 49 |
| gap | G1 | unknown name (path type through an inferred val (af.Z)) | 1 | 1 |
| gap | G6 | sealed family not summed (anonymous instance in its file) | 2 | 2 |
| gap | G6 | unknown name (scala.NamedTuple.NamedTuple spelled explicitly) | 2 | 2 |
| gap | G6 | sealed family not summed (subtype binds an existential parameter) | 1 | 1 |

The 6 stored gap rows: `optics.Optional` (sealed family not summed (subtype binds an existential parameter)); `optics.ComposedTraversal#X` (unknown name (path type through an inferred val (af.Z))); `jsoniter.JsoniterInPlacePrism` (sealed family not summed (anonymous instance in its file)); `jsoniter.InPlaceFocus` (sealed family not summed (anonymous instance in its file)); `docs.avrodocs.NameAge` (unknown name (scala.NamedTuple.NamedTuple spelled explicitly)); `docs.NameAge` (unknown name (scala.NamedTuple.NamedTuple spelled explicitly)).
Three of them (`optics.Optional`, `jsoniter.InPlaceFocus`, `jsoniter.JsoniterInPlacePrism`) are
generic templates that would read "depends on its instantiation" once the refusal is lifted;
`ComposedTraversal#X` depends on the class's parameters; only the two `NameAge` aliases, a named
tuple of ground types spelled `NamedTuple.NamedTuple[…]`, would become numbers.

### The largest gap categories

"First" = rows filed under the category; "pure" = rows with no by-design reason that carry it;
"all" = every `?` row carrying it.

| # | Gap category | First | Pure | All | Where | Rule status | Example and why the model has a number |
|---:|---|---:|---:|---:|---|---|---|
| 1 | recursive type not read | 241 | 251 | 352 | core 118, laws 114, schemes-laws 5, avro 2, schemes 2 | parked on main: [D4](../research/code-cardinality-foundations.md#d4-recursion-requires-productivity) reads a recursive type as a least fixed point; the cycle guards refuse rather than count a recursive record over its first-order atoms ([D40](../research/code-cardinality-foundations.md#d40-a-value-built-before-the-receiver-is-not-the-receiver)) | `laws.GetterLaws.reference: S => A` reads the record `Optic`, whose members mention `Optic` again; avro `WholeRecordBuilder.OptionPlan.<init>` over the recursive `FieldPlan` |
| 2 | sum elimination over an unbounded argument space | 13 | 13 | 13 | core 7, schemes 6 | D25 limit | `optics.Prism.optional[S, A](getOption, reverseGet)` |
| 3 | refined representation unavailable (constraints not erased) | 12 | 12 | 17 | core 7, laws 5 | D18 adopted transparent refinements; the record reading does not take them yet | `optics.Indexed.unit[A, B]: PGrate[A, B, A, B, Unit]`, read as `Indexed[A, B, A, B, Unit] { type X = Unit }` |
| 4 | sealed family has anonymous instances in its file | 8 | 8 | 13 | jsoniter 8 | analyzer limit: D15 refuses a family with an anonymous instance in its file | jsoniter `InPlaceFocus`, whose file builds anonymous instances of the family |
| 5 | Higher-order application is unsupported | 8 | 8 | 8 | core 8 | solver limit | `optics.Modify.andThen[C, D](inner): Modify[S, T, C, D]`: the environment is readable; only the application step is missing |
| 6 | inferred result type not resolved | 7 | 7 | 7 | core 7 | argument-type matching is not adopted, a model decision ([environment completeness §5](../research/environment-completeness.md)) | `optics.Lens.apply[S, A](get, enplace): <inferred>` calls `pLens`, whose result is itself inferred |
| 7 | receiver space of its public constructors and factories not compared | 5 | 6 | 14 | laws 4, core 1 | [D42](../research/code-cardinality-foundations.md#d42-a-class-no-callable-constructor-builds-is-counted-by-its-public-factories): the largest space a factory builds, compared only where every space is closed | `optics.Review.andThen[F[_], D](inner: Unfold[B, D, F])`: `Unfold`'s constructor is private, so its factories build it |
| 8 | polymorphic capability binder | 5 | 5 | 5 | laws 4, core 1 | D24; "not determined" needs coverage before a count above 1 | `laws.data.AffineLaws.functorIdentity(fa: Affine[X, A])(using FF: ForgetfulFunctor[Affine])`: `map`'s binder is not determined by its result |
| 9 | capability record with abstract type member not read | 4 | 4 | 13 | core 4 | adopted (D18: existential opening of member-bearing parameters), not implemented for records (D20) | the anonymous optic `data.MultiFocusK.tuple2multifocusPSVec` builds reads `o.X`, the record `Optic`'s abstract member |
| 10 | inline method | 3 | 3 | 4 | core 3 | analyzer limit of the inline body walk (D27) | `optics.MendTearPrism.fuseToMendTear` selects on a local value whose type is not read |
| 11 | unavailable stable path type | 2 | 2 | 4 | core 2 | D18 paths through a value are not followed yet | `optics.ComposedTraversal.to(s: S): MultiFocus[PSVec][X, C]` through the path `af` |
| 12 | overridable member may re-enter | 2 | 2 | 2 | core 2 | v1 §2 irrelevance clause; a lost correct number (known limit below) | `optics.Optic.to` / `Optic.from`: true 0, derived independently by the `b5045b8` audit |

### What would move the by-design rows

None of these is analyzer work under the current contract; each is a scope decision.

| Lever | Rows whose every by-design reason is in that group | Of which also carry a gap |
|---|---:|---:|
| dependency declarations (BD3): the TASTy and Java stubs the runner's classpath argument and the build plugins render ([bounded dependency resolution](../plans/bounded-dependency-resolution.md)); this run reads the sources only | 987 | 103 |
| compile-time code (BD1; outside the model, never) | 83 | 24 |
| reading constrained receivers and hidden state (BD5; D19, D35 and D42 keep them `?`) | 13 | 0 |
| a dependency **name** index for the wildcard imports D33 does not list (BD4; owner decision) — core 5, chimney 2 | 7 | 4 |
| what the closed world leaves open (BD6; members a supplied template overrides): only an irrelevance proof per row | 7 | 5 |
| a constructor row's `Throwable` slot (BD6 `open to implementations: Throwable`; the value-space convention, [plan §2, output meanings](../plans/v1.md#assumptions-and-output-meanings)): only a change of that convention | 7 | 1 |
| garbage-collector state and open key types (BD2; outside the model, never) | 3 | 0 |
| (rows with reasons in two or more by-design groups) | 110 | — |

## Known limits

Rows whose true number is known or expected but which stay `?`, readings that are sound but not
yet precise, and three name lookups that print a wrong number on reduced fixtures. Every bullet was
re-derived on main `36cd3cd`: the eo rows from this run, the rest by running the analyzer on the
fixture shown.

- **`Optic.to` / `Optic.from`** (`overridable member may re-enter the measured declaration:
  Optic.andThen`). The true count is 0: the `b5045b8` audit derived it independently (every
  `ReadCompose` fixes `Out` away from the goal). Supplied templates override `andThen`, so under the
  closed world ([D39](../research/code-cardinality-foundations.md#d39-the-supplied-sources-are-the-whole-program-closed-world)) it stays a
  capability and its possible re-entry a view, and the analyzer does not establish that the view
  cannot reach the goal.
- **A class reading its companion's type member.** Scala does not bring a companion's members into
  the class's scope, or the class's into the companion's; the analyzer does, ahead of a package
  member of another file:

  ```scala
  // a.scala
  package p
  class C { def m(t: T): T = t }
  object C { type T = Boolean }

  // t.scala
  package p
  type T = Unit
  ```

  Scala 3.9.0 types `T` in `C` as the package's `T`: `def m(t: T): Boolean = t` is a type mismatch
  there (found the package's `T`, required `Boolean`), and `def isUnit(t: T): Unit = t` compiles
  with no value-discard warning.
  So `C.m` is `Unit => Unit`, true **1**; the analyzer prints **4**. The same holds from the companion's side
  (`class C { type T = Boolean }; object C { def m(t: T): T = t }`: true 1, printed 4) and for an
  object member (`def k(x: Inner.T): Inner.T` in `class C` with `object C { object Inner { type T =
  Boolean } }`, and a top-level `object Inner { type T = Unit }` in another file of the package: true
  1, printed 4). With the package's `T` or `Inner` in the same file as `C`, the analyzer prints the
  true 1 in all three forms.
- **A self type's member.** In `trait A { self: B => … }` a name `B` declares is a member of
  `A.this`, ahead of a package member of another file; the analyzer reads the package member:

  ```scala
  // a.scala
  package p
  trait B { type T = Boolean }
  trait A { self: B => def m(t: T): T = t }

  // t.scala
  package p
  type T = Unit
  ```

  Scala 3.9.0 types `T` in `A` as `A.this.T`, `B`'s `Boolean`: `def isBoolean(t: T): Boolean = t`
  compiles there. So `A.m` is `Boolean => Boolean`, true **4**; the analyzer prints **1** (also with
  `final class K extends A with B` beside it). A fix of both lookups on the session's branch changed
  no cats-eo count; it was not ported.
- **A path's first name that an enclosing export defines** is neither followed nor refused; the
  analyzer reads another declaration of the name:

  ```scala
  package p
  object Holder { object Inner { type T = Boolean } }
  object Inner { type T = Unit }
  object User {
    export Holder.Inner
    def g(x: Inner.T): Inner.T = x
    val t: Inner.T = true
  }
  ```

  Scala 3.9.0 compiles it, binding `Inner` in `User` to the export, `Holder.Inner`, and
  `val w: Inner.T = ()` there is a type mismatch (found `Unit`, required `User.Inner.T`). So
  `User.g` is `Boolean => Boolean`, true **4**, and `User.t` true 2; the analyzer prints **1** for
  both. The measured sources have two exports, `export ForgetK.{given, *}` in `object Forget` and
  `export MultiFocusK.{given, *}` in `object MultiFocus`; neither encloses a path that starts with
  a name it defines.
- **Alias parents** (`type B = Base; final class Sub extends B { def f(x: Int): Int = x }` with
  `trait Base { type Int = Boolean }`) are refused as unsupplied parents (`inherited parent declaration
  not resolved: B`; true 4). No eo row carries the reason.
- **Constructor rows that read value spaces** keep the owner's convention and may print `ε₀` or a
  tier sum (`final class F(f: String => String)` prints `ε₀`, `final class S(a: Either[String, Int])`
  `ω + ≤2^32`); see [plan §2](../plans/v1.md#assumptions-and-output-meanings).
- **Runtime.** The runner analyzes the source set three times (forward, reversed, and through
  `Report.of`). Its sbt task took 18 s at main `613f655`, 74–88 s at this work's last integration
  commits and 32 s at main `36cd3cd`, on a 4-CPU machine shared with other builds.

## Independent verification

Every number this work added was checked by someone other than the feature that produced it, against
the documented contract and the eo source, never against the analyzer. The audits cover the revisions
named below; the count changes on main since `4eabd23` (the last history row) were not audited by this
work.

- **Full numeric audit at `b5045b8`.** Every numeric row of that run, 443 in all (247 method rows:
  152 finite and 95 ω; 196 stored rows with a size), was derived independently by 13 auditors, one
  per chunk of about 34 rows. Each suspected wrong count went to three verifiers with different
  lenses (the model, a reproduction fixture run at that revision, an independent derivation),
  confirmed at 2 of 3. Result: **0 wrong**; one contested row (`CanPlace.place`, auditor ω) where
  the analyzer's 1 was right.
- **Delta audits at each later landing**, every row that gained a number: `e039cd6` 9, `ad931e2`
  27, `382191a` 1 (audited at `fb5d936`), `cd2c845` 13, `b1eefb9` 4, `ecc47dd` 19, `08dcde1` 127,
  `83f5e76` 10 (4 method, 6 stored), `7e3e306` 12 (`280ab80` changed no count). **222 rows: 219
  correct, 3 wrong**, all at `ad931e2` and each fixed later: `chimney.Chimney.iso` and
  `Chimney.prism` printed 0 for inline methods that expand chimney's macros (true `?`; refused from
  `382191a` on, on main with `inline method: reads …, a declaration not supplied`), and
  `data.ModifyF.map.map` printed 1 where an unsupplied `cats` wildcard may bind its record's field
  types (true `?` under D16 at the time; refused from `83f5e76` to `7e3e306`). Main reads
  `ModifyF.map.map` 1 again, under register
  [D33](../research/code-cardinality-foundations.md#d33-a-listed-dependency-namespace-binds-only-the-members-its-artifacts-declare) (a listed
  namespace binds only the members its artifacts declare); that reading was not audited by this work.
  The 4 rows that lost a number at `83f5e76` were each confirmed an honest `?`.
- **Integrated review at `83f5e76`.** A fresh random sample of 72 numeric rows (48 method, 24
  stored) was re-derived: **72 of 72 correct**. Its adversarial lenses also found ten root causes of
  wrong counts on reduced fixtures, none reaching an eo row: inline members of a never-built static
  owner; local definitions re-entered through their enclosing member or constructor; abstract
  givens re-entered by implicit search; a hidden capture held by a parent's constructor `val` or an
  own member; enum explicit companions; package-object parents and other-file top-level exports on
  the stored side; inherited and self-type members on the stored side; opaque references; value
  rows of an empty declared type; non-final case-class leaves of sealed families. `280ab80` and
  `7e3e306` fixed all ten, each with positive and adversarial regressions (register D29–D31).

## Caveats

1. Gap-first rows are an upper bound on what closing gaps would number: a reading that stops at the
   first gap can hide a second. 349 of the 354 rows that carry `recursive type not read` name the
   `Optic` record; once a recursive record is read they may meet a polymorphic binder the result
   does not determine (D24) or another gap.
2. `overridable member may re-enter the measured declaration` is filed as by design, as v1 §2 allows
   a number only once irrelevance is established; only the two audited rows (`Optic.to` /
   `Optic.from`) are filed as gaps. The other rows carrying it (`Optic.andThen`, its anonymous
   optic's `to` / `from`, `Optic.cross`) also carry a by-design reason.
3. The classification is by reason text. Categories that mix causes are split by their detail:
   `outside the counting model` (quotes against garbage-collector state), `inline method`
   (compile-time operation or call-site dispatch, unsupplied callee, wildcard, or a gap), `inferred
   result type not resolved` (overriding an unsupplied declaration, an import-bound callee, or
   argument-type matching), `unresolved type` (all wildcard doubt) and the `inherited … member not
   resolved` pair (unsupplied parent against a parent a wildcard may bind).
4. Sources only. With the dependencies' declarations supplied as stubs, many by-design rows of BD3
   would read differently; the per-row effect of stubs on this run is not recorded here.

## Reproducing

```bash
git clone --branch v1.0.0 https://github.com/Constructive-Programming/eo.git
sbt "core/Test/runMain cardinality.EoCheckout <checkout-directory> <output-directory> <revision>"
sbt "core/Test/runMain cardinality.EoDelta <previous-output-directory> <output-directory>"
sbt "core/Test/runMain cardinality.EoBaseline <cats-eo_3-0.16.0-sources.jar> <output-directory> <revision>"
```

The classification above is a function of the two TSVs: a reason's category is the report's
blocker category, and the per-category tables are the verdicts. As always, the numbers are analyzer
output; the audits above are the evidence that they are right where they reach, and the five reviewed
model counts stay with the [0.16.0 review ledger](eo-core-0.16.0-review.md).
