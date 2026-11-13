# Compiled own types: reading the module's TASTy

**Status:** phases 1 to 3 implemented (2026-11-10/11). Phase 4 (the boundary, the differential
gate, row identity) is implemented with a first slice of the TASTy front end; phases 5 and 6 are
planned. A TASTy front end replaces the scalameta one (owner decisions, 2026-11-11, under
"Decisions"). Where the work stands and how to continue: "Front end: design and status" at the end.

## Goal

The analyzer reads sources with scalameta and never runs the typer. Wherever a fact depends on
type inference, the syntax reading either rebuilds part of the compiler or refuses with `?`. The
build tools compile the module anyway, and its TASTy holds every one of those facts as the compiler
settled them. Read them from there.

Dependencies already work this way: their TASTy is rendered as signature stubs
([bounded-dependency-resolution.md](bounded-dependency-resolution.md)). This plan applies the same
idea to the module being measured.

## Contract (rule T, v1.md §2)

A member written without a result type has the type the compiler gave it, read from the module's
own compiled TASTy when the build supplies its classes. That is the compiler's answer, so it counts
as typed soundly.

The type is not used when:

- **No compiled member matches.** A match needs the same source file, a member at the same
  name offset, the same name, and the same end of definition. Stale or foreign classes therefore
  refuse.
- **The type has no spelling here.** `StubTypes` cannot spell it, the spelling spans lines, or it
  does not parse.
- **The member is `transparent inline`.** Its callers see the type of the expansion (rule X).

Reports over sources that do not compile, and `cardinalityReportOf` over arbitrary paths, have no
compiled types: their untyped members refuse (`written without a type, in a source not compiled`).
The syntax inference that used to guess them is deleted (phase 3).

## Phase 1: untyped template and top-level members (done)

- `core/.../tasty/CompiledTypes.scala`:
  - loads a tasty-query context over the JDK, the module's output and its classpath;
  - indexes every member of the module's own classes by source file and name offset;
  - writes `: <type>` into each untyped `def`, `val` or `var` that matches, before the closed world
    and the rest of the analysis read the source.
- `StubTypes` gains `rooted`. Every package path is spelled from `_root_`, so a package the source
  declares cannot shadow it.
- `Report.of` and `Report.query` take `classes: Option[CompiledTypes.Classes]`.
- sbt reads `Compile / exportedProducts` (so it compiles first) together with
  `cardinalityDependencyClasspath`. If the sources do not compile, it logs a warning and reports
  from syntax.
- Mill reads `compile().classes` together with `compileClasspath()`. A module that does not compile
  fails the report, because Mill has no task-level failure recovery.
- Regressions:
  - `CompiledTypesSpec`: a tuple and a `map` are refused from syntax and read 4 and ω compiled; the
    final literal stays 1 value; a shifted source stays refused; `transparent inline` stays `?`.
  - The Mill spec checks `Q.pair` = 4.
- Self-measurement (core): `<inferred>` rows went from 426 to 55. 30 rows moved from `?` to a
  number, each a member whose compiler type is now read. Examples: `AnalysisWork.remaining:
  Long` is 2^(2^103) over its four captures; `GoalSolver.absurd: mutable.Map[Int, Int]` is
  4294967297^(2^96). Totals went from `62ε₀ + 675ω (3483 ?)` to `62ε₀ + 703ω (3471 ?)`, and that
  includes the new code's own rows.
- Known cost: report rows print the compiler's rooted spelling (`_root_.scala.Boolean`).

## Phase 2: locals, anonymous classes and pattern names (done)

- The TASTy index walks every tree of a class (`TreeTraverser`), not only its template, so local
  `def`s and `val`s, members of a `new T { … }` and the names of a pattern definition are found
  by the same name, offset and end match.
- The source walk visits every untyped `def`, `val` and `var`, wherever it is. A name inside a
  pattern is typed in place: `val (a, b) = …` becomes `val ((a: A), (b: B)) = …`.
- `SourceIndex` reads a name typed in its pattern as that name's type, compiled or hand-written.
  Before, every name of a pattern definition refused (`a name of a pattern definition`).
- A type that spells a compiler-made name (`$anon`, `x$1`) is not written.
- Named lambda parameters are typed too: in place when parenthesized, wrapped (`(x: T) =>`) when
  bare. No row of the self-measurement moves today; the types are kept for the body readings that
  will use them (owner decision, 2026-11-10).
- Regressions (`CompiledTypesSpec`): local `pick` `?` → 4; an anonymous member typed; pattern
  names `left` 2 and `right` 4, refused from syntax; a hand-written `val (a: Boolean, b: (Boolean,
  Boolean))` reads 2 and 4 without compiled classes.
- Self-measurement (core): `<inferred>` rows went from 55 to 0. Four rows moved from `?` to ω:
  the anonymous algebra's members in `ClosedSpaces.tiers` (`empty`, `plus`, `power`, `times`).
  Totals went from `62ε₀ + 703ω (3473 ?)` to `62ε₀ + 710ω (3469 ?)`, with the new code's own rows.

## Phase 3: delete the syntax inference (done, owner decision 2026-11-11)

Compiled classes are now the only source of an inferred type. Where a member has none, it refuses.

- Deleted: `OverriddenResults` (210 lines) and the body reading of `InferredResults` (523 → 78
  lines), plus the helpers only they used (`LiteralTypes.className`, `TermBindings.beside`).
  `ContextBinders` refuses an enclosing result that is neither written nor compiled. Net production
  change: −655 lines.
- `StubTypes` spells an empty-package type by its bare name (test fixtures declare no package).
- `CompiledTypes` reads the running JDK once per JVM, not once per call, and keeps every compiled
  member at a name offset (a `var`'s getter and setter share it).
- Tests: `cardinality.Compiled` compiles each fixture in-process with `scala3-compiler` (a test
  dependency) before analysis, in parallel, cached on disk by the sources' hash under
  `core/target/compiled-fixtures`. A compile that does not finish in 60 s counts as rejected
  (3.9.0's typer does not finish the 70-alias `ContextBindersSpec.limits` fixture). Core tests
  fork and get the test classpath through `-Dcardinality.test.classpath`.
- Every changed expectation is derived from the compiled type in its spec's comment
  (`InferredResultsSpec`, `PureCodeSpec`, `SyntaxFeaturesSpec`, `ContextBindersSpec`, …).
  Fixtures that name an unsupplied declaration on purpose no longer compiled, so their untyped
  members refused; most now compile against withheld support ("Gate assets").

The override-identity and given-name code first listed under this phase needs symbol facts a
source rewrite cannot carry. The TASTy front end (phase 5) answers both from symbols.

## Measured (2026-11-11)

One frozen corpus: core's 174 sources and its compiled classes at `53ab312`, with core's
dependency classpath. Each run is a fresh JVM (`-Xmx16g -Xss8m`, JDK 25, 32 cores) calling
`Report.of`; medians of 3 runs (2 for the runs without classes). Reports were byte-identical
across a version's runs.

| Version | Classes | Signatures: finite · ω · ε₀-tier · `?` | Total | Wall | CPU | Peak heap |
| --- | --- | --- | --- | --- | --- | --- |
| start `55f6ea1` | — | 200 · 505 · 31 · 3264 | `62ε₀ + 681ω` (3505 `?`) | 107.4 s | 473.6 s | 8.8 GiB |
| mid `53ab312` | yes | 202 · 535 · 31 · 3232 | `62ε₀ + 711ω` (3473 `?`) | 97.0 s | 428.0 s | 8.6 GiB |
| end `ffb94c1` | yes | 202 · 535 · 31 · 3232 | `62ε₀ + 711ω` (3473 `?`) | 98.1 s | 431.5 s | 9.0 GiB |
| mid `53ab312` | no | 200 · 505 · 31 · 3264 | `62ε₀ + 681ω` (3505 `?`) | 94.9 s | 426.9 s | 8.2 GiB |
| end `ffb94c1` | no | 174 · 465 · 31 · 3330 | `62ε₀ + 641ω` (3571 `?`) | 94.1 s | 403.4 s | 9.3 GiB |

- With classes, mid and end are byte-identical: the deleted inference ran only where no compiled
  type was written. From start, 32 rows move from `?` to a number (30 to ω, `AnalysisWork.remaining`
  to 2^(2^103), `GoalSolver.absurd` to 4294967297^(2^96)); none moves the other way.
- Without classes (`cardinalityReportOf`, sources that do not compile), end refuses 66 rows mid
  inferred (40 ω, 11 2^32, 8 one value, 7 larger finite): every change is a number becoming `?`,
  never another number. The definitions table is the same in all five runs.
- Reading compiled types costs about 3 s of wall time (94 s without classes, 97–98 s with).
  start → mid is about 10% faster; the likely cause is the closed-world fix (`53ab312`), which
  stopped walking about 1,300 unrelated JDK templates for scalameta members; not isolated.
- Core's suite: 87 specs in about 100 s with the fixture cache warm (1,490 cached fixtures).

## Working rules for phases 4 to 6 (owner, 2026-11-11)

- **Compiled code must not degrade.** For a module the build compiled, no row may go from a number
  to `?`, and no number may change, without a derivation showing the old number was wrong. The
  gates are the frozen self-measurement corpus (byte-identical report, or every moved row
  explained) and core's suite.
- **Uncompiled support is a future task.** No code is kept for the sake of sources without compiled
  classes. They still run, and whatever needs a compiled fact refuses.
- **The goal is deletion.** The new front end is not done until the scalameta one it replaces is
  deleted.

## Decisions (owner, 2026-11-11)

| | Decision | Chosen |
| --- | --- | --- |
| D1 | How compiled facts reach the analyzer | **A TASTy front end.** The analyzer reads the compiler's symbols and typed trees directly. The scalameta front end, the stub rendering and the source rewriting (`CompiledTypes`, `ClosedWorld`'s text edits) are deleted when it has taken over. Not chosen: a symbol index beside the scalameta trees; source rewriting only. |
| D2 | D43, the conversions and extensions a body may meet | **The conversions the compiled body applies.** A subset of the old candidates, so refusals only leave. |
| D3 | A builtin name the compiler binds differently from "builtin names are never shadowed by what is not supplied" | **The compiler's binding wins.** The rule is kept in the contract for uncompiled sources only (a future task). |
| D4 | A given left to implicit search in an inline body | **Read the given the compiler resolved.** Refuse only when that given is inline with a compile-time operation, as for a written call. |
| D5 (owner, via the coordinator, 2026-11-13) | A construction, body or default that throws, and a pattern definition | **Not refused: AGENTS.md's "Ignore throwing".** A construction that always throws builds no value, and the count follows from that, as the old front end reads it (register D26). A pattern definition is an ordinary binding; one that may not match is a possible throw, which is ignored. Applies wherever the compiled front end refused only for those reasons (the stored side's value rows, `Sums`, records, receivers). The 8 constructor rows refused for "sealed subtype hides a parameter" / "shape unreadable" stay as they are. |
| D6 (owner, 2026-11-13) | Re-entry among a capability record's members through another instance | **Counted.** A record value the body receives as a parameter or capture is universally quantified, so it is a free capability even where it may be an instance of the measured member's class: by parametricity the implementation must work for every value. Only same-instance re-entry refuses: the value is `this`, or an instance of the record's class the module builds or names (a `new`, an object, a factory; anywhere in the module, which only refuses more) reaches the site, or `this` is handed to code that may call back (`List(this).sorted(c)`). AGENTS.md is unchanged: the model never had the refusal. |
| D7 | Selected reports under the TASTy front end (coordinator, 2026-11-13, as Q11) | **Port:** `Report.query` is the module's report filtered by qualified name; the budgets that only bound the old search go away. The scripted `selected` test stays, its expectation rewritten where counts differ by listed corrections. |
| D8 | `cardinalityReportOf` without uncompiled support (owner, 2026-11-13) | **Read the library's binary jar**, which carries TASTy; its sources name and place the rows. Both plugins and the scripted `library` test change accordingly. |
| D9 | The scripted `dependencies` test (coordinator, 2026-11-13) | **A new expectation under the TASTy front end**, derived from the compiled reading. |
| D10 | The ledger and gate harnesses (coordinator, 2026-11-13) | **Keep the old front end until parity is certified**; rewrite `Compiled`'s `filled` mode and `Differential` at the switch itself, not before. |
| D11 | The 54 spec examples on fixtures scalac rejects (coordinator, 2026-11-13) | **They go with uncompiled support** (a future task). At the switch they move to the pending list ("Switch-over: pending examples"), not silently deleted. |

## Where the boundary is

About 30,000 of core's 36,800 lines read scalameta trees. What does not:

- the goal solver (`analysis/inhabitation`, 2,557 of its 3,082 lines): `Shape`, `Binding`, `Count`;
- the size algebra (`capacity/Size`, 341 lines);
- the rows a report is made of: `MethodAnalysis.Entry` (path, name, signature, line, kind, count,
  captures) and `capacity.Definition` (name, kind, params, size, unbound, line).

So the front end's whole job is: from a compiled module, produce for each target its environment
(`Binding`s) and goal (`Shape`), and for each definition its `Size`, with the row's name, signature
and line. The back end stays as it is. Everything between the trees and that boundary (name
resolution, scopes, imports, inheritance, the stdlib model's lookup by name, inline-body and
conversion reading, the dependency stubs) is rewritten over symbols, where most of it is a symbol
or type query instead of a reading of syntax.

The model rules are not compiler facts and move with their meaning unchanged: the closed world
(finality is still the analyzer's, never a TASTy flag), rules B, F and X, top types by place, the
capability reading of abstract and overridden members, lawful key equality, the size algebra.

## What TASTy holds, and what it does not

TASTy is the compiler's tree after type checking and before inlining and erasure. Every Scala 3
language feature is in it, typed: opaque types, match types, type lambdas, unions, refinements,
singletons, context functions, givens, extensions, enums, exports, inline definitions with their
bodies, `inline if` / `inline match`, quotes and splices. tasty-query 1.9.0 models each as a tree
node or a symbol flag.

What it does not hold, and what that costs the count:

| Not in TASTy | Effect on counting |
| --- | --- |
| Source that does not compile | Nothing to read. Accepted: uncompiled support is a future task. |
| Surface syntax: sugar is desugared (a `for` is `map`/`flatMap` calls, a context bound an evidence parameter, a pattern definition a synthetic value, a default argument a `f$default$n` method, an enum a sealed class and companion), comments, layout | None on the count. Row names and signatures are syntax: the source is still read for display, and compiler-made members (default getters, export forwarders, case-class members, derived givens) must be told from written ones by their flags so the row set stays the same. |
| What later phases produce: the expansion of a non-transparent inline call or macro, macro-annotation output, bridges, erasure | None: the contract already refuses compile-time operations and never read these. A `transparent` call is the exception: it arrives expanded, with the call kept beside the expansion. |
| Bodies of Java and Scala 2 classes (class files and pickles hold signatures only) | None: the scalameta front end never read `.java` sources, and dependencies supply no bodies today. |
| Constant expressions the type checker folded | None found: the contract reads bodies for inline expansion, factories, constructor statements and throws, not for arithmetic. To watch in slice 5d. |

Gained, not lost: a type with no source spelling (an anonymous class, a captured type variable) is
readable; names, overrides and parents are exact; a Scala 3 dependency's inline bodies are in its
TASTy; literal constants are exact (the parser loses the sign of `-0.0`).

## Phase 4: the boundary and a differential gate (done)

No behaviour change. This is what makes "compiled code must not degrade" checkable during a
rewrite: the old front end is the oracle until the new one matches it.

| Step | What |
| --- | --- |
| 4a. Front-end interface | One entry point the report calls, returning `Entry` and `Definition` rows for a compiled module. The scalameta pipeline becomes its first implementation, unchanged. |
| 4b. Differential runner | Runs two front ends over the same input and classifies every row: identical; better (`?` → number); **worse** (number → `?`, or a different number). Inputs: the frozen self-measurement corpus and every compiled test fixture in `core/target/compiled-fixtures`. |
| 4c. Row identity | Row names today come from syntax (`<block#1>`, `<new#1>`, `<lambda#1>`, `<extension@4>`). Fix how a symbol gets the same name, so rows pair up; pair by source position where a name cannot match. |

## Gate assets (2026-11-11)

What phases 4 to 6 are measured against, and how to rebuild it. Everything here is the scalameta
front end's: the costs at `5f89e91`, the rows at `1358b07`, the fixtures at this commit.

### Commands

| Command | What it does |
| --- | --- |
| `scripts/measure-corpus.sh <corpus> <report file> [runs]` | One `Report.of` per run over a frozen corpus with this checkout's build, each in a JVM of its own (`bench/runMain cardinality.bench.CorpusRun`, `-Xmx16g -Xss8m`). Only the call is timed; it includes rendering the dependency stubs. Prints each run's wall time, CPU time, GC time, peak heap and peak RSS, the median and range of the first, second and fourth, the rows by kind, and what the report shows of the compiled types; writes the rendered report; fails when two runs render different reports. |
| `scala-cli run .claude/skills/cardinality-reduction/report-diff.scala -- <before> <after> --rows` | Totals of two rendered reports and every row that moved. |
| `scripts/build-corpus.sh <new corpus directory> <organization:artifact_3:version>` | A corpus from a published artifact: its binary jar unpacked as `classes/`, each source of its sources jar at the path its TASTy records, and its dependency jars in `classpath.txt`. Nothing is compiled. |
| `rm -rf core/target/compiled-fixtures && sbt core/testFull` | Compiles every test fixture afresh; the entries are then corpora a differential runner can read, and `ls core/target/compiled-fixtures/*/failed` are the rejected ones. |
| `scripts/spec-ledger.sh <ledger file> [work directory]` | Runs `core/testFull` under each mode of the harness's front-end switch and writes, for each example that reads rows, whether it passes with the compiled front end's rows, waits for rows not read yet, or fails ("Specs against the compiled front end" below). The ledger at this commit is `docs/baselines/tasty-spec-ledger.tsv`. |
| `scripts/differential.sh <assets> <output directory> [input ...]` | The differential gate of the front-end rewrite over every corpus and the compiled fixtures: "Front end: design and status", "The gate". |
| `scripts/dead-code.sh` | Compiles every module and its tests, then lists the production symbols of `core` that nothing references, from the build's SemanticDB (`scripts/dead-code.scala` says how to read the three lists). |
| `sbt coverage "bench/runMain cardinality.bench.CorpusRun <corpus> <report file>" core/coverageReport` | What one corpus executes. Delete `target/out/jvm/scala-3.9.0/scala-cardinality/scoverage-data/scoverage.measurements.*` first; the report beside that directory lists every package. The task ends in an error because the run is below the suite's coverage floor: the report is written before that. |

A corpus is a directory with its sources (each at a path that ends with the one its TASTy records),
`classes` (the compiled output of exactly those sources, a directory or a jar) and `classpath.txt`
(one dependency jar per line). The `TYPES` line of a measurement is the check that sources and
classes match. `ascribed` counts the method rows whose signature spells a type from `_root_`, as the
compiler's types are written: each matched a compiled member by name, offset and end. `inferred`
counts the rows still printed `<inferred>`. A corpus whose sources are not where their TASTy records
them reads `ascribed=0` with every untyped member `inferred` (cats-eo with one directory renamed:
`ascribed=0 inferred=19`, against `ascribed=17 inferred=0`). A few `inferred` rows beside many
`ascribed` ones are members whose compiler type has no spelling in source. Code that writes every
type shows nothing either way: the check works through untyped members only.

### Corpora

All under `/var/cache/users/rhansen/cardinality-tasty/`, with the report of each in
`baseline/<name>-report-<commit>.txt` beside them: `5f89e91`, and `1358b07`, the baseline to compare
against.

| Corpus | Built from | Sources |
| --- | --- | --- |
| `corpus-core` | core's own sources and classes at `53ab312`, copied by hand with core's dependency classpath | 174 |
| `corpus-cats-eo` | `dev.constructive:cats-eo_3:1.2.1` | 55 |
| `corpus-cats-eo-generics` | `dev.constructive:cats-eo-generics_3:1.2.1` | 5 |
| `corpus-cats-eo-schemes` | `dev.constructive:cats-eo-schemes_3:1.2.1` | 2 |
| `corpus-cats-kernel` | `org.typelevel:cats-kernel_3:2.13.0` (Scala 3.3.4) | 95 |
| `corpus-cats-core` | `org.typelevel:cats-core_3:2.13.0` (Scala 3.3.4) | 245 |

Rows by kind, and the cost of one report (median of three runs, lowest and highest in
parentheses; JDK 25, 32 cores). The machine was shared while these ran (load average 8 to 36), so
the times are loose: compare two builds only from runs made back to back. Peak heap is the heap
pools' peaks summed, an upper bound that moves from run to run.

| Corpus | Signatures: finite · ω · beyond · `?` | Numeric | Definitions: finite · ω · beyond · `?` · `—` | Wall, s | CPU, s | Peak heap, GiB |
| --- | --- | --- | --- | --- | --- | --- |
| core | 202 · 535 · 31 · 3232 | 768 of 4000 | 280 · 86 · 29 · 241 · 5 | 111.2 (104.1 to 143.3) | 488.2 (457.2 to 555.0) | 8.62 (8.10 to 8.68) |
| cats-eo | 211 · 60 · 0 · 426 | 271 of 697 | 54 · 3 · 0 · 57 · 37 | 30.1 (25.1 to 38.1) | 167.2 (147.8 to 187.5) | 3.44 (3.42 to 3.51) |
| cats-eo-generics | 7 · 9 · 0 · 64 | 16 of 80 | 5 · 0 · 0 · 6 · 1 | 24.4 (20.9 to 28.2) | 112.3 (106.4 to 127.3) | 4.04 (3.45 to 4.62) |
| cats-eo-schemes | 5 · 0 · 0 · 14 | 5 of 19 | 1 · 0 · 0 · 1 · 0 | 15.8 (15.8 to 16.4) | 74.7 (72.6 to 74.9) | 3.08 (3.05 to 3.15) |
| cats-kernel | 280 · 88 · 0 · 1417 | 368 of 1785 | 77 · 0 · 0 · 59 · 176 | 33.2 (29.2 to 34.8) | 120.9 (115.8 to 142.3) | 3.06 (2.93 to 4.09) |
| cats-core | 705 · 204 · 0 · 6742 | 909 of 7651 | 494 · 2 · 0 · 506 · 557 | 65.5 (64.4 to 81.4) | 328.0 (324.0 to 371.9) | 5.09 (4.70 to 5.15) |

- The three runs of every corpus rendered the same report. Core's is the earlier baseline
  (`core-report-ffb94c1.txt`) byte for byte once the location prefix is removed: the harness in
  the repository reads what the earlier script read.
- Compiled types reach every corpus: 423 rows print one in core, 17 in cats-eo, 4 in
  cats-eo-generics, 61 in cats-kernel, 136 in cats-core, and no row is left `<inferred>` but 5 of
  cats-core's 7,651, whose compiler type has no spelling here. (cats-eo-schemes' only untyped
  member has a literal's type, `512`, which prints no `_root_`.)
- cats-eo-schemes gates almost nothing, and found an oracle bug. `CompiledTypes` wrote a pattern
  name's function type without parentheses (`val ((kids: …), (combine: PSVec[R] => R)) = …`), the
  edited source did not parse, and every type of that file was dropped with nothing said.
  `1358b07` writes the parentheses, reports a source it leaves untyped among the report's
  warnings, and matches a source to the compiled one whose recorded path is the longest tail of
  its own. Against the `5f89e91` baselines the six reports differ in one row:
  `Schemes.OnStackLimit` (`final private val OnStackLimit = 512`) goes from `?` to 1, the one
  value of the constant type scalac gives it. No report prints a compiled-types warning.

What each corpus executes, as the share of a package's statements one report runs (scoverage,
same build):

| Corpus | All of core | `analysis/expansion` |
| --- | --- | --- |
| core | 57.4% | 0.8% |
| cats-eo | 69.1% | 70.0% |
| cats-eo-generics | 49.6% | 3.0% |
| cats-eo-schemes | 38.2% | 0.8% |
| cats-kernel | 47.7% | 0.8% |
| cats-core | 62.0% | 0.8% |
| all six | 79.8% | 70.2% |

Only cats-eo reaches the inline-body and conversion reading: its sources hold the inline methods,
opaque types, extensions and givens the others lack. cats-kernel and cats-core add type-class
code (givens, implicit conversions, higher kinds) and 1,277 numeric rows, but no inline method, so
they move `analysis/resolution` and `analysis/inhabitation`, not `analysis/expansion`. A third of
`analysis/expansion` is run by no corpus and is gated by the test fixtures alone.

### Test fixtures

`cardinality.Compiled` compiles every fixture before the analysis reads it, and a TASTy front end
can read only the ones that compile. A cache entry, `core/target/compiled-fixtures/<digest>/`, is
a corpus of its own: `src` holds each source at the path its TASTy records (`-sourceroot`),
`classes` its compiled output, `support` the classes of what the analysis is not given,
`classpath.txt` what the fixture compiled against beside `support` (one path a line, as a corpus
has it: the two Scala library jars, or the whole test classpath for the 9 fixtures that need it),
and `ok` or `failed` the test that first asked and what stopped the compiler. A compile is written
under `staging/<pid>/` in the cache and moved into place; a test JVM that starts removes what the
ones that ended left there, so a compiler that ran out of time leaves nothing among the entries.

| | Fixtures asked for | Compile | Rejected |
| --- | --- | --- | --- |
| `5f89e91` | 1,493 | 1,290 | 203 |
| `597d7ef` (withheld support) | 1,496 | 1,424 | 72 |
| `26a1e6e` (every spec routed) | 1,976 | 1,892 | 84 |
| `7dd3053` (the ill-typed rewritten) | 1,977 | 1,906 | 71 |
| now (dependencies; counts off rejected fixtures) | 1,994 | 1,933 | 61 |

- **Withheld support.** A fixture's text carries the declarations it names and the analysis is
  not to have: `code + Compiled.withheld("package ext\nclass Schema")`, or one file of a set
  (`Compiled.withheldFile`). The harness compiles the support first, compiles the fixture against
  its classes, and gives the analysis neither the support's text nor its classes. `CompiledSpec`
  pins it: the same declaration is `?` withheld and 4 supplied, and a text that carries support
  does not parse when it is read without `Compiled.inputs`.
- **Every spec goes through it.** The stored-value specs parsed their fixtures and never compiled
  them. They read `Compiled.definitions(code)` (one source's rows; `Compiled.source(code)` where
  an example needs the tree itself) or `Compiled.report(files*)` (a source set on disk read by
  `Report.of` with its classes). A type fragment is read from `type T = …` in a compiled source.
- **A dependency, compiled apart and supplied as classes.** `Compiled.dependency(unit, code)` is
  one source of a dependency unit, a file of the fixture's set. The harness compiles each unit
  alone, against the units before it, into the entry's `dependency/<unit>`, and the fixture
  against them. `Compiled.reportOver(dependencies, files*)` reads the module as a build's report
  does, with a dependency classpath: the units, then `dependencies` (class directories or jars),
  then the Scala library, the first to declare a class winning. Neither a unit's text nor a row
  of it reaches the analysis. `Compiled.report` passes no classpath. Under the switch the compiled
  front end gets that classpath and nothing else the fixture compiled against; the entry's
  `classpath.txt` is not read by the harness, since for the 9 fixtures compiled against the whole
  test classpath it holds what the scalameta reading is not given.
- **One switch for the front end.** The rows a spec reads come from `Compiled.analyze` (method
  rows), `Compiled.definitions` (one source's definition rows) and `Compiled.report` (a source set
  as files), which `InventoryFixtures` and `capacity.Rows` go through. One method, `Compiled.read`,
  decides where they come from, by the system property `cardinality.frontend`
  (`sbt 'set core / Test / javaOptions += "-Dcardinality.frontend=tasty"' core/testFull`): the
  scalameta pipeline when it is unset, which is the only mode the suite is green under, or
  `TastyFrontEnd` over the fixture's classes, its dependencies and the Scala library (`tasty`, and
  `sentinel`, `filled` and `probe`, which a ledger is read from: `FrontEndMode` says what each
  is). The withheld support is not among them, so what a fixture withholds stays unsupplied.
  `Compiled.source`, `ascribed` and a request with dependency stubs given as sources read what
  only the scalameta front end has, and fail under any other mode saying so.
  `Rows.typeSize` (the alias row of `type T = …`) stands where `Counter.type` did in
  `CardinalitySpec` and `ArticleCardinalitySpec`, and `Rows.total` (a source's class, enum, module
  and top-level value rows, summed) where `Counter.source` did in those two, `CounterSpec`,
  `SignatureSpec` and `NamespaceSpec`. No expectation changed.
- **The rejected fixtures are a list.** `core/src/test/resources/rejected-fixtures.txt` holds the
  fixtures scalac is meant to reject, and the suite fails where it and the compiler disagree, so
  it cannot drift. It holds 60: 51 not valid Scala by design (the analyzer must refuse what scalac
  rejects), 5 unsupplied on purpose (3 whose point is a source that does not compile, 2 that
  scalac rejects with the declaration supplied too), 3 naming a declaration given only as a
  dependency stub, and `ContextBindersSpec.limits`, which the compiler does not finish in 60 s.
  One more rejected fixture is the one `CompiledSpec` leaves unlisted on purpose. A line names a
  fixture by the digest of its sources alone; a cache entry's directory is another digest, which
  also covers the compiler and its options. To find an example's entries:
  `grep -l 'NamespaceSpec.scala' core/target/compiled-fixtures/*/failed`.
- **Expectations that moved.** Each is a member written without a type that now has the
  compiler's (rule T), derived in its spec's comment: `Outer.Inner.g` 2
  (`EnvironmentCompletenessSpec`), `val n = 1` an Int, `def f(b: Boolean) = b` and its extension
  twin 4 (`SignatureSpec`, `ExtensionSignatureSpec`), `H#Y` 2 (`NamespaceScopingSpec`),
  `Holder#Y` ≤2^32 (`NamespaceSpec`). One refusal changed its reason, not its verdict
  (`InlineMethodsSpec.searchForms`, `E.literal`). No number became `?`.

The specs routed in `26a1e6e` ask for 534 fixtures, of which 522 compile, counting a fixture for
the first spec file on the stack of the test that asked (the first line of its marker; several of
these specs asked for some before, through `Rows.constructor`): `CardinalitySpec` 153,
`ArticleCardinalitySpec` 54, `SignatureSpec` 23, `GivenSignatureSpec` 17, `ExtensionSignatureSpec`
14, `CounterSpec` 7, `ImportClausesSpec` 13, `LeafFamiliesSpec` 8, `NamespaceScopingSpec` 27,
`NamespaceSpec` 56, `StoredFactoriesSpec` 1, `StoredScopesSpec` 21, `StoredValueSoundnessSpec` 58,
`ThrowableSlotsSpec` 5, `ValueRowsSpec` 2, `ReportSpec` 48, `ReportHygieneSpec` 21,
`OpaqueSumForwardingSpec` 1, `MeasuringSpec` 4, `DeepStackSpec` 1. The 12 rejected at that commit:
five by-name enum-case parameters in `CardinalitySpec` (scalac takes no by-name `val`; a case
class's lazy field is respelled as a plain class's parameter; an enum case's is a thunk since),
five names out of scope or ambiguous by design in `NamespaceSpec`, and one each in
`ThrowableSlotsSpec` and `ReportSpec`.

Not routed, because they test how a report is asked for and not what it counts:
`AnalysisQuerySpec`, `ReportQuerySpec` and `ReportDependenciesSpec` (requests, snapshots and
dependency jars over files of their own), the reading tests of `ReportSpec` and
`ReportHygieneSpec` (a directory, a jar, a missing or unparsable file, where a row is placed),
`TastyStubsSpec`, the `Eo*` specs over the published cats-eo sources, and `CompiledTypesSpec`,
whose fixture sbt compiles with the tests.

### Specs against the compiled front end

`scripts/spec-ledger.sh docs/baselines/tasty-spec-ledger.tsv` runs core's suite under each mode of
the harness's switch (four runs, about a minute each on a warm fixture cache) and writes the
ledger: one line for each example that reads rows, or what only the scalameta front end has, as
`specification<TAB>example<TAB>status<TAB>cause`, sorted, so two commits' ledgers diff line by
line. The script's header says how each status is decided. No expectation was changed for it.

| Status | Examples | Meaning |
| --- | --- | --- |
| PASS | 193 | Passes with `TastyFrontEnd`'s rows, and still passes when every row not read yet holds a count no example expects. |
| UNCOVERED | 1,338 | 1,318 fail with `TastyFrontEnd`'s rows and pass once each row not read yet holds what the scalameta pipeline gives the row of its name: they fail for those counts and nothing else. 20 pass, and fail under the sentinel: what they expect, a row not read yet meets too (a refusal, or a total the row is left out of). |
| FAIL | 99 | Fails with the counts not read yet answered too. The cause column is that failure. |
| PENDING | 19 | A target `ArticleCardinalitySpec` marks as not met yet, under either front end. |
| SCALAMETA | 81 | Reads what only the scalameta front end has: its tree, dependency stubs given as sources. |

The other 317 examples read nothing through the harness (before "Rules of class (b), on rows"
below the totals were 188, 1,252, 108, 19 and 133). What the ledger cannot see:

- An example passes whatever the sentinel is where it only says what a row is not, or where a
  row without a count meets its expectation too. Two are known, and read PASS on rows not read
  yet: `AliasOverrideIdentitySpec`, "keeps a nearer source alias distinct from an outer binder"
  (`c !== Count.Finite(1)`), and `ArticleCardinalitySpec`, "a data type with no constructors" (a
  total of 0, which no row adds to).
- A row is not read yet where every reason it gives starts with `TastyFrontEnd.Uncovered`, and
  the front end files a type that names a class the classpath does not hold under it (23 of the
  UNCOVERED causes). Where a fixture withholds the class, that is the final answer, not a slice
  to come: those examples pin `declaration not supplied` and stay UNCOVERED until the front end
  refuses in its own words.
- UNCOVERED says nothing of what the count will be once it is read, and a PENDING example stays
  PENDING whatever number the compiled front end gives: only its cause changes.
- The cause is an example's first failure. One that reads two fixtures shows one of them, and
  one marked SCALAMETA may assert rows as well.

The rows are the compiled front end's: under `probe` the 1,649 examples of class (a) below fail
at the request; under `tasty` the one method that answers a request calls `TastyFrontEnd.rows`
and nothing of the scalameta pipeline; 1,084 of the UNCOVERED causes quote a reason only
`TastyFrontEnd` gives (the others are a total or a size the missing count leaves short); and a
signature differs where the two spell differently (`pick[A](x: A, y: A)` against the scalameta
`pick[A](x: A, y: A): A`).

**The 99 FAIL examples, by cause.** No example fails on a number that differs from the number
its spec pins.

- **54: a fixture is one scalac rejects** (`no compiled classes`), so there is nothing to read.
  They pin what the analyzer answers for code that does not compile, which a front end that
  reads compiled classes never sees: they are deleted with the scalameta front end unless the
  owner wants those refusals kept another way. Five of them assert a count too ("Counts on, or
  behind, a fixture scalac rejects" below). By spec: `HigherKindedRefusalSpec`
  8, `AliasOverrideIdentitySpec` and `NamespaceSpec` 5 each, `StdlibModelSpec` 4,
  `MethodRefinementSpec` and `SingletonIntersectionMethodSpec` 3 each, six specs 2 each, fourteen
  1 each.
- **25: the spec pins a refusal and the compiled front end gives a number.** Each needs a
  derivation from the compiled types before its expectation may move; none was moved.
  `UnsuppliedWildcardSpec` 11, `ImportClausesSpec` 4, `StoredScopesSpec` 4,
  `GroundEnvironmentSpec` 2, `TopTypesUnionSpec` 2, `PolyLambdaCardinalitySpec` 1 ("refuses
  constrained lambda binders"), `TypeMemberScopeSpec` 1 ("a projection through `this` or an
  owner's member is refused"). All but the last two pin a name the scalameta front end cannot
  resolve (an unsupplied wildcard import, parent or export that may bind it): the compiler
  resolved the name, so the question each derivation has to answer is whether the count may
  rest on a declaration the fixture withholds.
- **9: a signature is spelled differently.** A result written without a type is not spelled
  (`pick[A](x: A, y: A)`; `val made` without `: _root_.ext.Schema`, `CompiledSpec`); a curried
  clause keeps the source's space (`f[A](x: A) (y: A): A`); a braceless match type has no `{`,
  `;` and `}`, and a refinement over several lines no `;`; a literal type's spaces; the order of
  a class row among its members. `ReportHygieneSpec` 6, `EoArtifactsSpec` 2, `CompiledSpec` 1.
- **8: a row has another identity.** A definition inside a `for` or a lambda is not listed as
  `Comp.f.<for#1>.<block#1>.g` (`NestedScopesSpec` 4), a secondary constructor's row is not of
  kind `method` with the signature `this(u: Unit): Open` (`DefinitionInventorySpec` 3), and a
  member of an anonymous class in a context-function literal is not listed as
  `Literal.anon.<lambda#1>.<new#1>.get` (`DefinitionInventorySpec` 1, which a rejected fixture hid
  before).
- **3: two sources are not told apart.** `Exp.scala` and `lib/Exp.scala` in one module:
  `compiled classes not told apart from another source's` (`CompiledSpec` 1, `StoredScopesSpec`
  2). The scalameta front end gives a source the compiled path that is its longest tail
  (`CompiledTypes.matching`).

**Every specification, by what its examples test.**

- (a) rows of a fixture, read through `Compiled`, `InventoryFixtures` or `capacity.Rows`: they
  carry over as they are. The ledger's PASS, UNCOVERED, FAIL and PENDING.
- (b) the scalameta front end's own: what the ledger marks SCALAMETA, and examples that call its
  internals directly (resolution and substitution over scalameta types, the index and its
  budgets, `CompiledTypes`, `TastyStubs`, `Spelling`). They are deleted with it, unless they pin
  a rule of the model that nothing pins on rows (below).
- (c) the back end (the solver on hand-built goals, `Size`, `Natural`), formats that do not
  depend on where rows come from, the compiled front end's own spec: unaffected.
- (c*) reporting and requests, tested by running `Report.of`, `Report.query` or
  `AnalysisQuery.run` over sources without classes. What they test is unaffected; how they are
  run is not, since the compiled front end reads no source without its classes: each needs a
  compiled fixture when `Report.of` changes front end.

| Specification | Examples | (a) rows | (b) scalameta only | (b) other | (c) | (c*) | What the examples that read no rows test |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `AliasOverrideIdentitySpec` | 25 | 25 | 0 | 0 | 0 | 0 |  |
| `AnalysisQuerySpec` | 14 | 1 | 0 | 5 | 0 | 8 | `AnalysisQuery.run` on sources without classes: budgets on the scalameta index, lookup and expansion (b); request semantics (c*) |
| `ArticleCardinalitySpec` | 71 | 71 | 0 | 0 | 0 | 0 |  |
| `BinderBoundsSpec` | 23 | 23 | 0 | 0 | 0 | 0 |  |
| `CapabilitySpec` | 51 | 51 | 0 | 0 | 0 | 0 |  |
| `CardinalitySpec` | 156 | 156 | 0 | 0 | 0 | 0 |  |
| `ClosedWorldSpec` | 7 | 5 | 2 | 0 | 0 | 0 | dependency stubs given as sources |
| `CompiledSpec` | 7 | 5 | 0 | 2 | 0 | 0 | the harness: a text carrying support does not parse as scalameta; what `Overriders` writes |
| `CompiledTypesSpec` | 10 | 0 | 0 | 8 | 0 | 2 | `CompiledTypes` ascription (b); two read rows of `Report.of` with classes (c*) |
| `ContextBindersSpec` | 15 | 15 | 0 | 0 | 0 | 0 |  |
| `ContextualOverrideIdentitySpec` | 18 | 18 | 0 | 0 | 0 | 0 |  |
| `ConversionClosureSpec` | 9 | 9 | 0 | 0 | 0 | 0 |  |
| `CounterSpec` | 4 | 4 | 0 | 0 | 0 | 0 |  |
| `DeepStackSpec` | 1 | 0 | 1 | 0 | 0 | 0 | `MethodAnalysis.analyze` on a small stack |
| `DefinitionInventorySpec` | 32 | 30 | 1 | 1 | 0 | 0 | `EoBaseline.identity` over the tree; `GivenNames.of` over parsed givens |
| `DependencyRowsSpec` | 16 | 16 | 0 | 0 | 0 | 0 |  |
| `EnvironmentBinderSpec` | 5 | 5 | 0 | 0 | 0 | 0 |  |
| `EnvironmentCompletenessSpec` | 37 | 37 | 0 | 0 | 0 | 0 |  |
| `EnvironmentSoundnessSpec` | 45 | 45 | 0 | 0 | 0 | 0 |  |
| `EoArtifactsSpec` | 17 | 2 | 0 | 0 | 9 | 6 | TSV fields, tags and the delta (c); rows of `Counter.definitions` on uncompiled parses (c*) |
| `EoCaseLedgerSpec` | 11 | 10 | 0 | 0 | 1 | 0 | the archive checksum |
| `EvidenceTransportCardinalitySpec` | 16 | 15 | 0 | 0 | 1 | 0 | the solver on a hand-built shape |
| `ExistentialInputSpec` | 23 | 17 | 0 | 0 | 6 | 0 | the solver and `ExistentialInputs.mapAtoms` on hand-built shapes |
| `ExtensionSignatureSpec` | 15 | 0 | 15 | 0 | 0 | 0 | `Counter.sourceSignature`, `defnSignature` |
| `GeneralInfixApplicationSpec` | 7 | 7 | 0 | 0 | 0 | 0 |  |
| `GivenSignatureSpec` | 18 | 0 | 18 | 0 | 0 | 0 | `Counter.sourceSignature`, `defnSignature` |
| `GroundEnvironmentSpec` | 40 | 40 | 0 | 0 | 0 | 0 |  |
| `GroundTypesSpec` | 35 | 35 | 0 | 0 | 0 | 0 |  |
| `HigherKindedBindingSpec` | 69 | 69 | 0 | 0 | 0 | 0 |  |
| `HigherKindedRefusalSpec` | 16 | 16 | 0 | 0 | 0 | 0 |  |
| `ImportClausesSpec` | 11 | 11 | 0 | 0 | 0 | 0 |  |
| `InferredResultsSpec` | 18 | 18 | 0 | 0 | 0 | 0 |  |
| `InhabitationSpec` | 9 | 0 | 0 | 0 | 9 | 0 | the solver |
| `InheritedOverrideSpec` | 9 | 9 | 0 | 0 | 0 | 0 |  |
| `InlineMethodsSpec` | 29 | 29 | 0 | 0 | 0 | 0 |  |
| `InlineScopesSpec` | 20 | 19 | 0 | 1 | 0 | 0 | `MethodAnalysis.Index` with a budget: a reading cut short caches no verdict |
| `KindedIdentitySpec` | 4 | 4 | 0 | 0 | 0 | 0 |  |
| `LeafFamiliesSpec` | 2 | 2 | 0 | 0 | 0 | 0 |  |
| `MeasuringSpec` | 3 | 0 | 3 | 0 | 0 | 0 | `Measuring.rows` workers, `Index` step accounting |
| `MethodAnalysisSpec` | 47 | 47 | 0 | 0 | 0 | 0 |  |
| `MethodFeatureIntegrationSpec` | 16 | 7 | 0 | 2 | 7 | 0 | `TypeSubstitution` on scalameta types (b); the solver (c) |
| `MethodMatchProjectionSpec` | 15 | 12 | 0 | 3 | 0 | 0 | `TypeSubstitution`, `MatchTypes.reduced` on scalameta types |
| `MethodRefinementSpec` | 19 | 15 | 0 | 4 | 0 | 0 | `MethodAnalysis.Index.resolve` on hand-built frames |
| `MethodTypePlaceholderSpec` | 12 | 9 | 0 | 3 | 0 | 0 | scalameta syntax of `*` and `?`; `TypePlaceholders.lambda` |
| `MethodUnionNullabilitySpec` | 11 | 11 | 0 | 0 | 0 | 0 |  |
| `NameLookupSoundnessSpec` | 11 | 11 | 0 | 0 | 0 | 0 |  |
| `NamespaceScopingSpec` | 17 | 17 | 0 | 0 | 0 | 0 |  |
| `NamespaceSpec` | 49 | 48 | 1 | 0 | 0 | 0 | `Reading.of(tree).read` on a local class's slot |
| `NaturalSpec` | 9 | 0 | 0 | 0 | 9 | 0 | `Natural` arithmetic |
| `NestedScopesSpec` | 12 | 12 | 0 | 0 | 0 | 0 |  |
| `NeutralSaturationSpec` | 25 | 25 | 0 | 0 | 0 | 0 |  |
| `OpaqueSumForwardingSpec` | 14 | 1 | 0 | 0 | 13 | 0 | the solver; one run of real Scala |
| `OpaqueTypeScopeSpec` | 20 | 19 | 0 | 0 | 1 | 0 | the fixture type-checks and runs |
| `OverrideIdentityImplementationSpec` | 13 | 13 | 0 | 0 | 0 | 0 |  |
| `OwnersAndRecordsSpec` | 16 | 16 | 0 | 0 | 0 | 0 |  |
| `PolyLambdaCardinalitySpec` | 17 | 17 | 0 | 0 | 0 | 0 |  |
| `PolymorphicMemberSpec` | 25 | 25 | 0 | 0 | 0 | 0 |  |
| `PureCodeSpec` | 42 | 42 | 0 | 0 | 0 | 0 |  |
| `ReceiverConstructorsSpec` | 14 | 14 | 0 | 0 | 0 | 0 |  |
| `RecursionProbeSpec` | 5 | 5 | 0 | 0 | 0 | 0 |  |
| `ReentrySpec` | 14 | 14 | 0 | 0 | 0 | 0 |  |
| `ReferenceInhabitantsSpec` | 27 | 0 | 0 | 0 | 27 | 0 | the solver |
| `RepeatedParameterFragmentSpec` | 15 | 8 | 0 | 1 | 6 | 0 | a scalameta parse fact (b); the solver (c) |
| `ReportDependenciesSpec` | 10 | 0 | 0 | 3 | 0 | 7 | the stub file and which jars render (b); `Report.of` and `Report.query` over dependency jars, sources without classes (c*) |
| `ReportHygieneSpec` | 41 | 19 | 0 | 12 | 8 | 2 | `Spelling` over scalameta tokens (b); `Blockers` categories and tallies (c); `Report.of` over mixed inputs (c*) |
| `ReportQuerySpec` | 6 | 0 | 0 | 0 | 1 | 5 | `Report.query` on sources without classes (c*); a snapshot bound (c) |
| `ReportSpec` | 49 | 41 | 2 | 0 | 2 | 4 | `Counter.stat`, `defn`, `ctor`, `param`, `source` on tree nodes; `Report.of` over a directory, a jar, a file, an unparsable source (c*); a missing path and an empty report (c) |
| `SequenceEliminationSpec` | 25 | 23 | 0 | 0 | 2 | 0 | the solver |
| `SignatureSpec` | 29 | 2 | 27 | 0 | 0 | 0 | `Counter.sourceSignature`, `defnSignature` |
| `SingletonIntersectionMethodSpec` | 15 | 15 | 0 | 0 | 0 | 0 |  |
| `SizeSpec` | 64 | 0 | 0 | 0 | 64 | 0 | the `Size` algebra |
| `SkillSpec` | 4 | 0 | 0 | 0 | 4 | 0 | the seeded skill |
| `SourceSnapshotSpec` | 13 | 0 | 0 | 0 | 13 | 0 | `SourceSnapshot.capture` |
| `StdlibModelSpec` | 57 | 56 | 1 | 0 | 0 | 0 | a dependency stub given as a source |
| `StoredFactoriesSpec` | 3 | 3 | 0 | 0 | 0 | 0 |  |
| `StoredScopesSpec` | 20 | 20 | 0 | 0 | 0 | 0 |  |
| `StoredValueSoundnessSpec` | 28 | 28 | 0 | 0 | 0 | 0 |  |
| `SyntaxFeaturesSpec` | 29 | 29 | 0 | 0 | 0 | 0 |  |
| `TargetViewsSpec` | 27 | 27 | 0 | 0 | 0 | 0 |  |
| `TastyFrontEndSpec` | 13 | 0 | 0 | 0 | 13 | 0 | the compiled front end and its differential gate |
| `TastyStubsSpec` | 18 | 0 | 9 | 9 | 0 | 0 | dependency stubs given as sources; stub text, `TastyStubs.render`, `Counter.definitions` over stubs |
| `TerminalHigherOrderSpec` | 30 | 1 | 0 | 0 | 29 | 0 | the solver |
| `ThrowableSlotsSpec` | 5 | 5 | 0 | 0 | 0 | 0 |  |
| `TopTypesUnionSpec` | 83 | 82 | 1 | 0 | 0 | 0 | `runtimeClasses.disjoint` on a hand-built index |
| `TransparentInlineSpec` | 6 | 6 | 0 | 0 | 0 | 0 |  |
| `TypeMemberBoundarySpec` | 16 | 15 | 0 | 0 | 1 | 0 | the fixture type-checks |
| `TypeMemberScopeSpec` | 14 | 13 | 0 | 0 | 1 | 0 | the fixture type-checks |
| `TypeMemberSpec` | 27 | 25 | 0 | 0 | 2 | 0 | the fixtures type-check |
| `UnsuppliedWildcardSpec` | 30 | 30 | 0 | 0 | 0 | 0 |  |
| `ValueRowsSpec` | 2 | 2 | 0 | 0 | 0 | 0 |  |
| **all 90** | **2,047** | **1,649** | **81** | **54** | **229** | **34** | |

Of the 135 (b) examples, what still has no example on rows ("Rules of class (b), on rows" below
gave the open-member rules and the dependency rules theirs):

- By decision: the member-signature reading (`SignatureSpec`, `GivenSignatureSpec`,
  `ExtensionSignatureSpec`, 60 examples), which is deleted with the scalameta front end; the
  index, lookup, alias and projection budgets (`AnalysisQuerySpec`, 5); stack independence
  (`DeepStackSpec`) and the refusal for a class the classpath does not hold, which are the
  compiled front end's to pin.
- **Rule T's edges** (`CompiledTypesSpec`, 8): a name typed in its pattern has that type; stale
  classes are not read as current (the compiled front end refuses the whole source, pinned only
  by its own spec).
- **A local class's slot resolves in the block around it** (`NamespaceSpec.slotLocal`, the type
  parameter half), and **a non-final class and an open trait may overlap**
  (`TopTypesUnionSpec.openPair`, which no count reaches by design).
- Of the dependency rules: a Scala 2 `var`'s setter, a jar that cannot be loaded, and a shaded
  jar's TASTy (below).

The rest of (b) pins no rule of the model that rows do not pin: `MeasuringSpec` pins the workers
of `Measuring`; the substitution, placeholder and refinement internals are pinned on rows by the
same specs' other examples; `Spelling` is a format the compiled front end spells itself (the 9
FAIL examples above are its differences); `TastyStubsSpec`'s other examples pin how a stub is
rendered.

Gaps in the front end's interface that the switch had to work around:

- `Report.of` is tied to `ScalametaFrontEnd`, so the harness builds a `Report` from
  `TastyFrontEnd.rows` itself, without `Report.of`'s reading of files, errors and locations. A
  front end `Report.of` can be given would make `Compiled.report` the production path.
- A source that defines nothing is an error (`no compiled classes for this source`), told from a
  source whose classes were not found only by its text; the harness matches the message.
- The compiled front end takes no `MethodAnalysis.Limits`.
- A dependency can be given only as classes: the fixtures that give stub sources have no
  reading.
- A class the classpath does not hold is filed under `Uncovered` (above), and the texts the
  harness has to match (`Uncovered` aside) are not named constants.
- `Differential` lists every cache directory with an `ok` marker, so it also reads entries of
  fixtures since edited or of the layout before `classpath.txt`; it can read each entry against
  its `classpath.txt` instead of the Scala library alone.

### Rules of class (b), on rows

Each rule of the model that only class (b) examples pin gets an example on rows here, so that none
is left without a test when the scalameta front end is deleted. Each passes on the scalameta front
end and is in the ledger. What is not given one, and why, is listed above ("Of the 135 (b)
examples"); the member-signature reading among it is what docs/type-arithmetic.md §10 describes
and README.md and docs/index.md show as library API, which goes with the scalameta front end.

**Open members (target views C6 and C6a, re-entry, the sandwich D31, a throwing default as its
abstract twin).** `TargetViewsSpec` (27 examples), `ReentrySpec` (14) and 12 of `PureCodeSpec` read
the open world (`MethodAnalysis.analyzeOpen`), where every overridable member is open. In the
closed world a member is open exactly where a supplied template overrides it, so the same 53
examples read the closed world with the overrider supplied, in place: none was added or dropped,
and no expectation changed.

- `cardinality.Overriders.opened` writes, beside each class and trait that is not final, a
  template that extends it and overrides each concrete method by calling it (`trait T__O extends
  T { override def m : Boolean = super.m }`); `CompiledSpec` pins what it writes. Ten fixtures
  write overriders themselves, for what it does not: a `val` or `lazy val` (`construction`,
  `bindingScope`, `builtBefore`), a member written without a type (`unimplementable`,
  `overridingResult`), an inherited member (`inheritedDefault`, `appliedTwins`, `openSubclass`)
  and a trait in a final class (`finalOuter`).
- Every rule has a closed-world form: none of the 53 needed the open world.
- The counts are the open world's. A derivation in a spec's comment that leaned on a member the
  open world opened without saying so now has that overrider in its fixture, or says what makes
  the count (`inheritedDefault`: the 1 is a receiver without an instance, once the inherited `k`
  is open).
- 46 of the 53 read another row without the overriders (the table's last column: the same
  fixture without what `Overriders` writes). In six the member is a definite view, which is left
  out of the count exactly as a final member that calls the target is: the count cannot tell the
  two apart, and what the overrider adds is that the member is open, so a view that was not
  recognized would count as an input (as `localGiven` and `nestedFree` count theirs, 16). In
  `builtBefore` the refusal does not depend on what is overridden.
- The rows of the opened members themselves differ from the open world's where a `val` is
  involved (the open world read every `val` as open; here only an overridden one is). No example
  asserts those rows.
- `MethodAnalysis.analyzeOpen` and the open-world mode of `MethodAnalysis.Index` have no caller
  left: they can be deleted with or before the scalameta front end.

| Example | Rule | Row | With the overriders | Without |
| --- | --- | --- | --- | --- |
| `TargetViewsSpec.overloadView` | an overload calling the declaration is a definite view | `Ovl.target` | 4 | the same |
| `TargetViewsSpec.ambiguousOverload` | an overload the arity does not settle is a possible view | `Amb.target` | `?` (may re-enter: Amb.target) | 4 |
| `TargetViewsSpec.defaults` | a default argument still selects the declaration | `D.target` | 16 | the same |
| `TargetViewsSpec.inheritedDefault` | a parent's default body resolves its names where written | `S0.???` | 1 | 2^32 |
| `TargetViewsSpec.etaExpansion` | an η-expanded method is not run: a possible view | `Eta.target` | `?` (may re-enter: Eta.m) | 4 |
| `TargetViewsSpec.rightAssociative` | a right-associative operator's operand may be by name | `R.target` | `?` (may re-enter: R.m) | 4 |
| `TargetViewsSpec.partial` | a partially applied method is η-expanded | `P.target` | `?` (may re-enter: P.m) | 16 |
| `TargetViewsSpec.selfAlias` | a self alias is the receiver | `S.target` | 4 | the same |
| `TargetViewsSpec.usingClauses` | an argument list fills the clause its `using` flag selects | `U.target` | `?` (may re-enter: U.m) | 4 |
| `TargetViewsSpec.boundQualifier` | a call on a local binding has no declaration read | `Q.target` | `?` (may re-enter: Q.m) | 4 |
| `TargetViewsSpec.localGiven` | a block's local given shadows the member of its name | `G.target` | 16 | 4 |
| `TargetViewsSpec.finalOuter` | `Outer.this.m` of a final outer is another declaration | `Outer.Inner.target` | 16 | 4 |
| `TargetViewsSpec.openOuter` | `Outer.this.m` of an open outer may be overridden to re-enter | `Outer.Inner.target` | `?` (may re-enter: Outer.Inner.m) | 4 |
| `TargetViewsSpec.enclosingPath` | an enclosing receiver's call through the object path is a view | `Outer.Inner.target` | 4 | the same |
| `TargetViewsSpec.enclosingNested` | an enclosing receiver's call through a nested object may re-enter | `Outer.Inner.target` | `?` (may re-enter: Outer.m) | 4 |
| `TargetViewsSpec.nested` | a nested object or class whose body re-enters is a possible view | `T.target` | `?` (may re-enter: T.m) | 4 |
| `TargetViewsSpec.positions` | an argument, tuple field, condition, scrutinee, unary or `!=` operand, `equals` receiver | `Arg.target` | 4 | the same |
| `TargetViewsSpec.equality` | a by-name `==` overload; a by-value one evaluates its operand | `Eq.target` | `?` (may re-enter: Eq.m) | 4 |
| `TargetViewsSpec.byNameExtension` | an extension whose receiver is by name | `S.target` | `?` (may re-enter: S.m) | 4 |
| `TargetViewsSpec.byNameConversion` | a conversion taking its argument by name | `T.target` | `?` (may re-enter: T.m) | 4 |
| `TargetViewsSpec.contextFunction` | a context-function parameter, or an alias of one | `T.target` | `?` (may re-enter: T.m) | 4 |
| `TargetViewsSpec.samLambda` | a lambda converted to a SAM; one returned as a function | `T.target` | `?` (may re-enter: T.m) | 4 |
| `TargetViewsSpec.implicitSearch` | implicit search selecting a given that re-enters | `T.target` | `?` (may re-enter: T.m) | 4 |
| `TargetViewsSpec.superSelection` | `super.m` runs a parent's definition | `T.target` | `?` (may re-enter: T.m) | 4 |
| `TargetViewsSpec.nestedFree` | a nested object that names nothing is no view | `T.target` | 16 | 4 |
| `TargetViewsSpec.importedExtension` | an import or export of an extension from outside the sources | `E.target` | `?` (may re-enter: E.m) | 4 |
| `TargetViewsSpec.blockImport` | a block's import reaches the statements after it in the block | `A.target` | `?` (may re-enter: A.n) | 4 |
| `ReentrySpec.localDefinite` | evaluated at a result, a statement or a nested block: dropped | `B.m.<block#1>.h` | 16 | the same |
| `ReentrySpec.localPossible` | in a branch, a lambda applied in place or an η-expansion: `?` | `C2.m.h` | `?` (may re-enter: C2.m) | 16 |
| `ReentrySpec.localLoose` | inside a match arm, a try, an interpolation or a loop: `?` | `C1.m.h` | `?` (may re-enter: C1.m) | 16 |
| `ReentrySpec.localShadowed` | a name bound again, or the definition's own recursion, is no mention | `Rec.m.h` | 2^16 | 16 |
| `ReentrySpec.secondary` | a secondary constructor's body, for a local or a member target | `D.<init#1>.helper` | `?` (may re-enter: D.other) | 16 |
| `ReentrySpec.primary` | strict initializers and template statements | `Init.h` | `?` (may re-enter: Init.other) | 16 |
| `ReentrySpec.subclass` | a subclass's construction runs its parents' | `C.h` | `?` (may re-enter: C.m) | 4 |
| `ReentrySpec.subclassOwnCode` | a receiver's subclass with code of its own may run any member | `C.h` | `?` (may re-enter: C.other) | 4 |
| `ReentrySpec.creators` | a creator application or `copy` builds as `new` does | `E.h` | `?` (may re-enter: E.other) | 16 |
| `ReentrySpec.literalSearch` | a literal searches only where its type is not the result type | `T.conv` | `?` (may re-enter: T.other) | 2^(2^32) |
| `ReentrySpec.abstractGiven` | an abstract given a sibling may summon is a possible view | `A7.g` | `?` (may re-enter: A7.other) | 2 |
| `ReentrySpec.settledBody` | a body returning its parameter of the result type searches none | `Xe.g` | 16 | 2 |
| `ReentrySpec.sandwichRefuses` | a refused binding, readings that disagree, or ≥ 2 stay `?` | `D.to` | `?` (may re-enter: D.m) | 0 |
| `ReentrySpec.builtBefore` | an enclosing parameter or a plain constructor parameter | `Field.to` | `?` (may be the receiver: Opt.to) | the same |
| `PureCodeSpec.privateVarField` | a private var field stays hidden state | `Use.f` | `?` (open class with hidden constructor state: RVH.x) | `?` (final class field not observable injectively: RVH.x) |
| `PureCodeSpec.throwingOpenMember` | an open member that only throws stays an open capability | `OpenGet.m` | 16 | 4 |
| `PureCodeSpec.unimplementable` | one of an empty type reads as its abstract twin: vacuous | `K1.m` | 1 | 4 |
| `PureCodeSpec.appliedTwins` | at the record's or receiver's type arguments | `V.mkGen` | 0 | 1 |
| `PureCodeSpec.subclassTwins` | a subclass's instance is its parent's: both vacuous | `U.viaQ` | 1 | 4 |
| `PureCodeSpec.boundedBinder` | a bound may supply a binder: such a member is kept | `Use.eqb` | `?` (polymorphic capability binder: B (bounded)) | 4 |
| `PureCodeSpec.overridingResult` | a member without a result type that may override is kept | `Recv.m` | 16 | 4 |
| `PureCodeSpec.emptyTypes` | an empty type is read from its structure | `Use.u1` | 1 | 4 |
| `PureCodeSpec.construction` | a constructor that only throws builds no instance | `Use.dk2` | 16 | 2 |
| `PureCodeSpec.bindingScope` | a level's body binds names in its own scope | `K.m` | 2^(2^33) | 4 |
| `PureCodeSpec.throwingRecord` | a record keeps a member no override can implement | `Use.f` | 1 | 2 |
| `PureCodeSpec.openSubclass` | an open subclass's overridable members are reasons | `U.u` | `?` (inherited stdlib member a subclass may override: `Exception.getCause`) | `?` (final class field not observable injectively: MyErr.msg) |

**Dependencies read from compiled classes.** `TastyStubsSpec` and two examples of
`ClosedWorldSpec` pin what a row counts through a dependency by giving the analysis stub sources
(`tasty/…` inputs), which only the scalameta front end reads, or by asserting a stub's text.
`DependencyRowsSpec` pins the same rules on rows of a module read over a dependency that is
compiled classes (`Compiled.reportOver`): `tastyfixtures` and `javafixtures` (each package of the
test classes copied into a class directory of its own), two of scalameta's Scala 2.13 jars, the
JDK, and units the fixture writes. Where the module can be read without the dependency the
example reads it so too, and asserts that the row refuses, or refuses for another reason, without
it; the Scala 3 one asserts that the dependency adds no row.

| Rule | Example it had | `DependencyRowsSpec` | Rows | Derivation |
| --- | --- | --- | --- | --- |
| A dependency counts as its source does, or refuses | `TastyStubsSpec`, "a stub never counts differently from the source it was compiled from" | `parity` | `t.pair`, `t.mkPair`, `t.light` 16; `t.suit` 4; `t.flag` 2; `t.endo`, `t.show` 2^8; `t.built` 2^32; `t.fed` its source's or `?` | Pair and Light have 4 values (2^4, 4^2), Suit 2; an opaque type outside its scope is one value; a record of one `Boolean => Boolean` beside `b` is 3 observed bits; `Built()` is one value; Fed's field is set by a constructor body, so it is not Built's 2^32 |
| It supplies what the module alone cannot name | "the stubs supply what the unsupplied environment cannot" | `supplied` | `?` without it | the names are declared nowhere the analysis reads |
| A compiled inline body is not read | "a call into a compiled inline body refuses: the body is not read" | `inlineBody` | `t.picked` `?` naming `pick`; 2 through the source | the caller expands a body the reading does not have |
| A stored value through a dependency is its source's | "stored values read through the stubs agree with the source, or refuse" | `stored` | `t.Holds` 32 | Pair × Light × Suit = 4 · 4 · 2 |
| A Java enum is its constants | "a Java enum is the closed set of its constants" | `javaEnum` | `j.coin`, `j.pickCoin`, `j.op`, `j.size` 4 | two constants; a static field of the enum's type is none |
| A Java interface is the record of its abstract methods | "a Java interface is the record of its abstract methods" | `javaInterface` | `j.mkToggle` 4; `j.toggle` 16 | one `Boolean => Boolean`; the default method is final, the statics are the companion's |
| A Java class's state and a sealed interface's subclasses are not read | "a Java class's state and a sealed interface's subclasses are not read" | `javaHidden`, `javaAlone` | `?`, for another reason than without the dependency | the class files do not tie state to constructor parameters |
| Java signatures: `Object` is `Any`, a wildcard is read, a raw type is not, statics are the companion's | "Java signatures: statics in the companion, Object as Any, wildcards spelled, raw types unread" (the stub's text) | `javaSignatures`, with `javaEnum` and `javaInterface` for the statics | `j.mkAny`, `j.useAny` ω; `j.useWild`, `j.useRaw` `?` | an `Any => Any` has a one-valued input and an ω output (v1.md §2); the wildcard is refused by name, the raw type as a signature not read |
| A Scala 2 class has a count | "a reference to a Scala 2 class gets a count instead of `declaration not supplied`" | `scala2` | `s.mkOut` 1; `s.out` 2; `s.drop` 1 | a record of one method whose result is Unit is one value |
| A sealed class with a child the pickle does not declare refuses | "a sealed class with a child no stub declares is neither sealed nor read" | `scala2Sealed` | `s.path`, `s.classpath` `?`, for another reason than without the dependency | read as sealed it would hold no value |
| A class two dependencies declare is the first one's | "a class two jars declare is read from the first, as the JVM loads it" | `firstWins` | `u.f` 4; with the units the other way round 8, and `u.edge` 3 | two cases against three, and a case only the second unit declares |
| A dependency's template is the one of its qualified name | `ClosedWorldSpec`, `stubParents` and `stubOwners` | `qualified` | `U.target` 4; `?` where q.D extends p.S, and with the same text as a source of the module | q.D's parent outside the sources is not below p.T, so p.T's `m` is final |
| A JDK module other than `java.base` is read where a source names it | "a class of a JDK module other than java.base is a supplied declaration" | `jdkModule` | `j.status` 16 | `ClientInfoStatus` has four constants |

- "stored values read through the stubs agree with the source, or refuse" compared rows that do
  not exist: a `val` in an object is no definition row, so it compared two empty strings. `stored`
  reads a case class of the dependency's types.
- Not given a row: a Scala 2 `var`'s setter (the one Scala 2 class with a `var` among the test
  classpath's jars, `SingletonReference.Ref`, is private; a row needs a public one); a jar that
  cannot be loaded being reported and not fatal (a warning of the report, which
  `ReportDependenciesSpec` reads and the harness turns into a failure); a TASTy file declaring
  its own package in a shaded jar (it needs a shaded jar); and that each stub parses (the stubs
  are deleted with the scalameta front end).

**Counts on, or behind, a fixture scalac rejects.** An example with a fixture that has no classes
fails whole under the compiled front end, whatever else it asserts. Twenty-two examples pinned
counts on such a fixture or beside one. Where the fixture can be valid Scala it is, with its
expectation unchanged; where part of an example cannot, that part is an example of its own.

| Example | What scalac rejected | The fixture | Count |
| --- | --- | --- | --- |
| `CardinalitySpec`: "enum arms to different successors", "a custom lazy list matches the built in", "a custom unit list collapses like the built in", "a custom string list retains the highest tier", "an empty alphabet leaves only a finite base" | a by-name parameter of an enum case, which is a `val` | the lazy field is a thunk, `tail: () => U` | ω + 2, ω, ω, ε₀, 1 |
| `GeneralInfixApplicationSpec.qualified` | `_root_.Models`, a member of the empty package, which `_root_` does not reach | `Models` in a package, `_root_.m.Models.Join[…]` | 6: x or y, beside None, Some(x) or Some(y) |
| `EnvironmentCompletenessSpec.exportOutranks` | a bare `T` in the overriding subclass, ambiguous between the outer and the inherited one | the subclass spells `A.T` | 4, 4, 2, 2^8, 2^8 |
| `SingletonIntersectionMethodSpec.binder`, `alias` | a public method's type naming the private parameter `x`; a body `= x` that is no `x.type & alias.type` to scalac | `class Env[A](val x: A)`, and `???` for the body | `?`; 1 |
| `SingletonIntersectionMethodSpec.marker`, `negative` | `import custom.Singleton`; a `var`'s singleton | split: `markerRebound` and `negativeVar` hold the fixture that does not compile | 1, 1; `?`, `?`, 1 |
| `StdlibModelSpec.javaWildcard` | `List[A]` under `import java.awt.*`, where `List` takes no type argument | the refusal is read from `def l(a: List): Boolean`, which compiles; the stub of `java.awt.List` is `javaWildcardStub`, a stub source (`DependencyRowsSpec.jdkModule` reads the class itself) | ω, 0, 1 and the refusal |
| `StdlibModelSpec.relativePaths`, `unapplied`; `InlineScopesSpec.localObject` | one member of the fixture (`_root_.Option[A]`; `Option` where a proper type is expected; a companion a local class does not have) | split: `rootedSimpleName`, `unappliedProper` and `localClass` hold that member | 2, 2, 2; 4, 27; 2^37 twice |
| `NamespaceSpec.packageClauses`, `packagePaths`, `wildcardImports` | one file of the set, or one of the example's sets | split: `singleClause`, `unopenedPath`, `ambiguousWildcards` | 2, 2; 2, 2, ≤2^64, 2; 2, 2 and two refusals |
| `PureCodeSpec.reboundNames`, `DefinitionInventorySpec.contextLiterals`, `GroundTypesSpec.closedRepeated` | another fixture of the example | split: `blockedImport`, `ambiguousLiterals`, `repeatedFunctionType` | 2^32 three times; six counts; ω, 1, ω |
| `BinderBoundsSpec.invalidTerminal` | the fixture's other members, invalid on purpose | `fBounded` reads the legal member from a fixture that compiles; the example keeps it as its control | 1 |

Counts that stay pinned on, or beside, code scalac rejects, which the compiled front end never
reads:

- `ExistentialInputSpec.aliasArgument`, 2: an alias applied to a wildcard (`Shared[?, R]`), which
  scalac reduces only where the wildcard is used once.
- `OverrideIdentityImplementationSpec.fileIdentity`, 16: a top-level declaration without a body in
  another file, read as a capability. No valid spelling was found.
- `GroundTypesSpec.repeatedFunctionType`, ω: a repeated parameter in a function type.
- `BinderBoundsSpec.invalidTerminal` and `HigherKindedRefusalSpec.argumentKinds` assert that a
  legal member beside invalid ones still counts (1; 2). The counts themselves are pinned in
  fixtures that compile, by `fBounded` and `HigherKindedBindingSpec.boxOption`.
- `ClosedWorldSpec.stubParents` and `stubOwners`, 4, read stub sources: `DependencyRowsSpec.qualified`
  pins them on compiled dependencies.

Two more examples pin a reading only the scalameta front end has, a report with no dependency
classpath at all, where a JDK class outside `java.base` is not supplied
(`DependencyRowsSpec.jdkUnread`, `StdlibModelSpec.javaAwtUnread`): a build always passes a
classpath, and the compiled front end always has the JDK to read.

### Inline bodies: `Inlines` and its verdict gate

Which inline methods count as methods (AGENTS.md, "Inline methods count as methods"; decisions D2
and D4), read from typed trees: `core/src/main/scala/cardinality/compiledinlines/Inlines.scala`,
in the package `cardinality.compiled`.

```scala
final private[compiled] class Inlines(own: Set[ClassSymbol], dependencies: => List[ClassSymbol])(
    using Context
) {
  /** Why a method is not counted as a method, if it is not. */
  def refusal(method: TermSymbol): Option[String]
}
```

`own` are the module's top-level classes (`CompiledTypes.loaded`) and `dependencies` those of its
classpath (`CompiledTypes.dependencies`), asked for only where a body selects a member of a class
outside the module on a receiver. `refusal` answers for any method: an inline method an expansion
of which a call site decides (the reason starts with `inline method:`, or is the transparent-result
reason of rule X), and a method written inside such a method, which every expansion copies
(`definition inside an inline method:`). `None` is "counts like a `def`"; the reasons keep the
scalameta front end's prefixes.

How each rule reads:

| Rule | Over the trees |
| --- | --- |
| A compile-time operation | The nodes `InlineIf`, `InlineMatch` (`summonFrom` is one without a selector), `Quote`, `Splice` and their patterns; a term reference or a written type whose symbol is declared under `scala.compiletime` or `scala.quoted`; the `Macro` flag. |
| Wherever it appears | One walk of the whole definition: nested definitions, lambdas, arguments by value and by name, and the default arguments, which the compiler keeps as methods beside the definition (`m$default$N`). What a `throw` is given, and the message of `sys.error`, is not read (AGENTS.md, "Ignore throwing"). |
| In an inline method it calls | A reference to an inline method brings that method's definition along, read once; the methods reached are decided together, breadth first, so a cycle is decided with everything it reaches and never assumed to pass. |
| A dependency's inline body | The same walk: the callee's symbol has its tree where its classes hold TASTy. The Scala library's inline methods (`summon`, `assert`, `Tuple.head`) are read from the library's own classes; no table of them is kept. An inline method without a tree refuses (`has no body in the classes read`), a name the classes do not hold refuses, and a method that is not inline (Java, Scala 2, a plain `def`) is a call. |
| Rule X | `isTransparentInline` and a declared result that is no singleton. An export's forwarder is transparent by construction and is read as the member it forwards to. |
| D2 | The conversions and extensions the body applies are calls in the tree, read like any call; one the body could have met and did not apply is not in the tree. |
| D4 | The given the compiler resolved is an argument in the tree; an inline given is an inline call. |
| An inline override on a receiver | A member selected on `this`, on a parameter (a lambda's included, and a function parameter's `apply`), or on what is selected or called on one (`p.g.f(x)`, `p.m(a).f(x)`: an override may narrow the type in between), when a subclass may override it: every inline method of the program that overrides it is brought along. The module's classes are searched, and the dependencies' where the member's class is not the module's (a dependency extends no class of the module). An ascription and a local value fix the receiver's type. |
| A runtime class of a type parameter | A type test (`isInstanceOf`, a typed pattern) or `classOf` whose type has no one class whatever the expansion instantiates: a type parameter, an abstract type member, an array of one. |

What the classes hold, as it bears on the reading:

- An inline method's body is kept as written: a call of another inline method, a `transparent`
  one included, is still a call. So "read the call" needs no `Inlined` node here.
- An `Inlined` node (in code that is not inline) keeps the expansion and the top-level class it
  came from, not the call that was expanded. Reading a call site's expansion back to its callee is
  not possible from it; nothing in this reading needs to.
- A method written inside an inline method that refuses must refuse too, and `refusal` says so
  for it. The scalameta front end answers 1 for a one-valued result before it reads the body, so
  the one-value rule has to stay ahead of `Inlines` in `Readings` for the rows to agree.

Size: `Inlines.scala` is 382 lines with its comments, against the 3,747 of `analysis/expansion`
(twelve files, `LibraryInlines` and the candidate rule of register D43 among them), which it
replaces whole. Its spec, `InlinesSpec`, is 18 examples over fixtures it compiles itself, each
verdict derived beside it.

The gate while no row shows the verdict is `InlineVerdicts`
(`core/src/test/scala/cardinality/compiled/InlineVerdicts.scala`):

```bash
sbt "core/Test/runMain cardinality.compiled.InlineVerdicts fixtures core/target/compiled-fixtures --list"
sbt "core/Test/runMain cardinality.compiled.InlineVerdicts corpus <corpus directory> --list"
```

For every inline method of an input, and every method written inside one, it compares two
verdicts that no code shares. The old one is read off the row `ScalametaFrontEnd` answers for the
method (`Differential.answer`, the code of `analysis/expansion`): refused where a reason of the row
starts with `inline method` or `definition inside an inline method`, or names the transparent
result. The new one is `Inlines.refusal` for the method's symbol over the compiled classes. A
method is paired by its source, line and name. Exit 1: a method only `Inlines` refuses (`worse`),
or an input a front end could not read. A fixture with dependency units is read as its test reads
it, the units' classes on the classpath (`Differential.fixtures` reads their sources as sources of
the module, which is what its own rows are compared over).

| Input | Methods | Same (count, refuse) | Counts where the old refuses | Worse | Masked | Other | Unpaired |
| --- | --- | --- | --- | --- | --- | --- | --- |
| The compiled fixtures (1,934, 101 with the word `inline`) | 337 | 298 (147, 151) | 35 | 0 | 1 | 1 | 2 |
| corpus-cats-eo | 82 | 39 (23, 16) | 27 | 0 | 0 | 12 | 4 |
| corpus-cats-eo-generics | 3 | 3 (0, 3) | 0 | 0 | 0 | 0 | 0 |
| corpus-cats-eo-schemes, corpus-cats-kernel, corpus-cats-core, corpus-core | 0 | | | | | | |

The last four have no inline method (cats' `@inline` is Scala 2's annotation). `Other` is a method
whose old row refuses for another reason and shows no inline verdict: `Inlines` counts all 13.
`Unpaired` is a method with no old row at its line and name: export forwarders (the four of
cats-eo, which count as the members they forward to, and one in a fixture, which refuses as its
member does) and a local inline given. `Masked` is `mk` inside `viaLocal`, a method the old row
counts 1 by its one-value rule and `Inlines` refuses, as the old front end does for every other
definition inside a refusing inline method.

Each method that counts where the scalameta front end refuses, by what it was refused for, with a
derivation of one of each:

| Old reason | Fixtures | cats-eo | Why it counts |
| --- | --- | --- | --- |
| D2: `may expand the inline conversion …` | 8 | | `inline def first(t: (Int, Int)): Int = t._1` beside `inline given conv: Conversion[Int, String]` whose body uses `codeOf`: no expression of the body needs a `String` from an `Int`, the compiler applied nothing, and every expansion returns `t._1`. |
| D2: `applies +` / `applies get` / `applies h` / `applies f` / `applies getOption` / `applies reverseGet` (…`which an extension of that name may answer when no member applies`, or `the implicit conversion … may answer`), and `calls inline method …, which applies …` | 12 | 9 | `inline def hOnT(t: lib.T, x: Int): Int = t.h(x)`: `T` declares `h(x: Int): Int`, a member always outranks an extension, and the tree selects `T.h`; the inline extension `h` of `T`'s companion is not applied. In cats-eo, `accessor.get(self.to(s))` selects `Accessor.get`, a member. |
| D2: `selects reverseGet, no member of … (an extension or conversion is not resolved)` | | 2 | The compiler resolved the extension: the tree calls the inline extension `reverseGet`, whose body `self.from(ra.reverseGet(b))` has no compile-time operation. |
| D2: `may expand an implicit conversion from outside the sources`; `selects toLong on a value whose type is not read` | 2 | | An import the sources do not supply rebinds nothing the compiler did not resolve; `i.toLong` selects `Int.toLong`. |
| D4: `leaves a given of … to implicit search` | 1 | 4 | `inline def implicitly1[A](a: A): String = useShow(a)`: the tree passes `Show.anyShow[A]`, the given the compiler chose where the method is written, which is no inline method. |
| D4: `resolves a given with summon`; a context function type, literal or callee | 4 | | `summon` is the library's `inline def summon[T](using x: T): x.type = x`, read from its classes: it returns its argument. A context function is a function whose argument the tree passes. |
| A name or type the scalameta reading does not resolve: `selects … on a value whose type is not read` (or `of an abstract type`), `… which the library table does not list`, `creates an instance of a class not read`, `declares the local type`, and the definitions inside those methods | 8 | 8 | `inline def head(t: (Int, Int)): Int = t.head`: `Tuple.head` is an inline method of the library whose body is a runtime selection and a cast, with no compile-time operation. `(new Gn)(x)` calls the `apply` of a class local to the body. |
| `tests the type XA at run time` | | 4 | `val (x, a) = to(s)` in `Lens.modify`: the pattern is irrefutable and the compiled tree has no type test. The test was in the scalameta front end's input only, where the compiled types are written into the pattern (`val (x: XA, a: A)`). |

No method is worse on the fixtures or on a corpus. One was, while this was built, on a fixture
written for it (`DependencyRowsSpec.inlineOverride`): an inline override declared in a dependency,
which is why the dependencies' classes are searched.

Plugging it in (the front-end agent's, in its own files):

1. Move `Inlines.scala` into `core/src/main/scala/cardinality/compiled/` and count it in
   `TastyFrontEndSpec`'s example over that directory's imports (`List.fill(8)` becomes 9). It is
   kept beside the directory only because that example pins the number of files; `InlinesSpec`
   holds the same import rule for it and names its path.
2. In `Readings`, where an inline method answers `an inline definition`, ask
   `inlines.refusal(symbol)` instead, with one `Inlines` per module:
   `new Inlines(own, CompiledTypes.dependencies(classes)._1)`, the second element going to the
   report's warnings. A refusal is a row's reason as it
   stands, not `not read from compiled classes yet`. Ask it for a method that is not inline too:
   one written inside a refusing inline method refuses.
3. Keep the one-value rule ahead of it, as the scalameta front end has it.
4. Rerun `scripts/spec-ledger.sh` and `InlineVerdicts`. Expectations that move, each with its
   derivation: `DependencyRowsSpec.inlineBody` (a dependency's inline body is read, so
   `picked[A](x: A, y: A): A` counts 2, its two projections) and the examples that pin a refusal
   of the table above.

Limits, each a place where this reading does no more than the scalameta one:

- A dependency class that does not load is left out of the search for inline overrides, as it is
  left out of the stubs. `CompiledTypes.dependencies` returns why: `Readings` has to pass it on
  as a warning of the report, and `InlineVerdicts` fails an input with one. No class of a corpus
  or a fixture fails to load.
- `InlineVerdicts` lists a method that counts where the scalameta front end refuses and does not
  fail on it, whatever the old reason: the reasons do not sort mechanically (a D2 reason ends
  `which uses scala.compiletime.codeOf` too). Each listed method is read by hand, as the table
  above records.
- A cast (`asInstanceOf`) to a type parameter is not a runtime class read: it is erased, and an
  unsafe cast is outside the model.

### The closed world and dependency classes over symbols: `ClosedProgram`, `DependencyClasses`

Two pieces of the compiled front end that nothing calls yet, in
`core/src/main/scala/cardinality/compiledworld/` (package `cardinality.compiled`; beside the
directory for the same reason `Inlines` was: `TastyFrontEndSpec` pins its number of files).

**`ClosedProgram`** (223 lines) is the closed world by exact symbols, in place of
`analysis/ClosedWorld.scala` (380 lines, by simple name, over parsed sources and stubs).

```scala
final private[compiled] class ClosedProgram(
    own: List[ClassSymbol],                              // CompiledTypes.loaded, second
    unloaded: List[String],                              // CompiledTypes.loaded, third
    dependencies: => (List[ClassSymbol], List[String])   // CompiledTypes.dependencies, by name
)(using Context) {
  def subclasses(c: ClassSymbol): Subclasses             // Subclasses(found, complete)
  def finalClass(c: ClassSymbol): Boolean
  def overriding(owner: ClassSymbol, member: TermOrTypeSymbol): Overriding  // No | Unknown | Supplied
}
```

What each answers:

- `subclasses(c)`: the classes of the program that extend `c` at any depth, nested, local,
  anonymous and given ones included, and whether they are all of them. A class written `final`
  and an object have none. A Scala 3 sealed class whose children are all classes and objects is
  answered from the compiler's list, without listing anything; one with an anonymous child (the
  compiler lists the class itself) or an enum's value case (an anonymous class) is found by
  listing.
- `finalClass(c)`: written `final`, or an object, or a concrete class (no trait, not `abstract`,
  `open` or `sealed`) with `Subclasses(Nil, complete = true)`. A non-public constructor does not
  open it.
- `overriding(owner, member)`: asked from each subclass's side. For a subclass `s`, the classes
  of its linearization before `owner` are what the member resolves through in `s`: a subclass of
  `owner`, or a parent mixed in beside it. `Supplied` where one of them declares a member that
  matches by signature (an abstract declaration counts only in a subclass of `owner`: mixed in
  beside, it leaves the member `owner`'s). So a member brought by a mixed-in parent that does not
  extend `owner` is found, which asking the overrider (`allOverriddenSymbols`) does not find.
  `No` for a private or final member and where no subclass has one. Where a signature is not
  read, a declaration of the name is taken for an overrider, as the scalameta reading takes one.

Where the program is not all known, no answer is "none": `complete` is false, `overriding` is
`Unknown` (the row's `member may be overridden through a parent outside the sources`),
`finalClass` is false. That is so for:

- a class that does not load: of the module, every answer; of the classpath, every answer about
  a class outside the module;
- a parent that is not read (a class compiled against something the classpath does not hold):
  it may extend any class outside the module, and it may override a member of the class it is
  mixed in beside, but it hides no class of the module (the classes it is a parent of are still
  filed under their other parents, and under each other);
- the JDK, which is not listed: a class of it is closed only where it is written `final`;
- a class without TASTy (Java, Scala 2): its local and anonymous subclasses are class files no
  symbol stands for (`javafixtures.Lone` has one, which overrides `on`), so it is closed only
  where it is written `final`, and the subclasses that are found are not known to be all.

The rule for the Scala library and the JDK, and why. The library's classes are on the dependency
classpath and are listed like any dependency's: they have TASTy, so their subclasses are all
found, and a library class nothing extends is final as one of the module would be
(`scala.Some`), while `scala.Function1` has the library's and the module's implementers. The JDK
is the context's own and is never listed (30,000 classes without TASTy): listing it would
answer nothing, since its classes are tree-less and stay open by the rule above. So neither makes
everything open or everything final. Two assumptions are left: a Scala 3 class of a dependency
is taken not to be extended by a local or anonymous class of a Java or Scala 2 dependency, and
a class a dependency jar declares again after the JDK is the JDK's (`CompiledTypes.dependencies`
leaves it out).

Cost. The module's part is one walk of the module's trees, on the first question about a class
of the module. The dependencies are listed and indexed on the first question about a class
outside the module that is neither final nor a sealed class answered from its children, and
never for a question about a class of the module:

| Corpus | Classes of the module | Module's trees walked, every class and member answered | Dependencies listed and indexed, once |
| --- | --- | --- | --- |
| corpus-cats-eo | 257 | 54 ms | 714 ms |
| corpus-cats-eo-generics | 26 | 123 ms | 893 ms |
| corpus-cats-eo-schemes | 4 | under 1 ms | 625 ms |
| corpus-cats-kernel | 458 | 111 ms | 237 ms |
| corpus-core | 1,228 | 249 ms | 1,503 ms |
| corpus-cats-core | 2,349 | 335 ms | 224 ms |

The second column's time includes the walk of the trees that lists the questions. The
dependencies' time is the listing (reading every class of the classpath, the Scala library
included) and the index over it; corpus-core's is the largest because its classpath is
(scalameta, tasty-query, the compiler).

**The differential**, `ClosedVerdicts` (`core/src/test/scala/cardinality/compiled/`):

```bash
sbt "core/Test/runMain cardinality.compiled.ClosedVerdicts fixtures core/target/compiled-fixtures --list"
sbt "core/Test/runMain cardinality.compiled.ClosedVerdicts corpus <corpus directory> --list"
```

For every class and trait a module's sources write and every member one defines, it compares
`analysis.ClosedWorld` asked by name over the scalameta front end's inputs (`extended`,
`overriding`: what decides the `final`s it writes) with `ClosedProgram` asked by symbol
(`subclasses`, `overriding`). Exit 1: an answer where the new one does not know and the old one
does (`coarser`), or a finer answer no fact of the old reading's inputs explains.

| Input | Classes paired | Same | Finer | Opener | Coarser | Members paired | Same | Finer | Opener | Coarser |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| corpus-cats-eo | 66 | 65 | 1 | 0 | 0 | 277 | 276 | 1 | 0 | 0 |
| corpus-cats-eo-generics | 6 | 6 | 0 | 0 | 0 | 32 | 32 | 0 | 0 | 0 |
| corpus-cats-eo-schemes | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| corpus-cats-kernel | 279 | 275 | 0 | 4 | 0 | 1,249 | 1,222 | 27 | 0 | 0 |
| corpus-core | 302 | 243 | 59 | 0 | 0 | 1,840 | 1,836 | 4 | 0 | 0 |
| corpus-cats-core | 1,158 | 1,137 | 20 | 1 | 0 | 4,395 | 4,099 | 295 | 1 | 0 |
| The compiled fixtures | 3,207 | 3,199 | 5 | 3 | 0 | 2,164 | 2,112 | 52 | 0 | 0 |

- Finer, the old answer open and the new one closed. Each is filed under a fact of the old
  reading's inputs it cannot tell apart, which makes the difference plausible and does not prove
  it (the facts are weak for common names; the lists are for reading by hand): another template or alias has the class's simple name
  (cats-core's `SortedSetHash` extends cats-kernel's `SortedSetHash`, so by name it extends
  itself); another template defines the member's name; a parent clause spells the simple name
  and names another class (`extends Exception` beside a trait `other.Exception`).
- Finer, the old answer unknown and the new one closed: a parent the old reading has no
  declaration of (a library class), which the symbols read.
- Opener, the new answer open and the old one closed: four traits of cats-kernel and one class of cats-core that a package object extends
  (`package object all extends AllInstances with …`), which the old reading does not take for a
  template; three fixture classes of the same kind; and cats-core's abstract `As.substitute`,
  which an anonymous class implements. The old answer is the unsound one in each.
- Coarser: none.
- Unpaired: a class or member only one side lists at that source, line and name. The old side
  lists what classes and traits write; the new one every class of the trees (enums and their
  cases, classes in bodies, members the compiler adds at a written position).

**`DependencyClasses`** (166 lines) answers what a dependency's class without TASTy is,
from its symbols:

```scala
final private[compiled] class DependencyClasses(
    classFile: ClassSymbol => (List[String], Boolean)   // CompiledTypes.javaFacts(classes)
)(using Context) {
  def of(c: ClassSymbol): Either[String, Declared]
}
enum Declared {
  case Sum(cases: List[TermOrTypeSymbol])                        // one of its cases
  case Fields(constructor: TermSymbol, fields: List[TermSymbol]) // what its primary constructor is given
  case Record(abstracts: List[TermSymbol], concrete: List[TermSymbol]) // no state: its abstract members
}
```

| Class | Answer | The stubs' guard it keeps |
| --- | --- | --- |
| A Java enum | `Sum` of its constants, by the class file's `ACC_ENUM` fields | A static field of the enum's type is no constant (`Coin.DEFAULT`). |
| A Java interface | `Record`: its abstract methods; default methods are `concrete` | Statics and constants are the companion's; what `Object` declares is left out. |
| A sealed Java class or interface | refused | Its permitted classes are not read. |
| Any other Java class | refused, `open class with hidden constructor state` | No signature ties a constructor's parameters to the state. |
| A Scala 2 sealed trait or abstract class | `Sum` of its children | Refused where a child is local or anonymous (the pickle lists the class itself), and where the class has instances of its own. |
| A Scala 2 case class | `Fields` of its public primary constructor | The primary is the constructor naming every field; none is chosen among several. |
| A Scala 2 trait, or abstract class without constructor parameters | `Record` | |
| A class with TASTy | refused: it is its tree's to read | |

No body is read anywhere: a `concrete` member is code that is not supplied, which a count may
call and may not read, never a capability and never a throw.

**Integration**: done in stretch 7 ("Wired into the front end", below). Whether two classes
share an instance (`mixes`) was written and asked by nothing, and is deleted: the old front end
asks it of a capability record's member named like the measured one, which the compiled reading
refuses outright, and a self type needs no such answer (stretch 9, below).

#### Wired into the front end (stretch 7)

The integration is done on `tasty-gate` (after merging `tasty-front` at `0538b5b`):

- `ClosedProgram` and `DependencyClasses` are in `compiled/`. `TastyFrontEnd` builds one
  `ClosedProgram` per module, which takes the same lazy dependency listing as `Inlines`, and one
  `DependencyClasses`, which reads Java class files only when a class without TASTy is asked
  about. `World.extended`, `World.finalClass` and `World.overriding` ask it; the module-only
  `extended`, `overridden` and `unjudged` are deleted.
- Classes without TASTy are closed by their class files: `CompiledTypes.unnamed` lists the local
  and anonymous classes of the module's output and of its classpath that no symbol stands for,
  with their super class, interfaces and declared methods (`JavaClassfile` reads them). One under
  a class is a subclass of it that overrides the members it declares, by name; one whose class
  file does not read, or whose parent does not resolve, leaves the answer incomplete. So
  `javafixtures.Lone` is open (its `Lone$1` overrides `on`), `javafixtures.Solo` final, and
  `Toggle.same` closed, as the stubs have them.
- `Shapes` and `Stored` read a dependency's class with TASTy by its declaration, its bodies
  unread (`World.readByDeclaration`); `Shapes` reads one without TASTy through
  `DependencyClasses`. A dependency's opaque type is one value (rule F). The standard library and
  the JDK stay the standard model's: the `scala` package and the subpackages the library declares,
  and `java` (`World.standard`, the stubs' `stdlibOwned`); a library's own namespace under
  `scala`, such as scalameta's, is a dependency like any other. An evidence type compares every
  type read by shape as a type (`World.shaped`).
- `Receivers` reads a template that is abstract or may be extended by what it inherits, as it
  resolves it (a concrete member mixed in beside an abstract one implements it; an abstract
  redeclaration in a subclass of the concrete one's class makes it abstract again): an inherited
  abstract member or deferred given is a capability read from its signature as the template sees
  it (`typeAsSeenFrom`, each overload its own capability), and a concrete one supplies nothing
  where no class of the program overrides it. Not read yet: a template that reaches one parent
  through two of its parents (the old reading refuses these too, and their counts are not
  derived), a parent with constructor parameters, in a class, or without TASTy and no trait. The
  compiler's super accessors (`superT$$m`) are no capabilities.
- `ClosedProgram.overriding` asks, for a member declared in an ancestor of `owner`, about the
  classes before the member's declaring class in each subclass's linearization, outside what
  `owner` extends; method names of class files are compared encoded.

The differential, before (`37668ad`, the merge of `tasty-front`) and after (`b3f8fce`); every
input passes, against the memory of the run before:

| Input | Table | Identical | Better | Worse | Uncovered |
| --- | --- | --- | --- | --- | --- |
| cats-core | definitions | 1318 → 1357 | 21 | 1 | 219 → 180 |
| cats-core | methods | 373 → 411 | 89 → 213 | 15 → 19 | 7174 → 7008 |
| cats-eo | definitions | 128 → 130 | 6 | 0 | 17 → 15 |
| cats-eo | methods | 153 → 159 | 1 → 3 | 0 | 543 → 535 |
| cats-eo-generics | definitions | 7 | 0 | 0 | 5 |
| cats-eo-generics | methods | 27 | 0 | 0 | 53 |
| cats-eo-schemes | definitions | 1 | 0 | 0 | 1 |
| cats-eo-schemes | methods | 4 | 0 | 0 | 15 |
| cats-kernel | definitions | 307 | 0 | 0 | 5 |
| cats-kernel | methods | 167 → 176 | 3 | 0 | 1615 → 1606 |
| core | definitions | 369 | 3 | 0 | 269 |
| core | methods | 999 → 1003 | 1 | 0 | 3000 → 2996 |
| fixtures | definitions | 4931 | 97 → 98 | 1 | 559 → 558 |
| fixtures | methods | 3623 → 3820 | 109 → 143 | 1 → 2 | 3009 → 2777 |

- Better past a model limit, all derived in `derived.txt`: 124 members of the type class
  instances of cats-core (`ListInstances`, `OptionInstances`, `SeqInstances`,
  `VectorInstances`, `EitherInstances`: each derived from its signature, the Option ones case by
  case, the sequence ones countably many), two of cats-eo's `SplitCombineLens` (`modify` 2,
  `replace` 1) and three of the fixtures (`T extends IterableOnce[Int]`).
- Corrected, with both values and the derivation in `corrected.txt`: four `align`s of cats-core's
  instances (the ruling on `Ior`: List, Seq and Vector 1 → ω, Option 1 → 16), and the fixture
  `S.productArity` 2^128 → 2^64 (`canEqual` is `S`'s own and nothing extends `S`, so it supplies
  nothing, where the old reading counted it a capability). These are the 4 and 1 rows the table's
  worse column gains.
- Better past a resolution limit, which needs no derivation (the old reading named something it
  could not resolve): 31 more method rows and one definition row of the fixtures, refused there
  for a declaration it was not supplied (26: a dependency's class, read now by its declaration),
  an inherited override it could not identify (5) or an imported definition (1).

The blockers the stretch aimed at, with the two it also touches, as the gate's "uncovered rows
need" lists them (each row counted once, under its first reason):

| Reason | Corpora before | Corpora after | Fixtures before | Fixtures after |
| --- | --- | --- | --- | --- |
| an enclosing class that inherits and is abstract or may be extended | 2382 | 0 | 339 | 0 |
| a class of a dependency | 1415 | 574 | 77 | 29 |
| an enclosing class with a member something overrides | 103 | 474 | 381 | 433 |
| a parent with constructor parameters or in a class | 179 | 181 | 75 | 96 |
| an enclosing class whose overriders are not read | 0 | 0 | 7 | 0 |
| an opaque type of a dependency | 6 | 0 | 12 | 0 |

- `an enclosing class that inherits and is abstract or may be extended` is gone. Its rows now
  count or stop at the next reason: mostly `an enclosing class with a member something
  overrides` (the old reading reads such a member as an open capability; not ported), `a class
  of a dependency` and the two-parent case.
- `a class of a dependency` that remains names, on corpus-core and cats-core, the standard library
  or the JDK (`scala.collection`, `scala.math`, `scala.util`, `scala.concurrent`, `java.nio`):
  classes the standard model does not hold, which the old reading does not read either.
  scalameta's classes are read now (corpus-core's rows of this reason: 1,078 → 215), their rows
  then stopping at `a class that is no product, sum or type-only template` (a record of abstract
  members, which no class is read as yet).
- `a parent with constructor parameters or in a class` (179 → 181, 75 → 96, rows that stopped at
  the inheriting template before) is unchanged in kind: such a parent's members may read state
  the template's own code may not.

Cost of the compiled front end, three runs each (`TastyFrontEnd.rows` over the corpus, wall time
and peak heap): corpus-core 3.4–3.6 s at `37668ad`, 3.3–3.4 s at `e952ebe`, 3.6–3.9 s at
`b3f8fce` (which reads scalameta's classes); cats-core 2.7–2.9 s, 2.9–3.0 s and 2.7–3.4 s; peak
heap unchanged (1.25 and 1.05 GiB). The class files of classes without TASTy are
read once per module, the classpath's only where a question is about a class outside the module.

#### The open member and parents with constructor parameters (stretch 8)

- An open member (`e0eb38a`): a concrete member a class of the program overrides varies across
  instances, so `Receivers` reads it as a capability, as an abstract one is, its own body one of
  its behaviors. `Reentry` decides the two views the scalameta front end has
  (`TargetViews`): a member whose own body evaluates the measured one on the instance as its
  result, a block statement or a block `val`'s value (`definite`, kept to calls no conversion,
  extension, given, same-arity overload or import can redirect) supplies nothing; one any
  behavior of which may reach the measured member (`may`: a call of its name, a super accessor,
  code outside the module called on the instance, the instance given away, an instance of its
  class built, a name that does not resolve, every body followed with its overriders,
  `ClosedProgram.overriders`) leaves the row. Whatever `definite` misses is left by `may`, never
  counted. An abstract member is a capability whatever its implementations call, as before.
- Parents with constructor parameters (`4c87c8d`): where the template or a class it extends
  passes the arguments, the parent holds a function of what the template holds; where a subclass
  passes them, each public field of the parent is a capability. A private field of such a
  parent, a field the line answers otherwise, a constructor field something overrides (two
  values: what the parent was given and what the getter answers) and a parent in a class still
  leave the receiver.

The differential, after `b3f8fce` and after `4c87c8d`; every input passes (definitions unchanged):

| Input | Table | Identical | Better | Worse | Uncovered |
| --- | --- | --- | --- | --- | --- |
| cats-core | definitions | 1357 | 21 | 1 | 180 |
| cats-core | methods | 411 → 413 | 213 | 19 | 7008 → 7006 |
| cats-eo | definitions | 130 | 6 | 0 | 15 |
| cats-eo | methods | 159 → 164 | 3 → 5 | 0 | 535 → 528 |
| cats-eo-generics | definitions | 7 | 0 | 0 | 5 |
| cats-eo-generics | methods | 27 | 0 | 0 | 53 |
| cats-eo-schemes | definitions | 1 | 0 | 0 | 1 |
| cats-eo-schemes | methods | 4 | 0 | 0 | 15 |
| cats-kernel | definitions | 307 | 0 | 0 | 5 |
| cats-kernel | methods | 176 → 182 | 3 | 0 | 1606 → 1600 |
| core | definitions | 369 | 3 | 0 | 269 |
| core | methods | 1003 | 1 | 0 | 2996 |
| fixtures | definitions | 4931 | 98 | 1 | 558 |
| fixtures | methods | 3820 → 4121 | 143 → 147 | 2 | 2777 → 2472 |

- Derived in `derived.txt`: `W.conv` (2^(2^96), a conversion that does not apply), cats-eo's
  `SimpleLens.place` and `transfer` (ω: `S` is `T`, so `combine`'s result can be split again)
  and the fixtures `Passed.mid.<new#1>.seed` (2) and `Passed.imported.<new#1>.seed` (16), whose
  parent's private parameter the instance's own construction passes.
- The two blockers are gone from the corpora: `an enclosing class with a member something
  overrides` 474 → 0 and `a parent with constructor parameters or in a class` 181 → 0 (12 fixture
  rows remain under the second: parents in a class and private fields a subclass passes). Their
  rows mostly stop at the next reason: `an enclosing sealed class or enum` (1,528 → 1,682), `an
  enclosing class with a bounded or higher-kinded type parameter` (1,343 → 1,458), `an enclosing
  class with a self type` (349 → 617).
- Cost, three runs each: corpus-core 3.4 s, cats-core 2.8–3.0 s, peak heap 1.25 and 1.15 GiB.

#### Self types, constructor rows and abstract type members (stretch 9)

- **Self types.** An untyped self alias (`self =>`) records the class's own type as its self type,
  so it was refused wherever a self type was. `World.ownSelf` reads a self type the class already
  conforms to (`appliedRefInsideThis <:< givenSelfType`) as none: `this` is of the class's own type.
  It frees receivers, capability records and type-only templates alike. A self type narrower than
  the class (11 classes in cats-core, `ApplyArityFunctions { self: Apply[F] => }` among them) stays
  uncovered: reading it is reading what a second parent brings, the case of templates that reach a
  parent through two parents, and it is left with them.
- **Constructor rows.** A constructor ignores the objects around it: an object's members compute
  from no state a position may take, so an object with parents is read like one without. A class
  in a class is built on an instance of it (`Constructors.count` takes the receivers around): a
  position over a type parameter places the caller's value or one the instance holds (`Outer[A](a:
  A) { class Two(x: A, y: A) }` is 3^2), a ground position is its value space, and the row lists
  the instance's captures, as the old front end counts and lists them.
- **Abstract type members.** An unbounded abstract type member of a receiver, declared or
  inherited, is an atom (`F[_]`-shaped: a rigid constructor), bound in the environment beside the
  type parameters and read only on the instance (`this.T`; another path to one is not read).
  Refused, as the old front end refuses: a bounded one; one two enclosing instances hold; one of a
  class no class written elsewhere extends (final, a value class, a private constructor of it or of
  a class it extends, a class private to a template around it), whose types are those the program
  picks; a sum's. tasty-query takes a parent's private type member for one a subclass inherits
  from elsewhere when it matches members (`P2 { private type X = Boolean }` beside `Q { type X }`
  makes two `to` methods one), so a private type member of a parent named like a held one refuses,
  and so does a declaration of the name elsewhere in the line that bounds it. A member is bound to
  the receiver that holds it: `this.T` of a fixed class inside it, which holds none, is not read.
- **`ClosedProgram.mixes`** is deleted: nothing in reach asks whether two templates share an
  instance.

Measured on the private gate copy, the merge (`ff83f6c`) against this stretch's head: method rows
identical, cats-core 644 to 701 (better 255 to 265), cats-kernel 191 to 275, cats-eo 187 to 188,
cats-eo-generics 31 to 37, corpus-core 1,385 to 1,531 (better 1 to 13), fixtures 4,435 to 4,580
(better 161 to 172); no row worse but the corrected ones. Blockers: `an enclosing class with a self
type` 735 to 184, `a capability record with a self type` 683 to 0, `an enclosing object with
parents` 245 to 0, `an enclosing class` 57 to 0, `an enclosing class with an abstract type member`
394 to 19 (sums), with 282 rows at `a bounded abstract type member`. Rows freed stop next at
the capability records' reasons (a member something overrides 54 to 508, a member named like the
measured definition 348 to 415) and at capabilities with type parameters of their own (148 to 274).

#### Re-entry among an enclosing class's members (group g3)

The receiver path of `Reentry` (the record-value path is g1's), ported from the old reading's
views (`TargetViews`, `ViewSandwich`), reading symbols and the typed tree:

- `definite` (the definite view) follows every position the body always evaluates: the result
  and a function literal the member returns as a function; block statements, strict `val`s; a
  call's function and the arguments its method type takes by value (not by name, not an inline
  method's, not `&&`/`||`'s operand); a selection's receiver; an `if`'s condition, a scrutinee,
  a named argument. A call is on the instance when it selects on `this`, names a member or local
  alone with a `this` or no prefix (not a member of another value an import brings), builds the
  measured constructor or a `c`, or selects a member of an object the instance names alone. A
  member further along the chain runs `c`'s nearest override (`effective`); its function literal
  is not applied. The old syntax guards (conversion, given, extension, same-arity overload,
  imports) are gone: the tree resolved the call. On a record value (`value`) only `this` and
  members named alone chain, as before.
- `may` (receiver): a member of an object named alone (`Predef.???`, an import) is a selection on
  that object, not a call on the instance; the measured member's own definition (a local one
  inside the member) is not a mention; building a class re-enters only where its construction
  reaches the measured member (a strict field initializer of it included) or a class of the
  module without a tree is built; a package named alone is no unresolved name.
- `Readings.bounded` (register D31): a row refused only by such members is counted again with
  them as capabilities (`Receivers(including = true)`); a 0 then is the count.

Owner ruling (re-entry through another instance is counted): the receiver rules refuse only for
the same instance (`this`, a class built within reach, `this` handed on); a path through another
value (`friend.Inner.k`, `import friend.target`) is never definite, and the search still leaves
it (more `?` only).

Rows (method rows, old numeric still uncovered, before -> after): fixtures 670 -> 605 (all 65 of
the group freed, 63 identical, `S2.later` corrected), cats-kernel 78 -> 76 (`Eq.eqv`,
`PartialOrder.partialCompare`), cats-eo 73 -> 72 (`singletonTo`); other inputs unchanged. Left
of the group: cats-kernel `Hash.hash` (an overrider of `neqv` the search follows through `Order`
instances reaches code it cannot settle) and cats-eo `singletonFrom` (the bound refuses for
another reason). Derived: 11 fixture rows and cats-core `Functor.map` (0, through the bound);
corrected: `S2.<extension@5>.later` 2^16 -> 2^8 (the old reading missed the extension call in
`m`). `ProgramRowsSpec.definitePositions` pins the positions, the bound and five cases that must
stay left.

#### Re-entry among an enclosing class's members, the rest (group h2)

The receiver path again (the record value's is g1's). What stays left, and why: a member that
calls the measured method with other arguments than its own parameters may return without calling
it again (the integration's base rule, `ArgF2.m = false`), so it is no definite view, and a count
that takes it for one is not derived; 35 of the group's 40 fixture rows and cats-kernel `Eq.eqv`,
`PartialOrder.partialCompare` rest on such a call and stay `?`. What is read now:

- A class's extension method is a method of its instance (`Shapes.member`, `extensions`), which a
  body in the class may call on any receiver: a capability over all its clauses. A capability
  record's is not read yet (g1's path).
- A member of a one-valued result supplies nothing, whatever it calls: neither re-entry nor an
  input not read leaves the row (`Receivers.own`, `inheritedCapability`, `Shapes.member`).
- A constructor without parameters builds one value, whatever the instances around hold
  (`Constructors.count`), as a method's one-valued goal does.
- A wrong count of both front ends, refused now: a local definition without parameters captures
  the parameters of the methods it is written in, which a call that reached it again may have given
  other values, so `again` holds only where those methods take none. `class C { def m(b: Boolean)
  = { def h: Boolean = ???; h } }` with `D extends C { override def m(b: Boolean) = false }`:
  `h = if b then m(false) else true` is total in a `C` (`m(false)` is the inner `h`, `true`) and
  in a `D` (`false`), and differs from the four functions of `b`; both said 4.
- Own members of a receiver are bound under their signed name, as inherited ones are: two
  overloads of one shape (extension methods on several receivers) are two capabilities.

Rows (method rows, old numeric still uncovered, before -> after): fixtures 399 -> 395 (`Ext.x` 0
and `T.H.<init>` 1 identical; `T.<extension@1>.flip` 4 -> 2^32 and `.choose` 2^8 -> 2^16
corrected: the old reading leaves a sibling extension method out, or reads it as its value at the
receiver, though it counts the plain-method twin the same as the new one), cats-eo 12 -> 8
(`singletonTo`, `singletonFrom`, `reconstructSingleton`, `mfAssocPSVec.maybeHitTo`, all
identical); other inputs unchanged; no row measured before is `?` now. Derived: fixture `E3.to`
4 (its overridden `m` returns `Unit`). `ContextualSpec`'s `Exts.get` 2 -> 16 is a correction of
the same kind. Left of the group: the 35 fixture rows and two cats-kernel rows of the base rule;
`Hash.hash` (as g3 left it); `C2.a` (an overrider of `d` returns `this`, which the search reads as
giving the instance away).

#### Re-entry on a record value (group g1b)

The owner's ruling D6 on `Reentry.may`'s record-value path (`instance = false`), finished from the
previous g1 agent's patch. A record value the body receives is a free capability; the row is left
(`Receivers.Reenters`) only where a behavior may re-enter on the same instance:

- `selfReenters`: a behavior run on the value hands `this` on, as `holder` reads it (an alias: a
  local, or a member of a class that may be the record's, with or without parameters, through
  every body a call may run; a pattern variable through its scrutinee; an instance of a class
  nested in such a class, which holds its outer instance; a member outside the module selected on
  or named in `this`, which may return it), to the measured member, a method of the module that
  passes it on (`passes`; an abstract one a function literal implements may), a constructor of the
  module whose members (its nested classes' included) or construction reach the member (a carried
  outer instance included: building an inner class runs its parents' arguments), or code
  outside the module, as an argument or the receiver, where that code may reach the member or the
  call gives it a function that may (`callsBack`: a literal whose body reaches it, any other term
  of a function type or of a class a literal of the module may implement, applying one).
- `handsMade`: an instance of the record's class the module builds or names (`madeOf`: a `new`, a
  function literal, an object, a factory, a pattern variable's scrutinee) is handed on anywhere in
  the module (`handings`), the measured member has an input that may hold it (a value parameter, a
  capture or an outer instance, its receiver or the receiver's fields), and the plain search
  reaches the member. The old narrowing by the class each parameter names was unsound for a
  wrapped input (`IterableOnce[Eq[A]]`); only "no input at all" narrows now.
- A constructor no longer dispatches by name (`new Object()` read as running `ListEq.<init>`);
  `built` reads a case class's synthetic `apply` through its type lambda.

Speed: `handings` is one index per module (every call and selection, with the classes of what it
hands on), read by `madeOf`; `madeOf` and `holder` are memoized per tree and member through `Fixed`
(least fixed points read on demand: every answer is read once per outermost read, and one that
depended on a member still being read is kept after only where the outermost answer is the
bottom); `ClosedProgram` memoizes `overriders`. Gate times (as the gate reports them, same machine,
`e318915` -> this): cats-kernel 10.4 s -> 14.2 s, cats-core 12.0 s -> 10.7 s (the patch as handed
over did not finish cats-core in 25 minutes).

`ProgramRowsSpec`: `anotherInstance` (2^256; and `Use.all`, with no input, 2^8), `sameInstance`
(four refusals), `sameInstanceHidden` (twenty-four same-instance programs four correctness
reviews found, each refused). Left open (more refusals only, or an owner question): `this == this`
with a module `equals` (the universal `==` is outside the model); `handsMade` looks anywhere in the
module, not within reach of the behavior; a local or anonymous class's captures make any home not
nested in objects alone an input (`Eq.allEqualBoundedSemilattice.<new#1>.empty` stays left). Rows (old numeric still uncovered, methods): cats-kernel 78 -> 30, fixtures 399 -> 397;
other inputs unchanged. Every better row was derived (the patch's four cats-kernel lines); its five
cats-core derivations stay in `derived.txt` for rows still left.

#### Record re-entry on cats-kernel (group m1)

The 26 cats-kernel rows g1b left on "an overridden member that may call it" (25 on a capability
record, `Hash.hash` on an enclosing class), each decided by hand first. All 25 record rows but one
refused through `handsMade` at one handing: `Eq.allEqualBoundedSemilattice`'s `combineAllOption`
builds `Some((x, y) => materialized.forall(_.eqv(x, y)))`, a literal implementing `Eq` handed to
`Some.apply`. `Some` cannot call a member of an `Eq`, so no same-instance path exists through it
(class (b), an over-approximation): `flows` refused every call to code outside the module where
that code may reach the measured member, whatever it was given, and `callsBack` counted every
literal given to it as callable.

The narrowing (`Reentry.inert`): code outside the module, given only terms it cannot call, runs no
code of the module (no side effects: it holds nothing from before, and calls only what its own
types declare). Inert, by the term's type: a plain value; a class of the module whose parents
outside it are `Any`, `Matchable`, `AnyVal`, `Object` or `java.io.Serializable` (`Product` and
`Equals` declare members a class may implement, and `ScalaRunTime._toString` calls them); an
object outside the module; a type parameter bounded by an inert type; `Option`, `Some`, `List` or
`Vector` of inert types (strict containers; any other outside value may wrap a function of the
module: `Ordering.by(f)`, a view, a lazy list). Not inert: a top type or an unbounded parameter
(outside code may test the value: `ScalaRunTime.stringOf` calls an `Iterable`'s `iterator`),
`this` (its `ThisType` is not read), varargs, refined, union, intersection and opaque types. Read in `flows` (a call to
code outside the module refuses only where a handed term is not inert), `callsBack` (a literal
counts only where it is not inert) and `handsOn`'s selection of an outside member. The instance
path's `handsOver` is unchanged.

Rows, by hand:

- (b), now counted: `ListEq`, `SeqEq`, `VectorEq`, `OptionEq`, `EitherEq` `eqv` and `<init>`
  (`loop` too), `ArraySeqEq`, `LazyListEq`, `StreamEq`, `MapEq`, `QueueEq`, `SortedMapEq` `<init>`,
  `Eq.apply`, `allEqualBoundedSemilattice.empty` and `combineAllOption`,
  `anyEqualSemilattice.combineAllOption`, `StaticMethods.iteratorEq`: 24 rows, each identical to
  the old front end. No behavior of `Eq.neqv` hands the record value itself back: the instances
  `Eq`'s behaviors reach are other instances (a `ListEq`'s `ev`), and the only module-built `Eq`
  handed on is the literal above. Better (old `?`, budget), derived in `derived.txt`: `Eq.and`,
  `Eq.or`, both semilattices' `combine`, 2^(2^17) each (two records of eqv and neqv observed on
  the four ordered pairs: 16 Booleans; two members of 2^(2^16)).
- (b), left: `StaticMethods.iteratorPartialCompare` (old ω). `SortedMapPartialOrder.partialCompare`
  hands it a tuple `PartialOrder` built afresh at that call (and an `Order` built in a local of the
  same call), which reaches it again only through `V.partialCompare` on another instance: an
  instance built during a behavior is never the record value. Counting it needs a freshness rule in
  `madeOf` (a `new` or literal evaluated in the handing, through methods and the call's own locals,
  holds no earlier instance); not done.
- (b), left: `Hash.hash` (old 2^128), the instance path: `Eq.neqv` -> `eqv` -> `Order.eqv` ->
  `compare` -> ... -> `DeadlineOrder.compare`'s `x > y` and `BigIntOrder`'s `x compare y`, which
  `handsOver` reads as handing module values to outside code. The values are of final outside
  classes (`Deadline`, `BigInt`) that hold no module code; inertness of such classes (final, with
  inert fields) would count it, a longer rule for one row; not done.
- No (a) or (c) row: none rests on an unsound old rule, none re-enters on the same instance.

Implicit `this` (agent m2's question): a call of an inherited member outside the module written
without `this` is read by `selfReenters`' `onInstance` case as the `this.` form is; both refuse,
pinned (`lt` and `this.lt` calling back into `compare`, which hands on `Reg.c`).

`ProgramRowsSpec`: `anotherInstance` gains `wrapped` (the cats-kernel shape, 2^256);
`sameInstanceHidden` gains seven refusals: `Cap` beside an `Ordering.by` literal, a literal
`Ordering`, `Cap` as an `Ordering`, the two `lt` forms, a `Product` whose `productElement` hands on
`this`, and an `Iterable` typed `Any` given to `stringOf` (the last two found by the correctness
review, each counted by a first version). Left open, older than this unit: a member of `Product` or
`Equals` a class of the module implements, selected on the instance (`me.canEqual(1)` with `me:
Equals`), is skipped as universal by `selfReenters` and `handsOn` (2^256 before and after).

Gate (all seven inputs pass): only cats-kernel's method rows move, 639 / 19 / 0 / 1,127 (identical
/ better / worse / uncovered) -> 664 / 23 / 0 / 1,098; old numeric uncovered 28 -> 4, past the
old-unsound list 26 -> 2. Gate times: cats-kernel 15.8 s -> 18.1 s, cats-core 19.4 s -> 19.6 s
(shared machine).

#### Record re-entry outside cats-kernel (group m2)

The 19 old numeric rows left as "an overridden member that may call it among the members of a
capability record" on cats-core (16), core (2) and cats-eo (1), checked by hand against D6 on
`a9ffbc2`. No row has a same-instance re-entry in its program, so none is (a) or (c); each is (b),
an over-approximation, and none is freed by a local narrowing. No code changed.

- The `===` family (`Const`, `EitherT`, `IorT`, `OptionT`, `OneAnd`, `EqOps`), `Op.eqv` and
  `Const.hash`: the record is the `Eq` (`Hash`) evidence. The module's instances that call these
  methods (`catsDataEqForConst`'s `_ === _`, and so on) pass their own evidence, a strict parameter
  given before the instance is built, never the instance. The module's only constructions that
  tie a knot, `Defer.fix` and `Defer.recursiveFn`, have no caller in the module.
- `catsDeferForEqCache` (its `Deferred.<init>` and `defer`) and `catsDeferForHashCache.defer`: a
  `Deferred`'s behaviors run the thunk it was given, and only a knot (`fix`, no caller) feeds an
  instance back to `defer`.
- `catsDataUpperBoundedForConst.maxBound`, `catsDataLowerBoundedForConst.minBound`: the record is
  the evidence's `partialOrder`. The instance's own `partialOrder` is a new instance built around
  it, so an instance would have to hold itself, again a knot.
- `catsDataCommutativeArrowForKleisliId`, `…CokleisliId`, core's `Size.order` and
  `FinitePart.order`, cats-eo's `mfAssocPSVec.singletonTo`: no behavior of the value the row
  reads (`Order.from` of core's functions; the arrow instances; `singletonTo`'s `ah`, which the
  site receives as the inner optic, while `andThen` hands `this` on as the outer one) reaches the
  site with that value.

What refuses them on this tree, from tracing `Reentry.may`:

- `selfReenters` first, through four over-approximations, each sound to remove and freeing no row
  on its own (all seven inputs identical):
  - a call selected on `this` of a member declared outside the module counts as handing `this` to
    that code (`this.eqv(x, y)` in kernel's `Eq.neqv`), though `selfReenters` already reads members
    selected on the instance, and the same call written without `this` (an `Ident`) is not counted;
  - a constructor field outside the module selected on `this` may return `this` (`SortedSetHash`'s
    `hashA`), though no constructor argument can name the instance being built (Scala rejects
    `this` there) and the module's own fields are not read so;
  - a function literal's parameter holds the receiver and arguments of a call of the module's own
    method (`this.dimap(fab)(f)(identity)` in `Profunctor.lmap`), whose body `passes` and
    `selfReenters` read anyway;
  - a member of a class that may be the record's, selected on another value (`o.to(s)` in eo's
    `Composer`), is read as if selected on `this`.
- Then `handsMade`, which is what keeps every row: a module-built instance of the record's class
  is handed to code outside the module (`Eq.by(_.value)` in `NonEmptyVector`) while `outward` holds
  (`callersOf` is open because function literals call the site), and the plain search reaches the
  site through a held function (`Deferred`'s `fa`, `Arrow.lift`'s `f`) or the receiver's state (a
  home class not inside objects). Narrowing this soundly needs a flow from the entries code
  outside the module may call (function literals, members it overrides) to the record's position
  at the site, with positions kept apart (`passes` takes any argument of the site for the
  record's). That is an interprocedural analysis, not a local change, so it was left. The four
  narrowings above are kept as a patch outside the tree for whoever takes it on.

Gate time for cats-core on this tree: 19.8 s (baseline run, shared machine).

#### Product and Equals members on a record value (group m5)

A wrong number m1 found, older than m1's change: `Reentry` skipped every member of `Product` and
`Equals` as universal (`World.universal`), so a class of the module implementing one by handing on
`this`, called through an alias typed by that trait (`val me: Equals = this; me.canEqual(1)`),
counted 2^256. `Reentry` now skips only what `Any`, `Matchable`, `AnyVal`, `Object` and
`Serializable` declare (`silent`), in `fromInstance`, `outside`, `flows`, `handsOn`,
`selfReenters` and `holder`'s `returned`: more refusals only. Also ported from `tasty-int`
(`d984c07`): `inertClass` reads every supplied class below the static one. `sameInstanceHidden`
gains the two alias programs and the four `stringOf` programs. Rows: none changes on any of the
seven inputs.

Left open: the argument-position flow for `handsMade` (the 16 cats-core, 2 core, 1 cats-eo rows).
A design was tried (`/tmp/m5/flow-wip.patch`): the measured member's input only at positions whose
type may hold the record (`Outside.holds`), a constructor read through its class's code with the
tainted fields, and code outside the module routing a value only through the module's entry points
(members it may call, function literals), searched once per measured member and record class. It
passes `ProgramRowsSpec` but moves no row: the global entry search meets `x === y` on cats'
newtypes (`NonEmptySetEq.eqv`, `sortedMap.scala`), whose selection tasty-query cannot resolve
(`None.get`), and an unresolved name refuses. A correctness review also found that same-instance
re-entry through the extractor protocol (`this match { case C(x) => x }` with a `_1`, `get` or
`isEmpty` handing on `this`) counts 2^256, before and after this unit.

#### Calls the compiler inserts, on the same instance (group m6)

Two wrong numbers, both 2^256 where a same-instance path exists: m5's `Product`/`Equals` members
(cherry-picked, `4f40d4d`, with its regressions), and the calls a match runs, which no tree holds.
`Matches` writes those calls as tasty-query trees on the scrutinee, built once per tree (`Fixed`
keys by identity), and `Reentry`'s `search` and `visit` read them as written ones. Which calls, as
Scala 3's `PatternMatcher` chooses them (each checked at run time with a probe):

- the extractor applied to the scrutinee (a generic one through `Reentry.unapplied`, its using
  clause after);
- on its result: `_1`… for a `Product` of as many as the patterns (a case class, `Tuple2`; never
  `isEmpty` or `get` then); nothing for a `Boolean`; else `isEmpty` and the parameterless `get`,
  and `_1`… of `get` for more than one pattern;
- `unapplySeq`: `lengthCompare`, `drop`, `toSeq`, `apply` on the result (or `get`), or, for a
  `Product` that is no sequence, `_1`… and the sequence protocol on the last;
- a stable pattern: `equals(Any)` of its value given the scrutinee, and an `==` overload of its
  class the scrutinee's type fits;
- outside matches: `==` and `!=` of `Any` run the receiver's `equals(Any)`, `##` its `hashCode`.

A nested pattern is matched against what its part yields. Overloads (`equals(Q)`, `get(Int)`) and
private members are not selected; a name that does not resolve adds nothing. `referrers` notes
`_1`… up to the pattern count plus one, and `equals`/`==` for a stable pattern.

Regressions: `sameInstanceHidden` gains 16 programs, each refusing and each counting 2^256 without
`Matches` (checked); `anotherInstance` gains a program counting 2^256 (D6: no same-instance path) with
a name-based extractor given `this`, a tuple pattern over another instance, a case class whose
`isEmpty` the product match never calls, and `==` / a stable pattern on an `Any` beside `==(Q)` and
`equals(Q)` overloads. Rows: none changes on any of the seven inputs (more refusals only, and none
of the corpus's matches reaches a measured member on the same instance).

Left open, found by the correctness review (wrong numbers, 2^256 where `Use.same` is reached on the
same instance), outside this unit:

- a universal member selected on a receiver typed `E` or `Any` dispatches to the module's override,
  which `silent` never reads: `val me: E = this; me.toString` with `toString` handing on `this`;
- `inert` takes a class of the module overriding `toString`, `hashCode` or `equals` (with no other
  parent outside the module) for one code outside it cannot call back: `s"$this"`, `"" + this`,
  `List[E](this, this).distinct`, `Set[E](o, this)`, `Some(this) == Some(o)`;
- pre-existing over-approximation the matches now also reach: an extractor wrapping its argument
  (`new Opt(e)`) refuses if any member of `Opt` reaches the site (`flows`' constructor branch), as
  the written `X.unapply(this).get` did.

The review found no miss in for-comprehensions, `copy` and default arguments, implicit and
`Conversion` applications, extension methods, inferred givens, `Ordering`/`Ordered`, `apply` and
`update` sugar, `+=`, dynamics, or the synthetic members of a case class: TASTy holds them as calls.

## Phase 5: the TASTy front end, slice by slice

Each slice lands when the differential runner shows no **worse** row for the rows it covers and its
better rows are accounted for ("The gate" and "Refined slice order" below have the full rule). A row
no slice covers yet is left uncovered by the new front end, which never asks the old one; the old
front end stays the report's until none is left.

| Slice | What it reads from symbols and typed trees | Replaces |
| --- | --- | --- |
| 5a. Types to shapes | A compiled type as a `Shape`: ground types, products, sums, functions, type parameters, aliases dealiased by the compiler, opaque types by scope (rule F), the stdlib model keyed by symbol, top types by place. | The type reading in `analysis/resolution` (8,950 lines) and `types` (2,264), and every lookup by name: D3 is this slice. |
| 5b. Definitions table | Class symbols: fields, constructors, sealed children, enum cases, public factories (rule B), recursion. | `capacity` (6,453) but for `Size`. |
| 5c. Method environments | A target's inputs, receiver fields, captures and enclosing scopes; abstract and overridden members as capabilities; the closed world over exact parent and override symbols; given names; context-function binders from the declared type. | `analysis` top level (5,583), `analysis/capability` (2,517), `analysis/model` (1,530); override identity and given names (the first plan's phase 3 items) are symbol queries here. |
| 5d. Bodies | Typed trees where the contract reads a body: inline bodies and the inline methods they call (compile-time operations are symbols and tree nodes), the conversions the body applies (D2), the givens it resolved (D4), factory bodies, constructor statements, defaults, capture aliases. | `analysis/expansion` (3,747), including the hand-written `LibraryInlines` table. |
| 5e. Dependencies as symbols | A dependency's declarations read from its symbols, with the stubs' guards kept: no body is supplied, no signature guessed. | The stub rendering and parsing in `tasty` (1,506); a report no longer parses about 5,200 stub sources. |

## Phase 6: switch over and delete

- The plugins and `Report` call the TASTy front end. Sources are still read, for a row's signature
  text and line only.
- Delete the scalameta front end, the stubs, `CompiledTypes` and the closed world's text edits.
  Rule T becomes trivial: every type is the compiler's.
- Tests: fixtures already compile through `cardinality.Compiled`, and one that names an unsupplied
  declaration on purpose is compiled against support the analysis is not given ("Gate assets").
  The 60 in `rejected-fixtures.txt` go with uncompiled support.
- Sources without compiled classes are not analyzed at all until uncompiled support returns.
- Measure again on the frozen corpus: rows, moved rows, wall, CPU, memory.

## Switch-over: wiring (2026-11-13)

- One switch: `Report.Environment.frontEnd: Report.Front` (`Scalameta`, the default, or `Tasty`).
  `Report.of` reads its rows from the front end it names. Under `Tasty`, `Report.query` is the
  module's report filtered by qualified name (D7), and a jar of compiled classes among the paths
  of `Report.of` joins the classes' output, its sources jar naming and placing the rows (D8;
  `39009dd`).
- sbt: setting `cardinalityFrontEnd` (autoImport alias `CardinalityFrontEnd`), default
  `Report.Environment().frontEnd`, passed by `cardinalityReport`, `cardinalityReportOf` and
  `cardinalityReportSelected`. A project that does not compile hands no classes: under `Tasty`
  every source is `could not read <file>: no compiled classes supplied`, the task fails and the log
  says `sources do not compile: the TASTy front end reads none of them`.
- Mill: `def cardinalityFrontEnd: Report.Front`, same default, passed by the three commands. A
  module that does not compile fails the command at `compile()`, under either front end.
- Both plugins already hand the TASTy front end what it reads: `CompiledTypes.Classes(module
  output, dependency classpath)`. `cardinalityReportOf` hands `Classes(Nil, dependency classpath)`
  for a binary jar to be read against; sbt resolves that classpath under `Tasty` only (`Def.taskIf`),
  Mill under either front end (a command evaluates all its tasks).
- Flipping the default is the one line `frontEnd: Front = Front.Scalameta` in `Report.scala`.
- Tests: the scripted `report` project now compiles (`Timeline` is a plain class: a case class
  cannot take a by-name parameter; before, it was reported from syntax alone) and runs again
  under `Tasty` with the same expected report, then checks that a source named to
  `cardinalityReportOf` without classes and a project that does not compile are refused. The Mill
  spec reports the fixture under both front ends and checks that only `Tasty` refuses
  `cardinalityReportOf` on a source. Scripted `selected` runs again under `Tasty` (the support
  compiled into the module, no request key or work lines); scripted `library` compiles the
  vendored library into a binary jar and reads it beside the sources jar (the row of the sealed
  `Optic`, not read yet, is not checked). `ReportQuerySpec` and `ReportSpec` pin both readings.

## Switch-over: deletion inventory (2026-11-13)

Measured by deleting on the throwaway branch `tasty-switch-dry` (`703f43f`, from `a1be95c`), not
estimated. The list was computed by a symbol-level reachability pass over SemanticDB (each
reference attributed to the innermost definition whose extent, read with scalameta, holds it;
tool: `/var/cache/users/rhansen/cardinality-tasty/tools/Reach.scala`, with its cut list
`reach-cuts.txt`). The roots are every symbol of `compiled/` and what the plugins reference. The
cuts are `ScalametaFrontEnd`, the stub rendering (`StubFile.render`, `StubFile.key`),
`CompiledTypes.ascribe`, `Report.query` and everything else of selected reports, and
`Environment.stubs`, `classpath` and `frontEnd`. Then the deletion was applied and repaired until
`core/compile`, `plugin/compile` and `millPlugin/compile` passed with `-Werror` (see "Dry run").

**Total: 30,487 lines net (30,558 deleted, 71 added) in 154 files.** That is 124 whole files
(25,713 lines) and parts of 30 (4,774). Core's main sources go from 43,614 lines to 13,349: 6,424
of them `compiled/`, 6,925 the shared rest.

Whole files (lines), under `core/src/main/scala/cardinality/`:

- `analysis/` (3,457 lines): AnalysisWork 78, BodyScopes 205, CaptureAliases 80, ClosedWorld 380, ConstrainedReceivers 111, ConstructorPositions 272, ContextBinders 125, ContextualScope 201, DeepStack 32, ForeignParents 81, HeldCaptures 233, ImportBinding 105, ImportShadowing 28, InheritedClosedSlots 133, Measurement 718, Measuring 96, ParameterScope 18, ReceiverFactories 223, Resolver 153, SealedReceivers 108, SecondaryConstructors 52, ViewSandwich 25
- `analysis/capability/` (2,517 lines): CapabilityRecords 502, Construction 129, DeferredInline 50, Insertions 101, PassedState 59, ReceiverCalls 210, ReceiverCapabilities 159, ReceiverMembers 696, TargetViews 611
- `analysis/expansion/` (3,747 lines): BodyTerms 753, BodyTypes 131, BodyWalk 769, Calls 104, Conversions 525, ImplicitScope 21, InlineBodies 288, LibraryInlines 357, Reachable 504, RestatedExtensions 186, Unconverted 91, Valued 18
- `analysis/model/` (731 lines): Arguments 28, ContextBounds 44, ContextualEvidence 182, DeclarationIdentity 8, DeclaredMembers 70, FactorySyntax 103, MemberSlot 24, Throws 74, TypeOperators 163, UniversalMembers 35
- `analysis/resolution/` (8,444 lines): BinderBounds 224, BoundChecks 98, Callees 112, ConstructorApplications 275, ContextFunctions 343, Denotation 51, FinalImplementations 184, Found 62, InferredResults 76, InheritedObjects 37, InheritedScopes 186, InheritedTerms 139, InheritedTypeMembers 480, Linearization 55, LookupBindings 216, MethodProjections 344, MethodTypeLambdas 91, NominalKeys 309, OpaqueTypes 46, OverrideIdentity 307, RefinedMembers 296, Representations 470, RuntimeClasses 435, ShadowedBuiltins 761, SingletonIntersections 288, SourceLookup 802, SuppliedNamespaces 81, TemplateClosures 81, TermBindings 79, TermMembers 167, TransparentResults 36, TypeIdentities 362, TypeMembers 741, UnionSums 190, WrittenType 20
- `capacity/` (4,777 lines): Builds 96, Counter 665, EnumCompanions 76, Exposure 84, Factories 118, FileParents 43, Imports 202, Inheritance 289, Instantiations 194, Library 286, Namespaces 605, OpaqueScopes 28, PackageMembers 151, ParentExports 58, ParentNames 169, Positivity 136, Qualified 17, Reading 148, SealedFamilies 422, Solver 288, ValueRows 37, Walk 665
- `reporting/` (84 lines): ScalametaFrontEnd 84
- `request/` (374 lines): AnalysisQuery 123, SourceSnapshot 251
- `tasty/` (481 lines): StubDeclarations 481
- `types/` (1,101 lines): MatchTypes 160, Memo 12, NamedTuples 32, Overrides 183, SealedSlots 312, StandardConstructors 32, TokenLayout 219, TypeName 24, TypePlaceholders 74, Variance 53

Parts of files (lines deleted, net):

- `capacity/Scope.scala` 751 of 800 (the read state; `Scope`, `World`, `Named`, `Members`,
  `Child` shells stay, typed for `Fixpoint`)
- `analysis/SourceIndex.scala` 707 of 737 (`SourceIndex.Root` and a few names stay)
- `analysis/MethodAnalysis.scala` 603 of 668 (`Entry`, `Frame`, `Resolved` stay)
- `types/ListedNamespaces.scala` 290; `analysis/model/MemberSyntax.scala` 244;
  `tasty/StubTypes.scala` 248; `tasty/CompiledTypes.scala` 204 (the syntax ascription;
  loading, `dependencies`, `javaFacts` and `unnamed` stay); `analysis/resolution/TopTypes.scala`
  185 (the syntax half; `polarized` and `terminalGoal` stay); `tasty/TastyStubs.scala` 163 (the
  rendering; the class loading `CompiledTypes.loaded` calls stays); `reporting/Report.scala` 155
  (query, `Front`, `stubs`, `classpath`); `analysis/StdlibModel.scala` 136;
  `types/TypeSubstitution.scala` 139; `types/Kinds.scala` 110; `types/Trees.scala` 109;
  `analysis/model/ResolutionContext.scala` 84; `analysis/model/ScopeTerms.scala` 68;
  `tasty/StubFile.scala` 64; `types/TypeApplications.scala` 59; `analysis/inhabitation/ExistentialInputs.scala`
  45; `analysis/BuiltinTypes.scala` 40; `types/Spelling.scala` 39; `analysis/resolution/LiteralTypes.scala`
  29; `capacity/Definition.scala` 23; `analysis/model/SourceBindings.scala` 20;
  `analysis/inhabitation/ClosedSpaces.scala` 16; `analysis/model/MemberPath.scala` 13;
  `analysis/resolution/StuckProjections.scala` 5; `analysis/model/GivenNames.scala` 3.
- `plugin/.../CardinalityPlugin.scala` 129 net and `mill-plugin/.../CardinalityModule.scala` 93
  net in the first dry run: the stub tasks and format, `cardinalityReportSelected` with its four
  query settings, `cardinalityReportOf`, the front-end setting. Since D7 and D8, the two report
  tasks stay (each a few lines); the query limits and budget settings, `AnalysisQuery` and
  `SourceSnapshot` still go.

Largest whole files: `SourceLookup` 802, `BodyWalk` 769, `ShadowedBuiltins` 761, `BodyTerms` 753,
`TypeMembers` 741, `Measurement` 718, `ReceiverMembers` 696, `Walk` 665, `Counter` 665,
`TargetViews` 611, `Namespaces` 605.

What stays because the TASTy front end shares it (6,925 lines outside `compiled/`):

- The solver and its model: `analysis/inhabitation/` (`Inhabitation`, `GoalSolver`,
  `EvidenceAnalysis`, `PolymorphicMembers`, `Natural`, `RuleGraph`, `NeutralSaturation`,
  `RepeatedArguments`, `OpaqueSumForwarding`, `SequenceElimination`, `TerminalHigherOrder`,
  `Card`, `ClosedSpaces`, `ExistentialInputs` in part).
- The stored side: `capacity/Fixpoint`, `Size`, `BaseTypes`, `Definition`, and the `Scope` shell.
- The model the plan keeps out of TASTy: `StuckProjections`, `LiteralTypes`, the shape half of
  `TopTypes`, `BuiltinTypes`, `StdlibModel` (in part), `types/TypeSubstitution` (`platedShape`),
  `Universe`, `Graphs`, `Traversals`.
- Class loading: `tasty/CompiledTypes` (loading part), `TastyStubs` (loading part),
  `JavaClassfile`, `TastyNames`, `tastyquery/Variances`.
- Reporting: `Report`, `FrontEnd`, `Blockers`, `Locations`, `Skill`. With one front end,
  `FrontEnd` (24 lines) can be inlined.

Second-order residue, not deleted by the dry run because something still reaches it: about 700
lines in 10 files still import scalameta (`MemberSyntax`, `GivenNames`, `ScopeTerms`,
`ResolutionContext`, `MemberPath`, `Trees`, `TypeApplications`, `TypeSubstitution`, `BaseTypes`,
`MethodAnalysis`); remnants of syntax helpers also stay in `ListedNamespaces` 65, `Spelling` 47,
`Kinds` 17, `StubTypes` 29 and `StubFile` 21. Most are held by two small references: `StuckProjections.refusal`
calls `SourceBindings.label`, which calls the extractor `MemberPath.Atom`, and that keeps the case
class `MemberPath`, `Frame` and everything under `Frame#values`. `StdlibModel` reads
`SourceIndex.Root`. Moving those two out is a hand edit, not part of the mechanical deletion. It
should free most of those lines and then the `scalameta` dependency (to check: `BaseTypes`,
`TypeSubstitution`).

Done in `403046a` (2026-11-13), without changing any reading: atom naming is `Atoms` (was
`MemberPath.Atom`, `SourceBindings.label`/`name`), the root package's name is
`StdlibModel.RootPackage` (`SourceIndex.Root` aliases it), and the generic `plated` and `hylo`
moved from `TypeSubstitution` to `Traversals`. Measured by a second dry run, `tasty-switch-dry2`
(`7a2938c`, from `161283b` with the move): 31,103 lines net (31,204 deleted, 101 added) in 160
files. Core's main sources go to 12,736 lines, 6,312 outside `compiled/`. `core`, `plugin` and
`millPlugin` compile with `-Werror` without the `scalameta` dependency, and without `kyo-core`,
which only `Measuring` used. That needed one more hand cut: `BaseTypes.unapply` on a scalameta
`Type`, which the tool keeps because it keeps every `unapply` of a reached object.

## Switch-over: spec inventory (2026-11-13)

`scripts/spec-ledger.sh` at `b977b40` (`a1be95c` adds one `ReportQuerySpec` example, not in the run): PASS 975, UNCOVERED 426,
FAIL 231, PENDING 19, SCALAMETA 81 (1,732 examples read rows or the scalameta front end). The
FAILs are sorted into classes by the pattern of their failure under `filled`. That is a
classification of messages; no row was derived again.

| Class | Examples | What it is | At the switch |
| --- | --- | --- | --- |
| (a) pins old behavior or spelling | 40 | row identity (10: `key not found`, `_root_` signatures), signature printing, reason atoms named differently (`Scopes$package$.ev[T]` against `T`, `en.E.A` against `A`), 4 TIGHTENED (`≤2^32` → `2^32` three times, `≤2^8` → `2^8`), `instantiation`/`open` member names; by pattern, so a few rows belong to (b) or (c) (`TypeMemberScopeSpec`, `GroundEnvironmentSpec`) | rewrite to the new text |
| (b) pins a refusal the compiled tree resolves | 88 | the example expects `?` (a `beLike` on `Unresolved`, `unresolved[…]`) and the new front end gives a number (`Finite`, `Countable`, `2^k`) | rewrite to the new count, each derived (`derived.txt` / `corrected.txt` rule) |
| (c) not at parity | 474 | 426 UNCOVERED, and 48 FAIL where the new front end refuses and the old counted: `inline method` 22, `transparent inline result` 7, `union alternatives require an overlap…` 7, `ambiguous anonymous evidence` 4, `sealed family has anonymous instances` 3, 5 others | wait for parity, or an owner decision on the new refusal |
| (d) tests of deleted code | 81 SCALAMETA examples, and the files below | the scalameta tree, stubs given as sources | delete with the code |
| fixture scalac rejects | 54 | `no compiled classes: scalac rejects this fixture` | go with uncompiled support (`rejected-fixtures.txt`) |
| count differs | 1 | `Finite(4) != Finite(2)` | derive: a correction or a bug |
| PENDING | 19 | `ArticleCardinalitySpec` targets | unchanged |

UNCOVERED by specification, the larger ones: `TargetViewsSpec` 23, `StdlibModelSpec` 19,
`CapabilitySpec` 19, `EnvironmentSoundnessSpec` 18, `PureCodeSpec` 17, `TopTypesUnionSpec` 16,
`StoredValueSoundnessSpec` 16, `ExistentialInputSpec` 16, `TypeMemberSpec` 15, `NamespaceSpec` 15,
`EnvironmentCompletenessSpec` 15, `ContextBindersSpec` 13, `TypeMemberBoundarySpec` 12,
`ReentrySpec` 12; 44 more with 1 to 10.

SCALAMETA by specification: `SignatureSpec` 27, `GivenSignatureSpec` 18, `ExtensionSignatureSpec`
15, `TastyStubsSpec` 9, `MeasuringSpec` 3, `ReportSpec` 2, `ClosedWorldSpec` 2, one each in
`TopTypesUnionSpec`, `StdlibModelSpec`, `NamespaceSpec`, `DefinitionInventorySpec`,
`DeepStackSpec`.

Test files that stop compiling on the dry branch (`core/Test/compile`, 331 errors in 64 files):

- Delete whole, class (d), 1,669 lines: `ReportQuerySpec` 129, `request/AnalysisQuerySpec` 266,
  `request/SourceSnapshotSpec` 195, `tasty/TastyStubsSpec` 370, `tasty/CompiledTypesSpec` 145,
  `analysis/MeasuringSpec` 91, `reporting/ScalametaInputs` 16, `tasty/TastyDump` 30,
  `reporting/ReportDependenciesSpec` 114, `analysis/DeepStackSpec` 45, `capacity/SignatureSpec`
  122, `capacity/GivenSignatureSpec` 76, `capacity/ExtensionSignatureSpec` 70. The gate itself
  (`compiled/Differential` 752, `DifferentialSpec` 275) goes once there is no old side. The eo
  baselines (`EoCheckout` 361, `EoBaseline` 192, `EoDelta` 230, `EoArtifactsSpec` 449) read
  sources without classes: delete, or port to compiled classes.
- Edit, a helper each (1 to 29 errors): the harness `Compiled` (its `filled` mode and
  `scalametaOnly` read the scalameta front end), `Overriders`, `InventoryFixtures`, the compiled
  specs' helpers (`ClosedVerdicts`, `InlineVerdicts`, `TastyFrontEndSpec`, `TypeParametersSpec`,
  `RecordsSpec`, `ReceiversSpec`, `NamesSpec`, `ContextualSpec`, `StoredRecursionSpec`,
  `ProgramRowsSpec`), and 30 analysis or capacity specs, mostly one reference each
  (`MethodAnalysis.Limits`, `AnalysisWork`, `Report.Environment(stubs = …)`), and
  `ReportHygieneSpec` (14), `ReportSpec` (2), `RecursionProbeSpec` (1).

## Switch-over: dry run (2026-11-13)

Branch `tasty-switch-dry` at `703f43f` (worktree outside the repository, never merged): default
flipped by deleting the switch, the inventory deleted, plugins reduced to `cardinalityReport`
and `cardinalitySeedSkill`. `core/compile`, `plugin/compile` and `millPlugin/compile` pass with
`-Werror`; the tests do not compile (see the spec inventory).

What broke after the computed deletion, and what it took (two passes of the tool, about ten compile
rounds):

- The first cut list was too wide: `TastyStubs` whole, though `CompiledTypes.loaded` uses its class
  loading (`firstLoaded`, `jdk`, `readable`, `javaClassfiles`, `symbolsOf`). Fixed by cutting only
  the rendering. Two reachability rules were also too generous: a companion object reached
  kept its non-case class (`SourceIndex.Root` kept all of `SourceIndex`), and a package reached
  kept its enums. And one was missing: an override reached keeps the member it overrides
  (`Kind.holes`).
- Syntax left by deleting members: an emptied `object Entry:` header, empty `extension` blocks
  (`Trees`, `TypeName`).
- Imports naming deleted symbols (about 40), removed by a script from the compiler's errors, then
  `scalafix RemoveUnused`.
- `-Wunused` then found what the deletion left unread: givens of a scalameta `Dialect` (4), a
  private case class and enum of the ascription (`Untyped`, `Site`), and the `AnalysisWork`,
  `Library` and `Scope` fields. These went with the second, corrected pass.
- sbt plugin: the cached `cardinalityDependencyClasspath` still needs
  `sbt.util.CacheImplicits.given`, which the stub task had seemed to be the only user of.

Blockers other than parity (decided 2026-11-13 as D7 to D11; D7 and D8 are done behind the
switch, `39009dd`):

1. Selected reports (`cardinalityReportSelected`, `Report.query`, `AnalysisQuery`, `SourceSnapshot`,
   the budgets; scripted `selected`) have no TASTy path. The owner decides: port them (filter
   `Report.of` rows by name; the budgets have no counterpart) or drop them.
2. `cardinalityReportOf` and the scripted `library` test measure a sources jar without classes.
   TASTy-only, they can only refuse. Drop them, or read the library's binary jar, which holds its
   TASTy.
3. The scripted `dependencies` test checks stub-specific behavior (`checkUnsupplied` with
   `cardinalityDependencyClasspath := Nil`) and needs a new expectation.
4. 54 spec examples sit on fixtures scalac rejects. The plan sends them with uncompiled support.
5. The ledger and gate harnesses (`Compiled` `filled` mode, `Differential`) read the old front
   end. Rewrite them at the switch, or keep the old front end until parity is certified.

## Switch-over: pending examples (D11)

The 54 examples that fail under the TASTy front end because scalac rejects their fixture (spec
ledger at `b977b40`, `no compiled classes: scalac rejects this fixture`). At the switch they move
here, marked pending, and come back with uncompiled support:

- `AliasOverrideIdentitySpec`: guards incorrect alias arity
- `AliasOverrideIdentitySpec`: guards mutually recursive aliases
- `AliasOverrideIdentitySpec`: guards nearer imports shadowing an outer binder
- `AliasOverrideIdentitySpec`: guards opaque aliases
- `AliasOverrideIdentitySpec`: guards recursive alias expansion
- `BinderBoundsSpec`: an invalid clause refuses a one-valued result too
- `BinderBoundsSpec`: bounds referring back through bounds alone are invalid
- `ContextBindersSpec`: a chain past the reading's budget or arity is refused
- `ContextualOverrideIdentitySpec`: does not guess identity across ordinary and contextual clauses
- `ContextualOverrideIdentitySpec`: preserves additional unsupported contextual modifiers
- `DefinitionInventorySpec`: a `summon` between two anonymous binders is refused
- `EnvironmentCompletenessSpec`: an export in a parent or beside a declaration is refused
- `EnvironmentCompletenessSpec`: cyclic, ambiguous and non-path exports are refused
- `ExistentialInputSpec`: capture one wildcard before substituting an alias
- `GeneralInfixApplicationSpec`: enforces the same source constructor arity
- `GroundEnvironmentSpec`: a name no builtin spells keeps lookup's own reason
- `GroundTypesSpec`: a function type's repeated parameter is a sequence (no Scala)
- `HigherKindedRefusalSpec`: a builtin applied at a wrong arity says so in both readings
- `HigherKindedRefusalSpec`: a constructor argument of another kind is not counted
- `HigherKindedRefusalSpec`: a higher-kinded name or a lambda where a type is expected
- `HigherKindedRefusalSpec`: a higher-kinded name read as a type
- `HigherKindedRefusalSpec`: a kind mismatch is not counted
- `HigherKindedRefusalSpec`: a view bound is not Scala 3
- `HigherKindedRefusalSpec`: a wildcard under a rigid binder is not counted
- `HigherKindedRefusalSpec`: a wrong arity is not counted
- `InferredResultsSpec`: a parent not supplied refuses the source's untyped members
- `InferredResultsSpec`: a source that does not compile refuses its untyped members
- `InlineScopesSpec`: a local class has no companion to select from
- `MethodAnalysisSpec`: stop type-resolution cycles with an unresolved diagnostic
- `MethodFeatureIntegrationSpec`: reject private singleton leaks through refined members
- `MethodFeatureIntegrationSpec`: validate private singleton aliases inside repeated types
- `MethodRefinementSpec`: do not resolve shadowed type binders through source aliases
- `MethodRefinementSpec`: reject hidden member equalities
- `MethodRefinementSpec`: reject recursive member equations
- `MethodTypePlaceholderSpec`: rejects placeholder application with the wrong arity
- `NamespaceSpec`: a path from a package no clause opens names nothing
- `NamespaceSpec`: a qualified miss is the reference as written
- `NamespaceSpec`: a single package clause opens only its own package
- `NamespaceSpec`: a type is not a namespace
- `NamespaceSpec`: two wildcard imports that bind a name leave it ambiguous
- `OverrideIdentityImplementationSpec`: does not drop declarations at matching offsets in other files
- `OwnersAndRecordsSpec`: a `deferred` nothing imports is no compiletime one
- `PolyLambdaCardinalitySpec`: refuses unapplied lambdas and wrong arity
- `PureCodeSpec`: a ??? an unsupplied import may rebind is not typed
- `PureCodeSpec`: a var is no stable path, by Scala's typing rule
- `ReportSpec`: a name two packages define stays unresolved
- `SingletonIntersectionMethodSpec`: refuse a `Singleton` bound by an unsupplied import
- `SingletonIntersectionMethodSpec`: reject a var's singleton
- `SingletonIntersectionMethodSpec`: reject inaccessible paths and alias leaks
- `StdlibModelSpec`: a gap inside the supplied sources stays unresolved
- `StdlibModelSpec`: a root-anchored simple name is a top-level declaration
- `StdlibModelSpec`: an inaccessible source module's singleton stays refused
- `StdlibModelSpec`: an unapplied entry where a proper type is expected is refused
- `ThrowableSlotsSpec`: a throwable the scala package does not alias is not one there

## Order, and what is not known yet

4a → 4b → 4c → 5a → 5b → 5c → 5d → 5e → 6. Every step: the differential report, core's suite, the
plugins' tests, CPD, the dead-code scan.

- **Size of the result.** The front end should shrink a lot (most of resolution is name lookup the
  compiler already did): about a third of today's 30,000 lines, estimated under "Front end: design
  and status".
- **Rules worded in syntax.** Some contract rules speak of what is "written" or "spelled" (rule T's
  matching, the simple-name closed world, D43's candidates, the builtin-shadowing rule). Each is
  restated over symbols when its slice lands, in `v1.md` and `AGENTS.md`.
- **tasty-query coverage.** It already reads Scala 3 TASTy, Scala 2 pickles and Java classes for
  the stubs; typed trees of bodies (5d) are new use.
- **Performance.** Measured only as far as the first slice reads (same section). No stub rendering
  or parsing should help; reading typed trees for every body the contract reads may cost.

## Not worth reading from TASTy

The analyzer's own model, not a compiler fact: `StuckProjections` (the `proj:` atom),
`LiteralTypes` (already exact), the shape logic in `SingletonIntersections`, `TypeMembers` and
`RefinedMembers` (path-keyed atoms), `Representations`, `Callees` (type-level dispatch), and the
contextual evidence for hypothetical bodies.

Finality never comes from TASTy flags. The closed world's `final`s exist only in the analyzer.

## Front end: design and status

Written 2026-11-11, after phase 4 and a first slice of phase 5. Read it with "Decisions" (D1 to
D4), the phase 5 table and phase 6 above, `AGENTS.md` and `v1.md` §2.

### Integration (`tasty-int`)

`tasty-int` is the gate agent's `6ed79bc` with six group branches merged, one merge commit each,
each certified by `sbt core/testFull` (104 specs), scalafmt and scalafix, the file metrics, CPD and
`scripts/differential.sh` on all seven inputs: `tasty-front` (`2d3fa6e`: hidden captures, throwing
constructions D5), `tasty-g5` (`06a180e`), `tasty-g4` (`ff8dc65`), `tasty-g6` (`deb9365`),
`tasty-g2` (`ea43080`), `tasty-g3` (`75c947a`). Each branch's own section below says what it reads.
What the merges decided:

- Hidden captures: one implementation, `Readings.local` (g5's scopes: function literals, case
  clauses, class-body statements) with `Readings.kept` (tasty-front's holders, read only where the
  definition can name them; a method the definition is in holds; a `throw` a `try` may catch). A
  self alias hides a parameter (g5's correction `Self.mk.<new#1>.run` 16 -> 4). A function literal
  the definition is in, and an expression statement, hold nothing.
- g2's "selected on `this` alone" rule lives in `Observers.observed` beside g4's observers.
- Fixed in the merges, each with a regression, each a wrong count refused now: a record of a class
  whose only overriding subclass is never built (`RecordsSpec` `dorm`); a receiver's anonymous
  evidence beside a class-body statement that may declare a given (`ContextualSpec` `Blk`); in the
  re-entry rule, an object that inherits the measured member, a `new` of the measured class read
  as re-entering its constructor row, a `new w.C` on another outer instance, and a call of the
  measured method with other arguments than its own parameters (`ProgramRowsSpec`
  `definitePositions`). The last is the base's rule too: 24 rows identical to the old front end
  (which reads them the same way) are uncovered now: cats-core `Bifunctor.bimap`,
  `Profunctor.dimap` (0), cats-eo `MultiFocusFromList.fromList` (0), 21 fixture rows of
  `Reentry.scala` and `Views.scala`; the corrections of `U.helper` and `S2.<extension@5>.later`
  went with them.

Method rows on the merged tree (identical / better / worse / uncovered), and old numeric rows
still uncovered, methods (both tables); every worse row is a listed correction:

| input | identical | better | worse | uncovered | old numeric uncovered |
|---|---|---|---|---|---|
| core | 2,880 | 15 | 0 | 1,105 | 8 (8) |
| cats-core | 1,846 | 369 | 72 | 5,364 | 28 (31) |
| cats-kernel | 521 | 15 | 0 | 1,249 | 78 (78) |
| cats-eo | 313 | 12 | 0 | 372 | 12 (15) |
| cats-eo-generics | 40 | 0 | 0 | 40 | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) |
| fixtures | 5,481 | 252 | 3 | 1,017 | 399 (445) |

The integration's private coverage memory dropped the rows above and `Outer.Inner.<init>` (g2's
own note). `tasty-g1` came as `tasty-g1b`, merged in round 3 below.

Round 2 merged, one merge commit each, each certified by the same gate: `tasty-h5` (`4d375f9`),
`tasty-h2` (`e414333`), `tasty-h4` (`5994049`), `tasty-h3` (`20fd65b`), `tasty-h1` (`cb0e41c`),
`tasty-h6` (`497b671`), `tasty-h7` (`0b9ccae`). What the merges decided or fixed, each fix with a
regression:

- Rule B's observed fields (`Factories.observed`) follow a member's override in the class, members
  of classes and objects declared in it, and public members a module parent declares that the
  class inherits as they are; a class passing a constructor parameter to a parent's constructor
  is refused on both sides (`Inh2`, `Nest`, `ToPar`, `UseInh2.f`: wrong numbers before).
- Constructors: h2's skip of the instances around applies where the primary constructor has no
  parameters (`OB.In`), never to a secondary that may pass the instance's fields (`OA.In()` 1);
  a secondary constructor's row takes the bounds' proofs (`Bounds.count`, shared with method rows;
  `Low`, `OL.In`); its anonymous evidence is compared by its own parameters' types (`Sec`: was 256,
  ambiguous); a secondary constructor of a class written in a body is not read yet.
- Captures: `Readings.captured` is shared by method and constructor rows and keeps h4's evidence
  search; wildcard imports are read (the selector filter dropped every wildcard), a selector binds
  a term only where its qualifier has one, a wildcard inside its own object binds private members.
- h7's reachable readers outside the class join the observed fields beside the above; a reader is
  reached through a public member it implements too (`Ko`: 2^4, both front ends said 2^2); h6's
  enum built from nothing needs its case's templates built (`Rose`, a throwing companion: was ω,
  now `?`; the empty sum's 1 is not read). `fix.Comp` stores 4 in both front ends now.
- One reading of a member bounded by one type on both sides (`PathTypes.pinned` through
  `PathTypes.exported`); one one-valued capability rule (h2's). The anonymous-evidence search moved
  from `Readings` to `Evidence.scala`.
- `old-unsound.txt`: the old numeric rows whose number rests on the rule that a member calling the
  measured one with other arguments, or a local definition in a method with parameters, always
  re-enters (found by switching the two `Reentry.again` checks and the `new w.C` check off: 35
  fixture rows and cats-kernel `Eq.eqv`, `PartialOrder.partialCompare`; the `new w.C` check alone
  frees none). `Differential --unsound` prints, per table, the old numeric rows still uncovered
  that it does not list.

Method rows after round 2 (identical / better / worse / uncovered), old numeric rows still
uncovered, methods (both tables), and past the old-unsound list:

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,138 | 373 | 72 | 5,068 | 25 (26) | 25 (26) |
| cats-kernel | 523 | 15 | 0 | 1,247 | 76 (76) | 74 (74) |
| cats-eo | 327 | 12 | 0 | 358 | 3 (4) | 3 (4) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,719 | 269 | 5 | 759 | 171 (190) | 136 (155) |

Round 3 merged `tasty-g1b` (`cb61054`, re-entry through another instance on a capability record's
value, the owner's ruling D6), certified by the same gate. Conflicts only in `derived.txt` and this
plan (both sides added; the union, no duplicate key). `Reentry.scala` auto-merged with h2's `again`
and h7's never-built overriders; its definite view moved verbatim to `Definite.scala` (no behavior
change) to keep the file under 1,000 lines. The correctness review of the merge found two ways a
same-instance re-entry was counted on the record path, both g1b's, each fixed with a regression in
`ProgramRowsSpec.sameInstanceHidden`:

- The value given through code outside the module to a function literal (a partial function's
  included) whose body wraps it: `Some[E](this).map(e => new Wrap(e)).get.go`, `List(this).map(new
  Wrap(_))`, `for`, `collect`, `Cap` named: `Use.same` was 2^256. A literal's parameter now holds
  what the call's receiver and other arguments hold (`Reentry.bound`, read by `holder` and
  `madeOf`).
- A site without value parameters whose result's implementation is given the value: `def all: E =>
  Boolean` (or `E ?=> Boolean`) with `Cap.neqv = Use.all(Cap)`, through a member of `Cap`, or a
  module-built `Reg.x`: `Use.all` was 2^256. `handsMade`'s narrowing to "no input" now counts as
  inputs what an abstract member of the result's class is given (`Reentry.opens`): a parameter that
  is no primitive, `String` or `Unit`, and no type parameter without a bound (a parametric body
  cannot use one, so `Eq.allEqual[A]: Eq[A]` stays counted); `Use.all: E`, whose members take
  Booleans, stays 2^8.

A first fix of the first finding (asking `dispatched` of any member selected on a term holding the
value) cost about 110 cats-kernel rows and was dropped. Coverage memory: `Direct.traverse.traverse`
(cats-eo) went from a measured `?` to uncovered (g1b's record path refuses `Invariant[G]`'s
`composeFunctor`, whose overriders outside the module are not read); both print `?`, old not
numeric; its line was deleted.

Method rows after round 3 (identical / better / worse / uncovered), old numeric rows still
uncovered, methods (both tables), and past the old-unsound list; every worse row is a listed
correction, as before:

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,138 | 373 | 72 | 5,068 | 25 (26) | 25 (26) |
| cats-kernel | 635 | 19 | 0 | 1,131 | 28 (28) | 26 (26) |
| cats-eo | 326 | 12 | 0 | 359 | 3 (4) | 3 (4) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,721 | 269 | 5 | 757 | 169 (188) | 134 (153) |

Gate times (as the gate reports them; the machine was shared, so noisy): cats-kernel 12.3 s
(round 2) / 19.1 s (round 2's tree, this session) -> 15.5 s; cats-core 14.3 s / 22.8 s -> 14.7 s.
`compiled/` is 8,849 lines (8,339 before).

Then `tasty-k3` (`bfc1fa3`), merged without conflicts (k3 changes no list file). Its review found
three wrong numbers, each refused now with a regression:

- A class read in place over a system's values (`World.strict`) at a type argument holding a
  function, context function or tuple of one: `LzB(b: Box[() => LzB])`, `Nb(b: Box[Boolean =>
  Nb])`, `Lc(b: Box[Boolean ?=> Lc])` stored 0 (the ν completion sees a lazy field only at its
  top: `LzB` is `Lz`, 1, `Nb` is ω). `Stored.declared` now reads in place only where no type
  argument is deferred (`World.deferred`, at any depth); else `variable` refuses
  (`StoredRecursionSpec.refusals`). Left, in both front ends and before k3: `Ltd(p: (Boolean, () =>
  Ltd))` stores 0 (a function inside a tuple field).
- A match whose scrutinee scalac finds uninhabited and leaves stuck: `Snd[(Nothing, Boolean)]`,
  `(A, Boolean)` with `A <: Nothing`, `(p.X, Boolean)` with `p.X <: Nothing`, and k3's nested
  `((Nothing, Int), Boolean)`: rows 4, 16, 2 that no program has. `PathTypes.trusted` refuses a
  scrutinee below `Nothing` or with a type argument at any depth below it (`empty`;
  `MatchTypesSpec.later`). The flat cases were wrong on the base too.

Method rows after k3 (identical / better / worse / uncovered), old numeric rows still uncovered,
methods (both tables), and past the old-unsound list; only fixtures and cats-eo's definition table
change (cats-eo 3 (4) -> 3 (3)):

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,138 | 373 | 72 | 5,068 | 25 (26) | 25 (26) |
| cats-kernel | 635 | 19 | 0 | 1,131 | 28 (28) | 26 (26) |
| cats-eo | 326 | 12 | 0 | 359 | 3 (3) | 3 (3) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,743 | 270 | 5 | 734 | 147 (160) | 112 (125) |

Gate times: cats-kernel 13.0 s, cats-core 10.2 s. `compiled/` is 8,997 lines. Next, in the order
handed over: `tasty-k2`, `tasty-k1`, `tasty-k4`.

Then `tasty-k2` (`4c7a503`). Conflicts: `Shapes.member` (k3's deferred-inline structure kept, its
`read` calling k2's `Capabilities.method`), the file-count pin (24), `derived.txt` and this plan
(the union). The review confirmed the 15 derivations (`Functor.imap` is overridden in
`package.scala`, `WriterT`, `OptionT`, `Nested`; every other member of each comonad has a body, so
ω; `Comonad.extract` has no capability returning a bare A, 0) and found one wrong number, older
than k2 but widened by it to inherited members: a member's own constructor binder applied only in
a parameter (`m[G[_]](g: G[Unit]): H[Int]`) was read as an unfillable parameter, so the member was
dead and `r.m[Option](None)` unseen: 0 where a body exists (1). `PolymorphicMembers.close` (shared
by both front ends) now refuses it, "a parameter instantiates the binder" (`RecordsSpec`
`polymorphic`: `hkInh`, `hkDecl`, `ngInh`). No corpus row moved with the fix.

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,179 | 388 | 72 | 5,012 | 22 (23) | 22 (23) |
| cats-kernel | 635 | 19 | 0 | 1,131 | 28 (28) | 26 (26) |
| cats-eo | 327 | 12 | 0 | 358 | 2 (2) | 2 (2) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,750 | 270 | 5 | 727 | 140 (153) | 105 (118) |

Gate times: cats-kernel 14.7 s, cats-core 10.3 s. `compiled/` is 9,023 lines.

Then `tasty-k1` (`1a10c0b`). The coordinator's ruling on k1's closed-world decision is recorded in
"Group k1": it stands, since by AGENTS.md the supplied sources and dependency declarations are the
whole program, so a class in a jar not supplied does not exist and cannot extend a program class.
Conflicts and resolutions:

- `Shapes.Env` moved to `ShapeEnv.scala` (k1) and gained k3's `held`: `under` joins both sites'
  instances, `within` (a written lambda's body read at the site that wrote it) takes the writing
  site's, as it takes its binders.
- One stored-side match reading over a class's parameter: k3's (outside a system only, refused
  where a symbol is bound differently at both sites, family cases through `Sums.carried`); k1's
  `apart` is dropped, k3's check being at least as strict wherever it applies. k1's `atClass` with
  the generic `Shapes.copied` is kept. k1's regressions (`MatchTypesSpec.storedValues`) pass.
- `Reentry.nameOf` (HEAD) and k1's `Reentry.written` were one function: `written` kept.
- `Stored.scala` (996 lines) split, no behavior change: the size algebra (`standard`, `product`,
  `sum`, `arrow`, `linear`, `lifted` and the library tables) is `Algebra.scala` (914 + 94 lines).

Three cats-core rows the merge measures that neither branch did (k2's inherited polymorphic
capabilities over k1's now-known dependency members), each derived in `derived.txt`:
`Invariant.catsContravariantMonoidalForEq` 2^131586 (unit 2^2, contramap 2^512, product 2^(2^17)),
`NestedContravariantMonoidal.FG` 0 and `OpCategory.Arr` 0 (every member that could build the
needed value re-enters). The review found one wrong number, k1's: `ClosedProgram.absent` looked up
only the text before a binary name's first `$`, so a supplied top-level class named `Wid$Impl` (or
a symbolic one, `$plus$plus`) read as outside the program, its anonymous subclass dropped and its
members final (`f` `No` where `Wid$Impl$1` overrides it). `absent` now needs the whole name,
encoded and decoded, missing too, and `named` resolves such a class (`ClosedProgramSpec.java`,
`Wid$Impl.java`: `Supplied`). No corpus row moved with the fix.

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,809 | 393 | 72 | 4,377 | 18 (19) | 16 (16) |
| cats-kernel | 639 | 19 | 0 | 1,127 | 28 (28) | 26 (26) |
| cats-eo | 329 | 12 | 0 | 356 | 2 (2) | 2 (2) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,761 | 275 | 5 | 711 | 129 (142) | 94 (107) |

Gate times: cats-kernel 14.5 s, cats-core 16.8 s. `compiled/` is 9,142 lines.

Then `tasty-k4` (`8e8f28c`). Conflicts: `Receivers.reachesTwice`, its reason and its regression;
`ReceiversSpec`'s reasons; the three list files and this plan (the union, no duplicate key). One
rule for a parent reached twice, k1's: the base types through each parent are compared, the same
one is one parent. It subsumes k4's (a parent without type parameters has one base type), and
reads k4's `GD[X] extends GB[X] with GC[X]` (both `GA[X]`): `GD.g: X = a` is 1, the one body;
`CS` (`CP[Boolean]` beside `CP[Any]`) stays refused. k4's `Deferred.m` expectation (refused) gives
way to k3's deferred inline rule (2). `World.mentioned` is the one walk every "does it mention"
caller uses (`World.mentions`: `Sums`, `Readings.singleGoal`, `Constructors`; `Inlines` directly).
The review tried again to break the polarity argument (aliases, paths, refinements, lambdas, match
types, by-name and function fields, opaque types, bounds, two slots at one parameter, private
fields, secondary constructors, nested sealed levels, rule B) and `singleGoal`, and found no wrong
number. Left, recorded: `World.mentioned` does not walk a `RecType` (no reader counts one through
a variant slot today), and `World.deferred` (k3's `strict`) does not see a function behind a match
type or a wildcard bound, as the variable path's ν completion does not either.

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,809 | 393 | 72 | 4,377 | 18 (19) | 16 (16) |
| cats-kernel | 639 | 19 | 0 | 1,127 | 28 (28) | 26 (26) |
| cats-eo | 329 | 12 | 0 | 356 | 2 (2) | 2 (2) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,773 | 285 | 5 | 688 | 116 (124) | 57 (65) |

Every worse row is a listed correction. Gate times: cats-kernel 14.7 s, cats-core 17.5 s.
`compiled/` is 9,270 lines. Round 3 is merged.

Round 4, one merge commit each, each certified by the same gate. First `tasty-m1` (`f2098e7`),
without conflicts (m1 started from `a9ffbc2`). The review found one wrong number, m1's: `inert`
read only the static class's own parents, so a value typed by a member-less record trait `E` was
inert though a supplied subclass mixing in an outside parent with members may be what it holds,
and outside code given it may test for that parent (`I extends E with Iterable[Boolean]` whose
`neqv` gives `(this: E)`, a `val me: E`, an `A <: E` or `Some[E](me)` to `ScalaRunTime.stringOf`,
which runs `I.iterator` on the same instance: `Use.same` 2^256 where a9ffbc2 refused). `inertClass`
now asks it of the class and every supplied class below it (`World.below`; unknown subclasses are
not inert), pinned in `sameInstanceHidden`. No corpus row moved with the fix. `Reentry.scala` is 998
lines.

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,885 | 19 | 0 | 1,096 | 3 (3) | 3 (3) |
| cats-core | 2,809 | 393 | 72 | 4,377 | 18 (19) | 16 (16) |
| cats-kernel | 664 | 23 | 0 | 1,098 | 4 (4) | 2 (2) |
| cats-eo | 329 | 12 | 0 | 356 | 2 (2) | 2 (2) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,773 | 285 | 5 | 688 | 116 (124) | 57 (65) |

Gate times: cats-kernel 18.0 s, cats-core 19.6 s. `compiled/` is 9,347 lines.

Then `tasty-m2` (`aafec99`), a plan section only: its conflict with m1's section was both adding
beside each other (both kept). No source changed, so the gate's rows are m1's above; m1's `inert`
moves none of the 19 rows m2 analysed (m2's table rows outside cats-kernel are unchanged by m1).

Then `tasty-m4` (`448a517`). Conflict only in `derived.txt` (both sides appended; the union, no
duplicate key in any list file). The review found one wrong number, m4's: a plain case class or a
types-only class nested in a class was read without asking whether the class around it has any
instance, so `Dead#Leaf` (`Dead`'s initializer throws), `Dead2#Leaf` (through its parent `Boom`),
`FA#Leaf` (`final abstract`) and `Priv#Leaf` (no constructor or factory the site may call) gave 4
(or 2 for a `Mark`) where the input is empty (1) or not determined (`?`); `451e7f1` refused them.
`Shapes.declared` now refuses such a class where a class around it is never built, `final
abstract`, or final with nothing the site may call to build it (`ReceiversSpec.inner`, `UseDead`).
No corpus row moved with the fix. The integration's coverage memory dropped the nine core `Memo`
vals m4 derives (measured `?` before, 1 now). Left, as the review noted: the stored side reads
`Dead#Leaf` 2 and `Dead` 1 on both parents, not the empty sum.

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,889 | 29 | 0 | 1,082 | 2 (2) | 2 (2) |
| cats-core | 2,809 | 393 | 72 | 4,377 | 18 (19) | 16 (16) |
| cats-kernel | 664 | 23 | 0 | 1,098 | 4 (4) | 2 (2) |
| cats-eo | 329 | 12 | 0 | 356 | 2 (2) | 2 (2) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,784 | 287 | 6 | 674 | 106 (114) | 39 (41) |

Every worse row is a listed correction (m4's `Outer.Inner.Use.<init>` 1 -> 2 the sixth). Gate
times: cats-kernel 23.1 s, cats-core 31.5 s (shared machine). `compiled/` is 9,391 lines.

Round 5, one merge commit each, each certified by the same gate. First `tasty-m3` (`a415439`).
Conflicts: `World.mentioned`'s comment (both sides' additions kept) and `corrected.txt` (the
union). The review found four wrong numbers, each fixed with a regression:

- m3's rule-B constructor rows dropped an unobserved field with its emptiness: `A(h: Boolean, b:
  => B)` with `B(x: Nothing, a: => A)` read 2, and the strict pair `C(h: Boolean, d: D)`, `D(x:
  Int, c: C)` 2 and 2^32, where no value exists (0). An empty estimate of the whole class now
  stands (`StoredRecursionSpec.hiddenPositions`: `Hm`, `Hk`, `Hc`, `Hd` 0).
- The method side reports a position's first refusal only, so a recursive (or hidden) position
  took the estimate though a part beside the cycle was refused for another reason:
  `A(h: Boolean, p: (B, Outer#In))` with `Outer` never built read 4, the same tuple swapped `?`.
  `Constructors.position` now asks the method side of every part the cycle's refusal covers
  (`beside`; `Hp`, `Hp2` refused). Older than m3 on the `Recursive` path, widened by m3's.
- An opaque alias read inside its scope (`opaque type Th = () => Q`, `Q(b: Boolean, t: Th)`) was
  a strict edge, so `Q` stored 0 (old front end ω); under a class argument (`Bx[Q]` with `Bx[A](a:
  Th[A])`) `Bx` was read in place, 0 in both. `Stored.mentions` blocks the alias (no number: the
  completion does not take it apart) and `World.deferred` takes an opaque alias as deferred
  (`uncompleted`: `Op.Q`, `Op2.Q` refused). Older than m3.
- Old front end: an alias under a function's result or a by-name type (`type T = (Boolean, X)`,
  `X(f: () => T)`) was a continuation to the alias, no node, so the cycle was lost: 0. `Solver`
  now expands the aliases there, and a continuation targets only a definition with constructors
  (`CardinalitySpec`: ω).

The fixture cache was rebuilt (a stale fixture of `NamespaceSpec` from before m3 paired `O.Cell`
twice); the coverage memory dropped `H9` and `H9.<init>` (0 -> ω, m3's). Every worse row is a
listed correction (m3's five added).

| input | identical | better | worse | uncovered | old numeric uncovered | past old-unsound |
|---|---|---|---|---|---|---|
| core | 2,889 | 29 | 0 | 1,082 | 2 (2) | 2 (2) |
| cats-core | 2,809 | 393 | 72 | 4,377 | 18 (19) | 16 (16) |
| cats-kernel | 664 | 23 | 0 | 1,098 | 4 (4) | 2 (2) |
| cats-eo | 329 | 12 | 0 | 356 | 2 (2) | 2 (2) |
| cats-eo-generics | 42 | 0 | 0 | 38 | 0 (0) | 0 (0) |
| cats-eo-schemes | 5 | 0 | 0 | 14 | 0 (0) | 0 (0) |
| fixtures | 5,809 | 287 | 11 | 659 | 96 (104) | 30 (32) |

Gate times: cats-kernel 15.1 s, cats-core 26.4 s. `compiled/` is 9,498 lines.

Then `tasty-m6` (`9be2bf6`, with m5's `a0b8144`), merged without conflicts. Coordinator ruling,
recorded here: a path through the universal members of `Any` (`toString`, `equals`, `hashCode`:
`s"$this"`, `Set(this, o)`) is outside the model (AGENTS.md excludes the universal members and
unrestricted equality tests), so a count it would change is no wrong number and is not chased. The
review found three ways `Matches` missed a call the compiler inserts, each a same-instance
re-entry counted (`Use.same` 2^256 where it is refused), each fixed with a regression in
`sameInstanceHidden`:

- A private `isEmpty`, `get` or `_1` was skipped, but a match written in its class selects it: the
  private filter is dropped (a private member elsewhere is only read, more conservatively).
- An extractor's result typed `N & Serializable` names no single class (the written `&` is an
  application of the alias `scala.&`), so no `isEmpty`/`get` was inserted: `Matches` now reads
  each side of an intersection or union, and an abstract type's upper bound.
- A pattern variable was bound to the scrutinee only: `a match { case Me(n) => Use.same(n) }`
  with `Me.unapply` returning `Some(N.this)`. A variable is now bound also to what its pattern is
  matched against (`Matches.boundTo`, read by `Reentry.bound`).

`Reentry.scala` went past 1,000 lines: the record-carrier predicates (`opens`, `carrier`) moved
verbatim to `Openings.scala` (no behavior change). No row moved; the table is m3's above. Gate
times: cats-kernel 15.7 s, cats-core 19.2 s. `compiled/` is 9,719 lines.

### What exists

| Piece | Where | What it is |
| --- | --- | --- |
| Boundary | `reporting/FrontEnd.scala` | `FrontEnd.rows(Module): Rows`. In: each source's path and text, and the `Report.Environment`. Out: one answer per source in order, its `Entry` and `Definition` rows or a `Report.Error`. |
| Old front end | `reporting/ScalametaFrontEnd.scala` | The pipeline `Report.of` had inline, moved unchanged; the only one `Report.of` calls. `Report.query` calls its steps directly. |
| New front end | `compiled/TastyFrontEnd.scala`, `Targets.scala`, `Readings.scala`, `Shapes.scala`, `Sums.scala`, `Stored.scala`, `Constructors.scala`, `Bodies.scala`, `World.scala`, `Source.scala` (2,157 lines) | Reads the compiled classes through tasty-query. |
| Gate | `core/src/test/scala/cardinality/compiled/Differential.scala` | See "The gate". |
| Regression | `TastyFrontEndSpec` (16 examples), `DifferentialSpec` (8) | Derived counts for each slice, the reason of each row a slice refuses or leaves, the gate's verdicts, row names for what the compiler desugars, and the front end's imports against an allow-list (`allowed`). |

The old front end's files changed in two places only, both extractions: `CompiledTypes.loaded` (the
class loading `read` had inline) and `Definition.qualified` (the name joiner `Walk` had).

### The first slice, and where the next ones go

`Targets.of` finds and names every row. `Readings.method` and `Readings.definition` measure one:
each answers a count or size, or `Left(what the row needs)`. A slice replaces `Left`s (and the
`Unread.Uncovered` a type reading gives) with readings. What is read today:

- **Method rows.** A method or value that is not inline, contextual, an extension or a constructor;
  whose owners up to the package are objects with no parent but `Object`, `Product`, `Equals` or
  `Serializable`, and without a pattern or multi-name value definition, named or not; whose type
  parameters are
  unbounded; over those parameters and the types `Shapes` reads (slice S1, below). Its
  environment is its own parameters. A member of an object that is never built counts 1 (register
  D26, `docs/research/code-cardinality-foundations.md`); a result with one value counts 1.
- **Definition rows.** An object; an enum without parameters whose cases are all values; a concrete
  class without type parameters, without parents but the four above, without a self type, with a
  public primary constructor, whose fields are base types (`Readings.Bases`).

### Architecture, and why

- **One boundary, two whole implementations, no hybrid.** A row no slice reads yet is `?` with the
  reason `not read from compiled classes yet: <what it needs>`. So a row the gate calls identical is
  one the new front end measured: a front end that fell back row by row would make the gate unable
  to fail.
- **Three answers for a method row.** A count; a refusal of the counting model (a real `?`, which
  the gate compares: an `Array` of a type parameter); or uncovered, exempt until its slice lands. A
  definition row has a size or is uncovered. A reading's own exception is not filed as uncovered: it
  fails the source, visibly. The one exception read as uncovered is a member the classpath does not
  hold (`MemberNotFoundException`).
- **Rows first, then measurement.** Row identity (4c) is tested on every row from the first slice
  on, not only on the measured ones.
- **The back end is called as it is.** A shape comes from `BuiltinTypes.known` (asked by the class
  symbol's fully qualified name), a count from `Inhabitation.count`, a size from `Size` and
  `BaseTypes`. `Readings` also calls `TopTypes.polarized` and `terminalGoal`, which work on shapes
  but live in a file that imports scalameta: the shape half of `TopTypes` stays with the back end.
- **The source text names and checks; no count reads it.** It gives a row's signature, the line of
  an `extension`, whether a given is anonymous, whether a trait is written with a parameter clause
  (its `<init>` row), whether an alias is written `type F[X] = …`, and which class is a `@main`
  method's. It is also what the stale-classes check reads.

### Row identity (4c)

A row's name is its owners' path, read from the tree walk: a body's scopes have no symbol.

| Segment | From |
| --- | --- |
| package, class, object, method, value | the symbol's name; an object's class is named after the object; the holder of top-level definitions (`X$package`) and a package object add no segment |
| `<init>` | a class's row measures its primary constructor; a secondary constructor is a `<init>` method row, its body's definitions under `<init#k>` |
| `<given@line>` | a given whose name the source does not spell where the tree places it |
| `<extension@line>` | the last `extension` keyword before the method's first parameter |
| `<block#n>`, `<new#n>`, `<lambda#n>`, `<case#n>` | a block, an anonymous class, a function literal, a case clause that holds a definition; n counts the scopes of that kind that hold one, under the same path |

Not rows: a member with the `Synthetic` flag (a function literal's method among them), a name no
source spells (`f$default$1`), a setter, an export forwarder, the method beside a given or implicit
class (it shares the class's name position), an enum's value case, a constructor parameter, a
`@main` method's class.

What the compiler desugars and the walk undoes: an inline method's body is ascribed with its type;
the body of a method whose result is a context function sits in a function literal of the
compiler's; `{ case … }` is a match on a parameter the compiler made.

What it does not undo:

- These rows pair by position, under another name. A `for` comprehension is function literals in
  the tree: `<for#1>.<block#1>` reads `<lambda#1>` or `<lambda#1>.<case#1>` (3 fixture rows). A
  function literal with an untupled parameter has one more block (2 corpus rows); a polymorphic one,
  one lambda fewer (1 fixture row).
- These would be unpaired, and neither input holds one. A definition inside an argument of a
  `transparent inline` call is not found (the walk skips an `Inlined` tree). A top-level pattern
  definition (`val (c, d) = …`) is a definition row per name; the old row is one, named by the
  printed pattern with the ascriptions rule T wrote into it.
- The source text is read without a parser: `type F [X] = …`, with a space before the bracket,
  reads as a constructor alias; a parameter clause on the line after a trait's name is not seen
  (no `<init>` row); a context function literal written with a wildcard parameter
  (`(_: Ctx) ?=> …`) reads as the compiler's and loses its `<lambda#n>`.

### The gate

```bash
sbt 'core/testFull'                                # fills core/target/compiled-fixtures
scripts/differential.sh <assets> <output directory> [input ...]
```

- `<assets>` holds the frozen corpora (`corpus-*`, "Gate assets" above; here
  `/var/cache/users/rhansen/cardinality-tasty`). The script runs `cardinality.compiled.Differential`
  once per corpus and once over the compiled fixtures (`bench/runMain`, `-Xmx16g -Xss8m`), writes
  each run to `<output directory>/<input>.txt`, prints the totals and exits 1 when a run fails.
  Naming inputs (`corpus-core`, `fixtures`) runs only those.
- `<assets>/old/<input>-<hash>.bin` keeps the scalameta rows of a corpus between runs (`--old`): the
  six take about four minutes to compute. The hash is of the checkout's `build.sbt` and `project/` files, core's main
  sources but the new front end's, and the gate's own source, so a change to any computes them
  again. Nothing removes the files of earlier hashes.
- `<assets>/covered/<input>.txt` holds the rows the new front end measured at the last passing run,
  each with its count (`--covered`). A row listed there that is uncovered or counted otherwise now
  fails the run. A passing run rewrites the rows it saw and keeps the lines of rows it did not see
  (another checkout's fixtures, a run over part of the cache). To accept a changed count, delete
  its line. Where two rows share a key (two overloads on one line) the key is kept only when both
  are measured alike.
- `core/src/test/resources/compiled/derived.txt` lists the better rows past a model limit that
  were derived by hand (`--derived`): the row's key as the run prints it, ` | `, its count, `:`, and
  the rule that makes it countable. The row passes while it has that count. A fixture's key holds
  its digest, so editing the fixture asks for the derivation again.
- `core/src/test/resources/compiled/corrected.txt` lists the rows whose number changes because
  the old one is derivably wrong (`--corrected`): the row's key, ` | `, `old -> new` as the run
  prints both values, `:`, and the derivation. A worse row passes only while it has both values.
  A line for a source of the run that passes no row fails the run; a key listed twice is read
  once (the last line), which nothing reports. The plan's status section keeps the table of all
  corrections: the owner sees every number the rewrite changes.
- A row whose old value is an upper bound (`≤2^32`) and whose new value is that bound's exact
  number (`2^32`) is TIGHTENED: it passes, and the run prints it.
- `--rows`, given to the runner itself (`bench/runMain cardinality.compiled.Differential corpus
  <dir> --rows`), lists every row with the front end that produced it.
- Verdicts: 4b's, with identical also `?` on both sides, and uncovered, only old, only new.
- Pairing, strictest first: name, kind and line; then position (the name without `<kind#n>`
  segments, same kind and line); then name and line across kinds. A key pairs only when both sides
  have as many rows under it.
- A better row is read by why the old front end refused it (`Differential.resolution`, by the
  reason's category). **A resolution limit** is a limit of reading syntax: a declaration not
  supplied or a type not resolved, an import, export or wildcard that may bind a name (a builtin
  one included), an inferred type, a parent outside the sources or reached through a qualified
  name, an override or inherited member whose identity was not resolved, a path through a value, an
  object's construction that rests on what `???` names. A number is expected there; the run prints
  the count per reason and one row to derive as its sample. **A model limit** is every other reason
  (a recursive type, a capability record, a field not observed injectively, an open template, a
  self type, a receiver space, a sealed family's or an enum's construction, a `transparent inline`
  result, a refinement, a budget): the new front end counts what the model refuses, and the row fails until it
  is in `derived.txt`. A row with several reasons is past a model limit as soon as one of them is.
- Exit 1: a worse row; a row only one side has; a better row past a model limit that is not
  derived with its count; a row measured before that is uncovered or counted otherwise now; a
  source or class either front end could not read, the module's compiled types or its classpath
  where the old one could not; an input no source is read of. What the old front end says of one
  dependency declaration it left unsupplied is printed, not failed: it only refuses more there.
- Listed to be explained: every row paired by a looser key than the first.
- A fixture is every directory of the cache with an `ok` marker and no `.` in its name (a
  `<key>.<thread>` directory is a compile's staging left behind). Its sources are those under `src`
  but `src/withheld`, which with their classes in `support` are what the fixture compiled against
  and the analysis is not given. The old front end spells its compiled types against `support`, as
  the tests do; the new one is given the Scala library alone, so a type that names a withheld or
  otherwise missing class is `a member the classpath does not hold` where it is read. A row is
  still measured where no type is read: a goal of one value, a member of an object never built.

What it cannot see:

- A wrong number past a resolution limit that is not the reason's sample. Nothing records that a
  sample was derived.
- `?` on both sides is identical whatever the reasons.
- The coverage memory is by the new row's key: a row that is renamed (a scope, a kind, a line) and
  left uncovered in one change is not found.
- A declaration that really is withheld from a fixture reads, on the old side, like one the syntax
  could not resolve (`declaration not supplied`): a better row there is past a resolution limit.
  The two such rows today count 1 by rules that read no type (a goal of one value, an object never
  built).
- A fixture's stubs: a test that hands the old front end rendered stubs (`TastyStubsSpec`) is
  compared here without them.
- Signatures are only counted, and captures are not compared (the new front end lists none yet).

### Measured, first slice (2026-11-11, before the fixtures were recompiled)

| Input | Rows old / new | Paired by name, kind, line | By position | Across kinds | Identical | Better | Worse | Unpaired | Uncovered |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Corpus, methods | 4,000 / 4,000 | 3,998 | 2 | 0 | 227 | 0 | 0 | 0 | 3,773 |
| Corpus, definitions | 641 / 641 | 641 | 0 | 0 | 258 | 0 | 0 | 0 | 383 |
| Fixtures, methods | 5,068 / 5,068 | 5,061 | 4 | 3 | 553 | 12 | 0 | 0 | 4,503 |
| Fixtures, definitions | 3,509 / 3,509 | 3,509 | 0 | 0 | 1,370 | 3 | 0 | 0 | 2,136 |

- Nothing is unread and both runs exit 0.
- Old rows without a reason that are identical in the new front end: corpus methods 221 of 768,
  corpus definitions 258 of 400, fixture methods 523 of 4,269, fixture definitions 1,370 of 2,884
  (a definition row without a reason may be a `—`, an abstract type with no size). The rest are
  uncovered; none is worse.
- Measured by the new front end itself: 5.7% of the corpus's method rows and 40.2% of its definition
  rows; 11.1% and 39.1% of the fixtures'.
- The fixtures are the 1,292 that compiled, of 1,496 in the cache (204 did not compile).
- The 15 better rows, each derived by hand and checked against the compiled signatures. In 14 the
  compiler bound a name the syntax reading refused to follow: `???` where a wildcard import is in
  scope (5, each 1 by D26), an alias through an import or a value's path (5), a builtin name a
  package object's parent rebinds (1), the projection `Q#X` (1), an inherited member of an export
  (1), an import's own alias in a field (1). In 1 it reduced a type lambda applied in a result
  (`([x <: A] =>> x)[A]` is `A`).
- 3 rows pair across kinds: `given g: T = deferred` is a method in the old rows and a declaration in
  the tree, which holds no right-hand side.
- Signatures: 433 corpus rows are spelled differently, 425 of them inferred results, which the old
  rows print rooted at `_root_` and the new do not print.

Time and memory on the corpus, one JVM (`-Xmx16g -Xss8m`, JDK 25): the TASTy front end takes 3.1 s,
JVM start included, with 1.1 GiB of heap used; it runs in `-Xmx768m` and not in 512m (the corpus
run with `--old` already kept, under `time`, `-Xlog:gc` and those two limits). The scalameta
front end takes 98 s and 9.0 GiB (the "Measured" table above). The first figure is a floor, not a
result: it loads the module and 36 jars, names all 4,641 rows and solves 485 of them. The solver's
time on the rows still uncovered is not in it.

### Status by slice

### Stretch 5, second unit: type constructors written as class arguments

A class's type parameter that takes a type constructor (`F[_]`) may be given a written one:
`Wrap[[X] =>> Either[Boolean, X], Boolean]`, `Functor[List]`. `Shapes.argument` keeps such an
argument as a closure: the written type with the environment it was read in, behind a token
that the parameter is bound to. Applying the parameter (`F[A]` in a field) applies the written
type to the arguments and reads it with the binders of both sites. Three readings refuse
instead of counting:

- One symbol bound differently at the two sites, as when a class's own parameter is passed back
  to the class (`def back(w: Wrap[[X] =>> (A, X), Boolean]): A` inside `Wrap`): read with the
  applying site's binding it would give 0 or 16 where 1 and 4 are right.
- A closure used whole as a type key reads the written type in its own scope, never the token,
  since two reads of one type mint two tokens.
- A class's proper parameter bound to a value's shape (`A` bound to the values of `Int`) is not a
  type key: `G[A]` inside a record and `G[Int]` at the site would be two types. This was a wrong
  number of the first unit (`def m[G[_]](b: Bx[G, Int]): G[Int] = b.v` read 0, where 1 is right);
  two cats-core rows that the first unit counted 1 under such keys,
  `IndexedStateTContravariantMonoidal.unit` and `.product` (`?` under scalameta), are uncovered
  again, their lines deleted from the coverage memory.

A deferred inline method now asks the templates around it for type parameters, not the
environment, which since this unit holds the site's binders too. Method rows are otherwise
identical on all seven inputs.

### Stretch 5, third unit: bounded proper type parameters

`Bounds` reads the bounds of a proper type parameter from its symbol, as `BinderBounds` reads
them from syntax: `A <: U` is the proof `A <:< U`, `A >: L` the proof `L <:< A`, joined to the
environment as `<bound#i>` values; `Any` above, `Nothing` below, and `AnyRef`, `Matchable` or
`AnyVal` above give no proof. Only another type parameter, a rigid application or a closed type
is a proof; a trait of the module whose record reads is closed, as the old resolution reads it. A
bound is read for the parameters the result and the values mention, and for those its bounds
mention in turn. Refused as before: a cycle of upper bounds through values (`A <: Option[A]`) and
a structural bound. Not read yet (uncovered): a bound that holds a top type, whose place a proof
does not carry (`A >: AnyRef` would build one value where any object is a result), and a
structural lower bound beside a class bound anywhere among the parameters read (the old reading
asks which class the lower bound's values are instances of). `Shapes.binder` makes such a
parameter an atom; constructor rows and sums still refuse it. `closed` moved from `BinderBounds`
to `EvidenceAnalysis`, which the compiled front end may import, as it may `Graphs`.

Method rows: cats-core 681 identical (22 more), fixtures 4,518 (74 more), the other inputs as
before. Four cats-core rows are corrections, listed with derivations: `Ior.getOrElse` (1, where
an Ior receiver gives 4), `Ior.mergeLeft` and `Ior.mergeRight` (1, where it gives 2), all three now
the solver's refusal of a proof beside a sum; and `As.reify` (0, where a witness exists), now the
anonymous-instance refusal of the As group. The fixtures for this unit and the closures of the
second unit are in `TypeParametersSpec`.

### Stretch 5, fourth unit: rigid constructors in constructor rows

A constructor row reads an invariant, unbounded `F[_]` parameter of its class as the rigid
constructor `Shapes.binder` gives; a position over it is a type parameter's slot, left to the
shape model. A class parameter that itself declares a variance (`+F[_]`, `-F[_]`) is no rigid
constructor anywhere now, since its applications are related by subtyping (a reviewer's finding:
`final class C[+F[_]](val a: F[Option[Int]], val b: F[Some[Int]])` would read 1 where 2 is
right); no row of the inputs had one. Method rows: cats-core 842 identical (161 more) and one
better past a resolution limit (`EitherK.<init>`, 1: the one `Either[F[A], G[A]]` in reach is
`run`), cats-eo 191 (4 more), fixtures 4,541 (23 more).

### Stretch 5, fifth unit: capabilities with type parameters of their own

A capability member's own type parameters are read as the scalameta front end reads them:
`PolymorphicMembers.close` (PB0, PB1 Yoneda, otherwise `Shape.Poly` for the solver; a bounded
binder the type meets, and one the result applies, refused). To share it, `PolymorphicMembers`
no longer imports scalameta: `close` takes each parameter's name and declared bounds, and the
syntax reading of a binder moved into `MemberSyntax`. The compiled front end binds each own
parameter to the member's `Binder` atom, or rigid constructor for `F[_]`, through
`Shapes.binder`; a written `<: Any` or `>: Nothing` is no bound there, which can only read more.
Inherited capabilities (`Receivers.inheritedCapability`, the gate agent's) still refuse one.

Method rows: cats-core 875 identical (33 more) and two better past a model limit, derived
(`Semigroupal[A => *]` and `Defer[A => *]`, each 1); cats-eo 193 (2 more); fixtures 4,604 (63
more).

### Stretch 5, sixth unit: literal and singleton types

A literal type is the one value of its constant, under the identity the old front end gives it
(`LiteralTypes.identity`, shared by both front ends now); as an argument of a rigid constructor
it is that identity's key. A floating zero is read too: the compiled constant keeps its sign,
which the parser did not (fixture `Z.zero`, 1, derived). A written `p.type` (`Shapes.singleton`)
has the one value p, a terminal goal: a method's parameter is that value of its declared type,
under the identity of the parameter's binding, so that an input `y: x.type` adds nothing beside
`x` (a reviewer's finding: a method of a class named it otherwise, and `final class Dep { def
f(x: Boolean, y: x.type): Boolean }` read 16 where 4 is right); a static object read by its
declaration is one value where it is written without a parent and declares types alone (the
compiler's members and parents of a case object aside), and refused otherwise with the old
reason; any other static path (an object or value of the standard library, an enum's value case)
is a value of its own name. Not read yet: a constructor's `val`, a local value, and a path through
an instance.

Method rows: cats-core 892 identical (17 more) and 7 better past a resolution limit, all of
`EitherObjectOps` over its `Either.type` field (`left`, `right`, `unit` 1; `fromOption` 2;
`raiseWhen`, `raiseUnless` 4: one of `Left(ifTrue)` or `Right(())` per Boolean;
`catchNonFatal` ω, a `Throwable` result), each derived by hand; cats-kernel 194 (3 more,
`Comparison`'s singletons), cats-eo 195 (2 more), cats-eo-schemes 5 (1 more), fixtures 4,679
(76 more).

### Stretch 5, seventh unit: unions and intersections

`Shapes.union` reads a union as `UnionSums` does, with the compiler's subtyping in place of the
syntactic type equalities: an alternative below another is that one (`Null` below a reference
type, `1` below `Int`, a class spelled twice), a top-like one takes them all (`AnyRef`,
`Matchable` are `Any`), `Nothing` and `Null` are empty. What remains is the sum of its
alternatives where their values are told apart: distinct constants, or runtime classes (a
primitive is its box: `Int | Integer` refused, a reviewer's finding) of which no instance has
two, by the closed world (`ClosedProgram.mixes`, now asked through `World.mixes`). Anything else
is the old refusal. `Shapes.intersection` keeps the narrowest parts by subtyping (`T & Any`,
`x.type & A` for an `x: A`), reads each part kept (what a part needs is the row's reason, as
there), reads two distinct static objects or enum value cases as empty, `Singleton` as a
demand for a singleton, and refuses any other meet. Reading the parts first keeps the
intersections of type classes (`Order[X] & Hash[X]`, 98 rows of cats-core and cats-kernel)
uncovered until their records read: a first version refused them at once, identical `?` to the
old rows, and its lines are deleted from this branch's private memory.

Four better rows past a model limit are derived (`U.wide`, `1 | Int` is `Int`, 2^(2^32); `U.v`,
`Vector[Int] | String`, ω; `u`, `S | Q`, 2 + 1; `v`, `AnyVal | Boolean` is `AnyVal`, ω as a
result). Method rows: corpus-core 1,395 identical (10 more, refusals of unions of enum case
types), cats-eo 197 (2 more: `Array[Int] | Null`; `S & B` refused as before), fixtures 4,771
(92 more).

### Stretch 5, eighth unit: type parameters of other kinds, read where named

`Shapes.binder` refuses a higher-kinded parameter with a bound, or with a variance on a parameter
of its own, with the old reasons (`types/Kinds.refusal`); the bounds are read, not the spelling,
so a written `F[_] <: Any` says nothing (the old front end refused it; no row of the inputs has
one). A parameter declared `+F[_]` itself, and a constructor of constructors, stay uncovered. A
capability's own constructor keeps the uncovered reason: the old front end reads it as rigid
whatever it declares (`MemberSyntax`). A type parameter that is not read is now kept in
`Shapes.Env.unread`, its reason that of a type that names it only (`Env.binds`, `Env.binding`),
as the old front end binds its parameters lazily: an unused parameter's kind tells nothing.
`Readings.environment`, `Receivers.atoms` and `Constructors.count` build their environment
with `Shapes.Env.of`.

Method rows: cats-core 936 identical (44 more: `NonEmptyCollection`'s members and the bounded
`S[T] <: Semigroup[T]` rows, refused as before), cats-kernel 231 (37 more, among them the eight
`*Functions.<init>` rows, 1 each: no parameter, whatever `E[T] <: Eq[T]` is), cats-eo 198 (1
more), fixtures 4,774 (3 more).

### Merge of `tasty-gate` at `a8dc05f` (the coordinator's instruction)

Records with parents, records with overridden members, rule B (`Factories`) and abstract type
members of receivers. Conflicts: `Receivers.atoms` keeps the gate's abstract type members, read
eagerly, beside this branch's type parameters read lazily (a list of each symbol's reading, for
`Shapes.Env.of`); `Shapes.binds` keeps the gate's path check for a type member and lets an unread
type parameter through to its reason; `Constructors.count` reads the receivers around it and
builds `Env.of`; the gate's `constructor(bounds)` is `rigidBounds` (an abstract type member's);
`ClosedProgram.mixes`, which the gate deleted as unused, is restored for `Shapes.union`. One row
the two branches free together is derived: cats-kernel's `catsKernelStdOrderForChar`, 1
(`CharOrder & Hash[Char] & BoundedEnumerable[Char]` is `CharOrder`, a concrete class without
parameters nothing extends). Bounded or other-kind abstract type members are the gate agent's
now (the coordinator). Method rows: corpus-core 1,543 identical, cats-core 1,280, cats-kernel
316, cats-eo 214, cats-eo-generics 37, cats-eo-schemes 5, fixtures 4,987; all inputs pass.

### Stretch 5, ninth unit: members named like the measured definition

The old front end's `selfView` (`CapabilityRecords`), over symbols in `Shapes.record`'s `clash`:
a record's member that varies across instances (abstract, or overridden by a class of the
program; an inherited declaration as the record's class sees it, `overridingSymbol`), of the
measured definition's name and arity (`Shapes.arity`, value parameters clause after clause),
refuses the row with the old reason, and only where one instance may be of the record's class
and of the class the definition is a member of (`World.mixes`). A value built before the
receiver is never it (`Env.preceding`, the old `builtBefore` and its anchor): a capture of a
method around the definition and a receiver's constructor parameter whose field nothing
overrides, not inline, and only the class the whole type names (not past a function arrow, a
by-name arrow, a type argument or a member). `ClosedProgram.mixes` now decides by the
subclasses of either class where those are all known and read (it gave up where one side's were
not). Every row of the reason is read or refused: corpus-core 1,599 identical, cats-core 1,527,
cats-kernel 330, cats-eo 228, fixtures 5,030.

Derived (better past a model limit): `cats.Show.show` ω (the object `Show` is no `Show`, which
the old reading confused by name); cats-eo `Getter.andThen`, `MendTearPrism.fuseToMendTear` and
`.andThen`, 1 each; fixture `Swapped.twice` ω, `PolyGetter.again`, `Named.call`,
`Hidden.call` 2 each (inline methods whose expansion calls a member that applies); the
`<new#1>.apply` of `applyK`, `localK`, `liftK`, both `liftFunctionK`, 1 each; eleven
`NonEmptyList` instance members, ω (a list of any length); fixtures `U.u` ω and `S.un` 3 (unions
the new `mixes` tells apart). Corrected: fixture `U.helper`, 2^32 -> 2^16 (`u.m` is `helper`
itself on `u`, no input). The gate's `RecordsSpec.show` is read now, 4. Private memory: the
lines of `U.u`, `S.un` (`?` under the earlier `mixes`) and of `Deferred.show` (counted by a
version that took a captured function's result for preceding) are deleted.

Found, not changed (both front ends agree, so the gate cannot see it): a capability record's
member whose value is another record with a polymorphic member reads too few implementations.
`trait FK[F[_], G[_]] { def apply[A](fa: F[A]): G[A] }; trait Inj[F[_], G[_]] { def prj: FK[G,
[a] =>> Option[F[a]]] }`: a method returning `new FK[...] { def apply[A](a: EK[H, G, A]):
Option[F[A]] = a.run.toOption.flatMap(i.prj(_)) }` counts 1 where `None` and `i.prj(g)` are 2.
This unit leaves such a row refused as before.

### Stretch 5, tenth unit: anonymous contextual parameters of the measured method

`Readings.anonymous`, the old `ContextualEvidence.unreachable` over symbols for the measured
method's own anonymous `using` parameters and context-bound evidence (`Readings.anonymous` of a
`ValDef`, the compiler's `x$N`/`evidence$N`): what encloses the method is shallower and loses,
and the body is counted, not searched (the old front end's method frame reads no statements
either), so its competitors are the method's other contextual parameters. One of its very type is
ambiguous (refused, reported before any uncovered pair); one of a distinct class, read by its
declaration or final, not `Null`, neither class extending the other, leaves it a value; any other
neighbour, and a context-function result (its binders hide it in every body), leave the row
uncovered. Captured enclosing methods' anonymous parameters, a receiver's and a capability's stay
uncovered (`plainInputs`). One better row derived: fixture `Neg.summoned`, 1 (`summon` is
Predef's inline method, resolved where the body is written). Method rows: corpus-core 1,601
identical, cats-core 1,556 (29 more), cats-eo 231 (3 more), cats-eo-generics 40, fixtures
5,078 (48 more); the 184 cats-core rows still named "an anonymous contextual parameter" are
receivers', capabilities' and captured ones.

### Relations moved out of Shapes

The union and intersection readings, the record clash test and their helpers moved, unchanged,
from `Shapes.scala` (928 lines) to `Relations.scala` (`Relations(world, Shapes.of)`); `Shapes`
is 787 lines. All seven inputs pass with the same rows.

### Stretch 5, eleventh unit: the back end's undercount of a polymorphic member inside a record

Root cause (`GoalSolver`, shared by both front ends): a record's member whose value is another
record with a polymorphic member (`trait Inj[F[_], G[_]] { def prj: FK[G, [a] =>> Option[F[a]]]
}`, `FK` holding `def apply[A](fa: F[A]): G[A]`) reached the solver as `Function(Nil,
Product(List(Poly(…))))`. Its producer path's output was the `Poly`, and only a value whose shape
is a `Poly` is instantiated (PB2, PB5), so the member was never applied: `def viaPrj[F[_], G[_],
A](i: Inj[F, G], g: G[A]): Option[F[A]]` read 1 (`None`) where `None` and `i.prj(g)` are 2. Fix:
`flatten` reads a function or polymorphic value that holds a polymorphic member, at any depth, as
that member taking the callable's parameters first (`hoisted`: a function into a product is a
product of functions; a binder commutes with an arrow, a product or another binder that does not
mention it). A binder is one per declaration, so a member met inside its own result (`trait K[F[_],
X] { def run[A](fa: F[A]): X }` at `K[F, K[G, C]]`) is not moved where it would be captured; a PB5
instance is read by `flatten` too, which applies the inner member once the outer binder is
substituted. A value that holds no polymorphic member is unchanged. Regression:
`PolymorphicMemberSpec.nestedRecord` (direct, one level, two levels, a member with a parameter, a
polymorphic outer member: 2 each; `k.run(fa).run(gb)` over `K[F, K[G, C]]`, which read 0 in both
front ends where 1 is right, is refused now).

Changed rows, all on cats-core, each a number of the compiled front end where the old one says `?`
(BETTER past a resolution limit, not listed in `derived.txt`), now the solver's own refusal (`sum
elimination over an argument space not enumerated`), identical to the old `?`: `InjectK.apply` (line
51, 0) and `InjectK.unapply` (53, 1), `InjectKInstances.catsRightInjectKInstance` (78, 0), its
`<new#1>.inj` (80, 0) and `<new#1>.prj` (83, 1), and `InjectK.apply` (91, 0). Each was the
undercount: the `inj` and `prj` of an `InjectK` (each a `FunctionK`) were never applied, so
`apply(fa: F[A]): G[A]`, which has `inj(fa)`, read 0; `unapply(ga: G[A]): Option[F[A]]`, which may
answer `None` or `prj(ga)`, read 1; and the object's `apply(implicit I: InjectK[F, G])`, which has
`I`, read 0. No old row changed: the
old front end reaches these members through an unresolved `λ` and refuses them first. Their six
lines (the wrong numbers) are deleted from this branch's private memory, and the shared memory needs
the same six deleted when this branch is merged; every input passes otherwise.

### Stretch 5, twelfth unit: match types, a constructor's anonymous evidence, evidence of one class

- A match type (`Shapes.matchType`) is what tasty-query's `MatchType.reduced` gives, where the
  scrutinee is ground (static classes and constants, not `Null`). On anything else tasty-query
  reduces where scalac stays stuck: `Fst[T]` for a `T <: (Int, String)` is `Int` there, and
  `def g[T <: (Int, String)](x: Fst[T]): Int = x` does not compile; `IsStr[Null]` likewise (a
  reviewer's finding). One stuck on an unbounded type parameter held rigid (a method's, or the
  class's own through `this`: `Shapes.rigidParameter`), under a single case projecting a tuple of
  distinct binders and without a bound, is the old projection atom (`StuckProjections`, members
  MT3), and `StuckProjections.refusal` is asked in `Readings` and `Constructors` as in the old
  `Measurement`. The index of the projected binder is shared (`StuckProjections.component`); the
  old syntax reading of the case moved to `MethodProjections`, so `StuckProjections` no longer
  imports scalameta and the compiled front end may use it. Not read yet: a match type in a
  constructor's position (tasty-query gives the alias's own parameter as its scrutinee there), one
  on a bounded parameter, any other stuck form.
- A constructor's anonymous contextual parameter (`Readings#constructorSearch`) is read by the
  measured method's rule (`searched`, the old `ContextualEvidence.unreachable`) where its class is
  written without parents or a self type and its body imports nothing and declares nothing
  contextual but its parameters; otherwise the class body's candidates compete with it, which the
  old `ContextualScope` reads at the template, and the row stays uncovered (a reviewer's finding:
  `final class K[A](using Option[A]) { given Some[A] = null }` would have counted a parameter no
  search reaches). Superseded by group h4: a constructor's evidence competes with its own parameters only.
- Two contextual parameters of one class whose arguments are rigid unbounded type parameters,
  distinct at some place (`[A: Show, B: Show]`), are never subtypes of each other whatever the
  variance (`Readings.unrelated`, the old `ContextualEvidence.unrelated` over distinct atoms).

Derived past a model limit: fixture `boundedMatch` 1 (a match type bounded by `Any` stays stuck,
`x` its only value) and `O.viaMatch` 2^(2^69) (`M[Int]` is `Int ?=> Int`; with `x: Int`,
(2^32)^(2^64)). Corrected: cats-core `IorInstances.catsDataShowForIor`, 1 -> ω (the Ior ruling: a
`Show` of a nonempty `Ior[A, B]` returns a `String`). Regressions: `MatchTypesSpec`, and
`ContextualSpec` (`both`, `Flat.<init>`, `Crowd`, `Selfish`). Method rows identical: cats-core 1,605
(4 more), cats-eo 242 (11 more), fixtures 5,150 (34 more), the other inputs as at the merge; every
input passes.

### Stretch 6, first unit: the stored side's fixed point

- One solver for both front ends. The old `capacity/Solver` keeps what it reads off the syntax
  (its `Shapes`, the dependency graph, positivity, the classification of a field into a
  continuation); the Kleene iteration with widening, the ν completion of lazy cycles and
  `completeCoinduction` moved unchanged to `capacity/Fixpoint.scala`, generic in the key of a
  definition and the store of its values (`Store`). The old front end's rows are byte-identical on
  the six corpora before and after (the cached `old/*.bin` of both hashes compared). The one change
  of code: the ν refinement stops when its values are stable (the old loop compared scopes by
  reference and always ran its budget, to the same values).
- `Stored`: a reading that reaches a class it is inside of (the former `a definition that reaches
  itself`) is read again as a system, one equation per class of the module it names (`Instance`:
  the class with its arguments' sizes), each a variable of the others, solved by `Fixpoint` with
  the old classification of continuations (`=> X`, `() => X`, a strict `Option[X]`, `D => X`, `X`
  named without arguments) and lazy fields. A definition's own row is its solved value; any other
  reading reads its root over the solved values; the reasons are those of every equation. Refused:
  a cycle through a function's domain or a partial function's keys, a lazy cycle the completion
  leaves incomplete, a widened value its equation does not reach again (an intersection's bound
  or a factor of 0: the old instantiation solver's `recursive instantiation not settled`), a class
  read with more than eight argument lists (polymorphic recursion that grows), a class read
  through a type argument that reads a variable.
- Not changed, found by a reviewer in the moved code: the ν completion's `holedChildren` takes
  every class whose field continues through a sealed parent, a child of it or not, so the old
  front end counts `W(t: () => T)` beside `sealed trait T; A(h: Boolean) extends T` as 3 (true: 2,
  probed). The compiled side reads it without a system, 2, and refuses such a case inside one (the
  widening check); the old rows stay as they were, for the owner to decide.
- `Constructors`: a position the method side refuses as recursive takes the estimate's size as the
  old `ConstructorPositions.readPosition` does, its reasons first as the old `declined`; only where
  the type is a class reference, applied or by-name type with no top, `Null` or `Nothing`
  (`estimable`), which is what the old rewrite hands to the estimate, and where the estimate's size
  says a value exists (not the algebra's 0 of a function from `Nothing`, an intersection's bound).
  A position refused for another reason stays uncovered: the old reading takes the estimate there
  too, but for `sealed subtype hides a parameter` the estimate counts a plain class's parameter
  the method side's equivalence does not (`Node(h: Boolean, next: => Lz)` beside `sealed Lz`: the
  estimate's ω where Node's values may all be one), so those 8 old numeric fixture rows stay
  uncovered for the owner; a union or an intersection refused inside a field would take the
  estimate's sum or bound. Rows the first version of this rule measured as `?` that way (core 7,
  cats-core 2, cats-eo 1, fixtures 14) were deleted from the private memory only (none is in the
  shared one).

Derived past a model limit (`derived.txt`): the two `D(q: C)(x: q.X)` fixtures (`X = D`: 0;
`X = Option[D]`: D = 1 × (1 + D) widened to ω, the position and `C#X` 1 + ω = ω + 1), the two
`M[A]` fixtures (M = 1 + min(M, 2), least fixed point 3; `Go` 2; `UM` 3; `UM.<init>` is `?`, its
estimate an intersection's bound), the cross-package
`A(b: Option[B])`, `B(a: Option[A])` (ω, positions ω + 1), `Chain[A](next: Option[Chain[A]])`
(ω + 1), and core's `Member.<init>` and `Entry.<init>` (ε₀: a factor `List[String]` in the ε₀ tier,
the others nonzero). No correction. Regressions: `StoredRecursionSpec`, and `TastyFrontEndSpec`
(`Rec` ω, its position ω + 1). Old numeric rows still uncovered (methods; both tables): core 8
(20), cats-core 44 (47), cats-kernel 111 (111), cats-eo 73 (76), cats-eo-generics 0, cats-eo-schemes
0, fixtures 673 (747); every input passes on the private memory.

### Stretch 6, second unit: a value of a class of the module

- A top-level value whose type names a class of the module is read as the old `ValueRows` reads
  it (register D30): one value where its type has one. While a value row reads a class (each one
  the reading meets, in a system too: `equation` takes the value flag), what it stores is no
  evidence that an instance is built (the old `Builds`): a class whose template, a module
  ancestor's or an enum class case's always throws leaves the row undecided (`Stored.unbuilt`,
  over `World.templates`). Refused: one whose construction binds a pattern (slice S6), an enum with
  parameters and a value case (its clause's arguments are not read), a class of a dependency.
- `Bodies.throws` follows a parent's default (`$default$` getter) into its body, as the old
  `Builds.omitted` does: `final class Dk extends Dp` with `class Dp(x: Int = ???)` is never built.
  This feeds the method side's `alwaysThrows` too. A by-name parameter's default is taken as
  evaluated, as a written by-name argument already is (a reviewer's note: `class A(x: => Int =
  ???)` is buildable; the old reading makes the same approximation).
- Regressions: `StoredRecursionSpec` (`values`), `TastyFrontEndSpec` (`made`, 1).
- A first version followed the default's symbol before its name and crashed on four cats-core
  sources (a refinement's member has no symbol): the name is read first.

Old numeric rows still uncovered (methods; both tables): core 8 (8: the 12 `Size` values are read),
cats-core 44 (47), cats-kernel 111 (111), cats-eo 73 (76), cats-eo-generics 0, cats-eo-schemes 0,
fixtures 673 (744); every input passes on the private memory. No derivation or correction.

### Stretch 6, third unit: hidden captures

- A parameter of a method around a local definition whose name a scope between binds again (a
  block's definition, an inner method's parameter, a class's member, the definition's own
  parameter) is hidden: the environment drops it, as the old front end binds the inner name over
  it (`Readings.hiddenCaptures`, was the refusal `a capture a nearer scope binds again`). The old
  `HeldCaptures` rule is ported over symbols (`Readings.kept`): a definition the measured one sees
  in such a scope (a block's statement, a member or parent of a template on the path, a method the
  definition is in) that reads the hidden parameter holds it. One that is the parameter and
  nothing else, at its type and nameable at the definition, keeps it a value; at a type with one
  value it shows nothing; any other reading is refused (`a hidden capture a definition reads`). A
  `throw`'s operand reads nothing unless a `try` around it may catch it. The row's captures still
  list a hidden name, as the old ones do.
- Refused where the old front end reads more than it should, each a reviewer's finding: a method
  the definition is in that reads the parameter outside it (the old reading skips it, though the
  definition may call it), a singleton-typed alias, an alias whose own name a nearer scope binds
  again, a named contextual parameter (search reaches it whatever hides its name: `a hidden
  contextual parameter`), and a parameter a self alias between may hide (the old reading keeps the
  outer one there: `a capture a self alias may hide`). An anonymous contextual parameter is not
  hidden by name: it keeps its own reason.
- Derived past a model limit: cats-core `catsStdInstancesForList.<new#1>.void.build`, ω (a
  `List[Unit]` of any length; the hidden `fa` of `void` is read by nothing it sees).
  Regressions: `ReceiversSpec.locals` (`Hide` 4, `Held.kept` 16, `Held.top` 4, `Held.gone` 4,
  `Held.own` 1, and the refusals).
- Of the 41 + 2 old numeric rows of the group, 13 fixture rows are identical (and
  cats-core's `foldMapK.loop`, `?` on both sides); every other one now stops at a later reason (a local class or object, a parent with
  constructor parameters, an overridden member: the gate agent's), or at a refusal above.
  Old numeric rows still uncovered (methods; both tables): core 8 (8), cats-core 44 (47),
  cats-kernel 111 (111), cats-eo 73 (76), cats-eo-generics 0, cats-eo-schemes 0, fixtures 660
  (731).

### Stretch 6, fourth unit: throwing constructions and pattern definitions (decision D5)

- A pattern definition is an ordinary binding: the refusals for one are gone (`Bodies.destructures`
  is deleted). A match whose scrutinee always throws throws (`val _ = ???`, `Bodies.throws`).
- A member runs in its own template and in each one its instances carry (`Readings.built`, the old
  `ReceiverCapabilities.construction`): an inner class's outer instance, an anonymous class's
  receiver, a member object's owning instance; a template a static object encloses is built
  without the object. Where one of them, or a module ancestor of one, always throws (the measured
  member's own initializer aside), no call returns: 1. An inline member is not stopped by a static
  owner it is expanded out of. A constructor builds its instance: its row is counted whatever the
  object around it does (was `a constructor in an object that is never built`). `Receivers` no
  longer refuses a class or object whose construction throws.
- A case of a sum whose construction always throws (its own template or a module ancestor's, the
  measured member's initializer aside: `World.unbuilt`) is never built and adds no summand, on
  both sides (`Sums`, `Stored.family`); a record of such a class is the empty sum (`Shapes.record`,
  its ancestors too). A value row of a class with a pattern definition is read (was refused).
- Derived past a model limit (`derived.txt`): 14 fixture rows, the `U.*` functions of
  `be82e558…/Example.scala` over families with a never-built case (an enum's singleton cases are built together, by its companion), `R.z` 2^32 and `E.x` 2^64 (their
  own initializer aside), the three `Destr` members, `PInner.f` 2^(2^32). In `TastyFrontEndSpec`:
  `Use.arg`, `thrown`, `rep`, `cur` 0 (derived), and six corrections, `Nam`, `Ord`, `Asc` 1 -> 0
  and `nam`, `ord`, `asc` 1 -> 0 (a throw behind a named argument or an ascription, which the old
  reading does not see: each construction always throws, so each family is empty).
- Regressions: `ConstructionSpec` (a pattern definition, `val _ = ???`, a pattern definition that
  throws with its own name measured, anonymous classes never built or never made),
  `ReceiversSpec.later` (`Thrown.get` 1, `Dead.In.get` 4), `StoredRecursionSpec.values` (`pat` 1),
  `TastyFrontEndSpec` (`Thr` 0, the families above). Reviewers' findings fixed in the unit: the
  measured member under an anonymous class, a static object a template is made in a method of,
  the statement of a pattern or multi-name definition (only in its own template), an enum's
  singleton cases built together.
- Every old numeric row of the group is read: 81 fixture rows become identical. Old numeric rows
  still uncovered (methods; both tables): core 8 (8), cats-core 44 (47), cats-kernel 111 (111),
  cats-eo 73 (76), cats-eo-generics 0, cats-eo-schemes 0, fixtures 583 (650).

### Group g5: captures a nearer scope binds again; definitions in function literals, case clauses and class bodies

Readings.local, read over the typed tree from the outermost code the definition sits in: the
outermost method or value, or the whole class for a class-body statement.

- A parameter that a nearer scope binds again is dropped from the inputs. The nearer scope is a
  block's definition, an inner method's or literal's parameter, a class's member or self alias,
  or the definition's own parameter. The drop happens unless other code holds the parameter
  (`holding`, the old `HeldCaptures`):
  - A definition beside the measured one that is exactly the parameter (`def seed: Boolean = b`,
    a `lazy val`, a given) keeps the parameter as an input when it has the parameter's own type.
    At a top type it shows nothing.
  - Any other holder refuses: a strict value computed from it, a class beside it, or the parents
    of a class around the definition.
  - A throw supplies nothing.
  - A contextual parameter is still found by implicit search, so it refuses.
  - An anonymous one has no name to hide.
- A function literal is a method around the definition. A wildcard parameter is no input. A
  generator's literal is refused: its parameter is written before the receiver of the call
  (`Readings.generators`).
- A case clause around the definition is read when its pattern binds nothing and, with a type
  parameter in scope, adds no type equation (the old `BodyScopes` refusals). These scope
  refusals come after the one-valued result check, as the old ones do.
- A class-body statement reads the instance as a member does. A parent's argument does not: it
  is evaluated outside the instance.
- Decision: a secondary constructor's body stays refused. The call it makes first fixes the
  fields, so a field there is no free receiver value. The old front end counts it as free:
  `S.<init#1>.helper` after `this(true)` is 4 there, and observationally 2. Its rows (7
  fixtures) are left for that reason.
- Decision: a case binder stays refused, as the old reading does. It aliases the scrutinee, so
  counting it as a parameter overcounts: `o match { case Some(b) => def g = b }` would be 2^6 in
  place of 4.
- Found: the old front end undercounts a hidden parameter that a strict value holds.
  `{ val h = !x; def inner(x: Boolean): Boolean = h }` is 4 there, and is 16. The new reading
  refuses it.
- Correction (in `ReceiversSpec`, no corpus row): `recv.Self.mk.<new#1>.run` goes from 16 to 4.
  The self alias `y` hides the parameter `y`.
- Derived: `catsStdInstancesForList.<new#1>.void.build` is ω. Its `fa` hides void's, which no
  definition beside it holds, and the result returns `acc: List[Unit]`.

Method rows identical before → after: core 2,832 → 2,841, cats-core 1,826 → 1,837, cats-eo
244 → 250, fixtures 5,204 → 5,236, others unchanged. Old numeric rows still uncovered: cats-eo
73 → 67, fixtures 670 → 640, the rest unchanged. No group reason is left on an old numeric
corpus row. Left in fixtures, 9 rows: 7 secondary constructor bodies, and 2 parent arguments that
hold a hidden parameter (`O.byParam`, `O.viaField`).

Merged into `tasty-int` with stretch 6's third unit, which ported the same old `HeldCaptures` rule
(`Readings.local` and `Readings.kept` hold the one implementation). The scopes walked are this
group's (function literals, case clauses, class-body statements); what holds a hidden parameter is
the third unit's: the holders of the scopes between the parameter's method and the definition,
each read only where the definition can name it (no nearer scope binds its name, inherits it or
imports), a method the definition is in among them, and a `throw` a `try` may catch. A self alias
hides a parameter, as here (`Self.mk.<new#1>.run` 4, a correction); the third unit refused it.

### Group g4: records of a class something extends, and parameters observed through a member or a parent

- A concrete class something extends is read as a trait or abstract class is (`Receivers.record`):
  its public fields and what a subclass supplies, a concrete member something overrides among them,
  as the old `openRecord` reads it. A non-field parameter refuses as before. A concrete case class
  something extends stays refused (`of a case class something extends`), as the old reading
  refuses it ("case class open to extension"). Decision: not read, though a goal of one such as
  `def i(x: Boolean): NC` with `NCX` overriding `NC.hit` would count 16 under the free-capability
  reading the old front end and `O.mk` (64) already use.
- A final class's parameter is observed as itself (`Observers`, the old `observes`): a field the
  site may select (not `protected[this]`, selected on `this` alone), an alias or an η-alias (a
  method of one clause whose body applies the field to its own parameters in order) of the field's
  type as the class sees it (shapes compared, so a member over a narrower domain is no observer),
  or the argument a parent's primary constructor takes verbatim, observed in turn in the parent at
  the template's parameter's shape (a wider parent field, `val w: Any`, observes nothing; a call of
  a parent's secondary constructor is not followed). An observer a class between declares again is
  overridden there. `Shapes.names` moved to `Observers`; `Bodies.passed` is shared.
- Derived (`derived.txt`): `Base.make` in the two `Scopes.scala` fixtures, 1: `Base` is extended by
  `Below`, has no field and no member besides the universal ones, so all its values are one value.
- Not taken: handing a constructor position the method side refuses for an unobserved parameter to
  the stored estimate, as the old `ConstructorPositions` does for any refusal. The estimate counts
  state no member observes: `A(h: Boolean, b: => B)` with memberless `B` is ω in the old reading
  but 2 by rule B, and likewise `UsesSt` (ω) and `UsesSorted` (8, the `Ord` evidence observed by no
  member). Those 15 fixture constructor rows stay uncovered; the old numbers there look like
  overcounts and are left for a decision.
- Regressions: `RecordsSpec` (`base` 4, `opened` 16, `fromHidA` 1, `getter` 1, `useGet` 256, `feed`
  4; `narrow`, `widen`, `viaSec` refused).

Old numeric method rows still uncovered after: core 8, cats-core 32, cats-kernel 78, cats-eo 60 (73
before), cats-eo-generics 0, cats-eo-schemes 0, fixtures 626; every input passes on the private
memory.

### Group g6: the rest of "a type", receivers' anonymous evidence

Paths, refinements and the polymorphic identity are read in `PathTypes.scala` (beside `Shapes`,
which stays under 1,000 lines); match types in `Shapes`, `Sums` and `Stored`; receivers' evidence
in `Readings` and `Receivers`.

- Aliases are followed as the compiler sees them (`PathTypes.follow`): a member alias through the
  prefix it is selected on (`TypeRef.optAliasedType`, the bounds as seen from the prefix), which
  `dealias` does not do: `p.X` for a `p: R { type X = A }` is `A`, a parent's `type Y = (A, A)`
  seen from `Q[B] extends P[B]` is `(B, B)`, `OG.T` of `object OG extends BaseG[Boolean]` is
  `Boolean`. An opaque alias stays (rule F is read where it is named).
- An abstract type member selected on a method's parameter (`p.X`), unbounded as the path sees it,
  is an atom of the path (members M0, the old `TypeMembers`): two paths never share it, and
  `q: p.type` is p's path. Read only where the parameter's type tells none of its values apart
  (only type members, or a top type): a record whose members may supply X (`trait Mk { type X;
  def mk: X }`) is not read, since its witness would not be tied to the path.
- A refinement of type members over such a type is that type where it can be satisfied: the
  member is new or abstract and unbounded in a trait, abstract class or `Object`, and the
  refinement fixes it or bounds it on one side. `T { type X = String }` beside `T`'s `type X =
  Int`, a refinement of a concrete class and a two-sided bound stay uncovered (the old reading
  refuses them as "not shown satisfiable").
- The polymorphic identity `[A] => A => A` (unbounded, one parameter, not contextual) is one value,
  the old `MethodTypeLambdas` closed fragment.
- Match types. A class's type parameter is the argument the class was named with
  (`Shapes.Env.written`, carried to a sealed case's parameters by `Sums.carried` and to a
  constructor's copies), so a field `Fst[A]` of `Miss[(Boolean, Unit)]` is the match on
  `(Boolean, Unit)`, which the compiler reduces; the two sites' binders are merged as for a written
  constructor, and one bound again refuses. A single case projecting a tuple of the scrutinee's
  arity reduces whatever the components are (`Shapes.projects`: scalac reduces it too), beside the
  ground scrutinee. The stored estimate of a constructor position reads a match type alias at its
  arguments (`Stored.matches`), reduced where the method side trusts it; one stuck on a type
  parameter that stands for what the instance chooses is that parameter's reason (the method side
  stands alone). The note "tasty-query gives the alias's own parameter as its scrutinee" was the
  estimate's `written` reading of the alias body, not tasty-query.
- A receiver's anonymous contextual parameter (a context bound's evidence, `final class
  EqOps[A: Eq](lhs: A)`) is read as a value where search from the member finds it
  (`Readings#receiverSearch`): its class is quiet (no parents, self type, imports or contextual
  members but its parameters), the definition is no local one (a method around it may declare
  givens in its body) and has no context-function result, and each competitor at its level or
  deeper (the class's other contextual parameters, those of the classes inside it, the member's own)
  is told apart from it by `apart` (the old `ContextualEvidence.unreachable` at those levels; one of
  the same type at another site is an unresolved overlap, not ambiguous). A constructor's parameter
  is compared by its field's type, which names the class's own type parameters rather than the
  constructor's copies (`Readings.seen`).

### Group h4: anonymous contextual parameters

The old `ContextualScope` and `ContextualEvidence.unreachable` over symbols, beyond the measured
method's own evidence (stretch 5, tenth and twelfth units) and receivers' (g6).

- One rule, `Readings#searched(evidence, competitors, wrapping)`: an anonymous parameter competes
  with the contextual parameters at its method or deeper and with the binders the measured
  definition's context-function result puts around every body (`binders`): the parameters of the
  function literals at the head of its body, which the compiler made from the result as it reads
  it (through projections, paths, match types and inherited aliases alike), or for a declaration
  without a body the result's clauses. A binder is declared by no site, so it is never ambiguous
  with a parameter; a literal names it, so it is no evidence.
- `apart` tells apart, beside the old classes: an unbounded rigid type parameter from a distinct
  one, and from a class type that is neither a top (`Any`, `Object`, `Matchable`, `Singleton`)
  nor `Null`/`Nothing` nor a context function (which also answers searches for its result): in a
  body only those relate to every such parameter (the old `unrelated` over atoms; the old reading
  keeps `Unit` related as the empty product, the new one does not).
- A constructor's anonymous parameter competes with its own parameters only. Decision (h4,
  reverting the twelfth unit's `quiet` rule for constructors): the constructor is a method; its
  body is the class's, which another implementation need not share ("a body is counted, not
  searched"), and no instance exists yet whose members, parents or self type could offer a given
  (the old front end exempts the constructor's own template from its self type for the same
  reason). The old front end reads it so: `Crowd.<init>` 2, `Selfish.<init>` 2^32 + 1, identical.
- A captured anonymous parameter of a method around a local definition (a function literal the
  compiler made around a context-function body included) is a value where `searched` finds it
  against the parameters of that method and the methods inside it and the definition's own, and
  no scope between (`Readings#local`, `Scope.offers`) declares a contextual definition, imports,
  inherits a contextual member through its linearization, or has a foreign self type.
- A capability's member's anonymous parameter is an argument its caller passes: read like a named
  one (`Shapes`).
- Captures name an anonymous parameter as the old front end keys it: `<using#i@path>`, and for a
  made function literal's binder `<context#i@path>` of the definition whose result it is
  (`Targets.pathOf`). A function literal's `_` the source writes (`(_: A) ?=> …`) is keyed by a
  `<lambda#n>` path the report does not spell: not read yet (`Literal.anon.<lambda#1>.<new#1>.get`).
  The key is of the owners' path only where each is a class, object or member.
- Correction: `shapes.Context.caller.<block#1>.passed` (`TastyFrontEndSpec`), 4 -> 2^(2^33): the
  block passed where `run` takes an `Int ?=> Boolean` is the body of a function literal giving an
  Int, which `passed`'s body may summon; the old reading sees the binders of a definition's own
  result only. Derived past a model limit (`derived.txt`): `S.fp.l` 1, `S.refined.lr` 2,
  `O.viaPath.lv` 2^(2^33), `O.viaMatch.lm` 2^(2^65), `O.byName.ln` 2^(2^33), `C2.h2.lka` 1, each a
  binder of a function literal the compiler made from a result the old reading does not follow.

Regressions: `ContextualSpec` (`captured`, `Crowd`, `Selfish`, `ctx`).

Method rows against `e318915` (identical / better / worse / uncovered; old numeric uncovered):
cats-core 2,130 / 369 / 72 / 5,080 (284 more identical; 28), fixtures 5,526 / 258 / 3 / 966
(45 more identical, 6 more better; 358, 41 fewer); the other inputs as at the integration. Of the
group's 42 fixture rows only `Literal.anon.<lambda#1>.<new#1>.get` stays uncovered.

### Group h3: types and type parameters

New readings in `PathTypes.scala` (beside `Shapes`, which stays under 1,000 lines), `Bounds`,
`Constructors` and `Stored`; regressions in `TypeFormsSpec`.

- A named tuple (`scala.NamedTuple.NamedTuple`, an opaque alias of the standard library) is read
  by the standard model under the name it spells (`PathTypes.standard`, `StdlibModel`: its fields'
  tuple); the stored side stores the fields too. Any other opaque alias of the standard library
  stays uncovered. A match on a named tuple is not reduced: the compiler reduces none (the old
  `namedStuck`), and `Shapes.trusted` does not trust tasty-query there (the opaque alias is no
  class, so the scrutinee is not ground and no tuple a case projects).
- A class applied to unbounded wildcards at proper parameters (`Packed[?, R]`) is a package
  `∃W. Packed[W, R]`, one witness per wildcard (`PathTypes.existential`, the old
  `ExistentialInputs`): the wildcards become the parameters of a type lambda whose references read
  as the witnesses' atoms. The solver opens an input package per value and refuses one built as a
  result. A bounded wildcard, or one for a bounded parameter or one that takes a constructor (a
  bound would give the witness proofs, not read), stays uncovered.
- A class bound (`AnyRef`, `Object`, `AnyVal`) beside structural lower bounds is a no-op where
  every structural lower bound is a subtype of every class bound of the environment (`Bounds`,
  tasty-query's `isSubType`; which parameter lies below which is not read), as `Matchable` always
  is; otherwise the row stays uncovered (the old reading refuses it). Scala 3.9.0 checked:
  `[A >: Flag <: AnyVal]` rejects every call (E057), and a trait extending a universal trait
  (`U1 extends U0`, `U0 extends Any`) is an `AnyRef` (`summon[U1 <:< AnyRef]` compiles), so
  `universal[A >: U1 <: AnyRef](x: A): A` is x or U1a, 2, where the old reading refuses it.
- A constructor row's bounded type parameters give their bounds' proofs (`Bounds`), as a method's
  do; the stuck projections are checked under them.
- A type parameter that takes constructors (`F[_[_]]`, `C[_[_]]`) is a rigid head like `F[_]`:
  its applications compare their arguments' identities (`key`), and a type lambda that applies one
  constructor to its parameters in order is that constructor (`PathTypes.Eta`: `[y] =>> G[y]` is
  `G`). A lambda of another body or order is not read as a key yet.
- An abstract type member bounded by one type on both sides, as its prefix sees it, is that type
  (`PathTypes.pinned`): an export's alias (`export Types.Pair`, applied or not) and a member a
  refinement fixes, selected on a type (`R#X` for `R[A] = { type X = A }`). `A | A` as a key is
  `A`. The stored side reads a type lambda applied at its arguments (`([x] =>> x)[A]` is `A`), as
  the method side's `dealias` does.

Rows (against the integration tree): the group's old numeric uncovered rows go from 80 to 26 (23
"a type", 3 "a written constructor over a parameter bound again"); method rows identical 5,481 ->
5,535 on the fixtures, 313 -> 321 on cats-eo, 40 -> 42 on cats-eo-generics; corpus-core better 15
-> 19 (four eo lenses whose complement is a named tuple, ω, derived in `derived.txt`, with
`K.universal`). No worse row; every gate passes.

Left: the rest of "a type" (paths through a class's `val`s and `this`, `this.Y` inside a
refinement, a higher-kinded member of a parameter applied, the stored side's match types over a
class's parameter, which its fixed point caches by sizes alone), a refinement's member aliasing a
sibling (`R[A, B]#X` for `X = Y; Y = A`: tasty-query leaves the lambda's parameter
unsubstituted), and the written constructors bound again.

### Rule B, first unit (g2): a sealed case's constructor and fields from the site

`Sums.caseFields` asks `World.callable` whether the measured site may call a case's primary
constructor and select each of its fields, instead of asking whether they are public: a
`private[p]` constructor or field counts inside `p`, a `private` one inside its class or companion
(rule B). A `protected[this]` field stays a restricted field: it is selected on `this` alone, never
on the value a site is given. Regression: `ProgramRowsSpec.scopedCase` (`V = E + Two`, `Two` two
`private[fx]` Booleans: inside `fx` `g: V => Boolean` is 2^(1+4) = 32; from `out` the constructor,
a `private[fx]` field behind a public one, and inside `Own` its `protected[this]` field stay
uncovered). Old numeric method rows uncovered: cats-eo 73 → 42, fixtures 670 → 658, all freed
rows identical; nothing else moved. Left out: a case the site may not build read through its
factories (`/tmp/g2/restricted-cases.patch`, part 2), whose review found it unsound (a case
pinning its parent's argument skips `Sums.carried`; `Factories.observed` misses a case class's
`unapply` and a protected override a parent member reads).

### Rule B, second unit (g2): a field selected on `this` alone

A plain class's parameter without `val` (`private[this]`) and a `protected[this]` field are
selected on `this` alone. `Shapes.exposed` took them as observed wherever the site was inside the
class, so a method of the class reading an argument of its own class counted a field the body
cannot read (`final class P(a: Boolean, protected[this] val b: Boolean) { def m(o: P) }` was
2^16). `World.selectable` (beside `callable`) refuses them on a value the site is given. TASTy
cannot tell a written `private[this]` from a `private` member the compiler found read on `this`
alone (both pickle `PRIVATE LOCAL`, checked with scalac 3.9), and a new body may select the second
on another value: a member private to the instance that is no field counts as `private` (an
explicit `private[this]` `val`, `def` or `object` in the body, deprecated, reads the same: a known
ceiling, `final class P(a: Boolean) { private[this] val x = a; def m(o: P) }` counts 16 where it
should be `?`). A `private val` parameter pickles `PRIVATE` without `LOCAL` and stays a field. Such a field still
counts where a member the site may select reads it (rule B's observed fields,
`Factories.observed`, which now asks `selectable` of members, nested templates and accessors), so
eo's `Getter.andThen` keeps its count. A constructor's position of its own class keeps the old
reading: the row reads it with the stored side's estimate (`Constructors.position`), whose value
holds every parameter. The correctness review flagged that estimate as a possible overcount of
the stored side's own (it counts fields no observer reads); the 18 fixture rows it gives
(`St(head: Boolean, tail: => St)` ω, …) are kept identical, and fixing `Stored` is left open.
Regression: `ProgramRowsSpec.thisOnly`. Rows: no number changed; one fixture constructor row
(`class Outer(a: Boolean) { case class Inner(o: Outer) }`, `Inner.<init>`, 2) is uncovered now,
its argument `o` observing no field of `Outer` (removed from the private coverage memory).

### Rule B, third unit (g2): cases the site may not construct, through factories (left out)

Tried and left uncommitted (`/tmp/g2/part2-wip.patch`, on `a7a2566`). A sealed case no constructor
of the site builds was read through `Factories`, gated on `Sums.carried` (a case pinning its
parent's argument, or with type parameters of its own, left), with a case class's synthetic
`unapply` observing its first clause and an override the site may not select leaving the class.
Every gate passed, but it frees one old numeric row (fixtures 659 → 658), and each correctness
review found another way for code outside the class to hand a hidden field to the site, an
undercount each time: a parent's member matching on the case, a companion's method, a parameter's
default getter (synthetic, skipped by `Factories.definitions`), another class's secondary
constructor or parent argument, a pattern through an extractor the site cannot reach, and a
public member reading a later clause beside the extractor. The same gap stands in `Factories` for
a final class of the module (`Shapes.record`): `observed` follows only the class's own body. Left
until observation is read over every definition the site may reach.

### Group h5: constructors

- **Recursive positions only (coordinator decision).** The constructor rows that keep the stored
  estimate through `Observers`' `own` clause (every field of the class observed in its own
  constructor) were checked by switching the clause off: 19 fixture rows move, 17 identical and 2
  better (`St(head: Boolean, tail: => St)`, `Dead`, `R`, `Loop`, `Loop2`, `Inf` ×2, `Timeline` ×2,
  `M3`, `CoNat` ×2, `Stream`, `O.St`, `K9`, `B7`, `H9`, and `D(q: C)(x: q.X)` ×2 with `C.X = D`).
  Each position's type reaches the class itself, so each is recursive and keeps the estimate; no
  row changed. The 12 "a constructor position the method side refuses" rows are refused for other
  reasons (a sealed subtype hiding a parameter, a type argument `Nothing` the estimate is not handed,
  an intersection, an anonymous instance) and stay `?`.
- **Secondary constructors** (`Constructors.secondary`, from `Readings.method` through
  `Constructors.count`): a method from the secondary's parameters to the tuple of the primary's
  arguments, built on the instances around the class as the primary's row is, the primary's
  copies of the class's type parameters bound as the class's are (the old
  `SecondaryConstructors`). A position over the class itself is refused (no estimate for a
  secondary). Bodies after the self-invocation (`<init#k>`) stay refused (g5's decision: the call
  fixes the fields).
- **Valued positions** (`Constructors.read`, the old `readPosition`): a singleton or evidence
  beside values with no type parameter is a value space; beside a top the method side reads it
  (`ClosedSpaces.pointed`, a singleton its one value), else the estimate's size, unless the method
  side's `pointed` reading disagrees with it, or the estimate says 0 with nothing to compare
  (`(Sing.type, Nothing => Boolean)` is `?`, where the old reading takes the estimate's 0).
- **Rule B on the stored side** (`Stored.restricted`): a class without type parameters whose
  constructor is restricted and that nothing supplied extends holds what its public builders build
  (`Factories.builders`, shared with the method side's `space`): each at most min(its parameters,
  the observed fields), the class the largest. The site is `defn.RootPackage`, outside every
  template. Observed fields (`Factories.observed(…, outside = true)`) add, beside the old reading's
  public members, every field module code outside the class names (a companion's, another class's,
  a default argument's, through private members of the class in turn) or that a member it
  implements names, and every field of a case class (a pattern may extract it): an over-reading
  where such a mention is unreachable, never the undercount g2's review found. A class something
  extends stays `a restricted constructor` (a subclass's `super` call builds it too); one with type
  parameters keeps its fields' product (as the old estimate). In a recursive system such a class is
  read through what it names, as a family is (`constructors`).
- **Fixed for the method side too** (`Factories`): a factory counts as a closed bound only where
  every template around it is an object (`World.inObjects`): one in an object nested in a class
  may build from the instance (was `Some(params)`, an undercount); a restriction on the class
  built or a template around it is disregarded (rule B). No method row moved.
- Derived (`derived.txt`): `fx.V.Two` 2 and `fx.V` 3: the public factory `V.of(a: Boolean): V`
  builds `Two` in one branch, min(2, the publicly shown `a`) = 2, `V = E + Two` = 3, as the old
  method side reads `out.Use.f` (2^3 = 8); the old estimate searched only `Two`'s companion.
- Not taken: part 2 of rule B for sealed cases (method side; `/tmp/g2/part2-wip.patch`): its holes
  are on the method side's per-site observation, which still follows only the class's own body;
  the sealed-subtype rows whose clause passes a parameter of another variance or bound (`Have[A]
  extends Opt[+A]`), which `Sums.carried` refuses for soundness (a contravariant use of the
  parameter would make the subtype's values unobservable), and parents in a class.
- Regressions: `ConstructionSpec` (`secondary`, `singletons`, `restricted`).

Old numeric rows still uncovered (methods; definitions), integration → h5: fixtures 399 → 371;
46 → 27, cats-core 28 → 28; 3 → 1, cats-eo 12 → 11; 3 → 1, cats-eo-schemes definitions 1 → 0,
the rest unchanged. Every freed row is identical but the two derived `fx` rows; cats-core's
`ZipNonEmptySeq.<init>` and `ZipNonEmptyVector.<init>` go from uncovered to better (old `?` for an
unresolved name; ω, as `Chain.Wrap`): the estimate now gives a restricted generic class its
fields' product, so the position's method-side reading stands. Two fixture rows `Base` (`Scopes`,
restricted constructor, extended by `Below`) stay uncovered: a class something extends is not
read by its builders.

### Group h1: classes written in a body, captures an import or an inherited member hides

What it reads (`Readings`, `Receivers.around`, `Constructors.count`, `Shapes.declared`):

- A class written in a method's body, or in an anonymous class or value initializer around one, is
  read as a class in an object is, its captures the parameters of the methods around it. Its
  members' rows were "a local class or object" (the closed world already makes it final where
  nothing in the body extends it); its constructor's row "an enclosing definition". A constructor
  row now takes the methods' parameters as values (`Readings.captured`, shared with members) and
  their type parameters as atoms (`binders`): `Box(x: A)` in `def f[A](a: A)` is 2 (x or a).
  The constructor's scope is the caller's: the class body's members do not hide a capture from it.
- An abstract type member of a local class refuses ("... no class written elsewhere extends: T is
  local"), as the old reading does ("fixed: L is local").
- A plain local case class (public fields only) or a members-free local class, with no instance
  class around it, is read as a type (`Out(i: In)` is 2).
- Captures a scope hides (`Readings.local`): an import written before the definition binds its
  explicit names and, for a wildcard, the qualifier's public non-given members that no sibling
  selector names; Scala then refers to neither (scalac: "Reference to seed is ambiguous"). A
  wildcard over a class not read, an unknown position, or a member of restricted access of that
  name leaves the row. An inherited non-private term member hides a capture of its name (the old
  check "a capture an inherited member hides" is gone): the capture stays a value only where a
  holder keeps it, now including a parent call of the nearest template that passes it whole to a
  public primary-constructor field of its type that nothing overrides (`passes`; `new P(n) { … }`).

Derivations added to `derived.txt` (better past a model limit): `U.hidden.Fn.apply`,
`U.fresh.Gn.apply` (2^(2^69), inline methods with no compile-time operation), `Out.outer.D.h`,
`Out.outer.W.h` (2: an export's forwarder hides the capture), `Out.run.C.g` (0: an import makes
the capture ambiguous). `Passed.imported.<new#1>.seed` (16, derived earlier) is uncovered now: the
wildcard `import Box.*` makes the captured `a` unnameable, and the parent call `Hide(a)` passes it
to a private parameter, a holder not read; the private coverage memory dropped its line.

Old numeric rows still uncovered (the `--rows` listing, against `e318915`): fixtures 445 → 359,
cats-core 31 → 29, the other corpora unchanged. Method rows, fixtures: identical 5,570, better
258, worse 3 (listed corrections), uncovered 922. Of this group's 108 fixture rows 23 remain: a
definition in a secondary constructor's body 7, a parent written in a body 4 (its members may read
a capture the subclass hides; not read), a class nested in a class 10, a wide or generic parent
field 2. cats-core: two `Deferred.<init>` rows are identical; the third stops at re-entry.

### Group h6: capability records and abstract type members

Four rules, each read from symbols and types:

- **An unread sum receiver beside an ω goal** (`Readings.counted`). This ports the old
  `SealedReceivers.stands`. A sum receiver that is not read cannot change a goal of infinitely many
  literals once the context is not absurd. So the row is solved again without the receivers
  `Receivers.supplied` words "an enclosing sum not read", and only an ω result is kept. Each
  dropped receiver must be an enum without type parameters, with a case the reading shows built
  from nothing: a value case, or a class case whose parameters, and the enum's own, are each of a
  closed nonempty space. A generic enum is not taken, because a case of a GADT may belong to one
  instantiation alone. That makes this check stricter than the old one, which did not look at
  that. A sealed trait is not taken either, as before.
- **An open constructor position over a type parameter is the shape model's alone**
  (`Constructors.position`). The stored estimate is asked only for a ground or closed position, the
  only ones whose size it is compared at. So its failure on a class it does not model
  (`IterableOnce`, `<:<`) no longer leaves `Wrap[A](io: IterableOnce[A])` uncovered.
- **A capability with a one-valued result supplies nothing, whatever its parameters**
  (`Shapes.member`, `Receivers.inheritedCapability`). Its parameter types and own type parameters
  are no longer read. For example `collectTo(…, lenBuf: IntArrBuilder, …): Unit` of cats-eo no
  longer stops its record.
- **An export's alias is the type it exports** (`PathTypes.follow`, `PathTypes.exported`,
  `Stored.body`). A type member bounded by one type on both sides is followed to that type, and it
  is not an abstract type member of a receiver.

Not taken, recorded for the next agent:

- Inherited capabilities with type parameters of their own (`PolymorphicMembers.close` over an
  inherited `PolyType`, instantiated at the member's own parameter symbols). This covered
  `Category.id` 0. But in cats-core it fed the solver large records, Comonad and CoflatMap among
  them, and two things went wrong. First, PB5's instantiation closure (`GoalSolver.instantiated`,
  which names each instance by its shape's `toString`) ran unbounded: one row ran for over 40
  minutes with no budget tick. Second, it counted ω for `Comonad[Tuple1]` and its siblings
  (15 rows past a model limit). I found no derivation for those. Reverted. To take it, give
  `instantiated` a budget first, then derive the ω rows.
- `cats.kernel.Order` "overriders not read" (`Size.order`, `FinitePart.order`,
  `catsDataOrderForIor`, the Const bounds) and `As.coerce` (a family with an anonymous instance):
  not investigated.

Regressions:

- `ReceiversSpec`: `Rose.name` ω; `Rose.flag`, `Thorn`, `Gadt`, `Bush` uncovered; `Exp.g` 4;
  `Exp.P.<init>` 2.
- `ConstructionSpec`: `Wrap.<init>` ω; `Ev.<init>` 1.

Old numeric method rows covered:

- core: `Meaning.phrase`, `Meaning.refusal`, `Card.literalSize`, `Count.render`, `Kind.render`
  (ω).
- cats-core: `Eval.Ident.<init>` (1).
- cats-kernel: `iterableOnceExtension.<init>`, `WrappedIndexedSeq.<init>` (ω).
- cats-eo: `reconstructSingleton` (0), `maybeHitTo` (ω), and two `MultiFocus` rows (0).
- fixtures: 22, the export rows and the evidence constructors (`Hidden`, `Carrier`) among them.

Every covered row is identical to the old count. One fixture row is tightened, from ≤2^33 to
2^33: `lib4.W2.<init>`, whose `Flag` is an exported `Int`.

### Group h7: known wrong numbers and inconsistencies

Five findings earlier units left, each fixed at its root with a regression in `CorrectionsSpec`
(both front ends asked for every row):

- **Rule B's observed fields read code beside the class.** A final class no constructor of the
  site builds counted only the fields its own body reads, so a companion's method, a parent's
  member or other module code reading a hidden field was missed (an undercount). The compiled
  `Factories.outside` follows the module's definitions upwards from the field (and what the field
  implements): a reader the site may reach and select makes it observed; a call of a member runs
  its overrides, so a parent's code calling a member the class implements is followed. The old
  front end's `FactorySyntax.readBy` does the same by name, climbing through the class's body, its
  companion and its parents' templates; a parent's template reads the field only where it declares
  a member of its name, and a reader outside the class only where it may select the field, selects
  its name and names the class. The stored side reads the companion. `Comp.getB` makes
  `out.Use.comp` 2^4 (was 2^2 in both). The old reading stays approximate by name: a public
  companion member mentioning a private helper's name (`m.get`) climbs as if it called it, and a
  selection of the name on another value that names the class counts (an overcount the compiled
  reading does not make); the old front end is the oracle being replaced, so this is recorded,
  not refined. A compiled index (`Factories.readers`) keeps the corpora's run time (core 21 s).
- **A member private to the instance is not selectable.** TASTy pickles an explicit
  `private[this]` member and a `private` one read on `this` alone both `PRIVATE LOCAL`, and the
  source's modifiers are not read here: `World.selectable` takes neither as selectable on another
  value, so a value observed only through one is refused (a capability record's "parameter that
  is no public field"). `final class PT(a: Boolean) { private[this] val x = a; def m(o: PT) }` was
  16, is `?` (should be 4); `PV` with `private val x` was 16 (right) and is `?` too; the old front
  end refuses both. `ProgramRowsSpec.thisOnly`'s `N.m` (a private `get`) is `?` now for the same
  reason.
- **An open or sealed concrete class nothing extends is final** (`ClosedProgram.finalClass`), as
  `Shapes` already read it: `open class OpJ extends Exception { def m(x: Boolean) }` counts 4 like
  `ClJ` (was refused for its Java parent). The old front end's `ClosedWorld` still leaves an `open`
  or `sealed` class open (unchanged: its receivers and records agree with each other).
- **An enum whose construction throws has no value on the stored side** (decision D5): the
  compiled `Stored.enumeration` reads `World.unbuilt` as `Sums` does (a value case through the
  companion and its anonymous class, all value cases together; a class case through its own
  template). The old `Counter.enumSize` reads the enum's body, its clause and each case's clause
  (not its companion or parents' bodies, which leaves `F` and `G` below corrections).
- **An override in classes never built does not vary** (D26). `Receivers.overridden` no longer
  takes a member as overridden where every override is in a class with no built instance
  (`World.idle`: some concrete class among it and its subclasses, each never built, the measured
  member's initializer aside). `trait DR { def m = true; def g(x: Boolean) = x }` with only
  `DRSub` (never built) overriding `m`: `DR.g` 4 (the old front end, 16, lets `m` vary). A record of
  a trait or abstract class with a never-built subclass is refused like a concrete one's (`DT`,
  `DA`: the old front end's 16 is an overcount).

Corrections (`corrected.txt`): fixtures `be82e558…` `G` 2 -> 0 and `F` 1 -> 0 (an enum whose value
cases are built by a companion that throws, or whose case's parent throws). Old front end rows
that changed: fixtures `E` 2 -> 0 (`enum E { case E1, E2; val x: Int = ??? }`, now identical);
no corpus row of either front end changed.

### Group k3: the rest of "a type", deferred inline members, two instances, systems over arguments

Each rule reads symbols and types; regressions in `ReceiversSpec`, `MatchTypesSpec`,
`PathTypesSpec` and `StoredRecursionSpec`.

- **A deferred inline member** (`Shapes.member`, the old `DeferredInline`). A body that is not
  inline cannot call it, so it supplies nothing; but the instance exists only where some class
  implements it. Its type is read as a capability's would be and dropped where it is built from
  nothing (the solver over no bindings, its atoms rigid, counts at least 1); otherwise it is
  refused ("a deferred inline member whose type may be empty": `inline def wish: A`). A deferred
  `inline val` an inline body reads stays refused. `Chooses.m` (`inline def choose[M](x: M, y: M):
  M` beside `m[B](a: B, b: B): B`) is 2; `Deferred.m` (`inline val flag: Boolean`) 2.
- **An abstract type member two enclosing instances hold** (`Shapes.holding`, `Env.held`). Each
  instance's `this.X` is its own atom (`Receivers.typeMember`'s identity, which names the class);
  the environment keeps every instance's atom beside the one map entry per symbol. A bounded such
  member stays refused: `Bounds` would read the proofs for one instance alone.
  `typeMember` moved from `Shapes` to `Receivers`.
- **Match types scalac reduces** (`PathTypes.trusted`, moved from `Shapes`). Beside a ground
  scrutinee, a reduction is trusted where the first case's pattern matches whatever the
  scrutinee's abstract parts are: `Any` (`case _`), a binder of the case met once, or a tuple of
  such patterns over a tuple of the same arity, nested (`((A, B), C) match { case ((f, s), t) =>
  f }` is A). A scrutinee that is itself such a match type is matched reduced
  (`Fst[Fst[((A, B), C)]]`). `IsInt[(A, A)]` (first case `Int`) stays stuck.
- **Paths** (`PathTypes.member`). An abstract member of kind `F[_]` of a method parameter's path,
  invariant and rigid as the path sees it, is the path's rigid constructor, applied as a type
  parameter of that kind (`p.F[Int]`). A capability record that holds no value may carry such a
  witness (`Receivers.record`: one value; with a value it stays refused, the solver renames atoms
  alone). A stuck projection of a path's atom is an atom (`Fst[p.X]`, members MT3 over M0).
- **A refinement naming its own value** (`T { type X = this.Y }`, a `RecType`, and a sibling
  `R[A, B]#X` for `X = Y; Y = A`): tasty-query leaves the refinement's `this` unsubstituted in the
  alias it gives a member selected on the refinement; `PathTypes.own` selects that member on what
  it was selected on (`p.Y`, `R[A, B]#Y`), which tasty-query resolves.
- **The stored side's match types over a class's parameter** (`Stored.Env.written`). A class's
  parameters, a family case's (`Sums.carried`) and the constructor's copies carry the argument
  types they were named with; a match over one is read at its argument with both sites' binders
  (one symbol bound at both refused), outside a system only (a system's classes are keyed by
  their arguments' sizes, which would conflate `Fst[(Unit, Boolean)]` with `Fst[(Boolean,
  Unit)]`; `R[A](g: Option[G[A]])` with `G[Int] = R[Int]` is refused there, not recursed into).
- **A class a system names over arguments that read the system's values** (`Stored.declared`) is
  read in place at them (`Ta(b: Option[Box[Ta]])`: T = 1 + T, ω), not keyed (the key would hold
  sizes the fixed point changes), where it is a concrete class none of whose fields holds a
  by-name or function type (`World.strict`: the ν completion reads those of a variable's own
  fields alone, so `Lb(b: Lbox[Lb])` over `Lbox[A](a: => A)`, the `Lz` knot, stays refused); one
  reached again so is refused (`Wt` over `W[A](a: A, w: Option[W[A]])`). The old reading counts
  `b.T`, `b.N`, `b.UseT` ω, as here.

Not taken, recorded for the next agent:

- Paths through an instance's `val` (`Open.this.p.X`, `In.this.p.Y`): sound for the instance's
  own members, but a record of the same class read inside it (`def use(o: Open)`) reads its
  members' `Open.this.p` too, which would be the instance's path, not `o`'s. Needs record
  readings to bind their own paths first.
- An abstract type member another declaration of its name hides (`C.pick`, a private `type X` in a
  parent): whether tasty-query resolves the name to the private member is not checked.
- Type lambdas of another body or order as keys (`T.other`, `T.swapped`), `Ctx.this.M.V`, and
  `x.type & alias.type`.

Rows (against `6156823`): 29 of the group's 38 old numeric uncovered rows are covered, each
identical to the old count; the 9 left are listed above. Fixtures: method rows identical 5,719 ->
5,741, uncovered 759 -> 736, old numeric uncovered 171 (190) -> 149 (162), past the old-unsound
list 136 (155) -> 114 (127); cats-eo's definition `pickFoldMiss` (1) is covered. One better row,
past a resolution limit: `q.V.andThen[C, F](using rc: RC[F]): rc.Out[S, C]` is 0 (the old front
end does not resolve `RC`): `rc` holds no value, `rc.Out` is the path's rigid constructor, and no
value of `rc.Out[S, C]` is in reach. No worse row; the corpora are unchanged otherwise. `compiled/`
grows by 126 lines (8,339 -> 8,465).

### Group k2: a capability with type parameters of its own

What h6 left. Three changes, the first two in the shared back end:

- **The instantiation closure has a budget** (`GoalSolver.instantiated`). Each polymorphic path read
  and each instance it gives is a step of the caller's budget (`Readings.Work`, 3,000,000 per row;
  past it the row reads "target budget exhausted"). An instance is labelled by a number its shape
  gets once per solver run, not by the shape's `toString`.
- **Only a path that can be called supplies an applied value** (`PolymorphicMembers.Supply`). PB5
  took the result of every monomorphic path as a candidate, even of one whose applied parameters no
  value of the context can fill. In `Cokleisli.andThen` (a `CoflatMap[F]`, no F value) `void`'s
  `F[Unit]` fixed `coflatten` at `Unit`, whose instance built `F[F[Unit]]`, and so on without end:
  that is the 40-minute row. Now the heads available are computed once per context (the least fixed
  point `opens` already used), and both the candidates and `opens` read the paths callable there.
  A path that an instance added later opens is refused rather than kept beside the instances it gave
  before. A context's supply is read once (`GoalSolver.supply`). The old front end gives the same
  rows, values and reasons, on all six corpora (compared row by row, old rows recomputed).
- **An inherited capability with type parameters of its own** (`Receivers.inheritedCapability`)
  is read as a declared one is: its `PolyType` is instantiated at its own type-parameter symbols, and
  `Capabilities.method` (moved out of `Shapes`, shared by both) binds them and closes the function
  (`PolymorphicMembers.close`). Bounds are read as declared, so a bound a subclass makes trivial still
  refuses; a member with several type-parameter clauses is uncovered.

Derivations (`derived.txt`, 15 rows of cats-core):

- `Comonad.extract` 0: F is not fixed, every capability of the instance returns an F or a function
  into one, and nothing takes an F apart.
- `Comonad[Tuple1]` … `Comonad[(A0, …, A9, *)]` (`catsStdInstancesForTuple1`–`11`),
  `Invariant.catsComonadForTuple2`, `catsInstancesForIdCompat2_6_1` (`Comonad[Id]`),
  `catsDataCoflatMapForWriterTId` ω: the goal record holds `Functor.imap`, concrete but overridden
  (`catsInstancesForId`, `WriterT`, `OptionT`), so it varies. Its result holds a B, and
  `f((g andThen f)^n(a))`, n ≥ 0, are pairwise distinct (f = _ + 1, g = identity at Int); every other
  member (extract, coflatMap, coflatten, void, map, lift) has at least one implementation and
  countably many. The rule of the derived row `catsInstancesForId.<new#1>.imap`.

Old numeric rows covered, each identical: cats-core `Category.id` (0),
`Profunctor.catsStrongForFunction1` (1), `KleisliInstances3.catsDataChoiceForKleisliId` (1); cats-eo
`ForgetfulApplicative.pure` (0); fixtures `UsesIds.pick`, `UsesAbsurd.pick`, `FL2O.other`,
`Instance.get` (two), `binders.ForgetfulApplicative.pure`, `Child.pick`. cats-core's new front end
runs in 15 s (24 s before).

Regression: `RecordsSpec` "an inherited member with type parameters of its own" (`get` 1, `noA` 0,
`run` 0, the last the endless closure).

### Group k1: corpus rows that are not record re-entry

The old numeric corpus rows `6156823` left uncovered for reasons other than re-entry through a
capability record (g1b) and a capability's own type parameters (k2). Each row was traced to what
the old front end does; five rules were ported, the rest are listed as resting on an old misreading
or left with their reason.

- **A written lambda over a symbol bound again** (`Shapes.bound`, `Env.applied`). A class's
  parameter passed back to the class (`FunctionK.or`'s `FunctionK[EitherK[F, H, *], G]` read inside
  `FunctionK`, `Kleisli.toReader`'s `Kleisli[Id, A, F[B]]`) binds one symbol at both sites. Where
  `Env.under` refuses, a written type lambda (an alias followed to one, `Id` among them) is read at
  the site that wrote it alone, each of its parameters standing for the argument read where it is
  applied (`of` and `key` look a `TypeParamRef` up in `Env.applied`): plain substitution, no
  symbol read at the wrong site. Any other written constructor is refused as before.
  `TypeParametersSpec.lambdas`: `Fk.or` 1, `Kl.plain` 1, `Wrap.Ops.back` 1 (was uncovered; the old
  front end agrees on all three). `Shapes.Env` moved to its own file (`ShapeEnv.scala`, aliased
  as `Shapes.Env`) to keep `Shapes.scala` under the 1,000-line limit.
- **A parent no classpath entry holds** (`ClosedProgram.indexed`, `absent`). A class without TASTy
  whose parent is on no classpath entry (corpus-core: 48 macro classes of scalameta extend
  `scala.reflect.api.TypeCreator`, scala-reflect not supplied) made every class outside the module
  "not all known", so every concrete member of a dependency was `Overriding.Unknown`. By the closed
  world the parent is outside the program and extends none of its classes: the class leads to no
  class through it, and the part stays "all known". What that parent declares is not read, so where
  the class does reach a class of the program beside it, it may override anything that class has
  (`Unnamed.whole`: `Overriding.Unknown`, an `overriders` entry not read, `mixes` true), as a named
  class's unread parent beside its owner already is (`unreadBeside`). Decision recorded here
  (values: the AGENTS.md closed world). A name is absent only where neither a class nor an object of
  its top-level name is found (`MemberNotFoundException`); any other failed lookup, and a parent on
  the classpath that does not resolve, leaves the whole part unknown as before. A correctness
  review noted the asymmetry this leaves: an absent parent that would itself extend a class of
  the program (`Gone extends D` in a jar not supplied) is taken to extend none, so `D`'s
  subclasses read as all known; a named class with an unread parent still makes the part unknown.
  That is the decision above (a class outside the supplied program does not exist). Coordinator's
  ruling (integration, round 3): the decision stands. By AGENTS.md the supplied sources and
  dependency declarations are the whole program, so a class in a jar not supplied does not exist
  and cannot extend a program class.
- **A member class of an object without a companion class** (`ClosedProgram.named`): a binary
  parent `scala/sys/process/BasicIO$Uncloseable` starts at the object's module class when no class
  of the top-level name exists (two anonymous classes of the library extend it).
- **A Scala 2 specialization** (`ClosedProgram.Specialized`). `AbstractFunction1$mcZI$sp` is a
  class file no symbol stands for (tasty-query does not list specialized classes): cats-core's
  classpath had two anonymous classes of `scala.sys.process` extending it. It is read as extending
  its general class alone and as overriding, by name, every member that class has (an overcount
  of what may vary, never an undercount). `named` also tries a binary name as a top-level class.
  `ClosedProgramSpec.java` (`Stray$1` extends `javamissing.Gone`, left off the classpath; `Gen$1`
  extends `Gen$mcI$sp`, `Stray$1Local` implements `Dial` beside the absent parent, so `Dial.same`
  is unknown) fails on the old code.
- **A reference resolved only where its written name matches** (`Bodies.throws`, `written`): once
  the closed world above was complete, `World.unbuiltBelow` read the bodies of more classes, and
  resolving every reference reached a signature tasty-query does not read (`Definitions.PureClass`
  of the 3.3.4 library, `None.get`), which failed whole sources. `???` and `sys.error` are matched
  by name before their symbol is resolved (`Reentry.written`, which `Reentry` already had for the
  same reading, now shared).
- **A parent reached twice with the same type arguments** (`Receivers.reachesTwice`): one parent,
  as the old `InheritedScopes.walk` takes it (it refuses only a parent "instantiated twice"). The
  base type through each parent (`Type.baseType`) is compared; one that differs (`CoTop[Boolean]`
  beside `CoTop[Any]`), and a parent that does not read, is refused as before, the reason now
  saying "with other type arguments". `RecordsSpec`: `dia` 16, `coDia` refused;
  `ProgramRowsSpec.twoPaths`: `D.g` and `E.h` 4 (were uncovered), `CS.s` refused.
- **A match type over a class's parameter on the stored side** (`Stored.Env.written`,
  `matchType`, `atClass`, `Shapes.copied` now generic): the type a position wrote for the
  parameter, carried to the constructor's copies, is the scrutinee; the match is read with the
  parameters of both sites, as `Shapes` reads it (`Env.under`), and not where one symbol is bound
  at both. `MatchTypesSpec.stored`: `val missed: Miss[(Unit, Boolean)]` and `flagged` 1 (D30),
  `HoldsUnit` 1, `HoldsFlag` 2, `HoldsPick` 2 (its field reduces to the class's other parameter,
  bound to `Boolean`). cats-eo `pickFoldMiss` (old 1) is identical.

Rows listed in `old-unsound.txt` (each old number rests on a misreading a correction already
records): `cats.evidence.As` (definition, old 0) and `As.coerce` (old 1), which read As as a
sealed family without a value (the correction of `As.reflAny`); `IorInstances.catsDataOrderForIor`
(old 1), which reads Ior as empty (the correction of `cats.data.Ior`, as `catsDataShowForIor`).

Left, each with its reason:

- `Traversal.selfChildren.<new#1>.from` (cats-eo, old 0, which is right: `MultiFocusK` is opaque
  outside `data`, one value by rule F, and no `S` is reachable): reading `PSVec` needs a sealed
  case no constructor of the site builds read through `Factories`, which g2 left out ("Rule B,
  third unit"). Ported and reverted here for the same reason.
- `ClosedWorld.extending` (core, old 1): `Memo[String, (Set[String], List[Node])]` holds no field
  the site observes, but `Shapes.named` reads every type argument first, and `Node` is a class
  nested in a class. Reading arguments only where the class's value uses them would change
  `declared`'s signature through `Sums` and `record`; not done.
- `Hash.hash` (cats-kernel, old 2^128): re-entry among the members of an enclosing class
  (`Receivers.reenters`), shared with g1b's record rows.

### Group k4: variance of sealed cases, one-valued goals, diamonds, parent fields

What it reads, each with a regression:

- **An invariant case parameter in a variant slot** (`Sums.carried`, `keepsPolarity`, `polar`;
  coordinator-approved, from h5's untested `/tmp/k4/variance.py`). `C[A] extends P[+A]` holds
  `C[S]` in `P[T]` for every `S <: T` (above `T` for a contravariant slot). Where every
  constructor parameter of a concrete `C` mentions `A` only at the slot's polarity (an argument of
  a class's covariant slot keeps it, a contravariant one flips it, an invariant one or any other
  form may not mention `A`; aliases expanded first), each such `C[S]` is a `C[T]`, so the union
  is `C[T]`'s values. Refused: a field the other way (`A => Boolean` beside a covariant slot), an
  alias whose arguments are not the class's in order, an invariant sealed level below (whose cases
  may take `A` negatively), `A` under an invariant slot or inside a type lambda. A bounded case
  parameter is read where its bounds are the slot's (or name the parameter another slot carries,
  `D[A <: B, B] extends Q[A, B]`): every `T` the parent admits the case admits. Both sides (the
  stored side calls `carried` too). Known limit: a class declared covariant through
  `@uncheckedVariance` is trusted, as the same-variance path already trusts declared variance.
  `TastyFrontEndSpec`'s pin `Inv[A](a: A) extends Co[A]` is reversed: `inv[A](c: Co[A]): Boolean`
  is 2 (the constant functions; `Co[A]` holds `Inv[A]`, an atom).
- **A sealed class written without `()`** (`class O extends L`) is one value, as `class O()` is:
  no field. The old reading refuses it in some places (`types.Use.bare`, derived 1) and reads it in
  others (`g.U.<init>` 2).
- **A one-valued goal whatever the reading leaves** (`Readings.singleGoal`): a member row whose
  result has one value is 1 when `measured` leaves it for any reason, as `counted` reads a
  terminal goal first. Read only for a result that names no type parameter of the member or a
  definition around it and no type member of a class around it or its ancestors (the receivers
  would give those; reading cats' `compose` without that overflowed the stack). Constructors, the
  rows `Readings.later` leaves and rule X keep their reason. Frees the three self-type rows `T.g`.
- **A diamond over a parent without type parameters** (`Receivers.reachesTwice`): only a generic
  ancestor reached through two parents is left (each path may give its own arguments).
- **A captured parameter a parent call passes to a field** (`Readings.passes`, `seen`): the field's
  type is read as the call sees it (`typeAsSeenFrom` the parent type, through an alias), so
  `new F[A](a1)`'s `f: X` holds the outer `a1` (kept, 3), and a field typed `Any`, written or as
  `PT[Any]`'s `X`, shows nothing of the capture (dropped, 4), as an alias at a one-valued type.
- **`World.mentioned` walks type lambdas and match types** (it answered "no mention" for them,
  which the variance check, among others, reads as safe).

Derived (`derived.txt`): the `Ev`/`Od` system of the two `8977c616…`/`8b0047a9…` fixtures, ω
each (`Ev = 1 + Ev` through `Even(Odd(…))`, countably many); `f.P.<init>` ω (`List[Boolean]` ×
(`A` + 1)); `P.iterator` ω, `P.knownSize` 2^32, `P.g` ω (the receiver `P` is its one fieldless
case `D1`); `types.Use.bare` 1.

`old-unsound.txt` gains 24 fixture rows of two more classes, under the coordinator's decision:
"constructor row: stored estimate for a non-recursive position" (17: the old reading takes the
estimate for a position the method side refuses, where the class does not reach the position's
type; several are overcounts by rule B, `Outer.Inner.<init>` 2 where `Outer` shows no field,
`UsesSorted` 8 where the `Ord` evidence is observed by no member) and "secondary constructor body:
the first this(...) call fixes the fields" (7: `S.<init#1>.helper` 4, `K.<init#1>.pick` 4, and
so on, where the call fixes `a`). Not listed, back to the work list because the position is
recursive: `A11`/`B11.<init>` (old 1), `A`/`B.<init>` (old ω, which rule B says is 2 and 2^32),
`Node`, `One`/`Two` (×2), `Go`, `One` (`d84b…`): the old reading takes the estimate for a recursive
position the method side refuses for a hidden parameter.

Not taken: "a parent with constructor parameters or in a class" (12, path-dependent inner traits
and local parents), "an enclosing class with a constructor field something overrides" (2: inside
`P` both the parameter and the overriding getter may be read, two values, so the old `P.m` 16 may
be an undercount), "a restricted constructor" (3: rule B part 2 for sealed cases, h5's note), the
generic diamond `D1.pick`.

Old numeric rows still uncovered (methods; definitions), integration → k4: fixtures 171 → 151;
19 → 14, and past the old-unsound list 136 → 92; the corpora unchanged (core 3, cats-core 25,
cats-kernel 76, cats-eo 3). Method rows, fixtures: identical 5,738, better 279, tightened 11
(old upper bounds now exact), worse 5 (listed corrections), uncovered 729.

### Group m4: unread arguments, classes nested in a class, parents in a body, Q10

What it reads, each with a regression:

- **An argument a class's value does not read** (`Shapes.named`). A class of the module (no enum,
  no sealed family) whose type argument does not read is read with the one-valued placeholder in
  that argument's place and its reason in `Env.unread`, which `binding` prefers wherever the
  value reads the parameter (`record` and `fields` copy it to the constructor's copies, as they do
  `written`); a refusal keeps the argument's reason. `TypeParametersSpec.unused`: `Graph.cache:
  Cache[String, List[Node]]` 1 (`Node` a class nested in a class), `Graph.box: Box1[Node]` keeps
  the reason. Frees core `ClosedWorld.extending` (old 1, identical) and ten `Memo[K, V]` vals the
  old front end refused (derived 1 each: `Memo`'s one public constructor takes nothing, so by rule B
  it builds one value whatever K and V are). Restricted to classes of the module: a dependency's
  record over an unread argument (scalameta's `Parsed[Source]`, read as 1) was not checked.
- **A class nested in a class** (`Shapes.declared`). One whose members are types alone is read
  anywhere (one value); a plain case class where no field's type names a symbol of a class around
  it or that class's `this` (`World.mentioned` now walks a `ThisType` and a `SuperType`: a member
  inherited or from a self type, reached as `Outer.this.T`, means another thing on another
  instance; no other caller's test accepts a class, so no other reading moves). The correctness
  review's program `Outer2.pick(o)(i: o.In): A` with `In(t: T)`, `T` inherited, was 4 (right: 1)
  before the `ThisType` walk; refused now. `ReceiversSpec.inner`. Coverage memory: cats-eo-generics
  `HearthPlateMacro.fieldOps` went from a measured `?` (quotes) to uncovered (`FieldOps` names
  `quotes.reflect` through the instance); both print `?`, old not numeric; its line was deleted.
- **A parent written in a body around the template** (`Receivers.inherits`, `instanceFree`): a
  local parent whose method owners all enclose the template, objects and packages around both
  otherwise, holds what that body captures, which the template sees too; it goes through the same
  field checks as a parent in objects. A parent in a class's method still refuses.
  `ReceiversSpec.bodyParent`.
- **Q10**: the 14 fixture rows "a member the classpath does not hold" each name a declaration whose
  support source is compiled but left off `classpath.txt` (`ext.Mixin`, `lib.Base`,
  `far.away.Unsupplied`, `missing`, `ext.Schema`): listed in `old-unsound.txt`.

Old numeric rows still uncovered (methods), integration -> m4: core 3 -> 2 (2 past old-unsound);
fixtures 116 -> 106, past the old-unsound list 57 -> 39 (14 of them Q10, 6 on the definitions
table: 8 -> 2 past the list); the other corpora unchanged. Method rows, fixtures: identical 5,773
-> 5,784, better 285 -> 287, worse 5 -> 6 (the listed correction), uncovered 688 -> 674; core:
identical 2,885 -> 2,889, better 19 -> 29 (the ten derived `Memo` vals), uncovered 1,096 -> 1,082.

Corrections and derivations: `Outer.Inner.Use.<init>` 1 -> 2 (`Item` in `Use` is the nearer
`Inner.Item`, 2 values; the old reading took Outer's type parameter); derived `C.viaThis` 1 (an
empty nested class, one value), `mk.D.other` 3 (`c`, `a`, `b`; `Mid.to` returns `a`).

Left, each with its reason:

- cats-eo `Traversal.selfChildren.<new#1>.from` (old 0, right): needs a sealed case no
  constructor of the site builds read through `Factories`; the holes of "Rule B, third unit" (a
  parent member matching on the case, a companion's method, a default getter, another class's
  secondary constructor or parent argument, an extractor the site cannot reach) are not closed, so
  it stays `?`.
- `K.Q.Outer.<init>`, `K2.Q.Outer.<init>` (a field over the outer class's type parameter):
  reading them needs the prefix compared with the site's instance.
- "a parent with constructor parameters or in a class" for parents in a class (`Outer.Instance`,
  `T__O.H__O`, `Outer2.B`, `OuterG2.B`, `O.C` ×3, `Outer.InnerM`): the parent holds the outer
  instance, reached through the template's own prefix; not read.
- "a capability record with a parameter that is no public field" (`A11`/`B11`, `A`/`B`): a
  parameter no member reads should drop out of the record (rule B gives 1, 1, 2, 2^32), which
  needs `Receivers.own` to skip it for a value; not done.
- "a type", "an abstract type member another declaration of its name bounds or hides", "an
  enclosing class with a constructor field something overrides", "a capability record whose
  parents are not read", "an anonymous contextual parameter", "a class of a dependency"
  (`LazyList`), `out.Use.f` and the two `Base` definitions (restricted constructors): not reached.

### How each model rule reads over symbols

Done in the first slice where marked; the rest is the plan for its slice.

| Rule | Over symbols and typed trees |
| --- | --- |
| Construction (D26) | A template is never built when its constructor always throws. The constructor evaluates each strict value's initializer and each statement of the template, and for a class its parents' arguments and its ancestors' constructors. A term throws when it is a `Throw`, a reference to `scala.Predef.???`, an application of `scala.sys.error`, a block whose result throws, or a conditional or match all of whose branches throw. By symbol, so the "construction not resolved" refusals of a name a wildcard may rebind go. Two counts move, each derived: `import scala.sys.error; error("no")` throws (D3: the old rule reads the spelling and says 4 where this says 1); a value of a context function type initialized with `???` does not throw, since the tree's initializer is the function literal the compiler made of it (the old rule says 1 where this says 4). Done for objects without parents and without a pattern definition (Q5). |
| Closed world: finality | A class is final when no class of the world has it as a parent. A member `m` of `A` is open when some class of the world with `A` in its linearization resolves `m` to another symbol: an override of its own, or one a mixed-in parent brings (`allOverriddenSymbols` alone misses the second, since it walks the overrider's linearization only). The world is every class the module and its dependencies hold, the module's local and anonymous classes included (found in trees, not in declarations). Symbols are exact, so "templates are matched by simple name" and "a parent outside the sources may define anything" go; what stays is a class the classpath does not hold. An old count that rested on two same-named templates sharing subclasses may change: the contract already calls that reading an overcount. |
| Capabilities | Abstract members are the symbols with `isAbstractMember` in the receiver's linearization (a symbol read from a class file has no tree to look for a body in); overridden ones come from the same index. "The measured declaration is not its own capability" is symbol identity and `allOverriddenSymbols`, replacing `OverrideIdentity`. |
| Rule B | A constructor is public at a site by its `visibility`: `Public`; `ScopedPrivate` whose scope is among the site's owners; `Private` or `Protected` where the site is inside the class, `Private` also inside its companion. A `protected[p]` one is ignored, as the contract says. Secondary constructors are `<init>` methods. A factory is a method or value whose result tree, through blocks, branches and local values, is a `new C` or a case class's own `apply` (the synthetic one); a call of any other method, a hand-written `apply` included, is a delegation level, two at most. Observed fields are those the site may select and those a member it may call selects in its tree; none observed is one value. Needs bodies (5d). |
| Rule F | A `TypeRef` whose symbol is an opaque alias is one value, unless the site is inside the alias's owner, where its `typeDef` alias is read. tasty-query's `dealias` keeps opaque types. It follows a member alias's own definition, not the prefix it is seen through: `Box[Boolean]#Elem` gives `Box`'s parameter, so S1 must substitute the prefix's arguments itself. |
| Rule X | `isTransparentInline` and a declared result that is not a singleton type refuse. |
| Rule T | Every type is the symbol's. The rule's matching (name offset, name, span) becomes the stale-classes check (Q7). |
| Top types, standard model | Unchanged and in the back end (`TopTypes.polarized`, `StdlibModel`). The front end asks `BuiltinTypes.known` by the fully qualified name of a top-level class under `scala` or `java` that the module does not compile itself (done). |
| Inline methods | Compile-time operations are tree nodes (`InlineIf`, `InlineMatch`, `Quote`, `Splice`), the `Macro` flag and symbols of `scala.compiletime`. A Scala 3 dependency's inline body is in its TASTy: `LibraryInlines` goes. |
| D2 | The conversions a body applies are `Apply` nodes the typer wrote; only those are read. |
| D3 | Every reference carries its symbol. `ShadowedBuiltins`, `ImportShadowing` and the lookup by name are not ported. |
| D4 | A given the compiler resolved is an argument in the tree; it refuses only when it is inline with a compile-time operation. |

### Refined slice order

What the uncovered rows need, by each row's first blocker, in the gate's own strings (corpus /
fixtures), with the slice that removes it. The twelve largest: `an enclosing class` 1,912 / 1,613
(S4); `a class of the module` 590 / 455 (S1); `a class of a dependency` 424 / 45 (S7); `a
constructor` 444 / 1,233 (S3); `an abstract class or trait` 11 / 1,004 (S2); `a type alias` 29 / 457
(S2); `an enclosing object with parents` 176 / 61 (S4); `an enclosing definition` 113 / 139 (S4);
`a class with type parameters` 3 / 312 (S2); `an inline definition` 0 / 271 (S6); `a bounded or
higher-kinded type parameter` 3 / 163 (S1); `a contextual definition` 31 / 134 (S5). The other 420 /
752 rows need `a type` (S1), `a class with parents` or `a field of type …` (S2), and smaller ones.
A count is an upper bound on what its slice gains: a row has a next blocker.

The slices are the plan's 5a to 5e, with 5c in three steps, in this order:

| Slice | Beyond the phase 5 table |
| --- | --- |
| S1 (5a) | Types of the module's sources first: records, sealed sums, enums, objects, aliases, opaque types, class type parameters and their bounds, unions, singletons, type members. Landed: products, sums, enums, aliases, opaque types ("After S1" above); records go with S4, the other forms are S1's second step. |
| S2 (5b) | Recursion through the existing `capacity.Solver`. Restricted constructors wait for S6 (rule B's factories). |
| S3 (5c) | Constructor rows: positions as value spaces, type-parameter slots to the shape model. |
| S4 (5c) | Class receivers and objects with parents: fields by rule B's public primary, abstract members as capabilities, the closed-world index, inherited declarations, enclosing parameters, local definitions. |
| S5 (5c) | Contextual scope: givens, `using` and implicit clauses, context bounds, context functions, extension methods. |
| S6 (5d) | Construction (D26) for classes, after it is restated for desugared values (Q5). The corpus hardly exercises these rules: their gate is the fixtures. |
| S7 (5e) | As planned. |
| S8 (6) | `Report.query` over the new rows; the specs' harness; delete the scalameta front end, the stubs and `CompiledTypes`. |

A slice lands when `scripts/differential.sh` passes on every input (so every better row past a
model limit is derived), one sample per reason of the better rows past a resolution limit is
derived, and the uncovered count has fallen by what the slice claims.

### Size estimate

| | Old, today | New, estimated | Why |
| --- | --- | --- | --- |
| Row finding and naming, source matching | `SourceIndex`, `BodyScopes`, `Walk`'s naming, `CompiledTypes`: about 1,300 | 700 (`Targets`, `Source`, `TastyFrontEnd`: 668 written) | measured |
| Types to shapes (5a) | `resolution` 8,950, `types` 2,264 | 2,000 to 2,800 | Name lookup, imports, shadowing and inheritance walks (about 4,500) are the compiler's. The model stays: records, sums, type members, unions, opaque scopes, evidence. |
| Definitions (5b) | `capacity` 6,453 less `Size` and `Definition` | 1,200 to 1,600, and `Solver` (579) kept | Namespaces, imports and package members (about 1,700) go. |
| Environments (5c) | `analysis` 5,583, `capability` 2,517, `model` 1,530 | 3,000 to 3,800 | Override identity, given names, term bindings become symbol queries; capability records and target views are model logic and stay. |
| Bodies (5d) | `expansion` 3,747 | 800 to 1,200 | Trees are typed: `BodyTypes`, `LibraryInlines` and candidate search go. |
| Dependencies (5e) | `tasty` 1,506 | 150 to 300 | A dependency class is a symbol like any other. |
| **Front end** | **about 30,000** (the first row's files are inside the others) | **7,850 to 10,400** | |

The standard model's table (583 lines), `BuiltinTypes` and the shape half of `TopTypes` are kept
with the back end. The first slice is the evidence for the ratios: the construction rule
(`Construction`, `Throws`, `TermBindings`: about 280 lines) is 45 lines over symbols for objects,
and the standard-type reading 50 lines against the name lookup it replaces. 5c has the widest range
and no code yet.

### Risks

- **The closed world over dependencies** needs every class of the module and its dependencies (see
  the rule table). Loading every dependency class's parents and members has a cost not measured yet;
  the stubs render about 5,200 sources for the same reason. Measure in S4.
- **Tests.** The 72 cached fixtures that do not compile have no rows in the new front end ("Test
  fixtures that do not compile" above lists them). `TastyFrontEndSpec.compiled` duplicates
  `cardinality.Compiled` (`core/src/test/scala/cardinality/Compiled.scala`), which should also
  record the classpath a fixture compiled against: merge them.
- **The gate is run by hand.** `TastyFrontEndSpec` runs in CI; `scripts/differential.sh` does not,
  and the corpora are on one machine ("Gate assets" above says how each is built). The new main code is
  exercised by that one spec: check the coverage floor (`coverageMinimumStmtTotal := 80` in
  `build.sbt`, `sbt coverageAll`) once in each stretch.

### Decisions on the open questions (coordinator, on the owner's behalf, 2026-11-11)

Decided on the owner's values: better or equal measurement, then less source, then speed.

| | Question | Decided | State |
| --- | --- | --- | --- |
| Q1 | Scope names the compiler desugars (a `for`, an untupled parameter, a polymorphic function literal) | Keep the tree's names. List the renamed rows at the switch-over. | The gate pairs them by position. |
| Q2 | `given g: T = deferred` | A declaration row. | Done; the rows pair across kinds. |
| Q3 | Signatures of inferred members | Printed as written. | Done. |
| Q4 | Top-level pattern and multi-name definitions | One definition row per bound name, as the tree gives them and as method rows are. No merging. | Done. No input holds one. The first that does fails the gate with unpaired rows, and nothing in the gate accepts them: it then needs a list of rows renamed by decision. |
| Q5 | Construction (D26) on desugared values | Restated over the tree's values, one definition's values excluded together. The tree is what the program evaluates: where the old rule disagrees, the old number is the wrong one, and each such row is a derived better row or a derived correction. | With S6. Until then a member beside such a definition is uncovered. |
| Q6 | Budgets | The solver's own only, depth 64 always. Then check whether `analysis/DeepStack` is still needed with the depth always bounded, and delete it if not. | Depth 64 done in `Readings`. `DeepStack` belongs to the old `MethodAnalysis.measure`: check at the switch-over. |
| Q7 | Stale classes | Accept the partial check: the plugins compile before they report. No more code. | Done. |
| Q8 | A source that defines nothing | Empty when its text holds no definition keyword, an error otherwise. | Done (`TastyFrontEnd.Defining`). |
| Q9 | Must refusal reasons match? | No. A refusal stays a refusal or becomes a derived number. List the reasons that changed at the switch-over. | The gate compares counts. |
| Q10 | A member the classpath does not hold | A refusal of its own, once fixtures record their classpath. | Uncovered until then. |
| Q11 | `Report.query` | A filter over the new rows. | With S8. |

### Group m3: lazy cycles at depth, two recorded gaps, recursive constructor rows by rule B

**Lazy cycles at depth (both front ends, `capacity/Fixpoint.scala`).** The ν completion saw a
lazy field only at its top, so `final case class Ltd(p: (Boolean, () => Ltd))` stored 0 in both
front ends (a wrong number: `lazy val l: Ltd = Ltd((true, () => l))`, an endless Boolean stream,
ω). Each front end now splits an arm's other fields into the nodes they demand (`Arm.names`) and
those they name only under a by-name type or a function's result at any depth (`Arm.deferred`;
`Solver.mentions` over the syntax, the body's bare aliases expanded; `Stored.mentions` over the
TASTy type, a form it does not take apart naming its classes both ways). `Fixpoint` derives the
lazily-named map from the arms (`Lazy.lazily`, `Solver.lazyTargets` and `Stored.lazyNames` are
gone) and completes:

- an inhabitance fixed point: a node holds a value when one is built in finitely many strict
  steps around lazy knots (`Kx(kids: List[Kx], f: () => Kx)`: `Kx(Nil, () => k)`), or μ is
  nonzero; a node with none keeps μ = 0 (`K9(k: K9, m: => K9)`), and one read as empty only
  because a field names a node both strictly and lazily (`Ks(p: (Ks, () => Ks))`) leaves its
  cycle open (`lazy cycle not completed`);
- cycles: strongly-connected groups, with a lazy edge, of every edge (strict, lazy, a sealed
  parent's children) between the nodes that hold a value, read without the arms that hold none
  and the names under an empty factor (`o: Option[(Nothing, Y)]`);
- per lap, each field read with the cycle as one value (the label space: a deterministic cycle
  raises it to ω, a branch saturates at ω, or ε₀ where the labels are infinite) and as two (a
  field that does not double branches); a definition between nodes (the source front end's
  alias) is read again at those values.

Rows: no corpus row moves on either side. Fixtures, both front ends alike: `Ltd` 0 -> ω, `Lu`
0 -> 1, `Lw` 0 -> ω, `Pair` 0 -> ω, `Kx` 0 -> ε₀, `H9(m: => H9, s: Set[H9])` 0 -> ω (`new H9(h,
Set())`), `T`/`A`/`B` (`A(t: () => T)`, `B(b: Boolean, a: A)`) 1/1/2 -> ω (a wrong number: `T`
was below its summand `B`), `Lst(p: List[() => Lst])` ω -> ε₀ (ω arities per lap), and the
strict-field cycles `S`/`Cell`/`Thunk`/`In`/`Out` `?` -> ω. Pins: `StoredRecursionSpec`
(`nestedLazyCycles`, `emptyParts`, `uncompleted`), `CardinalitySpec`, `NamespaceSpec.lazyCycle`,
`NamespaceScopingSpec.lazyAlias`. Left, both front ends, older than this unit: a sealed parent in a
cycle ignores a child outside it that holds only a ν value (`sealed trait Pt; Pa(t: => Pt); Pb(h:
Boolean, t: => Pb)`: `Pt` 1 where Pa(=> pb) for each Pb stream gives ω); the source front end does
not expand an applied alias (`type L[A] = () => A`) in a field.

**The recorded gaps.** `World.deferred` behind a match type: `Lm3(p: F[Lm3])` with `type F[X] = X
match { case Any => () => X }`, and `Lm`, `Lm2`, `Lg` beside it, stored 0 in the new front end (a
wrong number; 1 and ω). `Stored.mentions` names a match type's classes both ways, so the cycle is
refused. A wildcard bound is refused already (`a wildcard type argument`). `World.mentioned` now
walks a `RecType`'s parent; no probe found a reader that counts through one.

**Recursive constructor rows with a hidden parameter (coordinator: rule B).** A recursive position
(`Stored.recursive`: the position's system reaches the class) that the method side refuses for a
parameter a class does not show (`Sums.Hides`, `Shapes.NoField`) is the stored estimate read for
the constructor as observer (`Env.observer`): each class holds the fields `Factories.observed`
says the constructor observes, through the system's equations and the ν arms. Rows (old -> new):
`A11`/`B11.<init>` 1, `One`/`Two.<init>` ω (both fixtures: `Two(i: Int => E)` is a case class, its
field observed, E = 1 + E^(2^32)), `Go.<init>` 1, `One.<init>` (`d84b…`) 1, identical; corrected:
`Node.<init>` ω -> 2, `A.<init>` ω -> 2, `B.<init>` ω -> ≤2^32, and `O.Cell.<init>`, `Cell2.<init>`
ω -> 2 (`S(val head: Boolean, tail0: => O.Cell)` shows its head). `UsesCell.<init>` is a new old
numeric row still uncovered: the old front end now counts `Cell` (ω), and its position does not
reach `UsesCell`.

**Not done: a construction that always throws on the stored side** (the coordinator's extra item).
It reproduces (`final class Dead { val z: Int = ???; final case class Leaf(b: Boolean) }` stores 1
and 2 on both sides). Emptying it in the new front end (`World.unbuilt`, the classes around) is a
few lines, but the old front end's `Builds` reads `???` by spelling and overridden vals not at
all, so the two disagree on the D26 fixtures (`K` with `import Helpers.???` would be 0 there, a
wrong number), and emptying the new side alone turns about thirty fixture rows worse, each to list
as a correction, with `RecordsSpec`/`ReceiversSpec` agreement pins to adjust. Left for a unit of its
own.
