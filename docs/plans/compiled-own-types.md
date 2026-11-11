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

### In progress (gate agent, stretch 4: a row-level home for each rule only scalameta tests pin)

A note for whoever continues from the files alone. Stretch 3 is committed whole (`a584ae1`,
`baec0ca`, `0fd209d`: "Test fixtures" and "Specs against the compiled front end" below).

Stretch 4 gives each rule of the model that only class (b) examples pin an example on rows, which
passes on the scalameta front end today and joins the ledger. Decided, not to be re-homed: the
member-signature reading (deleted with the scalameta front end), the index, lookup, alias and
projection budgets (they will not exist), stack independence and the refusal for a class the
classpath does not hold (the front-end agent's). Units, each committed after
`sbt scalafmtAll scalafixAll`, `sbt core/testFull`, `bash scripts/check-file-metrics.sh`, CPD and
three reviews of its staged diff:

1. Open-world rules as closed-world rows. Committed: the 53 examples of `TargetViewsSpec`,
   `ReentrySpec` and `PureCodeSpec`'s open rows read the closed world with the overrider supplied
   (`cardinality.Overriders`; "Rules of class (b), on rows" below).
2. Dependency rules as rows over a dependency compiled apart and given to the analysis as classes.
   Committed: `Compiled.dependency` and `Compiled.reportOver` ("Test fixtures" below) and
   `DependencyRowsSpec` ("Rules of class (b), on rows").
3. The fixtures whose first failure hides assertions on code that compiles. Not started.
4. Ledger rerun, totals and the classification table. Not started.

### Commands

| Command | What it does |
| --- | --- |
| `scripts/measure-corpus.sh <corpus> <report file> [runs]` | One `Report.of` per run over a frozen corpus with this checkout's build, each in a JVM of its own (`bench/runMain cardinality.bench.CorpusRun`, `-Xmx16g -Xss8m`). Only the call is timed; it includes rendering the dependency stubs. Prints each run's wall time, CPU time, GC time, peak heap and peak RSS, the median and range of the first, second and fourth, the rows by kind, and what the report shows of the compiled types; writes the rendered report; fails when two runs render different reports. |
| `scala-cli run .claude/skills/cardinality-reduction/report-diff.scala -- <before> <after> --rows` | Totals of two rendered reports and every row that moved. |
| `scripts/build-corpus.sh <new corpus directory> <organization:artifact_3:version>` | A corpus from a published artifact: its binary jar unpacked as `classes/`, each source of its sources jar at the path its TASTy records, and its dependency jars in `classpath.txt`. Nothing is compiled. |
| `rm -rf core/target/compiled-fixtures && sbt core/testFull` | Compiles every test fixture afresh; the entries are then corpora a differential runner can read, and `ls core/target/compiled-fixtures/*/failed` are the rejected ones. |
| `scripts/spec-ledger.sh <ledger file> [work directory]` | Runs `core/testFull` under each mode of the harness's front-end switch and writes, for each example that reads rows, whether it passes with the compiled front end's rows, waits for rows not read yet, or fails ("Specs against the compiled front end" below). The ledger at this commit is `docs/baselines/tasty-spec-ledger.tsv`. |
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
| now (the ill-typed rewritten) | 1,977 | 1,906 | 71 |

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
  it cannot drift. It holds 70: 61 not valid Scala by design (the analyzer must refuse what scalac
  rejects), 5 unsupplied on purpose (3 whose point is a source that does not compile, 2 that
  scalac rejects with the declaration supplied too), 3 naming a declaration given only as a
  dependency stub, and `ContextBindersSpec.limits`, which the compiler does not finish in 60 s.
  The 71st rejected fixture is the one `CompiledSpec` leaves unlisted on purpose. A line names a
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
`OpaqueSumForwardingSpec` 1, `MeasuringSpec` 4, `DeepStackSpec` 1. The 12 rejected are listed:
five by-name enum-case parameters in `CardinalitySpec` (scalac takes no by-name `val`; a case
class's lazy field is respelled as a plain class's parameter, an enum case's has no spelling),
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
| PASS | 188 | Passes with `TastyFrontEnd`'s rows, and still passes when every row not read yet holds a count no example expects. |
| UNCOVERED | 1,252 | 1,233 fail with `TastyFrontEnd`'s rows and pass once each row not read yet holds what the scalameta pipeline gives the row of its name: they fail for those counts and nothing else. 19 pass, and fail under the sentinel: what they expect, a row not read yet meets too (a refusal, or a total the row is left out of). |
| FAIL | 108 | Fails with the counts not read yet answered too. The cause column is that failure. |
| PENDING | 19 | A target `ArticleCardinalitySpec` marks as not met yet, under either front end. |
| SCALAMETA | 133 | Reads what only the scalameta front end has: its tree, the open world, dependency stubs given as sources. |

The other 316 examples read nothing through the harness. What the ledger cannot see:

- An example passes whatever the sentinel is where it only says what a row is not, or where a
  row without a count meets its expectation too. Two are known, and read PASS on rows not read
  yet: `AliasOverrideIdentitySpec`, "keeps a nearer source alias distinct from an outer binder"
  (`c !== Count.Finite(1)`), and `ArticleCardinalitySpec`, "a data type with no constructors" (a
  total of 0, which no row adds to).
- A row is not read yet where every reason it gives starts with `TastyFrontEnd.Uncovered`, and
  the front end files a type that names a class the classpath does not hold under it (19 of the
  UNCOVERED causes). Where a fixture withholds the class, that is the final answer, not a slice
  to come: those examples pin `declaration not supplied` and stay UNCOVERED until the front end
  refuses in its own words.
- UNCOVERED says nothing of what the count will be once it is read, and a PENDING example stays
  PENDING whatever number the compiled front end gives: only its cause changes.
- The cause is an example's first failure. One that reads two fixtures shows one of them, and
  one marked SCALAMETA may assert rows as well.

The rows are the compiled front end's: under `probe` the 1,567 examples of class (a) below fail
at the request; under `tasty` the one method that answers a request calls `TastyFrontEnd.rows`
and nothing of the scalameta pipeline; 1,004 of the UNCOVERED causes quote a reason only
`TastyFrontEnd` gives (the others are a total or a size the missing count leaves short); and a
signature differs where the two spell differently (`pick[A](x: A, y: A)` against the scalameta
`pick[A](x: A, y: A): A`).

**The 108 FAIL examples, by cause.** No example fails on a number that differs from the number
its spec pins.

- **64: a fixture is one scalac rejects** (`no compiled classes`), so there is nothing to read.
  Most pin what the analyzer answers for code that does not compile, which a front end that
  reads compiled classes never sees: they are deleted with the scalameta front end unless the
  owner wants those refusals kept another way. Not all: five in `CardinalitySpec` pin counts (ω,
  ε₀, 1, ω + 2) for by-name enum-case parameters, a lazy field scalac has no spelling for; one
  in `GeneralInfixApplicationSpec` pins 6; and three read a fixture that compiles beside the
  rejected one (`SingletonIntersectionMethodSpec`, "refine a proven singleton with Singleton"
  and "refine a stable path by its own type binder"; `StdlibModelSpec`, "a JDK wildcard reads
  List by its rooted name, supplied or not"), whose first assertions are on rows. By spec:
  `HigherKindedRefusalSpec` 8, `AliasOverrideIdentitySpec`, `CardinalitySpec`, `NamespaceSpec`,
  `SingletonIntersectionMethodSpec` and `StdlibModelSpec` 5 each, `EnvironmentCompletenessSpec`
  and `MethodRefinementSpec` 3 each, six specs 2 each, thirteen 1 each.
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
- **7: a row has another identity.** A definition inside a `for` or a lambda is not listed as
  `Comp.f.<for#1>.<block#1>.g` (`NestedScopesSpec` 4), and a secondary constructor's row is not
  of kind `method` with the signature `this(u: Unit): Open` (`DefinitionInventorySpec` 3).
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
| `BinderBoundsSpec` | 22 | 22 | 0 | 0 | 0 | 0 |  |
| `CapabilitySpec` | 51 | 51 | 0 | 0 | 0 | 0 |  |
| `CardinalitySpec` | 156 | 156 | 0 | 0 | 0 | 0 |  |
| `ClosedWorldSpec` | 7 | 5 | 2 | 0 | 0 | 0 | dependency stubs given as sources |
| `CompiledSpec` | 6 | 5 | 0 | 1 | 0 | 0 | the harness: a text carrying support does not parse as scalameta |
| `CompiledTypesSpec` | 10 | 0 | 0 | 8 | 0 | 2 | `CompiledTypes` ascription (b); two read rows of `Report.of` with classes (c*) |
| `ContextBindersSpec` | 15 | 15 | 0 | 0 | 0 | 0 |  |
| `ContextualOverrideIdentitySpec` | 18 | 18 | 0 | 0 | 0 | 0 |  |
| `ConversionClosureSpec` | 9 | 9 | 0 | 0 | 0 | 0 |  |
| `CounterSpec` | 4 | 4 | 0 | 0 | 0 | 0 |  |
| `DeepStackSpec` | 1 | 0 | 1 | 0 | 0 | 0 | `MethodAnalysis.analyze` on a small stack |
| `DefinitionInventorySpec` | 31 | 29 | 1 | 1 | 0 | 0 | `EoBaseline.identity` over the tree; `GivenNames.of` over parsed givens |
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
| `GroundTypesSpec` | 34 | 34 | 0 | 0 | 0 | 0 |  |
| `HigherKindedBindingSpec` | 69 | 69 | 0 | 0 | 0 | 0 |  |
| `HigherKindedRefusalSpec` | 16 | 16 | 0 | 0 | 0 | 0 |  |
| `ImportClausesSpec` | 11 | 11 | 0 | 0 | 0 | 0 |  |
| `InferredResultsSpec` | 18 | 18 | 0 | 0 | 0 | 0 |  |
| `InhabitationSpec` | 9 | 0 | 0 | 0 | 9 | 0 | the solver |
| `InheritedOverrideSpec` | 9 | 9 | 0 | 0 | 0 | 0 |  |
| `InlineMethodsSpec` | 29 | 29 | 0 | 0 | 0 | 0 |  |
| `InlineScopesSpec` | 19 | 18 | 0 | 1 | 0 | 0 | `MethodAnalysis.Index` with a budget: a reading cut short caches no verdict |
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
| `NamespaceSpec` | 46 | 45 | 1 | 0 | 0 | 0 | `Reading.of(tree).read` on a local class's slot |
| `NaturalSpec` | 9 | 0 | 0 | 0 | 9 | 0 | `Natural` arithmetic |
| `NestedScopesSpec` | 12 | 12 | 0 | 0 | 0 | 0 |  |
| `NeutralSaturationSpec` | 25 | 25 | 0 | 0 | 0 | 0 |  |
| `OpaqueSumForwardingSpec` | 14 | 1 | 0 | 0 | 13 | 0 | the solver; one run of real Scala |
| `OpaqueTypeScopeSpec` | 20 | 19 | 0 | 0 | 1 | 0 | the fixture type-checks and runs |
| `OverrideIdentityImplementationSpec` | 13 | 13 | 0 | 0 | 0 | 0 |  |
| `OwnersAndRecordsSpec` | 16 | 16 | 0 | 0 | 0 | 0 |  |
| `PolyLambdaCardinalitySpec` | 17 | 17 | 0 | 0 | 0 | 0 |  |
| `PolymorphicMemberSpec` | 25 | 25 | 0 | 0 | 0 | 0 |  |
| `PureCodeSpec` | 41 | 29 | 12 | 0 | 0 | 0 | the open world (`analyzeOpen`) |
| `ReceiverConstructorsSpec` | 14 | 14 | 0 | 0 | 0 | 0 |  |
| `RecursionProbeSpec` | 5 | 5 | 0 | 0 | 0 | 0 |  |
| `ReentrySpec` | 14 | 0 | 14 | 0 | 0 | 0 | the open world (`analyzeOpen`) |
| `ReferenceInhabitantsSpec` | 27 | 0 | 0 | 0 | 27 | 0 | the solver |
| `RepeatedParameterFragmentSpec` | 15 | 8 | 0 | 1 | 6 | 0 | a scalameta parse fact (b); the solver (c) |
| `ReportDependenciesSpec` | 10 | 0 | 0 | 3 | 0 | 7 | the stub file and which jars render (b); `Report.of` and `Report.query` over dependency jars, sources without classes (c*) |
| `ReportHygieneSpec` | 41 | 19 | 0 | 12 | 8 | 2 | `Spelling` over scalameta tokens (b); `Blockers` categories and tallies (c); `Report.of` over mixed inputs (c*) |
| `ReportQuerySpec` | 6 | 0 | 0 | 0 | 1 | 5 | `Report.query` on sources without classes (c*); a snapshot bound (c) |
| `ReportSpec` | 49 | 41 | 2 | 0 | 2 | 4 | `Counter.stat`, `defn`, `ctor`, `param`, `source` on tree nodes; `Report.of` over a directory, a jar, a file, an unparsable source (c*); a missing path and an empty report (c) |
| `SequenceEliminationSpec` | 25 | 23 | 0 | 0 | 2 | 0 | the solver |
| `SignatureSpec` | 29 | 2 | 27 | 0 | 0 | 0 | `Counter.sourceSignature`, `defnSignature` |
| `SingletonIntersectionMethodSpec` | 13 | 13 | 0 | 0 | 0 | 0 |  |
| `SizeSpec` | 64 | 0 | 0 | 0 | 64 | 0 | the `Size` algebra |
| `SkillSpec` | 4 | 0 | 0 | 0 | 4 | 0 | the seeded skill |
| `SourceSnapshotSpec` | 13 | 0 | 0 | 0 | 13 | 0 | `SourceSnapshot.capture` |
| `StdlibModelSpec` | 53 | 53 | 0 | 0 | 0 | 0 |  |
| `StoredFactoriesSpec` | 3 | 3 | 0 | 0 | 0 | 0 |  |
| `StoredScopesSpec` | 20 | 20 | 0 | 0 | 0 | 0 |  |
| `StoredValueSoundnessSpec` | 28 | 28 | 0 | 0 | 0 | 0 |  |
| `SyntaxFeaturesSpec` | 29 | 29 | 0 | 0 | 0 | 0 |  |
| `TargetViewsSpec` | 27 | 0 | 27 | 0 | 0 | 0 | the open world (`analyzeOpen`) |
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
| **all 89** | **2,016** | **1,567** | **133** | **53** | **229** | **34** | |

Of the 186 (b) examples, the rules of the model that no example pins on rows:

- **Target views and re-entry (C6, the view sandwich D31, C6a; v1.md §2).** `ReentrySpec` (14),
  `TargetViewsSpec` (27) and 12 of `PureCodeSpec` read the open world, where every overridable
  member is open without an overriding template in the fixture. On rows only `CapabilitySpec`
  (`possibleView`, `openImplementation`, `selfView`) supplies an overrider. Not pinned on rows:
  views through an overload's arity, default arguments, η-expansion, by-name operands,
  extensions and conversions, context functions, SAM conversion, implicit search, `super`,
  enclosing and nested templates, imports; re-entry from a local definition, from what a
  constructor runs, through creators and `copy`, through an abstract given; the sandwich. To
  keep them, each fixture gets the overriding template the open world stood for.
- **A throwing default of an overridable member reads as its abstract twin** (`PureCodeSpec`'s
  open rows; docs/research/pure-code.md): the concrete member with a supplied overrider is not
  on rows, nor the parent-argument, `var` and statement forms of a constructor that only throws,
  nor a level's body binding names in its own scope (`bindingScope`).
- **Dependencies read from compiled classes** (`TastyStubsSpec`, 18; `ClosedWorldSpec`, 2; the
  fixtures give the dependency as stub sources, which the compiled front end reads from the
  classpath instead): stored values through a dependency agree with the source or refuse; Java
  signatures (`Object` as `Any`, raw types, wildcards, statics, a Java enum's constants, hidden
  state); a Scala 2 `var`'s setter is no member; a class two jars declare is read from the
  first; a stub's parent is the template of its qualified name. They need fixtures whose
  dependency is a class directory on the classpath.
- **Rule T's edges** (`CompiledTypesSpec`, 8): a name typed in its pattern has that type; stale
  classes are not read as current (the compiled front end refuses the whole source, pinned only
  by its own spec).
- **An exhausted budget is `?`** for the index, lookup, alias and projection budgets
  (`AnalysisQuerySpec`, 5). The compiled front end has no such budgets yet.
- **A measurement does not depend on the caller's stack** (`DeepStackSpec`). The scalameta
  pipeline wraps each measurement in `DeepStack`; the compiled front end calls the solver
  directly, and nothing pins what it does on a small stack.
- **A local class's slot resolves in the block around it** (`NamespaceSpec.slotLocal`, the type
  parameter half), and **a non-final class and an open trait may overlap**
  (`TopTypesUnionSpec.openPair`, which no count reaches by design).

The rest of (b) pins no rule of the model that rows do not pin: the member-signature reading
(`SignatureSpec`, `GivenSignatureSpec`, `ExtensionSignatureSpec`, 60 examples of
`Counter.sourceSignature` and `defnSignature`) is in no report and not in the contract (v1.md §2
has two tables); `MeasuringSpec` pins the workers of `Measuring`; the substitution, placeholder
and refinement internals are pinned on rows by the same specs' other examples; `Spelling` is a
format the compiled front end spells itself (the 9 FAIL examples above are its differences).

Gaps in the front end's interface that the switch had to work around:

- `Report.of` is tied to `ScalametaFrontEnd`, so the harness builds a `Report` from
  `TastyFrontEnd.rows` itself, without `Report.of`'s reading of files, errors and locations. A
  front end `Report.of` can be given would make `Compiled.report` the production path.
- A source that defines nothing is an error (`no compiled classes for this source`), told from a
  source whose classes were not found only by its text; the harness matches the message.
- The compiled front end takes no `MethodAnalysis.Limits` and has no open-world reading.
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
end and joins the ledger at its next run. Not given one, by decision (the note above): the
member-signature reading, which docs/type-arithmetic.md §10 describes and README.md and
docs/index.md show as library API, and which is deleted with the scalameta front end; the index,
lookup, alias and projection budgets; stack independence and the refusal for a class the classpath
does not hold.

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

## Phase 5: the TASTy front end, slice by slice

Each slice lands when the differential runner shows no **worse** row for the rows it covers; a
better row is recorded with its derivation ("Refined slice order" below has the full rule). A row
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
  The 70 in `rejected-fixtures.txt` go with uncompiled support.
- Sources without compiled classes are not analyzed at all until uncompiled support returns.
- Measure again on the frozen corpus: rows, moved rows, wall, CPU, memory.

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

### What exists

| Piece | Where | What it is |
| --- | --- | --- |
| Boundary | `reporting/FrontEnd.scala` | `FrontEnd.rows(Module): Rows`. In: each source's path and text, and the `Report.Environment`. Out: one answer per source in order, its `Entry` and `Definition` rows or a `Report.Error`. |
| Old front end | `reporting/ScalametaFrontEnd.scala` | The pipeline `Report.of` had inline, moved unchanged; the only one `Report.of` calls. `Report.query` calls its steps directly. |
| New front end | `compiled/TastyFrontEnd.scala`, `Targets.scala`, `Readings.scala`, `Source.scala` (998 lines) | Reads the compiled classes through tasty-query. |
| Gate | `core/src/test/scala/cardinality/compiled/Differential.scala` | See "The gate". |
| Regression | `TastyFrontEndSpec` (13 examples) | Derived counts for the first slice, the gate's verdicts, row names for what the compiler desugars, and the front end's imports against an allow-list (`allowed`). |

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
  unbounded; over the standard model's types (`BuiltinTypes.known`) and those parameters. Its
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
sbt 'core/testFull'                                          # fills core/target/compiled-fixtures
sbt 'core/Test/compile' 'export core/Test/fullClasspath'     # prints the classpath, once
java -Xmx16g -Xss8m -cp <classpath> cardinality.compiled.Differential \
  corpus <corpus> --old /tmp/corpus-old.bin
java -Xmx12g -Xss8m -cp <classpath> cardinality.compiled.Differential \
  fixtures core/target/compiled-fixtures
```

- `<classpath>`: the exported list joined with `:`. In it `${OUT}/…/scala-cardinality_3.jar` and
  `…_3-tests.jar` are `target/out/jvm/scala-3.9.0/scala-cardinality/classes` and `test-classes`;
  `${CSR_CACHE}` is the coursier cache, `${SBT_BOOT}` is `~/.sbt/boot`.
- `<corpus>`: a directory with `core/src/main/scala/**` (sources), `classes/` (their compiled output)
  and `classpath.txt` (one dependency jar per line). The frozen one is
  `/var/cache/users/rhansen/cardinality-tasty/corpus-core` (core at `53ab312`).
- `--old <file>` keeps the scalameta rows of the corpus (about 100 s to compute) between runs. Delete
  the file when the old front end, the row types or the corpus change; a file of another class
  version fails to load.
- `--rows` lists every row with the front end that produced it.
- Verdicts: 4b's, with identical also `?` on both sides, and uncovered, only old, only new.
- Pairing, strictest first: name, kind and line; then position (the name without `<kind#n>`
  segments, same kind and line); then name and line across kinds. A key pairs only when both sides
  have as many rows under it.
- Exit 1: a worse row, a row only one side has, or a source or class a front end could not read.
- Listed to be explained: every better row, and every row paired by a looser key than the first.
- A fixture is every directory of the cache with an `ok` marker and no `.` in its name (a
  `<key>.<thread>` directory is a compile's staging left behind). It is read against the Scala
  library alone, as the tests read it; one compiled against more leaves uncovered the rows whose
  types name those classes (24 rows: `a member the classpath does not hold`).

What it cannot see:

- A wrong number on a row that is `?` on the old side reads as better: each better row needs its
  derivation. That is 81% of the corpus's method rows.
- `?` on both sides is identical whatever the reasons (6 corpus and 30 fixture method rows the new
  front end answers `?` itself).
- An uncovered row is exempt without memory: a row measured at one commit and uncovered at the next
  passes, and only the totals move. Compare the summary with the table below.
- Signatures are only counted, and captures are not compared (the new front end lists none yet).

### Measured, first slice (2026-11-11)

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
| S1 (5a) | Types of the module's sources first: records, sealed sums, enums, objects, aliases, opaque types, class type parameters and their bounds, unions, singletons, type members. |
| S2 (5b) | Recursion through the existing `capacity.Solver`. Restricted constructors wait for S6 (rule B's factories). |
| S3 (5c) | Constructor rows: positions as value spaces, type-parameter slots to the shape model. |
| S4 (5c) | Class receivers and objects with parents: fields by rule B's public primary, abstract members as capabilities, the closed-world index, inherited declarations, enclosing parameters, local definitions. |
| S5 (5c) | Contextual scope: givens, `using` and implicit clauses, context bounds, context functions, extension methods. |
| S6 (5d) | Construction (D26) for classes, after it is restated for desugared values (Q5). The corpus hardly exercises these rules: their gate is the fixtures. |
| S7 (5e) | As planned. |
| S8 (6) | `Report.query` over the new rows; the specs' harness; delete the scalameta front end, the stubs and `CompiledTypes`. |

A slice lands when both differential runs exit 0, every better row is derived, and the uncovered
count has fallen by what the slice claims.

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
- **Tests.** The 204 cached fixtures that do not compile have no rows in the new front end (phase 6
  names the two ways out). `TastyFrontEndSpec.compiled` duplicates `cardinality.Compiled`
  (`core/src/test/scala/cardinality/Compiled.scala`), which should also record the classpath a
  fixture compiled against: merge them.
- **The gate is run by hand.** `TastyFrontEndSpec` runs in CI; the two differential runs do not, and
  the corpus is on one machine. To rebuild it: check core out at `53ab312`, compile it, copy
  `core/src/main/scala` and the compiled classes, and write core's dependency classpath one jar per
  line. The new main code is exercised by that one spec: check the coverage floor
  (`coverageMinimumStmtTotal := 80` in `build.sbt`, `sbt coverageAll`) before the next slice.

### Open questions for the owner

Until one is decided the new front end leaves the rows it touches uncovered, paired by a looser key,
or unread. The two counts that move without a decision are derived in the rule table (construction).

- **Q1. Scope names the compiler desugars** (a `for`, an untupled parameter, a polymorphic function
  literal: 6 rows). Keep the tree's names (`<lambda#n>`) in the report, or reconstruct the source's
  from positions? Proposed: keep the tree's. Before S8.
- **Q2. `given g: T = deferred`**: a method row (old) or a declaration row (the tree)? Proposed:
  declaration. Before S8.
- **Q3. Signatures of inferred members.** Print the signature as written (now), or add the
  compiler's type, and then rooted at `_root_` as the old rows do, or short? Proposed: as written.
  Before S8.
- **Q4. Top-level pattern definitions.** One definition row per bound name (now), or one per
  definition named by its pattern? The plan asks for the same row set; the old name carries rule T's
  ascriptions, which go. Proposed: one per definition, named by the names it binds (`c, d`). Before
  S2.
- **Q5. Construction (D26) on desugared values.** The tree splits `val (a, b) = (???, true)` into
  `val a = ???; val b = true`, so the object is never built, where the syntax rule sees a tuple and
  says built. `val a, b: Int = ???` is two values with one written initializer: the old rule excludes
  the whole definition as the measured choice. `val _ = ???` is a bare match on `???`, and a match
  whose selector throws is not in the rule's list. Restate the rule over the tree's values (an
  initializer is what the tree evaluates), or keep the source's statements? Proposed: the tree's values, with one definition's values excluded
  together. The 8 fixture rows beside such a definition are uncovered until then. Before S6.
- **Q6. Budgets.** With dependencies the old front end gives each target 300,000 of its own steps
  and solver depth 64; without, none. The new one calls the solver with its defaults today (256
  states, no depth limit). The steps count syntax work that no longer exists. Keep the
  step quota in some form, or only the solver's own budgets, and then with depth 64 always?
  Proposed: the solver's only, depth 64 always. Before S8.
- **Q7. Stale classes.** No count reads the source, so classes of another text give a row whose
  signature and count disagree. The check: the name of every class, object, method and value row is
  spelled at its offset, on the line and column the classes recorded. It is weaker than rule T's
  match, which includes the span: it misses an edit that keeps every line's length, an edit after
  the last definition, and alias rows. The build plugins compile before they report, so only a
  direct `Report.of` call with mismatched inputs can hit it. Accept; or compare the whole file's line
  sizes (in TASTy, not exposed by tasty-query); or have the plugins pass a digest? Proposed: line
  sizes, in S8.
- **Q8. A source that defines nothing** has no classes. Answer it as an empty source, as the old
  front end does, or as `no compiled classes for this source` (now)? Empty would also hide a source
  the build did not compile. Proposed: empty when the text holds no definition keyword. Before S8.
- **Q9. Must refusal reasons match?** "Byte-identical report" says yes; the gate compares counts
  only, and the new front end's reasons will differ wherever a syntax refusal has no counterpart.
  Proposed: no; list the reasons that changed at the switch-over. Before S8.
- **Q10. A member the classpath does not hold.** Uncovered (now), though no slice will ever read it,
  or a refusal of its own? With a build's classpath it does not occur. Proposed: a refusal, once the
  fixtures record their classpath. Before S8.
- **Q11. `Report.query`** is not behind the boundary: its targets, names and budgets are the
  scalameta index's. Reimplement it as a filter over the new rows (proposed), or keep a budgeted
  measurement per target? With Q6, in S8.
