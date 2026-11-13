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

### State (gate agent)

A note for whoever continues from the files alone. Nothing is open. Stretch 3 (the spec suite
against the compiled front end: `a584ae1`, `baec0ca`, `0fd209d`) and stretch 4 (a row-level home
for the rules only scalameta examples pinned: `8b6763f`, `95bd7a0`, `409578f` and the commit of
this ledger) are committed; "Test fixtures", "Specs against the compiled front end" and "Rules of
class (b), on rows" below say what each left. After a front-end slice lands, rerun
`scripts/spec-ledger.sh docs/baselines/tasty-spec-ledger.tsv` and diff the ledger: an example may
move from UNCOVERED to PASS, and one that moves to FAIL has a cause to read. An expectation moves
only with its derivation.

Stretch 5 (the inline-body reading, `Inlines`) is committed and plugged into `Readings`.

Stretch 6 (the closed world over symbols, `ClosedProgram`, and the dependency classes without
TASTy, `DependencyClasses`) is committed, and stretch 7 wired both into the compiled front end
(`e952ebe`, `b3f8fce`); stretch 8 read the open member and parents with constructor parameters
(`e0eb38a`, `4c87c8d`). "Wired into the front end" and "The open member and parents with
constructor parameters" at the end of the section "The closed world and dependency classes over
symbols" have what changed and the measurements. Nothing of it is half done. Left for whoever continues: templates
that reach one parent through two parents (475 corpus rows), generic classes without TASTy, and a
check of the dependencies' classes in `ClosedVerdicts`, which compares the module's classes only.

Stretch 9 merged `tasty-front` at `9bcea74` (`ff83f6c`), read a self type that adds nothing to its
class (`7825fbf`), and then constructor rows in a class or in an object with parents and the
abstract type members of a receiver; "Self types, constructor rows and abstract type members
(stretch 9)" at the end of the section has what changed. Edits to the shared coverage memory
(`/var/cache/users/rhansen/cardinality-tasty/covered/`): the gate run after `ff83f6c` added the rows
that merge newly measures, through `scripts/differential.sh`, and a second run over `corpus-cats-eo`
and `fixtures` added two more once their derivations were in `derived.txt`. The gate run at
`cc72929` (after merging `tasty-front` at `5357531`) added the rows the stretch newly measures:
cats-core 102, cats-eo 9, cats-eo-generics 6, cats-kernel 91, corpus-core 158, fixtures 479. No row
was removed.

Stretch 10 takes the capability records' remaining reasons. A record with parents, or with a member
something overrides, is read as what its template supplies as a receiver (`Receivers.record`, which
`Shapes.record` asks, `3adf7ed`). Rule B reads a final record: where the site may call its
primary constructor, a field it observes as itself (a field it may select, or a public alias);
where it may not, the largest of what its secondary constructors and factories build over the
fields it observes (`Factories`, the port of the scalameta reading's receiver factories, `6f23c0e`).
A record's abstract type members are next. Edits to the shared coverage memory: the gate run after
merging `tasty-front` at `662b56e` (`877c65f`, derivations in the next commit) added cats-core 545,
cats-eo 22, cats-kernel 1, corpus-core 2 and fixtures 233 rows. No row was removed.

Investigated for later: `a class that is no product, sum or type-only template` (1,268 corpus rows)
is almost all corpus-core's use of scalameta's Scala 2 traits without TASTy (`scala.meta.Tree`,
`Type`, `Stat`, `Source`), which `DependencyClasses` reads as `Declared.Record` and `Shapes` does not
read yet: they need a dependency record read from its pickled signatures (its abstract members as
capabilities, its subclasses on the classpath settling which concrete members vary). The record
reading's re-entry guard (`an overridden member that may call it among the members of a capability
record`, 1,146 rows: cats' `eqv`, `neqv`, `imap`, …) takes `Reentry`'s receiver rules, under which a
behavior that passes `this` on or calls through it may re-enter; for a record value, another
instance, only a behavior that reaches the site itself can, which a narrower search would read.

Stretch 11 narrows that guard for a record value (`Reentry.may` with `instance = false`): a
behavior re-enters the site only by naming a member of its name, calling a member of the module
that does (through its overriders), a name or selection that does not resolve, applying a
function a field holds, building an instance of the site's class, or handing an instance (`this`,
or one it builds) to code outside the module whose members the module overrides and which reach
the site. The receiver rule is unchanged. A finished search that found nothing caches the members
it visited per measured name and class. Record refusals for re-entry fall from 1,146 to 905 corpus
rows; cats-core's identical method rows rise from 1,195 to 1,198, and corpus time rises (cats-core
4.8 s to 7.0 s, the search reading the module's call graph). Next: dependency records from pickled
signatures (scalameta's Scala 2 traits), then records' abstract type members.

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

**In progress** (kept current, so that the work can be taken over from the files alone).

- Stretch 2, extended through S5: each slice is committed when `scripts/differential.sh` passes
  and it is reviewed three ways, the last fix round included.
- Done: the gate changes (limit classes, coverage memory, six corpora, withheld support; then
  TIGHTENED rows, listed corrections, malformed and dead lines of the lists), the decided Q4, Q6,
  Q8, S1, S2 with the sum of a sealed family ("After the sealed sums and the merge"), S3, the
  merge of the sibling's `9c4e107`, and `Differential.fixtures` reading `src/dependency/` as
  dependencies against each fixture's `classpath.txt`.
- Done since: `Inlines` plugged into `Readings` ("After `Inlines`"), with the first run of
  `scripts/spec-ledger.sh` on this branch and every new FAIL named and classed.
- Done since: S4's first step, receivers ("After S4"; `Receivers`, `ReceiversSpec`), and S5's
  first step, contextual scope ("After S5"; `ContextualSpec`). The stretch stops here, by
  instruction: the report goes to the coordinator.
- Stretch 3 (finish S4 and S5 without the cross-module index, which the sibling builds in
  `compiledworld/`: do not edit that directory). Done: the gate compares captures and accepts a
  bound tightened to any exact value within it; an abstract given with a `using` clause is read
  as a function (a correction, in `ContextualSpec`); a class that shares a given's name keeps its
  constructor row ("Stretch 3, first unit").
- Done since ("Stretch 3, second unit"): members of a given instance without parameters; context
  function types; the closed world's final for a sealed family's subtype, each better row
  derived; `sbt coverageAll` (91.50% of statements, floor 80).
- Stretch 4 (the coordinator's order, by rows unblocked: 1 enclosing definitions, 2 capability
  records, 3 sealed and enum receivers, 4 bounded and higher-kinded type parameters then
  unions, singletons, type members and refinements, 5 self types, abstract type members,
  objects with parents and constructor rows in a class, 6 anonymous contextual parameters).
  Decisions recorded: captures may change in the three classed ways, and the old alias listing
  is not imitated; no capture row is listed by key, but a new listing that lost every name fails
  the gate; the 7-against-ω family is a correction waiting for an input; a non-final sealed
  subtype with a parent outside the module waits for the index.
- Done in stretch 4 ("Stretch 4, first unit"): the capture guard; local definitions over the
  methods they are written in.
- Done ("Stretch 4, second unit"): members of anonymous classes. Decisions recorded: the
  capture class for a shared name stands; the ledger FAILs that pin a refusal the compiled tree
  resolves are specs to rewrite at the switch (listed under each unit). The gate agent is wiring
  its index into `World.scala`, `Shapes.named`, `Stored.named`, the finality test of `Receivers`
  and the callers of `world.extended`/`overridden`/`unjudged`: add beside those, do not
  restructure them.
- Done ("Stretch 4, third unit"): a captured parameter is refused only where a scope between
  its method and the definition binds its name again (it was: anywhere in the method).
- Done ("Stretch 4, fourth unit"): capability records without parents.
- Stretch 5 (the coordinator's order: item 4, records' remaining reasons, item 5, item 6).
  Decisions recorded: the 41 Ior corrections stay grouped under one rule with worked examples;
  the canEqual and As corrections stand; the three new ledger FAILs go on the rewrite-at-switch
  list. Done: higher-kinded type parameters ("Stretch 5, first unit"); the merge of `tasty-gate`
  at `7aba1c6` (`9bcea74`, verified by the coordinator); type constructors written as class
  arguments ("Stretch 5, second unit"). Scope change: item 5 (self types, enclosing objects
  with parents, abstract type members, constructor rows in a class) moved to the gate agent;
  this branch keeps item 4, the records' remaining reasons and item 6. Done since: the merge of
  `tasty-gate` at `ff83f6c` (the coordinator's instruction; the gate agent's merge of `9bcea74`
  over the open member and parents with constructor parameters), which merged without
  conflicts; two rows the branches measure together are derived (`T.g` of a fixture, 2^8, and
  cats-eo's `MultiFocusFromList.fromList`, 0). Merge a later head of the gate agent only when
  the coordinator says so. Next: the rest of item 4
  (bounded proper parameters, intersections, refinements, opaque members through paths,
  unions, singletons), then the records' remaining reasons, then item 6. Done since: bounded
  proper parameters ("Stretch 5, third unit"); rigid constructors in constructor rows ("Stretch 5,
  fourth unit"); capabilities with type parameters of their own ("Stretch 5, fifth unit"). Scope
  change: the records' remaining reasons (parents, members something
  overrides, rule B, abstract type members of records) moved to the gate agent, which works in
  `Shapes.scala` and `Receivers.scala` on records: keep edits to the record code small. Next, in
  order: the rest of item 4 (types, bounded or other-kind type parameters, abstract type members,
  capabilities with type parameters of their own), members named like the measured definition,
  then item 6.
  Behind named `Left`s for the index: `an enclosing class with a member something overrides`,
  `an enclosing class that inherits and is abstract or may be extended`, `a parent with
  constructor parameters or in a class`, `a class of a dependency`, `a family with a subtype not
  written final whose parent is outside the module`.
- A scratch probe for reviewers is kept outside the tree at `/tmp/ProbeScratch.scala` (both
  front ends' rows of the sources given as arguments, and the compiled front end's time and
  peak heap over a corpus): copy it into `core/src/test/scala/cardinality/compiled/` to use it,
  and never commit it.
- Interface notes from the sibling's ledger of the spec suite (`scripts/spec-ledger.sh`), for S8
  or earlier: `Report.of` is tied to `ScalametaFrontEnd`; a source that defines nothing is told
  only by message text; there is no `Limits` parameter; dependencies can only be given as
  classes; a member the classpath does not hold should be a refusal of its own (Q10); `Exp.scala`
  and `lib/Exp.scala` are not told apart (the old `CompiledTypes.matching` takes the longest
  recorded path that is a tail of the source's); a member of an anonymous class inside a
  context-function literal is not listed (`Literal.anon.<lambda#1>.<new#1>.get`). The inline-body
  verdicts (S6's compile-time operations, rule X, D2, D4) arrive as the sibling's module
  `compiledinlines`, to be called where `Readings.later` answers `an inline definition`.
- To run: `scripts/differential.sh /var/cache/users/rhansen/cardinality-tasty <output directory>`,
  after `sbt core/testFull` has filled `core/target/compiled-fixtures`. While developing, run the
  gate against a copy of `<assets>/covered`: a passing run writes the memory, and a row a
  work-in-progress reading counted and the finished one leaves then fails the next run.
- The shared coverage memory (`/var/cache/users/rhansen/cardinality-tasty/covered/`), a rule for
  both agents from the coordinator: never restore a saved copy over it. Development gates run on
  a private assets directory (this branch: `/tmp/tf/assets`, with copies of the corpora and of
  the memory, the other assets linked). The shared memory takes targeted edits only, each
  recorded here:
  - Deleted, proven unsound (stretch 5, second unit): cats-core's
    `IndexedStateTContravariantMonoidal.unit` and `.product` (`IndexedStateT.scala:485` and
    `:491`), counted 1 under a type key read from a class's parameter bound to a value.

Each table is one run of `scripts/differential.sh` over the six corpora and the compiled fixtures
(1,424 that compile, of 1,496). Rows is each side's count (they are equal: nothing is unpaired).
Identical includes `?` on both sides where the new front end refuses the row itself. Unpaired is
the rows only one side has. "Old numeric identical" is the share of the old rows without a reason
that the new front end measures itself with the same count.

**Before S1** (what stretch 1 read, with the gate's limit classes and coverage memory). Every row of
every input pairs; the three better rows past a model limit are derived in `derived.txt`.

| Input | Table | Rows | Identical | Better: resolution / model | Worse | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 227 | 0 / 0 | 0 | 0 | 3,773 | 221 of 768 (28.8%) |
| core | definitions | 641 | 258 | 0 / 0 | 0 | 0 | 383 | 258 of 400 (64.5%) |
| cats-core | methods | 7,651 | 5 | 2 / 0 | 0 | 0 | 7,644 | 5 of 909 (0.6%) |
| cats-core | definitions | 1,559 | 339 | 0 / 0 | 0 | 0 | 1,220 | 339 of 1,053 (32.2%) |
| cats-kernel | methods | 1,785 | 0 | 0 / 0 | 0 | 0 | 1,785 | 0 of 368 (0.0%) |
| cats-kernel | definitions | 312 | 34 | 0 / 0 | 0 | 0 | 278 | 34 of 253 (13.4%) |
| cats-eo | methods | 697 | 2 | 0 / 0 | 0 | 0 | 695 | 0 of 271 (0.0%) |
| cats-eo | definitions | 151 | 53 | 0 / 0 | 0 | 0 | 98 | 53 of 94 (56.4%) |
| cats-eo-generics | methods | 80 | 12 | 0 / 0 | 0 | 0 | 68 | 11 of 16 (68.8%) |
| cats-eo-generics | definitions | 12 | 4 | 0 / 0 | 0 | 0 | 8 | 4 of 6 (66.7%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 19 | 0 of 4 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 5,397 | 606 | 25 / 3 | 0 | 0 | 4,763 | 576 of 4,443 (13.0%) |
| fixtures | definitions | 3,858 | 1,521 | 10 / 0 | 0 | 0 | 2,327 | 1,521 of 3,166 (48.0%) |
| **all corpora** | methods | 14,232 | 246 | 2 / 0 | 0 | 0 | 13,984 | 237 of 2,336 (10.1%) |
| **all corpora** | definitions | 2,677 | 689 | 0 / 0 | 0 | 0 | 1,988 | 689 of 1,807 (38.1%) |

**After S1** (types of the module as shapes). Every row pairs; no row is worse; the six better
rows past a model limit are derived in `derived.txt`.

| Input | Table | Rows | Identical | Better: resolution / model | Worse | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 452 | 0 / 0 | 0 | 0 | 3,548 | 260 of 768 (33.9%) |
| core | definitions | 641 | 258 | 0 / 0 | 0 | 0 | 383 | 258 of 400 (64.5%) |
| cats-core | methods | 7,651 | 10 | 2 / 0 | 0 | 0 | 7,639 | 6 of 909 (0.7%) |
| cats-core | definitions | 1,559 | 339 | 0 / 0 | 0 | 0 | 1,220 | 339 of 1,053 (32.2%) |
| cats-kernel | methods | 1,785 | 0 | 0 / 0 | 0 | 0 | 1,785 | 0 of 368 (0.0%) |
| cats-kernel | definitions | 312 | 34 | 0 / 0 | 0 | 0 | 278 | 34 of 253 (13.4%) |
| cats-eo | methods | 697 | 5 | 0 / 0 | 0 | 0 | 692 | 1 of 271 (0.4%) |
| cats-eo | definitions | 151 | 53 | 0 / 0 | 0 | 0 | 98 | 53 of 94 (56.4%) |
| cats-eo-generics | methods | 80 | 12 | 0 / 0 | 0 | 0 | 68 | 11 of 16 (68.8%) |
| cats-eo-generics | definitions | 12 | 4 | 0 / 0 | 0 | 0 | 8 | 4 of 6 (66.7%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 19 | 0 of 4 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 5,397 | 791 | 27 / 6 | 0 | 0 | 4,573 | 726 of 4,443 (16.3%) |
| fixtures | definitions | 3,858 | 1,521 | 10 / 0 | 0 | 0 | 2,327 | 1,521 of 3,166 (48.0%) |
| **all corpora** | methods | 14,232 | 479 | 2 / 0 | 0 | 0 | 13,751 | 278 of 2,336 (11.9%) |
| **all corpora** | definitions | 2,677 | 689 | 0 / 0 | 0 | 0 | 1,988 | 689 of 1,807 (38.1%) |

What S1 reads (`Shapes`, `Sums`, `World`, `Bodies`), each rule with a derived example in
`TastyFrontEndSpec` (`products`, `refusals`, `later`):

- A plain case class is the product of its fields; a trait or class with only type members is one
  value; `Nothing` is the empty sum; an alias is what the compiler dealiases it to.
- An enum and a sealed family are the sum of their cases, through sealed intermediates. A subtype
  with type parameters is read where each is unbounded and passed to exactly one slot of its own
  variance, the other slots pinned with `Nothing` (covariant) or `Any` (contravariant).
- An opaque alias of the module is one value outside its owner and its representation inside
  (rule F).
- Refused with the old front end's reasons, so these rows are identical `?`: a type that reaches
  itself, an enum's class case as a type, a family with an anonymous, given, inner or local
  instance, an open subtype, an enum under a family, a class something extends, a class with two
  parameter lists, a plain class's parameter without `val`.
- Left uncovered on purpose, each a place where a reviewer derived a wrong number or the old
  reading has none to compare: a construction that always throws, binds a pattern or inherits an
  abstract member of type `Nothing` (decision Q5: with S6); a subtype reached through two sealed
  levels; a subtype that pins a slot to a type, passes a parameter twice, bounds it or varies
  otherwise; a restricted field or constructor (rule B); `class L` under a family (one tree with
  `class L()`); an abstract member of type `Nothing` that takes parameters; a class nested in a
  class or a body; a type of the module under `=:=` or `<:<` (two types of one shape are two
  types, and the shape model compares shapes); an opaque type of a dependency.

What the plan's S1 row lists and S1 did not land (the gate's strings, corpus-core / cats-core /
fixtures): unions, intersections, singletons, refinements, match types and type lambdas (`a type`:
17 / 23 / 281), bounded and higher-kinded type parameters of the method (3 / 61 / 164), wildcard
arguments (0 / 0 / 22), and a member alias seen through a prefix's arguments. They are a second
step of S1, taken after S3: the corpora hardly hold them, and what stops their rows first is S4
(`an enclosing class`: 1,912 / 5,253 / 1,671) and S5 (`a contextual definition`: 32 / 1,345 / 139).
A class of the module that is no product, sum or type-only template (334 / 7 / 303) is a capability
record, read with its receiver in S4.

What S1 gained is small on the corpora for that reason: 225 more identical method rows on
corpus-core (39 of them old numeric rows), 5 on cats-core, 3 on cats-eo, none elsewhere; 185 on
the fixtures (150 numeric).

The coverage memory lost eight lines by hand in this slice: the rows `U.s` to `U.fg` of the
fixture `8fde8aab…/Example.scala` (families and enums whose construction throws). A run of the
unfinished slice had written them as the front end's own `?`, and the finished slice leaves them
uncovered until S6. No committed state measured them.

Wrong numbers the old front end has too, found in review and not changed here (the gate cannot
see them, both sides agree): a case that implements an abstract member of an empty type reached
through a type argument (`trait B[A] { def no: A }; sealed trait H extends B[Nothing]`) counts as
a value; a parent's default argument that throws, a secondary constructor that throws and a throw
in an enclosing object's body are not read as a construction that throws; a case class another
class extends is its product where it is read alone.

**After S2** (definition rows, `Stored`). Every row pairs; no row is worse; one better row past a
model limit is derived (`FromUnit`, a constructor alias written with a placeholder).

| Input | Table | Rows | Identical | Better: resolution / model | Worse | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 452 | 0 / 0 | 0 | 0 | 3,548 | 260 of 768 (33.9%) |
| core | definitions | 641 | 363 | 3 / 0 | 0 | 0 | 275 | 341 of 400 (85.2%) |
| cats-core | methods | 7,651 | 10 | 2 / 0 | 0 | 0 | 7,639 | 6 of 909 (0.7%) |
| cats-core | definitions | 1,559 | 1,169 | 21 / 0 | 0 | 0 | 369 | 907 of 1,053 (86.1%) |
| cats-kernel | methods | 1,785 | 0 | 0 / 0 | 0 | 0 | 1,785 | 0 of 368 (0.0%) |
| cats-kernel | definitions | 312 | 306 | 0 / 0 | 0 | 0 | 6 | 252 of 253 (99.6%) |
| cats-eo | methods | 697 | 5 | 0 / 0 | 0 | 0 | 692 | 1 of 271 (0.4%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 12 | 0 / 0 | 0 | 0 | 68 | 11 of 16 (68.8%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 19 | 0 of 4 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 5,397 | 791 | 27 / 6 | 0 | 0 | 4,573 | 726 of 4,443 (16.3%) |
| fixtures | definitions | 3,858 | 3,461 | 15 / 1 | 0 | 0 | 381 | 3,033 of 3,166 (95.8%) |
| **all corpora** | methods | 14,232 | 479 | 2 / 0 | 0 | 0 | 13,751 | 278 of 2,336 (11.9%) |
| **all corpora** | definitions | 2,677 | 1,974 | 30 / 0 | 0 | 0 | 673 | 1,598 of 1,807 (88.4%) |

What S2 reads, each rule with a derived example in `TastyFrontEndSpec` (`stored`, `unstored`):

- A row is a size and, where it is no number, reasons; an open trait or abstract class and a
  constructor alias are a dash.
- A concrete class is the product of what its constructor's parameters store, parents ignored; an
  enum the sum of its cases (`Sums.carried`, the method side's slot rule); an alias its
  right-hand side; a top-level value one value where its type is known to have one (D30).
- Types by `Counter.typeIn` and `builtin`'s rules, asked by the class the compiler bound.
- A type is read where it is written (a class's fields at the class, an object's alias at the
  alias, arguments where they are), which is how the old estimate decides rule F. The method side
  follows aliases first and decides at the use site. Both agree with the old rows.

Left uncovered on purpose (corpus-core / cats-core / fixtures definition rows): a sealed family
(41 / 301 / 206, below); a definition that reaches itself, which needs the solver's fixed point
(86 / 0 / 0); a class the algebra does not model or of a dependency (133 / 47 / 14); a
restricted constructor, rule B (3 / 7 / 28); a value of a class of the module, whose construction
says whether it exists (12 / 0 / 8); other forms of type (0 / 8 / 72: match types, type lambdas,
polymorphic functions); smaller ones (a refinement of no written type, a function type refined by
its `apply`, a wildcard argument, an alias applied to another number of arguments, an alias of a
class beside an opaque type's scope, an enum case that pins its enum's parameter).

**The sum of a sealed family on the stored side is written and taken out again**: it cannot
reach "no worse row", and the evidence is this. Read as the sum of its cases with the same slot
rule, it adds 103 identical rows on the fixtures, 149 on cats-core and 6 on corpus-core, and
then:

- `cats.data.Ior[A, B]` (cats-core): old 0, new `?` for A and B. The old estimate does not see the
  cases, whose clauses are written infix (`extends (A Ior Nothing)`). The old number is wrong.
- Fixture `2d9bd461…/Top.scala:3`, `S`: old 1, new 2. `final case class C() extends Id[S]` is a
  subtype through a generic alias the old estimate does not follow. The old number is wrong.
- Fixture `8fde8aab…`, `S`, `R`, `Q`, `M`: old 1, 0, 1, 1; new 3, 1, 2, 3. The old estimate drops
  a case whose construction throws (D26), which decision Q5 puts with S6: these stay uncovered
  until then.
- 45 better rows past a model limit, `sealed family not summed`: a subtype open to extension (24,
  some for a parent the old reading did not read), through an alias (7) or a renamed import (5),
  an enum case that extends the family (2), a subtype outside its body (1); and four generic
  families whose row the old estimate gives its parameter as reason where no case stores one.

The first two are numbers that change because the old one is derivably wrong, which the owner's
rule allows and the gate has no way to accept: a worse row always fails. It needs a decision:
either `derived.txt` may also list a worse row with its new value and why the old one is wrong,
or these families stay uncovered until the switch-over.

The same holds, unexercised by any input, for two readings the reviews found where the new
number would be right and the old wrong; both are left uncovered: a method over a sealed family
one of whose cases is an enum's value case (the old method side drops the case), and an alias of
a class read from inside an opaque type's scope.

**After S3** (constructor rows, `Constructors`; the gate's TIGHTENED rows and corrections). Every
row pairs; no row is worse; no correction is listed yet.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 508 | 1 / 0 | 0 | 0 | 0 | 3,491 | 315 of 768 (41.0%) |
| core | definitions | 641 | 363 | 3 / 0 | 0 | 0 | 0 | 275 | 341 of 400 (85.2%) |
| cats-core | methods | 7,651 | 228 | 12 / 0 | 0 | 0 | 0 | 7,411 | 224 of 909 (24.6%) |
| cats-core | definitions | 1,559 | 1,169 | 21 / 0 | 0 | 0 | 0 | 369 | 907 of 1,053 (86.1%) |
| cats-kernel | methods | 1,785 | 44 | 0 / 0 | 0 | 0 | 0 | 1,741 | 44 of 368 (12.0%) |
| cats-kernel | definitions | 312 | 306 | 0 / 0 | 0 | 0 | 0 | 6 | 252 of 253 (99.6%) |
| cats-eo | methods | 697 | 33 | 0 / 0 | 0 | 0 | 0 | 664 | 28 of 271 (10.3%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 13 | 0 / 0 | 0 | 0 | 0 | 67 | 12 of 16 (75.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 0 | 19 | 0 of 4 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 5,397 | 1,819 | 34 / 6 | 4 | 0 | 0 | 3,534 | 1,735 of 4,443 (39.1%) |
| fixtures | definitions | 3,858 | 3,461 | 15 / 1 | 0 | 0 | 0 | 381 | 3,033 of 3,166 (95.8%) |
| **all corpora** | methods | 14,232 | 826 | 13 / 0 | 0 | 0 | 0 | 13,393 | 623 of 2,336 (26.7%) |
| **all corpora** | definitions | 2,677 | 1,974 | 30 / 0 | 0 | 0 | 0 | 673 | 1,598 of 1,807 (88.4%) |

What S3 reads, with derived examples in `TastyFrontEndSpec` (`constructors`): a class's row is its
primary constructor's, the product of its positions' value spaces. A closed position is its value
space (exact, or the size algebra's tier), compared with the stored-size estimate of the same
position read as an input (`Stored.position`: a top type one value, `Null` empty, collections not
lifted, through aliases but not into a class the type names): the row is refused where the two
disagree and declined where the estimate has a reason for a ground position. A type parameter's
slot is the shape model's.

Left uncovered: a class in an object with parents (every enum class case and every case class in
the companion of a sealed type, whose companion has a `Mirror` parent: S4), a position the method
side refuses (a recursive type takes the estimate's tier: with the solver), one `Shapes` or
`Stored` does not read (a capability record, a sealed family, unions, literal types), bounded or
higher-kinded class type parameters, `using` clauses and context bounds (S5), secondary
constructors.

Tightened rows (an upper bound that became the exact number of the same value), all four in the
fixtures and all constructor rows whose position is written through a path the old method side
did not follow, so that it took the estimate's bound:

| Row | Old | New |
| --- | --- | --- |
| `434f4d9c…/U.scala:1` `U.<init>`, `U(x: q.X)` through an export | `≤2^32` | `2^32` |
| `434f4d9c…/lib9/Q.scala:3` `lib9.R.<init>`, `R(x: q.X)` | `≤2^16` | `2^16` |
| `5ed1c78e…/C.scala:3` `a.b.UsesT.<init>`, `UsesT(t: x.T)` through a value | `≤2^8` | `2^8` |
| `5ed1c78e…/C.scala:4` `a.b.UsesS.<init>`, `UsesS(s: x.S)` | `≤2^8` | `2^8` |

Open, for the coordinator: the gate accepts a bound only when the exact number is the bound's own
value. A review produced rows no input holds where the old bound is a rounded capacity and the
exact number is below it (`P1(t: Option[Paths.x.T])`: old `≤2^9`, new 257). Such a row is worse
to the gate, and it is no correction either, since the old bound still holds.

Corrections (every number this rewrite changes): the table under "After the sealed sums and the
merge".

The old front end's rows did not move. S3 extracted `agrees` and the disagreement message from
`analysis/ConstructorPositions.scala` into `analysis/inhabitation/ClosedSpaces.scala` (a pure
extraction, read line by line in review). With it in place, `scripts/measure-corpus.sh` renders
reports byte-identical to `baseline/*-report-1358b07.txt` for corpus-core, cats-core, cats-kernel,
cats-eo and cats-eo-generics. cats-eo-schemes is byte-identical to `…-report-5f89e91.txt` and
differs from `…-1358b07.txt` in one row (`Schemes.OnStackLimit`, an untyped value): `1358b07` is
the sibling's rule T fix, which this branch does not hold before the merge.
Rechecked after the merge, with the sealed sums in place: the cats-eo-schemes report is
byte-identical to `baseline/cats-eo-schemes-report-1358b07.txt`.

**After the sealed sums and the merge** (the stored-side sum of a sealed family restored in S2;
the sibling's `9c4e107` merged, so the fixtures are 1,934 and each is read against its own
`classpath.txt`, its `src/dependency/` units given as classes). Every row pairs. Two rows are
worse, both listed corrections; the eight better definition rows past a model limit in the
fixtures are derived in `derived.txt`.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 509 | 1 / 0 | 0 | 0 | 0 | 3,490 | 316 of 768 (41.1%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 228 | 13 / 0 | 0 | 0 | 0 | 7,410 | 224 of 909 (24.6%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 44 | 0 / 0 | 0 | 0 | 0 | 1,741 | 44 of 368 (12.0%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 33 | 0 / 0 | 0 | 0 | 0 | 664 | 28 of 271 (10.3%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 13 | 0 / 0 | 0 | 0 | 0 | 67 | 12 of 16 (75.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 0 | 19 | 0 of 5 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 2,346 | 59 / 5 | 4 | 0 | 0 | 4,332 | 2,247 of 5,485 (41.0%) |
| fixtures | definitions | 5,588 | 4,923 | 65 / 8 | 0 | 1 | 0 | 591 | 4,342 of 4,513 (96.2%) |
| **all corpora** | methods | 14,232 | 827 | 14 / 0 | 0 | 0 | 0 | 13,391 | 624 of 2,337 (26.7%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

What the sum reads, with derived examples in `TastyFrontEndSpec` (`stored`, `unstored`): a sealed
family is the sum of its subtypes, an object one value, a concrete class what it stores, a sealed
level its own sum, each where its clause passes the family's parameters on (`Sums.carried`). A
case under a sealed level is that level's, and one reached under two levels leaves the family
uncovered. A generic family's row carries the sum over its parameters in the form of a generic
product: the parameters as its reasons.

Left uncovered: a family nested in a class or a body; an anonymous, given, inner or local subtype;
an enum under a family, or an enum's value case reached through a level or a reference (the row of
a family such a case extends directly is the old one, no size and its refusal); an open abstract
subtype; a subtype something extends; a subtype not written final that a subclass could tell apart
(a plain class, a parent outside the module with members, a member to override), which the old
estimate refuses and the closed world may close: an open question below; a construction that
always throws or binds a pattern (S6); a family that reaches itself (the solver).

Corrections (every number this rewrite changes; `corrected.txt` holds the two rows of the gate's
inputs, `TastyFrontEndSpec.agrees` the four of its own fixture):

| Row | Old | New | Derivation |
| --- | --- | --- | --- |
| cats-core `cats.data.Ior` (`Ior.scala:47`), definition | 0 | depends on A and B (`?` with the reasons A, B; size shown 3ω) | The owner's ruling: the cases are `Left(a: A)`, `Right(b: B)` and `Both(a: A, b: B)`, so the value space is A + B + A × B. The old estimate does not see cases whose clause is written infix (`extends (A Ior Nothing)`) and takes the family for empty. |
| fixture `a5a7ef45…/Top.scala:3` `S`, definition | 1 | 2 | `S` has the cases `S1()` and `C()`, one value each: `C extends Id[S]` with `Id[X] = X`, so `C` is a subtype of `S` and the compiler lists it. The old estimate does not follow a generic alias in an `extends` clause. |
| spec fixture `sizes.Ior`, definition | 0 | depends on A and B | As `cats.data.Ior`, on a copy of its shape. |
| spec fixture `sizes.Iors`, definition (`Iors(i: Ior[Boolean, Boolean])`) | 0 | 8 | 2 + 2 + 2 × 2 at Boolean and Boolean. |
| spec fixture `sizes.Iors.<init>`, constructor | 0 | 8 | The same position as an input. |
| spec fixture `sizes.UseIor.split`, method (`split(i: Ior[Boolean, Boolean]): Boolean`) | 1 | 2^8 | The input splits into its 8 values, each sent to one of 2. |

Better rows past a model limit, by rule (all in the fixtures, all definitions):

| Rule | Rows | Count |
| --- | --- | --- |
| A generic family none of whose cases stores a parameter is the number of its cases at every argument; the old estimate gives a generic refusal | `q.Affine` (two fixtures), `O.Tag`, `Two` (two fixtures) | 1, 2, 2 |
| A value class cannot be extended, so a case that is one is closed though not written final; the old estimate did not read its parent `AnyVal` | `w.W` (two fixtures) | 3 = 2 + 1 |
| `Function1[Unit, *]` is a type lambda, so its alias is a constructor alias (from S2) | `FromUnit` | a dash |

Open, for the owner:

- Printing `A + B + A·B`. A definition row holds one size and a list of reasons, so a generic
  family shows `3ω` (each unbound parameter is ω) with the reasons A and B, as a generic product
  shows `ω` with its parameter. A formula needs a symbolic size in the row model (a polynomial
  over the parameters, kept through `Size`'s sum and product and rendered by the report and the
  artifacts), and a decision on how a total adds such rows.
- Whether the closed world makes a subtype final that is not written final and has a member a
  subclass could override, where nothing supplied extends it. The old estimate refuses such a
  family; the new front end leaves it uncovered rather than decide.

**After `Inlines`** (the sibling's inline-body reading, moved into `compiled/` and asked by
`Readings`). Every row pairs; the two worse rows are the listed corrections; the 25 better method
rows past a model limit that the inline verdict adds in the fixtures are derived in `derived.txt`.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 509 | 1 / 0 | 0 | 0 | 0 | 3,490 | 316 of 768 (41.1%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 228 | 13 / 0 | 0 | 0 | 0 | 7,410 | 224 of 909 (24.6%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 44 | 0 / 0 | 0 | 0 | 0 | 1,741 | 44 of 368 (12.0%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 35 | 0 / 0 | 0 | 0 | 0 | 662 | 28 of 271 (10.3%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 13 | 0 / 0 | 0 | 0 | 0 | 67 | 12 of 16 (75.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 0 | 19 | 0 of 5 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 2,506 | 59 / 30 | 4 | 0 | 0 | 4,147 | 2,327 of 5,485 (42.4%) |
| fixtures | definitions | 5,588 | 4,923 | 65 / 8 | 0 | 1 | 0 | 591 | 4,342 of 4,513 (96.2%) |
| **all corpora** | methods | 14,232 | 829 | 14 / 0 | 0 | 0 | 0 | 13,389 | 624 of 2,337 (26.7%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

How it is plugged in: `TastyFrontEnd` builds one `Inlines` per module, over the module's classes
and, listed only when an inline body selects a member of a class outside the module, its
classpath's (a class that does not load then is a warning of the report). `Readings.counted` asks
in this order: rule X (`Inlines.narrowed`: a `transparent inline` result that is no singleton is
`?`); the one-value rule (a goal with one value is 1 before the body is read, as the old front end
has it); `Inlines.refusal`, whose reason is the row's `?` as it stands, the model's own and not
`not read from compiled classes yet`; then the solver. An inline method of an object that is
never built counts like any other, as the old front end counts it: a call is its expansion and
builds no object. An inline parameter is a plain input. A method written inside an inline method
is asked too, and is still uncovered for its enclosing definition until S4.

Fixes to `Inlines` from this stretch's reviews, each with an example in `InlinesSpec`: a receiver
whose type names a type parameter of the method or of a class around it, or a parameter (its
singleton, a member of its path), is typed by the expansion, so an inline override is brought
along (`onTyped`, `onSingleton`, `onPath`); a type test named without a qualifier is a type test
(`unqualified`); a compile-time type inside a wildcard's bounds or a refinement is seen
(`World.mentioned`, which replaces the module's own walk).

The 25 derived rows, by the old reason (the counts are those of a `def` of the same signature):

| Old reason | Rows | Why it counts |
| --- | --- | --- |
| D2: `may expand the inline conversion …`, `may expand an implicit conversion from outside the sources`, `applies +, whose arguments this reading does not type …`, `applies +, which an extension of that name may answer` | 14 | The compiled tree applies no conversion and no extension there: the member applies. |
| D2: `selects plusOne on scala.Int, which the library table does not list` | 1 | The compiler resolved the extension of the same object, which is no inline method. |
| D4: `leaves a given of useShow to implicit search`; a context function type, literal or callee | 4 | The tree passes the given, or the function's argument, that the compiler chose where the method is written. |
| A name the scalameta reading does not resolve: `selects head on Tuple2, which the library table does not list`, `reads Inlines, which the wildcard import … may bind`, `declares the local type`, `creates an instance of a class not read`, `selects … on a value whose type is not read` | 6 | The compiled tree names what the compiler resolved; no body reached has a compile-time operation. |

`InlineVerdicts` over the fixtures after the plug-in: 1,934 inputs, 337 methods; same verdict 296
(147 count, 149 refuse), counts where the old refuses 37, worse 0, masked 1, other 1, unpaired 2.

Spec ledger (`scripts/spec-ledger.sh`), first run on this branch, against the sibling's baseline
`docs/baselines/tasty-spec-ledger.tsv` (taken before S1):

| | PASS | UNCOVERED | FAIL | PENDING | SCALAMETA |
| --- | --- | --- | --- | --- | --- |
| baseline (`9c4e107`, before S1) | 193 | 1,338 | 99 | 19 | 81 |
| after `Inlines` | 508 | 951 | 172 | 19 | 81 |

314 examples went from UNCOVERED to PASS and one from FAIL to PASS (`StoredScopesSpec`, "a
path's first name through a followed export"). 74 went from UNCOVERED to FAIL: each reads a
fixture the differential gate reads too, where no row is worse but the two listed corrections, so
each fails for one of four reasons, none a wrong number. They are to be settled at the switch-over
(S8), when the specs' expectations move to the compiled front end:

| Class | Examples | What the example pins |
| --- | --- | --- |
| The same `?`, other words (16) | `EnvironmentSoundnessSpec`: "refuse a sealed family extended through an alias", "refuse a sealed family with a child in any block", "refuse a sealed family with an inner or local child"; `GroundEnvironmentSpec`: "an enum's cases shadow it after a wildcard import"; `ImportClausesSpec`: "an import of an inherited member says so"; `NamespaceScopingSpec`: "a generic definition named without arguments has no value space", "an abstract type member shadows an outer type"; `NamespaceSpec`: "an enum's cases are not read as a namespace", "an open trait of an object is open"; `ReportSpec`: "an abstract member is open"; `StoredScopesSpec`: "a singleton case is a term only, a plain object no enum", "a wildcard, a selector, a path and a bare name bind the case", "case parameters read in the companion, the body imports cases"; `StoredValueSoundnessSpec`: "a parameterised family instantiates through its slots"; `ThrowableSlotsSpec`: "every root-import spelling of a throwable is the open class"; `ValueRowsSpec`: "an inhabited, undeclared or unresolved type" | A refusal's text: the name's spelling (`open[Throwable]` against `open[java.lang.Throwable]`, `enum case type not read: A` against `en.E.A`), the order of two reasons, or which of two true reasons is given. The row is `?` on both sides. |
| The same `?`, the inline verdict's words (28) | `InferredResultsSpec`: "a transparent inline method's result is refused"; `InlineMethodsSpec`: "a conversion is read by its type, wherever it may be found", "a cycle of inline calls is decided as one", "a member an argument's or a subclass's class may override", "a member that does not apply falls back to an extension", "a passing-looking method calling a failing one", "a type test on an array reads its element type", "an extension on its own type parameter may answer anything", "an extension restating the member is never taken (P2)", "an inline parameter inspected by a callee", "implicit search through extensions, context functions, eta", "inline vals and inline givens named in the body", "names the reading cannot resolve", "the A5 fixtures stay refused"; `InlineScopesSpec`: "a clause a callee does not take applies its result's apply", "a conversion behind any number of aliases is found", "a listed namespace's wildcard binds its own terms only", "a local method's using clause, context bound and default", "classOf of an array of a type variable reifies the variable", "library and Predef results applied again", "only a template extending the receiver overrides its member", "this in an anonymous class, a local class, a local given"; `SyntaxFeaturesSpec`: "an inline method counts unless its expansion reads its call"; `TransparentInlineSpec`: "a closed transparent result is still narrowed at the call site", "a declared top type hides what callers observe", "a narrower class, None, or a declared singleton", "a transparent function result is refused too", "a transparent result of an open class may be a subclass" | The scalameta reading's reason for an inline method. `Inlines` refuses the same method with its own reason (what the compiled tree shows). |
| A count where the old front end refuses, inline (16) | `ConversionClosureSpec`: "imported into the body's scope", "imported into the body's scope #2", "in the companion of a type the body writes", "reached through a given of a reached companion, transitively", "reached through a referenced member's declared type", "reached through the signature of a given the body may summon"; `DependencyRowsSpec`: "a compiled inline body is not read: its caller refuses"; `InlineMethodsSpec`: "a channel outside the sources refuses all but identities", "a given left to implicit search, summon, valueOf", "an exported or a local conversion is found where it is visible", "an inline conversion of the sources refuses every body", "library members, extractors, named arguments, overriding", "reasons quote what they name"; `InlineScopesSpec`: "a conversion private to another package is never inserted", "a given whose type a wildcard may hide is read as its declaration", "a local type that hides an outer one refuses" | A refusal that decisions D2 and D4 replace by reading the compiled tree, or a name the scalameta reading did not resolve: the classes of the table above. |
| A count where the old front end refuses for a limit of reading syntax (13) | `GroundEnvironmentSpec`: "a constructor position reads the inherited member on both sides"; `ImportClausesSpec`: "a clause the sources do not supply stays its reason"; `MethodAnalysisSpec`: "keep a file-private type from a sibling file", "keep an import after the reference from binding it"; `NamespaceScopingSpec`: "a class member's owner binds as any type name does"; `PureCodeSpec`: "an alias nested out of reach does not stand for a parent"; `StoredScopesSpec`: "abstract, opaque and parameterised members are not read"; `StoredValueSoundnessSpec`: "a parent name an object rebinds names another type", "a subtype through a renamed import", "a subtype through an alias, or reached twice"; `TypeMemberScopeSpec`: "an opaque alias spelled through `this` is not followed"; `UnsuppliedWildcardSpec`: "nor a builtin a constructor slot reads, unlike `zz.*`", "refuses a member of a record its field type names" | A refusal for a name, import, alias or scope the compiler resolved: the gate's better rows past a resolution limit, and the derived ones. |
| Tightened (1) | `GroundEnvironmentSpec`: "a symbolic value space agrees only with a wide estimate" | `≤2^8`, now the exact `2^8` (a row of the tightened table). |

**After S4** (receivers: a member of a class reads what the instance holds). Every row pairs. The
worse rows are listed corrections: eleven in cats-core and two in the fixtures.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 967 | 1 / 0 | 0 | 0 | 0 | 3,032 | 506 of 768 (65.9%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 321 | 37 / 3 | 0 | 10 | 0 | 7,280 | 282 of 909 (31.0%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 151 | 3 / 0 | 0 | 0 | 0 | 1,631 | 144 of 368 (39.1%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 123 | 0 / 1 | 0 | 0 | 0 | 573 | 105 of 271 (38.7%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 26 | 0 / 0 | 0 | 0 | 0 | 54 | 12 of 16 (75.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 0 | 19 | 0 of 5 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 3,363 | 68 / 36 | 4 | 1 | 0 | 3,274 | 3,173 of 5,485 (57.8%) |
| fixtures | definitions | 5,588 | 4,923 | 65 / 8 | 0 | 1 | 0 | 591 | 4,342 of 4,513 (96.2%) |
| **all corpora** | methods | 14,232 | 1,588 | 41 / 4 | 0 | 10 | 0 | 12,589 | 1,049 of 2,337 (44.9%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

What S4 reads, with derived examples in `ReceiversSpec`: every parameter of the primary
constructor of each class around a member is a value of its environment, `val` or not; every
abstract member is a capability (a value, or one function from all a method's parameters to its
result; a one-valued result supplies nothing); a concrete member supplies nothing where no class
of the module overrides it; the classes' type parameters are atoms. A class that inherits is read
by its own state alone where it is an object or a concrete class that is final or that nothing
extends (the closed world's final) and no parent of the module holds state of its own. An object
with parents is read the same way. A row's captures are the names of what its receivers supply.

Left uncovered, each by its own reason: a sealed class or an enum as receiver (it splits into its
cases); a class that inherits and is abstract or may be extended; a parent of the module with
constructor parameters, in a class or without a tree; a concrete member something overrides, or
overriders tasty-query does not read; a self type; a bounded or higher-kinded class type
parameter; an abstract type member; a capability with type parameters of its own; a deferred
`inline val`, and a deferred inline method where a type parameter is in scope; a class or an
object around it whose construction always throws or binds a pattern (S6); given instances and
contextual parameters and members (S5); local definitions, anonymous and local classes
(`an enclosing definition`); rule B's secondary constructors and factories for a class named by
a type. The closed-world index over dependencies is not built: a class of the module is asked
only what the module's own classes extend and override.

Corrections added by S4 (`corrected.txt`), all method rows:

| Row | Old | New | Derivation |
| --- | --- | --- | --- |
| cats-core `cats.evidence.As.reflAny`, `fromPredef` | 0 | `?`: sealed family has anonymous instances in its file: As | The old 0 rests on reading `As` as a sealed family without a value. It has one: `reflAny = new (Any As Any) { … }`, an anonymous instance whose type is written infix, which the old reading does not see. The model refuses a sealed family with an anonymous instance. |
| cats-core `cats.evidence.As.compose`, `onF`, `conF`, `invF` | 1 | the same `?` | The old 1 is the one function from an input taken for empty. |
| cats-core `cats.syntax.IorIdOps.rightIor`, `leftIor` | 0 | 1 | `Ior[X, Y]` is X + Y + X × Y (the correction of `cats.data.Ior`). `IorIdOps[A](a: A)` holds one A and no B is in reach: `Right(a)`, `Left(a)`. |
| cats-core `cats.syntax.OptionOps.toRightIor`, `toLeftIor` | 0 | 3 | `OptionOps[A](oa: Option[A])` and `b: B`: where `oa` is `None` one choice, where it is `Some(a)` three (`Left(b)`, `Right(a)`, `Both(b, a)`): 1 × 3. |
| fixture `1e44fbe4…/Aliases.scala:4` `Outer.Base.get(a: Library.Alias): T` | 1 | 0 | `Library.Alias` is `Library`'s own `T`, `Unit` (an alias's right-hand side is read where it is written), and the result `T` is `Outer`'s type parameter, of which nothing in reach holds a value: no parametric implementation. The old reading resolved the alias to `Outer`'s `T` and counted the identity. |

Better rows past a model limit that S4 adds, each derived in `derived.txt`:

| Rule | Rows | Count |
| --- | --- | --- |
| `Ior` is A + B + A × B, so a function to C over its cases yields Cs | cats-core `cats.Align.alignWithIterator` | ω |
| No class of the module overrides a member of the trait, so its concrete members supply nothing (the old reading held two to be overridable) | cats-core `ValidatedFunctions.invalid`, `valid` | 1, 1 |
| Rule F: a result that is an opaque type outside its scope is one value | cats-eo `ComposedTraversal.to` | 1 |
| A given initialized with a value named `deferred` that is not `scala.compiletime.deferred` is a concrete member | fixture `7dcd4c40…` `V.deferred`, `V.other`, `TXs.TX.other`, `TL.other` | 2 each |
| An import through a value binds what the compiler resolved | fixture `7124cd7e…` `NP.y` | 2 |
| An alias with a bounded parameter reduces (`Alias[A] = A`) | fixture `10e29c04…` `Base.get` | 1 |

Spec ledger after S4:

| | PASS | UNCOVERED | FAIL | PENDING | SCALAMETA |
| --- | --- | --- | --- | --- | --- |
| after `Inlines` | 508 | 951 | 172 | 19 | 81 |
| after S4 | 582 | 871 | 178 | 19 | 81 |

74 examples went from UNCOVERED to PASS. Six went from UNCOVERED to FAIL, each a count where the
old front end refuses for a limit of reading syntax (the gate's better rows past a resolution
limit, or derived above): `ClosedWorldSpec` "a parent outside the sources may override it:
refused" (the parent is a class the compiled front end holds, and nothing overrides the member);
`EnvironmentCompletenessSpec` "a clause whose prefix is no path refuses the builtin" and "a
wildcard export outside the sources refuses the name"; `GroundEnvironmentSpec` "aliases, enum
bodies, explicit exports and unread steps"; `OwnersAndRecordsSpec` "a deferred given is
abstract: every class extending it gives one" (the `V`, `TX`, `TL` rows above);
`UnsuppliedWildcardSpec` "an import whose prefix is no path".

Open, for the coordinator: the gate compares counts, not captures. The captures the compiled
front end lists are the labels of the receivers' bindings. Against the old rule (every name
bound from an owner other than the row's own) they differ in three known places, none a count:
a row refused by the model lists none; a second abstract overload is listed under its name, not
`name#2`; a concrete alias of a capture (`val again = captured`) is not listed.

**After S5** (contextual scope, where implicit search has nothing to choose). Every row pairs.
The worse rows are listed corrections: fourteen in cats-core and two in the fixtures.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 978 | 1 / 0 | 0 | 0 | 0 | 3,021 | 507 of 768 (66.0%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 328 | 37 / 3 | 0 | 13 | 0 | 7,270 | 282 of 909 (31.0%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 158 | 3 / 0 | 0 | 0 | 0 | 1,624 | 144 of 368 (39.1%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 129 | 0 / 1 | 0 | 0 | 0 | 567 | 107 of 271 (39.5%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 26 | 0 / 0 | 0 | 0 | 0 | 54 | 12 of 16 (75.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 0 | 19 | 0 of 5 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 3,517 | 68 / 36 | 4 | 1 | 0 | 3,120 | 3,303 of 5,485 (60.2%) |
| fixtures | definitions | 5,588 | 4,923 | 65 / 8 | 0 | 1 | 0 | 591 | 4,342 of 4,513 (96.2%) |
| **all corpora** | methods | 14,232 | 1,619 | 41 / 4 | 0 | 13 | 0 | 12,555 | 1,052 of 2,337 (45.0%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

What S5 reads, with derived examples in `ContextualSpec`: a body is counted, not searched, so a
`using` or `implicit` parameter with a name is a value like any other (of the method, of an
enclosing class's constructor, of a capability); a given alias, an implicit value or method and
an implicit conversion count as the methods they are, and a concrete given supplies nothing to
the members beside it; a given written `= deferred` (`scala.compiletime.deferred`, as the
compiler resolved the name) is a capability of its trait's other members, like an abstract
member; an extension method counts over all its clauses; the members and constructor of a given
instance with parameters count like a class's.

Left uncovered: a contextual parameter without a name (`using T`, a context bound's evidence),
which the old front end binds and then refuses or not by what else implicit search could find
(the competition of givens, imports and parents in scope: not ported); a member of a given
instance without parameters; among a class's members a deferred given without a name, an
abstract given with parameters and an abstract extension method; context function types.

Corrections added by S5 (`corrected.txt`), the rule of the `cats.evidence.As` rows above:

| Row | Old | New |
| --- | --- | --- |
| cats-core `cats.evidence.AsSupport.asFromPredef` | 0 | `?`: sealed family has anonymous instances in its file: As |
| cats-core `cats.evidence.As.refl` | 0 | the same `?` |
| cats-core `cats.evidence.As.witness` | 1 | the same `?` |

Spec ledger after S5:

| | PASS | UNCOVERED | FAIL | PENDING | SCALAMETA |
| --- | --- | --- | --- | --- | --- |
| after S4 | 582 | 871 | 178 | 19 | 81 |
| after S5 | 596 | 857 | 178 | 19 | 81 |

Fourteen examples went from UNCOVERED to PASS; FAIL did not grow.

Found in S5's review, not in any gate input, for the coordinator:

- An abstract given with a `using` clause, as a capability of its class's other members: the old
  front end reads it as a value of its type (`trait E { given cap(using x: Boolean):
  Option[Boolean]; def f: Option[Boolean] }`: 27 = 3^3), where it is a function of its
  parameter (3^9). The old front end itself counts 3^9 for the same member written `implicit
  def`. The compiled front end leaves the row uncovered rather than change the number unlisted.
- `Targets.instances` takes a class for a given instance's when a given of the same statement
  list has its name: `object G { class s; given s: s = new s }` loses the row `G.s.<init>` that
  the old front end has. It predates this stretch.
- Rows of a trait with two givens written `= deferred` count where the old front end refuses
  for re-entry (`given g: Boolean = deferred; given k: Option[Boolean] = Some(g)`: `k` is 3^2):
  right by the rule above, and underived only because no input holds one.

Time and memory of the compiled front end at this commit (`TastyFrontEnd.rows` over the frozen
corpus in one JVM, classes loaded included; the scalameta front end's medians are in "Corpora"):

| Corpus | Rows (methods) | Wall | Peak heap | Scalameta front end |
| --- | --- | --- | --- | --- |
| corpus-core | 4,000 | 3.2 s | 1.25 GiB | 111.2 s, 8.62 GiB |
| corpus-cats-core | 7,651 | 2.6 s | 1.06 GiB | 65.5 s, 5.09 GiB |

The comparison is not like for like yet: the compiled front end measures 978 of core's 4,000
method rows and 328 of cats-core's 7,651 itself, and answers the rest as uncovered at once.

The new front end is 3,087 lines in `core/src/main/scala/cardinality/compiled/` (twelve
files), `Inlines` (about 390) among them.

**Stretch 3, first unit** (the coordinator's decisions on the open points of stretch 2).

- The gate compares the captures of every row both front ends count (a refused row lists none
  on either side). The compiled front end lists each constructor parameter and each
  subclass-supplied member of the classes around a row, and an extension group's own
  parameters. Over all inputs 140 rows differ, all in classes the gate accepts and prints: 3 "a
  second overload by its name" (the old front end lists `name#2`), 34 "the old lists more: an
  alias of a capture" (`val again = captured`), 103 "the new lists more: names the old did not
  bind" (a capture whose type the old reading did not resolve, in a row counted 1 by its
  result). Two listings neither of which holds the other fail the gate; none does. The classes
  are told by the names alone, not checked row by row. Matching the old listing exactly costs
  real code: the alias rule is about 40 lines (a fixed point over a body's values, with the
  type comparison of `CaptureAliases`), and the 103 rows cannot match at all, since they follow
  what the old reading failed to resolve. For the owner: may captures change in these three
  ways?
- TIGHTENED: an old bound and an exact new value within it (`≤2^9` against 257), not only the
  bound's own value. A value above the bound is worse. No input holds a new such row: the four
  tightened rows are those of S3.
- An abstract given with a `using` clause is a function of it. Correction, in `ContextualSpec`
  (no gate input holds one): `trait Fed { given feed(using b: Boolean): Boolean; def get:
  Boolean }`, row `Fed.get`: old 4, new 16. `feed` is one of the 2^2 functions Boolean =>
  Boolean, and `get` a Boolean computed from it: 2^4. The old front end reads `feed` as a
  Boolean (2^2), though it reads the same member written `implicit def` as the function.
- `Targets.instances` and `Readings.instance` take a class for a given instance's only where it
  starts at the given's own place: `object Shared { class s; given s: s = new s }` keeps its row
  `Shared.s.<init>`, and such a given counts as the alias it is.
- A trait with a deferred given beside an alias given that reads it is derived in
  `ContextualSpec`: `given g: Boolean = deferred; given k: Option[Boolean] = Some(g)`, `k` is
  3^2 = 9.

**Stretch 3, second unit** (given instances, context functions, the closed world's final).

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 978 | 1 / 0 | 0 | 0 | 0 | 3,021 | 507 of 768 (66.0%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 328 | 37 / 3 | 0 | 13 | 0 | 7,270 | 282 of 909 (31.0%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 158 | 3 / 0 | 0 | 0 | 0 | 1,624 | 144 of 368 (39.1%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 143 | 0 / 1 | 0 | 0 | 0 | 553 | 121 of 271 (44.6%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 26 | 0 / 0 | 0 | 0 | 0 | 54 | 12 of 16 (75.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 0 | 0 / 0 | 0 | 0 | 0 | 19 | 0 of 5 (0.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 3,572 | 69 / 37 | 4 | 1 | 0 | 3,063 | 3,358 of 5,485 (61.2%) |
| fixtures | definitions | 5,588 | 4,931 | 65 / 32 | 0 | 1 | 0 | 559 | 4,348 of 4,513 (96.3%) |
| **all corpora** | methods | 14,232 | 1,633 | 41 / 4 | 0 | 13 | 0 | 12,541 | 1,066 of 2,337 (45.6%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

- A given instance without parameters is an object: its members count over nothing of its own,
  like an object's with parents (`ContextualSpec`, `ReceiversSpec`).
- A context function type is read as the function type (`A ?=> B` as `A => B`): a body is
  counted, not searched.
- A subtype of a sealed family that no class of the module extends is final in the closed world,
  written `final` or not, whatever members it declares or inherits from a class of the module
  (the coordinator's decision 2). `Stored.unclosed` is deleted. 24 definition rows of the
  fixtures count where the old estimate refuses "a subtype open to extension", each listed in
  `derived.txt` under that one rule (an object is one value, a class what its constructor
  stores: `Boolean` + object 3, two objects 2, `Int` + object `≤2^33`), and three more in
  `TastyFrontEndSpec`. A subtype not written final whose parent is outside the module stays
  uncovered, for parity with its `final` twin, which the old estimate counts.
- One more derived row: `p.g.apply(i: Int): Long` in a given instance, 2^(2^38).

Spec ledger: PASS 606, UNCOVERED 846, FAIL 179, PENDING 19, SCALAMETA 81. Ten examples went
from UNCOVERED to PASS and one to FAIL: `LeafFamiliesSpec` "an extension, a parent's field, an
overload, a later clause", which pins the refusal of a family the closed world now sums (3
against "a subtype open to extension").

Found by review, in no gate input: a family of three `final` classes that each pass arguments
to one stateful module parent (`final class D(val a: Boolean) extends Q2(a, a) with F2`, …) is ω
to the old estimate and 7 to the compiled front end; 7 is right (2 + 1 + 4), each class alone
agrees. It would need a correction if an input held one. A concrete `sealed class` with
subclasses is 2 to both front ends where its values are 5: shared with the old estimate.

What the uncovered rows need at this commit, by first blocker (corpus rows / fixture rows; a row
has a next blocker, so each is an upper bound on what removing it gains):

| Blocker | Corpus | Fixtures | Whose |
| --- | --- | --- | --- |
| an enclosing definition (locals, anonymous classes) | 2,483 | 279 | S4 remainder |
| a class that is no product, sum or type-only template (capability records) | 1,958 | 487 | S4 remainder (records) |
| an enclosing class that inherits and is abstract or may be extended | 1,783 | 342 | the index |
| a class of a dependency | 1,277 | 77 | the sibling's S7 |
| an enclosing class with a bounded or higher-kinded type parameter | 1,231 | 59 | S1's second step |
| a bounded or higher-kinded type parameter | 1,060 | 222 | S1's second step |
| an enclosing sealed class or enum | 1,076 | 33 | S4 remainder |
| a type (unions, singletons, type members, refinements) | 560 | 514 | S1's second step |
| an enclosing class with a member something overrides | 84 | 367 | the index |
| an enclosing object with parents (constructor rows) | 245 | 117 | S4 remainder |
| an enclosing class with a self type | 304 | 16 | S4 remainder |
| a parent with constructor parameters or in a class | 197 | 94 | the index |
| a definition that reaches itself | 119 | 146 | S2 (solver) |
| an anonymous contextual parameter | 100 | 107 | S5 remainder |
| a member the classpath does not hold | 0 | 166 | Q10 |
| a class the algebra does not model | 137 | 20 | S2 |
| a family with an anonymous instance | 110 | 20 | S6 or a model decision |
| a constructor position the method side refuses | 33 | 79 | S2 (solver) |
| an enclosing class (constructor rows in a class) | 57 | 50 | S4 remainder |
| an enclosing class whose construction always throws or binds a pattern | 38 | 39 | S6 |
| a restricted constructor (rule B) | 30 | 45 | S6 |
| an enclosing class with an abstract type member | 31 | 42 | S4 remainder |
| every other reason (about twenty, each under 70 rows) | 204 | 304 | |
| **all** | 13,057 | 3,622 | |

**Stretch 4, first unit** (the capture guard; local definitions). All inputs pass; across the
corpora 1,660 method rows are identical (1,633 before), 1,086 of the 2,337 old numeric rows.

- The gate fails a row whose old capture listing is non-empty and whose new one is empty. A row
  where each side lists a name the other lacks passes only where they share a name (one row,
  cats-eo `Plated.transform.machine.drive`: the old lists an alias, the new a named `using`
  parameter the old did not bind). Capture differences at this commit: 3 second overloads, 34
  aliases only the old lists, 114 names only the new lists, 1 of both.
- A local definition (a `def` or `val` written in a method body) counts over its own parameters
  and those of every method it is written in, whose type parameters are atoms, then over the
  classes around the outermost method; its captures are those parameters' names
  (`ReceiversSpec.locals`: `inner(x: Boolean): Boolean` in `outer(y: Boolean)` is 2^(2 × 2) =
  16). A value a definition is written in adds nothing. An inline method around it exempts the
  row from the never-built rule, as for the method itself.
- Left uncovered, each by its reason: a definition in a function literal (its parameter may be a
  generator's binder, which the old front end refuses); in a case clause; in a constructor's
  body, a class-body statement included (it runs before the instance holds what its members
  read: the old front end counts over the constructor's parameters alone); in the initializer of
  a template that is never built; a parameter's name that another definition or binder of the
  outermost method has too (`a capture another definition may hide`: 122 corpus rows); a class
  in a body.
- One derived row: `ex.Local.outer.succ`, a local inline method, 2^(2^69).
- Spec ledger: PASS 612, UNCOVERED 839, FAIL 180, PENDING 19, SCALAMETA 81. Six examples went
  to PASS and one to FAIL: `SyntaxFeaturesSpec` "a binder behind a reading that stops is
  refused", which pins a refusal for a local definition the compiled tree resolves.
- What item 1 really needs: of the 2,483 corpus rows blocked by "an enclosing definition", 2,418
  are members of a class in a body (anonymous instances), now named `an enclosing class in a
  body`. Local methods themselves were 27 rows.

**Stretch 4, second unit** (members of anonymous classes). All inputs pass; across the corpora
1,723 method rows are identical (1,660 before), 1,120 of the 2,337 old numeric rows.

- An anonymous class (`new T { … }`) is final: where its inherited members are implemented and
  no parent of the module holds state, a member of it counts over its own parameters, those of
  every method the instance is written in, and the classes around the outermost method
  (`ReceiversSpec.locals`: `run(b: Boolean)` in `mk(y: Boolean)` is 16). A template inside such
  a class reads the same methods.
- Left uncovered: a local named class or object; a parameter of an enclosing method named like
  a member that a class written in the method inherits (`hashCode`, a parent's `helper`: the
  compiler rejects the reference, so the parameter cannot be read there).
- 46 rows of cats-core count where the old front end refuses (members of the anonymous type
  class instances `catsInstancesForId`, `ArraySeqInstances.stdInstances` and
  `Align.alignWithIterator`: "inherited parent instantiated twice", or a capture a parent
  outside the sources may hide), each listed in `derived.txt` under one rule with its count's
  reason (1: the signature leaves one implementation; ω: a sequence or an iterated function;
  0: nothing total builds the result; `size` 2^64; `get` 2^(2^64); `isEmpty` 2).
- Two corrections, both the rule of `cats.data.Ior` (A + B + A × B, which the old reading took
  for empty): `cats.catsAlignForId.<new#1>.align[A, B](fa: Id[A], fb: Id[B]): Id[Ior[A, B]]`,
  0 becomes 3 (`Left(fa)`, `Right(fb)`, `Both(fa, fb)`); `ArraySeqInstances.stdInstances.
  <new#1>.align`, 1 becomes ω (a sequence of any length, where the old saw the empty one).
- Spec ledger after the unit: see the totals in the report of the stretch; examples that pin a
  refusal the compiled tree resolves, to rewrite at the switch: `LeafFamiliesSpec` "an
  extension, a parent's field, an overload, a later clause", `SyntaxFeaturesSpec` "a binder
  behind a reading that stops is refused".

**Stretch 4, third unit** (the hidden-capture check follows scopes). A captured parameter is
refused only where a scope on the path from its method to the definition binds the name again:
a block of the path, an inner method's parameters, a class's members or self alias, the
definition's own parameters (`a capture a nearer scope binds again`, was `a capture another
definition may hide`). A same-named definition elsewhere in the method hides nothing. 37 more
corpus method rows are identical (1,760); no row is worse.

**Stretch 4, fourth unit** (capability records). All inputs pass.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 1,357 | 1 / 0 | 0 | 0 | 0 | 2,642 | 542 of 768 (70.6%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 396 | 42 / 47 | 0 | 15 | 0 | 7,151 | 320 of 909 (35.2%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 176 | 3 / 0 | 0 | 0 | 0 | 1,606 | 162 of 368 (44.0%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 157 | 0 / 1 | 0 | 0 | 0 | 539 | 135 of 271 (49.8%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 31 | 0 / 0 | 0 | 0 | 0 | 49 | 16 of 16 (100.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 4 | 0 / 0 | 0 | 0 | 0 | 15 | 4 of 5 (80.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 3,820 | 72 / 41 | 4 | 2 | 0 | 2,807 | 3,586 of 5,485 (65.4%) |
| fixtures | definitions | 5,588 | 4,931 | 65 / 32 | 0 | 1 | 0 | 559 | 4,348 of 4,513 (96.3%) |
| **all corpora** | methods | 14,232 | 2,121 | 46 / 48 | 0 | 15 | 0 | 12,002 | 1,179 of 2,337 (50.4%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

- A class or trait of the module with members, named by a type (`Shapes.record`), is a
  capability record: a trait or abstract class is the tuple of its public constructor fields and
  of what its abstract members supply (a value, or one function from all of a method's
  parameters; a one-valued result supplies nothing); a concrete class nothing extends is the
  tuple of its public fields; a class written `final abstract`, or whose construction always
  throws, is the empty sum, as the old front end reads it. A member restating one of `Any`'s
  (`toString`, `hashCode`, `equals(Any)`, by name and parameters) is outside the model; every
  other member counts. `Shapes.member` now reads a supplied member for receivers and records
  alike (`RecordsSpec`).
- Left uncovered, each named `a capability record …`: with parents (382 corpus rows), with a
  parameter that is no public field (336, rule B's observers), with a restricted constructor
  (261), with a self type (296), with a member named like the measured definition (119), with
  an abstract type member, with a member hidden at the use site, with a member something
  overrides, of a sealed class, of a class something extends, whose construction binds a
  pattern, whose overriders are not read.
- Correction: fixture `5a9bcf40…/Capability.scala:3` `U.eq2[A](e: Eq2[A]): Boolean`, 2 becomes
  4. `trait Eq2[A] { def x: A; def canEqual(that: Any): Boolean }` has no parent, so `canEqual`
  restates nothing: it is an abstract member any implementation supplies, a Boolean of a
  one-valued input. The model excludes only `Any`'s universal members; the old reading dropped
  `canEqual` by its name. The ledger example `CapabilitySpec` "`Any`'s and `Product`'s members
  are universal (`canEqual`)" pins the old 2: a spec to rewrite at the switch, with
  `MethodAnalysisSpec` "do not project an abstract case class as a product" (now 1, derived:
  `Holder[A]`'s abstract `value: A` is the record) and `InlineScopesSpec` "a qualifier is
  converted from its own type's implicit scope" (an inline method the record reading now
  counts, derived).
- Three rows derived: `extract[A](h: Holder[A]): A`, 1; `app.C.onT` and `app.C.hOnT`, inline
  methods over a final class with no field, 2^(2^37) each.
- Spec ledger: PASS 637, UNCOVERED 811, FAIL 183, PENDING 19, SCALAMETA 81.

**Stretch 4, fifth unit** (sealed and enum receivers). All inputs pass.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 1,380 | 1 / 0 | 0 | 0 | 0 | 2,619 | 559 of 768 (72.8%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 488 | 42 / 55 | 0 | 58 | 0 | 7,008 | 360 of 909 (39.6%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 176 | 3 / 0 | 0 | 0 | 0 | 1,606 | 162 of 368 (44.0%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 157 | 0 / 1 | 0 | 0 | 0 | 539 | 135 of 271 (49.8%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 31 | 0 / 0 | 0 | 0 | 0 | 49 | 16 of 16 (100.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 4 | 0 / 0 | 0 | 0 | 0 | 15 | 4 of 5 (80.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 3,835 | 72 / 41 | 4 | 2 | 0 | 2,792 | 3,600 of 5,485 (65.6%) |
| fixtures | definitions | 5,588 | 4,931 | 65 / 32 | 0 | 1 | 0 | 559 | 4,348 of 4,513 (96.3%) |
| **all corpora** | methods | 14,232 | 2,236 | 46 / 56 | 0 | 58 | 0 | 11,836 | 1,236 of 2,337 (52.9%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

- A member of a sealed family or of an enum counts with its receiver as one value of the
  family's sum, which case and what that case holds (`Receivers.split`, `Shapes.receiver`), as
  the old front end splits it; the family's own fields and members are not bound beside it, and
  what it inherits or a case overrides computes from the case. The row captures `<receiver
  full.Name>`. A sum this reading does not take leaves the row uncovered (`an enclosing sum not
  read`), never refused: the old front end still counts a row whose goal stands without the
  split. A concrete sealed class and an enum case's own class stay uncovered.
- 43 corrections, two rules (`corrected.txt`):
  - the Ior rule (39 rows of cats-core, receiver members of `cats.data.Ior` such as `fold` 1 to
    3, `putLeft` 1 to 9, `isLeft` 1 to 8, `left` 1 to 4, `pad` 1 to 16, `exists` 1 to 32,
    `toList` 1 to ω; members of the instances `IorInstances…` such as `size` 1 to 2^192 and
    `get` 1 to 2^(2^65); `IorFunctions.left` and `right` 0 to 1; `Validated.toIor` 0 to 1).
    `Ior[A, B]` is A + B + A × B; the old reading took it for empty, so it counted the one
    function from it, or nothing into it. A reviewer re-derived sixteen of them by hand.
  - the As rule (4 rows: `As.andThen`, `As.compose`, `AsInstances.liskov.<new#1>.id` and
    `.compose`, old 1 or 0, new the model's `?` "sealed family has anonymous instances in its
    file: As"), as in stretch 2.
- Eight rows derived: members of the anonymous type class instances in `ConstInstances` and
  `ValidatedInstances`, whose receiver is now the one-case sum of a sealed abstract class
  (`foldLeft` 1, `size` 2^64, `get` 1, `mapAccumulate` 1, `bifoldLeft` ω, `bimap` 1, `leftMap`
  1).
- Spec ledger: PASS 640, UNCOVERED 808, FAIL 183, PENDING 19, SCALAMETA 81. Capture
  differences: 3 second overloads, 42 aliases only the old lists, 129 names only the new lists,
  2 of both.

**Stretch 5, first unit** (higher-kinded type parameters). All inputs pass.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 1,380 | 1 / 0 | 0 | 0 | 0 | 2,619 | 559 of 768 (72.8%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 552 | 42 / 57 | 0 | 58 | 0 | 6,942 | 385 of 909 (42.4%) |
| cats-core | definitions | 1,559 | 1,318 | 21 / 0 | 0 | 1 | 0 | 219 | 1,048 of 1,053 (99.5%) |
| cats-kernel | methods | 1,785 | 176 | 3 / 0 | 0 | 0 | 0 | 1,606 | 162 of 368 (44.0%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 171 | 0 / 1 | 0 | 0 | 0 | 525 | 149 of 271 (55.0%) |
| cats-eo | definitions | 151 | 128 | 6 / 0 | 0 | 0 | 0 | 17 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 31 | 0 / 0 | 0 | 0 | 0 | 49 | 16 of 16 (100.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 4 | 0 / 0 | 0 | 0 | 0 | 15 | 4 of 5 (80.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 3,927 | 72 / 41 | 4 | 2 | 0 | 2,700 | 3,691 of 5,485 (67.3%) |
| fixtures | definitions | 5,588 | 4,931 | 65 / 32 | 0 | 1 | 0 | 559 | 4,348 of 4,513 (96.3%) |
| **all corpora** | methods | 14,232 | 2,314 | 46 / 58 | 0 | 58 | 0 | 11,756 | 1,275 of 2,337 (54.6%) |
| **all corpora** | definitions | 2,677 | 2,130 | 30 / 0 | 0 | 1 | 0 | 516 | 1,746 of 1,807 (96.6%) |

- A type parameter of a measured definition, of a method around it or of a receiver whose bounds
  are those of any invariant type constructor of proper, unbounded parameters (`F[_]`,
  `G[_, _]`) is a rigid constructor (`Shapes.binder`), and `F[X]` is the uninterpreted
  application over `X`'s key (`Shapes.key`): a binder is its atom, a static class or an opaque
  alias outside its scope is its name over its arguments' keys, an alias is followed (`Id[A]` is
  `A`), an opaque alias inside its scope is its representation. A variance on the constructor's
  parameter (`F[+_]`: subtyping between applications), a path-dependent type, a union, a
  singleton or a refinement as an argument, and constructor rows of such classes stay uncovered.
  tasty-query keeps a lambda parameter's variance private: `tastyquery/Variances.scala` reads it
  from inside the package, and through the flags the bytecode keeps public.
- Two rows derived: `IndexedStateTContravariantMonoidal.unit` and `.product`, 1 each: a sealed
  abstract class that nothing extends has no case, so its members are never called.
- An open point for the owner: `F[Any]` against `F[AnyRef]` is 0 to both front ends, the two
  being distinct keys; AGENTS.md reads `AnyRef` as `Any` everywhere, so it might be 1.
- Spec ledger: PASS 673, UNCOVERED 775, FAIL 183, PENDING 19, SCALAMETA 81.

**Merge of `tasty-gate` at `7aba1c6`** (the closed-world index `ClosedProgram` and dependency
classes `DependencyClasses`, wired in by the gate agent). Conflicts resolved in `Receivers`
(a sum receiver skips the inheritance reading and the overriding check together; the Unknown
overriders check stays for every receiver; inherited abstract members of a template that is
not fixed are capabilities beside the members `Shapes.member` reads), `Shapes` (a record asks
`world.overriding`), the two lists (both sides kept) and `ReceiversSpec`. With members of
anonymous classes now read, cats-core's anonymous instances ask the index about parents outside
the module, which lists the classpath: a second standard-library jar declaring `scala.caps`
again is refused by tasty-query, and is not reported as a lost class (the context holds it).
34 rows the two branches open together are derived (the anonymous `Either` instance's members,
the `NonEmptyParallel` zip instances, `Other.get` 0 over a rigid `G[Option[B]]`, `m.M.onPlain`).
The full gate ran on the shared assets and refreshed the coverage memory.

| Input | Table | Rows | Identical | Better: resolution / model | Tightened | Worse (corrected) | Unpaired | Uncovered | Old numeric identical |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| core | methods | 4,000 | 1,384 | 1 / 0 | 0 | 0 | 0 | 2,615 | 559 of 768 (72.8%) |
| core | definitions | 641 | 369 | 3 / 0 | 0 | 0 | 0 | 269 | 347 of 400 (86.8%) |
| cats-core | methods | 7,651 | 629 | 42 / 213 | 0 | 62 | 0 | 6,705 | 392 of 909 (43.1%) |
| cats-core | definitions | 1,559 | 1,357 | 21 / 0 | 0 | 1 | 0 | 180 | 1,049 of 1,053 (99.6%) |
| cats-kernel | methods | 1,785 | 185 | 3 / 0 | 0 | 0 | 0 | 1,597 | 164 of 368 (44.6%) |
| cats-kernel | definitions | 312 | 307 | 0 / 0 | 0 | 0 | 0 | 5 | 253 of 253 (100.0%) |
| cats-eo | methods | 697 | 182 | 0 / 3 | 0 | 0 | 0 | 512 | 152 of 271 (56.1%) |
| cats-eo | definitions | 151 | 130 | 6 / 0 | 0 | 0 | 0 | 15 | 91 of 94 (96.8%) |
| cats-eo-generics | methods | 80 | 31 | 0 / 0 | 0 | 0 | 0 | 49 | 16 of 16 (100.0%) |
| cats-eo-generics | definitions | 12 | 7 | 0 / 0 | 0 | 0 | 0 | 5 | 6 of 6 (100.0%) |
| cats-eo-schemes | methods | 19 | 4 | 0 / 0 | 0 | 0 | 0 | 15 | 4 of 5 (80.0%) |
| cats-eo-schemes | definitions | 2 | 1 | 0 / 0 | 0 | 0 | 0 | 1 | 1 of 1 (100.0%) |
| fixtures | methods | 6,746 | 4,131 | 110 / 46 | 4 | 3 | 0 | 2,452 | 3,893 of 5,485 (71.0%) |
| fixtures | definitions | 5,588 | 4,931 | 66 / 32 | 0 | 1 | 0 | 558 | 4,348 of 4,513 (96.3%) |
| **all corpora** | methods | 14,232 | 2,415 | 46 / 216 | 0 | 62 | 0 | 11,493 | 1,287 of 2,337 (55.1%) |
| **all corpora** | definitions | 2,677 | 2,171 | 30 / 0 | 0 | 1 | 0 | 475 | 1,747 of 1,807 (96.7%) |

Spec ledger: PASS 727, UNCOVERED 711, FAIL 193, PENDING 19, SCALAMETA 81 (from 673 / 775 /
183). Ten examples went from UNCOVERED to FAIL, each pinning a refusal of the old reading that
the two branches now count: `AliasOverrideIdentitySpec` (five guards of alias, type-lambda and
expansion limits), `DefinitionInventorySpec` "an unmatched inherited given leaves the identity
unresolved", `DependencyRowsSpec` "a dependency's template is the one of its qualified name",
`GroundEnvironmentSpec` "an overload is a second member; another shape is refused",
`HigherKindedBindingSpec` "inherited substitution keeps nominal identity" (`Other.get` 0,
derived) and `StdlibModelSpec` "IterableOnce leaves iterator to the sources". They go on the
rewrite-at-switch list; the last one (a Boolean counted ω over an inherited `iterator`
capability) is for the gate agent to confirm, being its reading of inherited members.

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
