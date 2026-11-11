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

What phases 4 to 6 are measured against, and how to rebuild it. Everything here was produced with
the scalameta front end at `5f89e91`.

### Commands

| Command | What it does |
| --- | --- |
| `scripts/measure-corpus.sh <corpus> <report file> [runs]` | One `Report.of` per run over a frozen corpus with this checkout's build, each in a JVM of its own (`bench/runMain cardinality.bench.CorpusRun`, `-Xmx16g -Xss8m`). Only the call is timed; it includes rendering the dependency stubs. Prints each run's wall time, CPU time, GC time, peak heap and peak RSS, the median and range of the first, second and fourth, the rows by kind, and what the report shows of the compiled types; writes the rendered report; fails when two runs render different reports. |
| `scala-cli run .claude/skills/cardinality-reduction/report-diff.scala -- <before> <after> --rows` | Totals of two rendered reports and every row that moved. |
| `scripts/build-corpus.sh <new corpus directory> <organization:artifact_3:version>` | A corpus from a published artifact: its binary jar unpacked as `classes/`, each source of its sources jar at the path its TASTy records, and its dependency jars in `classpath.txt`. Nothing is compiled. |
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

All under `/var/cache/users/rhansen/cardinality-tasty/`, with the report of each at `5f89e91` in
`baseline/<name>-report-5f89e91.txt` beside them.

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
| cats-eo-schemes | 4 · 0 · 0 · 15 | 4 of 19 | 1 · 0 · 0 · 1 · 0 | 15.8 (15.8 to 16.4) | 74.7 (72.6 to 74.9) | 3.08 (3.05 to 3.15) |
| cats-kernel | 280 · 88 · 0 · 1417 | 368 of 1785 | 77 · 0 · 0 · 59 · 176 | 33.2 (29.2 to 34.8) | 120.9 (115.8 to 142.3) | 3.06 (2.93 to 4.09) |
| cats-core | 705 · 204 · 0 · 6742 | 909 of 7651 | 494 · 2 · 0 · 506 · 557 | 65.5 (64.4 to 81.4) | 328.0 (324.0 to 371.9) | 5.09 (4.70 to 5.15) |

- The three runs of every corpus rendered the same report. Core's is the earlier baseline
  (`core-report-ffb94c1.txt`) byte for byte once the location prefix is removed: the harness in
  the repository reads what the earlier script read.
- Compiled types reached every corpus but the smallest: 423 rows print one in core, 17 in cats-eo,
  4 in cats-eo-generics, 61 in cats-kernel, 136 in cats-core, and no row of those five is left
  `<inferred>` but 5 of cats-core's 7,651, whose compiler type has no spelling here.
- cats-eo-schemes is the exception and gates almost nothing. Its one untyped member
  (`Schemes.OnStackLimit`) stays `<inferred>` although its classes match (tasty-query finds it at
  the same offset and end): `CompiledTypes` writes a pattern name's function type without
  parentheses (`val ((kids: …), (combine: PSVec[R] => R)) = …`), the edited source does not parse,
  and every type of that file is dropped without a warning. A TASTy front end has no such step, so
  this row should come out better.

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

### Test fixtures that do not compile

`cardinality.Compiled` compiles every fixture before the analysis reads it, and a TASTy front end
can read only the ones that compile. At `5f89e91` the suite asked for 1,493 fixtures and the
compiler rejected 203 of them.

| Why the fixture did not compile | Fixtures | Now |
| --- | --- | --- |
| It names a declaration the analysis is not supplied, on purpose | 133 | 125 compile; 8 left |
| It spells the kind-projector placeholder (`F[A, *]`) | 7 | 7 compile (`-Xkind-projector`) |
| It is not valid Scala by design: the analyzer must refuse what scalac rejects | 48 | left |
| It is ill-typed beside the test's point (a body, a member not implemented, a missing language import) | 14 | left |
| The compiler does not finish in 60 s | 1 | left |

The suite now asks for 1,496 fixtures: 1,424 compile and 72 are rejected, the 71 left above and
one `CompiledSpec` rejects on purpose. To audit again, start from an empty cache: a fixture's
`failed` marker holds the test that first asked for it and what stopped the compiler.

```bash
rm -rf core/target/compiled-fixtures && sbt core/testFull
grep -h '^asked by' core/target/compiled-fixtures/*/failed   # one line per rejected fixture
```

- **Withheld support.** A fixture's text carries the declarations it names and the analysis is
  not to have: `code + Compiled.withheld("package ext\nclass Schema")`. `Compiled.inputs` parses
  the support as a source of its own under `withheld/`. The harness compiles the support first,
  compiles the fixture against its classes, and gives the analysis neither the support's text nor
  its classes. The cache entry keeps them apart: `src/withheld` and `support` beside `src` and
  `classes`. A source set that also goes to a reader which compiles nothing takes the support as
  a file of its own (`Compiled.withheldFile`), which that reader leaves out (`Rows.rowsOf`).
- **What pins it.** A fixture that has support and is still rejected fails its test, so a
  migrated fixture cannot fall back to being read uncompiled. `CompiledSpec` pins the rest: the
  same declaration is `?` withheld and 4 supplied, and a text that carries support does not parse
  when it is read without `Compiled.inputs`.
- **No expectation changed.** Each migrated example still asserts its refusal (`declaration not
  supplied: ext.Schema`, `unresolved type: Two (may be imported by zzz.*)`, `inherited parent
  declaration not supplied: lib.Base`, …), now over a fixture that compiles. A support declares
  only what the fixture names, so the compiled program binds every other name as the example
  assumes (`import ext.*` beside a `ext` that declares no `Any`).
- **Fixtures left, by example.** They go with uncompiled support unless rewritten.
  - Unsupplied on purpose, left (8). The point is a source that does not compile, so that its
    untyped members refuse: `InferredResultsSpec` (`callBindings`, `unsuppliedOverride`),
    `PureCodeSpec.reboundNames`. Scalac rejects the fixture with the declaration supplied too:
    `AliasOverrideIdentitySpec.nearerImport`, `SingletonIntersectionMethodSpec.marker`. The
    declaration is supplied to the analysis as a dependency stub, which is not compiled:
    `ClosedWorldSpec` (`stubOwners`, `stubParents` twice).
  - Not valid Scala by design (48): `AliasOverrideIdentitySpec` (`arity`, `mutual`, `opaque`,
    `recursive`), `BinderBoundsSpec` (`cyclicBounds`, `invalidTerminal`),
    `ContextualOverrideIdentitySpec` (`inlineParameter`, `ordinary`),
    `EnvironmentCompletenessSpec` (`exportCycles`, `exportOutranks`, `exportRefusals`),
    `ExistentialInputSpec.aliasArgument`, `GeneralInfixApplicationSpec` (`arity` twice,
    `qualified`), `GroundEnvironmentSpec.notBuiltin`, `GroundTypesSpec.closedRepeated`,
    `HigherKindedRefusalSpec` (`argumentKinds`, `arity`, `builtinArity`, `constructorsAsTypes`,
    `kindMismatch`, `properUse`, `viewBound`, `wildcard`), `InlineScopesSpec.localObject`,
    `MethodAnalysisSpec.recursive`, `MethodFeatureIntegrationSpec` (`privateRefinedMember`,
    `privateRepeatedAlias`), `MethodRefinementSpec` (`hiddenEquality`, `recursive`,
    `shadowedBinder`), `MethodTypePlaceholderSpec.arity`,
    `OverrideIdentityImplementationSpec.fileIdentity`, `OwnersAndRecordsSpec.deferredBare`,
    `PolyLambdaCardinalitySpec.arity` (twice), `PureCodeSpec.varPath`,
    `SingletonIntersectionMethodSpec` (`access` twice, `alias`, `binder`, `negative`),
    `StdlibModelSpec` (`gap`, `inaccessibleSingleton`, `javaWildcard`, `relativePaths`,
    `unapplied`).
  - Ill-typed beside the point (14), each a small rewrite away from compiling:
    `DefinitionInventorySpec.contextLiterals`, `GroundEnvironmentSpec` (`dynamicParent`,
    `nestedParentName` twice), `InlineMethodsSpec.searchForms`, `InlineScopesSpec.localClauses`,
    `MethodAnalysisSpec` (`hiddenField`, `privateConstructor`),
    `OpaqueTypeScopeSpec.inheritedBoundary`, `OwnersAndRecordsSpec.emptyFamily`,
    `StdlibModelSpec` (`dependencyNamespaces`, `wildcardParent`), `TopTypesUnionSpec`
    (`referenceTops`, `shadowInherited`).
  - Not finished in 60 s (1): `ContextBindersSpec.limits`.
- **Fixtures the cache never sees.** About twenty spec files read sources without compiling them:
  the nine stored-value specs on `capacity.Rows` (`rows`, and `rowsOf`, which is `Report.of` over
  files with no classes), and `CounterSpec`, `CardinalitySpec`, `SignatureSpec`, `ReportSpec` and
  others through `Counter.definitions` or `Report.of`. Those fixtures are in no cache entry, so
  neither the differential runner nor a TASTy front end reads them as they are.

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
  The 71 that still do not compile go with uncompiled support.
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
