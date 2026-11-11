# Compiled own types: reading the module's TASTy

**Status:** phases 1 to 3 implemented (2026-11-10/11). Phases 4 to 6 are planned: a TASTy front
end replaces the scalameta one (owner decisions, 2026-11-11, under "Decisions").

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
- Core's suite: 87 specs in about 100 s with the fixture cache warm (1,490 compiled fixtures).

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

## Phase 4: the boundary and a differential gate

No behaviour change. This is what makes "compiled code must not degrade" checkable during a
rewrite: the old front end is the oracle until the new one matches it.

| Step | What |
| --- | --- |
| 4a. Front-end interface | One entry point the report calls, returning `Entry` and `Definition` rows for a compiled module. The scalameta pipeline becomes its first implementation, unchanged. |
| 4b. Differential runner | Runs two front ends over the same input and classifies every row: identical; better (`?` → number, or a refusal whose reason is a later one); **worse** (number → `?`, or a different number). Inputs: the frozen self-measurement corpus and every compiled test fixture (the 1,490 in `core/target/compiled-fixtures`). |
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
better row is recorded with its derivation. Rows a slice does not cover yet stay with the old front
end.

| Slice | What it reads from symbols and typed trees | Replaces |
| --- | --- | --- |
| 5a. Types to shapes | A compiled type as a `Shape`: ground types, products, sums, functions, type parameters, aliases dealiased by the compiler, opaque types by scope (rule F), the stdlib model keyed by symbol, top types by place. | The type reading in `analysis/resolution` (8,950 lines) and `types` (2,264), and every lookup by name: D3 is this slice. |
| 5b. Definitions table | Class symbols: fields, constructors, sealed children, enum cases, public factories (rule B), recursion. | `capacity` (6,449) but for `Size`. |
| 5c. Method environments | A target's inputs, receiver fields, captures and enclosing scopes; abstract and overridden members as capabilities; the closed world over exact parent and override symbols; given names; context-function binders from the declared type. | `analysis` top level (5,583), `analysis/capability` (2,517), `analysis/model` (1,530); override identity and given names (the first plan's phase 3 items) are symbol queries here. |
| 5d. Bodies | Typed trees where the contract reads a body: inline bodies and the inline methods they call (compile-time operations are symbols and tree nodes), the conversions the body applies (D2), the givens it resolved (D4), factory bodies, constructor statements, defaults, capture aliases. | `analysis/expansion` (3,747), including the hand-written `LibraryInlines` table. |
| 5e. Dependencies as symbols | A dependency's declarations read from its symbols, with the stubs' guards kept: no body is supplied, no signature guessed. | The stub rendering and parsing in `tasty` (1,499); a report no longer parses about 5,200 stub sources. |

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
  compiler already did), but by how much is an estimate until 5a and 5b exist: perhaps a third of
  today's 30,000 lines.
- **Rules worded in syntax.** Some contract rules speak of what is "written" or "spelled" (rule T's
  matching, the simple-name closed world, D43's candidates, the builtin-shadowing rule). Each is
  restated over symbols when its slice lands, in `v1.md` and `AGENTS.md`.
- **tasty-query coverage.** It already reads Scala 3 TASTy, Scala 2 pickles and Java classes for
  the stubs; typed trees of bodies (5d) are new use.
- **Performance.** Unknown until measured. No stub rendering or parsing should help; reading
  typed trees for every body the contract reads may cost.

## Not worth reading from TASTy

The analyzer's own model, not a compiler fact: `StuckProjections` (the `proj:` atom),
`LiteralTypes` (already exact), the shape logic in `SingletonIntersections`, `TypeMembers` and
`RefinedMembers` (path-keyed atoms), `Representations`, `Callees` (type-level dispatch), and the
contextual evidence for hypothetical bodies.

Finality never comes from TASTy flags. The closed world's `final`s exist only in the analyzer.
