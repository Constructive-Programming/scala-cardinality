# Compiled own types: reading the module's TASTy

**Status:** phases 1 to 3 implemented (2026-11-10/11). Phases 4 to 6 are planned (re-planned
2026-11-11); phase 6 waits on the owner decisions listed under "Decisions pending".

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
  Fixtures that name an unsupplied declaration on purpose no longer compile, so their untyped
  members refuse.

The override-identity and given-name code first listed under this phase needs symbol facts a
source rewrite cannot carry. It moved to phase 4, behind the symbol index.

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
- **The goal is deletion.** A phase that only adds a second path is not done until the syntax path
  it replaces is deleted.

## Mechanism: a symbol index (recommended; decision D1)

Types can be written into the source. Symbol facts cannot: which declaration a name denotes, which
inherited member a method overrides, a class's exact parents, the conversions the typer inserted.
Phases 4 to 6 read those from a **symbol index**: built in the same TASTy walk as `CompiledTypes`,
keyed by source file and offset, queried by the analyzer where it resolves a name or a member today.
The analyzer keeps its scalameta trees. Each use of the index deletes the syntax code that answered
the same question.

## Phase 4: total typing, the index, override identity, given names

No contract change. Expected report movement: none.

| Step | What | Deletes |
| --- | --- | --- |
| 4a. Total typing | After `CompiledTypes`, every definition has a written type. A type with no spelling, a `transparent inline` result and a member of a source not compiled get a reserved marker type (as stubs use `_root_.SignatureNotRead`), which refuses by name. | The "no written type" branches: 166 `decltpe` sites in 42 files to review, 4 refusal sites (`ContextBinders`, `Measurement`, `BodyTerms`, `MemberSyntax`). |
| 4b. Symbol index | Offset → symbol facts for the module's own trees: owner, overridden symbols, the symbol each written name resolves to. | Nothing by itself: groundwork. |
| 4c. Override identity | Which inherited declaration a member overrides or implements: the symbol's overridden symbols. | `OverrideIdentity` (307), `TypeIdentities.Overrides` (about 200), the matching in `FinalImplementations` (about 60), `Measurement` 226-244: about 550. |
| 4d. Given names | An anonymous given's name: the symbol's. | `GivenNames` (105). |

## Phase 5: exact structure

Contract text changes (the simple-name closed world becomes exact for compiled sources), no owner
decision needed. Expected movement: refusals leave, rows move to their next reason or to a number.

| Area | Fact | From the index | Deletes |
| --- | --- | --- | --- |
| `ClosedWorld` Ref/Node matching by simple name, for sources | Which templates extend which | Exact parent symbols (stubs are already exact, `53ab312`) | About 80, and the "same-named templates share subclasses" overcount. Writing `final` stays the analyzer's. |
| `Linearization`, `InheritedScopes`, `InheritedTerms` (foreign), `ForeignParents` | Linearization, parent type arguments, members of library parents | The class symbol's linearization and parent types | About 200. |
| `ReceiverCalls` 76-170, `Construction` | Callee of a call, by-value or by-name, throwing statements | The applied symbol and its parameter types | About 150. |
| `ContextFunctions` 121-318 | Whether an expected type, dealiased, is a context function | The dealiased declared type (rule F: never through an opaque type outside its scope) | About 180. |

## Phase 6: the large ones

The most code, and where `analysis/expansion` sits: 3,747 lines, 98% of its statements unexecuted
on core's own sources (2026-11-11 coverage run), so the least exercised code on real input.

| Step | What | Deletes | Waits on |
| --- | --- | --- | --- |
| 6a. `LibraryInlines` | Whether a standard-library member is inline: its symbol's flag, read from scala-library's TASTy. | The hand-written table (357). | Nothing. |
| 6b. Conversion and extension candidates | The conversions and extensions a body meets. | `Conversions` (525), `Reachable` (504), `RestatedExtensions` (186), `Unconverted` (91), `Insertions` (101), parts of `TargetViews`: about 1,300, less the new reading of applied conversions. | D2 |
| 6c. Name resolution | Which declaration a written name denotes. | `ShadowedBuiltins` (761), `LookupBindings` (216), `SuppliedNamespaces` (81), `ImportBinding` (105), `ImportShadowing` (28), most of `SourceLookup` (802), `TermMembers` (167), `InheritedTypeMembers` (480): about 2,000. Lookup of rooted names the analyzer builds itself stays. | D3 |
| 6d. Inline-body reading | Compile-time operations in an inline body and the inline methods it calls. | About 1,500 of `InlineBodies`, `BodyWalk`, `BodyTerms`, `BodyTypes`, `Calls` (2,045). `transparent` calls arrive already expanded in TASTy and are read as written there. | D4 |

## Decisions pending (owner)

| | Decision | Options |
| --- | --- | --- |
| D1 | How compiled facts reach the analyzer | **Symbol index** beside the scalameta trees (recommended): incremental, every step gated, about 5,500 lines removable in all. **TASTy front end**: the analyzer reads typed trees directly; removes far more (most of `analysis/resolution`, `expansion` and the stub rendering) but is a rewrite of the front half, with every count at risk until it converges. **Source rewriting only**: stops after 4a and 4d. |
| D2 | D43, the conversions and extensions a body may meet | **Rewrite to "the conversions the compiled body applies"** (recommended): a subset of today's candidates, so refusals only leave; deletes about 1,300 lines; the contract text changes and each moved row is explained. **Keep the candidate superset**: no movement, the code stays. |
| D3 | A builtin name the compiler binds differently from the rule "builtin names are never shadowed by what is not supplied" | **The compiler's binding wins for compiled code** (recommended): the rule remains for uncompiled sources only; unlocks 6c. A row that today counts `scala.Int` where the program means another `Int` becomes that declaration's count, or `?` when it is not rendered: a correction, not a degradation. **Keep the rule over the compiler**: `ShadowedBuiltins` stays and 6c shrinks to about half. |
| D4 | A given left to implicit search in an inline body | **Read the given the compiler resolved** (recommended): refuse only when that given is inline with a compile-time operation, as for a written call; rows only gain numbers. **Keep refusing every such given**: the index must tell synthesized arguments apart; no movement. |

## Order

4a → 4b → 4c → 4d → 5 → 6a → 6b (D2) → 6c (D3) → 6d (D4). Each step lands with: the syntax code
deleted, the frozen-corpus report compared with the previous step, core's suite, the plugins'
tests, CPD, and the SemanticDB dead-code scan clean.

## Not worth reading from TASTy

The analyzer's own model, not a compiler fact: `StuckProjections` (the `proj:` atom),
`LiteralTypes` (already exact), the shape logic in `SingletonIntersections`, `TypeMembers` and
`RefinedMembers` (path-keyed atoms), `Representations`, `Callees` (type-level dispatch), and the
contextual evidence for hypothetical bodies.

Finality never comes from TASTy flags. The closed world's `final`s exist only in the analyzer.
