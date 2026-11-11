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
  Fixtures that name an unsupplied declaration on purpose no longer compile, so their untyped
  members refuse.

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

## Phase 4: the boundary and a differential gate

No behaviour change. This is what makes "compiled code must not degrade" checkable during a
rewrite: the old front end is the oracle until the new one matches it.

| Step | What |
| --- | --- |
| 4a. Front-end interface | One entry point the report calls, returning `Entry` and `Definition` rows for a compiled module. The scalameta pipeline becomes its first implementation, unchanged. |
| 4b. Differential runner | Runs two front ends over the same input and classifies every row: identical; better (`?` → number, or a refusal whose reason is a later one); **worse** (number → `?`, or a different number). Inputs: the frozen self-measurement corpus and every compiled test fixture (the 1,490 in `core/target/compiled-fixtures`). |
| 4c. Row identity | Row names today come from syntax (`<block#1>`, `<new#1>`, `<lambda#1>`, `<extension@4>`). Fix how a symbol gets the same name, so rows pair up; pair by source position where a name cannot match. |

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
- Tests: fixtures already compile through `cardinality.Compiled`. A fixture that names an unsupplied
  declaration on purpose cannot compile; it becomes "compiled against a jar the analysis is not
  given", or goes with uncompiled support.
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
