# Compiled own types: reading the module's TASTy

**Status:** phases 1 to 3 implemented (2026-11-10/11). Phases 4 and 5 are proposed.

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

The override-identity and given-name code listed earlier did not refuse anything on this repository,
and replacing it needs symbol facts a source rewrite cannot carry: it stays for now.

## Phase 4: exact structure (contract text changes)

| Area | Fact | TASTy instead | Note |
| --- | --- | --- | --- |
| `ClosedWorld` Ref/Node matching by simple name | Which templates extend which | Exact parent symbols | Removes the "same-named templates share subclasses" overcount. The rule that writes `final` stays on the syntax side. |
| `Linearization`, `InheritedScopes`, `InheritedTerms` (foreign), `ForeignParents` | Linearization, parent type arguments, members of library parents | `ClassSymbol.linearization`, parent types | Removes "may be overridden through a parent outside the sources". |
| `ReceiverCalls` 76-170, `Construction` | Callee of a call, by-value or by-name, throwing statements | The `Apply` function symbol and parameter types | |
| `ContextFunctions` 121-318 | Whether an expected type, dealiased, is a context function | Dealiased `declaredType` | Rule F: never dealias an opaque type outside its scope. |

## Phase 5: the large ones (each needs a decision first)

- **Inline-body reading** (`InlineBodies`, `BodyWalk`, `BodyTerms`, `LibraryInlines`, about 1,500
  of 2,400 lines). The typed tree resolves every name, so a compile-time operation is a symbol
  under `scala.compiletime` or `scala.quoted`, or an inline `if`/`match`/quote/splice node.
  `LibraryInlines` becomes an `isInline` check on scala-library TASTy, even in syntax mode.
  Open points:
  - TASTy is pickled before inlining, but `transparent` calls arrive already expanded.
  - "A given left to implicit search" must still refuse, so synthesized `using` arguments have
    to be told apart by span.
- **D43 conversion and extension candidates** (`Conversions`, `Reachable`, `RestatedExtensions`,
  `Unconverted`, `Insertions`, about 1,300 lines). The typed body shows exactly the conversions
  the typer inserted. Decision: rewrite D43 from "the candidates a body may meet" to "the
  conversions the typed body applies". That is a sound subset, but counts move.
- **Name resolution** (`SourceLookup`, `LookupBindings`, `ShadowedBuiltins`, … about 2,000
  lines eventually). Build a span → symbol index answering `Found` at `Resolver.lookup`. Types
  the analyzer builds itself have no span, so the lookup stays. Decision: what happens where the
  compiler binds `Int` to a declaration the contract treats as unsupplied ("builtin names are
  never shadowed by what is not supplied").

## Not worth reading from TASTy

The analyzer's own model, not a compiler fact: `StuckProjections` (the `proj:` atom),
`LiteralTypes` (already exact), the shape logic in `SingletonIntersections`, `TypeMembers` and
`RefinedMembers` (path-keyed atoms), `Representations`, `Callees` (type-level dispatch), and the
contextual evidence for hypothetical bodies.

Finality never comes from TASTy flags. The closed world's `final`s exist only in the analyzer.
