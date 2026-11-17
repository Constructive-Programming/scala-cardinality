# Pattern catalog

What earlier passes learned: a signal one of the three tools gives, the fix that worked, and
where the fix does not apply. SKILL.md §7 says how to use and grow this file.

Sources: **R** = cardinality report row, **M** = measurement (baseline diff, ratchet, corpus
run, gates), **C** = CPD.

Status: **proposed** (not yet applied and gated), **confirmed** (applied and gated at least
once), **rejected** (do not propose again unless the boundary changes), **retired** (the code it
concerned is gone).

Evidence lines are a record and keep the names of their time: many name classes of the scalameta
front end, which the TASTy front end (`core/src/main/scala/cardinality/compiled/`) replaced.

Entry shape:

```
### <ID> <name> — <status>
- Signal (<source>): what the tool shows, and the grep or row shape that finds it.
- Fix: the refactor.
- Expect: report movement and gates.
- Boundary: where it does not apply.
- Evidence: <commit or baseline note>: before → after.
```

## Report (R)

### R1 Closed vocabulary in a String — confirmed
- Signal (R): `ω` stored-value row whose `String` field is only built from a few literals or
  constants at its construction sites.
- Fix: stage 2. An enum that carries the same message text.
- Expect: row loses its `ω` term (the constructor goes from `ω` to the case count).
- Boundary: a String that embeds open names (rendered refusal reasons, type syntax) stays.
- Evidence: 331b579, `StdlibModel.Reading.Excluded(why: String)` → `Excluded(Exclusion)`;
  `Reading` `2ε₀ + ω + 4` → `2ε₀ + 7`.
  Same shape in an exception's message: `Natural.Exceeded(limit: String)` (three templates over
  constants) → `Exceeded(Breach)`, ω → 3; `Inhabitation.Limit(message: String)` → `Limit(Budget)`
  with `Budget.States(max: Int)` keeping the one open number: ω → ≤2^33 (`<init>` 2^32 + 2).
  Pass 4: `Live.Walk(step: String, skipFinished: Boolean, …)` had three literal steps, each with
  one fixed flag → `Live.Step` (3). `Walk` keeps its rule function, so its own row stays `?`.

### R2 Correlated fields in a product — confirmed
- Signal (R): `ω` row with generic fields (`budget`, `frontier`, `message`) where one field's
  meaning depends on another's value.
- Fix: stage 1. A sum, one case per combination the code builds; each case renders the old
  message.
- Expect: `ω` → `kω`, the case count as coefficient. A win at the same tier.
- Boundary: fields that are independent knobs on separate paths, pinned by a test or doc.
- Evidence: `Exhausted(budget: String, frontier: String)` had 4 literal budgets and a frontier
  that was an operation name for three stops but a path for one →
  `Fuel(index | target, operation)`, `ResolutionDepth(operation)`, `SyntaxDepth(path)`, each
  rendering the old message.

### R6 One flag implies the other — confirmed
- Signal (R/source): two `Boolean` fields (or two `X => Boolean`) where one being true forces the
  other: built from two runs of the same computation, one over a subset of the other's rules.
- Fix: stage 1. A three-case enum (`Standing`: `Unbuilt | Optimistic | Proved`), with the old
  questions as predicates **on the enum** (M5).
- Expect: `4^n` → `3^n` in the row.
- Evidence: pass 4, `Verdicts(proved, optimistic, absurd)` → `Verdicts(standing, absurd)`;
  `<init>` `2^(2^33)·…` → `3^(2^32)·…`.

### R7 Flags only ever read together — confirmed
- Signal (source): two `Boolean` fields whose only reader is `a || b` (grep each field's reads).
- Fix: stage 2. One flag; the doc comment keeps the two reasons.
- Expect: the row halves.
- Boundary: a writer may still set one of them alone; only the readers decide.
- Evidence: pass 4, `TargetViews.Reads(outer, escapes)` → `reaches`; 2^8589934595 → 2^8589934594.

### R3 Case analysis outside the sum — confirmed
- Signal (R/source): `match` / `isInstanceOf` on one of the build's own sums in another file,
  the same question asked at several sites.
- Fix: stage 3. A `def` on the sum; callers delegate.
- Expect: adds signature rows, moves none. Gate on tests.
- Boundary: switchboards (one-line delegation per case: the `Unbound` match in
  `TastyFrontEnd.spelled`), renderers (`FinitePart.render`), interpreters (the
  `Shape` matches in the inhabitation solver), a single equality test on a case object
  (`edge.kind == Continuation.OptionBranch`). Sites that agree on two arms but differ on the
  rest are switchboards too.
- Evidence: 331b579, four `case Found.Entry(entry)` sites → `Found.declared`. Earlier:
  `Definition.instantiationDependent` matched `Unbound` cases and `Definition.open` used
  `isInstanceOf[Unbound.Open]`; both moved onto `Unbound`, and the object helpers delegate
  (`unbound.forall(_.instantiationDependent)`). Pass 3: `Count.atLeast(n)` replaces
  `SequenceElimination.atLeastTwo` and the `DeferredInline` match; `Card.literalSize` replaces
  `Count.sizeOf` and the `ClosedSpaces.ground` match; `Kinds.proper` replaces
  `Kinds.of(_) == Kind.Proper` at three sites. Pass 4: `Count.isZero` replaces the
  `Count.Finite(n) if n == 0` guards in `ViewSandwich` and `SequenceElimination`.

### R8 Unannotated member type — confirmed
- Signal (R): `?` signature row `val x: <inferred>` with `inferred result type not resolved: …`.
- Fix: the build plugins now read the compiler's type from the module's TASTy (rule T,
  `docs/plans/compiled-own-types.md`); a template or top-level member needs no annotation.
  Annotate only for `cardinalityReportOf` runs; locals and pattern names are typed too (phase 2).
- Expect: the row leaves `?` for its real size; nothing else moves.
- Boundary: `transparent inline` results (rule X) stay `?`.
- Evidence: 553de73 follow-up, `Skill.markers` `?` → `ω` by hand. Rule T: core `<inferred>` rows
  426 → 55, 30 rows `?` → a number (`62ε₀ + 675ω` → `62ε₀ + 703ω`, `?` 3483 → 3471).
  Phase 2: 55 → 0, `ClosedSpaces.tiers` anonymous members `?` → ω (`703ω` → `710ω`).

### R4 Independent numeric knobs — rejected
- Signal (R): `2^n` row of several `Int`/`Long` fields (a budget's limits).
- Boundary: each knob is enforced on its own path. Not a target.

### R5 Splitting a rendered-message case — rejected
- Signal (R): `ω` case holding one `why: String` built from two or three templates over open
  names.
- Boundary: the split adds a case and a renderer and moves no tier.

## Measurement (M)

### M1 Unsupplied wildcard import makes rows `?` — confirmed
- Signal (M): rows turn `?` after adding `import x.*` for a package outside the sources
  ("may bind `Inhabitation`, …").
- Fix: import the members by name (`cats.syntax.traverse.toTraverseOps`).
- Expect: the rows come back.
- Evidence: 331b579, `cats.syntax.all.*` made 23 rows `?`.

### M2 Behaviour-preserving swap needs checked semantics — confirmed
- Signal (M): a stage 4 swap passes tests but the analysis spends fuel per step, so order and
  short-circuiting matter.
- Fix: read the library source (cellar) before swapping; record what was checked.
- Evidence: 331b579, `flatTraverse` / `foldLeftM` / `sequence` checked in cats-core 2.13.0.
  Earlier: `Shape.sequenceEithers` short-circuits on the leftmost `Left`; cats `sequence` for
  `Either` matches, so the swap landed (the row stayed one `?`).

### M3 The report needs more than the default heap — confirmed
- Signal (M): `cardinalityReport` dies with `OutOfMemoryError`.
- Fix: run the sbt launcher with `-Xmx16g -Xss8m`. A wrapper script that hardcodes `-Xmx`
  ignores `.jvmopts`.
- Evidence: pass 3, 4g failed, 16g ran (97 s).

### M4 sbt JUnit XML listener race — confirmed
- Signal (M): `testFull` ends in `ConcurrentModificationException` at
  `sbt.JUnitXmlTestsListener$TestSuite.stop`, with no failing spec.
- Fix: run `core/testFull` again on its own; it is sbt's listener, not a test.

### M5 A method on a function-carrying class is a huge row — confirmed
- Signal (M): a new signature row of the form `2^(2^32·…)` after adding a helper `def` to a case
  class whose fields are functions (`Verdicts.proved(id: Int)` counted `Boolean^(Int × Verdicts)`).
- Fix: put the question on the small type the function returns (`Standing.proved`, 2^3) and let
  callers apply the function themselves.
- Expect: the total's finite part must not grow; `report-diff` shows it when it does.
- Evidence: pass 4, total ≈2^(2^103) → ≈2^(2^144246315611) with the helpers on `Verdicts`, back
  to ≈2^(2^103) with them on `Standing`.

### M6 report-diff must read nested products — confirmed
- Signal (M): a size such as `2^(2^32·3^(2^32)·…)` has `·` inside parentheses and is far past a
  Double. The first script split on `·` and missed the growth in M5.
- Fix: `report-diff.scala` parses the size and sums exponent towers; `--check` runs its self-check.

### M7 Selected report on core fails with `duplicate source origin` — retired
- Signal (M): `core/cardinalityReportSelected <name>` logs `could not read : duplicate source
  origin` and fails; the full `cardinalityReport` on the same sources succeeds.
- Retired: the check that raised it went with the scalameta front end.
- Evidence: 2026-11-10, while measuring `Skill` (issue #30 pass).

### M8 A measurement overflows a 1 MiB stack — retired
- Signal (M): `StackOverflowError` at `EvidenceAnalysis.scala:350` under `GoalSolver.goal`, e.g.
  `PolymorphicMemberSpec` "cannot create an instance", when sbt runs without `.jvmopts`
  (`-Xss8m`); CI's launcher reads it and passes.
- Cause: unbudgeted runs of the deleted scalameta front end (its `analyze`,
  `cardinalityReportOf`) had `solverDepth = Int.MaxValue`, so a growing goal chain recurses until
  the 256-state budget, each goal folding a deeper shape. `StackOverflowError` is fatal: `NonFatal`
  does not catch it. Every row of the TASTy front end runs under `Readings.Depth` (64) and
  `Readings.Work`.
- Fix (retired with that front end): `DeepStack` ran every measurement on its own 256 MiB
  (reserved) stack; no count moved.
- Evidence: 2026-11-10, local `sbt` wrapper with a hard-coded `-Xmx4g` and no `.jvmopts`.

### M9 Stub members read open through same-named JDK templates — retired
- Signal (M): thousands of `member may be overridden through a parent outside the sources:
  scala.meta.Tree.begOffset` (and `Type.`, `Stat.` …) on stub-backed receivers.
- Cause: the closed world asked about a stub template by simple name, and keyed a stub parent the
  stubs do not declare (stdlib-held `java.util.Map`, `java.lang.CharSequence`) by simple name, so
  scalameta's `Member.Function` / `Type.Map` pulled in every JDK `Function` / `Map` implementor and
  their undeclared JDK parents.
- Fix: a stub parent is its qualified name, declared or not; a stub owner is asked about by its
  qualified name (`ClosedWorld.owner`); library markers are known by qualified name too.
- Expect: the reason leaves; rows move to their next reason (`capability record with members
  hidden at the use site`, 926 → 2216). No count moved.
- Retired: the stubs and the scalameta front end that read them are gone; the TASTy front end keys
  every template by the compiler's symbol.
- Evidence: 2026-11-10, reason 2277 → 0 lines.

## Source shape (stage 4)

### S1 Hand-rolled short-circuiting Option fold — confirmed
- Signal (source): `foldLeft(Option(...))((acc, x) => acc.flatMap(...))`.
- Fix: `flatTraverse` or `foldLeftM`, after M2.
- Evidence: 331b579, `Conversions.all`, `ConstrainedReceivers`.

### S2 Hand-written worklist reachability — confirmed
- Signal (source): `@tailrec` loop over `(pending, seen)` that returns start + reachable.
- Fix: `Graphs.reachable`.
- Boundary: a walk that drops the start node or needs visit order.
- Evidence: 331b579, `Linearization.bases`. Pass 3: the alias and synonym closures
  (`SourceIndex.closeAliases`, `RuntimeClasses.spellings`, `SealedFamilies.Synonyms.of`) and
  `EvidenceAnalysis.Closure.expand`, each over a `Graphs.fromEdges` graph built once.
- Also: a fixpoint that grows a set by `found ++ edges.collect { case (a, b) if found(a) => b }`
  is the same reachability.

### S4 Hand-written "until `next == current`" loop — confirmed
- Signal (source): `if (next == x) x else loop(next)` or a `var changed` / `while (changed)` loop.
- Fix: `Graphs.fixpoint(start)(step)`.
- Boundary: a loop with a widening branch (`Fixpoint.iterate`) or a solver loop
  (`GoalSolver.leastProductive` spends `inspect` fuel per node and mutates in place).
- Evidence: pass 3, `PolymorphicMembers.opens`, `TargetViews.classify`, `Positivity.slots`,
  `OpaqueSumForwarding` (its `var` loop).

### S5 Hand-written parent walk — retired
- Signal (source): `@tailrec` match on `tree.parent`, or `Iterator.iterate(tree.parent)(…)`.
- Fix (retired with the scalameta trees): `Trees.ancestors(tree)` with `collectFirst` / `forall`;
  `Trees.innermost` when `f` is costly and must stop early. tasty-query's trees have no parent
  links.
- Evidence: pass 3, `Walk.owner`, `SourceIndex.static`.

### S6 Walk that needs a layer it cannot import — rejected
- Boundary: a walk that restates a helper in a layer `types` may not import stays where it is;
  moving the helper is a separate refactor.
- Also kept: `NeutralSaturation.unbounded.reach` (stops early).

### S3 `mapAccumulate` for a counter-threading fold — rejected
- Boundary: the two `.swap` calls read worse than the fold.

## CPD (C)

### C1 Over-gate duplicate — confirmed
- Signal (C): any group of 25+ tokens; CI fails.
- Fix: one shared copy where both callers reach it. Copies that differ in what they check are
  restructured until CPD no longer matches.
- Boundary: duplicated import blocks follow SKILL.md §6.2: restructure first; `// CPD-OFF` only
  after that is declined, and never outside imports.
- Evidence: pass 4, a new `Trees.hasLazy` import made the headers of `Throws` and `ScopeTerms`
  match for 25 tokens; `ScopeTerms` now imports `cardinality.types.*` (it sorts first).

### C2 Type union spelled at many sites — confirmed
- Signal (C): 12-token idiom such as `Defn.Class | Defn.Trait | Defn.Object | Defn.Enum`
  (25 occurrences in 16 files at 331b579).
- Fix: a type alias beside the code that uses it (once in `types/Trees.scala`, deleted with the
  scalameta front end).
- Expect: no report movement.
- Evidence: `TemplateDefn`, `ClassDefn`, `MethodDefn`; at pass 4 each union is spelled once, in
  its alias.

### C3 Repeated parameter group — proposed
- Signal (C): the same parameters in many signatures: `tpe, context, resolver` (16),
  `name, frame, at` (6).
- Fix: stage 2. A case class naming the group; methods move onto it.
- Expect: a new definitions row. Record how the affected signature rows move.

### C4 Bare tuple with a fixed meaning — confirmed
- Signal (C/grep): `Option[(String, Int)]` as (source, offset): 35 uses in 9 files.
- Fix: stage 2. One named case class.
- Expect: a new definitions row at the tuple's size (ω here); no row moves.
- Boundary: grep the tuple by meaning, not by type: `CapabilityRecords.measured` is also
  `Option[(String, Int)]` but means (name, arity), and stays.
- Evidence: pass 3, `model.Anchor(source, offset)` beside `ResolutionContext`.

### C6 Modifier test spelled at many sites — retired
- Signal (C): `mods.exists(_.is[Mod.X])` (12× `Case`, 8× `Abstract`, 7× `Private` at 6e4b304).
- Fix (retired with the scalameta front end): an extension beside `hasFinal` in
  `types/Trees.scala`; import it by name.
- Expect: new `?` rows for the predicates (scalameta `Mod` is open); nothing moves.
- Boundary: `clause.mod.exists(_.is[Mod.Using])` (5 sites) is on an `Option[Mod]`, not a modifier
  list; left.
- Evidence: pass 3, `hasCase`, `hasAbstract`, `hasPrivate`. Pass 4: `hasInline`, `hasLazy` (9
  sites, `BodyWalk.inlineMod` deleted); three spelled tests now call the existing
  `MemberSyntax.implicitModifier` and two the existing `MemberSyntax.restricted`.

### C7 Tree accessor chain spelled at many sites — retired
- Signal (C): `paramClauses.toList.flatMap(_.values)` (20 sites) and
  `flatMap(_.paramClauses).flatMap(_.values)` over clause groups.
- Fix: an extension on the scalameta trait that declares the member
  (`Tree.WithParamClauses.parameters`): it covers `Ctor.Primary`, `Ctor.Secondary` and
  `Member.ParamClauseGroup` at once. Check the trait in the scalameta jar (`javap`).
- Evidence: pass 3, `Trees.parameters`; `Trees.fields` reads through it. Retired with the
  scalameta front end.

### C5 Standard one-liner — rejected
- Signal (C): `.groupMap(_._1)(_._2)` and similar library one-liners.
- Boundary: already minimal.
