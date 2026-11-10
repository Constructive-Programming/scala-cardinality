# Pattern catalog

What earlier passes learned: a signal one of the three tools gives, the fix that worked, and
where the fix does not apply. SKILL.md §7 says how to use and grow this file.

Sources: **R** = cardinality report row, **M** = measurement (baseline diff, ratchet, corpus
run, gates), **C** = CPD.

Status: **proposed** (not yet applied and gated), **confirmed** (applied and gated at least
once), **rejected** (do not propose again unless the boundary changes).

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

### R3 Case analysis outside the sum — confirmed
- Signal (R/source): `match` / `isInstanceOf` on one of the build's own sums in another file,
  the same question asked at several sites.
- Fix: stage 3. A `def` on the sum; callers delegate.
- Expect: adds signature rows, moves none. Gate on tests.
- Boundary: switchboards (one-line delegation per case: `SingletonIntersections.resolved`, the
  `Found` match in `MethodAnalysis`), renderers (`FinitePart.render`), interpreters (the
  `Shape` matches in the inhabitation solver), a single equality test on a case object
  (`edge.kind == Continuation.OptionBranch`). Sites that agree on two arms but differ on the
  rest are switchboards too (`Binding.Here`/`Elsewhere` in `Scope`, `Solver`, `Namespaces`;
  the builtin-key `Found` matches in `ConstructorApplications` and `NominalKeys`).
- Evidence: 331b579, four `case Found.Entry(entry)` sites → `Found.declared`. Earlier:
  `Definition.instantiationDependent` matched `Unbound` cases and `Definition.open` used
  `isInstanceOf[Unbound.Open]`; both moved onto `Unbound`, and the object helpers delegate
  (`unbound.forall(_.instantiationDependent)`). Pass 3: `Count.atLeast(n)` replaces
  `SequenceElimination.atLeastTwo` and the `DeferredInline` match; `Card.literalSize` replaces
  `Count.sizeOf` and the `ClosedSpaces.ground` match; `Kinds.proper` replaces
  `Kinds.of(_) == Kind.Proper` at three sites.

### R4 Independent numeric knobs — rejected
- Signal (R): `2^n` row of several `Int`/`Long` fields (`SourceSnapshot.Limits`,
  `AnalysisQuery.Budget`, `MethodAnalysis.Limits`).
- Boundary: each knob is enforced on its own path. Not a target.

### R5 Splitting a rendered-message case — rejected
- Signal (R): `ω` case holding one `why: String` built from two or three templates over open
  names (`TemplateClosures.Closure.Hidden`).
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
- Signal (M): `cardinalityReport` dies with `OutOfMemoryError` in `Library$Packages.solve`.
- Fix: run the sbt launcher with `-Xmx16g -Xss8m`. A wrapper script that hardcodes `-Xmx`
  ignores `.jvmopts`.
- Evidence: pass 3, 4g failed, 16g ran (97 s).

### M4 sbt JUnit XML listener race — confirmed
- Signal (M): `testFull` ends in `ConcurrentModificationException` at
  `sbt.JUnitXmlTestsListener$TestSuite.stop`, with no failing spec.
- Fix: run `core/testFull` again on its own; it is sbt's listener, not a test.

## Source shape (stage 4)

### S1 Hand-rolled short-circuiting Option fold — confirmed
- Signal (source): `foldLeft(Option(...))((acc, x) => acc.flatMap(...))`.
- Fix: `flatTraverse` or `foldLeftM`, after M2.
- Evidence: 331b579, `Conversions.all`, `ConstrainedReceivers`.

### S2 Hand-written worklist reachability — confirmed
- Signal (source): `@tailrec` loop over `(pending, seen)` that returns start + reachable.
- Fix: `Graphs.reachable`.
- Boundary: a walk that drops the start node or needs visit order (`ClosedWorld.ancestors`,
  `BinderBounds.collect`).
- Evidence: 331b579, `Linearization.bases`. Pass 3: the alias and synonym closures
  (`SourceIndex.closeAliases`, `RuntimeClasses.spellings`, `SealedFamilies.Synonyms.of`) and
  `EvidenceAnalysis.Closure.expand`, each over a `Graphs.fromEdges` graph built once.
- Also: a fixpoint that grows a set by `found ++ edges.collect { case (a, b) if found(a) => b }`
  is the same reachability.

### S4 Hand-written "until `next == current`" loop — confirmed
- Signal (source): `if (next == x) x else loop(next)` or a `var changed` / `while (changed)` loop.
- Fix: `Graphs.fixpoint(start)(step)`.
- Boundary: a loop with a widening branch (`Instantiations.iterate`) or a solver loop.
- Evidence: pass 3, `PolymorphicMembers.opens`, `TargetViews.classify`, `Positivity.slots`,
  `OpaqueSumForwarding` (its `var` loop).

### S5 Hand-written parent walk — confirmed
- Signal (source): `@tailrec` match on `tree.parent`, or `Iterator.iterate(tree.parent)(…)`.
- Fix: `Trees.ancestors(tree)` with `collectFirst` / `forall`; `Trees.innermost` when `f` is
  costly and must stop early.
- Evidence: pass 3, `Walk.owner`, `SourceIndex.static`.

### S6 Walk that needs a layer it cannot import — rejected
- Boundary: `Overrides.ancestry` restates `InheritedScopes.breadthFirst`, but `types` would
  import `analysis.resolution`. Moving `breadthFirst` is a separate refactor.
- Also kept: `RuntimeClasses.closure` (stops at the first `Left`, order picks the error),
  `NeutralSaturation.unbounded.reach` (stops early).

### S3 `mapAccumulate` for a counter-threading fold — rejected
- Boundary: the two `.swap` calls read worse than the fold (`NominalKeys.all`).

## CPD (C)

### C1 Over-gate duplicate — confirmed
- Signal (C): any group of 25+ tokens; CI fails.
- Fix: one shared copy where both callers reach it. Copies that differ in what they check
  (`ClosedWorld.open` vs `TypeMembers.open`) are restructured until CPD no longer matches.
- Boundary: duplicated import blocks follow SKILL.md §6.2: restructure first; `// CPD-OFF` only
  after that is declined, and never outside imports.

### C2 Type union spelled at many sites — proposed
- Signal (C): 12-token idiom such as `Defn.Class | Defn.Trait | Defn.Object | Defn.Enum`
  (25 occurrences in 16 files at 331b579).
- Fix: a type alias in `types/Trees.scala`.
- Expect: no report movement.

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

### C6 Modifier test spelled at many sites — confirmed
- Signal (C): `mods.exists(_.is[Mod.X])` (12× `Case`, 8× `Abstract`, 7× `Private` at 6e4b304).
- Fix: an extension beside `hasFinal` in `types/Trees.scala`; import it by name.
- Expect: new `?` rows for the predicates (scalameta `Mod` is open); nothing moves.
- Evidence: pass 3, `hasCase`, `hasAbstract`, `hasPrivate`.

### C7 Tree accessor chain spelled at many sites — confirmed
- Signal (C): `paramClauses.toList.flatMap(_.values)` (20 sites) and
  `flatMap(_.paramClauses).flatMap(_.values)` over clause groups.
- Fix: an extension on the scalameta trait that declares the member
  (`Tree.WithParamClauses.parameters`): it covers `Ctor.Primary`, `Ctor.Secondary` and
  `Member.ParamClauseGroup` at once. Check the trait in the scalameta jar (`javap`).
- Evidence: pass 3, `Trees.parameters`; `Trees.fields` reads through it.

### C5 Standard one-liner — rejected
- Signal (C): `.groupMap(_._1)(_._2)` and similar library one-liners.
- Boundary: already minimal.
