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
  (`edge.kind == Continuation.OptionBranch`).
- Evidence: 331b579, four `case Found.Entry(entry)` sites → `Found.declared`. Earlier:
  `Definition.instantiationDependent` matched `Unbound` cases and `Definition.open` used
  `isInstanceOf[Unbound.Open]`; both moved onto `Unbound`, and the object helpers delegate
  (`unbound.forall(_.instantiationDependent)`).

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
- Evidence: 331b579, `Linearization.bases`.

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

### C4 Bare tuple with a fixed meaning — proposed
- Signal (C/grep): `Option[(String, Int)]` as (source, offset): 35 uses in 9 files.
- Fix: stage 2. One named case class.

### C5 Standard one-liner — rejected
- Signal (C): `.groupMap(_._1)(_._2)` and similar library one-liners.
- Boundary: already minimal.
