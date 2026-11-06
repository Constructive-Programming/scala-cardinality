---
name: cardinality-reduction
description: "Reduce Total Program Cardinality (TPC) in this repository using the project's own analyzer. Use when asked to 'reduce cardinality', 'shrink types', 'find cardinality candidates', 'run the cardinality report', or before/after refactors that change data or method signatures, or 'run CPD' / 'find duplication' after a reduction round. Encodes the stage hierarchy from Rodolfo Hansen's Type System Savings / Hidden Powers of TPC articles, ranked by impact."
---

# Cardinality reduction in scala-cardinality

This repository runs its own analyzer on itself. The plugin is wired into
`project/plugins.sbt` (self-published), so the report is one command away.

Terms used below:

- **Pass**: one round of this workflow (§3 to §6), from measuring to the gates.
- **Baseline**: the newest `docs/baselines/self-*.txt`. Its prose header is the **baseline
  note**, where each pass records what it changed and why.
- **Catalog**: `patterns.md` next to this file. What earlier passes learned (§7).
- **The user**: the repository owner. Their decisions override the catalog.

## 1. Measure

```bash
sbt publishLocal        # only needed after changing core/ or plugin/ sources
sbt -Dcardinality.self=0.1.0-SNAPSHOT cardinalityReport   # target/**/<module>/cardinality/report.txt
```

- The build loads its own plugin only when `-Dcardinality.self=<version>` is set (CI never does).

- Reports are per module (`scala-cardinality` = core, `sbt-cardinality` = plugin); the root
  aggregates and has no sources of its own.
- Narrow a single signature with `sbt -Dcardinality.self=0.1.0-SNAPSHOT 'cardinalityReportSelected <fqcn>'`.
- Diff each run against the baseline.

### Reading sizes

A size is the polynomial `a·ε₀ + b·ω + n` (`docs/type-arithmetic.md`). It falls in one tier:

| Tier | Condition | Examples |
| --- | --- | --- |
| finite | `a = 0` and `b = 0` | `3`, `2^64` |
| `ω` | `a = 0`, `b > 0` | `ω`, `3ω + 4` |
| `ε₀` | `a > 0` | `ε₀`, `2ε₀ + 7` |

Both infinite tiers are countable ordinals; `ε₀` is the larger one.

Smaller is better, compared lexicographically (`a`, then `b`, then `n`): `16` beats `2^10`
beats any larger finite beats `ω` beats `ε₀`. Moving a row down a tier (`ε₀` → `ω`, `ω` → finite)
is the largest win; shrinking a coefficient or a finite size inside a tier is also a win.

`?` means unresolved: the trailing `unresolved:` reasons say what stopped the bound. It is not
a size and never means infinity. `—` is a definition with no cardinality of its own (abstract).

The signature section counts the implementation space of methods and constructors; the
stored-value section counts the state space of data definitions.

## 2. The hierarchy: where the value is

From Hansen's ["Type System Savings"](https://medium.com/swlh/type-system-saving-55db2ca062ff)
and ["The Hidden Powers of Total Program Cardinality" 1](https://medium.com/@rodolfohansen/the-hidden-powers-of-total-program-cardinality-9351ba774d4d)
/ [2](https://medium.com/@rodolfohansen/the-hidden-powers-of-total-program-cardinality-2-of-2-ceb0a3af13dd).
Four stages, ranked by impact. Try the lowest-numbered stage that applies first.

1. **Replace products with sums.** `|A| + |B|` < `(1+|A|) × (1+|B|)`. Split "either A or B"
   fields into a sealed trait or enum; model absence in the type, not in two nullable fields.
2. **Narrow types.** A type broader than the domain carries states the code never sees:
   - a closed vocabulary in a `String` → an enum;
   - a count that is always positive → a refined number;
   - a `List` that never repeats or reorders → a `Set` or a sized structure;
   - an `Either[E, A]` with one possible error → `Option` or a concrete type;
   - a supertype where only one subtype is ever supplied → that subtype.
3. **Take sums out of exponents.** `aᵇ + aᶜ` < `a^(b+c)`: a sum-typed parameter multiplies
   the implementation space by its case count. Distribute: one method per case, dispatch on
   the sum, map over an `Option` instead of passing it.
4. **Generics.** Parametricity shrinks implementation space toward one inhabitant
   (`[A] A => A`). Smallest absolute shift, highest reuse.

## 3. Data pass (stages 1 and 2)

1. Read the catalog (§7), then run the report (§1).
2. List stored-value rows from largest to smallest (§1 order). These are the candidates.
   Skip `?` rows in both sections: they are analyzer resolution limits (here mostly scalameta
   `Tree`/`Type` openness), not reduction targets.
3. For each candidate, read its construction sites before classifying: the report shows
   sizes, not fields.
   - Several numeric fields: stage 1 or 2 only if the fields are not independent knobs.
   - A `String` field built from a few literals: a hidden enum (stage 2).
   - Fields whose meaning depends on each other: a sum of the combinations the code builds
     (stage 1).
   - An open `String` (identifiers, source text, type syntax): leave it.

   Check the catalog's R entries for each shape's worked example and boundary.
4. Re-measure. A product with any `ω` factor (`String`, `List`) stays in the `ω` tier however
   its finite factors shrink, so narrowing one of those shows no movement: groundwork, not a
   win. Splitting the product into a sum does move it (`ω` → `3ω`): count that as a win.
5. Ratchet: no existing row may move up a tier, or grow inside its tier, without a
   justification in the baseline note. Refresh the baseline when the pass is committed.

## 4. Sums pass (stage 3)

The report shows a sum-typed parameter's size but not whether callers branch on it, so find
those sites in the sources.

1. Inventory the build's own sums: `enum`, `sealed trait`, `sealed abstract class` under
   `core/src/main`. Third-party hierarchies (scalameta `Tree`, `Type`, `Defn`, `Stat`) are
   visitor territory; leave them.
2. Find case analysis outside the sum: `match`, `isInstanceOf[`, `== CaseObject` on one of
   those sums, directly or through a field, in code that is not the sum's own.
3. Classify each site:
   - The same question answered outside the sum: move it onto the sum as a `def`; callers
     delegate.
   - A switchboard (each branch a one-line delegation), a renderer, or an interpreter of the
     sum: keep.
   - An equality test on a case object: keep, unless the same test repeats across sites.

   Catalog entry R3 has the examples.

## 5. Generics pass (stage 4)

Scouts grep first (`def \w+.*List\[`, `@tailrec`, `foldLeft|foldRight`) and read only the
hit regions: whole-file reads exhaust a scout's context on this codebase.

Three buckets, in order of value:

1. **Type-duplicated helpers**: functions differing only in the type operated on. One
   generic function takes the varying operation as a parameter.
2. **Hand-rolled machinery**: folds or loops that restate `TypeSubstitution.hylo`,
   `localCata`, `plated`, `Graphs.reachable` or a cats combinator. Swap them.
3. **Concrete-typed structural functions**: containers in and out, no domain logic. Add a
   type parameter only with a second caller; generalizing a single caller is speculation.

Before a swap, check the library's semantics (order, short-circuiting, strictness) in its
source, not from the combinator's name: the analysis spends fuel per step, so order matters.
Use cellar for external artifacts (`cellar get-external <coordinate> <fqn>`, then
`cellar get-source` when the doc does not settle it), and grep for internal symbols.
Leave domain-coupled code (`Measurement.resolve`, the solver walks): invariants pin it, not
types.

## 6. Duplication pass (CPD)

Stage 3 and 4 changes move code into sums and shared helpers, so the same shape shows up in
more places. Run CPD after each pass. CI gates on 25 tokens (`.github/workflows/ci.yml`, job
`cpd`, version in `PMD_VERSION`).

```bash
PMD=~/pmd/pmd-bin-<PMD_VERSION>/bin/pmd     # install as ci.yml does if missing
DIRS="--dir=core/src/main/scala --dir=plugin/src/main/scala"
$PMD cpd --minimum-tokens=25 --language=scala $DIRS --format=text   # the gate: must be empty
$PMD cpd --minimum-tokens=12 --language=scala $DIRS --format=csv > /tmp/cpd12.csv
scala-cli run .claude/skills/cardinality-reduction/cpd-idioms.scala -- /tmp/cpd12.csv 40
# native binary, once: scala-cli --power package --native .claude/skills/cardinality-reduction/cpd-idioms.scala -o /tmp/cpd-idioms
```

1. **Clear the gate.** Merge every group of 25 or more tokens into one copy, unless the copies
   differ in what they check; then restructure until CPD no longer matches.
2. **Imports are the one exception.** For a duplicated import block, first try to restructure
   or simplify it (drop unused imports, import a package instead of its members). Only if
   that is declined may the block be wrapped in `// CPD-OFF` / `// CPD-ON`. CPD-OFF anywhere
   else is prohibited.
3. **Read the 12-token groups as idioms.** The script drops import/package groups and ranks
   the rest by occurrences × tokens: one line repeated in 15 files outranks one block in two.
4. **Classify each top idiom** by what the repetition says is missing:

   | Repetition | Missing piece |
   | --- | --- |
   | A type union spelled at many sites | a type alias in `types/Trees.scala` |
   | A modifier test (`mods.exists(_.is[Mod.Sealed])`) | a named predicate; reuse an existing one first |
   | The same parameter group in many signatures | a domain type (stage 2) |
   | A bare tuple with a fixed meaning | a named type (stage 2) |
   | Two helpers identical up to a name | one shared helper |
   | A standard library one-liner | nothing |

   Parameter groups and bare tuples matter most: they are types the domain uses but the code
   never named.

## Gates

Every pass ends with all of these green:

```bash
sbt core/testFull plugin/scripted   # testFull is uncached; plain `test` may run nothing
sbt scalafmtAll scalafixAll
bash scripts/check-file-metrics.sh
# and the 25-token CPD run from §6, empty
```

Then re-run the report and compare against what the pass should move:

| Pass | Expected report movement |
| --- | --- |
| Data (§3) | rows shrink or split into sums |
| Sums (§4) | new `def` rows, none removed |
| Generics (§5) | none |
| Duplication (§6) | none for an alias or predicate; a new definitions row for a new type |

Anything else that moves is a measurement surprise: explain it before landing (§7).

## 7. Learn: grow the catalog

The catalog holds what earlier passes learned from the report (R), measurement (M) and CPD
(C): a signal, a fix, the expected movement, a boundary and evidence. It is how the next pass
starts ahead of this one.

1. **Before a pass**, read it. Check every signal not marked rejected; skip a candidate that
   matches a rejected entry unless its boundary no longer holds.
2. **During a pass**, match each fix and each rejection:
   - Matches an entry: add a line to its Evidence (commit, before → after).
   - Matches nothing: add an entry as **proposed**. Write the signal as something the next
     pass can search for (a row shape, a grep, a CPD idiom), not a description of one case.
   - Rejected by you or the user: add it as **rejected** with the reason in Boundary.
   - A measurement surprise (rows going `?`, a gate failing, an unrelated count moving) is an
     M entry even when no refactor follows.
3. **After the gates pass**: proposed → confirmed once its fix is applied and gated.
   Confirmed → rejected only on a failed gate or the user's decision. When a boundary proves
   wrong, edit it rather than adding a second entry.
4. Keep entries short; the derivation goes in the baseline note. When a confirmed pattern
   changes how a pass is run, fold it into §3 to §6 and keep the entry as the pointer.
5. Commit the catalog, the baseline and its note with the pass.
