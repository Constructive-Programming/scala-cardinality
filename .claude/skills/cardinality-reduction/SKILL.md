---
name: cardinality-reduction
description: "Reduce Total Program Cardinality (TPC) in this repository using the project's own analyzer. Use when asked to 'reduce cardinality', 'shrink types', 'find cardinality candidates', 'run the cardinality report', or before/after refactors that change data or method signatures. Encodes the stage hierarchy from Rodolfo Hansen's Type System Savings / Hidden Powers of TPC articles, ranked by impact."
---

# Cardinality reduction in scala-cardinality

This repository runs its own analyzer on itself. The plugin is wired into
`project/plugins.sbt` (self-published), so the report is one command away.

## 1. Measure first

```bash
sbt publishLocal        # only needed after changing core/ or plugin/ sources
sbt cardinalityReport   # per-module report; also written to target/out/jvm/scala-3.9.0/<module>/cardinality/report.txt
```

- Reports are **per module** (`scala-cardinality` = core, `sbt-cardinality` = plugin); the root
  aggregates and has no sources of its own.
- Narrow a single signature with `sbt 'cardinalityReportSelected <fqcn>'`.
- The checked-in baseline is `docs/baselines/self-2026-11-03.txt` (analyzer rev 613f655).
  Diff a new run against it; refresh the baseline only when a change lands.

### Reading rows

| Size | Meaning |
| --- | --- |
| `2^n`, `8`, `3` | finite upper bound (constructor inputs only) |
| `ω` | countably infinite value space (recursive fixed point, lazy hole, `String`-like base) |
| `ε₀`, `aω` | beyond countable / coefficient of countable alternatives |
| `?` | unresolved — the trailing `unresolved:` reasons say what stopped the bound |
| `—` | definition with no cardinality of its own (abstract) |

The signature section counts *implementation space* of methods/constructors; the stored-value
section counts *state space* of data definitions. Never read `?` as infinity.

## 2. The hierarchy: where the value is

From Hansen's ["Type System Savings"](https://medium.com/swlh/type-system-saving-55db2ca062ff)
and ["The Hidden Powers of Total Program Cardinality" 1](https://medium.com/@rodolfohansen/the-hidden-powers-of-total-program-cardinality-9351ba774d4d)
/ [2](https://medium.com/@rodolfohansen/the-hidden-powers-of-total-program-cardinality-2-of-2-ceb0a3af13dd).
Four stages, **ranked by impact on the TPC score** — always try the lowest-numbered applicable
stage first; the bulk of the returns are reaped by stage 3, and stage 4 is the craftsman's tier.

### Stage 1 — Replace products with sums (biggest win)

`|A| + |B|` vs `(1+|A|) × (1+|B|)`: a nullable product of two engines has 54 states where the
sum has 13; the article's validation function drops from `1.8×10¹⁶` possible implementations.
Make illegal states unrepresentable: split "either A or B" fields into a sealed trait/enum,
kill nullable pairs, model `Option` in the type, not in two independent fields.

### Stage 2 — Narrow types (functional domain design / domain driven design)

Any type broader than the domain needs carries states the code must never see. The article's
primitive case — `String`/`Int` → enum or refined type (`Int Refined Positive` exposed the
booking bug) — is one instance of the general rule: wherever a broader type is used than the
situation admits, narrow it. A closed vocabulary carried in `String` becomes an enum; a count
that is always positive becomes a refined number; a `List` whose elements never repeat or
reorder becomes a `Set` or a sized structure; an `Either[E, A]` with exactly one possible
error becomes `Option` or a concrete type; a supertype accepted where one subtype is ever
supplied narrows to that subtype. Every unnecessary breadth removed deletes a family of
invalid states — and shows up as a smaller report row.

### Stage 3 — Take sums out of exponents (functions)

`(aᵇ + aᶜ) < a^(b+c)`: an enum/Option/interface as a parameter multiplies the implementation
space by the case count. Distribute instead: one method per enum value, extension methods,
type classes, overloads, pattern matching. Avoid `Option` as a parameter — map over it and
pass the simpler value. This is also the quantitative reading of SOLID
(`aᵇ < (a+1)ᵇ` for single responsibility; `(aᶜ + bᶜ) < (a+b)ᶜ` for open–closed).

### Stage 4 — Generics (theorems for free)

Parametricity shrinks implementation space to a unique inhabitant: `[A] A => A` has exactly
one implementation, `fold :: (a -> c) (b -> c) Either[a,b] -> c` likewise. Smallest absolute
shifts, but highest reuse — the marker of stage-4 code is 3–5 line bodies.

## 3. Workflow for a reduction pass

1. Run the report (§1). List stored-value rows ordered by size: `2^320` beats `ω` beats `ε₀`
   beats finite. Ignore `?` signature rows — those are analyzer resolution limits, not
   reduction targets (the `unresolved:` reasons here are mostly scalameta `Tree`/`Type`
   openness in this repo's own sources).
2. For each top row, classify which stage applies and name the smallest refactor. The report
   shows sizes, not fields — audit the construction sites (there are few) before classifying:
   - Numeric-field products (`SourceSnapshot.Limits` = 7 numeric fields = `2^320`,
     `AnalysisQuery.Budget` = `2^256`, `AnalysisWork` ctor = `2^96`): stage 1/2 — check whether
     the fields are genuinely independent knobs enforced on separate paths; if a test or doc
     pins the independence, leave it.
   - String-factor products (an `ω` row with generic-sounding fields — `budget`, `frontier`,
     `message`): two checks at the construction sites. First, closed vocabulary: a field
     assembled from a handful of string literals is a hidden enum (stage 2). Second,
     correlated fields: if one field's meaning depends on another field's value — an
     operation label for some values, a source path for others — the product admits
     combinations no value can produce; split it into a sum of cases (stage 1). Worked
     example: `Exhausted(budget: String, frontier: String)` had 4 literal budgets and a
     frontier that was an operation name for three stops but a path for one; the refactor is
     `Fuel(index | target, operation)`, `ResolutionDepth(operation)`, `SyntaxDepth(path)`,
     with each case rendering the exact prior message string.
   - Genuinely open Strings (identifiers, source text, arbitrary type syntax —
     `Definition.Unbound.Syntax` embeds `TypeSubstitution.described`'s fallback): leave them.
   - Method rows with enum/`Option` parameters: stage 3 — split per case. The report only
     flags the parameter; finding the case splits themselves is a source-level pass — §4.
3. Propose, then re-measure: behavior preserved is the hard gate — `sbt test` for core,
   `sbt plugin/scripted` for plugin behavior. The report is the scoreboard with one blind
   spot: products are deliberately coarse, so a class with any `ω`-factor (`String`, `List`)
   reads `ω` no matter how the other factors shrink. Narrowing a finite factor beside an
   `ω` factor (a hidden enum inside such a product) shows no change — groundwork, not a win.
   Splitting the product into a sum does move the row: `ω` → `3ω`, the case count surfaced
   as an explicit coefficient. Count that as a win even though the tier is unchanged, and
   record the before/after reading in the baseline note per the ratchet.
4. Ratchet: refresh `docs/baselines/self-<date>.txt` and never land a run that moves an
   existing row **up a tier** (`ω` → `ε₀`, `2^n` growing) without an explicit justification.
   A same-tier coefficient appearing (`ω` → `3ω`) is precision gained by a sum split, not a
   regression — justify it in the baseline note.

## 4. Stage-3 pass: sums in exponents (source-level audit)

The report cannot show this class of problem: a parameter's size reads the same whether the
caller dispatches the sum or branches around it, and stored-value rows never move when only
method bodies change. Run it as a separate pass over the sources.

1. **Inventory the build's own sums**: `enum`, `sealed trait`, `sealed abstract class` in
   `core/src/main`. Third-party sealed hierarchies (scalameta's `Type`/`Tree`/`Defn`/`Stat`)
   are visitor territory — splitting those per case is not a win; leave them.
2. **Find the external case analysis**: `= x match` / `isInstanceOf[` / `== caseObject`
   where `x` is one of those sums — directly, or as a field of a product parameter — and the
   site lives outside the sum's own methods.
3. **Classify each site before touching it**:
   - *Case analysis outside the sum* — an `exists(_.isInstanceOf[Case])` or a `match`
     duplicating a question the sum should answer: move it into the sum as a `def` on `this`.
     Worked example: `Definition.instantiationDependent` matched on `Unbound` cases and
     `Definition.open` used `isInstanceOf[Unbound.Open]`; the refactor put
     `instantiationDependent`/`opensAbstraction` on `Unbound` itself, and the object helpers
     delegate (`unbound.forall(_.instantiationDependent)`). The sum owns its cases; callers
     can no longer reach into them.
   - *Switchboard* — a few cases, each branch a one-line delegation onto a dedicated function
     (`SingletonIntersections.resolved`, the `Found` match in `MethodAnalysis`): the article
     endorses pattern matching that isolates the sum's value before the body is read; keep.
   - *Companion renderer or ADT interpreter* (`FinitePart.render`, the `Shape` matches in the
     inhabitation solver): the canonical exceptions; keep.
   - *Equality predicates on case objects* (`edge.kind == Continuation.OptionBranch`): fine
     on their own; extract into the sum only when the same predicate repeats across sites.
4. **Expect no report movement, and accept it**: moving dispatch into a sum adds signature
   rows (the new `def`s) and drops none. That is the article's own prescription — dynamic
   dispatch is the language feature that addresses sums-in-exponents — so this pass is gated
   by behavior tests, not by the ratchet's row diff. Record the classification table and the
   keep/refactor verdicts in the baseline note so the next pass starts from it.

## 5. Stage-4 pass: generics (theorems for free)

Parametricity narrows implementation space to (near-)unique inhabitants: `[A] A => A` has
exactly one implementation, and a function that receives its operation as a parameter cannot
do anything but structure. The report cannot show this — `?`-tier signature rows hide
implementation space — so run it as a source-level pass. Scout sweeps must grep first
(`def \w+.*List\[`, `@tailrec`, `foldLeft|foldRight`) and read only hit regions: whole-file
reads blow a scout's context on this codebase.

Three buckets, in order of value:

1. **Type-duplicated helpers** — near-identical functions differing only in the operated
   type: one generic function replaces them, taking the varying operation as a parameter.
2. **Hand-rolled recursions re-implementing existing machinery** — manual folds or loops that
   restate `TypeSubstitution.hylo`/`localCata`/`plated` (cats-eo `Schemes`) or a cats
   combinator. Swap, preserving semantics exactly.
3. **Concrete-typed purely structural functions** — containers/primitives in and out, body
   free of domain logic: a type parameter would make the implementation parametrically
   unique. Require a second use or an already-forced body; generalizing a single-caller
   helper is speculation, not a win.

**Verify library semantics with cellar before swapping.** Cellar (MCP/CLI) is allowed here for
external dependency API lookups: `cellar get-external org.typelevel:cats-core_3:2.13.0 cats.Traverse`
or, from the project root, `cellar get --module core <fqn>`; drop to
`cellar get-source <coordinate> <fqn>` when the doc does not pin the behavior. Never guess
failure order, error accumulation, or strictness from a combinator's name — the swap is only
behavior-preserving if the semantics match (example: `Shape.sequenceEithers` short-circuits
with the leftmost `Left`; cats `Traverse.sequence` for `Either` matches, which is why the
swap landed). Cellar is for external artifacts only — internal symbols go through the
language server, not cellar.

**Gates**: behavior tests are the hard gate; expect no report movement (parametric bodies do
not move signature or stored-value rows — `Shape.sequenceEithers` stayed one `?` row through
the swap). Record what was swapped and which semantics were checked in the baseline note.
Reject domain-coupled candidates (`Measurement.resolve`, the solver walks): there the
implementation space is pinned by invariants, not types, and generalization is type
shuffling.
