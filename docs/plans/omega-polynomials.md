# Intermediate power tier in both counting algebras

## Agreed scope

Add one tier between ω and ε₀ to both stored-value estimates and method
implementation counts. Write the tier as `P = ωⁿ`: it represents any finite power
of ω greater than one, without recording or comparing individual exponents.
Thus ω² and ω³ both report P. P is an analysis marker, not a variable exponent.

Preserve coefficients and distribute multiplication over sums. Cover collections
and recursion in this change. These decisions replace the earlier proposal for
sparse per-degree polynomials and a stored-value-only first stage.

The response becomes `aε₀ + bP + cω + d`, ordered lexicographically in that order.
A pair of Strings therefore ranks above `Either[String,String]`: P > 2ω.
This remains structural ordering by convention, not an ordinary cardinal
inequality between these countably infinite spaces.

## Arithmetic design

One additional coefficient field is sufficient. There is no degree map, no
representation of huge exponents, and no per-degree expansion. Preserve each
algebra's finite precision: Size has exact/capacity/lossy finite estimates; Card
has exact or symbolic Natural expressions. Do not route exact method counts
through the rounded Size carrier.

Share the tier multiplication rules between the carriers, with separate coefficient
operations. The proposed basis multiplication table is:

| × | 1 | ω | P | ε₀ |
| --- | --- | --- | --- | --- |
| 1 | 1 | ω | P | ε₀ |
| ω | ω | P | P | ε₀ |
| P | P | P | P | ε₀ |
| ε₀ | ε₀ | ε₀ | ε₀ | ε₀ |

Extend this table bilinearly: multiply coefficients and add every contribution to
its output tier. In particular `2ω * 3ω = 6P`, `P * ω = P`, and
`(P + ω) * ω = 2P`. Collapsing degree does not mean dropping coefficients.
The ε₀ row is a proposed extension of the existing cap, retaining coefficients
so that distributivity also holds there. Existing finite rounding still limits
algebraic laws on estimated coefficients; test exact and approximate cases apart.

Addition is componentwise. Zero annihilates products, one preserves them, and
zero/one powers retain their current identities. Finite powers use repeated
squaring on the four coordinates; no exponent identity beyond zero, one or
greater-than-one is needed for a unit monomial. Coefficients may still become very
large: retain symbolic computation or honest bounds and existing arithmetic budgets.

The following power rules are proposed consistently for both algebras:

- A positive finite power greater than one of ω gives P; the same power of P gives P.
- An infinite base raised to an infinite exponent reaches ε₀.
- A finite base greater than one raised to an infinite exponent retains the existing
  finite-program convention of ω.
- Mixed sums raised to finite powers use the multiplication table; do not just
  select their highest tier. Powers involving finite bounds must retain their
  uncertainty rather than manufacture exact coefficients.

Keep the function-level eager empty-domain convention separate from arithmetic
`b^0 = 1`. Keep the positional reading of top types, opaque types, Rule B, purity,
closed-world rules and unresolved refusals unchanged.

## Collections and recursive boundaries

Planning assumption: retain the earlier proposed ε₀ cap for genuinely unbounded
degree. The user confirmed these boundaries are in scope; the precise cap rule
still needs its derivation in the counting contract. A finite pair or triple is
P; an arbitrary-length sequence of Strings can require unbounded degree and is
proposed to reach ε₀. Collapsing all finite powers does not by itself decide this
infinite-length boundary.

Derive collection equations from their models: finite sequences, finite subsets,
finite partial functions, and lazy streams. In particular, do not infer finite
Set/Map results solely from the total-function exponential after its approximations.
Preserve `List[Nothing] = 1` and established zero/one cases.

Analyze growth on recursive components before freezing results. The four-coordinate
carrier makes finite multiplication bounded in tier, but cannot alone distinguish
a fixed product from arbitrarily many ω factors: both eventually reach P. Retain
enough solver-local growth information, or analyze the recursive equations, to
justify promotion to ε₀. No exact exponent needs to be exposed or retained in counts.

Distinguish coefficient growth from multiplicative-depth growth. Cover finite-label
cycles, String-labelled cycles, multiple continuations, mutual recursion, lazy
completion and acyclic consumers. A settled Nat remains ω, while `(Nat,Nat)` is P;
a consumer must not collapse just because a child was recursive.

On the method side, `RuleGraph.Live.result` and `throughCycle` currently bypass
arithmetic with `Count.Countable`. Replace blanket saturation with a justified
cycle result and evaluate the surrounding rules using the new algebra. A proved
cycle establishes at least ω, not necessarily a precise structural tier when other
branches are unresolved. Revisit the C11 shortcut: preserve its evidence without
turning a lower bound into an unsupported exact tier; retain a reason when the new
contract cannot determine the result. Keep quotient/free-theorem guards intact.

## Code impact map

Core paths below are relative to `core/src/main/scala/cardinality/`. Audit entries
may work unchanged after shared operations change, but need source-level regressions.

| Area | Files | Work |
| --- | --- | --- |
| Stored algebra | `capacity/Size.scala` | Add the power coefficient; update construction, arithmetic, predicates, ordering, rendering, widening and optics assumptions. Decide coefficient bounds/lossiness, preserving exact additive coefficients where possible. |
| Method algebra | `analysis/inhabitation/Card.scala`, `Natural.scala` | Replace Exact/Omega-only saturation with four-coordinate counts; preserve exact and symbolic finite arithmetic and coefficient budgets. Share tier rules with Size without sharing its rounding. |
| Public method results | `analysis/inhabitation/Inhabitation.scala` | Extend Count, Card-to-Count and reverse conversion, rendering, threshold predicates and summary classification. Do not lose coefficients through Countable or label all new tiers as mathematically uncountable. |
| Method graph | `analysis/inhabitation/RuleGraph.scala`, `GoalSolver.scala`, `SequenceElimination.scala` | Update productive-cycle results, surrounding sums/products/powers, result matching and sequence shortcuts. Preserve productivity, absurdity and quotient safeguards. |
| Closed shapes and builtins | `analysis/inhabitation/ClosedSpaces.scala`, `analysis/BuiltinTypes.scala`, `StdlibModel.scala` | Both readings gain the tier. Replace nonliteral-to-ω conversion shortcuts with explicit conversion. Derive repeated/collection shapes from elements rather than a fixed omega constant. |
| Receivers and top types | `analysis/SealedReceivers.scala`, `ConstrainedReceivers.scala`, `resolution/TopTypes.scala` | Audit every Card.Omega match and producer: distinguish intentional base ω from destructive saturation. Preserve innermost-arrow top semantics and Rule B. |
| Type equations | `capacity/Counter.scala` | Audit tuples, class fields, sums, function domains/powers, intersections, Option/Either, Set/Map/PartialFunction, strict sequences and lazy streams. Its finite-collection guard currently only notices ε₀. |
| Stored recursion | `capacity/Solver.scala` | Revise productive-cycle widening/freezing, label products, cycle contribution and completeCoinduction. Keep termination and preserve settled tiers in consumers. |
| Constructor bridge | `capacity/Reading.scala`, `Scope.scala`, `analysis/ConstructorPositions.scala` | Replace exact-Omega-only branches and collection-only saturation assumptions. Agree conversions across algebras while preserving finite precision differences. Constructor top input remains 1; stored top field remains ω. |
| Bounds and aggregation | `capacity/Factories.scala`, `Instantiations.scala`, `Builds.scala`, `Definition.scala`, `ValueRows.scala`, `Walk.scala` | Audit products, factory min/max, bound comparisons, aggregation and settled/cached values. Factories still combine by maximum, not sum. |
| Report and API | `reporting/Report.scala`, `Entry.scala` | Replace fixed-coordinate sorting with shared ordering, preserving unresolved-first/abstract-last/name ties. Update method and definition rendering and summaries. Size cells are already text; no new table column is needed. Assess public case-class/enum source and binary compatibility. |
| Integrations | `plugin-sbt/.../CardinalityTool.scala`, `plugin/`, `sbt1-plugin/`, `mill-plugin/`, `core-native/` | Audit forwarding, snapshots and both sbt integrations. Native shares core sources. No new plugin setting is expected. |
| Contract | `README.md`, `docs/plans/v1.md`, `docs/type-arithmetic.md`, `docs/counting-type-inhabitants.md`, `docs/research/code-cardinality-foundations.md`, `AGENTS.md` | Document four tiers in both questions, coefficient multiplication and approximations. Revise D2/C11 implications and recursive/collection derivations; remove outdated three-tier and universal method-saturation claims. |

Parsing, resolution and dependency-stub extraction need no new syntax. Their
fixtures may have changed counts and belong in the output audit.

## Algebra library evaluation

An isolated JMH project ([bench-algebra](../../bench-algebra/REPORT.md)) compared Symja 3.2.0,
Rings 2.5.5, a hand-rolled sparse-map carrier and the existing `Size`/`Natural` baselines on
the two operations the analyzer performs: order comparisons (<, =, >) and the four-coordinate
projection, over a twelve-case corpus of cardinality shapes including ω in the exponent
(`Int => String` is `ω^(2^32)`, `Option[String] => String` is `ω^(1+ω)`, `String => String`
is `ω^ω`). Decision: no CAS dependency.

- Rings handles every finite-degree polynomial in the corpus exactly, then refuses at
  construction the moment the exponent stops being a small constant.
- Symja's `Exponent`/`Coefficient` queries — the only route to compare or project without
  expanding — fail from degree `2^20` (iteration limit) and return unevaluated forms at
  `Int.MaxValue`; at `2^32` and at symbolic exponents a comparison can be answered through
  collapsed degrees (0 vs 1), right or wrong by accident. For the polynomial-exponent cases
  its projection silently returns `-1 P + 1 w + 1` — a negative coefficient, impossible for
  a cardinality. `0^0` evaluates to `Indeterminate` and `ω^ω` stays unevaluated; the
  contract needs values for both.
- Current `Size` gets 0 of 7 corpus comparison signs right (its `mul` collapses `2·ω` to
  `ω`), but it is the only pre-existing backend that answers the ε₀ cases, and correctly:
  its power rules already encode the model's exponent conventions.
- The four-coordinate carrier is implemented in the evaluation itself (`ModelAlgebra`, 92
  lines): the only backend answering all twelve corpus cases exactly per the model — ω and
  ε₀ in the exponent included — and the fastest on both use cases; huge finite powers cost
  logarithmic work and never expand. `Natural` keeps huge finite coefficients compact.
- Both libraries are JVM-only (conflicting with `core-native`) and heavier than four fields.
- Multiplicative-depth growth stays a solver-local counter, not a polynomial representation.

## Implementation sequence

1. Update the agreed counting contract with the tier table, coefficient policies,
   power rules and independently derived collection/cycle rules. Capture baseline
   reports and derive expected changes before changing analyzer code.
2. Add failing algebra tests, implement shared tier operations, then extend Size
   and Card independently. Verify cross-algebra agreement where finite values are
   exact; preserve intentional precision differences.
3. Update both solvers, builtin interpretations, conversions and constructor
   handling. Add source-level tests with the relevant lexical scopes, factory
   accessibility and recursive dependencies.
4. Update public results, sorting, rendering, classifications and integration
   fixtures. Explain every changed report count before accepting snapshots.
5. Complete documentation and run targeted tests followed by repository gates.

## Acceptance cases and validation

These proposed outputs use `P` only as shorthand; reports display `ωⁿ`.

| Stored space or corresponding closed method result space | Result |
| --- | --- |
| `Either[String,String]` | `2ω` |
| `(String,String)` | `P > 2ω` |
| Three String fields | `P`, equal tier to two fields |
| `(Boolean,String)` | `2ω` |
| `(Option[String],Option[String])` | `P + 2ω + 1` |
| `Boolean => String` | `P` |
| `String => Boolean` | `ω` under finite-program convention |
| `String => String` | `ε₀` under proposed aligned power rules |
| `(Nat,Nat)` | `P`, after solving Nat |
| `List[String]` | proposed `ε₀`, subject to unbounded-degree derivation |

Add paired method/definition fixtures, including a parameterless method returning
each closed space. Test sums/products surrounding independent method cycles;
symbolic finite coefficients beside infinite tiers; coefficient bounds/lossiness;
finite versus unbounded repetitions; mutual and lazy recursion; zero/one cases;
top inputs versus stored tops; refusals; deterministic report ordering. Verify
that P*P stays P but addition retains coefficients, and test distributivity on
exact coefficients across all four tiers.

Primary existing suites: `capacity/SizeSpec`, `CounterSpec`, `CardinalitySpec`,
`StoredValueSoundnessSpec`, `StoredFactoriesSpec`, `SignatureSpec`,
`ArticleCardinalitySpec`; `analysis/inhabitation/InhabitationSpec`, `NaturalSpec`,
`SequenceEliminationSpec`, `ReferenceInhabitantsSpec`; `analysis/ReceiverConstructorsSpec`
and relevant top-type/capability suites; `reporting/ReportSpec`, `ReportQuerySpec`,
`ReportHygieneSpec`; `EoArtifactsSpec` and plugin report snapshots. Add a dedicated
Card algebra suite if direct coverage is missing.

Run `sbt core/testFull`, `sbt plugin/scripted`, the configured sbt1 scripted suite,
`sbt scalafmtAll scalafixAll`, `sbt coverageAll`, `bash scripts/check-file-metrics.sh`
and the repository CPD gate. Check all changed report rows against derivations.

The extra field is straightforward and substantially simpler than per-degree
polynomials. The cross-cutting work is aligning both algebras and replacing
cycle/collection shortcuts without claiming more precision than the model proves.
