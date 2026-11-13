# Symbolic algebra evaluation

Isolated, unpublished sbt/JMH project comparing Symja 3.2.0, Rings 2.5.5, a hand-rolled
four-coordinate model carrier (the design issue #39 plans), and this checkout's cardinality
`Size`/`Natural` baselines.
It is not aggregated by the root project and does not add dependencies to core. See
`REPORT.md` for measured results.

## Reproduce

Run from the repository root, using the same JDK for all forks:

```bash
sbt 'algebraBench/runMain cardinality.bench.algebra.Verify self'
sbt 'algebraBench/Jmh/run -prof gc -rf json -rff bench-algebra/results/jmh.json cardinality.bench.algebra.*Bench.*'
timeout 90s sbt 'algebraBench/runMain cardinality.bench.algebra.Stress'          # corpus, beyond-polynomial
timeout 90s sbt 'algebraBench/runMain cardinality.bench.algebra.Stress huge'
timeout 90s sbt 'algebraBench/runMain cardinality.bench.algebra.Stress symbolic'
timeout 90s sbt 'algebraBench/runMain cardinality.bench.algebra.Stress boundary'
timeout 180s sbt 'algebraBench/runMain cardinality.bench.algebra.DependencyMeasure'
python3 bench-algebra/summarize.py
```

The stress commands deliberately run in separate, heap-bounded processes. A timeout, a
refusal or an unsupported query is a result, not permission to expand a billion-degree
polynomial.

## Use cases

Nothing here solves an expression to a numerical value. The two use cases are the ones the
analyzer needs:

1. **Compare**: decide `<`, `=`, `>` under the model's leading-degree order — equal degrees
   compare coefficients from the top down. A CAS's own expression sorting is not that order.
2. **Project**: render the reporting projection `aε₀ + bωⁿ + cω + d` — every degree ≥ 2
   collapses into one tier carrying the sum of its coefficients.

Construction (binomial, product, sum, parametrized by n) is timed as context: the counting
rules build these expressions before comparing or projecting them.

All polynomial workloads use nonnegative integer coefficients and a formal symbol `w`, not
built-in Infinity. `Size` uses its existing ω marker and existing coarse arithmetic; its
multiplication collapses `2·ω` to `ω`, so `w^3` and `2·w^2` both become `ω` — the degree and
coefficient information the ωⁿ tier exists to carry — and its timings are an approximation
baseline, not a performance win over equivalent work. Its power rules do encode the model's
exponent conventions, so `w^w` already reaches `ε₀`.

## The twelve-case corpus

Each case is an expression the analyzer's arithmetic produces, tied to the Scala shape that
gives rise to it; `w` stands for `ω`. The first eight are polynomials a materialized carrier
can hold. The last four put `w` in the exponent — a huge finite exponent, two polynomial
exponents and an infinite one — which is where the function spaces of ordinary types live.
Powers take their exponent as a value, because a function space raises the codomain to the
domain's cardinality and that domain can itself be a polynomial.

| Case | Shape | Expression | Model projection |
| --- | --- | --- | --- |
| eitherStrings | `Either[String, String]` | `w + w` | `2ω` |
| optionString | `Option[String]` | `w + 1` | `ω + 1` |
| pairStrings | `(String, String)` | `w·w` | `ωⁿ` |
| pairOptions | `(Option[String], Option[String])` | `(1+w)^2` | `ωⁿ + 2ω + 1` |
| tripleOptions | `(Option[String], Option[String], Option[String])` | `(1+w)^3` | `4ωⁿ + 3ω + 1` |
| booleanOption | `(Boolean, Option[String])` | `2·(1+w)` | `2ω + 2` |
| eitherStringPair | `Either[String, (String, String)]` | `w^2 + w` | `ωⁿ + ω` |
| optionEither | `(Option[String], Either[Boolean, String])` | `(1+w)·(2+w)` | `ωⁿ + 3ω + 2` |
| intToString | `Int => String` | `w^(2^32)` | `ωⁿ` |
| optionDomain | `Option[String] => String` | `w^(1+w)` | `ε₀` |
| eitherDomain | `Either[String, (String, String)] => String` | `w^(w^2+w)` | `ε₀` |
| stringToString | `String => String` | `w^w` | `ε₀` |

### Comparison pairs

| Pair | Left | Right | Expected |
| --- | --- | --- | --- |
| eitherBelowPair | `2w` | `w^2` | `<` (the ordering this evaluation exists to check) |
| pairBelowSumOfPowers | `w^2` | `w^2 + 2w + 1` | `<` |
| sumBelowCube | `w^2 + 2w + 1` | `w^3 + 3w^2 + 3w + 1` | `<` |
| eitherPairBelowOptionPair | `w^2 + w` | `w^2 + 2w + 1` | `<` |
| optionBelowBooleanOption | `w + 1` | `2w + 2` | `<` |
| mixedAboveOptionPair | `w^2 + 3w + 2` | `w^2 + 2w + 1` | `>` |
| squaredEqualsExpansion | `(1+w)^2` (factored) | `w^2 + 2w + 1` (expanded) | `=` |

### Operations

| Operation | Work timed |
| --- | --- |
| compare | Decide one corpus pair under the model's order |
| project | Render the projection of one polynomial corpus case |
| binomial / product / sum | Construct expressions at size n (scaling context) |
| render | Print a prepared expression in the backend's native representation |
| finitePower | `2^n`: the exact coefficient arithmetic a tier coefficient needs |

The beyond-polynomial cases are not benchmarked for every backend: Rings refuses to
construct them, so `Verify` classifies each backend's answer as exact, refused or silently
wrong, and `Stress` probes Symja's compact expression trees on them. The model carrier
answers all twelve cases — its huge finite powers cost logarithmic work and never expand.

Symja's projection reads the two low coefficients and substitutes `w = 1` exactly; that
substitution is an implementation detail of the use case, not a solve handed to a user. A
query that stays symbolic refuses instead of decoding to `0`.

Adapters use typed construction, not string parsing in the timed region. Rings inputs are
cloned before mutating operations. Symja uses one evaluator per JMH thread with history
disabled. Evaluator initialization and prepared inputs are outside timing. These are warmed,
repeated-input measurements, including any backend caching; they do not measure fresh-process
startup or arbitrary-input cache misses. JMH consumes returned objects; finite inputs come
from mutable @Param state.

Defaults: one thread, two JVM forks, three 300 ms warmups, five 300 ms measurements,
average microseconds/op, GC profiler for allocated bytes/op, fixed 768 MiB maximum heap.
Short iterations make this exploratory evidence; retain JMH confidence intervals and rerun
unstable or decision-critical comparisons with longer warmup. Do not run heavyweight tests
or another benchmark concurrently with JMH.

## Correctness and self-measurement

`Verify` derives every expectation independently of the backends: the eight polynomial
projections and the seven comparison signs by hand from the shapes, the beyond-polynomial
model values from the tier rules. A backend may refuse a case it cannot decide, but it must
not answer wrong; violations are recorded as `SILENT WRONG ANSWER` in
`results/correctness.txt`. `Verify` also checks that operands are not mutated, records
`Size`'s documented coarse semantics, and confirms that `Natural` keeps `2^65537` compact.

`Verify self` runs this library on the shared algebra and adapters with sources only.
`DependencyMeasure` supplies actual compiled dependency signatures and `Size` source, with
explicit analysis budgets. It renders selected Symja and Rings packages while using the full
runtime classpath for type resolution, and records missing declarations/refusals. This
measures exposed implementation choices under the cardinality contract, not runtime
complexity, proof of adapter correctness, or library quality. Do not interpret `?` as high
cardinality or fewer LOC as fewer possible implementations.

The LOC comparison is physical lines and nonblank/non-comment-only lines for the adapters,
the shared glue and the existing Size/Natural sources. It does not compare the entire CAS
implementations or claim the adapters implement our full model.
