# Agent guide

This file is for coding agents working on `scala-cardinality`. The [README](README.md) is the
entry point for people; this file repeats the rules an agent must not get wrong.

## The modelling assumption: no side effects

Every count assumes that **the measured code has no side effects**. That applies to the
stored-value capacity of definitions and to the method-side implementation counts. Hold to it
in code, tests and docs:

- **Treat a `var` as a `val`.**
  - A `var` member, local or constructor parameter holds the value it was given.
  - Reassignment is never modelled.
  - Never refuse a count, or add a reason, because something is mutable. A `var` field of a
    capability record or class is read like a `val` field, and a captured `var` is an ordinary
    capture.
  - Only Scala's own typing rules still distinguish a `var`. It is not a stable path, so
    `p.type` and `p.T` through a `var` do not typecheck. Word any such refusal as a typing fact,
    not as mutation.
- **Ignore throwing.**
  - A `throw`, or a call that may throw, is neither a value nor an implementation. A body or
    default whose result is a throw supplies nothing.
  - Never withhold a count because code might throw.
  - Exceptions used as data are ordinary values of open `java.lang` classes: a `Throwable`
    passed, stored or returned.
- **Exclude every other effect the same way**, like the universal members of `Any`: I/O,
  logging, `printStackTrace`, `fillInStackTrace`, `addSuppressed`, `initCause`, and so on.
  Divergence, a `null` the type does not declare, unsafe casts, reflection and unrestricted
  runtime type or equality tests are outside the model too.

The analyzer does not certify that a body obeys the assumption. A `0` means "no pure, total,
parametric implementation", not "no Scala body compiles".

## Where the model is defined

The analyzer's code does not define what a number means; these documents do.

- [docs/plans/v1.md §2](docs/plans/v1.md), "Agreed counting contract". It sets the two
  questions (implementation cardinality and stored-value cardinality), the scope and
  equivalence, the assumptions and what each output means.
- [docs/counting-type-inhabitants.md](docs/counting-type-inhabitants.md) covers the method side:
  observational equivalence, parametric atoms, sums, products and exponentials, least fixed
  points and absurd elimination.
- [docs/type-arithmetic.md](docs/type-arithmetic.md) covers the stored-value side.
- The per-rule notes are in [docs/research/](docs/research). The evidence register,
  [code-cardinality-foundations.md](docs/research/code-cardinality-foundations.md), records the
  sources and the status of each rule.

## Correct numbers before coverage

- A wrong number is worse than a `?`. When the supplied sources do not determine a count, report
  `?` with a specific reason. Examples are an unsupplied declaration, an overridable member and
  an inferred type that cannot be typed soundly. Never use `ω`, `ε₀` or a convenient finite
  answer as an unknown-value fallback.
- Follow the working loop in v1.md §1:
  - Derive the expected count independently of the analyzer.
  - Add a regression that contains the relevant source and scope.
  - Implement the general rule, never a special case for one signature.
- Every changed count in a report run must be explained. A pinned expectation that changes needs
  its derivation.

## Build and gates

The build uses sbt 2 (`project/build.properties`) and Scala 3.9.0 (`build.sbt`). `core` is the
calculator, a pure library tested with specs2. `plugin` is the sbt plugin, tested with scripted.

```bash
sbt core/testFull                    # core test suite, uncached
sbt plugin/scripted                  # plugin end-to-end tests
sbt scalafmtAll scalafixAll          # formatting and semantic rules (CI checks both)
sbt coverageAll                      # coverage report; CI gates on a floor
bash scripts/check-file-metrics.sh   # per-file length and comment-ratio gate
```

CPD (PMD, duplicates of 25 or more tokens) also gates in CI. sbt 2 keeps a machine-wide task
cache, so a re-run can report "no tests to run"; see the README's note on cold runs.
