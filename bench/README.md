# Benchmarks

JMH over a real checkout, the same inputs the `EoCheckout` runner reads. Not part of CI.

```bash
git clone https://github.com/Constructive-Programming/eo /tmp/eo && git -C /tmp/eo checkout v1.1.1
cs fetch --scala-version 3.9.0 <the checkout's dependencies> > /tmp/eo-classpath.txt
CARDINALITY_BENCH_CHECKOUT=/tmp/eo CARDINALITY_BENCH_CLASSPATH=/tmp/eo-classpath.txt \
  sbt "bench/Jmh/run"                       # all benchmarks
  sbt "bench/Jmh/run -prof jfr .*withStubs"  # one, with a flight recording
```

- `sourcesOnly`: every method row of the sources alone.
- `definitions`: every stored-value row of the sources alone.
- `renderStubs`: the dependency jars' TASTy rendered as stubs.
- `withStubs`: every fourth source's method rows with the stubs in the environment, under the
  runner's 300,000-step budget per target, on one worker.
- `withStubsParallel`, `sourcesOnlyParallel`: the same with `Measuring`'s four workers, each with
  an index of its own.
