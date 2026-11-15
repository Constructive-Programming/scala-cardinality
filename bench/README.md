# Benchmarks

One `Report.of` over a frozen corpus, timed in a JVM of its own. Not part of CI.

```bash
scripts/measure-corpus.sh <corpus directory> <report file> [runs, default 3]
```

A corpus directory holds the sources, `classes` (their compiled output) and `classpath.txt` (one
dependency jar per line); `scripts/build-corpus.sh` builds one. Each run is
`bench/runMain cardinality.bench.CorpusRun <corpus> <report>`, which prints a `PERF` line (wall,
CPU, GC, peak heap and resident set) and a `ROWS` line (rows by kind).
