#!/usr/bin/env bash
#
# Measures one frozen corpus with this checkout's build.
#
#   scripts/measure-corpus.sh <corpus directory> <report file> [runs, default 3]
#
# Each run is one `Report.of` in a JVM of its own (`cardinality.bench.CorpusRun`, forked by the
# `bench` project with the heap and stack build.sbt gives it). Only that call is timed: sbt's start,
# the compile of this checkout and the JVM's start are outside it. Prints every run's `PERF` line,
# the median with the lowest and highest run for wall time, CPU time and peak heap (the lower of the
# two middle runs when the count is even), and the first run's `ROWS` line; writes the
# rendered report to <report file>. Fails when two runs render different reports, or when the
# report read no source.
#
# Compare two reports with
# `.claude/skills/cardinality-reduction/report-diff.scala` (`--rows` lists the rows that moved).
#
# Run it on a machine doing nothing else: the report uses every core. Paths may not hold spaces
# (sbt splits its arguments on them).
set -euo pipefail

if [ $# -lt 2 ] || ! [[ ${3:-3} =~ ^[1-9][0-9]*$ ]] || [[ "$1$2" =~ [[:space:]] ]] ||
  [ ! -f "$1/classpath.txt" ]; then
  echo "usage: $0 <corpus directory> <report file> [runs]" >&2
  echo "  the corpus directory holds classpath.txt; no path holds a space; runs is 1 or more" >&2
  exit 2
fi
corpus=$(realpath "$1")
report=$(realpath -m "$2")
runs=${3:-3}
cd "$(dirname "$0")/.."

log=$(mktemp)
commands=()
for run in $(seq "$runs"); do
  commands+=("bench/runMain cardinality.bench.CorpusRun $corpus $report.$run")
done
if ! sbt "${commands[@]}" < /dev/null > "$log" 2>&1 || [ "$(grep -c '^PERF ' "$log")" -ne "$runs" ]; then
  tail -n 40 "$log" >&2
  echo "measurement failed: full log in $log" >&2
  exit 1
fi

for run in $(seq 2 "$runs"); do
  if ! cmp -s "$report.1" "$report.$run"; then
    echo "run $run rendered another report than run 1: the reports are kept as $report.<run>" >&2
    rm "$log"
    exit 1
  fi
done
for run in $(seq 2 "$runs"); do rm "$report.$run"; done
mv "$report.1" "$report"

# The median of one PERF field, with the lowest and highest run.
spread() {
  grep '^PERF ' "$log" | sed -E "s/.* $1=([0-9.]+) .*/\1/" | sort -n |
    awk '{ v[NR] = $1 } END { printf "%s (%s to %s)", v[int((NR + 1) / 2)], v[1], v[NR] }'
}
grep '^PERF ' "$log"
echo "MEDIAN of $runs: wall=$(spread wall) s cpu=$(spread cpu) s peakHeap=$(spread peakHeap) GiB"
grep -m 1 '^ROWS ' "$log"
if grep -q '^ROWS sources=0 ' "$log"; then
  echo "the report read no source under $corpus" >&2
  rm "$log"
  exit 1
fi
rm "$log"
