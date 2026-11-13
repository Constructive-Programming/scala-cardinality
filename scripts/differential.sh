#!/usr/bin/env bash
#
# Runs the differential gate of the front-end rewrite (docs/plans/compiled-own-types.md) with this
# checkout's build: the scalameta and the TASTy front end over the same inputs, every row
# classified (`cardinality.compiled.Differential`).
#
#   scripts/differential.sh <assets directory> <output directory> [input ...]
#
# The inputs are every frozen corpus `<assets>/corpus-*` and the compiled test fixtures
# (`core/target/compiled-fixtures`, which `sbt core/testFull` fills); naming some (`corpus-core`,
# `fixtures`) runs only those. Each input is one run in a JVM of its own. Beside the corpora the
# gate keeps:
#
#   <assets>/old/<input>-<hash>.bin   the scalameta front end's rows of a corpus, which take minutes
#                                     to compute; <hash> is of this checkout's build.sbt and
#                                     project/ files, core's main sources but the new front
#                                     end's, and the gate, so a change to any computes them again.
#                                     Nothing removes the files of earlier hashes; delete them,
#                                     and every file of a corpus that changes
#   <assets>/covered/<input>.txt      the rows the TASTy front end measured at the last passing
#                                     run, with their counts: one of them uncovered or counted
#                                     otherwise now fails the run (to accept a changed count, delete
#                                     its line); a passing run keeps the lines of rows it did not
#                                     see
#
# and reads `core/src/test/resources/compiled/derived.txt`: the better rows past a model limit that
# were derived by hand; and `corrected.txt` beside it: the rows whose number changes because the old
# one is wrong, each with both values and the derivation; and `old-unsound.txt`, the old numeric
# rows whose number rests on a rule shown unsound, which the summary sets apart. Each run's output is `<output>/<input>.txt` and its build log
# `<output>/<input>.log`. Prints each input's totals and whether it passed; exits with 1 when any
# input fails. Paths may not hold spaces (sbt splits its arguments on them).
set -euo pipefail

if [ $# -lt 2 ] || [[ "$1$2" =~ [[:space:]] ]] || [ ! -d "$1" ]; then
  echo "usage: $0 <assets directory> <output directory> [input ...]" >&2
  exit 2
fi
assets=$(realpath "$1")
output=$(realpath -m "$2")
shift 2
cd "$(dirname "$0")/.."
if ! compgen -G "$assets/corpus-*" > /dev/null; then
  echo "no corpus-* directory under $assets" >&2
  exit 2
fi
mkdir -p "$output" "$assets/old" "$assets/covered"

gate=core/src/test/scala/cardinality/compiled/Differential.scala
hash=$({ find core/src/main/scala -name '*.scala' -not -path '*/compiled/*' -print0 | sort -z |
  xargs -0 sha1sum; sha1sum "$gate" build.sbt project/*.sbt project/*.scala \
    project/build.properties; } |
  sha1sum | cut -c1-12)
derived=core/src/test/resources/compiled/derived.txt
corrected=core/src/test/resources/compiled/corrected.txt
unsound=core/src/test/resources/compiled/old-unsound.txt
inputs=("$@")
if [ ${#inputs[@]} -eq 0 ]; then
  for corpus in "$assets"/corpus-*; do inputs+=("$(basename "$corpus")"); done
  inputs+=(fixtures)
fi

status=0
for input in "${inputs[@]}"; do
  if [ "$input" = fixtures ]; then
    what="fixtures $PWD/core/target/compiled-fixtures"
  else
    what="corpus $assets/$input --old $assets/old/$input-$hash.bin"
  fi
  rm -f "$output/$input.txt"
  passed=FAILED
  if sbt "bench/runMain cardinality.compiled.Differential $what \
--covered $assets/covered/$input.txt --derived $PWD/$derived --corrected $PWD/$corrected --unsound $PWD/$unsound --out $output/$input.txt" \
    < /dev/null > "$output/$input.log" 2>&1; then
    passed=passed
  else
    status=1
  fi
  if ! grep -q ' inputs in ' "$output/$input.txt" 2> /dev/null; then
    tail -n 40 "$output/$input.log" >&2
    echo "$input: the gate did not finish: $output/$input.log" >&2
    exit 1
  fi
  echo "== $input: $passed"
  sed -n '/^[a-z]* rows: /,/inputs in /p' "$output/$input.txt"
  # What a front end could not read, and what the old one says it left unsupplied.
  grep -E 'could not read|^no source read|^(scalameta|tasty) (warns|notes) in' \
    "$output/$input.txt" || true
done
exit $status
