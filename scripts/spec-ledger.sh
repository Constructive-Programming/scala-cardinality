#!/usr/bin/env bash
#
# The ledger of core's specifications against the compiled front end (`TastyFrontEnd`).
#
#   scripts/spec-ledger.sh <ledger file> [work directory, default a new temporary one]
#
# Runs `core/testFull` four times, each with the fixture harness (`cardinality.Compiled`) under
# another `-Dcardinality.frontend` and with every example's outcome logged
# (`cardinality.ExampleLog`):
#
#   probe     every request for rows fails: the examples that fail so are the ones that read rows
#   tasty     the rows are `TastyFrontEnd`'s
#   sentinel  the same, each row it does not read yet holding a count of its own no example expects
#   filled    the same, each row it does not read yet holding what the scalameta pipeline gives
#             the row of that name: a method row's count and captures, a definition row's size and
#             reasons (the n-th row of a name for the n-th, where both front ends list as many;
#             the name, kind, signature and line stay the compiled front end's)
#
# and writes one line for each example that reads rows, or what only the scalameta front end has,
# sorted, as `specification<TAB>example<TAB>status<TAB>cause`:
#
#   PASS       passes under tasty and under sentinel: it does not rest on a row not read yet
#   UNCOVERED  passes under tasty and fails under sentinel (it pins a refusal, and `not read from
#              compiled classes yet` is one), or fails under tasty and passes under filled (it fails
#              only for counts not read yet). The cause is the failure under tasty, or how it passed.
#   FAIL       fails under tasty and under filled. The cause is the failure under filled: what is
#              left when the counts not read yet are answered.
#   PENDING    pending under tasty, or failing there and pending under filled: a target its
#              specification marks as not met yet, which says nothing of the front end.
#   SCALAMETA  reads what only the scalameta front end has (its tree, the open world, dependency
#              stubs given as sources): the example is not one of rows.
#
# A row counts as not read yet only where every reason it gives starts with
# `TastyFrontEnd.Uncovered`; the harness decides that, row by row, not this script from a message.
# Blind spots: an example that only says what a row is not (`count !== Finite(1)`), or whose
# expectation a row without a count meets too (a total of nothing), passes whatever the sentinel
# is, and reads PASS on a row not read yet. A PENDING example stays PENDING whatever number the
# compiled front end gives: only its cause changes. The cause is the first failure of an example,
# so one that reads two fixtures shows one of them.
#
# Prints the totals, and for each specification how many of its examples are of each status and
# how many read neither rows nor anything of the scalameta front end through the harness. Fails,
# leaving the ledger file as it was, when a run logs nothing, when an example that reads rows was
# not run under every front end (a specification that reads rows where it is constructed fails
# whole: read them in a `lazy val`), or when an example fails under probe for another reason.
#
# The runs fail by design, so their exit status is not read; a run's log is `<work>/<mode>.log`
# and its examples `<work>/<mode>.tsv`. Needs GNU awk. Paths may not hold spaces (sbt splits its
# arguments on them).
set -euo pipefail

if [ $# -lt 1 ] || [[ "$1${2:-}" =~ [[:space:]] ]]; then
  echo "usage: $0 <ledger file> [work directory]" >&2
  exit 2
fi
ledger=$(realpath -m "$1")
work=$(realpath -m "${2:-$(mktemp -d)}")
mkdir -p "$work"
cd "$(dirname "$0")/.."

for mode in probe tasty sentinel filled; do
  rm -f "$work/$mode.tsv"
  sbt "set core / Test / javaOptions ++= Seq(\"-Dcardinality.frontend=$mode\", \"-Dcardinality.examples=$work/$mode.tsv\")" \
    'set core / Test / testOptions += Tests.Argument(TestFrameworks.Specs2, "notifier", "cardinality.ExampleLog")' \
    core/testFull < /dev/null > "$work/$mode.log" 2>&1 || true
  if [ ! -s "$work/$mode.tsv" ]; then
    tail -n 40 "$work/$mode.log" >&2
    echo "no example was logged under $mode: full log in $work/$mode.log" >&2
    exit 1
  fi
done

: > "$work/ledger.tsv"
LC_ALL=C gawk -F'\t' -v OFS='\t' -v ledger="$work/ledger.tsv" '
  # What the harness says where rows are requested under probe, and where an example reads what
  # only the scalameta front end has (`Compiled.read`, `Compiled.scalametaOnly`).
  BEGIN {
    probed = "rows requested from the fixture harness"
    scalameta = "which only the scalameta front end has"
  }
  FNR == 1 { run++ }
  {
    key = $1 OFS $2
    keys[key] = $1
    outcome[run, key] = $3
    message[run, key] = $4
  }
  END {
    count = asorti(keys, sorted)
    for (i = 1; i <= count; i++) {
      key = sorted[i]
      spec = keys[key]
      examples[spec]++
      reads = index(message[1, key], probed) > 0
      if (index(message[2, key], scalameta) > 0) {
        status = "SCALAMETA"
        cause = ""
      } else if (!reads) {
        if (outcome[1, key] == "pass" || outcome[1, key] == "pending") continue
        missing = missing "\n  " key
        continue
      } else if (!((2, key) in outcome) || !((3, key) in outcome) || !((4, key) in outcome)) {
        missing = missing "\n  " key
        continue
      } else if (outcome[2, key] == "pending" || (outcome[2, key] == "fail" && outcome[4, key] == "pending")) {
        status = "PENDING"
        cause = message[4, key]
      } else if (outcome[2, key] == "pass" && outcome[3, key] == "pass") {
        status = "PASS"
        cause = ""
      } else if (outcome[2, key] == "pass") {
        status = "UNCOVERED"
        cause = "passes on a row not read yet"
      } else if (outcome[4, key] == "pass") {
        status = "UNCOVERED"
        cause = message[2, key]
      } else {
        status = "FAIL"
        cause = message[4, key]
      }
      print key, status, cause > ledger
      total[status]++
      of[spec, status]++
    }
    if (missing != "") {
      print "examples not run under every front end, or failing under probe for another reason:" missing > "/dev/stderr"
      exit 1
    }
    printf "PASS %d  UNCOVERED %d  FAIL %d  PENDING %d  SCALAMETA %d\n", total["PASS"], total["UNCOVERED"], total["FAIL"], total["PENDING"], total["SCALAMETA"]
    print "specification", "examples", "PASS", "UNCOVERED", "FAIL", "PENDING", "SCALAMETA", "neither"
    specs = asorti(examples, names)
    for (i = 1; i <= specs; i++) {
      spec = names[i]
      rows = of[spec, "PASS"] + of[spec, "UNCOVERED"] + of[spec, "FAIL"] + of[spec, "PENDING"] + of[spec, "SCALAMETA"]
      print spec, examples[spec], of[spec, "PASS"] + 0, of[spec, "UNCOVERED"] + 0, of[spec, "FAIL"] + 0, of[spec, "PENDING"] + 0, of[spec, "SCALAMETA"] + 0, examples[spec] - rows
    }
  }
' "$work/probe.tsv" "$work/tasty.tsv" "$work/sentinel.tsv" "$work/filled.tsv"
mkdir -p "$(dirname "$ledger")"
mv "$work/ledger.tsv" "$ledger"
echo "runs: $work"
