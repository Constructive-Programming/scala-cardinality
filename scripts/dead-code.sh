#!/usr/bin/env bash
#
# Lists the production symbols nothing references (scripts/dead-code.scala says how to read the
# lists). Compiles every module and its tests first: the scan reads the SemanticDB that compile
# writes. Needs scala-cli.
set -euo pipefail
cd "$(dirname "$0")/.."
log=$(mktemp)
if ! sbt compile Test/compile < /dev/null > "$log" 2>&1; then
  tail -n 40 "$log" >&2
  echo "compile failed: full log in $log" >&2
  exit 1
fi
rm "$log"
scala-cli run scripts/dead-code.scala
