#!/usr/bin/env bash
#
# Builds a frozen corpus for scripts/measure-corpus.sh from one published Scala 3 artifact, with
# nothing compiled here: the binary jar is the compiled output of the sources jar published beside
# it.
#
#   scripts/build-corpus.sh <new corpus directory> <organization:artifact_3:version>
#
# The corpus directory gets:
#   classes/        the binary jar, unpacked
#   <source path>   every source of the sources jar, at the path its TASTy records
#   classpath.txt   the artifact's dependency jars (in the coursier cache), one per line
#   COORDINATES     the artifact, as given
#
# A sources jar holds each source at its package path (`cats/Functor.scala`); TASTy records it where
# the library's build had it (`core/src/main/scala/cats/Functor.scala`). The recorded paths are read
# from the name tables of the `.tasty` files with `strings`, and each one names the source that is
# its longest tail among the jar's sources. A source no recorded path names is left out and listed;
# one that two recorded paths name fails the build.
#
# Nothing here checks that the two jars are the same build: they are fetched by the same
# coordinates. A source at another path than its TASTy records is refused by the report ("no
# compiled classes for this source").
#
# Needs coursier (`coursier`, or `cs` where that name is coursier's), `unzip`, `strings` and GNU
# `find`.
set -euo pipefail

if [ $# -ne 2 ]; then
  echo "usage: $0 <new corpus directory> <organization:artifact_3:version>" >&2
  exit 2
fi
out=$1
artifact=$2
if [ -e "$out" ]; then
  echo "$out exists already: a corpus is built once and never changed" >&2
  exit 1
fi
coursier=$(command -v coursier || command -v cs) || {
  echo "coursier is not on the PATH" >&2
  exit 1
}

unpacked=$(mktemp -d)
# A failed build leaves no half-made corpus behind.
trap 'status=$?; rm -rf "$unpacked"; [ "$status" -eq 0 ] || rm -rf "$out"' EXIT
fail() {
  echo "$1" >&2
  exit 1
}

binary=$("$coursier" fetch --intransitive "$artifact")
sources=$("$coursier" fetch --sources --intransitive "$artifact")
[ "$(printf '%s\n' "$binary" "$sources" | grep -c '\.jar$')" -eq 2 ] ||
  fail "expected one binary jar and one sources jar for $artifact"

mkdir -p "$out/classes"
unzip -q "$binary" -d "$out/classes"
unzip -q "$sources" -d "$unpacked"
"$coursier" fetch "$artifact" | { grep -vxF "$binary" || true; } > "$out/classpath.txt"
echo "$artifact" > "$out/COORDINATES"

recorded=$(find "$out/classes" -name '*.tasty' -exec strings -n 7 {} + | { grep '\.scala$' || true; } | sort -u)
[ -n "$recorded" ] || fail "no TASTy in $binary records a source: is $artifact a Scala 3 artifact?"
if printf '%s\n' "$recorded" | grep -qE '(^|/)\.\.(/|$)'; then
  fail "a recorded source path leaves the corpus directory"
fi
entries=$(cd "$unpacked" && find . -name '*.scala' -printf '%P\n' | sort)

# Each recorded path beside the source it names: its longest tail that is a source of the jar.
placed=$(awk -F/ '
  NR == FNR { source[$0]; next }
  {
    for (i = 1; i <= NF; i++) {
      tail = $i
      for (j = i + 1; j <= NF; j++) tail = tail "/" $j
      if (tail in source) { print tail "\t" $0; next }
    }
  }' <(printf '%s\n' "$entries") <(printf '%s\n' "$recorded"))
twice=$(printf '%s\n' "$placed" | cut -f 1 | sort | uniq -d)
[ -z "$twice" ] || fail "two recorded paths name one source: $twice"

while IFS=$'\t' read -r source path; do
  [ -n "$source" ] || continue
  mkdir -p "$out/$(dirname "${path#/}")"
  cp "$unpacked/$source" "$out/${path#/}"
done <<< "$placed"
absent=$(comm -23 <(printf '%s\n' "$entries") <(printf '%s\n' "$placed" | cut -f 1 | sort))
[ -z "$absent" ] || printf 'no TASTy names %s: left out\n' $absent

echo "$out: $(printf '%s\n' "$placed" | grep -c .) sources placed," \
  "$(printf '%s' "$absent" | grep -c . || true) left out," \
  "$(wc -l < "$out/classpath.txt") dependency jars"
