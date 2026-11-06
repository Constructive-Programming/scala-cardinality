#!/usr/bin/env bash
# Cardinality before and after a pass: the summary lines, the stored-value tiers and every row
# whose size changed, appeared or left. Usage: report-diff.sh <before/report.txt> <after/report.txt>
set -euo pipefail
[ $# -eq 2 ] || { echo "usage: $0 <before report.txt> <after report.txt>" >&2; exit 2; }

summary() { grep -m1 -E '^  [0-9]+ signatures:' "$1" | sed 's/^ *//'; }

# One line per row: section, name, size. Rows start in column 1; columns are 2+ spaces apart.
# A name that repeats (a class and its companion, overloads) is numbered in report order.
rows() {
  awk -F'  +' '
    /^Stored-value estimates/ { section = "stored" }
    /^Generic method/         { section = "signature" }
    /^[^ ]/ && NF >= 3 && section != "" {
      key = section "\t" $2; n = ++seen[key]
      print section "\t" $2 (n > 1 ? " #" n : "") "\t" $1
    }
  ' "$1"
}

tiers() {
  rows "$1" | awk -F'\t' '$1 == "stored" {
    s = $3
    t = s ~ /ε₀/ ? "ε₀" : s ~ /ω/ ? "ω" : s == "?" ? "?" : s == "—" ? "—" : "finite"
    n[t]++
  } END { printf "ε₀ %d · ω %d · finite %d · ? %d", n["ε₀"], n["ω"], n["finite"], n["?"] }'
}

echo "signatures   before: $(summary "$1")"
echo "             after:  $(summary "$2")"
echo "stored-value before: $(tiers "$1")"
echo "             after:  $(tiers "$2")"
echo
echo "changed rows (section, before → after, name):"
join -t$'\t' -a1 -a2 -e '·' -o 0,1.2,2.2 \
  <(rows "$1" | awk -F'\t' '{ print $1 ":" $2 "\t" $3 }' | sort -t$'\t' -k1,1) \
  <(rows "$2" | awk -F'\t' '{ print $1 ":" $2 "\t" $3 }' | sort -t$'\t' -k1,1) |
  awk -F'\t' '$2 != $3 {
    split($1, k, ":"); name = substr($1, length(k[1]) + 2)
    printf "  %-9s %16s → %-16s %s\n", k[1], $2, $3, name
  }'
