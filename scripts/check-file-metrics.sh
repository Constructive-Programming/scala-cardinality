#!/usr/bin/env bash
#
# File length and comment-ratio gate for the Scala sources.
#
# Two per-file rules, both measured over every `*.scala` file under `core/src/` and
# `plugin/src/main/` (the scripted fixtures under `plugin/src/sbt-test` are deliberately
# repetitive and out of scope):
#
#   - line count:     warn above WARN_LINES, fail above MAX_LINES
#   - comment share:  warn above WARN_PCT, fail above MAX_PCT (percent of
#                     total lines that contain any comment text)
#
# The warning tier is advisory: it prints an annotation and the script keeps
# going. Only the error tier sets the exit status, so a PR can land a file in
# warning territory but not in error territory. CodeScene reaches for the same
# signals fuzzily (its `clean_code_collective` profile flags long files and
# comment-heavy hotspots by its own internal thresholds); this script pins
# concrete numbers, mirroring how the coverage floor in `build.sbt` pins what
# scoverage only measures.
#
# A comment line is one that contains a comment outside string and char
# literals — full-line, block-comment continuation, or trailing. The scanner
# tracks `/* */`, `"..."` (with escapes), `"""..."""` raw strings, and char
# literals across lines, so a URL or `//` inside a literal does not register.
# The denominator is every line in the file, blank lines included, and the
# ratio is compared to the thresholds at whole-percent rounding — simple to
# state, and it prices blank lines the same as code, which is fine for a
# ratio gate.
#
# On GitHub Actions the findings are emitted as `::warning::` / `::error::`
# annotations (shown inline on the diff); locally as plain lines. Exit 1 if
# any file breaches an error threshold.
set -uo pipefail

WARN_LINES=800
MAX_LINES=1000
WARN_PCT=25
MAX_PCT=35

github_notice() { # level file message
  if [ "${GITHUB_ACTIONS:-}" = true ]; then
    printf '::%s file=%s::%s\n' "$1" "$2" "$3"
  else
    printf '%-7s %s: %s\n' "$1:" "$2" "$3"
  fi
}

# One pass per file; awk prints "total<TAB>comments", computed by a small
# character scanner that carries block-comment and raw-string state across
# lines. (POSIX awk has no regex lookahead or stateful lexer, hence the loop.)
# The scanner compares against a quote character; `sq` is passed in as an awk
# variable because a literal apostrophe cannot appear inside the single-quoted
# program text without breaking the shell quoting around it.
count_comments() {
  awk -v sq="'" '
    BEGIN { inblock = 0; inraw = 0; comments = 0 }
    {
      total++
      line = $0
      n = length(line)
      pos = 1
      comment = inblock          # raw-string continuations are not comments
      while (pos <= n) {
        if (inraw) {
          if (substr(line, pos, 3) == "\"\"\"") { inraw = 0; pos += 3 } else pos++
          continue
        }
        if (inblock) {
          if (substr(line, pos, 2) == "*/") { inblock = 0; pos += 2 } else pos++
          continue
        }
        if (substr(line, pos, 2) == "//") { comment = 1; break }
        if (substr(line, pos, 2) == "/*") { comment = 1; inblock = 1; pos += 2; continue }
        if (substr(line, pos, 3) == "\"\"\"") { inraw = 1; pos += 3; continue }
        c = substr(line, pos, 1)
        if (c == "\"") {                       # ordinary string: skip to close
          pos++
          while (pos <= n) {
            cc = substr(line, pos, 1)
            if (cc == "\\") { pos += 2; continue }
            if (cc == "\"") { pos++; break }
            pos++
          }
          continue
        }
        if (c == sq) {                         # char literal: skip to close
          pos++
          while (pos <= n) {
            cc = substr(line, pos, 1)
            if (cc == "\\") { pos += 2; continue }
            if (cc == sq) { pos++; break }
            pos++
          }
          continue
        }
        pos++
      }
      if (comment) comments++
    }
    END { printf "%d\t%d\n", total, comments }
  ' "$1"
}

errors=0
while IFS= read -r file; do
  IFS=$'\t' read -r total comments < <(count_comments "$file")
  [ "$total" -eq 0 ] && continue
  pct=$(awk -v c="$comments" -v t="$total" 'BEGIN { printf "%.0f", 100 * c / t }')

  # One annotation per rule, at the highest tier it reaches.
  if [ "$total" -gt "$MAX_LINES" ]; then
    github_notice error "$file" "$total lines, over the $MAX_LINES-line limit"
    errors=$((errors + 1))
  elif [ "$total" -gt "$WARN_LINES" ]; then
    github_notice warning "$file" "$total lines, over the $WARN_LINES-line warning threshold"
  fi
  if [ "$pct" -gt "$MAX_PCT" ]; then
    github_notice error "$file" "$pct% comment lines, over the $MAX_PCT% limit"
    errors=$((errors + 1))
  elif [ "$pct" -gt "$WARN_PCT" ]; then
    github_notice warning "$file" "$pct% comment lines, over the $WARN_PCT% warning threshold"
  fi

  printf '%6s lines  %4s%% comments  %s\n' "$total" "$pct" "$file"
done < <(find core/src plugin/src/main -name '*.scala' | sort)

if [ "$errors" -gt 0 ]; then
  echo "$errors file metric(s) over the hard limit."
  exit 1
fi
