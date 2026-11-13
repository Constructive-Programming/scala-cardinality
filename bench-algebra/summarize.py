"""Summarize saved JMH data and adapter LOC without third-party Python dependencies."""
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RESULTS = ROOT / "bench-algebra/results"
records = []
for name in ("jmh.json",):
    path = RESULTS / name
    if path.exists():
        for item in json.loads(path.read_text()):
            item["source"] = name
            records.append(item)

lines = ["# Measured benchmark results", "",
         "Times are microseconds/op; allocation is bytes/op. ± is JMH's reported",
         "99.9% confidence interval half-width, not standard deviation.", "",
         "Current Size is a coarse baseline and does not produce exact polynomials.", "",
         "| Run | Operation | Backend | n | µs/op | ± | B/op |",
         "| --- | --- | --- | ---: | ---: | ---: | ---: |"]
for item in records:
    params = item["params"]
    primary = item["primaryMetric"]
    alloc = item.get("secondaryMetrics", {}).get("gc.alloc.rate.norm", {}).get("score", 0)
    operation = params.get("operation") or params.get("pair") or params.get("expr") or "finitePower"
    lines.append(f'| {item["source"]} | {operation} | {params["backend"]} | {params.get("n", "-")} | '
                 f'{primary["score"]:.4f} | {primary["scoreError"]:.4f} | {alloc:.1f} |')

lines += ["", "## Physical source size", "",
          "Code lines exclude blank and comment-only lines, but include imports and braces.",
          "No attempt is made to treat a whole upstream library as equivalent adapter code.", "",
          "| Source | Physical lines | Code lines |", "| --- | ---: | ---: |"]
sources = list((ROOT / "bench-algebra/src/main/scala/cardinality/bench/algebra").glob("*.scala"))
sources += [ROOT / "core/src/main/scala/cardinality/capacity/Size.scala",
            ROOT / "core/src/main/scala/cardinality/analysis/inhabitation/Natural.scala"]
for path in sorted(sources):
    content = path.read_text().splitlines()
    count = sum(bool(s.strip()) and not s.lstrip().startswith(("//", "/*", "*")) for s in content)
    lines.append(f"| {path.relative_to(ROOT)} | {len(content)} | {count} |")

(RESULTS / "summary.md").write_text("\n".join(lines) + "\n")
print(f"Summarized {len(records)} JMH results in {RESULTS / 'summary.md'}")
