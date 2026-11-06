#!/usr/bin/env python3
"""Total program cardinality before and after a pass, as one polynomial a·ε₀ + b·ω + n.

Usage: report-diff.py <before report.txt> <after report.txt> [--rows]

Every row of both sections (implementations and stored values) is summed. `?` rows have no
size and are counted beside the polynomial; `—` rows have none of their own. A bound `≤2^k`
counts as 2^k. The finite part is exact while it fits in 64 bits, otherwise printed as a power
of two. `--rows` also lists every row whose size changed, appeared (`·`) or left.
"""
import math
import re
import sys
from collections import Counter


def rows(path):
    """(section:name, size) per row; a repeated name is numbered in report order."""
    section, seen, out = None, Counter(), []
    for line in open(path, encoding="utf-8"):
        if line.startswith("Stored-value estimates"):
            section = "stored"
        elif line.startswith("Generic method"):
            section = "signature"
        elif section and line[:1] not in ("", " ", "\n"):
            cols = re.split(r"  +", line.rstrip("\n"))
            if len(cols) >= 3:
                key = f"{section}:{cols[1]}"
                seen[key] += 1
                out.append((key + (f" #{seen[key]}" if seen[key] > 1 else ""), cols[0]))
    return out


def log2_finite(term):
    """log2 of a finite term: `n`, `a^b`, `a^(b^c)`, a product of those, `≤` ignored."""
    total = 0.0
    for factor in term.replace("≤", "").replace("(lossy)", "").strip().split("·"):
        nums = [int(x) for x in re.findall(r"\d+", factor)]
        if len(nums) == 1:
            total += math.log2(nums[0]) if nums[0] > 0 else -math.inf
        elif len(nums) == 2:
            total += nums[1] * math.log2(nums[0])
        elif len(nums) == 3:
            total += nums[1] ** nums[2] * math.log2(nums[0])
        else:
            raise ValueError(f"unread size: {term}")
    return total


def polynomial(path):
    eps, omega, logs, unresolved = 0, 0, [], 0
    for _, size in rows(path):
        if size == "?":
            unresolved += 1
            continue
        if size == "—":
            continue
        for term in size.split(" + "):
            m = re.fullmatch(r"(\d*)(ε₀|ω)", term.strip())
            if m:
                k = int(m.group(1) or 1)
                if m.group(2) == "ε₀":
                    eps += k
                else:
                    omega += k
            else:
                logs.append(log2_finite(term))
    return eps, omega, logs, unresolved


def render(eps, omega, logs, unresolved):
    finite = [l for l in logs if l > -math.inf]
    if not finite:
        n = "0"
    else:
        top = max(finite)
        e = top + math.log2(sum(2 ** (l - top) for l in finite))
        if e < 64:
            n = str(round(sum(2**l for l in finite)))
        elif e < 1e6:
            n = f"≈2^{e:.1f}"
        else:
            n = f"≈2^(2^{math.log2(e):.2f})"
    parts = ([f"{eps}ε₀"] if eps else []) + ([f"{omega}ω"] if omega else []) + [n]
    return " + ".join(parts) + f"   ({unresolved} rows ?)"


def main(argv):
    args = [a for a in argv if a != "--rows"]
    if len(args) != 2:
        sys.exit(__doc__)
    before, after = args
    print("before:", render(*polynomial(before)))
    print("after: ", render(*polynomial(after)))
    if "--rows" in argv:
        old, new = dict(rows(before)), dict(rows(after))
        for key in sorted(old.keys() | new.keys()):
            a, b = old.get(key, "·"), new.get(key, "·")
            if a != b:
                section, name = key.split(":", 1)
                print(f"  {section:9} {a:>16} → {b:<16} {name}")


if __name__ == "__main__":
    main(sys.argv[1:])
