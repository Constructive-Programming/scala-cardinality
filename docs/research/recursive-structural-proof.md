# The structural proof for recursive types (open lane)

*Status: designed, not implemented. The current behavior is pinned by
`RecursionProbeSpec`; the refusal "recursive type requires a structural proof: X" is
honest while this lane is open (93 eo rows carry it, 133 in the kyo-deps universe —
the single largest non-dependency blocker for the corpus's `core` module).*

## What the model must count

A self-referential (or mutually recursive) trait `R` — `trait R { def self: R; def k: Int }`
— read through its abstract type. The ground truth (probed, and pinned by the spec):

- A **concrete instance** reads like any record: `R0.k` counts `2^32` (the constants).
- A row read through the **abstract recursive type** refuses: the record shape would have
  to count the atom stream the recursive member generates.

## The three tiers, and why they must land together or not at all

**Tier 1 — member rows of the recursive trait** (`R.k`). The model counts
recursion-free implementations, so a body of `k` may not call `k` — every atom
reachable only through the recursive member (`r.selfⁿ.k`) is dead. The count is the
plain reading over the first-order members: `2^32`. Sound in isolation *for member
rows*.

**Tier 2 — function rows with a recursive parameter** (`f(r: R): Int`). A body of `f`
may legally call `r.self.k` — a *second*, independent `Int` atom. Counting over the
finite first-order atom set would undercount: `f(r) = r.self.k + 1` distinguishes
receivers no first-order-atom count separates. The honest reading: the recursive
member contributes a **countable atom stream** (`r.k, r.self.k, r.self.self.k, …`),
and the goal's count is `|goal|^ℵ₀` — the **beyond-countable (ε₀)** tier the solver
already carries (`Count.Bounded` with `epsilon > 0`). This is why tier 1's marker
cannot land alone: the same record shape feeds both rows, and the finite reading is a
wrong number for tier 2.

**Tier 3 — a recursive type at a goal position** (`f(b: Boolean): R`). The
least-fixed-point rule the counting contract already states
([counting-type-inhabitants.md](counting-type-inhabitants.md), "A recursive type is a
least fixed point"): `μx. 1 + x ~ ℵ₀` — `Count.Countable` — *only when a starting
inhabitant exists*; `μx. 0 = 0`. The seed is a parameter (or supplied value) of the
recursive type itself: with `f(r: R, b: Boolean): R` the constructions are `r`,
`r.self`, `r.self.self`, … — countably many; with `f(b: Boolean): R` and no seed the
type is uninhabited and the row is `0`.

## What the implementation needs

1. The record/type reading's cycle guard (`MethodProjections.applyAlias`,
   `OpaqueTypes`, `BoundChecks`) produces a **recursive-atom marker** in the shape
   instead of a refusal: the recursive occurrence binds to the record's own identity.
2. The capability machinery tiers by position: member rows drop the atoms reachable
   only through the measured declaration (tier 1); parameter positions count the
   stream into the ε₀ tier (tier 2); goal positions resolve the seed rule (tier 3).
3. Adversarial fixtures per tier: the stream-walking body (`r.self.k + 1`) for tier
   2's ε₀; the seed/no-seed pair for tier 3; the mutual-recursion pair; the
   concrete-instance control; a base-case instance (`object R0 extends R`) making the
   LFP inhabited for the goal tier.

## Precedent in the contract

- The μ rule itself: docs/counting-type-inhabitants.md ("A recursive type is a least
  fixed point … only when a starting inhabitant exists").
- The opaque-callable analogy for the stream: "An opaque callable is unconstrained, so
  applying it repeatedly is counted as countable (`∀a. (a -> a) -> a -> a ~ ℵ₀`)".
- The absurd elimination the solver already implements (a goal with no productive rule
  is `0`, not unknown) is tier 3's no-seed case.
