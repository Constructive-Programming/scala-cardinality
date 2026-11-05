# Method unions and explicit nullability

Method and constructor implementation analysis retains the [v1 counting
contract](../plans/v1.md#_2-agreed-counting-contract): pure, total, parametric,
null-free implementations. It does not count stored values or certify an
existing body.

Scala's `A | B` is untagged and alternatives may overlap. It is not `Either[A, B]`
in general. The normalization:

- Repeated syntactically identical alternatives collapse: `A | A` is `A`.
- `Nothing` alternatives are empty.
- `Null` (including `scala.Null`) is empty **under the null-free method model**.
  Thus `A | Null` is `A`, not `Option[A]`; a result `Null` has zero implementations,
  and an input `Null` is an impossible context with a unique absurd eliminator.
- Aliases resolving to the same free type binder can collapse too.
- `T | Any` is `Any`, one class spelled twice is one alternative, and a class
  absorbs an alternative known to derive from it.
- Remaining distinct alternatives are a tagged sum only when each has a runtime
  class, no class carries two of them, and the classes are pairwise provably
  disjoint: then a class-test `match` is the sum's eliminator. Equal structural
  shapes alone never suffice. Everything else stays unresolved with the failed
  condition as its reason — see [top types and unions](top-types-unions.md).
- Unsupported branches remain unresolved even when another branch is supported.
  Normalization does not make missing source declarations or concrete primitive
  capabilities known.

Source declarations shadow builtin names as usual. This rule does not assume
that a user-defined type called `Null` is empty.

This is intentionally not a nullable stored-value estimate: stored values can
include null and their union sizes require their own overlap reasoning. In
particular, this change does not claim that nullable data has the same number of
runtime values as non-nullable data. A constructor position is read through the
null-free model before the stored-value estimate is asked, so the two readings
meet on the same type.
