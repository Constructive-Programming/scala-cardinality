# Repeated arguments in implementation counts

`Type.Repeated(T)` in a method or constructor parameter is modeled as a finite,
possibly empty sequence of `T`, not as one supplied `T`. Parameter clauses keep
their existing ordering and binder identities. This model belongs to pure,
total canonical implementation counting; it is not a stored-value estimate for
Scala's `Seq` representation.

## Supported fragment

- A sequence whose element shape is structurally empty has just the empty
  sequence. The analyzer normalizes it to `Unit`. Structural emptiness includes
  `Nothing`, sums of empty alternatives and products with an empty field.
- A structurally singleton result (`Unit`, products of singleton results,
  one-alternative singleton sums, or functions returning singleton results)
  has one implementation even when repeated arguments are supplied. Sequence
  elimination cannot distinguish results in this case.
- Sequence introduction in the shape calculus has the canonical constructors
  `Nil` and `Cons`. Without a way to produce an element, only `Nil` is available.
  With a productive element construction, the existing productive-cycle proof
  gives `ω`: sequences of length 0, 1, 2, and so on are distinguishable. An
  unseeded endomorphism does not prove this cycle productive.

For example:

| Signature or shape goal | Count | Reason |
|---|---|---|
| `def ignore[A](xs: A*): Unit` | `1` | Only the Unit result is observable |
| `def pick[A](x: A)(xs: Nothing*): A` | `1` | `xs` is necessarily empty |
| `case class Empty(xs: Nothing*)` constructor | `1` | The only field sequence is empty |
| Sequence-of-`A` shape goal with no `A` producer | `1` | Only `Nil` |
| Sequence-of-`A` shape goal with `x: A` | `ω` | Productive `Cons` cycle, distinct lengths |
| `def use[A](x: A)(fs: (A => A)*): A` | `?` | Arbitrary sequence elimination is outside the fragment |
| `case class Many[A](xs: A*)` constructor | `?` | Arbitrary input-sequence transformations are not enumerated |

## Deliberate limits

The solver does not enumerate total length tests, safe element selection,
mapping, folds or recursive sequence elimination. If a supplied sequence (or a
callable capability involving one) can be present in a non-singleton goal, it
reports:

> Repeated-argument sequence elimination is unsupported (length tests, selection and folds)

This is conservative: even when a stronger parametricity proof might show a
particular sequence irrelevant, this fragment does not assert that proof.
Arrow-introduced sequence parameters are checked too. In particular, repeated
functions are not treated as one reusable callable; their sequence can be empty.
Partial operations such as unchecked `head` cannot justify a total inhabitant.

Element types still use the ordinary resolver and environment checks. Unsupported
elements remain diagnostics, including in a Unit-returning signature. General
`Seq[T]` names, aliases and collection methods are not newly resolved by this
feature. Sequence-introduction examples above are shape-calculus tests, not a
claim of general Scala collection support.
