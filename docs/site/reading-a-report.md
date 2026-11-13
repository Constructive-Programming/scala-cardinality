# Reading a report

A report answers two different questions. Keep them apart; the same type has a number in
each.

- **Stored-value estimate** — how many values an instance of a definition can hold. A
  `case class Pixel(shade: Byte, on: Boolean)` holds $2^9$ values.
- **Implementation cardinality** — how many pure, total implementations a method or
  constructor signature admits with everything in scope. `def choose[A](x: A, y: A): A`
  has 2.

`Counter.source` sums the value spaces of the definitions in a source;
`Counter.sourceSignature` sums what their members declare. A module with two
`String => String` methods and one `String` field is $2\varepsilon_0 + \omega$ by the
second reading and one value by the first.

## A row

```
?  example.Holder[A]  class     Light.scala:11  unresolved: A
ω  example.Succ       class     Light.scala:17
2  example.Custom     class     Light.scala:5
—  example.Light      abstract  Light.scala:3
```

The columns are the number, the definition, its kind, its source position, and the reason
when the number is not exact.

| Mark | Meaning |
| --- | --- |
| `0` | No total value: `Nothing`, or a recursive type with no base case. |
| `1` | Exactly one value: `Unit`, a singleton, a pure lazy self-wrapper. |
| `n` | A finite count, exact up to 127. Above that, a bit capacity: $\le 2^{32}$. |
| `ω` | Countably infinite: `String`, a productive recursive type, a function over an infinite domain. |
| `ε₀` | The tier above $\omega$: a function space with infinite domain and codomain, e.g. `String => String`. |
| `?` | The calculator could not bound it. The row still carries the size it reached and names the type that stopped it. `?` is not a proof of infinity. |
| `—` | No cardinality of its own, such as an abstract type. |

Sizes are polynomials $a\cdot\varepsilon_0 + b\cdot\omega + n$, compared lexicographically
by tier. Addition is componentwise, so two countable alternatives stay $2\omega$
(`Either[String, String]`) instead of collapsing into one $\omega$.

Products and powers stay coarse above the finite tier: $2 \cdot \omega = \omega$ and
$\omega \cdot \omega = \omega$. So `(String, String)` prints `ω` even though its cardinal
is $\omega^2$. The coefficient counts additive contributions, not repeated products; that
is why `Either[String, String]` is $2\omega$ and the isomorphic `(String, String)` is
`ω`.

## Shrinking a number

The report is only useful if the next change lowers it. Four moves cover most code:

1. **Replace `String` with an `enum`.** A free-text field admits $\omega$ values; the cases the
   domain actually has are a handful. Validation at the boundary becomes the type.
2. **Replace boolean flags with a sum.** Three independent `Boolean`s hold 8 states, most
   illegal. One `enum` with the legal combinations holds exactly those.
3. **Replace a sentinel with `Option` or a case.** `-1`, `""` and `null` each add a value
   the type cannot distinguish from a real one. `Option[A]` names the absence.
4. **Replace a record with a capability.** A function that needs `CanModify[T, String]`
   does not need to know `T`'s fields. Fewer fields in scope means fewer states to
   consider, and the contract is the signature.

Apply one move, re-run `cardinalityReport`, and keep the diff. A count that falls is a
reviewable improvement; a count that rises is a question for the author.

## In CI

`target/cardinality/report.txt` is a plain text file. Keep it as a build artifact, or
commit a baseline and diff it on each change. A PR that widens a type shows the new number
next to the old one; a PR that narrows one shows the improvement. The
[bibliography](references/bibliography.md) page links the baselines this repository keeps.
