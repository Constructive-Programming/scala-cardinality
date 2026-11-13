package cardinality.bench.algebra

import cardinality.capacity.*

/** Existing coarse Size semantics: performance baseline, not an exact polynomial contender. Its
  * multiplication collapses `2·ω` to `ω`, so `w^3` and `2·w^2` both become `ω` — the degree and
  * coefficient information the ωⁿ tier exists to carry — but its power rules already encode the
  * model's exponent conventions, so `w^w` reaches ε₀.
  */
final class CurrentAlgebra extends Algebra {
  type Value = Size
  def number(n: Int): Size = Size(0, 0, FinitePart.exact(BigInt(n)))
  def omega: Size = EffectiveOmega
  def add(a: Size, b: Size): Size = a + b
  def multiply(a: Size, b: Size): Size = a * b
  def power(a: Size, exponent: Size): Size = a.pow(exponent)
  override def compare(a: Size, b: Size): Int = Size.order.compare(a, b)
  override def project(a: Size): String = a.render
  override def render(a: Size): String = a.render
}
