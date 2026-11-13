package cardinality.bench.algebra

import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.poly.PolynomialMethods
import cc.redberry.rings.poly.univar.UnivariatePolynomial

/** Rings mutates polynomials: clone inputs so the adapter has value semantics. The carrier is
  * already expanded, so comparison and projection read it directly. An exponent must be a small
  * nonnegative constant: the carrier has Int degree, so a huge or polynomial exponent refuses
  * rather than overflowing.
  */
final class RingsAlgebra extends Algebra {
  type Value = UnivariatePolynomial[BigInteger]
  def number(n: Int): Value = UnivariatePolynomial.create(n.toLong)
  def omega: Value = UnivariatePolynomial.create(0L, 1L)
  def add(a: Value, b: Value): Value = a.clone().add(b)
  def multiply(a: Value, b: Value): Value = a.clone().multiply(b)

  def power(a: Value, exponent: Value): Value = {
    if (exponent.degree() > 0)
      throw new IllegalArgumentException(s"exponent is not a constant: $exponent")
    val e = exponent.get(0)
    if (e.signum() < 0 || e.bitLength() > 31)
      throw new IllegalArgumentException(s"exponent is outside the carrier's Int range: $e")
    PolynomialMethods.polyPow(a, e.intValue(), true)
  }

  private def coeff(p: Value, k: Int): BigInt =
    if (k >= 0 && k <= p.degree()) BigInt(p.get(k).toString) else BigInt(0)

  def compare(a: Value, b: Value): Int = {
    val byDegree = Integer.compare(a.degree(), b.degree())
    if (byDegree != 0) byDegree
    else
      (math.max(a.degree(), b.degree()) to 0 by -1).iterator
        .map(k => coeff(a, k).compare(coeff(b, k)))
        .find(_ != 0)
        .getOrElse(0)
  }

  def project(a: Value): String = {
    val constant = coeff(a, 0)
    val linear = coeff(a, 1)
    val higher = (2 to a.degree()).foldLeft(BigInt(0))((s, k) => s + coeff(a, k))
    s"$higher P + $linear w + $constant"
  }

}
