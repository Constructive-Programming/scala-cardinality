package cardinality.bench.algebra

/** The four-coordinate carrier issue #39 plans: `a·ε₀ + b·ωⁿ + c·ω + d` with exact coefficients,
  * hand-rolled with no library. `ωⁿ` is one tier for every finite power of ω above one — the
  * deliberate collapse the counting contract chose — so `w^(2^32)` and `w^2` compare equal here
  * while exact polynomials order them apart. ε₀ is the cap infinite exponents reach, and it does
  * not absorb: `ε₀ + 1` stays above `ε₀`.
  *
  * Multiplication is bilinear over the tier table (ω·ω = P, ω·P = P·P = P, anything with ε₀ is ε₀),
  * so coefficients multiply and every contribution lands in its output tier: `2ω·3ω = 6P` and
  * `(P + ω)·ω = 2P`. Finite powers use repeated squaring on the four coordinates, which is why a
  * huge finite exponent costs logarithmic work and never expands. Coefficients materialize as
  * BigInt; the analyzer's carrier will keep them symbolic through `Natural`.
  */
final case class Model(epsilon: BigInt, power: BigInt, omega: BigInt, finite: BigInt) {
  def isZero: Boolean = epsilon == 0 && power == 0 && omega == 0 && finite == 0
  def isOne: Boolean = epsilon == 0 && power == 0 && omega == 0 && finite == 1
}

final class ModelAlgebra extends Algebra {
  type Value = Model

  def number(n: Int): Value = Model(0, 0, 0, BigInt(n))
  def omega: Value = Model(0, 0, 1, 0)

  def add(a: Value, b: Value): Value =
    Model(a.epsilon + b.epsilon, a.power + b.power, a.omega + b.omega, a.finite + b.finite)

  def multiply(a: Value, b: Value): Value = {
    // Bilinear over the tier table: a pair of tiers (i, j) lands in ε₀ when either is ε₀,
    // in P when both are ω, else in the higher tier. The sixteen coordinate pairs below
    // enumerate that table; check: 1 + 2 + 6 + 7 = 16.
    val finitePart = a.finite * b.finite
    val omegaPart = a.finite * b.omega + a.omega * b.finite
    val powerPart =
      a.finite * b.power + a.power * b.finite + a.omega * b.omega +
        a.omega * b.power + a.power * b.omega + a.power * b.power
    val epsilonPart =
      a.finite * b.epsilon + a.epsilon * b.finite + a.omega * b.epsilon + a.epsilon * b.omega +
        a.power * b.epsilon + a.epsilon * b.power + a.epsilon * b.epsilon
    Model(epsilonPart, powerPart, omegaPart, finitePart)
  }

  def power(base: Value, exponent: Value): Value =
    if (exponent.isZero) Model(0, 0, 0, 1) // b^0 = 1, including 0^0
    else if (exponent.epsilon > 0 || exponent.power > 0 || exponent.omega > 0)
      if (base.epsilon > 0 || base.power > 0 || base.omega > 0)
        Model(1, 0, 0, 0) // infinite^infinite = ε₀
      else if (base.finite == 1) Model(0, 0, 0, 1)
      else if (base.finite == 0) Model(0, 0, 0, 0)
      else Model(0, 0, 1, 0) // finite base, infinite exponent: the finite-program convention
    else if (exponent.finite == 1) base
    else if (base.isZero) Model(0, 0, 0, 0)
    else if (base.isOne) Model(0, 0, 0, 1)
    else if (base.epsilon > 0) Model(1, 0, 0, 0)
    else intPower(base, exponent.finite)

  private def intPower(base: Value, exponent: BigInt): Value = {
    var acc: Value = Model(0, 0, 0, 1)
    var square = base
    var remaining = exponent
    while (remaining > 0) {
      if (remaining.testBit(0)) acc = multiply(acc, square)
      remaining >>= 1
      if (remaining > 0) square = multiply(square, square)
    }
    acc
  }

  def compare(a: Value, b: Value): Int = {
    val byEpsilon = a.epsilon.compare(b.epsilon)
    if (byEpsilon != 0) byEpsilon
    else {
      val byPower = a.power.compare(b.power)
      if (byPower != 0) byPower
      else {
        val byOmega = a.omega.compare(b.omega)
        if (byOmega != 0) byOmega else a.finite.compare(b.finite)
      }
    }
  }

  def project(a: Value): String =
    if (a.epsilon == 0) s"${a.power} P + ${a.omega} w + ${a.finite}"
    else {
      val head = if (a.epsilon == 1) "ε₀" else s"${a.epsilon}ε₀"
      if (a.power == 0 && a.omega == 0 && a.finite == 0) head
      else s"$head + ${a.power} P + ${a.omega} w + ${a.finite}"
    }

  override def render(a: Value): String =
    s"Model(${a.epsilon}, ${a.power}, ${a.omega}, ${a.finite})"

}
