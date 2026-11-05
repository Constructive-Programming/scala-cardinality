package cardinality.analysis.inhabitation

/** A count the solver carries: an exact natural, or the countable tier of the finite-program
  * reading (register D2), where a power or product with an infinite operand saturates at 0, 1 or ω
  * — or the ε₀ tier a top type has in a positive position (v1.md §2), which absorbs ω the way the
  * size algebra's ε₀ does (`2^ε₀ = ε₀`).
  */
enum Card {
  case Exact(value: Natural)
  case Omega
  case Epsilon0

  def isZero: Boolean = this == Card.zero

  def isOne: Boolean = this == Card.one

  def +(other: Card): Card = (this, other) match {
    case (Exact(a), Exact(b)) => Exact(a + b)
    case _                    => infinite(other)
  }

  // The tier of an infinite result with this and `other` as operands.
  private def infinite(other: Card): Card =
    if (this == Epsilon0 || other == Epsilon0) Epsilon0 else Omega

  def *(other: Card): Card =
    if (isZero || other.isZero) Card.zero
    else
      (this, other) match {
        case (Exact(a), Exact(b)) => Exact(a * b)
        case _                    => infinite(other)
      }

  def ^(exponent: Card): Card =
    if (exponent.isZero) Card.one
    else if (isZero || isOne) this
    else
      (this, exponent) match {
        case (Exact(a), Exact(b)) => Exact(a ^ b)
        case _                    => infinite(exponent)
      }

}

object Card {

  val zero: Card = Exact(Natural.Zero)
  val one: Card = Exact(Natural.One)

  def exact(value: BigInt): Card = Exact(Natural(value))

}
