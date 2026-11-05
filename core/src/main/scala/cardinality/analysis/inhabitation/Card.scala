package cardinality.analysis.inhabitation

/** A count the solver carries: an exact natural, or the countable tier of the finite-program
  * reading (register D2), where a power or product with an infinite operand saturates at 0, 1 or ω.
  */
enum Card {
  case Exact(value: Natural)
  case Omega

  def isZero: Boolean = this == Card.zero

  def isOne: Boolean = this == Card.one

  def +(other: Card): Card = (this, other) match {
    case (Exact(a), Exact(b)) => Exact(a + b)
    case _                    => Omega
  }

  def *(other: Card): Card =
    if (isZero || other.isZero) Card.zero
    else
      (this, other) match {
        case (Exact(a), Exact(b)) => Exact(a * b)
        case _                    => Omega
      }

  def ^(exponent: Card): Card =
    if (exponent.isZero) Card.one
    else if (isZero || isOne) this
    else
      (this, exponent) match {
        case (Exact(a), Exact(b)) => Exact(a ^ b)
        case _                    => Omega
      }

}

object Card {

  val zero: Card = Exact(Natural.Zero)
  val one: Card = Exact(Natural.One)

  def exact(value: BigInt): Card = Exact(Natural(value))

}
