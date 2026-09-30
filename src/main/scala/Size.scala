/** How many values a type can hold: a natural-sum polynomial over three tiers.
  *
  * A size is `a·ε₀ + b·ω + n`. The coefficients `a` and `b` are non-negative counts of additive
  * contributions at the ε₀ and ω (countable) tiers, and `n` is the finite component: exact up to
  * 127, then a rounded bit capacity. Addition is componentwise — the Hessenberg natural sum of
  * those terms — so `ω + ω = 2ω` and `ε₀ + 3` keeps both summands. Comparison is lexicographic: the
  * ε₀ coefficient, then ω, then the finite component.
  *
  * Multiplication and exponentiation stay coarse above the finite tier: after the zero and one
  * identities, an infinite operand is projected to its dominant tier, so `2 * ω = ω` and
  * `ω * ω = ω`. Coefficients therefore count additive contributions, not repeated products:
  * `Either[String, String]` is `2ω` while the isomorphic `(String, String)` is `ω`.
  *
  * `EffectiveEpsilon0` is an analysis tier, not the ordinal ε₀. It caps what the finite-program
  * reading in `docs/type-arithmetic.md` places above countable infinity — `String => String`,
  * `LazyList[String]` — covering everything from `ω^ω` up.
  */
final case class Size(epsilon: BigInt, omega: BigInt, finite: FinitePart) { self =>

  /** Lexicographic comparison, strict: a size is never larger than itself. */
  def larger(other: Size): Boolean =
    if (epsilon != other.epsilon) epsilon > other.epsilon
    else if (omega != other.omega) omega > other.omega
    else finite.larger(other.finite)

  /** Componentwise addition: every coefficient survives the sum. */
  def add(other: Size): Size =
    Size(epsilon + other.epsilon, omega + other.omega, finite.add(other.finite))

  /** Product of two value spaces, coarse above the finite tier: `2 * ω = ω` and `ω * ω = ω`. Zero
    * annihilates, and one is an identity that preserves whole polynomials.
    */
  def mul(other: Size): Size =
    if (isZero || other.isZero) NothingSize
    else if (isOne) other
    else if (other.isOne) this
    else if (hasInfinite || other.hasInfinite) dominant.widest(other.dominant)
    else Size(0, 0, finite.mul(other.finite))

  /** `self ^ other`, the size of a function space when `self` is the codomain. Coarse above the
    * finite tier: a finite base over an infinite domain stays countable, an infinite base over an
    * infinite domain is `ε₀`, and an infinite base over a finite exponent keeps its tier.
    */
  def pow(other: Size): Size =
    if (other.isZero) UnitSize
    else if (other.isOne) this
    else if (isOne) UnitSize
    else if (isZero) NothingSize
    else if (hasInfinite) {
      if (other.hasInfinite) EffectiveEpsilon0 else dominant
    } else if (other.hasInfinite) EffectiveOmega
    // A finite base over an exponent too large to materialize is still finite; the calculator
    // reports it as countable.
    else finite.pow(other.finite).fold(EffectiveOmega: Size)(Size(0, 0, _))

  def exp: Size => Size = _.pow(self)

  def +(other: Size): Size = add(other)

  def *(other: Size): Size = mul(other)

  def ^(other: Size): Size = pow(other)

  def min(other: Size): Size = if (larger(other)) other else self

  /** No values at all. */
  def isZero: Boolean = epsilon == 0 && omega == 0 && finite.isZero

  /** Exactly one value. */
  def isOne: Boolean = epsilon == 0 && omega == 0 && finite.isOne

  /** A contribution at the ω or ε₀ tier. */
  def hasInfinite: Boolean = epsilon > 0 || omega > 0

  /** This size as a single unit of its highest tier: `ε₀` when any ε₀ contribution is present, `ω`
    * when the size is infinite, and the size itself when it is finite.
    */
  private def dominant: Size =
    if (epsilon > 0) EffectiveEpsilon0
    else if (omega > 0) EffectiveOmega
    else self

  /** The larger of two sizes, as one of them. */
  private def widest(other: Size): Size = if (larger(other)) self else other

  /** Analysis widening: the tier a value has grown into, as a single unit.
    *
    * The recursion solver applies this to a component whose estimate keeps growing — a recognized
    * productive cycle — so that iteration terminates. It drops coefficients (`2ω` widens to `ω`)
    * and never demotes: an ε₀-containing estimate widens to ε₀, a bounded cycle that has not grown
    * at all stays `0`. This is analysis loss, not arithmetic; ordinary addition never coarsens a
    * sum this way.
    */
  def widen: Size =
    if (epsilon > 0) EffectiveEpsilon0
    else if (omega > 0 || !finite.isZero) EffectiveOmega
    else NothingSize

  /** `TinySize(7)`, `FiniteSize(8)`, `2ε₀ + ω + 3`, … — zero terms and unit coefficients are left
    * out, and the finite component prints as digits when it is an exact count.
    */
  override def toString: String =
    if (epsilon == 0 && omega == 0) finite.toString
    else
      List(
        if (epsilon == 0) "" else if (epsilon == 1) "ε₀" else s"${epsilon}ε₀",
        if (omega == 0) "" else if (omega == 1) "ω" else s"${omega}ω",
        if (finite.isZero) "" else finite.term
      ).filter(_.nonEmpty).mkString(" + ")

}

object Size {

  /** The number of bits a finite cardinality needs: the smallest `b` with `cardinality <= 2^b`.
    * Illegal cardinalities (0) report 0, which keeps the finite coordinate total.
    */
  def bits(cardinality: BigInt): Int = (cardinality - 1).bitLength

  /** A size of whole tiers with nothing finite: `a·ε₀ + b·ω`. */
  def tiers(epsilon: BigInt, omega: BigInt): Size = Size(epsilon, omega, FinitePart.Zero)

}

/** The finite component of a `Size`: the third coordinate of `a·ε₀ + b·ω + n`.
  *
  * Counts up to 127 are exact (`Exact`). Above that the component records only a capacity: the
  * count lies in `(2^(bits-1), 2^bits]`. `Lossy` is the stand-in for floating-point numbers, whose
  * precision the calculator does not model. The capacity-like algebra lives here, once: two exact
  * counts combine exactly (`Exact` specializes where the count itself matters), and any other
  * combination rounds up the way the whole calculator rounds counts, `max(bits) + 1` for a sum
  * included, which is not associative — the finite coordinate is an estimate, not an exact natural
  * number.
  */
sealed trait FinitePart { self =>

  import FinitePart.{One, Zero}

  /** The comparison order the finite cases have always had: exact, then capacity, then lossy. */
  def rank: Int

  /** The capacity in bits; an exact count reports its own binary width. */
  def bits: BigInt

  /** Sum of two finite counts, rounded up the same way the whole calculator rounds counts. */
  def add(other: FinitePart): FinitePart = (self, other) match {
    case (Zero, _) => other
    case (_, Zero) => self
    case _         => FinitePart.Capacity(bits.max(other.bits) + 1)
  }

  /** Product of two finite counts. */
  def mul(other: FinitePart): FinitePart = (self, other) match {
    case (Zero, _) => FinitePart.Zero
    case (_, Zero) => FinitePart.Zero
    case (One, _)  => other
    case (_, One)  => self
    case _         => FinitePart.Capacity(bits + other.bits)
  }

  /** `this ^ other` for finite operands, or None when the exponent is too large to materialize: the
    * true result is still finite, but the calculator reports it as countable.
    */
  def pow(other: FinitePart): Option[FinitePart] = (self, other) match {
    case (_, Zero)                   => Some(FinitePart.One)
    case (_, One)                    => Some(self)
    case (Zero, _)                   => Some(FinitePart.Zero)
    case (One, _)                    => Some(FinitePart.One)
    case (_, FinitePart.Exact(that)) => Some(FinitePart.Capacity(bits * that))
    case _ if other.bits.isValidInt  =>
      Some(FinitePart.Capacity(bits * (BigInt(1) << other.bits.toInt)))
    case _ => None
  }

  /** Strict comparison: by rank first, then by width. */
  def larger(other: FinitePart): Boolean =
    if (rank != other.rank) rank > other.rank else bits > other.bits

  /** No values at all: the exact count zero, and nothing else. */
  def isZero: Boolean = self == Zero

  /** Exactly one value: the exact count one, and nothing else. */
  def isOne: Boolean = self == One

  /** How the component prints after an infinite term: exact counts as digits, capacities as the
    * marker that names them.
    */
  def term: String = self match {
    case FinitePart.Exact(repr) => repr.toString
    case other                  => other.toString
  }

}

object FinitePart {

  /** The one part with no values: the exact count zero. A stable value, so a bare pattern
    * `case Zero` matches it by equality.
    */
  val Zero: FinitePart = exact(0)

  /** The one part with a single value: the exact count one. A stable value, so a bare pattern
    * `case One` matches it by equality.
    */
  val One: FinitePart = exact(1)

  /** An exact count. Every size the arithmetic produces keeps this case from 0 to 127, and it is
    * the only case whose arithmetic works on the count itself: two exact counts combine exactly,
    * and everything else falls back to the capacity-like default.
    */
  final case class Exact(repr: BigInt) extends FinitePart {

    def rank: Int = 0

    def bits: BigInt = Size.bits(repr)

    override def add(other: FinitePart): FinitePart = other match {
      case Exact(that) => FinitePart.exact(repr + that)
      case _           => super.add(other)
    }

    override def mul(other: FinitePart): FinitePart = other match {
      case Exact(that) => FinitePart.exact(repr * that)
      case _           => super.mul(other)
    }

    override def pow(other: FinitePart): Option[FinitePart] = other match {
      case Exact(that) => Some(FinitePart.exact(repr.pow(that.toInt)))
      case _           => super.pow(other)
    }

    override def larger(other: FinitePart): Boolean = other match {
      case Exact(that) => repr > that
      case _           => false
    }

    override def toString: String = s"TinySize($repr)"

  }

  /** A count somewhere in `(2^(bits-1), 2^bits]`: the calculator tracks the width, not the count.
    */
  final case class Capacity(bits: BigInt) extends FinitePart {

    def rank: Int = 1

    override def toString: String = s"FiniteSize($bits)"

  }

  /** A floating-point stand-in: finite, of unmodelled precision, and wider than any exact width.
    */
  final case class Lossy(bits: BigInt) extends FinitePart {

    def rank: Int = 2

    override def toString: String = s"LossyInfiniteSize($bits)"

  }

  /** An exact count when it fits a byte, a rounded capacity above that. */
  def exact(cardinality: BigInt): FinitePart =
    if (cardinality.isValidByte) Exact(cardinality) else Capacity(Size.bits(cardinality))

}

/** A size that is exactly `repr` values, for counts up to 127. */
object TinySize {

  def apply(repr: Byte): Size = Size(0, 0, FinitePart.Exact(BigInt(repr)))

}

/** A size of `2^bits` at most: the count lies in `(2^(bits-1), 2^bits]`. */
object FiniteSize {

  def apply(bits: BigInt): Size = Size(0, 0, FinitePart.Capacity(bits))

}

/** A size of unmodelled precision, at least as wide as `bits`: the floating-point stand-in. */
object LossyInfiniteSize {

  def apply(bits: BigInt): Size = Size(0, 0, FinitePart.Lossy(bits))

}

val NothingSize: Size = TinySize(0)
val UnitSize: Size = TinySize(1)
val BooleanSize: Size = TinySize(2)
val ByteSize: Size = FiniteSize(8)
val ShortSize: Size = FiniteSize(16)
val CharSize: Size = FiniteSize(16)
val IntSize: Size = FiniteSize(32)
val LongSize: Size = FiniteSize(64)
val FloatSize: Size = LossyInfiniteSize(32)
val DoubleSize: Size = LossyInfiniteSize(64)

/** Countable infinity: one unit of the ω tier. */
val EffectiveOmega: Size = Size(0, 1, FinitePart.Zero)

/** The ε₀ tier: everything the finite-program reading places from `ω^ω` up to the least fixed point
  * of `α ↦ ω^α`. A tier marker, not the ordinal itself.
  */
val EffectiveEpsilon0: Size = Size(1, 0, FinitePart.Zero)
