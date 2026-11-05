package cardinality.analysis.inhabitation

import scala.annotation.tailrec

/** An exact natural number, kept symbolic once it outgrows a literal.
  *
  * A count up to `MaxLiteralBits` bits is a `Lit`, always: a value that fits is never a power, a
  * product or a sum, so two equal small counts are equal values. Above that bound the number is a
  * sum of monomials, each an odd-or-unabsorbable literal coefficient times powers of distinct bases
  * (2, other literal roots, or symbolic sums). Every symbolic node is at least 2 — sums and
  * products of positives and powers of bases ≥ 2 never fall below — so zero and one are exactly
  * `Lit(0)` and `Lit(1)`, which is what the solver's 0 / 1 / ≥ 2 saturation needs.
  *
  * The form is canonical for the values the solver builds: literal bases are reduced to their
  * power-of-two part and an odd root, equal bases merge, coefficients absorb into equal literal
  * bases, like terms of a sum merge, and factors and terms are sorted. It does not distribute, and
  * an odd base wider than `RootBits` is taken as its own root, so two equal values written through
  * such bases may differ in form; nothing the counter builds reaches that case.
  *
  * Representation, not magnitude, is bounded: a literal wider than `MaxLiteralBits`, more than
  * `MaxNodes` nodes or a power tower higher than `MaxHeight` is refused with `Exceeded`.
  */
sealed trait Natural

object Natural {

  val MaxLiteralBits = 65536
  val MaxNodes = 256
  val MaxHeight = 8
  private val RootBits = 4096
  private val Primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61)

  final case class Lit(value: BigInt) extends Natural
  final case class Pow(base: Natural, exponent: Natural) extends Natural
  final case class Prod(factors: List[Natural]) extends Natural
  final case class Sum(terms: List[Natural]) extends Natural

  final class Exceeded(limit: String)
      extends RuntimeException(s"Inhabitation arithmetic budget exceeded (symbolic form: $limit)")

  /** Arithmetic whose representation may outgrow the budget, with the breach as its reason. */
  def attempt[A](arithmetic: => A): Either[String, A] =
    try Right(arithmetic)
    catch { case exceeded: Exceeded => Left(exceeded.getMessage) }

  val Zero: Natural = Lit(0)
  val One: Natural = Lit(1)

  def apply(value: BigInt): Natural = {
    require(value >= 0, s"a natural number is not negative: $value")
    checked(literal(value))
  }

  extension (a: Natural) {

    def +(b: Natural): Natural = checked(plus(a, b))

    def *(b: Natural): Natural = checked(times(a, b))

    def ^(b: Natural): Natural = checked(power(a, b))

    /** Decimal digits up to 100 bits, `2^k` for a power of two from 2^8 up, a wider literal as its
      * odd root's power times its power of two; `·` between factors.
      */
    def render: String = a match {
      case Lit(value)          => literalText(value)
      case Pow(base, exponent) => s"${operand(base)}^${operand(exponent)}"
      case Prod(factors)       =>
        factors
          .map {
            case sum: Sum => s"(${sum.render})"
            case factor   => factor.render
          }
          .mkString("·")
      case Sum(terms) => terms.map(_.render).mkString(" + ")
    }

    def nodes: Int = a match {
      case Lit(_)              => 1
      case Pow(base, exponent) => 1 + base.nodes + exponent.nodes
      case Prod(factors)       => 1 + factors.map(_.nodes).sum
      case Sum(terms)          => 1 + terms.map(_.nodes).sum
    }

    /** The depth of the power tower: a literal exponent is one level. */
    def height: Int = a match {
      case Lit(_)              => 0
      case Pow(base, exponent) => base.height.max(exponent.height + 1)
      case Prod(factors)       => factors.map(_.height).max
      case Sum(terms)          => terms.map(_.height).max
    }

  }

  /** Structural total order: literals, powers, products, sums; then by their parts. */
  given order: Ordering[Natural] = new Ordering[Natural] {

    def compare(a: Natural, b: Natural): Int = (a, b) match {
      case (Lit(x), Lit(y))           => x.compare(y)
      case (Pow(b1, e1), Pow(b2, e2)) =>
        val byBase = compare(b1, b2)
        if (byBase != 0) byBase else compare(e1, e2)
      case (Prod(x), Prod(y)) => lexicographic(x, y)
      case (Sum(x), Sum(y))   => lexicographic(x, y)
      case _                  => rank(a).compare(rank(b))
    }

  }

  @tailrec private def lexicographic(x: List[Natural], y: List[Natural]): Int = (x, y) match {
    case (Nil, Nil)           => 0
    case (Nil, _)             => -1
    case (_, Nil)             => 1
    case (h1 :: t1, h2 :: t2) =>
      val byHead = order.compare(h1, h2)
      if (byHead != 0) byHead else lexicographic(t1, t2)
  }

  private def rank(n: Natural): Int = n match {
    case _: Lit  => 0
    case _: Pow  => 1
    case _: Prod => 2
    case _: Sum  => 3
  }

  private def operand(n: Natural): String = {
    val text = n.render
    if (text.forall(_.isDigit)) text else s"($text)"
  }

  // Exact and short: a literal too wide to read in digits keeps them only for a part that has no
  // shorter exact spelling here.
  private def literalText(value: BigInt): String =
    if (value >= 256 && value.bitCount == 1) s"2^${value.bitLength - 1}"
    else if (value.bitLength <= 100) value.toString
    else {
      // The normal form's own root policy, so a rendered literal spells the factors `roots` finds.
      val (twos, odd) = roots(value).partition(_._1 == 2)
      (odd.map((root, multiplicity) =>
        if (multiplicity == 1) root.toString else s"${operand(Lit(root))}^$multiplicity"
      ) ++ twos.map((_, power) => literalText(BigInt(2).pow(power.toInt)))).mkString("·")
    }

  private def checked(n: Natural): Natural =
    if (n.nodes > MaxNodes) throw new Exceeded(s"more than $MaxNodes nodes")
    else if (n.height > MaxHeight) throw new Exceeded(s"power tower higher than $MaxHeight")
    else n

  private def fits(value: BigInt): Boolean = value.bitLength <= MaxLiteralBits

  // A product of powers of distinct bases with one literal coefficient; Lit(c) is (c, Nil).
  final private case class Mono(coefficient: BigInt, powers: List[(Natural, Natural)]) {
    def times(other: Mono): Mono = Mono(coefficient * other.coefficient, powers ++ other.powers)
  }

  private def mono(n: Natural): Mono = n match {
    case Lit(value)          => Mono(value, Nil)
    case Pow(base, exponent) => Mono(1, List(base -> exponent))
    case Prod(factors)       => factors.map(mono).reduce(_.times(_))
    case sum: Sum            => Mono(1, List(sum -> One))
  }

  private def literal(value: BigInt): Natural =
    if (fits(value)) Lit(value) else symbolic(value, Nil)

  // Two literals combine as integers; only a symbolic operand takes the normal-form path.
  private def combine(a: Natural, b: Natural)(exact: (BigInt, BigInt) => BigInt)(
      symbolically: => Natural
  ): Natural = (a, b) match {
    case (Lit(x), Lit(y)) => literal(exact(x, y))
    case _                => symbolically
  }

  private def plus(a: Natural, b: Natural): Natural = combine(a, b)(_ + _) {
    if (a == Zero) b
    else if (b == Zero) a
    else
      merged(terms(a) ++ terms(b)) match {
        case List(single) => single
        case many         => Sum(many.sorted(using order.reverse))
      }
  }

  private def terms(n: Natural): List[Natural] = n match {
    case Sum(parts) => parts
    case other      => List(other)
  }

  // Like terms (equal powers) add their coefficients; a merged coefficient may itself absorb into
  // a power and meet another term, so merge until nothing collides.
  @tailrec private def merged(parts: List[Natural]): List[Natural] = {
    val next = parts
      .map(mono)
      .groupMapReduce(_.powers)(_.coefficient)(_ + _)
      .toList
      .map((powers, coefficient) => build(Mono(coefficient, powers)))
    if (next.size == parts.size) next else merged(next)
  }

  private def times(a: Natural, b: Natural): Natural = combine(a, b)(_ * _) {
    if (a == Zero || b == Zero) Zero
    else if (a == One) b
    else if (b == One) a
    else build(mono(a).times(mono(b)))
  }

  private def power(a: Natural, exponent: Natural): Natural =
    if (exponent == Zero) One
    else if (exponent == One || a == Zero || a == One) a
    else
      (a, exponent) match {
        case (Lit(base), Lit(e)) => literalPower(base, e).fold(raised(mono(a), exponent))(Lit(_))
        case _                   => raised(mono(a), exponent)
      }

  private def raised(m: Mono, exponent: Natural): Natural = {
    val coefficient =
      if (m.coefficient == 1) Nil else List((Lit(m.coefficient): Natural) -> exponent)
    build(Mono(1, coefficient ++ m.powers.map((base, e) => base -> times(e, exponent))))
  }

  // The normal form of one monomial: literal bases reduced to roots and equal bases merged; every
  // literal power that fits a literal folds into the coefficient, which then absorbs into the
  // literal bases left. So a monomial that fits is a literal, whichever way it was built.
  private def build(m: Mono): Natural =
    if (m.coefficient == 0) Zero
    else {
      val (coefficient, powers) =
        merge(m.powers.flatMap(reduce)).foldLeft(m.coefficient -> List.empty[(Natural, Natural)]) {
          case ((c, kept), power @ (Lit(base), Lit(e))) =>
            literalPower(base, e).fold(c -> (kept :+ power))(value => (c * value) -> kept)
          case ((c, kept), power) => c -> (kept :+ power)
        }
      symbolic(coefficient, powers)
    }

  private def literalPower(base: BigInt, e: BigInt): Option[BigInt] =
    Option
      .when(BigInt(base.bitLength - 1) * e < MaxLiteralBits)(base.pow(e.toInt))
      .filter(fits)

  private def reduce(power: (Natural, Natural)): List[(Natural, Natural)] = power match {
    case (Lit(value), exponent) =>
      roots(value).map((root, multiplicity) =>
        (Lit(root): Natural) -> times(Lit(multiplicity), exponent)
      )
    case other => List(other)
  }

  // value = 2^k · r^j with r odd and, below RootBits, not a perfect power.
  private def roots(value: BigInt): List[(BigInt, BigInt)] = {
    val twos = value.lowestSetBit
    val odd = value >> twos
    val (root, multiplicity) = if (odd.bitLength > RootBits) (odd, 1) else minimalRoot(odd, 1)
    (if (twos > 0) List(BigInt(2) -> BigInt(twos)) else Nil) ++
      (if (root > 1) List(root -> BigInt(multiplicity)) else Nil)
  }

  @tailrec private def minimalRoot(value: BigInt, multiplicity: Int): (BigInt, Int) =
    if (value < 3) (value, multiplicity)
    else
      Primes.iterator.map(p => p -> integerRoot(value, p)).collectFirst {
        case (p, root) if root.pow(p) == value => p -> root
      } match {
        case Some((p, root)) => minimalRoot(root, multiplicity * p)
        case None            => (value, multiplicity)
      }

  // floor(value^(1/p)) by Newton's iteration from an upper bound.
  private def integerRoot(value: BigInt, p: Int): BigInt = {
    @tailrec def descend(x: BigInt): BigInt = {
      val next = (x * (p - 1) + value / x.pow(p - 1)) / p
      if (next >= x) x else descend(next)
    }
    descend(BigInt(1) << (value.bitLength / p + 1))
  }

  private def merge(powers: List[(Natural, Natural)]): List[(Natural, Natural)] =
    powers
      .groupMapReduce(_._1)(_._2)(plus)
      .toList
      .filter(_._2 != Zero)
      .sortBy(_._1)

  private def symbolic(coefficient: BigInt, powers: List[(Natural, Natural)]): Natural = {
    val (rest, absorbed) = absorb(coefficient, powers)
    if (fits(rest)) assemble(rest, absorbed)
    else {
      // An oversized coefficient keeps only what a power of two can carry exactly.
      val twos = rest.lowestSetBit
      val odd = rest >> twos
      if (!fits(odd)) throw new Exceeded(s"literal wider than $MaxLiteralBits bits")
      val (left, merged) = absorb(odd, merge(absorbed :+ ((Lit(2): Natural) -> Lit(twos))))
      assemble(left, merged)
    }
  }

  // Pull every factor of a literal base out of the coefficient into that base's exponent.
  private def absorb(
      coefficient: BigInt,
      powers: List[(Natural, Natural)]
  ): (BigInt, List[(Natural, Natural)]) =
    powers.foldLeft(coefficient -> List.empty[(Natural, Natural)]) {
      case ((c, done), (Lit(root), e)) =>
        val (left, count) = divideOut(c, root, 0)
        left -> (done :+ ((Lit(root): Natural) -> (if (count == 0) e else plus(e, Lit(count)))))
      case ((c, done), other) => c -> (done :+ other)
    }

  @tailrec private def divideOut(value: BigInt, root: BigInt, count: Int): (BigInt, Int) =
    if (value % root == 0) divideOut(value / root, root, count + 1) else value -> count

  private def assemble(coefficient: BigInt, powers: List[(Natural, Natural)]): Natural = {
    val factors = (if (coefficient == 1) Nil else List(Lit(coefficient))) ++ powers.map {
      case (base, e) if e == One => base
      case (base, e)             => Pow(base, e)
    }
    factors.sorted match {
      case Nil          => One
      case List(single) => single
      case many         => Prod(many)
    }
  }

}
