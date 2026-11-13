package cardinality.bench.algebra

/** Common ground; w is a formal indeterminate, never a CAS's built-in infinity.
  *
  * The analyzer needs exactly two things from a carrier, and only those two are use cases here:
  * decide the model's order on two expressions (<, =, >), and render the reporting projection
  * `aε₀ + bωⁿ + cω + d`. Everything else is construction of the expressions the counting rules
  * build. No operation solves an expression to a numerical value; a backend may substitute
  * internally only as an exact means of computing a projection coefficient.
  *
  * Powers take their exponent as a value: a function space raises the codomain to the domain's
  * cardinality, and that domain can itself be a polynomial.
  */
trait Algebra {
  type Value
  def number(n: Int): Value
  def omega: Value
  def add(a: Value, b: Value): Value
  def multiply(a: Value, b: Value): Value
  def power(a: Value, exponent: Value): Value
  def render(a: Value): String = a.toString

  /** The model's leading-degree order: negative, zero or positive. Equal degrees compare
    * coefficients from the top down. A CAS's own expression sorting is not this order; every
    * backend implements it itself.
    */
  def compare(a: Value, b: Value): Int

  /** The reporting projection: every degree >= 2 collapses into one ωⁿ tier carrying the sum of its
    * coefficients, then the ω coefficient, then the constant.
    */
  def project(a: Value): String

  def binomial(n: Int): Value = power(add(number(1), omega), number(n))

  def product(n: Int): Value = (1 to n).foldLeft(number(1)) { (p, i) =>
    multiply(p, add(number(i % 3 + 1), omega))
  }

  def sum(n: Int): Value = (1 to n).foldLeft(number(0)) { (s, i) =>
    add(s, multiply(add(number(i), omega), add(number(i + 1), omega)))
  }

}

object Algebra {

  def named(name: String): Algebra = name match {
    case "symja"   => new SymjaAlgebra
    case "rings"   => new RingsAlgebra
    case "model"   => new ModelAlgebra
    case "current" => new CurrentAlgebra
    case other     => throw new IllegalArgumentException(other)
  }

}

/** Twelve expressions as the analyzer's arithmetic produces them, each tied to the Scala shape that
  * gives rise to it; w stands for ω. The first eight are polynomials a materialized carrier can
  * hold. The last four put w in the exponent — a huge finite exponent, two polynomial exponents and
  * an infinite one — which is where the function spaces of ordinary types live.
  */
object Cases {

  val corpus = List(
    "eitherStrings", // Either[String, String] -> w + w
    "optionString", // Option[String] -> w + 1
    "pairStrings", // (String, String) -> w·w
    "pairOptions", // (Option[String], Option[String]) -> (1+w)^2
    "tripleOptions", // (Option[String], Option[String], Option[String]) -> (1+w)^3
    "booleanOption", // (Boolean, Option[String]) -> 2·(1+w)
    "eitherStringPair", // Either[String, (String, String)] -> w^2 + w
    "optionEither", // (Option[String], Either[Boolean, String]) -> (1+w)·(2+w)
    "intToString", // Int => String -> w^(2^32)
    "optionDomain", // Option[String] => String -> w^(1+w)
    "eitherDomain", // Either[String, (String, String)] => String -> w^(w^2+w)
    "stringToString" // String => String -> w^w
  )

  val polynomial = corpus.take(8)

  val beyond = corpus.drop(8)

  def corpusExpr(a: Algebra)(name: String): a.Value = name match {
    case "eitherStrings"    => a.add(a.omega, a.omega)
    case "optionString"     => a.add(a.omega, a.number(1))
    case "pairStrings"      => a.multiply(a.omega, a.omega)
    case "pairOptions"      => a.power(a.add(a.number(1), a.omega), a.number(2))
    case "tripleOptions"    => a.power(a.add(a.number(1), a.omega), a.number(3))
    case "booleanOption"    => a.multiply(a.number(2), a.add(a.number(1), a.omega))
    case "eitherStringPair" => a.add(a.multiply(a.omega, a.omega), a.omega)
    case "optionEither"     => a.multiply(a.add(a.number(1), a.omega), a.add(a.number(2), a.omega))
    case "intToString"      => a.power(a.omega, a.power(a.number(2), a.number(32)))
    case "optionDomain"     => a.power(a.omega, a.add(a.number(1), a.omega))
    case "eitherDomain"     => a.power(a.omega, a.add(a.multiply(a.omega, a.omega), a.omega))
    case "stringToString"   => a.power(a.omega, a.omega)
    case other              => throw new IllegalArgumentException(other)
  }

  /** Comparisons over the corpus, signs derived by hand from the leading-degree order. */
  val corpusPairs = List(
    "eitherBelowPair", // 2w < w^2: the ordering this evaluation exists to check
    "pairBelowSumOfPowers", // w^2 < w^2 + 2w + 1
    "sumBelowCube", // w^2 + 2w + 1 < w^3 + 3w^2 + 3w + 1
    "eitherPairBelowOptionPair", // w^2 + w < w^2 + 2w + 1
    "optionBelowBooleanOption", // w + 1 < 2w + 2
    "mixedAboveOptionPair", // w^2 + 3w + 2 > w^2 + 2w + 1
    "squaredEqualsExpansion" // (1+w)^2 = w^2 + 2w + 1, factored against expanded
  )

  private val sides = Map(
    "eitherBelowPair" -> ("eitherStrings", "pairStrings"),
    "pairBelowSumOfPowers" -> ("pairStrings", "pairOptions"),
    "sumBelowCube" -> ("pairOptions", "tripleOptions"),
    "eitherPairBelowOptionPair" -> ("eitherStringPair", "pairOptions"),
    "optionBelowBooleanOption" -> ("optionString", "booleanOption"),
    "mixedAboveOptionPair" -> ("optionEither", "pairOptions")
  )

  def corpusLeft(a: Algebra)(pair: String): a.Value = pair match {
    case "squaredEqualsExpansion" => corpusExpr(a)("pairOptions")
    case other                    => corpusExpr(a)(sides(other)._1)
  }

  def corpusRight(a: Algebra)(pair: String): a.Value = pair match {
    case "squaredEqualsExpansion" =>
      a.add(a.add(a.multiply(a.omega, a.omega), a.multiply(a.number(2), a.omega)), a.number(1))
    case other => corpusExpr(a)(sides(other)._2)
  }

}
