package cardinality.bench.algebra

import org.matheclipse.core.eval.ExprEvaluator
import org.matheclipse.core.expression.F
import org.matheclipse.core.interfaces.IExpr

/** One evaluator per JMH thread. All construction uses ASTs, not parsed strings. Comparison goes
  * through Exponent/Coefficient queries without requesting Expand; the projection reads the two low
  * coefficients and substitutes w=1 exactly. A query that stays symbolic (huge or symbolic
  * exponents) refuses instead of decoding to 0.
  */
final class SymjaAlgebra extends Algebra {
  type Value = IExpr
  val evaluator = new ExprEvaluator(true, 0.toShort)
  val omega: IExpr = F.symbol("w")
  def number(n: Int): IExpr = F.ZZ(n)
  def add(a: IExpr, b: IExpr): IExpr = evaluator.eval(F.Plus(a, b))
  def multiply(a: IExpr, b: IExpr): IExpr = evaluator.eval(F.Times(a, b))
  def power(a: IExpr, exponent: IExpr): IExpr = evaluator.eval(F.Power(a, exponent))

  private def integer(e: IExpr): BigInt =
    try BigInt(e.toString)
    catch {
      case _: NumberFormatException => throw new IllegalArgumentException(s"not an integer: $e")
    }

  private def coeff(p: IExpr, k: Int): BigInt =
    integer(evaluator.eval(F.Coefficient(p, omega, F.ZZ(k))))

  def compare(a: IExpr, b: IExpr): Int = {
    val da = integer(evaluator.eval(F.Exponent(a, omega)))
    val db = integer(evaluator.eval(F.Exponent(b, omega)))
    val byDegree = da.compare(db)
    if (byDegree != 0) byDegree
    else {
      require(da.isValidInt, s"degree $da: comparing degree by degree is infeasible")
      val top = da.toInt
      (top to 0 by -1).iterator.map(k => coeff(a, k).compare(coeff(b, k))).find(_ != 0).getOrElse(0)
    }
  }

  def project(a: IExpr): String = {
    val constant = coeff(a, 0)
    val linear = coeff(a, 1)
    val total = integer(evaluator.eval(F.ReplaceAll(a, F.Rule(omega, F.C1))))
    s"${total - constant - linear} P + $linear w + $constant"
  }

}
