package cardinality.capacity

import org.specs2.Specification
import org.specs2.matcher.MatchResult

class CounterSpec extends Specification {

  override def is =
    s2"""
      Can parse simple products $parseProduct
      Can parse lists $parseListOfSomething
      Looks inside packages $parsePackage
      Skips unknown statements, bounds unknown types $parseUnknown
      """

  def parseProduct: MatchResult[Size] = {
    (Rows.held("case class Four(a: Boolean, b: Boolean)") must
      beEqualTo(TinySize(4)))
      .and(
        Rows.held("case class ThirtyThree(a: Boolean, b: Int)") must
          beEqualTo(FiniteSize(33))
      )
      .and(
        Rows.held("case class NinetySix(a: Long, b: Int)") must
          beEqualTo(FiniteSize(96))
      )
      .and(
        // Two floating-point stand-ins multiply to a width of unmodelled precision, still marked.
        Rows.held("case class NinetySix(a: Double, b: Double)") must
          beEqualTo(LossyInfiniteSize(128))
      )
  }

  def parseListOfSomething: MatchResult[Size] = {
    Rows.held("case class NinetySix(a: List[Double])") must
      beEqualTo(EffectiveOmega)
  }

  def parsePackage: MatchResult[Size] = {
    Rows.held("package foo\ncase class Four(a: Boolean, b: Boolean)") must
      beEqualTo(TinySize(4))
  }

  def parseUnknown: MatchResult[Size] = {
    Rows.held("import a.b\ntrait T\ncase class X(a: Color)") must
      beEqualTo(EffectiveOmega)
  }

}
