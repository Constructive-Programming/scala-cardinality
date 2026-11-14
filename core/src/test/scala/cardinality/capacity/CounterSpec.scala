package cardinality.capacity

import cardinality.Pending
import org.specs2.Specification
import org.specs2.matcher.MatchResult

class CounterSpec extends Specification {

  override def is =
    s2"""
      Can parse simple products $parseProduct
      Can parse lists $parseListOfSomething
      Looks inside packages $parsePackage
      Skips unknown statements, bounds unknown types ${Pending.countDiffers(parseUnknown)}
      """

  def parseProduct: MatchResult[Size] = {
    (Rows.total("case class Four(a: Boolean, b: Boolean)") must
      beEqualTo(TinySize(4)))
      .and(
        Rows.total("case class ThirtyThree(a: Boolean, b: Int)") must
          beEqualTo(FiniteSize(33))
      )
      .and(
        Rows.total("case class NinetySix(a: Long, b: Int)") must
          beEqualTo(FiniteSize(96))
      )
      .and(
        // Two floating-point stand-ins multiply to a width of unmodelled precision, still marked.
        Rows.total("case class NinetySix(a: Double, b: Double)") must
          beEqualTo(LossyInfiniteSize(128))
      )
  }

  def parseListOfSomething: MatchResult[Size] = {
    Rows.total("case class NinetySix(a: List[Double])") must
      beEqualTo(EffectiveOmega)
  }

  def parsePackage: MatchResult[Size] = {
    Rows.total("package foo\ncase class Four(a: Boolean, b: Boolean)") must
      beEqualTo(TinySize(4))
  }

  def parseUnknown: MatchResult[Size] = {
    Rows.total(
      "import a.b\ntrait T\ncase class X(a: Color)" +
        cardinality.Compiled.withheld("class Color\npackage a { object b }")
    ) must
      beEqualTo(EffectiveOmega)
  }

}
