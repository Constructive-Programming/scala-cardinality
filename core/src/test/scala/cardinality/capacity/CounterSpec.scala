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
    (Counter.source(cardinality.Compiled.source("case class Four(a: Boolean, b: Boolean)")) must
      beEqualTo(TinySize(4)))
      .and(
        Counter.source(
          cardinality.Compiled.source("case class ThirtyThree(a: Boolean, b: Int)")
        ) must
          beEqualTo(FiniteSize(33))
      )
      .and(
        Counter.source(cardinality.Compiled.source("case class NinetySix(a: Long, b: Int)")) must
          beEqualTo(FiniteSize(96))
      )
      .and(
        // Two floating-point stand-ins multiply to a width of unmodelled precision, still marked.
        Counter
          .source(cardinality.Compiled.source("case class NinetySix(a: Double, b: Double)")) must
          beEqualTo(LossyInfiniteSize(128))
      )
  }

  def parseListOfSomething: MatchResult[Size] = {
    Counter.source(cardinality.Compiled.source("case class NinetySix(a: List[Double])")) must
      beEqualTo(EffectiveOmega)
  }

  def parsePackage: MatchResult[Size] = {
    Counter.source(
      cardinality.Compiled.source("package foo\ncase class Four(a: Boolean, b: Boolean)")
    ) must
      beEqualTo(TinySize(4))
  }

  def parseUnknown: MatchResult[Size] = {
    Counter.source(
      cardinality.Compiled.source(
        "import a.b\ntrait T\ncase class X(a: Color)" +
          cardinality.Compiled.withheld("class Color\npackage a { object b }")
      )
    ) must
      beEqualTo(EffectiveOmega)
  }

}
