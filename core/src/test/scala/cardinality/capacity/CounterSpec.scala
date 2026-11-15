package cardinality.capacity

import org.specs2.Specification
import org.specs2.matcher.MatchResult

class CounterSpec extends Specification {

  override def is =
    s2"""
      Can parse simple products $parseProduct
      Can parse lists $parseListOfSomething
      Looks inside packages $parsePackage
      Skips unknown statements, refuses unknown types $parseUnknown
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

  // `Color` is not supplied: X's row names it and has no number (never the ω placeholder), and the
  // import and the trait add no value.
  def parseUnknown: MatchResult[String] = {
    val found = Rows.rows(
      "import a.b\ntrait T\ncase class X(a: Color)" +
        cardinality.Compiled.withheld("class Color\npackage a { object b }")
    )
    (found(("X", Definition.Kind.Class)) must (startWith("unresolved[").and(contain("Color"))))
      .and(found(("T", Definition.Kind.Abstract)) === "—")
  }

}
