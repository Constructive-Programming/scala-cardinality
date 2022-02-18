
import scala.meta._
import org.specs2.Specification
import org.specs2.matcher.MatchResult

class CounterSpec extends Specification {
  override def is =
    s2"""
      Can parse simple products $parseProduct
      Can parse lists $parseProduct
      """

  def parseProduct: MatchResult[Size] = {
    (Counter.source("case class Four(a: Boolean, b: Boolean)".parse[Source].get) must
      beEqualTo(TinySize(4))) and
    (Counter.source("case class ThirtyThree(a: Boolean, b: Int)".parse[Source].get) must
      beEqualTo(FiniteSize(33))) and
    (Counter.source("case class NinetySix(a: Long, b: Int)".parse[Source].get) must
      beEqualTo(FiniteSize(96))) and
    (Counter.source("case class NinetySix(a: Double, b: Double)".parse[Source].get) must
      beEqualTo(FiniteSize(128)))
  }

  def parseListOfSomething: MatchResult[Size] = {
    (Counter.source("case class NinetySix(a: List[Double])".parse[Source].get) must
      beEqualTo(EffectiveOmega))
  }
}
