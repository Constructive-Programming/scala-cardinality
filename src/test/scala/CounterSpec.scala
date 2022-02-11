
import scala.meta._
import org.specs2.Specification
import org.specs2.matcher.MatchResult

class CounterSpec extends Specification {
  override def is =
    s2"""
      Can parse a simple product $parseProduct
      """

  def parseProduct: MatchResult[Size] = {
    Counter.source("case class Four(a: Boolean, b: Boolean)".parse[Source].get) must beEqualTo(TinySize(4)) and
      (Counter.source("case class Four(a: Boolean, b: Int)".parse[Source].get) must beEqualTo(FiniteSize(33)))
  }
}
