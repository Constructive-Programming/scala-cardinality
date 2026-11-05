package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** The binders a context-function result puts around a body (docs/research/syntax-features.md):
  * where Scala wraps, and what a search inside the wrapped body reaches. Fixtures are Scala 3.9.0
  * programs of the review round (a5-fix1 r1–r8, w1–w4), run where a value decides the count; each
  * expected number is derived beside it.
  */
class ContextBindersSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    An overridden result is the one the subclass sees
      several overridden declarations refuse the local definition    $severalOverridden
  """

  private def rows(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Binders.scala", dialects.Scala3(code).parse[Source].get)))
      .map(e => e.name -> e.count)
      .toMap

  private def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  // Both declarations match `g`: Scala intersects their results, which this reading does not.
  def severalOverridden =
    refused(
      rows("""trait Two[A] { def g: A ?=> A }
        |trait Other[A] { def g: A }
        |final class Both[A](a: A) extends Two[A], Other[A ?=> A] { def g = { def lb: A = a; lb } }
        |""".stripMargin)("Both.g.lb"),
      "overriding member g of several declarations, in the result of enclosing g"
    )

}
