package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.matcher.{MatchResult, MustMatchers}

/** Source-to-row helpers of the definition inventory's specs: every row of a fixture by name. */
trait InventoryFixtures extends MustMatchers {
  import Inhabitation.Count

  protected def entries(code: String): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyze(
      List(MethodAnalysis.Input("Example.scala", dialects.Scala3(code).parse[Source].get))
    )

  protected def rows(code: String): Map[String, Count] =
    entries(code).map(entry => entry.name -> entry.count).toMap

  protected def rendered(code: String): Map[String, String] =
    rows(code).view.mapValues(_.render).toMap

  protected def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  protected val getter = "trait Getter[S, A] { def get(s: S): A }\n"

}
