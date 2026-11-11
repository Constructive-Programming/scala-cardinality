package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.matcher.{MatchResult, MustMatchers}

/** Source-to-row helpers of the method-side specs: every row of a fixture by name. A single source
  * is read as `Example.scala`.
  */
trait InventoryFixtures extends MustMatchers {
  import Inhabitation.Count

  protected def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    cardinality.Compiled.inputs(sources*)

  protected def counts(sources: (String, String)*): Map[String, Count] =
    cardinality.Compiled.analyze(inputs(sources*)).map(e => e.name -> e.count).toMap

  protected def entries(code: String): List[MethodAnalysis.Entry] =
    cardinality.Compiled.analyze(inputs("Example.scala" -> code))

  protected def rows(code: String): Map[String, Count] =
    entries(code).map(entry => entry.name -> entry.count).toMap

  /** The rows of the fixture with its methods open: read with the overriders `Overriders.opened`
    * writes. A `val`, a member written without a type or an inherited one is opened by an overrider
    * the fixture writes itself.
    */
  protected def openRows(code: String): Map[String, Count] =
    cardinality.Compiled
      .analyze(cardinality.Overriders.opened(inputs("Example.scala" -> code)))
      .map(entry => entry.name -> entry.count)
      .toMap

  protected def count(code: String, name: String): Count = rows(code)(name)

  protected def rendered(code: String): Map[String, String] =
    rows(code).view.mapValues(_.render).toMap

  protected def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  protected val getter = "trait Getter[S, A] { def get(s: S): A }\n"

}
