package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** A subtype bound beside a `Unit` goal (docs/research/higher-kinded-binders.md, R5). The fixture
  * compiles with Scala 3.9.0.
  */
class BinderBoundsSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Interactions
      a Unit goal is not refused for what it never reads            $unitGoal
  """

  private def entries(code: String, path: String = "Bounds.scala"): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private val flag =
    "sealed trait Flag\ncase object On extends Flag\ncase object Off extends Flag\n"

  // One construction whatever the context holds (before: the callable refused it).
  def unitGoal = {
    val counts = entries(flag + """
      object U {
        def callable[A <: B, B](g: B => A): Unit = ()
        def shared[A <: Flag](a: A, b: A): Unit = ()
      }
    """)
    (counts("U.callable") === Count.Finite(1)).and(counts("U.shared") === Count.Finite(1))
  }

}
