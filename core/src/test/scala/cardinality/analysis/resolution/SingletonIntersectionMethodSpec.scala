package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class SingletonIntersectionMethodSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Singleton and intersection method shapes
      reject uncertain stable-value overlap                         $overlap
      reject unrelated trait and structural intersections           $unsupported
      preserve module member environment diagnostics                $members
      reject inaccessible paths and alias leaks                     $access
  """

  private def entry(code: String, name: String): MethodAnalysis.Entry =
    MethodAnalysis
      .analyze(
        List(
          MethodAnalysis.Input(
            "Singletons.scala",
            dialects.Scala3(code).parse[Source].get
          )
        )
      )
      .find(_.name == name)
      .get

  private def unknown(code: String, name: String = "pick") =
    entry(code, name).count must beLike { case Count.Unresolved(_) => ok }

  def overlap =
    unknown("def pick[A](x: A, y: A): x.type & y.type = ???")

  def unsupported =
    unknown("trait A { def a: Int }\ntrait B { def b: Int }\ndef pick(x: A & B): A = x")
      .and(unknown("def pick[A](x: A): (A, A) & A = ???"))
      .and(unknown("def pick(): String & Singleton = ???"))

  def members =
    unknown("object Token { def produce[A](): A = ??? }; def pick(x: Token.type): Boolean = true")
      .and(unknown("trait Base\nobject Token extends Base\ndef pick(): Token.type = Token"))

  def access =
    unknown("object Outer { private object Hidden }; def pick(): Outer.Hidden.type = ???")
      .and(
        unknown(
          "object Outer { private object Hidden; type Leak = Hidden.type }; " +
            "def pick(): Outer.Leak = ???"
        )
      )

}
