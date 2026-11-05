package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** First-order beta reduction and the closed parametric identity fragment, not general rank-n. */
class PolyLambdaCardinalitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Type lambda beta reduction
      applies a direct lambda to caller arguments                 $direct
      does not capture a caller binder of the same spelling       $capture
      keeps nested lambda shadowing lexical                      $shadowing
      measures constructor fields through beta reduction          $constructor
      refuses an external constructor inside the lambda           $external
      refuses constrained lambda binders                         $bounds
    Closed polymorphic identity
      retains unknown external dependencies                       $dependency
  """

  private def count(code: String, name: String = "use"): Count =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Poly.scala", dialects.Scala3(code).parse[Source].get)))
      .find(_.name == name)
      .get
      .count

  private def unresolved(code: String, reason: String) =
    count(code) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString(" ") must contain(reason)
    }

  def direct =
    count("def use[A](x: A, y: A): ([x] =>> (x, x))[A] = ???") === Count.Finite(4)

  def capture =
    count("def use[A, B](x: A, y: B): ([B] =>> B)[A] = ???") === Count.Finite(1)

  def shadowing =
    count("def use[A, B](x: A, y: B): ([x] =>> ([x] =>> x)[B])[A] = ???") ===
      Count.Finite(1)

  def constructor =
    count("case class C[A](x: ([x] =>> x)[A], y: A)", "C.<init>") === Count.Finite(4)

  def external =
    unresolved("def use[A](x: A): ([x] =>> ZIO[Unit, Unit, x])[A] = ???", "ZIO")

  def bounds =
    unresolved("def use[A](x: A): ([x <: A] =>> x)[A] = ???", "constrained type lambda")

  def dependency =
    unresolved("def use: [a] => a => External[a] = ???", "External")

}
