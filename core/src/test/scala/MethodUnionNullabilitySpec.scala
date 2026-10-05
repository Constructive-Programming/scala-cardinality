package cardinality

import scala.meta.*

import org.specs2.Specification

class MethodUnionNullabilitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Untagged union method shapes
      collapse duplicate alternatives rather than tag them          $duplicates
      remove Nothing alternatives                                  $bottom
      preserve aliases of one free binder                           $binderAlias
      reject distinct potentially overlapping free binders           $overlapping
      do not confuse nominal types with equal product shapes         $nominal
      keep unsupported concrete alternatives unresolved             $unsupported
    Null-free implementation semantics
      erase explicit nullability without adding a constructor        $nullable
      treat Null alone as empty                                      $nullOnly
      use the same normalization for constructor signatures          $constructor
      respect a source declaration shadowing Null                    $shadowedNull
  """

  private def entry(code: String, name: String = "pick"): MethodAnalysis.Entry =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Union.scala", dialects.Scala3(code).parse[Source].get)))
      .find(_.name == name)
      .get

  def duplicates =
    entry("def pick[A](x: A, y: A): (A | A) | A = x").count === Count.Finite(2)

  def bottom =
    entry("def pick[A](x: Nothing | A, y: A): A | scala.Nothing = x").count === Count
      .Finite(2)

  def binderAlias =
    entry(
      "class E[A] { type Same = A; def pick(x: A, y: Same): A | Same = x }",
      "E.pick"
    ).count === Count.Finite(2)

  def overlapping =
    entry("def pick[A, B](x: A, y: B): A | B = x").count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("overlap and discrimination proof")) must beTrue
    }

  def nominal =
    entry("case class One()\ncase class Two()\ndef pick(): One | Two = One()").count must
      beLike { case Count.Unresolved(_) => ok }

  def unsupported =
    entry("def pick(x: String): String | Boolean = x").count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(r =>
          r.contains("union") && r.contains("unresolved type: String")
        ) must beTrue
    }

  def nullable =
    entry("def pick[A](x: A | Null, y: A): (A | scala.Null) | Null = x").count === Count
      .Finite(2)

  def nullOnly = {
    val empty = entry("def pick(): Null = null").count
    val absurd = entry("def pick(x: Null | Nothing): Boolean = true").count
    (empty === Count.Finite(0)).and(absurd === Count.Finite(1))
  }

  def constructor =
    entry(
      "case class NullablePair[A](x: A | Null, y: A | Null)",
      "NullablePair.<init>"
    ).count === Count.Finite(4)

  def shadowedNull =
    entry("type Null = Boolean\ndef pick(): Null | scala.Null = true").count === Count
      .Finite(2)

}
