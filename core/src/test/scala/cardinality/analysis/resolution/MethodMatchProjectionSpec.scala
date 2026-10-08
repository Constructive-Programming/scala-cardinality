package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.types.*
import org.specs2.Specification

class MethodMatchProjectionSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Method match-type tuple projections
      reduce concrete first and second components                  $concrete
      preserve argument identities through aliases                 $application
      normalize tuple aliases and nested projections               $nesting
      reduce direct and nested tuple patterns                       $direct
      project constructor inputs                                   $constructor
      keep owner and method binders distinct                       $hygiene
      keep pattern binders separate from alias parameters          $patternHygiene
      do not reduce a free scrutinee                                $inert
      report a nonmatching tuple                                   $nonmatching
      do not confuse a case-class product with a tuple              $nominal
      do not guess fixed-pattern identity or skip an unknown case   $fixed
      do not skip a nested pattern on an abstract component         $nestedInert
      substitute nested alias syntax                               $replacement
      keep selected member names nominal                           $selectionHygiene
      substitute outer parameters in a match-case body             $outerReplacement
  """

  private val projections =
    "type Fst[T] = T match { case (f, s) => f }\n" +
      "type Snd[T] = T match { case (f, s) => s }\n"

  private def entry(code: String, name: String = "pick"): MethodAnalysis.Entry =
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("MatchProjection.scala", dialects.Scala3(code).parse[Source].get))
      )
      .find(_.name == name)
      .get

  private def unresolved(code: String, reason: String) =
    entry(code).count must beLike {
      case Count.Unresolved(reasons) if reasons.exists(_.contains(reason)) => ok
    }

  def concrete = {
    val first = entry(projections + "def pick(): Fst[(Boolean, Unit)] = true")
    val second = entry(projections + "def pick(): Snd[(Boolean, Unit)] = ()")
    (first.count === Count.Finite(2)).and(second.count === Count.Finite(1))
  }

  def application =
    entry(projections + "def pick[A, B](x: A, y: A, z: B): Fst[(A, B)] = x").count ===
      Count.Finite(2)

  def nesting =
    entry(
      projections + "type Pair[A, B] = (A, B)\n" +
        "type Wrapped[T] = Fst[T]\n" +
        "def pick[A, B, C](x: A, y: B, z: C): Wrapped[Fst[Pair[Pair[A, B], C]]] = x"
    ).count === Count.Finite(1)

  def constructor =
    entry(projections + "case class Box[A, B](x: Fst[(A, B)], y: A)", "Box.<init>").count ===
      Count.Finite(4)

  def direct =
    entry(
      "def pick[A, B, C](x: A): ((A, B), C) match { case ((f, s), t) => f } = x"
    ).count === Count.Finite(1)

  def hygiene =
    entry(
      "class Outer[A](val outer: A) {\n" +
        "type Fst[T] = T match { case (f, s) => f }\n" +
        "def pick[A](x: A): Fst[(A, Unit)] = x\n}",
      "Outer.pick"
    ).count === Count.Finite(1)

  def patternHygiene =
    entry(
      "type Pick[f, T] = T match { case (f, s) => f }\n" +
        "def pick[A, B](x: A, y: B): Pick[B, (A, Unit)] = x"
    ).count === Count.Finite(1)

  def inert =
    unresolved(projections + "def pick[T](x: T): Fst[T] = ???", "inert match type")

  def nonmatching =
    unresolved(projections + "def pick[A, B, C](x: A): Fst[(A, B, C)] = ???", "nonmatching tuple")

  def nominal =
    unresolved(
      projections + "case class Pair[A, B](x: A, y: B)\n" +
        "def pick[A, B](x: A): Fst[Pair[A, B]] = ???",
      "inert match type"
    )

  def fixed =
    unresolved(
      "type Pick[T] = T match {\ncase (Boolean, s) => Boolean\ncase (f, s) => f\n}\n" +
        "def pick[A](x: A): Pick[(A, Unit)] = ???",
      "type-identity/disjointness proof"
    )

  def replacement =
    MatchTypes
      .replace(
        dialects.Scala3("Fst[T]").parse[Type].get,
        Map(TypeName.of("T") -> Type.Name("Boolean"))
      )
      .syntax === "Fst[Boolean]"

  def nestedInert =
    unresolved(
      "type Pick[T] = T match {\ncase ((f, s), t) => f\ncase (f, s) => f\n}\n" +
        "def pick[A](x: A): Pick[(A, Unit)] = ???",
      "nested scrutinee is not a proven tuple"
    )

  def outerReplacement =
    MatchTypes
      .reduced(
        dialects.Scala3("T match { case (f, s) => A }").parse[Type].get,
        Map(
          TypeName.of("T") -> Type.Tuple(List(Type.Name("Boolean"), Type.Name("Unit"))),
          TypeName.of("A") -> Type.Name("Unit")
        )
      )
      .map(_.syntax) === Some("Unit")

  def selectionHygiene =
    MatchTypes
      .replace(
        dialects.Scala3("Owner.A").parse[Type].get,
        Map(TypeName.of("A") -> Type.Name("Boolean"))
      )
      .syntax === "Owner.A"

}
