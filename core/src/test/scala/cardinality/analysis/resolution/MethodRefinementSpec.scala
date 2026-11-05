package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class MethodRefinementSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Refined members in implementation signatures
      do not erase refinements on a known product                   $noProductErasure
      do not invent structural object representations               $noTraitProduct
      retain structural term constraints                           $noTermErasure
  """

  private def input(code: String) =
    MethodAnalysis.Input("Refinement.scala", dialects.Scala3(code).parse[Source].get)

  private def count(code: String, name: String = "choose"): Count =
    MethodAnalysis.analyze(List(input(code))).find(_.name == name).get.count

  def noProductErasure =
    count(
      "case class P[A](value: A); " +
        "def choose[A](x: A): P[A] { type X = A } = ???"
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString must contain("constraints not erased")
    }

  def noTraitProduct =
    count(
      "trait Optic[A] { type X }; " +
        "def choose[A](x: A): Optic[A] { type X = A } = ???"
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString must contain("representation unavailable")
    }

  def noTermErasure =
    count("def choose(): Unit { def unavailable: Boolean } = ???") must beLike {
      case Count.Unresolved(_) => ok
    }

}
