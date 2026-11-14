package cardinality.analysis.resolution

import cardinality.Pending
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class MethodRefinementSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Refined members in implementation signatures
      substitute a generic member equality                         $equality
      follow sibling equalities without merging independent binders $siblings
      use source member equalities without treating traits as products $sourceEquality
      resolve exact lower and upper bounds                         $exactBounds
      resolve a bottom upper bound                                 $bottomBound
      preserve non-exact Tuple bounds as diagnostics                ${Pending.knownLoss(tupleBound)}
      reject recursive member equations                            ${Pending.rejectedFixture(
      recursive
    )}
      do not erase refinements on a known product                   ${Pending.knownLoss(
      noProductErasure
    )}
      do not invent structural object representations               ${Pending.resolvedRefusal(
      noTraitProduct
    )}
      retain structural term constraints                           $noTermErasure
      resolve projected fields for constructor analysis             $constructor
      preserve qualification through a forward alias               $qualifiedAlias
      reject hidden member equalities                              ${Pending.rejectedFixture(
      hiddenEquality
    )}
      do not resolve shadowed type binders through source aliases   ${Pending.rejectedFixture(
      shadowedBinder
    )}
      do not assume a shadowed Nothing name denotes bottom          ${Pending.knownLoss(
      shadowedBottom
    )}
  """

  private def input(code: String) =
    cardinality.Compiled.Input("Refinement.scala", code)

  private def count(code: String, name: String = "choose"): Count =
    cardinality.Compiled.analyze(List(input(code))).find(_.name == name).get.count

  def equality =
    count(
      "type R[A] = { type X = A }; " +
        "def choose[A](x: R[A]#X, y: A): A = y"
    ) === Count.Finite(2)

  def siblings =
    count(
      "type R[A, B] = { type X = Y; type Y = A; type Z = B }; " +
        "def choose[A, B](x: R[A, B]#X, y: A, z: R[A, B]#Z): A = y"
    ) === Count.Finite(2)

  def sourceEquality =
    count(
      "trait R[A] { type X = A }; def choose[A](x: R[A]#X, y: A): A = y"
    ) === Count.Finite(2)

  def exactBounds =
    count("type R = { type X >: Unit <: Unit }; def choose(): R#X = ()") === Count.Finite(1)

  def bottomBound =
    count("type R = { type X <: Nothing }; def choose(): R#X = ???") === Count.Finite(0)

  def tupleBound =
    count("type R = { type X <: Tuple }; def choose(): R#X = ???") must beLike {
      case Count.Unresolved(reasons) =>
        reasons.mkString must contain("non-exact bounds")
    }

  def recursive =
    count("type R = { type X = Y; type Y = X }; def choose(): R#X = ???") must beLike {
      case Count.Unresolved(reasons) => reasons.mkString must contain("recursive refined member")
    }

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

  def constructor =
    count(
      "type R[A] = { type X = A }; case class Pair[A](x: R[A]#X, y: A)",
      "Pair.<init>"
    ) === Count.Finite(4)

  def qualifiedAlias =
    count(
      "object Types { type R = S; type S = { type X = Unit } }; " +
        "def choose(): Types.R#X = ()"
    ) === Count.Finite(1)

  def hiddenEquality =
    count(
      "trait R { private type X = Unit }; def choose(): R#X = ???"
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString must contain("non-public or opaque")
    }

  def shadowedBinder =
    count(
      "type R = { type X = Unit }; def choose[R](): R#X = ???"
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString must contain("member on type parameter")
    }

  def shadowedBottom =
    count(
      "type Nothing = Unit; type R = { type X <: Nothing }; def choose(): R#X = ???"
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString must contain("non-exact bounds")
    }

}
