package cardinality

import scala.meta.*

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
      preserve non-exact Tuple bounds as diagnostics                $tupleBound
      substitute stable parameter paths                            $path
      reject recursive member equations                            $recursive
      do not erase refinements on a known product                   $noProductErasure
      do not invent structural object representations               $noTraitProduct
      retain structural term constraints                           $noTermErasure
      resolve projected fields for constructor analysis             $constructor
      preserve qualification through a forward alias               $qualifiedAlias
      reject hidden member equalities                              $hiddenEquality
      do not resolve shadowed type binders through source aliases   $shadowedBinder
      do not assume a shadowed Nothing name denotes bottom          $shadowedBottom
      resolve paths in the binding's type-binder scope              $pathScope
      resolve declared stable values                               $stableValue
      do not read type equalities through mutable paths             $mutablePath
  """

  private def input(code: String) =
    MethodAnalysis.Input("Refinement.scala", dialects.Scala3(code).parse[Source].get)

  private def count(code: String, name: String = "choose"): Count =
    MethodAnalysis.analyze(List(input(code))).find(_.name == name).get.count

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

  def path = {
    val in = input("def choose[A](r: { type X = A }, x: A): r.X = x")
    val method = in.tree.stats.collectFirst { case d: Defn.Def => d }.get
    val frame = MethodAnalysis.Frame(
      "test",
      Nil,
      None,
      Nil,
      method.paramClauseGroups.flatMap(_.tparamClause.values),
      method.paramClauseGroups.flatMap(_.paramClauses.flatMap(_.values))
    )
    val index = new MethodAnalysis.Index(List(in), MethodAnalysis.Limits())
    val env = index.typeParameters(frame)
    index.resolve(method.decltpe.get, frame, env, Set.empty) === env("A")
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

  def pathScope = {
    val in = input("def choose[A](r: { type X = A }): r.X = ???")
    val method = in.tree.stats.collectFirst { case d: Defn.Def => d }.get
    val outer = MethodAnalysis.Frame(
      "outer",
      Nil,
      None,
      Nil,
      method.paramClauseGroups.flatMap(_.tparamClause.values),
      method.paramClauseGroups.flatMap(_.paramClauses.flatMap(_.values))
    )
    val inner = outer.copy(id = "inner", parent = Some(outer), params = Nil)
    val index = new MethodAnalysis.Index(List(in), MethodAnalysis.Limits())
    val result = index.resolve(method.decltpe.get, inner, index.typeParameters(inner), Set.empty)
    (result === index.typeParameters(outer)("A")).and(result !== index.typeParameters(inner)("A"))
  }

  private def valuePath(code: String): MethodAnalysis.Resolved = {
    val in = input(code)
    val method = in.tree.stats.collectFirst { case d: Defn.Def => d }.get
    val frame = MethodAnalysis.Frame("values", Nil, None, in.tree.stats)
    val index = new MethodAnalysis.Index(List(in), MethodAnalysis.Limits())
    index.resolve(method.decltpe.get, frame, Map.empty, Set.empty)
  }

  def stableValue =
    valuePath("val r: { type X = Unit } = ???; def choose(): r.X = ()") ===
      Right(Inhabitation.Shape.Product(Nil))

  def mutablePath =
    valuePath("var r: { type X = Unit } = ???; def choose(): r.X = ()") must beLike {
      case Left(reason) => reason must contain("unavailable stable path type")
    }

}
