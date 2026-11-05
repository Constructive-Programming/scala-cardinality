package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class MethodRefinementSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Refined members in implementation signatures
      substitute stable parameter paths                            $path
      do not erase refinements on a known product                   $noProductErasure
      do not invent structural object representations               $noTraitProduct
      retain structural term constraints                           $noTermErasure
      resolve paths in the binding's type-binder scope              $pathScope
      resolve declared stable values                               $stableValue
      do not read type equalities through a var (no stable path)    $mutablePath
  """

  private def input(code: String) =
    MethodAnalysis.Input("Refinement.scala", dialects.Scala3(code).parse[Source].get)

  private def count(code: String, name: String = "choose"): Count =
    MethodAnalysis.analyze(List(input(code))).find(_.name == name).get.count

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
      case Left(reason) => reason must contain("not a stable path: r is a var")
    }

}
