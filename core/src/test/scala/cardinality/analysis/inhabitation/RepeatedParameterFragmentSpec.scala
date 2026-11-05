package cardinality.analysis.inhabitation

import scala.meta.*

import cardinality.analysis.*
import org.specs2.Specification

class RepeatedParameterFragmentSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Repeated argument sequences
      parse the repeated wrapper around a function                $functionAst
      construct only Nil without an element producer              $emptyIntroduction
      prove infinitely many lengths with an element producer      $nonemptyIntroduction
      do not confuse an unproductive element cycle with a seed    $unproductiveElement
      construct repeated functions by arrow introduction          $functionIntroduction
      ignore sequence elimination for a Unit result               $unitResult
      preserve normal lists with an always-empty repeated list    $normalLists
      preserve choices when an empty sequence is captured         $emptyCapture
      count an empty repeated constructor                         $emptyConstructor
      eliminate a constructor slot's sequence by length           $ordinaryConstructor
      eliminate repeated functions by length                      $repeatedFunctions
      count length-dependent selection beside a fallback          $fallback
      reject a sequence under a deeper arrow                      $nestedArrow
      retain unsupported element-type diagnostics                 $unknownElement
  """

  private val a = Atom("A")
  private val unit = Product(Nil)
  private val nothing = Sum(Nil)

  private def entry(code: String, name: String): MethodAnalysis.Entry = {
    val source = dialects.Scala3(code).parse[Source].get
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Repeated.scala", source)))
      .find(_.name == name)
      .get
  }

  private def unsupported(count: Count) =
    count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("Repeated-argument sequence elimination is unsupported")) must
          beTrue
    }

  def functionAst = {
    val source = dialects.Scala3("def f[A](fs: (A => Unit)*): Unit = ()").parse[Source].get
    source.collect { case Type.Repeated(_: Type.Function) => true } === List(true)
  }

  def emptyIntroduction =
    (Inhabitation.count(Nil, Repeated(a)) === Count.Finite(1)).and(
      Inhabitation.count(Nil, Repeated(nothing)) === Count.Finite(1)
    )

  def nonemptyIntroduction =
    (Inhabitation.count(List(Binding("seed", a)), Repeated(a)) === Count.Countable).and(
      Inhabitation.count(Nil, Repeated(unit)) === Count.Countable
    )

  def unproductiveElement =
    Inhabitation.count(List(Binding("step", Function(List(a), a))), Repeated(a)) ===
      Count.Finite(1)

  def functionIntroduction =
    Inhabitation.count(Nil, Repeated(Function(List(a), a))) === Count.Countable

  def unitResult =
    entry("def ignore[A](xs: A*): Unit = ()", "ignore").count === Count.Finite(1)

  def normalLists =
    entry("def pick[A](x: A)(xs: Nothing*): A = x", "pick").count === Count.Finite(1)

  def emptyCapture =
    entry(
      "class Env(xs: Nothing*) { def pick[A](x: A, y: A): A = x }",
      "Env.pick"
    ).count === Count.Finite(2)

  def emptyConstructor =
    entry("case class Empty(xs: Nothing*)", "Empty.<init>").count === Count.Finite(1)

  // The slot `xs: A*` is filled from `xs` itself: Nil alone at length 0, every list of e₁ at length
  // 1 — ω implementations (identity, tail, reverse, …).
  def ordinaryConstructor =
    entry("case class Many[A](xs: A*)", "Many.<init>").count === Count.Countable

  // No f: only x. One f: x, f(x), f(f(x)), … — two answers at length 1 already, so ω.
  def repeatedFunctions =
    (entry("def use[A](x: A)(fs: (A => A)*): A = x", "use").count === Count.Countable).and(
      entry("def ignore[A](fs: (A => Unit)*): Unit = ()", "ignore").count === Count.Finite(1)
    )

  // x or y at every length, so infinitely many length tests tell implementations apart: ω.
  def fallback =
    entry("def pick[A](x: A, y: A)(xs: A*): A = if xs.isEmpty then x else y", "pick").count ===
      Count.Countable

  // A sequence parameter of a function the goal returns is not an input of the outer environment.
  def nestedArrow =
    unsupported(
      Inhabitation.count(List(Binding("x", a)), Product(List(a, Function(List(Repeated(a)), a))))
    )

  def unknownElement =
    entry("def ignore(xs: Missing*): Boolean = true", "ignore").count must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains("Missing")) must beTrue
    }

}
