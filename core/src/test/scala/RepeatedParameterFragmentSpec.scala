package cardinality

import scala.meta.*

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
      reject nonempty repeated constructor elimination            $ordinaryConstructor
      reject treating repeated functions as a supplied callable   $repeatedFunctions
      reject total length-dependent selection despite a fallback  $fallback
      reject elimination introduced under a result arrow          $negativeArrow
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
        reasons.exists(_.contains("Repeated-argument sequence elimination")) must beTrue
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

  def ordinaryConstructor =
    unsupported(entry("case class Many[A](xs: A*)", "Many.<init>").count)

  def repeatedFunctions =
    unsupported(
      entry("def use[A](x: A)(fs: (A => A)*): A = x", "use").count
    ).and(
      entry("def ignore[A](fs: (A => Unit)*): Unit = ()", "ignore").count === Count.Finite(1)
    )

  def fallback =
    unsupported(
      entry("def pick[A](x: A, y: A)(xs: A*): A = if xs.isEmpty then x else y", "pick").count
    )

  def negativeArrow =
    unsupported(Inhabitation.count(Nil, Function(List(Repeated(a), a), a)))

  def unknownElement =
    entry("def ignore(xs: Missing*): Unit = ()", "ignore").count must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains("Missing")) must beTrue
    }

}
