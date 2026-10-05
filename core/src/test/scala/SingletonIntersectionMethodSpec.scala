package cardinality

import scala.meta.*

import org.specs2.Specification

class SingletonIntersectionMethodSpec extends Specification {
  import Inhabitation.{Count, Shape}

  def is = s2"""
    Singleton and intersection method shapes
      count an accessible empty module and constructor             $module
      preserve a supplied stable path and its widened provenance   $stable
      normalize stable alias identity                              $alias
      keep different singleton identities distinct                  $identities
      eliminate idempotent intersections without multiplying        $idempotent
      refine a proven singleton with Singleton                      $marker
      refine a stable path by its own type binder                    $binder
      keep distinct module intersections empty                      $disjoint
      reject uncertain stable-value overlap                         $overlap
      reject unrelated trait and structural intersections           $unsupported
      preserve module member environment diagnostics                $members
      reject inaccessible paths and alias leaks                     $access
      reject unstable, missing and non-capture references            $negative
  """

  private def entry(code: String, name: String = "pick"): MethodAnalysis.Entry =
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

  def module =
    (entry("object Token\ndef pick(): Token.type = Token").count === Count.Finite(1))
      .and(
        entry("object Token\ncase class Box(x: Token.type)", "Box.<init>").count === Count.Finite(1)
      )

  def stable =
    (entry("def pick[A](x: A): x.type = x").count === Count.Finite(1))
      .and(entry("def pick[A](x: A)(same: x.type): A = same").count === Count.Finite(1))

  def alias =
    entry(
      "class Env[A](x: A) { val alias: A = x; def pick(): x.type & alias.type = x }",
      "Env.pick"
    ).count === Count.Finite(1)

  def identities = {
    val a = Shape.Singleton("a", None)
    val b = Shape.Singleton("b", None)
    (a !== b)
      .and(a !== Shape.Product(Nil))
      .and(entry("object A\nobject B\ndef pick(x: A.type): B.type = B").count === Count.Finite(1))
  }

  def idempotent =
    (entry("def pick[A](x: A, y: A): A & A = x").count === Count.Finite(2))
      .and(entry("def pick(x: Boolean & Boolean): Boolean = x").count === Count.Finite(4))

  def marker =
    (entry("object Token\ndef pick(): Token.type & Singleton = Token").count === Count.Finite(1))
      .and(entry("def pick[A](x: A): scala.Singleton & x.type = x").count === Count.Finite(1))
      .and(
        unknown("object Token\nimport custom.Singleton\ndef pick(): Token.type & Singleton = Token")
      )

  def disjoint =
    entry("object A\nobject B\ndef pick(): A.type & B.type = ???").count === Count.Finite(0)

  def binder =
    (entry("def pick[A](x: A): x.type & A = x").count === Count.Finite(1))
      .and(entry("def pick[A](x: A): A & x.type = x").count === Count.Finite(1))
      .and(unknown("class Env[A](x: A) { def pick[A](): x.type & A = ??? }", "Env.pick"))

  def overlap =
    unknown("def pick[A](x: A, y: A): x.type & y.type = ???")

  def unsupported =
    unknown("trait A\ntrait B\ndef pick(x: A & B): A = x")
      .and(unknown("def pick[A](x: A): (A, A) & A = ???"))
      .and(unknown("def pick(): String & Singleton = ???"))

  def members =
    unknown("object Token { def produce[A](): A = ??? }; def pick(x: Token.type): Unit = ()")
      .and(unknown("trait Base\nobject Token extends Base\ndef pick(): Token.type = Token"))

  def access =
    unknown("object Outer { private object Hidden }; def pick(): Outer.Hidden.type = ???")
      .and(
        unknown(
          "object Outer { private object Hidden; type Leak = Hidden.type }; " +
            "def pick(): Outer.Leak = ???"
        )
      )

  def negative =
    unknown("var x: Boolean = true\ndef pick(): x.type = x")
      .and(unknown("def pick(): missing.type = ???"))
      .and(
        unknown(
          "def outer[A](x: A): A = { def pick(): later.type = ???; val later = x; x }",
          "outer.pick"
        )
      )
      .and(
        unknown(
          "def outer(): Unit = { def pick(): Later.type = ???; object Later; () }",
          "outer.pick"
        )
      )

}
