package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Rule X: a `transparent inline` method's call site types the expansion, so a declared result is
  * only an upper bound of what callers observe — only a declared singleton type leaves nothing
  * narrower (docs/research/top-types-unions.md, register D27). An `inline` method that is not
  * transparent counts like a `def` when its body passes (InlineMethodsSpec).
  */
class TransparentInlineSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    A transparent result is what the expansion returns
      a transparent result of an open class may be a subclass        $transparentSubclass
      a closed transparent result is still narrowed at the call site $transparentClosed
      a transparent function result is refused too                   $transparentFunction
  """

  private def rows(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Top.scala", dialects.Scala3(code).parse[Source].get)))
      .map(e => e.name -> e.count)
      .toMap

  private def count(code: String, name: String): Count = rows(code)(name)

  private def reasonContains(code: String, name: String, fragments: String*) =
    count(code, name) must beLike {
      case Count.Unresolved(reasons) =>
        fragments.forall(f => reasons.exists(_.contains(f))) must beTrue
    }

  // Scala 3.9.0 reads `mk(true).n` at the call site: `new Sub(true)` and `new Sub(false)` are
  // distinct implementations the declared Foo cannot tell apart.
  def transparentSubclass = {
    val code = "import ext.Schema\ncase class Foo()\nclass Sub(val n: Boolean) extends Foo()\n" +
      "object M { transparent inline def mk(b: Boolean): Foo = new Sub(b)\n" +
      "  transparent inline def mk2(s: Schema): Foo = Foo()\n" +
      "  transparent inline def fn(b: Boolean): Boolean => Boolean = x => x }"
    reasonContains(code, "M.mk", "not the declared Foo")
      .and(reasonContains(code, "M.mk2", "not the declared Foo"))
      .and(reasonContains(code, "M.fn", "not the declared Boolean => Boolean"))
  }

  // A closed result's values are what the methods compute, but not what callers observe: `s` with
  // body `S2` is typed `S2.type` at the call site and `(S2: S)` is typed `S` (fixture review-A6 h13
  // picks a different overload for each), `None` is `None.type`, and a body may return a stable
  // path of type `Fin` as its singleton type (fixture a6-fix1 t1). Only a declared singleton type
  // counts (rule X); `inline b match` is refused for its own reason.
  def transparentClosed = {
    val code =
      "final case class Fin()\nsealed trait S\nfinal case class S1(b: Boolean) extends S\n" +
        "case object S2 extends S\nobject T { transparent inline def s(b: Boolean): S = S2\n" +
        "  transparent inline def o(b: Boolean): Option[Fin] = None\n" +
        "  transparent inline def f(s: ext.Schema): Fin = Fin()\n" +
        "  transparent inline def m(b: Boolean): S = inline b match { case true => S2; case _ => S1(b) } }"
    val narrowed = "transparent inline result: the call site observes the expansion's type"
    reasonContains(code, "T.s", narrowed, "not the declared S")
      .and(reasonContains(code, "T.o", narrowed))
      .and(reasonContains(code, "T.f", narrowed))
      .and(reasonContains(code, "T.m", "inline method: uses inline match"))
  }

  // An expansion may be a function class with members of its own.
  def transparentFunction =
    reasonContains(
      "import ext.Schema\ntransparent inline def tf(s: Schema): Int => Unit = _ => ()",
      "tf",
      "declaration not supplied: ext.Schema (named import Schema)"
    ).and(
      reasonContains(
        "import ext.Schema\ntransparent inline def t(s: Schema): Unit = ()",
        "t",
        "not the declared Unit"
      )
    )

}
