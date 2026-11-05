package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** The binders a context-function result puts around a body (docs/research/syntax-features.md):
  * where Scala wraps, and what a search inside the wrapped body reaches. Fixtures are Scala 3.9.0
  * programs of the review round (a5-fix1 r1–r8, w1–w4), run where a value decides the count; each
  * expected number is derived beside it.
  */
class ContextBindersSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    An enclosing method's binders are anonymous evidence
      a deeper binder of the same type hides the outer one           $deeperBinder
      binders of one clause are ambiguous                            $oneClause
      a nested using, given or own binder hides the outer one        $deeperCandidate
    The chain is followed only where Scala wraps
      a projection or a refinement of one is refused                 $projection
      a union or an intersection is never wrapped                    $plain
      a chain past the reading's budget or arity is refused          $limits
    An overridden result is the one the subclass sees
      a parameter an extends clause substitutes is refused           $substituted
      several overridden declarations refuse the local definition    $severalOverridden
  """

  private def rows(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Binders.scala", dialects.Scala3(code).parse[Source].get)))
      .map(e => e.name -> e.count)
      .toMap

  private def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  private val Overlap = "anonymous evidence selection not resolved: context function binder A"

  // Fixture r1: `two(using 1)(using 2)` prints 2, the inner binder. g has 1 implementation, never
  // the 2 of both binders: the outer one is refused. three.g3 likewise (1, never 3). The methods
  // themselves name their binders in literals: two is `A ⇒ A ⇒ A` (2), three 3.
  def deeperBinder = {
    val found = rows("""object B {
      |  def two[A]: A ?=> A ?=> A = { def g: A = summon[A]; g }
      |  def three[A]: A ?=> A ?=> A ?=> A = { def g3: A = summon[A]; g3 }
      |}""".stripMargin)
    refused(found("B.two.g"), s"$Overlap (overlaps context function binder A)")
      .and(refused(found("B.three.g3"), Overlap))
      .and(found("B.two") === Count.Finite(2))
      .and(found("B.three") === Count.Finite(3))
  }

  // `(A, A) ?=> A`: a search for A inside is ambiguous (E172), so gp reads a alone (1), never 3.
  def oneClause =
    refused(
      rows("object B { def pair[A](a: A): (A, A) ?=> A = { def gp: A = a; gp } }")("B.pair.gp"),
      "ambiguous anonymous evidence: context function binder A"
    )

  // Fixture r1 prints 10 for each: u's own using, the body's given and k's own binder each hide
  // outerX's binder (true 2, 1, 2, and mid.g2 2), so it is refused, never counted (3, 2, 3, 3).
  def deeperCandidate = {
    val found = rows("""object B {
      |  def outerU[A](a: A): A ?=> A = { def u(using A): A = summon[A]; u(using a) }
      |  def outerG[A](a: A): A ?=> A = { given A = a; def w: A = summon[A]; w }
      |  def outerF[A](a: A): A ?=> A = { def k: A ?=> A = summon[A]; k(using a) }
      |  def show[A](a: A): A ?=> A = { def mid: A ?=> A = { def g2: A = summon[A]; g2 }; mid(using a) }
      |}""".stripMargin)
    refused(found("B.outerU.u"), s"$Overlap (overlaps A)")
      .and(refused(found("B.outerG.w"), s"$Overlap (overlaps given A = a)"))
      .and(refused(found("B.outerF.k"), s"$Overlap (overlaps context function binder A)"))
      .and(refused(found("B.show.mid.g2"), Overlap))
  }

  // Fixtures w1 and r4: a projection and a refinement declaring `apply` wrap (`proj(using 6)`
  // prints 6), while a refinement of a type member does not; neither is followed here.
  def projection = {
    val found = rows("""trait H[A] { type T = A ?=> A }
      |object S {
      |  def gp[A](using A): H[A]#T = summon[A]
      |  def fp[A]: H[A]#T = { def l: A = summon[A]; l }
      |  def refined[A](a: A): (A ?=> A) { def apply(using x: A): A } = { def lr: A = a; lr }
      |}""".stripMargin)
    refused(found("S.gp"), "context function behind a type projection not read: H[A]#T")
      .and(refused(found("S.fp.l"), "behind a type projection not read: H[A]#T, in the result of"))
      .and(refused(found("S.refined.lr"), "context function behind a refinement not read"))
  }

  // Fixture w2: `K | K`, `K & K` and `(Int ?=> Int) | Nothing` are not wrapped (type mismatch), so
  // the local definitions read a alone (1), never a binder too (2).
  def plain = {
    val found = rows("""object S {
      |  def un[A](a: A): (A ?=> A) | Unit = { def lu: A = a; () }
      |  def meet[A](a: A): (A ?=> A) & (A ?=> A) = { def lm: A = a; ??? }
      |}""".stripMargin)
    (found("S.un.lu") === Count.Finite(1)).and(found("S.meet.lm") === Count.Finite(1))
  }

  // Seventy aliases deep the chain stops on its budget; an alias applied to fewer arguments than it
  // takes (not Scala, but a reading must not guess) is refused. A match type reducing to a type
  // lambda and a curried type parameter are applied to arguments the reading does not apply
  // (Scala 3.9.0 compiles both, fixtures a5-fix2 ap1, ap2): refused where the anonymous evidence
  // they might shadow is read.
  def limits = {
    val aliases = (0 until 70).map(i => s"type T$i = T${i + 1}").mkString("\n")
    val found = rows(s"""$aliases
      |type T70 = Int ?=> Int
      |type P2[A, B] = A ?=> B
      |type M[X] = X match { case Int => [Y] =>> Y ?=> Int }
      |object O {
      |  def deep: T0 = { def l(b: Boolean): Boolean = b; ??? }
      |  def part: P2[Int] = { def lp(b: Boolean): Boolean = b; ??? }
      |  def leftOver(using Boolean): M[Int][Boolean] = { def lo(b: Boolean): Boolean = b; ??? }
      |  def curriedParam[G <: [X] =>> [Y] =>> Any](using Int): G[Int][Boolean] = ???
      |}""".stripMargin)
    refused(found("O.deep.l"), "context function chain budget exhausted")
      .and(refused(found("O.part.lp"), "context function behind an alias applied partially"))
      .and(
        refused(
          found("O.leftOver"),
          "context function behind an applied type not read: M[Int][Boolean]"
        )
      )
      .and(
        refused(
          found("O.curriedParam"),
          "context function behind an applied type parameter not read: G[Int][Boolean]"
        )
      )
  }

  // Fixture r2: `F5[Int]().run3(using 2)` prints 2 under a given 1, so F5.run3 is 1, never 2; the
  // `extends` argument `X ?=> X` reaches this reading as a function shape, which has lost `?=>`:
  // refused, for the target and for F3's local gf3 (true 1, the reading gave 0). A substituted
  // `X` wraps nothing: G3.run3.g reads x alone (1).
  def substituted = {
    val found = rows("""trait P3[X, Y] { def run3: Y }
      |final class F3[X] extends P3[X, X ?=> X] { def run3 = { def gf3: X = summon[X]; gf3 } }
      |final class F5[X](using X) extends P3[X, X ?=> X] { def run3 = summon[X] }
      |final class G3[X](x: X) extends P3[X, X] { def run3 = { def g: X = x; g } }
      |""".stripMargin)
    val behind = "context function behind a substituted type parameter not read: Y"
    refused(found("F5.run3"), behind)
      .and(refused(found("F3.run3.gf3"), s"$behind, in the result of enclosing run3"))
      .and(found("G3.run3.g") === Count.Finite(1))
  }

  // Both declarations match `g`: Scala intersects their results, which this reading does not.
  def severalOverridden =
    refused(
      rows("""trait Two[A] { def g: A ?=> A }
        |trait Other[A] { def g: A }
        |final class Both[A](a: A) extends Two[A], Other[A ?=> A] { def g = { def lb: A = a; lb } }
        |""".stripMargin)("Both.g.lb"),
      "overriding member g of several declarations, in the result of enclosing g"
    )

}
