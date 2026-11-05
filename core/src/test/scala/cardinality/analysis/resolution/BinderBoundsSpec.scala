package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Subtype bounds read together (docs/research/higher-kinded-binders.md, R5): a view's fields are
  * read through their own bounds, bounds that refer back to their binder are refused — as invalid
  * programs where Scala 3.9.0 rejects them (E140), as fixpoints the shapes do not decide where it
  * accepts them — `AnyRef`, `AnyVal` and `Matchable` above a structural lower bound are checked
  * against the bound's class, and the refusals of interactions the solver does not count. Every
  * fixture marked valid compiles with 3.9.0; the invalid ones were checked to fail.
  */
class BinderBoundsSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Views of views
    Bounds that refer back
      an F-bound through a structure is refused                     $recursiveBounds
    Class bounds
      a class bound reached through a bound or a proof              $reachedClassBounds
    Interactions
      a Unit goal is not refused for what it never reads            $unitGoal
  """

  private def entries(code: String, path: String = "Bounds.scala"): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refused(counts: Map[String, Count], name: String, reason: String) =
    counts(name) must beLike {
      case Count.Unresolved(reasons) => reasons must contain(reason)
    }

  private val flag =
    "sealed trait Flag\ncase object On extends Flag\ncase object Off extends Flag\n"

  private val products = "final case class P[X](x: X)\nfinal case class P2[X](x: X, n: Unit)\n"

  // Every A holds a smaller A: for an invariant final P no value exists (the context is vacuous,
  // exact 1), `A <: Option[A]` admits None. Before: 2, 4, 2^32. The order-swapped chain is legal
  // Scala (`[A <: B, B <: Option[A]]` is E046 in 3.9.0, order-dependent).
  def recursiveBounds = {
    val counts = entries(products + """
      object R {
        def fbound[A <: P[A]](a: A): A = a.x
        def toBool[A <: P[A]](a: A, n: Boolean): Boolean = n
        def toInt[A <: P[A]](a: A): Int = 0
        def mutual[A <: P[B], B <: P[A]](a: A): A = a.x.x
        def option[A <: Option[A]](a: A): A = a
        def chain[B <: Option[A], A <: B](a: A): A = a
      }
    """)
    def recursive(name: String, bound: String) =
      refused(counts, name, s"recursive bound not read: $bound")
    recursive("R.fbound", "A <: P[A]")
      .and(recursive("R.toBool", "A <: P[A]"))
      .and(recursive("R.toInt", "A <: P[A]"))
      .and(recursive("R.mutual", "B <: P[A]"))
      .and(recursive("R.option", "A <: Option[A]"))
      .and(recursive("R.chain", "B <: Option[A]"))
  }

  private val classReason = "structural lower bound under a class bound requires class analysis: "

  // B >: Flag lies below A <: AnyVal through B's own bound or through a proof: no type fits.
  def reachedClassBounds = {
    val counts = entries(flag + """
      object K {
        def cross[A <: AnyVal, B >: Flag <: A](x: B): A = x
        def viaEv[A <: AnyVal, B >: Flag](x: B)(using ev: B <:< A): A = ev(x)
      }
    """)
    refused(counts, "K.cross", classReason + "A <: AnyVal")
      .and(refused(counts, "K.viaEv", classReason + "A <: AnyVal"))
  }

  // One construction whatever the context holds (before: the callable refused it).
  def unitGoal = {
    val counts = entries(flag + """
      object U {
        def callable[A <: B, B](g: B => A): Unit = ()
        def shared[A <: Flag](a: A, b: A): Unit = ()
      }
    """)
    (counts("U.callable") === Count.Finite(1)).and(counts("U.shared") === Count.Finite(1))
  }

}
