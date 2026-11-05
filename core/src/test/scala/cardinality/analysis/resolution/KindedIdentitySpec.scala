package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Identities the higher-kinded reading changed (docs/research/higher-kinded-binders.md): an
  * override matched by binder kind and by the type its sugar spells, overloads apart by kind, and
  * constructor positions keyed by place. Every fixture compiles with Scala 3.9.0.
  */
class KindedIdentitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    binder renaming, function and tuple sugar keep an override    $overrideSugar
    overloads that differ only in a binder's kind stay apart      $kindOverloads
  """

  private def entries(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("Identity.scala", dialects.Scala3(code).parse[Source].get))
      )
      .map(entry => entry.name -> entry.count)
      .toMap

  // Each override implements its declaration, which therefore is no capability of C: `x`; the
  // functionals of four `Boolean => Boolean` values, 2^4; those of four pairs, 2^4.
  def overrideSugar = {
    val counts = entries("""
      trait P {
        def f[F[_]](x: F[Int]): F[Int]
        def g(h: Boolean => Boolean): Boolean
        def k(p: (Boolean, Boolean)): Boolean
      }
      final class C extends P {
        override def f[G[_]](x: G[Int]): G[Int] = x
        override def g(h: Function1[Boolean, Boolean]): Boolean = h(true)
        override def k(p: Tuple2[Boolean, Boolean]): Boolean = p._1
      }
    """)
    (counts("C.f") === Count.Finite(1))
      .and(counts("C.g") === Count.Finite(16))
      .and(counts("C.k") === Count.Finite(16))
  }

  // Each override implements only its own overload: the other stays an inherited capability whose
  // result instantiates its own binder (`K` for C3, `H` for C4), which the reading refuses.
  def kindOverloads = {
    val counts = entries("""
      import scala.annotation.targetName
      trait P2 {
        def m[H[_]](x: H[Int]): H[Int]
        @targetName("m2") def m[K[_, _]](x: K[Int, Int]): K[Int, Int]
      }
      abstract class C3 extends P2 { override def m[F[_]](x: F[Int]): F[Int] = x }
      abstract class C4 extends P2 {
        @targetName("m2") override def m[F[_, _]](x: F[Int, Int]): F[Int, Int] = x
      }
    """)
    def inherits(name: String, kept: String, overridden: String) =
      counts(name) must beLike {
        case Count.Unresolved(reasons) =>
          (reasons must contain(s"polymorphic capability binder: $kept"))
            .and(reasons.exists(_.contains(s"binder: $overridden")) must beFalse)
      }
    inherits("C3.m", "K (the result instantiates the binder)", "H")
      .and(inherits("C4.m", "H (the result instantiates the binder)", "K"))
  }

}
