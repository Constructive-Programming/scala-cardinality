package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Identities the higher-kinded reading changed (docs/research/higher-kinded-binders.md): an
  * override matched by binder kind and by the type its sugar spells, overloads apart by kind,
  * constructor positions keyed by place, and stdlib-model entries keyed by declaration. Every
  * fixture compiles with Scala 3.9.0.
  */
class KindedIdentitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    anonymous constructor positions are keyed by place            $anonymousPositions
    binder renaming, function and tuple sugar keep an override    $overrideSugar
    overloads that differ only in a binder's kind stay apart      $kindOverloads
    stdlib-model entries are keys, one per declaration            $modelKeys
  """

  private def entries(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("Identity.scala", dialects.Scala3(code).parse[Source].get))
      )
      .map(entry => entry.name -> entry.count)
      .toMap

  // Anonymous parameters all have the empty name, so a position is keyed by its place: the order of
  // `A` and `Boolean`, or naming them, changes nothing. The Boolean position is its space, 2; each
  // `A` slot is one of the two A values (the field or the evidence): 2 · 2 · 2.
  def anonymousPositions = {
    val counts = entries("""
      final class K4[A](a: A)(using A, Boolean)
      final class K5[A](a: A)(using Boolean, A)
      final class K6[A](a: A)(using x: A, y: Boolean)
    """)
    (counts("K4.<init>") === Count.Finite(8))
      .and(counts("K5.<init>") === Count.Finite(8))
      .and(counts("K6.<init>") === Count.Finite(8))
  }

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

  // Each override implements only its own overload: the other stays an inherited capability, named
  // by its own binder (`K` for C3, `H` for C4).
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
          (reasons must contain(s"polymorphic capability binder: $kept (higher-kinded)"))
            .and(reasons.exists(_.contains(s"binder: $overridden")) must beFalse)
      }
    inherits("C3.m", "K[_, _]", "H").and(inherits("C4.m", "H[_]", "K"))
  }

  private def refused(found: Count, fragment: String) =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  // A rigid `F` tells its arguments apart by the declarations they name (hkt R2), every spelling of a
  // model entry one key: `List` and `scala.collection.immutable.List` (also through a named
  // import) are one type, `Option[Int]` and `Either[Unit, Int]` two, though both are `1 + Int`. An
  // excluded or unmodelled entry's parameters are not held (`scala.quoted.Type` takes any kind), so
  // its key is refused with the entry's reason; a throwable is a read entry, its own key (x).
  def modelKeys = {
    val code = """object K {
      |  def list[F[_]](x: F[List[Int]]): F[scala.collection.immutable.List[Int]] = x
      |  def seq[F[_]](x: F[Seq[Int]]): F[scala.collection.immutable.Seq[Int]] = x
      |  def sum[F[_]](x: F[Option[Int]]): F[Either[Unit, Int]] = ???
      |  def map[F[_]](x: F[Map[Int, Int]]): F[Map[Int, Int]] = x
      |  def throwable[F[_]](x: F[Throwable]): F[Throwable] = x
      |}""".stripMargin
    val imported = entries(
      "import scala.collection.immutable.List\nobject I { def l[F[_]](x: F[List[Int]]): F[List[Int]] = x }"
    )("I.l")
    val counts = entries(code)
    (counts("K.list") === Count.Finite(1))
      .and(counts("K.seq") === Count.Finite(1))
      .and(counts("K.sum") === Count.Finite(0))
      .and(
        refused(counts("K.map"), "outside the counting model: scala.collection.immutable.Map")
      )
      .and(counts("K.throwable") === Count.Finite(1))
      .and(imported === Count.Finite(1))
  }

}
