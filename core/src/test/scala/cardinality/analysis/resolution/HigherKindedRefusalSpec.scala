package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** What the higher-kinded reading does not count (docs/research/higher-kinded-binders.md): invalid
  * programs — Scala 3.9.0 rejects each fixture marked so — and valid ones whose identity or kind it
  * cannot establish, each with the reason naming it. Every valid fixture compiles with 3.9.0.
  */
class HigherKindedRefusalSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Invalid programs
      a wrong arity is not counted                                  $arity
      a builtin applied at a wrong arity says so in both readings   $builtinArity
      a kind mismatch is not counted                                $kindMismatch
      a constructor argument of another kind is not counted         $argumentKinds
      a higher-kinded name read as a type                           $properUse
      a wildcard under a rigid binder is not counted                $wildcard
      a view bound is not Scala 3                                   $viewBound
    Unreadable identities
      a wildcard where a constructor is expected                    $wildConstructor
      a nested wildcard has no key                                  $nestedWildcard
      distinct union alternatives have no key                       $unionKeys
      variance on a rigid hole                                      $variant
      a bounded higher-kinded binder                                $boundedHk
      abstract members, paths and inherited members key as members  $members
      a structural bound beside a proof                             $coercionBound
      a higher-kinded name or a lambda where a type is expected     $constructorsAsTypes
      java.lang's boxed names under its wildcard import             $boxedNames
  """

  private def entries(code: String, path: String = "Refusals.scala"): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def count(code: String, name: String): Count = entries(code)(name)

  private def unresolved(code: String, name: String, fragment: String) =
    count(code, name) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(fragment)) must beTrue
    }

  private def refused(counts: Map[String, Count], name: String, reason: String) =
    counts(name) must beLike {
      case Count.Unresolved(reasons) => reasons must contain(reason)
    }

  private val box = "case class Box[F[_], A](value: F[A])\n"

  def arity =
    unresolved("object T { def f[F[_], A](x: F[A, A]): A = ??? }", "T.f", "type argument arity: F")

  def kindMismatch =
    unresolved(
      "case class Bi[F[_, _], A](value: F[A, A])\nobject T { def f[G[_], A](x: Bi[G, A]): A = ??? }",
      "T.f",
      "kind mismatch: G has kind (*) → *, expected (*, *) → *"
    )

  // `Either[A]` is no type: the shape reading said `unresolved type: Either`, the key reading
  // `type argument arity: Either`. Both say the arity now.
  def builtinArity = {
    val counts = entries("""
      object T {
        def direct[A](x: Either[A]): A = ???
        def keyed[F[_], A](x: F[Either[A]]): A = ???
        def ground[A](x: Int[A]): A = ???
      }
    """)
    refused(counts, "T.direct", "type argument arity: Either")
      .and(refused(counts, "T.keyed", "type argument arity: Either"))
      .and(refused(counts, "T.ground", "type argument arity: Int"))
  }

  // 3.9.0: "Type argument Option does not have the same kind as its bound [_$1,_$2]", whether or
  // not the parameter is ever applied. Before: `Phantom[Option]` counted 1, `Box[Either, A]` was
  // `unresolved type: Either`.
  def argumentKinds = {
    val counts = entries(box + """
      final case class Phantom[F[_, _]]()
      object T {
        def make: Phantom[Option] = ???
        def make2: Phantom[Int] = ???
        def box[A](a: A): Box[Either, A] = ???
        def box2[A](a: A): Box[Int, A] = ???
        def lambda[A](a: A): Box[[x, y] =>> x, A] = ???
        def fits[A](a: A): Box[Option, A] = ???
      }
    """)
    refused(counts, "T.make", "kind mismatch: Option has kind (*) → *, expected (*, *) → *")
      .and(refused(counts, "T.make2", "kind mismatch: Int has kind *, expected (*, *) → *"))
      .and(refused(counts, "T.box", "kind mismatch: Either has kind (*, *) → *, expected (*) → *"))
      .and(refused(counts, "T.box2", "kind mismatch: Int has kind *, expected (*) → *"))
      .and(
        refused(
          counts,
          "T.lambda",
          "kind mismatch: [x, y] =>> x has kind (*, *) → *, expected (*) → *"
        )
      )
      .and(counts("T.fits") === Count.Finite(2))
  }

  def properUse =
    unresolved(
      "object T { def f[F[_]](x: F): Int = ??? }",
      "T.f",
      "higher-kinded parameter used as a proper type: F"
    )

  // Scala 3.9.0 rejects an abstract constructor applied to a wildcard (E043), for a method's binder
  // and a class's alike. Before: counted 1.
  def wildcard = {
    val counts = entries("""
      object T { def w[F[_], A](x: F[?]): Option[A] = None }
      class C[F[_]] { def w[A](x: F[?]): Option[A] = None }
    """)
    def invalid(name: String) =
      counts(name) must beLike {
        case Count.Unresolved(reasons) =>
          reasons must contain("unreducible application of a higher-kinded binder to a wildcard: F")
      }
    invalid("T.w").and(invalid("C.w"))
  }

  // 3.9.0: "view bounds `<%' are no longer supported", used or not.
  def viewBound = {
    val counts = entries("""
      object V {
        def f[A <% Int](a: A): A = a
        def unused[A <% Int](n: Boolean): Boolean = n
      }
    """)
    val reason = "view bound is not Scala 3 (an invalid program): A <% Int"
    refused(counts, "V.f", reason).and(refused(counts, "V.unused", reason))
  }

  def wildConstructor =
    unresolved(
      box + "object T { def f[A](x: Box[?, A]): A = ??? }",
      "T.f",
      "existential type constructor requires witness-kind analysis"
    )

  def nestedWildcard =
    unresolved(
      "object T { def f[F[_]](x: F[Option[?]]): F[Option[?]] = ??? }",
      "T.f",
      "nominal identity of argument not established: ?"
    )

  def unionKeys =
    unresolved(
      "object T { def f[F[_], A, B](x: F[A | B]): F[A] = ??? }",
      "T.f",
      "nominal identity of argument not established: A | B"
    )

  // Exact count 1 once covariant views exist; the invariant twin is 0.
  def variant =
    unresolved(
      "object T { def f[F[+_], A](x: F[Nothing]): F[A] = x }",
      "T.f",
      "variant higher-kinded parameter"
    )

  def boundedHk = {
    val counts = entries("""
      object T {
        def f[CC[X] <: Seq[X], A](x: CC[A]): CC[A] = x
        def unused[CC[X] <: Seq[X], A](x: A): A = x
      }
    """)
    (counts("T.f") must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("bounded higher-kinded parameter")) must beTrue
    }).and(counts("T.unused") === Count.Finite(1))
  }

  // The members cluster's (B2): a key would need the member's owner or prefix.
  def members = {
    val counts = entries("""
      trait M { type X; def g[F[_]](x: F[X]): F[Int] }
      final class P { type T = Int }
      object Q { def f[F[_]](p: P)(x: F[p.T]): F[Int] = ??? }
      trait Base { type T = Int }
      trait D extends Base { def h[F[_]](x: F[T]): F[Int] }
    """)
    // Type members now key as their paths' (members M1/M2): `M.this.X` is an atom, so F[X] and
    // F[Int] are two keys and g has no F[Int] — 0, as for a class parameter X; `p.T` and the
    // inherited `T` are the alias Int, so F[Int] is x — 1 each.
    (counts("M.g") === Count.Finite(0))
      .and(counts("Q.f") === Count.Finite(1))
      .and(counts("D.h") === Count.Finite(1))
  }

  def coercionBound =
    unresolved(
      "object R { def c[A <: (B =:= C), B, C](a: A): B = ??? }",
      "R.c",
      "bound to a structural type requires coercion analysis: A <: (B =:= C)"
    )

  def constructorsAsTypes = {
    val counts = entries("""
      object S { def k[F[_], A](x: F[A]): F[F] = ??? }
      object L { def l[A](x: [y] =>> y): A = ??? }
    """)
    refused(counts, "S.k", "higher-kinded parameter used as a proper type: F")
      .and(refused(counts, "L.l", "unapplied type lambda used as a proper type"))
  }

  // Under `import java.lang.*`, `Boolean` is java.lang.Boolean (3.9.0 accepts `val b: Boolean =
  // null` there and rejects `F[Boolean]` as an `F[scala.Boolean]`): exact count 0, not the 1 the
  // two scala keys gave. A wildcard of a namespace the sources do not supply and the stdlib model
  // does not list may bind any builtin name (D16), so `Int` is refused too (true 1: java.lang has
  // no `Int`, but the model does not list its members). A renaming import binds no bare name.
  def boxedNames = {
    val counts = entries("""
      package boxed {
        import java.lang.*
        object T {
          def plain(x: Boolean): Boolean = x
          def boo[F[_]](x: F[Boolean]): F[scala.Boolean] = ???
          def lng[F[_]](x: F[Long]): F[scala.Long] = ???
          def int[F[_]](x: F[Int]): F[scala.Int] = x
        }
      }
      package rooted {
        import _root_.java.lang.*
        object T { def dbl[F[_], A](x: F[(Double, A)]): F[(scala.Double, A)] = ??? }
      }
    """)
    val renamed = count(
      "import java.lang.{Boolean => JB}\nobject T { def boo[F[_]](x: F[Boolean]): F[scala.Boolean] = x }",
      "T.boo"
    )
    def boxed(name: String, builtin: String, prefix: String) =
      refused(
        counts,
        name,
        s"builtin name $builtin may be shadowed through a reference not supplied: $prefix " +
          s"(wildcard import of $prefix)"
      )
    boxed("boxed.T.plain", "Boolean", "java.lang")
      .and(boxed("boxed.T.boo", "Boolean", "java.lang"))
      .and(boxed("boxed.T.lng", "Long", "java.lang"))
      .and(boxed("boxed.T.int", "Int", "java.lang"))
      .and(boxed("rooted.T.dbl", "Double", "_root_.java.lang"))
      .and(renamed === Count.Finite(1))
  }

}
