package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** Types selected on paths and refined, in the compiled front end: an alias as its prefix sees it,
  * an abstract member of a parameter as that path's own atom (members M0), a refinement of type
  * members over a type whose values nothing tells apart, and the polymorphic identity. Every
  * expected count is derived in its example's comment.
  */
class PathTypesSpec extends Specification {

  def is = s2"""
  Types on paths and refinements
    an alias is read as its prefix sees it                    $aliases
    an abstract member of a parameter is the path's own atom  $paths
    a member constructor, a stuck projection of a path        $kinds
    a refinement naming its own value                         $recursive
    a refinement of type members that can be satisfied        $refinements
    a refinement not shown satisfiable, or of a record        $unsatisfied
    a path of another method's parameter is not read          $otherPaths
    the polymorphic identity function type is one value       $identity
  """

  private val text =
    """package pt
      |
      |trait R { type X }
      |trait Mk { type X; def mk: X }
      |trait T { type X = Int }
      |trait P[A] { type Y = (A, A); def get: Y }
      |trait Q[B] extends P[B] { def other: B = ??? }
      |trait BaseG[X] { type T = X }
      |object OG extends BaseG[Boolean]
      |final case class Dep(p: R, x: p.X, y: p.X)
      |final class Concrete { type X = Int }
      |trait T2 { type X; type Y }
      |type Rf[A, B] = { type X = Y; type Y = A; type Z = B }
      |trait H { type F[_] }
      |trait Hc { type F[+_] }
      |type Fst[T] = T match { case (f, s) => f }
      |type Snd[T] = T match { case (f, s) => s }
      |object M {
      |  def g(p: R, x: p.X, y: p.X): p.X = x
      |  def f(p: R, q: R, x: p.X): q.X = ???
      |  def h(p: R)(q: p.type)(x: q.X, y: p.X): q.X = x
      |  def t(p: { type X }, x: p.X, y: p.X): p.X = x
      |  def k(p: Mk): p.X = p.mk
      |  def r[A](p: R { type X = A }, a: A, x: p.X): A = a
      |  def upper(p: R { type X <: Tuple }, b: Boolean): Boolean = b
      |  def conflict(p: T { type X = String }, b: Boolean): Boolean = b
      |  def record(p: Mk { type X = Int }, b: Boolean): Boolean = b
      |  def seen(t: OG.T): Boolean = t
      |  def u[A](id: [a] => a => a): A = ???
      |  def v[A](id: [a] => a => a, x: A, y: A): A = id(x)
      |  def w[A](id: [a] => (a, a) => a, x: A): A = x
      |  def two(a: Dep, b: Dep): Dep = a
      |  def onClass(p: Concrete { type X = Int }, b: Boolean): Boolean = b
      |  def both(p: R { type X >: Int <: AnyVal }, b: Boolean): Boolean = b
      |  def kind(p: H, x: p.F[Int], y: p.F[Boolean]): p.F[Int] = x
      |  def kinds(p: H, q: H, x: p.F[Int]): q.F[Int] = ???
      |  def covariant(p: Hc, x: p.F[Int]): p.F[Int] = x
      |  def fst(p: R, x: Fst[p.X], y: Snd[p.X]): Fst[p.X] = x
      |  def fsts(p: R, q: R, x: Fst[p.X]): Fst[q.X] = ???
      |  def self(p: T2 { type X = this.Y }, x: p.X, y: p.X): p.X = x
      |  def selfY(p: T2 { type X = this.Y }, x: p.X, b: Boolean): Boolean = b
      |  def sibling[A, B](x: Rf[A, B]#X, y: A, z: Rf[A, B]#Z): A = y
      |}
      |""".stripMargin

  private lazy val rows = TastyFrontEndSpec.rows("Paths.scala" -> text)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name == s"pt.$name")
      .get
      .count

  // `Q`'s inherited `get: Y` is `(B, B)` as `Q` sees it: `other` has `get._1` or `get._2`, 2.
  // `OG.T` is `BaseG[Boolean]`'s alias at `Boolean`: `Boolean => Boolean`, 4.
  private def aliases =
    List("Q.other", "M.seen").map(count) must_== List(Count.Finite(2), Count.Finite(4))

  // `p.X` is an atom of the path p: `g` has x or y, 2; `f` has no `q.X`, 0; `q: p.type` is p's
  // path, so `h` has x or y, 2; `t` over a structural `{ type X }`, 2. A path whose type holds
  // members that may supply X (`Mk.mk`) is not read yet.
  private def paths =
    (List("M.g", "M.f", "M.h", "M.t").map(count) must_== List(
      Count.Finite(2),
      Count.Finite(0),
      Count.Finite(2),
      Count.Finite(2)
    )).and(count("M.k").toString must contain("a type: p.X"))

  // In `T2 { type X = this.Y }`, `this` is the refinement's own value: `p.X` is `p.Y`, an atom of
  // the path. `self` has x or y, 2; `selfY` is `Boolean => Boolean` beside an atom, 4.
  // `Rf[A, B]#X` is its sibling `Y`, which is A: `sibling` has x or y, 2.
  private def recursive =
    List("M.self", "M.selfY", "M.sibling").map(count) must_== List(
      Count.Finite(2),
      Count.Finite(4),
      Count.Finite(2)
    )

  // A member of kind `F[_]` of a path is the path's rigid constructor: `kind` has x alone (`y` is
  // `p.F[Boolean]`), 1; `kinds` has no `q.F[Int]`, 0. One declared `F[+_]` relates its
  // applications by subtyping: not read. A stuck projection of a path's atom is an atom too:
  // `fst` has x alone (y is the other component), 1; `fsts` has no `Fst[q.X]`, 0.
  private def kinds =
    (List("M.kind", "M.kinds", "M.fst", "M.fsts").map(count) must_== List(
      Count.Finite(1),
      Count.Finite(0),
      Count.Finite(1),
      Count.Finite(0)
    )).and(count("M.covariant").toString must contain("a type: p.F"))

  // `R { type X = A }` is one value, and `p.X` is A: `r` has a or x, 2. `R { type X <: Tuple }`
  // is one value: `upper` is `Boolean => Boolean`, 4.
  private def refinements =
    List("M.r", "M.upper").map(count) must_== List(Count.Finite(2), Count.Finite(4))

  // `T` fixes X to `Int`, so `T { type X = String }` is not shown satisfiable; nor a refinement
  // of a concrete class, or one bounding the member on both sides. `Mk { type X = Int }` changes
  // what `mk` supplies. None is read.
  private def unsatisfied =
    List("M.conflict", "M.record", "M.onClass", "M.both")
      .map(count(_).toString)
      .must(contain(contain("a type:")).forall)

  // `Dep`'s fields name its constructor's `p`, one path per instance: `two` may not mix `a.x`
  // into `b`, so the field is not read as one atom shared by every `Dep`.
  private def otherPaths = count("M.two").toString must contain("a type:")

  // `[a] => a => a` has one implementation, the identity: `u` has no A, 0; `v` has x or y, 2. A
  // polymorphic function of another form is not read.
  private def identity =
    (List("M.u", "M.v").map(count) must_== List(Count.Finite(0), Count.Finite(2)))
      .and(count("M.w").toString must contain("a type:"))

}
