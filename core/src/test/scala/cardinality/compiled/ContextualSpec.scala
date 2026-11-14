package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** Slice S5 of the compiled front end (docs/plans/compiled-own-types.md): contextual scope. A body
  * is counted, not searched, so what is contextual counts as the plain definition it is, where
  * implicit search has nothing to choose. Every expected count is derived in its example's comment,
  * and the scalameta front end is asked for the same rows.
  */
class ContextualSpec extends Specification {

  def is = s2"""
  Contextual definitions count as the definitions they are
    a named using or implicit parameter is a value like any other $parameters
    a given alias and an implicit definition are methods          $givens
    a given written `= deferred` is what each subclass supplies   $deferred
    an extension method counts over all its clauses               $extensions
    an anonymous parameter no other candidate competes with       $anonymous
    an anonymous parameter around a definition or told apart      $captured
    what implicit search would choose is not read yet             $later
  """

  private val text =
    """package ctx
      |
      |import scala.compiletime.deferred
      |
      |object Params {
      |  def pick[A](x: A)(using o: Option[A]): A = x
      |  def imp(x: Boolean)(implicit y: Boolean): Boolean = x
      |  def anon[A](x: A)(using Option[A]): A = x
      |  def bound[A: List](x: A): A = x
      |  def ctxRes[A](a: A): A ?=> A = a
      |  def ctxIn[A](f: A ?=> A, a: A): A = a
      |  def twin[A](x: A)(using Show, Pick[A]): A = x
      |  def same[A](x: A)(using Option[A], Option[A]): A = x
      |  def local[A](x: A)(using Pick[A]): A = { given Show = null; x }
      |  def near[A](x: A)(using Option[A]): A = { given Some[A] = Some(x); x }
      |  def wide[A](x: A)(using Option[A], List[A]): A = x
      |  def nul(x: Show)(using Show)(using n: Null): Show = x
      |  def ctx[A](x: A)(using Pick[A]): A ?=> A = x
      |  def both[A, B](x: A)(using Pick[A], Pick[B]): A = x
      |}
      |final class Held(a: Boolean)(using b: Boolean) { def get: Boolean = a }
      |object Givens {
      |  given named: Boolean = true
      |  given pair[A](using a: A): (A, A) = (a, a)
      |  implicit val flag: Boolean = true
      |  implicit def conv(b: Boolean): Unit = ()
      |  def other: Boolean = true
      |}
      |trait Later { given g: Boolean = deferred; def other: Boolean = g }
      |object Ext {
      |  extension (b: Boolean) def flip: Boolean = !b
      |  extension [A](x: A) def or(y: A): A = x
      |  extension [A](x: A)(using o: Option[A]) def choose: A = x
      |}
      |trait Show { def show(b: Boolean): Boolean }
      |trait Pick[A] { def pick(x: A, y: A): A }
      |object Inst { given shown: Show with { def show(b: Boolean): Boolean = b } }
      |trait Anon { given Boolean = deferred; def get: Boolean = true }
      |trait Fed { given feed(using b: Boolean): Boolean; def get: Boolean = true }
      |final class Flat[A](using Option[A]) { def f(x: A): A = x }
      |final class Vied[A](using Option[A]) { def g(x: A)(using Option[A]): A = x }
      |final class Outer[A](using Option[A]) {
      |  final class In { given Option[A] = None; def h(x: A): A = x }
      |}
      |final class Crowd[A](using Option[A]) { given Option[A] = None }
      |final class Blk[A](using Option[A]) { locally { given Option[A] = None; def pick(x: A): A = x } }
      |trait Base { given Option[Int] = None }
      |final class Selfish(using Option[Int]) { self: Base => }
      |object Shared { class s; given s: s = new s }
      |trait Two { given g: Boolean = deferred; given k: Option[Boolean] = Some(g) }
      |trait Exts { extension (b: Boolean) def tag: Boolean; def get: Boolean = true }
      |trait Get[A] { def get(s: Unit): A }
      |object Caps {
      |  def f[A]: A ?=> A = { def g: A = summon[A]; g }
      |  def mixed[A, B]: A ?=> B ?=> A = { def ga: A = summon[A]; ga }
      |  def outer[A](using A): Unit = { def inner[B](using B): A = ???; () }
      |  def held[A]: A ?=> Get[A] = new Get[A] { def get(s: Unit): A = summon[A] }
      |  def later[A](using A)(a2: A): A = { def inner2(): A = ???; given A = a2; inner2() }
      |  def shadowed[A](using A): Get[A] =
      |    new Get[A] { given Option[A] = None; def get(s: Unit): A = ??? }
      |  def inherits[A](using A): Get[A] = new Base with Get[A] { def get(s: Unit): A = ??? }
      |  def imports[A](using A): Unit = { import Givens.given; def m: A = ???; () }
      |  def projected[A](using A): H[A]#T = summon[A]
      |  def twice[A](using A): Unit = { def inner3(using A): A = ???; () }
      |}
      |object Apart {
      |  def lost[A, B](x: A)(using Option[A], B): A = x
      |  def both[A, B](using A, B): (A, B) = ???
      |  def u[A, B](using A): B ?=> A = summon[A]
      |  def useCb[A](c: CB, a: A): A = a
      |}
      |trait Le[M] { def le(x: M, y: M): Boolean }
      |trait H[A] { type T = A ?=> A }
      |trait CB { def fold[M: Le](m: M): M }
      |final class K4[A](a: A)(using A, Boolean)
      |final class Pv(val b: Boolean)
      |final class Rv(val b: Boolean)
      |final class Qv(val b: Boolean)
      |final class Sec(using Pv, Rv) { def this()(using Qv, Qv) = this(using Pv(true), Rv(true)) }
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Contextual.scala" -> text)
  private lazy val rows = TastyFrontEnd.rows(input)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name.endsWith(name))
      .get
      .count

  // `pick` returns `x` or, where `o` holds one, its A: an `Option[A]` is `None` or `Some`, and the
  // choice is free only at `Some`: 1 × 2 = 2. `imp` is a Boolean computed from two Booleans:
  // 2^(2 × 2) = 16. A constructor's named `using` parameter is held like any other: `Held.get` is
  // 16 too.
  private def parameters =
    (count("Params.pick") must_== Count.Finite(2))
      .and(count("Params.imp") must_== Count.Finite(16))
      .and(count("Held.get") must_== Count.Finite(16))

  // A given alias is a value or method of its type: `named` is one of 2 Booleans, `pair` builds
  // the one pair of `a` with itself, `flag` is one of 2, and `conv` returns `Unit`, one value. A
  // concrete given supplies nothing to the members beside it: `other` is one of 2. A given
  // instance without parameters is an object: its `show` is one of the 2^2 functions. A context
  // function is a function: `ctxRes` returns `a` or the A it is given, 2, and `ctxIn` returns
  // `a`, `f(a)`, `f(f(a))`, …: ω.
  private def givens =
    (count("Givens.named") must_== Count.Finite(2))
      .and(count("Givens.pair") must_== Count.Finite(1))
      .and(count("Givens.flag") must_== Count.Finite(2))
      .and(count("Givens.conv") must_== Count.Finite(1))
      .and(count("Givens.other") must_== Count.Finite(2))
      .and(count("shown.show") must_== Count.Finite(4))
      .and(count("Params.ctxRes") must_== Count.Finite(2))
      .and(count("Params.ctxIn") must_== Count.Countable)

  // Each class extending `Later` gives its own `g`, so `other` is a Boolean computed from one:
  // 2^2. `g` itself is one of 2.
  // A class a given merely shares its name with keeps its constructor row. Beside a deferred
  // given `g`, the alias given `Two.k` is an `Option[Boolean]` (3 values) computed from `g`'s Boolean: 3^2.
  // An abstract given with a `using` clause is a function of it: `Fed.feed` is one of the 2^2
  // functions Boolean => Boolean, so `Fed.get`, a Boolean computed from it, is one of 2^4 = 16.
  // (The old front end reads `feed` as a Boolean and says 2^2 = 4, though it reads the same
  // member written `implicit def` as the function: a correction.)
  private def deferred =
    (count("Later.other") must_== Count.Finite(4))
      .and(count("Later.g") must_== Count.Finite(2))
      .and(count("Shared.s.<init>") must_== Count.Finite(1))
      .and(count("Two.k") must_== Count.Finite(9))
      .and(count("Fed.get") must_== Count.Finite(16))

  // An extension method is a method of all its clauses: `flip` is one of the 2^2 functions, `or`
  // returns `x` or `y`, and `choose` returns `x` or the A its `Option[A]` may hold, 2. A class's
  // abstract extension method is a method of its instance, which a body in the class may call on
  // any Boolean: `Exts.get` is a Boolean computed from `tag`, one of the 2^2 functions, 2^4 = 16.
  // (The old front end leaves `tag` out and says 2: a correction.)
  private def extensions =
    (count(".flip") must_== Count.Finite(4))
      .and(count(".or") must_== Count.Finite(2))
      .and(count(".choose") must_== Count.Finite(2))
      .and(count("Exts.get") must_== Count.Finite(16))

  // An anonymous parameter no other candidate competes with is the value search finds: `anon`
  // returns `x` or what its `Option[A]` may hold, 2, as the named `pick`; `bound`'s `List[A]` holds any
  // number of As: ω. `twin`: a `Show` and a `Pick[A]`, traits of the module neither of which
  // extends the other, so no search finds both; `pick` nests without end (`pick(x, pick(x,
  // x))`, …): ω.
  // A body is counted, not read: the given `local` and `near` declare in theirs competes with no
  // other body, `local` is ω as `Pick[A]` nests, and `near` 2, as `anon`. Two of one type at one
  // level are ambiguous (`same`); an `Option` beside a `List`, classes of the standard library
  // whose bases the old reading does not know, is not read yet (`wide`), nor a `Show` beside a
  // `Null`, which is below every reference type (`nul`). A context-function result's binder is
  // one more candidate, deeper: `ctx`'s `A` is no `Pick[A]` (a rigid parameter relates to no class
  // type but the tops and bottoms), so the `Pick[A]` is found, and its `pick` nests: ω.
  // `both`'s `Pick[A]` and `Pick[B]` are never subtypes of each other (A and B are distinct and
  // unbounded, rigid in the body), so each is the one search finds: `pick` nests, ω. A
  // constructor's anonymous parameter is read by the same rule over its own parameters: its body
  // is the class's, which another implementation need not share, and no instance exists yet whose
  // members, parents or self type could offer a given. `Flat`'s and `Crowd`'s `Option[A]` is a
  // position over A, built from the supplied one or `None`, 2; `Selfish`'s `Option[Int]` is
  // ground, 2^32 + 1. A receiver's is found from its members where its class's body declares
  // nothing contextual and every deeper contextual parameter is told apart from it: `Flat.f`
  // returns `x` or what the `Option[A]` may hold, 2.
  private def anonymous =
    (List(
      "Params.anon",
      "Params.bound",
      "Params.twin",
      "Params.local",
      "Params.near",
      "Params.both",
      "Params.ctx",
      "Flat.<init>",
      "Crowd.<init>",
      "Selfish.<init>",
      "Flat.f"
    ).map(count) must_== List(
      Count.Finite(2),
      Count.Countable,
      Count.Countable,
      Count.Countable,
      Count.Finite(2),
      Count.Countable,
      Count.Countable,
      Count.Finite(2),
      Count.Finite(2),
      Count.Finite(4294967297L),
      Count.Finite(2)
    ))
      .and(count("Params.same").toString must contain("ambiguous anonymous evidence"))
      // A secondary constructor's two `Qv`s are ambiguous too, whatever the primary's fields are.
      .and(
        rows.sources
          .flatMap(_.toOption.toList.flatMap(_.methods))
          .filter(_.name.endsWith("Sec.<init>"))
          .last
          .count
          .toString must contain("ambiguous anonymous evidence")
      )
      .and(
        List("Params.wide", "Params.nul")
          .map(count(_).toString)
          .must(contain(contain("an anonymous contextual parameter")).forall)
      )

  // An anonymous parameter of a method around a local definition, a context-function literal's
  // among them, is found from it where each contextual parameter at its method or deeper is told
  // apart from it and no scope between declares, imports or inherits a given: `g`, `ga`, `inner`
  // and `get` each return the one A the evidence holds, 1 (`inner`'s own `B` is no `A`). Told
  // apart, too: a rigid parameter from a class type (`lost` returns `x` or what the `Option[A]`
  // may hold, 2; `K4` holds `a` or the evidence at `a`, the evidence, and a Boolean: 2 × 2 × 2 =
  // 8) and from another (`both` builds its one pair, 1; `u`'s result binder `B` is no `A`, 1). A
  // capability's anonymous parameter is an argument its caller passes: `useCb` has no `Le[A]`
  // for `fold`, so `a` alone, 1. Not read: beside a block's given (`inner2`) or import (`m`), an
  // anonymous class's own or inherited given (`shadowed`, `inherits`), a deeper parameter of its
  // type (`inner3`), or the binder of its very type that a result behind a projection puts around
  // the body (`projected`: the compiler's function literal names it).
  private def captured =
    (List("Caps.f.g", "Caps.mixed.ga", "Caps.outer.inner", "<new#1>.get")
      .map(count) must_== List.fill(4)(Count.Finite(1)))
      .and(
        List("Apart.lost", "Apart.both", "Apart.u", "Apart.useCb", "K4.<init>").map(count) must_==
          List(2, 1, 1, 1, 8).map(Count.Finite(_))
      )
      .and(
        List(
          "inner2",
          "imports.m",
          "shadowed.<new#1>.get",
          "inherits.<new#1>.get",
          "inner3",
          "Caps.projected"
        )
          .map(count(_).toString)
          .must(contain(contain("an anonymous contextual parameter")).forall)
      )

  // Left to implicit search, and so uncovered: a receiver's contextual parameter without a name
  // beside a member's own of its type (`Vied.g`) or a given of a class inside its (`Outer.In.h`,
  // which search finds first), a receiver's beside the code between a local definition and its
  // class, which may declare one (`Blk`), and among a class's members a deferred given without a
  // name.
  private def later = {
    val expected = List(
      "Vied.g" -> "an anonymous contextual parameter search may not tell apart",
      "In.h" -> "an anonymous contextual parameter",
      "Blk.<block#1>.pick" -> "an anonymous contextual parameter",
      "Anon.get" -> "an anonymous given among the members of an enclosing class"
    )
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    expected.map { (name, _) =>
      count(name) match {
        case Count.Unresolved(List(reason)) => reason.stripPrefix(prefix).takeWhile(_ != ':')
        case other                          => other.toString
      }
    } must_== expected.map(_._2)
  }

}
