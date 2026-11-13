package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.ScalametaFrontEnd
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
    what implicit search would choose is not read yet             $later
    the scalameta front end agrees on every row measured          $agrees
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
      |final class Crowd[A](using Option[A]) { given Option[A] = None }
      |trait Base { given Option[Int] = None }
      |final class Selfish(using Option[Int]) { self: Base => }
      |object Shared { class s; given s: s = new s }
      |trait Two { given g: Boolean = deferred; given k: Option[Boolean] = Some(g) }
      |trait Exts { extension (b: Boolean) def tag: Boolean; def get: Boolean = true }
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Contextual.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

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
  // returns `x` or `y`, and `choose` returns `x` or the A its `Option[A]` may hold, 2.
  private def extensions =
    (count(".flip") must_== Count.Finite(4))
      .and(count(".or") must_== Count.Finite(2))
      .and(count(".choose") must_== Count.Finite(2))

  // An anonymous parameter no other candidate competes with is the value search finds: `anon`
  // returns `x` or what its `Option[A]` may hold, 2, as the named `pick`; `bound`'s `List[A]` holds any
  // number of As: ω. `twin`: a `Show` and a `Pick[A]`, traits of the module neither of which
  // extends the other, so no search finds both; `pick` nests without end (`pick(x, pick(x,
  // x))`, …): ω.
  // A body is counted, not read: the given `local` and `near` declare in theirs competes with no
  // other body, `local` is ω as `Pick[A]` nests, and `near` 2, as `anon`. Two of one type at one
  // level are ambiguous (`same`); an `Option` beside a `List`, classes of the standard library
  // whose bases the old reading does not know, is not read yet (`wide`), nor a `Show` beside a
  // `Null`, which is below every reference type (`nul`), nor one beside a context-function
  // result, whose binder may hide it (`ctx`). `both`'s `Pick[A]` and `Pick[B]` are never
  // subtypes of each other (A and B are distinct and unbounded, rigid in the body), so each is
  // the one search finds: `pick` nests, ω. A constructor's anonymous parameter is read by the same
  // rule: `Flat`'s `Option[A]` is a position over A, built from the supplied one or `None`, 2.
  private def anonymous =
    (List(
      "Params.anon",
      "Params.bound",
      "Params.twin",
      "Params.local",
      "Params.near",
      "Params.both",
      "Flat.<init>"
    ).map(count) must_== List(
      Count.Finite(2),
      Count.Countable,
      Count.Countable,
      Count.Countable,
      Count.Finite(2),
      Count.Countable,
      Count.Finite(2)
    ))
      .and(count("Params.same").toString must contain("ambiguous anonymous evidence"))
      .and(
        List("Params.wide", "Params.nul", "Params.ctx")
          .map(count(_).toString)
          .must(contain(contain("an anonymous contextual parameter")).forall)
      )

  // Left to implicit search, and so uncovered: a receiver's contextual parameter without a name
  // (`Flat.f`), a constructor's beside a given its class body declares or its self type holds
  // (`Crowd`, `Selfish`, ambiguous there), and among a class's members a deferred given without a name and an abstract
  // extension method.
  private def later = {
    val expected = List(
      "Flat.f" -> "an anonymous contextual parameter",
      "Crowd.<init>" -> "an anonymous contextual parameter beside the class's contextual scope",
      "Selfish.<init>" -> "an anonymous contextual parameter beside the class's contextual scope",
      "Anon.get" -> "an anonymous given among the members of an enclosing class",
      "Exts.get" -> "an extension method among the members of an enclosing class"
    )
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    expected.map { (name, _) =>
      count(name) match {
        case Count.Unresolved(List(reason)) => reason.stripPrefix(prefix).takeWhile(_ != ':')
        case other                          => other.toString
      }
    } must_== expected.map(_._2)
  }

  // Two rows differ, each derived above: `Two.k` counts where the old front end refuses for
  // re-entry, and `Fed.get` is the correction of an abstract given read as a function.
  private def agrees = {
    val compared = Differential.run(input, Differential.answer(ScalametaFrontEnd, input.old))
    def key(name: String) = compared.compared.filter(_.row.name == name).map(_.row.key)
    val derived = key("ctx.Two.k").map(_ -> "9").toMap
    val corrected = key("ctx.Fed.get").map(_ -> "4 -> 16").toMap
    ((compared.failed, compared.noted) must_== (Nil, Nil))
      .and((derived.size, corrected.size) must_== (1, 1))
      .and(Differential.failures(compared.compared, derived, corrected) must_== Nil)
      .and(Differential.unused(compared.compared, corrected) must_== Nil)
  }

}
