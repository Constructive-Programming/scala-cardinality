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
      |object Inst { given shown: Show with { def show(b: Boolean): Boolean = b } }
      |trait Anon { given Boolean = deferred; def get: Boolean = true }
      |trait Fed { given feed(using b: Boolean): Boolean; def get: Boolean = true }
      |final class Flat[A](using Option[A]) { def f(x: A): A = x }
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
  // concrete given supplies nothing to the members beside it: `other` is one of 2.
  private def givens =
    (count("Givens.named") must_== Count.Finite(2))
      .and(count("Givens.pair") must_== Count.Finite(1))
      .and(count("Givens.flag") must_== Count.Finite(2))
      .and(count("Givens.conv") must_== Count.Finite(1))
      .and(count("Givens.other") must_== Count.Finite(2))

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

  // Left to implicit search, and so uncovered: a contextual parameter without a name (a `using`
  // type, a context bound's evidence, a constructor's too), a member of a given instance, and among a class's members
  // a deferred given without a name and an abstract extension method.
  private def later = {
    val expected = List(
      "Params.anon" -> "an anonymous contextual parameter",
      "Params.bound" -> "an anonymous contextual parameter",
      "Flat.f" -> "an anonymous contextual parameter",
      "shown.show" -> "a member of a given instance",
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
