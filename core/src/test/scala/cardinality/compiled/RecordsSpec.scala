package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.ScalametaFrontEnd
import org.specs2.Specification

/** Capability records in the compiled front end (docs/plans/compiled-own-types.md, stretch 4): a
  * class or trait with members, named by a parameter's type. Every expected count is derived in its
  * example's comment, and the scalameta front end is asked for the same rows.
  */
class RecordsSpec extends Specification {

  def is = s2"""
  A capability record is what a body can observe of it
    an abstract member is what any implementation supplies       $open
    a final class is the tuple of its public fields               $fixed
    what a record inherits, and a member something overrides      $inherited
    a record this step does not read is uncovered                 $later
    the scalameta front end agrees on every row measured          $agrees
  """

  private val text =
    """package rec
      |
      |trait CanGet[S, A] { def get(s: S): A }
      |trait Src[A] { def x: A; def y: A = x }
      |trait Monoid[A] { def empty: A; def combine(x: A, y: A): A }
      |trait Log { def log(b: Boolean): Unit; def id: Boolean }
      |final class Box2[A](val a: A, val b: A)
      |trait Ext { extension (b: Boolean) def tag: Boolean }
      |trait Over { def equals(x: Boolean, y: Boolean): Boolean }
      |final abstract class Never
      |final class Hid(secret: Boolean) { def reveal: Boolean = secret }
      |class Base(val b: Boolean)
      |class Sub extends Base(true)
      |trait Seeded { def seed: Boolean }
      |trait Grown extends Seeded { def more: Boolean }
      |trait Derived extends Seeded { def other: Boolean = seed }
      |abstract class Field(val p: Boolean) { def q: Boolean }
      |abstract class Passed extends Field(true) { def r: Boolean }
      |trait Ov { def f: Boolean = true; def g: Boolean }
      |class OvSub extends Ov { override def f: Boolean = false; def g: Boolean = true }
      |trait Prod extends Product { def x: Boolean; def canEqual(that: Any): Boolean }
      |trait Shown { def show: Boolean }
      |trait Shows extends Shown
      |trait Free extends Field
      |final class Pin private (val a: Boolean, val b: Boolean)
      |object Pin { def one(x: Boolean): Pin = new Pin(x, x) }
      |final class Shut private (val a: Boolean)
      |trait Typed { type T; def t: T }
      |trait Typing extends Typed { def b: Boolean }
      |trait Guarded { protected def h: Boolean }
      |trait Guards extends Guarded { def k: Boolean = h }
      |object Use {
      |  def read[S, A](s: S, g: CanGet[S, A]): A = g.get(s)
      |  def noSource[S, A](g: CanGet[S, A]): A = ???
      |  def pick[A](t: Src[A]): A = t.x
      |  def twice[A](m: Monoid[A], a: A): A = a
      |  def ask(l: Log): Boolean = l.id
      |  def fromBox[A](b: Box2[A]): A = b.a
      |  def makeBox[A](x: A, y: A): Box2[A] = Box2(x, y)
      |  def tagged(e: Ext): Boolean = true
      |  def hid(h: Hid): Boolean = true
      |  def over(o: Over, b: Boolean): Boolean = b
      |  def never(n: Never): Boolean = true
      |  def make(): Never = ???
      |  def base(b: Base): Boolean = true
      |  def grown(g: Grown): Boolean = true
      |  def derived(d: Derived): Boolean = true
      |  def passed(p: Passed): Boolean = true
      |  def ov(o: Ov): Boolean = true
      |  def prod(p: Prod): Boolean = true
      |  def show(s: Shows): Boolean = true
      |  def free(f: Free): Boolean = true
      |  def pin(p: Pin): Boolean = true
      |  def mkPin(x: Boolean, y: Boolean): Pin = ???
      |  def shut(s: Shut): Boolean = true
      |  def typing(t: Typing): Boolean = true
      |  def guards(g: Guards): Boolean = true
      |}
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Records.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name == s"rec.Use.$name")
      .get
      .count

  // A trait is any implementation of its abstract members. `read` can only apply `get` to `s`:
  // 1; `noSource` has no S to apply it to: 0. A concrete member computes from the others: `pick`
  // returns `x`, 1. `twice` returns `a`, `empty`, or any combination of them: ω. `log` builds
  // `Unit`, which needs no member: `ask` is a Boolean computed from `id`, 2^2. A member named like
  // one of `Any`'s with other parameters is a member like any: `over` is a Boolean computed from
  // `b` (2 values) and the function `equals` holds (2^(2 × 2) = 16 values): 2^(2 × 16) = 2^32.
  private def open =
    (count("read") must_== Count.Finite(1))
      .and(count("noSource") must_== Count.Finite(0))
      .and(count("pick") must_== Count.Finite(1))
      .and(count("twice") must_== Count.Countable)
      .and(count("ask") must_== Count.Finite(4))
      .and(count("over") must_== Count.Finite(BigInt(2).pow(32)))

  // A final class holds its public fields: `fromBox` returns `a` or `b`, 2, and `makeBox` builds
  // one of the 2 × 2 pairs of `x` and `y`. A parameter a public member returns as it is observed:
  // `Hid` is its `secret`, through `reveal`, so `hid` is one of 2^2. A final class whose
  // constructor the site may not call is one of what its factories build (rule B): `Pin.one`
  // builds 2 of its 2 × 2 values, so `pin` is one of 2^2 and `mkPin` one of the 2^(2 × 2)
  // functions from two Booleans to those 2. A class written `final abstract` has no value: `never`
  // is the one function from it, and `make` has no total body.
  private def fixed =
    (count("fromBox") must_== Count.Finite(2))
      .and(count("makeBox") must_== Count.Finite(4))
      .and(count("never") must_== Count.Finite(1))
      .and(count("make") must_== Count.Finite(0))
      .and(count("hid") must_== Count.Finite(4))
      .and(count("pin") must_== Count.Finite(4))
      .and(count("mkPin") must_== Count.Finite(16))

  // A record holds what it inherits as a receiver does. `Grown` is its own `more` and the `seed` it
  // inherits: 2 × 2 values, so `grown` is one of 2^4. `Derived`'s `other` computes from `seed`:
  // `derived` is one of 2^2. `Passed` passes `true` to `Field`'s `p`, a constant, and holds `q` and
  // `r`: 2^4. `Free` passes nothing, so `p` is a value each instance chooses beside `q`: 2^4.
  // `OvSub` overrides `Ov.f`, so `f` varies beside `g`: 2^4. A member restating one of
  // `Product`'s (`canEqual`) is outside the model: `Prod` is its `x`, 2^2.
  private def inherited =
    (count("grown") must_== Count.Finite(16))
      .and(count("derived") must_== Count.Finite(4))
      .and(count("passed") must_== Count.Finite(16))
      .and(count("ov") must_== Count.Finite(16))
      .and(count("prod") must_== Count.Finite(4))
      .and(count("free") must_== Count.Finite(16))

  // Not read yet, each by its reason: an extension member; a class something extends; a record
  // that inherits a member named like the measured definition, which may be that one on another
  // value; one that inherits an abstract type member; one whose inherited member the site may not
  // call; and a final class that nothing the site may call builds.
  private def later = {
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    def reason(name: String) = count(name) match {
      case Count.Unresolved(List(why)) => why.stripPrefix(prefix).takeWhile(_ != ':')
      case other                       => other.toString
    }
    List("tagged", "base", "show", "typing", "guards", "shut").map(reason) must_== List(
      "an extension method among the members of a capability record",
      "a capability record of a class something extends",
      "a capability record with a member named like the measured definition",
      "a capability record with an abstract type member",
      "a capability record with a member hidden at the use site",
      "a capability record with no constructor or factory the site may call"
    )
  }

  private def agrees = {
    val compared = Differential.run(input, Differential.answer(ScalametaFrontEnd, input.old))
    ((compared.failed, compared.noted) must_== (Nil, Nil))
      .and(Differential.failures(compared.compared) must_== Nil)
  }

}
