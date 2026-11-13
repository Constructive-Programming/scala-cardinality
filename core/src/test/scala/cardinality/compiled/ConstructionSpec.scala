package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** Construction in the compiled front end (decision D5, register D26): a pattern definition is an
  * ordinary binding, and a template whose construction always throws builds no instance. Every
  * expected count is derived in its example's comment.
  */
class ConstructionSpec extends Specification {

  def is = s2"""
  A construction is read, never refused for throwing
    a pattern definition binds, and one that throws builds nothing $patterns
  A constructor is a method
    a secondary one chooses the primary constructor's arguments    $secondary
    a singleton beside values is one value of a position          $singletons
  A class whose constructor is restricted stores what its public builders build
    each at most min(its parameters, the fields others may read)  $restricted
  """

  private val text =
    """package ctor
      |
      |object Pattern { val (a, b) = (true, false); def g(x: Boolean): Boolean = x }
      |object Nameless { val _ = ???; def g(x: Boolean): Boolean = x }
      |object PatT { val (pa, pb): (Boolean, Boolean) = ???; def g(b: Boolean): Boolean = b }
      |trait AT { def m(b: Boolean): Boolean }
      |class AD { def f: AT = new AT { val v: Boolean = ???; def m(b: Boolean): Boolean = b } }
      |object AO { val boom: Int = ???; def f: AT = new AT { def m(b: Boolean): Boolean = b } }
      |final class Two(val b: Boolean) { def this(u: Unit) = this(true); def this(b: Boolean, c: Boolean) = this(b && c) }
      |final class Gen[A] private (val a: A, val b: Boolean) { def this(u: Unit, a: A) = this(a, true) }
      |final class Lazy(f: => Boolean, xs: Boolean*) { def this(u: Unit) = this(true) }
      |class Outer(val o: Boolean) { final class In(val i: Boolean) { def this(u: Unit) = this(o) } }
      |final class Rec(val r: Option[Rec]) { def this(u: Unit) = this(None) }
      |object Sing
      |final class Pos(p: (Sing.type, Boolean), q: (Any, Sing.type))
      |final class Mute private (private val a: Boolean)
      |object Mute { def of(a: Boolean): Mute = new Mute(a) }
      |final class Pair private (val a: Boolean, val b: Boolean) { def this(u: Unit) = this(true, true) }
      |object Pair { def of(a: Boolean): Pair = new Pair(a, a); val one: Pair = new Pair(true, false) }
      |final class Hidden private (val a: Boolean)
      |object Hidden { def made(h: Hidden): Hidden = h }
      |final class Lone private (val a: Boolean) { private def peek: Boolean = a }
      |trait Makes { def lone(a: Boolean): Lone }
      |final class Gen2[A] private (val a: A, val b: Boolean)
      |final class Peek private (private val a: Boolean) { private def get: Boolean = a }
      |object Peek { def of(a: Boolean): Peek = new Peek(a); def read(p: Peek): Boolean = p.get }
      |final case class Kept private (private val a: Boolean)
      |object Kept { def of(a: Boolean): Kept = new Kept(a) }
      |class Base private[ctor] (val a: Boolean)
      |object Base { val one: Base = new Base(true) }
      |class Sub(x: Boolean) extends Base(x)
      |final class Made private[ctor] (val a: Boolean)
      |class Holder(val flag: Boolean) { object F { def mk: Made = new Made(flag) } }
      |trait Shown { protected def a: Boolean; def show: Boolean = a }
      |final class Inh private (protected val a: Boolean) extends Shown
      |object Inh { def of(a: Boolean): Inh = new Inh(a) }
      |final class Absurd(p: (Sing.type, Nothing => Boolean))
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Construction.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

  private def count(name: String): Count = counts(name).head

  // The counts of the rows of one name in source order: a class's primary constructor, then its
  // secondary ones.
  private def counts(name: String): List[Count] =
    rows.sources.flatMap(_.toOption.toList.flatMap(_.methods)).filter(_.name == name).map(_.count)

  // `Pattern` is built, and its `g` is one of the 2^2 functions. `Nameless`'s `val _ = ???` throws
  // whenever the object is built, so no call of its `g` returns: 1, and so for `PatT.g`; but `pa`
  // is the choice being counted, its whole statement aside, and `PatT` holds nothing else that
  // throws: one of 2. An anonymous class whose `v` throws is never built (`m` 1), `v` itself one
  // of 2; one made in a method of a static object that is never built is never made (`AO`'s `m`
  // 1).
  private def patterns =
    (count("ctor.Pattern.g") must_== Count.Finite(4))
      .and(count("ctor.Nameless.g") must_== Count.Finite(1))
      .and(count("ctor.PatT.g") must_== Count.Finite(1))
      .and(count("ctor.PatT.pa") must_== Count.Finite(2))
      .and(count("ctor.AD.f.<new#1>.m") must_== Count.Finite(1))
      .and(count("ctor.AD.f.<new#1>.v") must_== Count.Finite(2))
      .and(count("ctor.AO.f.<new#1>.m") must_== Count.Finite(1))

  // A secondary constructor's row is a method from its parameters to the primary's arguments.
  // `Two`: a Boolean from `u`, 2; from two Booleans, 2^4 = 16. `Gen[A]`, whose primary is
  // private: `(A, Boolean)` from `(u, a)`, `a` the one `A` and any Boolean, 2. `Lazy`: a by-name
  // argument is its Boolean, a repeated one a sequence of them, ω. `In` is built on an instance of
  // `Outer`, whose `o` it may pass: a Boolean from `u` and `o`, 2^2 = 4. `Rec`'s argument is its
  // own class, which the method side refuses: no estimate stands for a secondary constructor.
  private def secondary =
    (counts("ctor.Two.<init>") must_== List(2, 2, 16).map(Count.Finite(_)))
      .and(counts("ctor.Gen.<init>").last must_== Count.Finite(2))
      .and(counts("ctor.Lazy.<init>").last must_== Count.Countable)
      .and(counts("ctor.Outer.In.<init>").last must_== Count.Finite(4))
      .and(counts("ctor.Rec.<init>").last must beLike { case Count.Unresolved(_) => ok })

  // A tuple holding a singleton beside values, no type parameter: `(Sing.type, Boolean)` is the
  // estimate's 1 · 2; beside a top, `(Any, Sing.type)` is read on the method side, the top one
  // value in an input and the singleton its one value: 1. The row: 2.
  // `(Sing.type, Nothing => Boolean)` holds the absurd function, one value, where the estimate
  // says 0: the two readings disagree, and the row is `?`.
  private def singletons =
    (count("ctor.Pos.<init>") must_== Count.Finite(2))
      .and(count("ctor.Absurd.<init>") must beLike { case Count.Unresolved(_) => ok })

  private def definition(name: String) =
    rows.sources.flatMap(_.toOption.toList.flatMap(_.definitions)).find(_.name == name)

  private def size(name: String): Option[String] = definition(name).flatMap(_.size.map(_.render))

  // `Mute.of` builds at most min(2, the fields shown publicly): nothing public reads `a`, one
  // value. `Pair`: the secondary constructor builds 1, `of` 2, `one` 1, of the 4 the public fields
  // show: the largest, 2. `Hidden.made` declares `Hidden` and builds none, one value; `Lone`'s
  // factory has no body, so no bound is known: no size. `Gen2` has a type parameter
  // and keeps its fields' product, as a generic class's row shows it: its parameter the reason.
  // Code outside the class may read a field: `Peek.read` reads `a` through the private `get`, and
  // a pattern may extract a case class's `a`: 2 each. `Sub` builds a `Base` with any `a`, which
  // `Base.one` does not show: no size. `Holder.F.mk` builds a `Made` from the instance's `flag`:
  // no known bound, no size. `Shown.show`, public, reads `Inh`'s protected `a`: 2.
  private def restricted =
    (size("ctor.Mute") must beSome("1"))
      .and(size("ctor.Pair") must beSome("2"))
      .and(size("ctor.Hidden") must beSome("1"))
      .and(size("ctor.Lone") must beNone)
      .and(definition("ctor.Gen2").toList.flatMap(_.unbound.map(_.subject)) must_== List("A"))
      .and(size("ctor.Peek") must beSome("2"))
      .and(size("ctor.Kept") must beSome("2"))
      .and(List("ctor.Base", "ctor.Made").map(size) must_== List(None, None))
      .and(size("ctor.Inh") must beSome("2"))

}
