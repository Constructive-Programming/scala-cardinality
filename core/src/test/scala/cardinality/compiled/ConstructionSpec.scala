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
    an open position over a type parameter is the shape model's   $open
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
      |final class Wrap[A](val io: IterableOnce[A])
      |final class Ev[A, B](val ev: A <:< B)
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Construction.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

  private def count(name: String): Count =
    rows.sources.flatMap(_.toOption.toList.flatMap(_.methods)).find(_.name == name).get.count

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

  // A position over a type parameter whose shape is open is the shape model's alone, whatever the
  // stored estimate says of its class: `Wrap` stores the sequence of `A`s it is given, any of
  // infinitely many built from the input's, ω; `Ev` stores the evidence it is given, of which there
  // is one at most, 1 (the old front end's counts).
  private def open =
    (count("ctor.Wrap.<init>") must_== Count.Countable)
      .and(count("ctor.Ev.<init>") must_== Count.Finite(1))

}
