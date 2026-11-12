package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.ScalametaFrontEnd
import org.specs2.Specification

/** Type parameters of the compiled front end beyond atoms and rigid constructors: a type
  * constructor written as a class's argument (`Shapes.argument`), and a bounded proper parameter,
  * whose bound is an ambient proof read from its symbol, as the old front end reads it from syntax
  * (`Bounds`). Every expected count is derived in its example's comment, and the scalameta front
  * end is asked for the same rows.
  */
class TypeParametersSpec extends Specification {

  def is = s2"""
  A written type constructor is applied where its parameter is
    a closure is read with the binders of both sites           $closures
    a symbol two sites bind differently is not read yet        $rebound
  A bound of a proper type parameter is a proof beside it
    an upper bound views the values, a lower bound builds them  $proofs
    a bound the model does not read is refused, as before       $refused
    a top inside a bound, or a lower one by a class, is later   $later
    the scalameta front end agrees on every row measured        $agrees
  """

  private val text =
    """package bnd
      |
      |trait Show { def show: Boolean }
      |sealed trait Two
      |case object One extends Two
      |case object Other extends Two
      |object Use {
      |  def up[A <: Boolean](a: A): Boolean = a
      |  def keep[A <: Boolean](a: A): A = a
      |  def low[A >: Boolean](a: A): A = a
      |  def ref[A <: AnyRef](a: A): A = a
      |  def chain[A <: B, B](a: A): B = a
      |  def fam[A <: Two](a: A): Two = a
      |  def open[A <: Show](a: A): Boolean = a.show
      |  def rec[A <: Option[A]](a: A): Boolean = true
      |  def same[A, B >: A <: A](x: B, y: B): A = x
      |  def top[A >: AnyRef](): A = ???
      |  def fn[A <: (Boolean => Any)](a: A): Boolean = true
      |  def cls[A >: Boolean <: AnyVal](a: A): A = a
      |}
      |final class Box[A <: Boolean](val a: A) { def get: Boolean = a }
      |final case class Wrap[F[_], A](v: F[A]) {
      |  object Ops { def back(w: Wrap[[X] =>> (A, X), Boolean]): A = w.v._1 }
      |}
      |object Hk {
      |  def wrap(w: Wrap[[X] =>> Either[Boolean, X], Boolean]): Boolean = true
      |  def held[G[_]](w: Wrap[G, Int]): G[Int] = w.v
      |}
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("TypeParameters.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name.endsWith(name))
      .get
      .count

  // `wrap`'s argument holds an `Either[Boolean, Boolean]`, 4 values: it is one of 2^4.
  private def closures = count("Hk.wrap") must_== Count.Finite(16)

  // In `Wrap.Ops.back`, the written `(A, X)` means the `A` of the enclosing `Wrap`, while reading
  // the argument `Wrap[..., Boolean]` binds that same `A` to `Boolean`: one symbol with two
  // meanings. In `held`, the field `v: F[A]` is `G` applied to `A`, an uninterpreted application
  // whose argument is read as a type, but `A` is bound to the values of `Int`: a class's parameter
  // bound to a value is not read as a type. Both are uncovered.
  private def rebound =
    (uncovered("Wrap.Ops.back") must startWith("a written constructor over a parameter bound"))
      .and(uncovered("Hk.held") must startWith("a class's parameter bound to a value"))

  // A row's reason when it is uncovered, without the prefix that says so.
  private def uncovered(name: String): String = count(name) match {
    case Count.Unresolved(List(reason)) if reason.startsWith(s"${TastyFrontEnd.Uncovered}: ") =>
      reason.stripPrefix(s"${TastyFrontEnd.Uncovered}: ")
    case other => other.toString
  }

  // `up`: `a` read as a Boolean is 2 values, so 2^2 functions to Boolean. `keep`: an A is never
  // built from a Boolean, so `a` alone, 1. `low`: an A is `a`, `true` or `false`, 3. `ref`: an
  // `AnyRef` bound says nothing, 1. `chain`: `a` upcast, the only B, 1. `fam`: `a` read as a Two
  // is one of 2 cases, and the result is either case: 2^2. `open`: `a` read as a Show, a
  // capability record of one Boolean member, 2^2. `Box.get`: its field read as a Boolean, 2^2.
  private def proofs =
    List("Use.up", "Use.keep", "Use.low", "Use.ref", "Use.chain", "Use.fam", "Use.open", "Box.get")
      .map(count) must_== List(4, 1, 3, 1, 1, 4, 4, 4).map(Count.Finite(_))

  // `A <: Option[A]` describes a type by a fixpoint: the model's refusal. `same`'s B lies between
  // A and A, no cycle of upper bounds: x or y upcast, 2.
  private def refused =
    (count("Use.rec").toString must contain("recursive bound not read"))
      .and(count("Use.same") must_== Count.Finite(2))

  // A top in a bound is read by the place the bound puts it, which a proof does not carry: `A >:
  // AnyRef` would build one value where any object is a result (ω), and `A <: (Boolean => Any)`
  // would hide the output top of the function a is read as. `AnyVal` above admits `Boolean` below
  // only because Boolean is a value class, which is not read.
  private def later =
    List("Use.top", "Use.fn", "Use.cls").map(uncovered) must_== List(
      "a bound that holds a top type: A",
      "a bound that holds a top type: A",
      "a structural lower bound beside a class bound"
    )

  private def agrees = {
    val compared = Differential.run(input, Differential.answer(ScalametaFrontEnd, input.old))
    ((compared.failed, compared.noted) must_== (Nil, Nil))
      .and(Differential.failures(compared.compared) must_== Nil)
  }

}
