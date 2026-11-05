package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// What a top-level value holds (docs/research/pure-code.md §2, register D30): `min(1, |T|)` for its
// declared type, read as inhabitation: a nonzero capacity read with no reason is a value, and a
// zero or an overcount (an intersection, an unbuilt class) leaves the row undecided. Fixtures
// checked with scalac 3.9.0 (the integrated review's probes, z1b/fx and z1bfix2/fx).
class ValueRowsSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    A value row holds one value when its declared type has one
      an inhabited, undeclared or unresolved type                     $valueReasons
      a sum drops the summands that provably build nothing            $sumDropsEmptySummand
    """

  // `Int`, `Unit` and `Either[Nothing, Boolean]` (0 + 2) are inhabited: 1. A value with no
  // declared type holds one. A function from `Nothing` is the absurd function, which the
  // algebra's 0 does not decide: refused, though the method side counts the same `val` 1; so is a
  // class holding one. The algebra's intersection is an upper
  // bound (`Int & String` is empty, its bound 2^32): refused. An unresolved declared type may be
  // empty: its reason, not 1 — `Foo => Nothing` too, inhabited exactly when `Foo` is empty. Read
  // from another package, through a wildcard import or a path, the class and the alias are refused
  // alike: the reading that meets them is the value row's own.
  def valueReasons = {
    val files = List(
      "vr/a.scala" ->
        """|package vr
           |val y: Int = 3
           |val u: Unit = ()
           |val ok: Either[Nothing, Boolean] = Right(true)
           |val n = 3
           |val f: Nothing => Int = (n: Nothing) => 1
           |final case class P(f: Nothing => Int)
           |val p: P = P((n: Nothing) => 1)
           |val q: Foo = ???
           |val h: Foo => Nothing = ???
           |val vint: Int & String = ???
           |type I = Int & String
           |""".stripMargin,
      "vb/b.scala" ->
        """|package vb
           |import vr.*
           |val p: P = ???
           |val i: I = ???
           |val p2: vr.P = ???
           |""".stripMargin
    )
    val found = rowsOf(files*)
    (found(("vr.y", Value)) === "1")
      .and(found(("vr.u", Value)) === "1")
      .and(found(("vr.ok", Value)) === "1")
      .and(found(("vr.n", Value)) === "1")
      .and(constructor("vr.f", files*) === Count.Finite(1))
      .and(
        found(
          ("vr.p", Value)
        ) === "unresolved[value not decided: P]"
      )
      .and(found(("vr.q", Value)) === "unresolved[Foo]")
      .and(found(("vr.h", Value)) === "unresolved[Foo]")
      .and(
        found(("vr.vint", Value)) ===
          "unresolved[value not decided: Int & String]"
      )
      .and(
        found(("vb.p", Value)) ===
          "unresolved[value not decided: P]"
      )
      .and(
        found(("vb.i", Value)) ===
          "unresolved[value not decided: I]"
      )
      .and(
        found(("vb.p2", Value)) ===
          "unresolved[value not decided: vr.P]"
      )
  }

  // A sum drops the summands that provably build nothing: S1's parent's constructor throws, so a
  // value of S is S2's alone - 1, not the refusal any unread summand gave (follow-up 25).
  def sumDropsEmptySummand = {
    val found = rows(
      """|package uf
         |abstract class Base(n: Int)
         |sealed trait S
         |case class S1(i: Int) extends Base(???)
         |case object S2 extends S
         |val s: S = ???
         |""".stripMargin
    )
    found(("uf.s", Value)) === "1"
  }

}
