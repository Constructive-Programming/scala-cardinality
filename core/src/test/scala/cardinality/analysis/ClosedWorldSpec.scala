package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** The closed world (docs/plans/v1.md §2): the supplied sources are the whole program, so a class
  * no supplied template extends and a member none overrides are final (`ClosedWorld`). Each count
  * is derived in the comment beside it: with the member final, `target` ranges over the functions
  * of b, 2^2 = 4; with it open, over those of (b, m), 2^4 = 16.
  */
class ClosedWorldSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    A member no supplied template overrides is final
      in any file, through an alias, or by a mixin of another parent  $overrides
      a parent outside the sources may override it: refused            $unknownParent
      in a braceless body or after an annotation                      $layout
      a literal `val` with no type, and a constructor `val`           $values
    A class no supplied template extends is final
      its value is its constructor arguments                          $classes
  """

  private val base =
    "a.scala" -> "abstract class A { def m: Boolean = true; def target(b: Boolean): Boolean }"

  // B (another file) overrides m: 16; spelled through an alias of A too; with no B, m is final: 4.
  // D extends C and mixes in T2, whose m overrides C's: 16; with no D, 4.
  def overrides = {
    val sub =
      "class B extends A { override def m: Boolean = false; def target(b: Boolean): Boolean = b }"
    val mixin = """trait P { def m: Boolean }
      |trait T2 extends P { override def m: Boolean = false }
      |abstract class C extends P { def m: Boolean = true; def target(b: Boolean): Boolean }
      |""".stripMargin
    (counts(base, "b.scala" -> sub)("A.target") === Count.Finite(16))
      .and(
        counts(base, "b.scala" -> s"type AA = A\n${sub.replace("extends A", "extends AA")}")(
          "A.target"
        ) === Count.Finite(16)
      )
      .and(counts(base)("A.target") === Count.Finite(4))
      .and(rows(mixin + "abstract class D extends C with T2")("C.target") === Count.Finite(16))
      .and(rows(mixin)("C.target") === Count.Finite(4))
  }

  // D mixes in Iterator, a library parent whose members this reading does not list: it may define
  // m, so m is neither final nor known to vary — refused, never 4 or 16.
  def unknownParent =
    refused(
      rows(
        "abstract class C { def m: Boolean = true; def target(b: Boolean): Boolean }\n" +
          "abstract class D extends C with Iterator[Int]"
      )("C.target"),
      "member may be overridden through a parent outside the sources: C.m"
    )

  // `final` is written before the keyword, so the indentation a braceless body reads is kept, and
  // after an annotation: m is final in both — 4.
  def layout = {
    val braceless =
      "abstract class A:\n  def m: Boolean = true\n  def target(b: Boolean): Boolean\n"
    val annotated = "abstract class A { @deprecated(\"x\", \"1\") def m: Boolean = true; " +
      "def target(b: Boolean): Boolean }"
    (rows(braceless)("A.target") === Count.Finite(4))
      .and(rows(annotated)("A.target") === Count.Finite(4))
  }

  // `final val y = true` would be the constant `true`, and a constructor `val` spells the signature:
  // both are left as written, and the receiver asks whether a supplied template overrides them.
  // Nothing does: y is true and R passes P's p true — 4 each. B overrides y, which is then open,
  // and an open member's inferred type is not read: refused.
  def values = {
    val literal = "abstract class A { val y = true; def target(b: Boolean): Boolean }"
    val field =
      "class P(val p: Boolean)\nabstract class R extends P(true) { def target(b: Boolean): Boolean }"
    (rows(literal)("A.target") === Count.Finite(4))
      .and(
        refused(
          rows(literal + "\nabstract class B extends A { override val y = false }")("A.target"),
          "overridable member's result type is inferred: y"
        )
      )
      .and(rows(field)("R.target") === Count.Finite(4))
  }

  // Box no template extends is its argument a: f is a's functions, 4. Sub overrides get, so a Box
  // is the record (a, get) of two Booleans: 2^4 = 16.
  def classes = {
    val box =
      "class Box(val a: Boolean) { def get: Boolean = a }\nobject U { def f(x: Box): Boolean = ??? }"
    (rows(box)("U.f") === Count.Finite(4))
      .and(
        rows(box + "\nclass Sub extends Box(true) { override def get: Boolean = false }")("U.f") ===
          Count.Finite(16)
      )
  }

}
