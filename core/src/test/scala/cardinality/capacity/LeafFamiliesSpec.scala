package cardinality.capacity

import org.specs2.Specification
import org.specs2.execute.Result

// Case-class leaves on the stored side (docs/research/stored-value-families.md, register D15): a
// leaf no `final` closes is no closed summand when a subclass may override a member it has, read
// across its whole ancestry before any member is. Fixtures are valid Scala 3.9.0 (scalac); the
// overrides were observed by 3.9.0 runs on session fixtures (not retained).
class LeafFamiliesSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    A subclass may override what the leaf has
      an extension, a parent's field, an overload, a later clause    $open
    A member fixed anywhere in the ancestry closes the leaf
      a parent's final override, or the leaf's own field             $closed
  """

  // A family W of the leaf and an object K, after a prelude, and a class using it.
  private def family(prelude: String, leaf: String): String =
    s"${prelude}sealed trait W\n$leaf\ncase object K extends W\nfinal case class Use(w: W)"

  private def use(prelude: String, leaf: String): String =
    rows(family(prelude, leaf))(("Use", Class))

  private def opens(prelude: String, leaf: String, member: String): Result =
    use(prelude, leaf) ===
      s"unresolved[sealed family not summed: W (a subtype open to extension: C (overridable member $member))]"

  // The leaf's extension b; V's foo, which the second clause's plain parameter does not override;
  // FP's `val` field d; OP's d(s: String), which the final overload d(i: Int) leaves open; PP's d,
  // which GP's private d does not fix; an overload of `==`. Each family is refused (each read 3).
  def open =
    opens(
      "",
      "case class C(x: Boolean) extends W { extension (y: Boolean) def b: Boolean = x }",
      "b"
    )
      .and(
        opens(
          "trait V { def foo: Boolean = true }\n",
          "case class C(x: Boolean)(foo: Unit) extends V, W",
          "V.foo"
        )
      )
      .and(opens("class FP(val d: Int)\n", "case class C(x: Boolean) extends FP(1), W", "FP.d"))
      .and(
        opens(
          "trait OP { def d(s: String): Int = 2 }\n",
          "case class C(x: Boolean) extends OP, W { final def d(i: Int): Int = 1 }",
          "OP.d"
        )
      )
      .and(
        opens(
          "trait GP { private def d: Int = 1 }\ntrait PP extends GP { def d: Int = 2 }\n",
          "case class C(x: Boolean) extends PP, W",
          "PP.d"
        )
      )
      .and(opens("", "case class C(x: Boolean) extends W { def ==(i: Int): Boolean = true }", "=="))

  // P's final override fixes GP's d for every subclass of C (scalac rejects overriding it again,
  // E164); C's field b overrides V's b, and an override of the field is another value of its type.
  // W = C's 2 + K's 1 = 3 each.
  def closed =
    (use(
      "trait GP { def d: Int = 1 }\ntrait P extends GP { final override def d: Int = 2 }\n",
      "case class C(x: Boolean) extends P, W"
    ) === "3")
      .and(
        use(
          "trait V { def b: Boolean = true }\n",
          "case class C(override val b: Boolean) extends V, W"
        ) === "3"
      )

}
