package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** A capability record's member named like the measured definition (`Shapes.record`): it may be the
  * definition itself only on a value that is an instance of the definition's class too, and only
  * with the definition's arity, as the old front end reads it. Every expected count is derived in
  * its example's comment, and the scalameta front end is asked for the same rows.
  */
class NamesSpec extends Specification {

  def is = s2"""
  A member named like the measured definition
    is read where no instance has both classes, or arities differ $read
    is refused where it may be the definition itself, as before   $refused
  """

  private val text =
    """package nm
      |
      |trait Get { def get: Boolean }
      |trait Take { def get(b: Boolean): Boolean }
      |object Use { def get(g: Get): Boolean = g.get }
      |final class Holder { def get(t: Take): Boolean = true }
      |trait Cell { def value: Boolean; def value(c: Cell, b: Boolean): Boolean = b }
      |trait Node { def size: Boolean; def size(n: Node): Boolean }
      |trait Pair { def pick(b: Boolean): Boolean }
      |object Make { def pair(m: Pair): Pair = new Pair { def pick(b: Boolean): Boolean = m.pick(b) } }
      |""".stripMargin

  private lazy val rows = TastyFrontEndSpec.rows("Names.scala" -> text)

  // The last row of a name: an overload taking parameters is written after the member.
  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .filter(_.name.endsWith(name))
      .last
      .count

  // `Use` is an object no class extends: a `Get` is never it, and `g` is one of 2 Booleans, so
  // `Use.get` is one of 2^2 functions. `Holder` is final and no `Take`: `t` is one of the 2^2
  // functions of a Boolean, and `Holder.get` one of 2^4. In `Cell`, `value(c, b)` takes two
  // values and the member `value` none: the receiver's `value`, `c`'s and `b` are 2^3 inputs to
  // a Boolean, 2^8.
  // In `Make.pair`, the instance's `pick` reads `m`, built before the instance and so never it:
  // `m.pick` is one of 2^2 functions, and with `b` the instance's `pick` is one of 2^(4·2).
  private def read =
    List("Use.get", "Holder.get", "Cell.value", "<new#1>.pick").map(count) must_==
      List(4, 16, 256, 256).map(Count.Finite(_))

  // The abstract `Node.size(n)` is a member of the record `n` with its own name and arity: on `n`
  // it may be the definition again, called on another receiver. A concrete member nothing
  // overrides computes from the instance (`Cell.value` above) and is no such member.
  private def refused =
    count("Node.size").toString must contain("may be the measured declaration on its receiver")

}
