package cardinality.capacity

import org.specs2.Specification

// Rule B's factories on the stored-value side (v1.md §2, register D42): a class whose constructor
// is restricted holds the largest space a public factory builds, each at most min(its parameters,
// the fields a public member shows). Fixtures are valid Scala 3.9.0.
class StoredFactoriesSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    A class only factories build
      the largest space a factory builds, read through delegation    $largest
      fields no public member shows are one value                    $hidden
      no factory: no space                                           $noBuilder
  """

  private lazy val found = rows(
    """final class Pair private (val a: Boolean, val b: Boolean)
      |object Pair {
      |  def same(x: Boolean): Pair = new Pair(x, x)
      |  def fixed: Pair = build()
      |  private def build(): Pair = new Pair(true, false)
      |}
      |final class Mute private (private val a: Boolean)
      |object Mute { def of(a: Boolean): Mute = new Mute(a) }
      |final class Lone private (val a: Boolean)
      |""".stripMargin
  )

  // `Pair` stores 4 values; `same` builds min(|Boolean|, 4) = 2, `fixed` (read through `build`)
  // min(1, 4) = 1: the largest is 2.
  def largest = found(("Pair", Class)) === "2"

  // `Mute.of` builds min(2, |a|), but no public member reads `a`: one value.
  def hidden = found(("Mute", Class)) === "1"

  // `Lone` has neither a public constructor nor a factory.
  def noBuilder =
    found(("Lone", Class)) === "unresolved[receiver has no public constructor: Lone]"

}
