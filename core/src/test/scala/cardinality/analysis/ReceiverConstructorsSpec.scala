package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Rule B (v1.md §2, receiver constructors): a receiver holds what its public constructors build,
  * the largest space any of them builds. Every fixture is valid Scala 3.9.0; each expected number
  * is derived in the comment beside it.
  */
class ReceiverConstructorsSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Public constructors build the receiver
      a public primary builds every field value, whatever else is public   $publicPrimary
      a private primary is ignored for its public secondary                $privateSecondary
      several public secondaries: the largest space they build             $largestSecondary
      a secondary over a larger space builds no more than the fields hold  $widerSecondary
      a non-public class or trait is read as public                        $nonPublicClass
      no public constructor: no space                                      $noPublic
      a public secondary over a type parameter: not compared               $parametric
    Sealed families
      a non-public leaf is a summand; one with no public constructor no    $sealedLeaves
  """

  private val code =
    """package rc
      |final class Two(val b: Boolean) { def this(u: Unit) = this(true); def get: Boolean = b }
      |final class One private (val b: Boolean) { def this(u: Unit) = this(true); def get: Boolean = b }
      |final class Max private (val b: Boolean, val c: Boolean) {
      |  def this(u: Unit) = this(true, true)
      |  def this(b: Boolean) = this(b, b)
      |  def get: Boolean = b
      |}
      |final class Wide private (val b: Boolean) { def this(s: String) = this(s.isEmpty); def get: Boolean = b }
      |private[rc] final class Pkg(val b: Boolean) { def get: Boolean = b }
      |private[rc] trait Tr(val b: Boolean) { def get: Boolean = b }
      |final class Factory private (val b: Boolean) { def get: Boolean = b }
      |object Factory { def on: Factory = new Factory(true) }
      |final class Gen[A] private (val a: A) { def this(u: Unit, a: A) = this(a); def get: A = a }
      |object Use {
      |  def two(x: Two): Boolean = ???
      |  def one(x: One): Boolean = ???
      |  def max(x: Max): Boolean = ???
      |  def pkg(x: Pkg): Boolean = ???
      |  def factory(x: Factory): Boolean = ???
      |}
      |""".stripMargin

  private lazy val found = rows(code)

  // `Two` has two public constructors; the secondary's `Unit` builds one of the primary's two
  // values, so the largest is the primary's: `b` holds 2 values, `get` is 2^2 = 4, and so is a
  // function of a `Two` record (its field `b`).
  def publicPrimary =
    (found("rc.Two.get") === Count.Finite(4)).and(found("rc.Use.two") === Count.Finite(4))

  // `One`'s private primary is ignored; `this(u: Unit)` builds min(|Unit|, |Boolean|) = 1 receiver:
  // `get` is 2^1 = 2, and so is a function of a `One`.
  def privateSecondary =
    (found("rc.One.get") === Count.Finite(2)).and(found("rc.Use.one") === Count.Finite(2))

  // `Max`'s fields hold 4 values, its public secondaries build min(1, 4) = 1 and min(2, 4) = 2: the
  // largest is 2, not their sum 3 nor the fields' 4. `get` is 2^2 = 4 (16 over every field value).
  def largestSecondary =
    (found("rc.Max.get") === Count.Finite(4)).and(found("rc.Use.max") === Count.Finite(4))

  // `Wide`'s `this(s: String)` takes ω arguments, but builds at most the fields' 2 values: `get` is
  // 2^2 = 4.
  def widerSecondary = found("rc.Wide.get") === Count.Finite(4)

  // A `private[rc]` class or trait has no use site outside `rc`: its restriction is disregarded and
  // its primary is public, so `b` holds 2 values and `get` is 4.
  def nonPublicClass =
    (found("rc.Pkg.get") === Count.Finite(4))
      .and(found("rc.Use.pkg") === Count.Finite(4))
      .and(found("rc.Tr.get") === Count.Finite(4))

  // Only the factory `Factory.on` builds a `Factory` (`b` is always true): no public constructor
  // says which values its fields hold.
  def noPublic =
    refused(found("rc.Factory.get"), "receiver has no public constructor: rc.Factory")
      .and(refused(found("rc.Use.factory"), "receiver has no public constructor: rc.Factory"))

  // `Gen`'s public secondary takes an `A`, a choice of supplied values, not a value space.
  def parametric =
    refused(
      found("rc.Gen.get"),
      "receiver space of its public secondary constructors not compared: rc.Gen"
    )

  // `S` sums a private leaf (its restriction disregarded: 2 values) and `Z` (1): 3, so `f` is
  // 2^3 = 8. `L`'s leaf `M` has only a private constructor: no space, so `g` has none either.
  def sealedLeaves = {
    val found = rows("""package sl
      |sealed trait S
      |private final case class P(b: Boolean) extends S
      |case object Z extends S
      |sealed trait L
      |final case class M private (b: Boolean) extends L
      |object Use { def f(s: S): Boolean = ???; def g(l: L): Boolean = ??? }
      |""".stripMargin)
    (found("sl.Use.f") === Count.Finite(8))
      .and(refused(found("sl.Use.g"), "receiver has no public constructor: sl.M"))
  }

}
