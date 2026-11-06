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
      a private primary is ignored outside its class for its secondary     $privateSecondary
      several public secondaries: the largest space they build             $largestSecondary
      a secondary over a larger space builds no more than the fields hold  $widerSecondary
      a non-public class or trait is read as public                        $nonPublicClass
      no public constructor: its factory builds it                         $noPublic
      a public secondary over a type parameter: not compared               $parametric
    Public factories build a class no constructor the site may call builds
      two factories: the largest space they build, not their sum           $twoFactories
      a factory whose body is read as no construction counts 1             $unreadFactory
      no factory and no public constructor: no space                       $noFactory
      fields nothing the site may call reads: one value                    $hiddenFields
      a factory read through one and two delegating calls, not three       $delegation
    Sealed families
      a leaf is a summand where a constructor of it may be called      $sealedLeaves
      a hidden leaf built inside a family-typed factory is its space   $familyFactory
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
      |  def gen[A](x: Gen[A]): A = ???
      |}
      |""".stripMargin

  private lazy val found = rows(code)

  // `Two` has two public constructors; the secondary's `Unit` builds one of the primary's two
  // values, so the largest is the primary's: `b` holds 2 values, `get` is 2^2 = 4, and so is a
  // function of a `Two` record (its field `b`).
  def publicPrimary =
    (found("rc.Two.get") === Count.Finite(4)).and(found("rc.Use.two") === Count.Finite(4))

  // Outside `One`, its private primary is no constructor the caller may call; `this(u: Unit)`
  // builds min(|Unit|, |Boolean|) = 1 receiver, so a function of a `One` is 2^1 = 2. Inside `One`
  // the primary is callable: `get` reads `b` over both values, 2^2 = 4.
  def privateSecondary =
    (found("rc.Use.one") === Count.Finite(2)).and(found("rc.One.get") === Count.Finite(4))

  // `Max`'s fields hold 4 values, its public secondaries build min(1, 4) = 1 and min(2, 4) = 2: the
  // largest is 2, not their sum 3 nor the fields' 4: a function of a `Max` outside it is 2^2 = 4.
  // Inside, the primary builds all 4: `get` is 2^4 = 16.
  def largestSecondary =
    (found("rc.Use.max") === Count.Finite(4)).and(found("rc.Max.get") === Count.Finite(16))

  // `Wide`'s `this(s: String)` takes ω arguments, but builds at most the fields' 2 values: `get` is
  // 2^2 = 4.
  def widerSecondary = found("rc.Wide.get") === Count.Finite(4)

  // A `private[rc]` class or trait has no use site outside `rc`: its restriction is disregarded and
  // its primary is public, so `b` holds 2 values and `get` is 4.
  def nonPublicClass =
    (found("rc.Pkg.get") === Count.Finite(4))
      .and(found("rc.Use.pkg") === Count.Finite(4))
      .and(found("rc.Tr.get") === Count.Finite(4))

  // Outside `Factory` only its factory `Factory.on` builds one: no parameters, min(1, 2) = 1 value
  // (`b` is always true), so a function of a `Factory` is 2^1 = 2. Inside, the private primary is
  // callable: 4.
  def noPublic =
    (found("rc.Use.factory") === Count.Finite(2)).and(found("rc.Factory.get") === Count.Finite(4))

  // Outside `Gen`, its public secondary takes an `A`, a choice of supplied values, not a value
  // space.
  def parametric =
    refused(
      found("rc.Use.gen"),
      "receiver space of its public constructors and factories not compared: rc.Gen"
    )

  private val factories = rows(
    """package fy
      |final case class Pair private (a: Boolean, b: Boolean)
      |object Pair {
      |  def same(x: Boolean): Pair = new Pair(x, x)
      |  def fixed: Pair = { val p = Pair(true, false); p }
      |}
      |final class Cached private (val b: Boolean)
      |object Cached {
      |  private val one = new Cached(true)
      |  def make(s: String): Cached = one
      |}
      |final class Hidden private (val b: Boolean)
      |object Hidden { def copy(h: Hidden): Hidden = h }
      |final class None private (val b: Boolean)
      |object Use {
      |  def pair(x: Pair): Boolean = ???
      |  def cached(x: Cached): Boolean = ???
      |  def hidden(x: Hidden): Boolean = ???
      |  def none(x: None): Boolean = ???
      |}
      |""".stripMargin
  )

  // `Pair`'s fields hold 4 values; `same` builds min(|Boolean|, 4) = 2 of them (through `new`),
  // `fixed` min(1, 4) = 1 (a case class `apply` through a local `val`). The class holds the
  // largest, 2 (not the sum 3, not the fields' 4): a function of a `Pair` is 2^2 = 4.
  def twoFactories = factories("fy.Use.pair") === Count.Finite(4)

  // `Cached.make` declares a `Cached` but returns a value it did not build (its private `one`, so
  // `b` is always true): a factory whose body is read as no construction, 1 value. A function of
  // a `Cached` is 2^1 = 2. `Hidden.copy` is one too: 2^1 = 2.
  def unreadFactory =
    (factories("fy.Use.cached") === Count.Finite(2))
      .and(factories("fy.Use.hidden") === Count.Finite(2))

  private val delegating = rows(
    """package fz
      |final class Mute private (private val a: Boolean)
      |object Mute { def of(a: Boolean): Mute = new Mute(a) }
      |final class Told private (private val a: Boolean) { def get: Boolean = a }
      |object Told { def of(a: Boolean): Told = new Told(a) }
      |final class Half private (val a: Boolean, b: Boolean) { private def peek: Boolean = b }
      |object Half { def of(a: Boolean, b: Boolean): Half = new Half(a, b) }
      |final class One private (val b: Boolean)
      |object One { def of(b: Boolean): One = build(b); private def build(b: Boolean): One = new One(b) }
      |final class Two private (val b: Boolean)
      |object Two {
      |  def of(b: Boolean): Two = l1(b)
      |  private def l1(b: Boolean): Two = l2(b)
      |  private def l2(b: Boolean): Two = new Two(b)
      |}
      |final class Three private (val b: Boolean)
      |object Three {
      |  def of(b: Boolean): Three = l1(b)
      |  private def l1(b: Boolean): Three = l2(b)
      |  private def l2(b: Boolean): Three = l3(b)
      |  private def l3(b: Boolean): Three = new Three(b)
      |}
      |object Use {
      |  def mute(x: Mute): Boolean = ???
      |  def told(x: Told): Boolean = ???
      |  def half(x: Half): Boolean = ???
      |  def one(x: One): Boolean = ???
      |  def two(x: Two): Boolean = ???
      |  def three(x: Three): Boolean = ???
      |}
      |""".stripMargin
  )

  // `Mute.of` builds min(2, |a|) values, but nothing `Use` may call reads `a`: its values are not
  // told apart there, so the fields it observes are none, one value, and `mute` is 2^1 = 2.
  // `Told.get` reads `a`: 2 values, `told` 2^2 = 4. `Half` shows `a` (a public field) and hides `b`
  // (read only by a private member): 2 observed values of `of`'s 4 arguments, `half` 2^2 = 4.
  def hiddenFields =
    (delegating("fz.Use.mute") === Count.Finite(2))
      .and(delegating("fz.Use.told") === Count.Finite(4))
      .and(delegating("fz.Use.half") === Count.Finite(4))

  // `One.of` returns `build(b)`, which constructs: read through it, min(|Boolean|, 2) = 2 values,
  // `one` 2^2 = 4. `Two.of` reaches `new` through `l1` and `l2`, two calls: 4. `Three.of` needs a
  // third call (`l3`), past the depth followed: a factory whose body is not read, 1 value,
  // `three` 2^1 = 2.
  def delegation =
    (delegating("fz.Use.one") === Count.Finite(4))
      .and(delegating("fz.Use.two") === Count.Finite(4))
      .and(delegating("fz.Use.three") === Count.Finite(2))

  // `None` has neither a public constructor nor a factory: no space.
  def noFactory = refused(factories("fy.Use.none"), "receiver has no public constructor: fy.None")

  // `S` sums a private leaf (its restriction disregarded: 2 values) and `Z` (1): 3, so `f` is
  // 2^3 = 8. `L`'s leaf `M` has only a private constructor: no space, so `g` has none either. `R`'s
  // leaf `Q` has a `private[sl]` constructor: callable inside `sl`, where `h` is 2^(2+1) = 8, and
  // not from `out`.
  def sealedLeaves = {
    val found = counts(
      "a.scala" -> """package sl
        |sealed trait S
        |private final case class P(b: Boolean) extends S
        |case object Z extends S
        |sealed trait L
        |final case class M private (b: Boolean) extends L
        |sealed trait R
        |final case class Q private[sl] (b: Boolean) extends R
        |case object Y extends R
        |object Use { def f(s: S): Boolean = ???; def g(l: L): Boolean = ???; def h(r: R): Boolean = ??? }
        |""".stripMargin,
      "b.scala" -> "package out\nobject Use { def h(r: sl.R): Boolean = ??? }"
    )
    (found("sl.Use.f") === Count.Finite(8))
      .and(refused(found("sl.Use.g"), "receiver has no public constructor: sl.M"))
      .and(found("sl.Use.h") === Count.Finite(8))
      .and(refused(found("out.Use.h"), "receiver has no public constructor: sl.Q"))
  }

  // `V = E + Two`, `Two` built only inside `fx` (its constructor and fields are `private[fx]`), as
  // eo's `PSVec.Slice` is. From `out` the family-typed `V.of` is `Two`'s factory, and `out` observes
  // only `a` (through `first`; `b` nothing it may call reads): the `else` branch builds
  // min(|Boolean|, 2) = 2 of the values `out` tells apart, so `V` is 1 + 2 = 3 and `f` is 2^3 = 8. Inside
  // `fx` the constructor is callable and the fields readable: 1 + 4 = 5, `g` is 2^5 = 32.
  def familyFactory = {
    val found = counts(
      "a.scala" -> """package fx
        |sealed trait V
        |object V {
        |  case object E extends V
        |  final class Two private[fx] (private[fx] val a: Boolean, private[fx] val b: Boolean) extends V {
        |    def first: Boolean = a
        |  }
        |  def of(a: Boolean): V = if (a) E else new V.Two(a, true)
        |}
        |object Use { def g(v: V): Boolean = ??? }
        |""".stripMargin,
      "b.scala" -> "package out\nobject Use { def f(v: fx.V): Boolean = ??? }"
    )
    (found("out.Use.f") === Count.Finite(8)).and(found("fx.Use.g") === Count.Finite(32))
  }

}
