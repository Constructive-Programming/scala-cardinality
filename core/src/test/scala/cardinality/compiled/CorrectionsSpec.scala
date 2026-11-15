package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.TinySize
import cardinality.reporting.Report
import org.specs2.Specification

/** Wrong numbers found in both front ends and corrected (docs/plans/compiled-own-types.md, group
  * h7). Every expected count is derived in its example's comment.
  */
class CorrectionsSpec extends Specification {

  def is = s2"""
  A count earlier readings got wrong
    a hidden field module code outside the class reads is observed $outside
    a member private to the instance is read on `this` alone       $local
    a concrete class nothing extends is final, written open or not $open
    an enum whose construction throws has no value on either side  $thrown
    a class never built stores nothing, nor does a class inside it $unbuilt
    a member only a never-built subclass overrides does not vary   $dormant
  """

  private val text =
    """package fix {
      |  final class Comp private (a: Boolean, private val b: Boolean) { def getA: Boolean = a }
      |  object Comp {
      |    def make(a: Boolean, b: Boolean): Comp = new Comp(a, b)
      |    def getB(c: Comp): Boolean = c.b
      |  }
      |  trait Shows { protected def peek: Boolean; def show: Boolean = peek }
      |  final class Par private (b: Boolean) extends Shows { protected def peek: Boolean = b }
      |  object Par { def make(b: Boolean): Par = new Par(b) }
      |  trait TB { protected def b: Boolean; def show: Boolean = b }
      |  final class Impl private (protected val b: Boolean) extends TB
      |  object Impl { def make(b: Boolean): Impl = new Impl(b) }
      |  final class Pk private (val a: Boolean, private[fix] val b: Boolean)
      |  object Pk { def make(a: Boolean, b: Boolean): Pk = new Pk(a, b) }
      |  object Peek { def b(p: Pk): Boolean = p.b }
      |  final class Shut private (val a: Boolean, private val b: Boolean)
      |  object Shut {
      |    def make(a: Boolean, b: Boolean): Shut = new Shut(a, b)
      |    private def hidden(s: Shut): Boolean = s.b
      |  }
      |  final class Coll private (a: Boolean, b: Boolean) { private def get: Boolean = b; def getA: Boolean = a }
      |  object Coll { def make(a: Boolean, b: Boolean): Coll = new Coll(a, b) }
      |  object Util { def x(m: Map[Int, Int]): Option[Int] = m.get(1) }
      |  class PB(b: Int) { def pb: Int = b }
      |  final class Xb private (a: Boolean, b: Boolean) extends PB(0) { def getA: Boolean = a }
      |  object Xb { def make(a: Boolean, b: Boolean): Xb = new Xb(a, b) }
      |  final class Pq private (val a: Boolean, private[fix] val b: Boolean)
      |  object Pq { def make(a: Boolean, b: Boolean): Pq = new Pq(a, b) }
      |  final case class Other(b: Boolean)
      |  object Peek2 { def f(o: Other): Boolean = o.b }
      |  final class PT(a: Boolean) { private[this] val x = a; def m(o: PT): Boolean = true }
      |  final class PV(a: Boolean) { private val x = a; def m(o: PV): Boolean = true }
      |  open class OpJ extends Exception { def m(x: Boolean): Boolean = true }
      |  class ClJ extends Exception { def m(x: Boolean): Boolean = true }
      |  trait DR { def m: Boolean = true; def g(x: Boolean): Boolean = x }
      |  class DRSub extends DR { override def m: Boolean = false; val z: Int = ??? }
      |  trait DM { def m: Boolean = true; def g(x: Boolean): Boolean = x }
      |  class DMSub extends DM { override def m: Boolean = false; val z: Int = ??? }
      |  class DMLive extends DM { override def m: Boolean = false }
      |  class LL { val x: Int = ???; def m(b: Boolean): Boolean = b }
      |  class LLX extends LL { override def m(b: Boolean): Boolean = !b }
      |  enum Boom { case A, B; val z: Int = ??? }
      |  enum BoomC { case C(b: Boolean); case D; val z: Int = ??? }
      |  enum Arg2(val n: Int) { case One extends Arg2(???); case Two extends Arg2(2) }
      |  final class Dead { val z: Int = ???; final case class Leaf(b: Boolean) }
      |  final class Live { final case class Leaf(b: Boolean) }
      |  object DeadO { val z: Int = ???; final case class In(b: Boolean) }
      |  final case class HoldsLeaf(l: Dead#Leaf, b: Boolean)
      |  trait DT { def m: Boolean = true; def k: Boolean }
      |  class DTSub extends DT { override def m: Boolean = false; def k = true; val z: Int = ??? }
      |  abstract class DA { def m: Boolean = true; def k: Boolean }
      |  class DASub extends DA { override def m: Boolean = false; def k = true; val z: Int = ??? }
      |  final class Ko private (val a: Boolean, private[fix] val b: Boolean)
      |  object Ko { def make(a: Boolean, b: Boolean): Ko = new Ko(a, b) }
      |  trait Obs { def f(k: Ko): Boolean }
      |  object PO { private class Impl extends Obs { def f(k: Ko): Boolean = k.b }; def obs: Obs = new Impl }
      |  object Here {
      |    def boom: Boom = ???
      |    def boomC: BoomC = ???
      |    def arg2: Arg2 = ???
      |    def dt(t: DT): Boolean = true
      |    def da(a: DA): Boolean = true
      |  }
      |}
      |package out {
      |  import fix.*
      |  object Use {
      |    def comp(c: Comp): Boolean = true
      |    def par(p: Par): Boolean = true
      |    def impl(i: Impl): Boolean = true
      |    def pk(p: Pk): Boolean = true
      |    def shut(s: Shut): Boolean = true
      |    def coll(c: Coll): Boolean = true
      |    def pq(p: Pq): Boolean = true
      |    def xb(x: Xb): Boolean = true
      |    def ko(k: Ko): Boolean = true
      |  }
      |}
      |""".stripMargin

  private lazy val rows: List[Report.Source] =
    TastyFrontEndSpec.rows("Fix.scala" -> text).sources.flatMap(_.toOption)

  // A row's field.
  private def field[R, A](of: Report.Source => List[R], name: R => String, field: R => A)(
      wanted: String
  ): A = field(rows.flatMap(of).find(r => name(r) == wanted).get)

  private def count(name: String): Count = field(_.methods, _.name, _.count)(name)
  private def size(name: String) = field(_.definitions, _.name, _.size)(name)
  private def counts(name: String, expected: Count) = count(name) must_== expected
  private def next(name: String): Count = count(name)

  private def unread(count: Count, reason: String) =
    count must beLike { case Count.Unresolved(List(why)) => why must contain(reason) }

  // A final class whose constructor the site may not call is what its factories build, over the
  // fields the site observes (rule B). Code outside the class that the site may call observes a
  // field it reads: `Comp.getB` reads `b`, so `Comp` is its 2 × 2 fields (`make` builds each) and
  // `comp` one of 2^4; `Shows.show` reads `peek`, which `Par` implements with `b`: `Par` is 2, `par`
  // one of 2^2; `TB.show` reads `b`, which `Impl`'s field implements: `impl` one of 2^2;
  // `Peek.b` reads the `private[fix]` field `b`: `Pk` is 2 × 2, `pk` one of 2^4. A reader the site
  // may not reach observes nothing: `Shut` is its `a`, `shut` one of 2^2; so does a name that is
  // another member's (`Util.x`'s `get`, `Peek2.f`'s `b`, `PB`'s own `b`): `coll`, `pq` and `xb` are
  // each one of 2^2.
  // The stored side reads the companion: `Comp` holds 2 × 2, in both front ends (rule B on the
  // stored side, tasty-h5).
  private def outside =
    counts("out.Use.comp", Count.Finite(16))
      .and(counts("out.Use.par", Count.Finite(4)))
      .and(counts("out.Use.impl", Count.Finite(4)))
      .and(counts("out.Use.pk", Count.Finite(16)))
      .and(counts("out.Use.shut", Count.Finite(4)))
      .and(counts("out.Use.coll", Count.Finite(4)))
      .and(counts("out.Use.pq", Count.Finite(4)))
      .and(counts("out.Use.xb", Count.Finite(4)))
      .and(size("fix.Comp") must_== Some(TinySize(4)))
      // `PO.obs.f(k)` runs the private `Impl.f`, which reads `b`: through the public `Obs.f` the
      // site reaches it, so `Ko` is 2 × 2 and `ko` one of 2^4 (the old front end misses it, 2^2).
      .and(next("out.Use.ko") must_== Count.Finite(16))

  // TASTy pickles an explicit `private[this]` member and a `private` one the compiler found read on
  // `this` alone the same. `PT.m` may not select `o.x` (its receiver's `a`, 2^2), `PV.m` may
  // (2^(2 × 2)): neither argument is read, and so the old front end reads them.
  private def local =
    unread(next("fix.PT.m"), "a parameter that is no public field")
      .and(unread(next("fix.PV.m"), "a parameter that is no public field"))

  // A concrete class nothing extends is final, written `open` or not: `OpJ` reads its parent
  // compiled from Java as `ClJ` does, and `m` is one of the 2^2 functions of `x`.
  private def open =
    (next("fix.OpJ.m") must_== Count.Finite(4)).and(counts("fix.ClJ.m", Count.Finite(4)))

  // An enum whose body throws builds no case (D26): `Boom` and `BoomC` have no value, a method
  // returning one has no body (0), and the stored side's rows are 0 too. `Arg2`'s value cases are
  // built together by its companion, and `One`'s clause throws: no case is built, 0.
  private def thrown =
    (next("fix.Here.boom") must_== Count.Finite(0))
      .and(next("fix.Here.boomC") must_== Count.Finite(0))
      .and(next("fix.Here.arg2") must_== Count.Finite(0))
      .and(
        List("fix.Boom", "fix.BoomC", "fix.Arg2").map(size) must_== List.fill(3)(Some(TinySize(0)))
      )

  // A class whose initializer always throws is never built (D26, decision D5): `Dead` stores no
  // value (it was 1), and neither does `Dead#Leaf`, whose every instance needs a `Dead` around it
  // (it was 2); `HoldsLeaf` holds one, so it stores none either (it was 2 × 2). `Live#Leaf` is its
  // Boolean, 2. An object around a class is not read (register D26): `DeadO.In` is its Boolean, 2.
  private def unbuilt =
    (List("fix.Dead", "fix.Dead#Leaf", "fix.HoldsLeaf").map(size) must_== List.fill(3)(
      Some(TinySize(0))
    ))
      .and(List("fix.Live#Leaf", "fix.DeadO.In").map(size) must_== List.fill(2)(Some(TinySize(2))))

  // `DRSub` is never built, so its override of `m` never runs: on every `DR`, `m` is `true`, and
  // `g` is one of the 2^2 functions of `x` (the old front end lets `m` vary: 2^(2 × 2)). `DMLive`
  // is built and overrides `m`, which varies: `DM.g` is one of 2^(2 × 2). A record of a trait or
  // abstract class something never built extends is not read yet (`DT`, `DA`, which the old front
  // end counts with `m` varying, 2^(2 × 2)). `LLX` is built but for `LL.x`'s initializer, which is
  // the choice being counted: `m` varies, and `x` is an Int computed from it, (2^32)^(2^2) = 2^128.
  private def dormant =
    (next("fix.DR.g") must_== Count.Finite(4))
      .and(next("fix.DM.g") must_== Count.Finite(16))
      .and(counts("fix.LL.x", Count.Finite(BigInt(2).pow(128))))
      .and(unread(next("fix.Here.dt"), "a class something extends that is never built"))
      .and(unread(next("fix.Here.da"), "a class something extends that is never built"))

}
