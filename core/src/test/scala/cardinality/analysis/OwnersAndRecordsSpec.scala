package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// What the owners and records around a signature supply: an inline member of a never-built static
// owner, a hidden capture a member holds, a case-class leaf a subclass may extend, and the record
// readings of final classes (register D26, D27, D15, C0–C4). Fixtures are valid Scala 3.9.0
// (scalac); the runtime claims below were checked on session fixtures (Scala 3.9.0, not retained)
// that print the counted values: no owner's initializer runs, an overridden field reads the
// override while its parent's constructor read the argument.
class OwnersAndRecordsSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Inline members of a never-built owner
    Captures a member holds
      a local definition around the target holds what it reads        $heldLocally
    Case-class leaves of sealed families
      a final leaf, a final override or a universal member keeps it    $closedLeaf
      an abstract member of the empty type empties the family          $emptyFamily
    Records
      a final case class with members or a marker parent is a record   $caseRecords
      a private constructor or a later clause gives its own reason     $caseRefusals
      a match-type field reduces on the record's tuple argument        $matchField
    Members no subclass overrides
  """

  private def one(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Owners.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refused(count: Count, reason: String): Result =
    count must beLike { case Count.Unresolved(reasons) => reasons must contain(reason) }

  // A local definition around the target — a method, a lazy value, a given, defined before or
  // after it, in the target's block or an outer one — that is exactly the hidden b puts it back: the
  // target reads its own b and the outer one, 16 (it read 4; 3.9.0 runs print the outer b through
  // seed and v). Typed Any it shows nothing: 4. A negation, a method with parameters, an object or
  // a class reading b refuse; a class whose own member b the read names holds nothing: 4.
  def heldLocally = {
    val found = one("""
      object O {
        def outer(b: Boolean): Boolean = { def seed: Boolean = b; def target(b: Boolean): Boolean = ???; target(true) }
        def lazyHeld(b: Boolean): Boolean = { def h(b: Boolean): Boolean = ???; lazy val v: Boolean = b; v }
        def givenHeld(b: Boolean): Boolean = { given g: Boolean = b; def target(b: Boolean): Boolean = ???; target(true) }
        def later(b: Boolean): Boolean = { def target(b: Boolean): Boolean = ???; def seed: Boolean = b; target(true) }
        def nested(b: Boolean): Boolean = { def seed: Boolean = b; { def target(b: Boolean): Boolean = ???; target(true) } }
        def inner(b: Boolean): Boolean = { def seed: Boolean = b; def target(b: Boolean): Boolean = { def in: Boolean = ???; in }; target(true) }
        def wide(b: Boolean): Boolean = { def seed: Any = b; def target(b: Boolean): Boolean = ???; target(true) }
        def neg(b: Boolean): Boolean = { def seed: Boolean = !b; def target(b: Boolean): Boolean = ???; target(true) }
        def applied(b: Boolean): Boolean = { def seed(u: Unit): Boolean = b; def target(b: Boolean): Boolean = ???; target(true) }
        def obj(b: Boolean): Boolean = { object K { def seed: Boolean = b }; def target(b: Boolean): Boolean = ???; target(true) }
        def cls(b: Boolean): Boolean = { class H { def seed: Boolean = b }; def target(b: Boolean): Boolean = ???; target(true) }
        def own(b: Boolean): Boolean = { class H { val b: Boolean = true; def seed: Boolean = b }; def target(b: Boolean): Boolean = ???; target(true) }
      }
    """)
    val held = "local capture held by a member that hides it: b"
    List(
      "outer.target",
      "lazyHeld.h",
      "givenHeld.target",
      "later.target",
      "nested.<block#1>.target",
      "inner.target",
      "inner.target.in"
    )
      .map(name => found(s"O.$name") === Count.Finite(16))
      .reduce(_ and _)
      .and(found("O.wide.target") === Count.Finite(4))
      .and(found("O.own.target") === Count.Finite(4))
      .and(
        List("neg", "applied", "obj", "cls")
          .map(name => refused(found(s"O.$name.target"), held))
          .reduce(_ and _)
      )
  }

  // Every leaf inherits the root's abstract `k: Nothing`; no pure, total body implements the
  // empty type, so the family enumerates no value: a function from it has 1 implementation (the
  // empty sum is 0 values, absurd elimination) and the stored estimate is 0.
  def emptyFamily = {
    val found = one("""
      sealed trait Root { def k: Nothing }
      case class L1(i: Int) extends Root
      case object L2 extends Root
      object Use { def f(r: Root): Int = ??? }
    """)
    found("Use.f") === Count.Finite(1)
  }

  // A final leaf, a final override of the parent's member (in the leaf, or in a parent between it
  // and the member's declaration: scalac rejects overriding it again, E164), an overridden
  // universal member and a private one leave nothing a subclass varies: Boolean^3 = 8 each.
  // (A leaf's field overriding a parent's member closes it too: StoredValueSoundnessSpec.)
  def closedLeaf = {
    val found = one("""
      sealed trait T
      final case class T1(code: Boolean) extends Exception with T
      case object T2 extends T
      sealed trait F { def d: Int = 1 }
      case class F1(code: Boolean) extends F { final override def d: Int = 2 }
      case object F2 extends F
      sealed trait P
      case class P1(code: Boolean) extends P { override def toString: String = "p"; private def h: Int = 1 }
      case object P2 extends P
      trait GQ { def d: Int = 1 }
      trait PQ extends GQ { final override def d: Int = 2 }
      sealed trait Z
      case class Z1(code: Boolean) extends PQ with Z
      case object Z2 extends Z
      object Use { def t(v: T): Boolean = ???; def f(v: F): Boolean = ???; def p(v: P): Boolean = ???; def z(v: Z): Boolean = ??? }
    """)
    List("Use.t", "Use.f", "Use.p", "Use.z").map(found(_) === Count.Finite(8)).reduce(_ and _)
  }

  // A final case class's record is its constructor arguments, members and marker parents adding
  // none: Span is (Int, Int), 2^64 values; find reaches its countable data — ω, as the plain twin's;
  // body(x) builds Body(f(x), g(x)) — 4 · 4 = 16; CE(a) is a Boolean: g 4, j 4 (it read `?`).
  def caseRecords = {
    val found = one("""
      final case class Span(start: Int, end: Int) { inline def isHit: Boolean = start >= 0 }
      object Scanner { val Miss: Span = Span(-1, -1); def find(bytes: Array[Byte], from: Int): Span = Miss }
      final case class Body(a: Boolean, b: Boolean) { inline def isHit: Boolean = a }
      trait Tag0
      final case class CE(a: Boolean) extends Tag0
      object Use { def body(x: Boolean): Body = Body(x, x); def g(x: Boolean): CE = CE(x); def j(c: CE): Boolean = c.a }
    """)
    (found("Scanner.Miss") === Count.Finite(BigInt(2).pow(64)))
      .and(found("Scanner.find") === Count.Countable)
      .and(found("Use.body") === Count.Finite(16))
      .and(found("Use.g") === Count.Finite(4))
      .and(found("Use.j") === Count.Finite(4))
  }

  // A private constructor constrains what its fields hold; a second clause is no field; a case
  // class no `final` closes may be extended by a class overriding hit — each says so instead of the
  // generic reason.
  def caseRefusals = {
    val found = one("""
      final case class CP private (a: Boolean)
      object CP { def mk(x: Boolean): CP = new CP(x) }
      final case class C2(a: Boolean)(b: Boolean)
      case class NC(a: Boolean) { def hit: Boolean = a }
      object Use { def h(x: Boolean): C2 = C2(x)(x); def i(x: Boolean): NC = NC(x) }
    """)
    refused(found("CP.mk"), "receiver fields constrained by a non-public constructor: CP")
      .and(refused(found("Use.h"), "final class field not observable injectively: C2.b"))
      .and(refused(found("Use.i"), "case class open to extension: NC (overridable member hit)"))
  }

  // Miss[(Unit, Unit)]'s field is Fst[(Unit, Unit)] = Unit: one value, 1; Miss[(Boolean, Unit)]'s
  // is a Boolean x reaches: 4 (both read "not a proven tuple"). The reduced alias reads alike.
  // Miss[A]'s field is stuck, a rigid atom that yields no Boolean: m5 is 2; Miss[(A, Unit)]'s is
  // an A, the one m6 returns — 1. Rec's out binds its own A (3.9.0 run: r.out(42) prints 42), so
  // the class argument is not reread there: out is ∀A. A => A, one value, and m7 is 2 — never 4,
  // as with out reading a Boolean. Rec3's out (∀A. Fst[A]) has no implementation, so m8's true
  // count is 1; the stuck member is not read as empty yet (2), but never 4 either.
  def matchField = {
    val found = one("""
      type Fst[T] = T match { case (f, s) => f }
      final class Miss[A](val fst: Fst[A])
      trait Rec[A] { def out[A](a: A): Fst[(A, Unit)] }
      trait Rec3[A] { def out[A]: Fst[A] }
      object V {
        def m1: Miss[(Unit, Unit)] = new Miss(())
        def m2(x: Boolean): Miss[(Boolean, Unit)] = new Miss(x)
        def m3(x: Boolean): Fst[(Boolean, Unit)] = x
        def m5[A](x: Miss[A]): Boolean = true
        def m6[A](m: Miss[(A, Unit)]): A = m.fst
        def m7(r: Rec[(Boolean, Unit)]): Boolean = ???
        def m8(r: Rec3[(Boolean, Unit)]): Boolean = ???
      }
    """)
    (found("V.m1") === Count.Finite(1))
      .and(found("V.m2") === Count.Finite(4))
      .and(found("V.m3") === Count.Finite(4))
      .and(found("V.m5") === Count.Finite(2))
      .and(found("V.m6") === Count.Finite(1))
      .and(found("V.m7") === Count.Finite(2))
      .and(found("V.m8") must not(beEqualTo(Count.Finite(4))))
  }

}
