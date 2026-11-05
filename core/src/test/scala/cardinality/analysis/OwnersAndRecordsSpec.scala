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
      a static object's, a given object's or a file's inline member    $staticInline
      a class's, a member object's or a plain def keeps one call      $builtOwners
    Captures a member holds
      a readable field or own member that is the capture, or of type 1 $heldAlias
      any other reading, a wider type or an overridden field, refuses  $heldOtherwise
      a local definition around the target holds what it reads        $heldLocally
    Case-class leaves of sealed families
      a final leaf, a final override or a universal member keeps it    $closedLeaf
      an abstract member of the empty type empties the family          $emptyFamily
    Records
      a final case class with members or a marker parent is a record   $caseRecords
      a private constructor or a later clause gives its own reason     $caseRefusals
      a match-type field reduces on the record's tuple argument        $matchField
    Members no subclass overrides
      a given or a final member an inline body selects is fixed        $fixedSelection
      a deferred given is abstract: every class extending it gives one $deferredGiven
      a `deferred` nothing imports is no compiletime one               $deferredBare
  """

  private def one(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Owners.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refused(count: Count, reason: String): Result =
    count must beLike { case Count.Unresolved(reasons) => reasons must contain(reason) }

  // Each owner's body throws, yet an expansion builds none (3.9.0 runs: no `K init`, nor `G init`
  // for a `given … with` object): each inline member is counted as in a built owner — !b among b's
  // functions: 4; b && c: 16; pick[A]: y or x, 2; an inline given Int: 2^32; nest's h reads c and
  // the capture b: 16. Before: 1 each. r reads the owner's z, which supplies nothing (an object
  // declares no slot): b's functions, 4 — never 1, nor 16 as with z a free Boolean.
  def staticInline = {
    val found = one("""
      object K {
        val z: Boolean = ???
        inline def f(b: Boolean): Boolean = !b
        inline def two(b: Boolean, c: Boolean): Boolean = b && c
        inline def pick[A](x: A, y: A): A = y
        extension (b: Boolean) inline def flip: Boolean = !b
        inline given g: Int = 7
        inline def nest(b: Boolean): Boolean = { def h(c: Boolean): Boolean = !c; h(b) }
      }
      object Outer { object M { val z: Int = ???; inline def f(b: Boolean): Boolean = b } }
      val topZ: Boolean = ???
      inline def topF(b: Boolean): Boolean = !b
      object R { val z: Boolean = ???; inline def r(b: Boolean): Boolean = b && z }
      trait Base
      given G: Base with { val z: Int = ???; inline def f(b: Boolean): Boolean = b }
    """)
    val flip = found.collectFirst { case (name, count) if name.endsWith(".flip") => count }
    List("K.f" -> 4, "K.two" -> 16, "K.pick" -> 2, "K.nest" -> 4)
      .map((name, n) => found(name) === Count.Finite(n))
      .reduce(_ and _)
      .and(flip must beSome(Count.Finite(4)))
      .and(found("K.g") === Count.Finite(BigInt(2).pow(32)))
      .and(found("K.nest.h") === Count.Finite(16))
      .and(found("Outer.M.f") === Count.Finite(4))
      .and(found("topF") === Count.Finite(4))
      .and(found("R.r") === Count.Finite(4))
      .and(found("G.f") === Count.Finite(4))
  }

  // A class or trait owner is the receiver every call needs, and a member object of one is built
  // with it: never built, no call returns — 1. A def that is not inline needs its owner too.
  def builtOwners = {
    val found = one("""
      class C { val z: Boolean = ???; inline def f(b: Boolean): Boolean = b; object N { inline def f(b: Boolean): Boolean = b } }
      object K { val z: Boolean = ???; def g(b: Boolean): Boolean = !b }
    """)
    (found("C.f") === Count.Finite(1))
      .and(found("C.N.f") === Count.Finite(1))
      .and(found("K.g") === Count.Finite(1))
  }

  // Inside each anonymous or local class the capture n (or b, or a1) is hidden — by the inherited
  // field n, an own member n, or m's own parameter — yet a field the template reads passed exactly
  // it, or a member that is it, holds it: m reads b and n — 16 (it read 4). mk3 and viaField: a1,
  // a2 and the held outer a1 — 3 (they read 2). lit passes true: n is unreachable and the field
  // constant — 4. A holder typed Any (a field, a binder given Any, a member) shows nothing of the
  // capture (D-top): m reads b alone — 4; wide's m reads a1 and a2 — 2.
  def heldAlias = {
    val found = one("""
      class P(val n: Boolean) { def m(b: Boolean): Boolean = b }
      class Q(val k: Boolean) { def m(b: Boolean): Boolean = b }
      trait T { def m(b: Boolean): Boolean }
      trait U[A] { def m(a1: A): A }
      class F[A](val f: A) { def m(a1: A): A = a1 }
      object O {
        def mk(n: Boolean): P = new P(n) { override def m(b: Boolean): Boolean = !b }
        def otherName(n: Boolean): Q = new Q(n) { def n(u: Unit): Unit = u; override def m(b: Boolean): Boolean = !b }
        def byParam(b: Boolean): Q = new Q(b) { override def m(b: Boolean): Boolean = !b }
        def ownAlias(b: Boolean): T = new T { def seed: Boolean = b; def m(b: Boolean): Boolean = !b }
        def viaLocal(n: Boolean): Boolean = { final class Impl extends P(n) { def g(y: Boolean): Boolean = y }; true }
        def lit(n: Boolean): P = new P(true) { override def m(b: Boolean): Boolean = b }
        def mk3[A](a1: A, a2: A): U[A] = new U[A] { def seed: A = a1; def m(a1: A): A = a1 }
        def viaField[A](a1: A, a2: A): F[A] = new F[A](a1) { override def m(a1: A): A = a1 }
        def wideField(n: Boolean): PA = new PA(n) { override def m(b: Boolean): Boolean = !b }
        def wideArg(n: Boolean): PT[Any] = new PT[Any](n) { override def m(b: Boolean): Boolean = !b }
        def wideAlias(b: Boolean): T = new T { def seed: Any = b; def m(b: Boolean): Boolean = !b }
        def wide[A](a1: A, a2: A): U[A] = new U[A] { def seed: Any = a1; def m(a1: A): A = a1 }
      }
      class PA(val n: Any) { def m(b: Boolean): Boolean = b }
      class PT[X](val n: X) { def m(b: Boolean): Boolean = b }
    """)
    List("mk", "otherName", "byParam", "ownAlias")
      .map(name => found(s"O.$name.<new#1>.m") === Count.Finite(16))
      .reduce(_ and _)
      .and(
        List("wideField", "wideArg", "wideAlias")
          .map(name => found(s"O.$name.<new#1>.m") === Count.Finite(4))
          .reduce(_ and _)
      )
      .and(found("O.wide.<new#1>.m") === Count.Finite(2))
      .and(found("O.viaLocal.Impl.g") === Count.Finite(16))
      .and(found("O.lit.<new#1>.m") === Count.Finite(4))
      .and(found("O.mk3.<new#1>.m") === Count.Finite(3))
      .and(found("O.viaField.<new#1>.m") === Count.Finite(3))
  }

  // neg's field holds !n, a function of the hidden n the reading does not invert; H's private h
  // is read by peek and holds the hidden n: both refuse (neg read 4). overField's template
  // overrides the field, so this.n is the constant — but P's constructor read the argument it was
  // given (a strict val of P computed from n keeps it: 3.9.0 run), so it refuses (it read 16).
  // some's field is an Option[Boolean] holding the capture: a wider type than the capture's, not
  // read — refused.
  def heldOtherwise = {
    val found = one("""
      class P(val n: Boolean) { def m(b: Boolean): Boolean = b }
      class PO(val n: Option[Boolean]) { def m(b: Boolean): Boolean = b }
      abstract class H(h: Boolean) { def peek: Boolean = h; def m(b: Boolean): Boolean }
      object O {
        def neg(n: Boolean): P = new P(!n) { override def m(b: Boolean): Boolean = !b }
        def hidden(b: Boolean): H = new H(b) { def m(b: Boolean): Boolean = !b }
        def overField(n: Boolean): P = new P(n) { override val n: Boolean = true; override def m(b: Boolean): Boolean = !b }
        def some(n: Some[Boolean]): PO = new PO(n) { override def m(b: Boolean): Boolean = !b }
      }
    """)
    val held = "local capture held by a member that hides it"
    refused(found("O.neg.<new#1>.m"), s"$held: n")
      .and(refused(found("O.hidden.<new#1>.m"), s"$held: b"))
      .and(refused(found("O.overField.<new#1>.m"), s"$held: n"))
      .and(refused(found("O.some.<new#1>.m"), s"$held: n"))
  }

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

  // No subclass overrides a given (E164) or a final member: f's expansion selects T's g and U's g
  // on every call site — g, 2 (T.f read `?`). V's g is open: a call site's subclass may override
  // it inline.
  def fixedSelection = {
    val found = one("""
      trait T { given g: Boolean = true; inline def f: Boolean = g }
      trait U { final def g: Boolean = true; inline def f: Boolean = g }
      trait V { def g: Boolean = true; inline def f: Boolean = g }
    """)
    (found("T.f") === Count.Finite(2))
      .and(found("U.f") === Count.Finite(2))
      .and(
        refused(
          found("V.f"),
          "inline method: selects g on this, which a call site may type as a subclass of V that " +
            "overrides g inline"
        )
      )
  }

  // `given g: Boolean = deferred` is supplied by each class extending T (3.9.0 run: two classes
  // with different givens in scope print false and true): other reads a free Boolean g — g, !g,
  // true, false: 4 (it read 2). TI.f's expansion selects g, which a subclass gives: refused. A
  // `deferred` another declaration binds — a member of V, X's imported in TX's object, a local
  // `compiletime` object's selected in TL — is no abstract given.
  def deferredGiven = {
    val found = one("""
      import scala.compiletime.deferred
      trait T { given g: Boolean = deferred; def other: Boolean = g }
      trait TI { given g: Boolean = deferred; inline def f: Boolean = g }
      trait Q { given g: Boolean = scala.compiletime.deferred; def other: Boolean = g }
      trait V { val deferred: Boolean = true; given g: Boolean = deferred; def other: Boolean = g }
      object X { val deferred: Boolean = true }
      object TXs { import X.deferred; trait TX { given g: Boolean = deferred; def other: Boolean = g } }
      object compiletime { val deferred: Boolean = true }
      trait TL { given g: Boolean = compiletime.deferred; def other: Boolean = g }
    """)
    val other = "given alias of a deferred not resolved to scala.compiletime"
    (found("T.other") === Count.Finite(4))
      .and(found("Q.other") === Count.Finite(4))
      .and(
        refused(
          found("TI.f"),
          "inline method: selects g on this, which a call site may type as a subclass of TI that " +
            "overrides g inline"
        )
      )
      .and(refused(found("V.other"), s"$other: V.g"))
      .and(refused(found("TXs.TX.other"), s"$other: TXs.TX.g"))
      .and(refused(found("TL.other"), s"$other: TL.g"))
  }

  // A bare `deferred` no import binds (scalac rejects it, E006): this reading does not take it for
  // scala.compiletime's.
  def deferredBare =
    refused(
      one("trait TB { given g: Boolean = deferred; def other: Boolean = g }")("TB.other"),
      "given alias of a deferred not resolved to scala.compiletime: TB.g"
    )

}
