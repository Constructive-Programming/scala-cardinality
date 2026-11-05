package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// Re-entry through what the receiver's members run (capabilities C6) where the measured
// declaration is a local definition, a constructor runs it, or implicit search may select it; and
// the view sandwich (C6, register D31). Fixtures are valid Scala 3.9.0 (scalac, on session
// fixtures not retained).
class ReentrySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    A local definition re-entered by its enclosing member
      evaluated at a result, a statement or a nested block: dropped   $localDefinite
      in a branch, a lambda applied in place or an η-expansion: `?`    $localPossible
      inside a match arm, a try, an interpolation or a loop: `?`       $localLoose
      a name bound again, or the definition's own recursion, is no mention $localShadowed
    Constructing a class runs its constructors
      a secondary constructor's body, for a local or a member target   $secondary
      strict initializers and template statements                      $primary
      a subclass's construction runs its parents'                      $subclass
      a receiver's subclass with code of its own may run any member    $subclassOwnCode
      a creator application or `copy` builds as `new` does            $creators
    Implicit search may select the measured given
      an abstract given a sibling may summon is a possible view        $abstractGiven
      a body returning its parameter of the result type searches none  $settledBody
    The view sandwich
      an upper reading of 0, or of 1 with a lower 1, is the count      $sandwich
      a refused binding, readings that disagree, or ≥ 2 stay `?`       $sandwichRefuses
  """

  private def one(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Reentry.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def view(count: Count, member: String): Result =
    count must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(s"overridable member may re-enter the measured declaration: $member")
    }

  // m runs h whenever it runs (its result, a statement, a nested block), and other runs m: both
  // re-enter h, so neither is an input. h reads y and the capture b: 2^(2·2) = 16 (it read 2^32
  // with m and other free, 2^16 with m alone). C3's f is a local lambda, concrete code: 16 too.
  def localDefinite = {
    val found = one("""
      class C { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; h(b) }; def other: Boolean = m(true) }
      trait T { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; h(b) } }
      class S { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; val r = h(b); r } }
      class B { def m(b: Boolean): Boolean = { val r = { def h(y: Boolean): Boolean = y; h(b) }; r } }
      class C3 { def m(b: Boolean): Boolean = { val f = (y: Boolean) => y; def h(z: Boolean): Boolean = f(z); h(b) } }
    """)
    List("C.m.h", "T.m.h", "S.m.h", "B.m.<block#1>.h", "C3.m.h")
      .map(found(_) === Count.Finite(16))
      .reduce(_ and _)
  }

  // Re-entry depends on what m is given (a branch), or the position is not on C6's definite list (a
  // lambda applied in place, a local def calling h, an η-expansion): a possible view, never 16.
  def localPossible = {
    val found = one("""
      class C2 { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; if b then h(b) else false } }
      class L { def m(b: Boolean): Boolean = ((y: Boolean) => { def w(z: Boolean): Boolean = z; w(y) })(b) }
      class Two { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; def k(z: Boolean): Boolean = h(z); k(b) } }
      class Eta { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; val f = h; f(b) } }
    """)
    view(found("C2.m.h"), "C2.m")
      .and(view(found("L.m.<lambda#1>.w"), "L.m"))
      .and(view(found("Two.m.h"), "Two.m"))
      .and(view(found("Eta.m.h"), "Eta.m"))
  }

  // Each m runs h on every run (both arms, a try, an interpolation, a partial function applied to
  // b) or may (a loop), but the mention sits where this walk reads loosely: a possible view, never
  // the 2^16 of m left free — the upper reading binds m (2^16), the lower drops it (16): `?`. A
  // named argument of a by-value parameter is evaluated like a positional one: N's m is dropped, 16.
  def localLoose = {
    val found = one("""
      class C1 { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; b match { case true => h(b); case false => h(!b) } } }
      class C2 { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; try h(b) finally () } }
      class C3 { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; s"${h(b)}" == "true" } }
      class C4 { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; val f: PartialFunction[Boolean, Boolean] = { case x => h(x) }; f(b) } }
      class W1 { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; var r = b; while r do r = h(r); r } }
      class N { final def id(x: Boolean): Boolean = x; def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; id(x = h(b)) } }
    """)
    List("C1", "C2", "C3", "C4", "W1")
      .map(c => view(found(s"$c.m.h"), s"$c.m"))
      .reduce(_ and _)
      .and(found("N.m.h") === Count.Finite(16))
  }

  // A lambda's parameter h is another binding, and h's own recursion is the choice being counted:
  // m never runs h, so it is a free Boolean => Boolean beside y and b — 2^(2^4) = 2^16.
  def localShadowed = {
    val found = one("""
      class Sh { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = y; val g = (h: Boolean) => h; g(b) } }
      class Rec { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = if y then h(!y) else y; b } }
    """)
    (found("Sh.m.h") === Count.Finite(65536)).and(found("Rec.m.h") === Count.Finite(65536))
  }

  // `new D(())` runs the secondary constructor, which runs helper (and E's h): other may re-enter
  // them — `?`, not 2^8 (y, n, other). DN's never calls helper: y, n, other — 2^8. A final class has
  // no open member: helper reads y and n — 16.
  def secondary = {
    val found = one("""
      class D(val n: Boolean) { def other: Boolean = new D(()).n; def this(u: Unit) = { this(true); def helper(y: Boolean): Boolean = y; val _ = helper(true) } }
      class DN(val n: Boolean) { def other: Boolean = new DN(()).n; def this(u: Unit) = { this(true); def helper(y: Boolean): Boolean = y; val _ = true } }
      class E(val n: Boolean) { def h(y: Boolean): Boolean = y; def other: Boolean = new E(()).n; def this(u: Unit) = { this(true); val _ = h(true) } }
      final class F(val n: Boolean) { def this(u: Unit) = { this(true); def helper(y: Boolean): Boolean = y; val _ = helper(true) } }
    """)
    view(found("D.<init#1>.helper"), "D.other")
      .and(found("DN.<init#1>.helper") === Count.Finite(256))
      .and(view(found("E.h"), "E.other"))
      .and(found("F.<init#1>.helper") === Count.Finite(16))
  }

  // Constructing Init runs x's initializer, and Stmt's template statement: both run h, so other
  // may re-enter it (it read 2^8: y, n, other).
  def primary = {
    val found = one("""
      class Init(val n: Boolean) { val x: Boolean = h(true); def h(y: Boolean): Boolean = y; def other: Boolean = new Init(true).n }
      class Stmt(val n: Boolean) { h(true); def h(y: Boolean): Boolean = y; def other: Boolean = new Stmt(true).n }
    """)
    view(found("Init.h"), "Init.other").and(view(found("Stmt.h"), "Stmt.other"))
  }

  // `new Sub` runs C's primary constructor, so init runs h: m may re-enter it — `?` (it read 16, m
  // a free Boolean). `new Exception("e")` builds a class the stdlib model reads, which extends no
  // class of the sources: Lib's m is no view, and h reads b and m — 16.
  def subclass = {
    val found = one("""
      class C { val init: Boolean = h(true); def h(b: Boolean): Boolean = b; def m: Boolean = { new Sub; true } }
      class Sub extends C
      class Lib { val init: Boolean = h(true); def h(b: Boolean): Boolean = b; def m: Boolean = { new Exception("e"); true } }
    """)
    view(found("C.h"), "C.m").and(found("Lib.h") === Count.Finite(16))
  }

  // `new Sub()` builds a subclass of C whose strict x runs h on it, C's h (3.9.0 run: other's
  // implementation re-entering h overflows the stack): other may re-enter h — `?`, not 16 (y and
  // other free); the true count is 4. Sub2 runs no code of its own: other builds nothing that runs
  // a member of C, so h reads y and other — 16.
  def subclassOwnCode = {
    val found = one("""
      class C { def h(y: Boolean): Boolean = y; def other: Boolean = new Sub().x }
      class Sub extends C { val x: Boolean = h(true) }
      class C2 { def h(y: Boolean): Boolean = y; def other: Boolean = new Sub2().x }
      class Sub2 extends C2 { def x: Boolean = true }
    """)
    view(found("C.h"), "C.other").and(found("C2.h") === Count.Finite(16))
  }

  // `E(true)` (the companion's apply), `EA.apply(true)`, `copy(b = true)`, a plain class's
  // constructor proxy `P(true)` and `q.copy(…)` on another value build the class as `new` does,
  // running n = h(b): other may re-enter h (E read 2^8; 3.9.0 run: E's twin returning other
  // overflows). K builds an unrelated case class: h reads y, b and other — n, a definite view, is
  // left out: 2^8.
  def creators = {
    val found = one("""
      case class E(b: Boolean) { val n: Boolean = h(b); def h(y: Boolean): Boolean = y; def other: Boolean = E(true).n }
      case class EA(b: Boolean) { val n: Boolean = h(b); def h(y: Boolean): Boolean = y; def other: Boolean = EA.apply(true).n }
      case class G(b: Boolean) { val n: Boolean = h(b); def h(y: Boolean): Boolean = y; def other: Boolean = copy(b = true).n }
      class P(val b: Boolean) { val n: Boolean = h(b); def h(y: Boolean): Boolean = y; def other: Boolean = P(true).n }
      case class Q(b: Boolean) { val n: Boolean = h(b); def h(y: Boolean): Boolean = y; def other(q: Q): Boolean = q.copy(b = true).n }
      case class K(b: Boolean) { val n: Boolean = h(b); def h(y: Boolean): Boolean = y; def other: Boolean = Other(true).b }
      case class Other(b: Boolean)
    """)
    List("E", "EA", "G", "P", "Q")
      .map(c => view(found(s"$c.h"), s"$c.other"))
      .reduce(_ and _)
      .and(found("K.h") === Count.Finite(256))
  }

  // `summon[Boolean]` and an implicitly filled `using` clause may select g itself: other (T3) and
  // use (T4) may re-enter it — `?` (they read 4 and 2^8). Naming g is a definite view (T8: g is a
  // value, 2); with no sibling g is any Boolean, 2.
  def abstractGiven = {
    val found = one("""
      trait T3 { given g: Boolean; def other: Boolean = summon[Boolean] }
      trait T4 { given g: Boolean; def other(using b: Boolean): Boolean = b; def use: Boolean = other }
      abstract class A7 { given g: Boolean; def other: Boolean = implicitly[Boolean] }
      trait T8 { given g: Boolean; def other: Boolean = g }
      trait T0 { given g: Boolean }
    """)
    view(found("T3.g"), "T3.other")
      .and(view(found("T4.g"), "T4.use"))
      .and(view(found("A7.g"), "A7.other"))
      .and(found("T8.g") === Count.Finite(2))
      .and(found("T0.g") === Count.Finite(2))
  }

  // `other(using b: Boolean): Boolean = b` returns a value of its declared result type: nothing is
  // converted or searched, so other is free. g reads it — the functions of a Boolean => Boolean,
  // 16 — and use, passing g explicitly, is a definite view.
  def settledBody =
    one("""
      trait Xe { given g: Boolean; def other(using b: Boolean): Boolean = b; def use: Boolean = other(using g) }
    """)("Xe.g") === Count.Finite(16)

  // Z's m returns Int: with m bound, no T is reachable — 0. O.to: with m bound or left out, None is
  // the one Option[T] — 1. Laws (eo's ComposedGetOptionLaws shape): the record CanGetOption[A, B]
  // has one implementation, the None-returning one, either way — 1.
  def sandwich = {
    val found = one("""
      trait Z[T] { def to(u: Unit): T; def m(b: Boolean): Int = if b then { val t = to(()); 0 } else 1 }
      trait O[T] { def to(u: Unit): Option[T]; def m(b: Boolean): Boolean = if b then to(()).isEmpty else false }
      trait CanGetOption[S, A] { def getOption(s: S): Option[A] }
      trait Laws[S, A, B] {
        def getOptionOuter: CanGetOption[S, A]
        def getOptionInner: CanGetOption[A, B]
        def getOptionComposed: CanGetOption[S, B]
        def composedGetOption(s: S): Boolean =
          getOptionComposed.getOption(s) == getOptionOuter.getOption(s).flatMap(getOptionInner.getOption)
      }
    """)
    (found("Z.to") === Count.Finite(0))
      .and(found("O.to") === Count.Finite(1))
      .and(found("Laws.getOptionInner") === Count.Finite(1))
  }

  // D.to: 0 without m, 2 with it (m(true), m(false)) — they disagree. E.to: 4 without m, and with
  // it bound m: Boolean => Int reaches Int — they disagree too. E3's m returns Unit: 4 either way;
  // agreeing bounds fix the count (4), but the rule decides only 0 and 1, a deliberately narrow
  // choice, so it stays `?`. Opt.to's upper reading binds andThen, whose record has an abstract
  // type member: refused, so the row is (its true count is 0, but the closed-world reading that
  // says so is unsound: an open class's concrete members are capabilities, X5).
  def sandwichRefuses = {
    val found = one("""
      trait D[T] { def to(u: Unit): T; def m(b: Boolean): T = if b then to(()) else to(()) }
      trait E { def to(b: Boolean): Boolean; def m(b: Boolean): Int = if b then { val x = to(true); 0 } else 1 }
      trait E3 { def to(b: Boolean): Boolean; def m(b: Boolean): Unit = if b then { val x = to(true); () } else () }
      trait RC[T] { type Out; def compose(o: Opt[T], t: T): Out }
      abstract class Opt[T](secret: T) { def to(u: Unit): T; def andThen(using rc: RC[T]): rc.Out = rc.compose(this, secret) }
    """)
    view(found("D.to"), "D.m")
      .and(view(found("E.to"), "E.m"))
      .and(view(found("E3.to"), "E3.m"))
      .and(view(found("Opt.to"), "Opt.andThen"))
  }

}
