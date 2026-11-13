package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.ScalametaFrontEnd
import org.specs2.Specification

/** Capability records in the compiled front end (docs/plans/compiled-own-types.md, stretch 4): a
  * class or trait with members, named by a parameter's type. Every expected count is derived in its
  * example's comment, and the scalameta front end is asked for the same rows.
  */
class RecordsSpec extends Specification {

  def is = s2"""
  A capability record is what a body can observe of it
    an abstract member is what any implementation supplies       $open
    a final class is the tuple of its public fields               $fixed
    what a record inherits, and a member something overrides      $inherited
    an inherited member with type parameters of its own           $polymorphic
    a record this step does not read is uncovered                 $later
    the scalameta front end agrees on every row measured          $agrees
  """

  private val text =
    """package rec
      |
      |trait CanGet[S, A] { def get(s: S): A }
      |trait Src[A] { def x: A; def y: A = x }
      |trait Monoid[A] { def empty: A; def combine(x: A, y: A): A }
      |trait Log { def log(b: Boolean): Unit; def id: Boolean }
      |final class Box2[A](val a: A, val b: A)
      |trait Ext { extension (b: Boolean) def tag: Boolean }
      |trait Over { def equals(x: Boolean, y: Boolean): Boolean }
      |final abstract class Never
      |final class Hid(secret: Boolean) { def reveal: Boolean = secret }
      |final class HidA[A](secret: A) { def reveal: A = secret }
      |final class Get2[S, A](read: S => A) { def get(s: S): A = read(s) }
      |final class Narrow(f: Int => Boolean) { def at(b: Byte): Boolean = f(b) }
      |abstract class Held(val held: Boolean)
      |final class Feed(n: Boolean) extends Held(n)
      |abstract class Wide(val w: Any)
      |final class Widen(n: Boolean) extends Wide(n)
      |abstract class Sec(val s: Boolean) { def this(x: Int) = this(x > 0) }
      |final class ViaSec(n: Int) extends Sec(n)
      |class Base(val b: Boolean)
      |class Sub extends Base(true)
      |class Open(val b: Boolean) { def m: Boolean = b; def n: Boolean = !m }
      |class OpenSub extends Open(true) { override def m: Boolean = false }
      |class Dorm(val b: Boolean) { def m: Boolean = b }
      |class DormSub extends Dorm(true) { override def m: Boolean = false; val z: Boolean = ??? }
      |trait Seeded { def seed: Boolean }
      |trait Grown extends Seeded { def more: Boolean }
      |trait Derived extends Seeded { def other: Boolean = seed }
      |abstract class Field(val p: Boolean) { def q: Boolean }
      |abstract class Passed extends Field(true) { def r: Boolean }
      |trait Ov { def f: Boolean = true; def g: Boolean }
      |class OvSub extends Ov { override def f: Boolean = false; def g: Boolean = true }
      |trait Prod extends Product { def x: Boolean; def canEqual(that: Any): Boolean }
      |trait Shown { def show: Boolean }
      |trait Shows extends Shown
      |trait Free extends Field
      |trait Same { def eqv(a: Boolean): Boolean; def neqv(a: Boolean): Boolean = !eqv(a) }
      |object SameI extends Same { def eqv(a: Boolean): Boolean = a; override def neqv(a: Boolean): Boolean = !a }
      |trait Back { def k: Boolean = true; def j: Boolean = k }
      |object BackI extends Back { override def k: Boolean = Use.target(this) }
      |trait Ord { def m: Boolean = true; def n: Boolean = m }
      |object OrdI extends Ord with Ordering[Int] { def compare(a: Int, b: Int): Int = if (Use.ord(this)) 0 else 1; override def m: Boolean = List(2, 1).sorted(this).isEmpty }
      |trait S1 { def m: Boolean = true; def n: Boolean = m }
      |object SrtB extends S1 { override def m: Boolean = List(2, 1).sorted(new CmpB).isEmpty }
      |final class CmpB extends Ordering[Int] { def compare(a: Int, b: Int): Int = if (Use.sortBuilt(SrtB)) 0 else 1 }
      |trait S2 { def m: Boolean = true; def n: Boolean = m }
      |object SrtF extends S2 { val c: Ordering[Int] = CmpF; override def m: Boolean = List(2, 1).sorted(c).isEmpty }
      |object CmpF extends Ordering[Int] { def compare(a: Int, b: Int): Int = if (Use.sortField(SrtF)) 0 else 1 }
      |trait S3 { def m: Boolean = true; def n: Boolean = m }
      |final class SrtA(c: Ordering[Int]) extends S3 { override def m: Boolean = List(2, 1).sorted(c).isEmpty }
      |object CmpA extends Ordering[Int] { def compare(a: Int, b: Int): Int = if (Use.sortArg(new SrtA(CmpA))) 0 else 1 }
      |trait S4 { def m: Boolean = true; def n: Boolean = m }
      |final class SrtD(c: Ordering[Int]) extends S4 { override def m: Boolean = c.compare(1, 2) == 0 }
      |object CmpD extends Ordering[Int] { def compare(a: Int, b: Int): Int = if (Use.sortDirect(new SrtD(CmpD))) 0 else 1 }
      |trait S5 { def m: Boolean = true; def n: Boolean = m }
      |final class SrtE(c: Ordering[S5]) extends S5 { override def m: Boolean = List[S5](this).sorted(c).isEmpty }
      |object CmpE extends Ordering[S5] { def compare(a: S5, b: S5): Int = if (Use.sortSelf(a)) 0 else 1 }
      |trait S6 { def m: Boolean = true; def n: Boolean = m }
      |final class Z7 { val v: Boolean = Use.sortNew(SrtN) }
      |object SrtN extends S6 { override def m: Boolean = { new Z7; true } }
      |trait S7 { def m: Boolean = true; def n: Boolean = m }
      |final class SrtL(f: Int => Boolean) extends S7 { override def m: Boolean = List(1).exists(f) }
      |object MkL { def l: SrtL = new SrtL(i => Use.sortLambda(l)) }
      |trait S8 { def m: Boolean = true; def n: Boolean = m }
      |object SrtX extends S8 { override def m: Boolean = List(2, 1).sorted(CmpX).isEmpty }
      |trait Half { def compare(a: Int, b: Int): Int = if (Use.sortMixed(SrtX)) 0 else 1 }
      |object CmpX extends Half with Ordering[Int]
      |trait S9 { def m: Boolean = true; def n: Boolean = m }
      |trait Sam { def run(): Boolean }
      |final class SrtS(f: Sam) extends S9 { override def m: Boolean = f.run() }
      |object MkS { def s: SrtS = new SrtS(() => Use.sortSam(s)) }
      |trait S10 { def m: Boolean = true; def n: Boolean = m }
      |object Z10 { val v: Boolean = Use.sortInit(SrtO); def ok: Boolean = true }
      |object SrtO extends S10 { override def m: Boolean = Z10.ok }
      |trait Fb { def k: Boolean = true; def j: Boolean = k }
      |final class FbF(f: () => Boolean) extends Fb { override def k: Boolean = f() }
      |final class Pin private (val a: Boolean, val b: Boolean)
      |object Pin { def one(x: Boolean): Pin = new Pin(x, x) }
      |final class Shut private (val a: Boolean)
      |trait Typed { type T; def t: T }
      |trait Typing extends Typed { def b: Boolean }
      |trait Pack { type T; def x: T; def same(a: T, b: T): Boolean }
      |trait Bounded { type T <: Int; def t: T }
      |trait Guarded { protected def h: Boolean }
      |trait Guards extends Guarded { def k: Boolean = h }
      |trait Ex[F[_]] { def extract[A](fa: F[A]): A }
      |trait Ex2[F[_]] extends Ex[F]
      |trait Cof[F[_]] { def coflatten[A](fa: F[A]): F[F[A]]; def void[A](fa: F[A]): F[Unit] }
      |trait Cof2[F[_]] extends Cof[F]
      |object Use {
      |  def read[S, A](s: S, g: CanGet[S, A]): A = g.get(s)
      |  def noSource[S, A](g: CanGet[S, A]): A = ???
      |  def pick[A](t: Src[A]): A = t.x
      |  def twice[A](m: Monoid[A], a: A): A = a
      |  def ask(l: Log): Boolean = l.id
      |  def fromBox[A](b: Box2[A]): A = b.a
      |  def makeBox[A](x: A, y: A): Box2[A] = Box2(x, y)
      |  def tagged(e: Ext): Boolean = true
      |  def hid(h: Hid): Boolean = true
      |  def fromHidA[A](h: HidA[A]): A = h.reveal
      |  def getter[S, A](f: S => A): Get2[S, A] = new Get2(f)
      |  def useGet(g: Get2[Boolean, Boolean], b: Boolean): Boolean = g.get(b)
      |  def narrow(n: Narrow): Boolean = true
      |  def widen(x: Widen): Boolean = true
      |  def viaSec(x: ViaSec): Boolean = true
      |  def feed(f: Feed): Boolean = f.held
      |  def over(o: Over, b: Boolean): Boolean = b
      |  def never(n: Never): Boolean = true
      |  def make(): Never = ???
      |  def base(b: Base): Boolean = true
      |  def opened(o: Open): Boolean = true
      |  def dorm(o: Dorm): Boolean = true
      |  def grown(g: Grown): Boolean = true
      |  def derived(d: Derived): Boolean = true
      |  def passed(p: Passed): Boolean = true
      |  def ov(o: Ov): Boolean = true
      |  def prod(p: Prod): Boolean = true
      |  def show(s: Shows): Boolean = true
      |  def free(f: Free): Boolean = true
      |  def same(s: Same): Boolean = true
      |  def target(b: Back): Boolean = true
      |  def ord(o: Ord): Boolean = true
      |  def fb(b: Fb): Boolean = true
      |  def sortBuilt(s: S1): Boolean = true
      |  def sortField(s: S2): Boolean = true
      |  def sortArg(s: S3): Boolean = true
      |  def sortDirect(s: S4): Boolean = true
      |  def sortSelf(s: S5): Boolean = true
      |  def sortNew(s: S6): Boolean = true
      |  def sortLambda(s: S7): Boolean = true
      |  def sortMixed(s: S8): Boolean = true
      |  def sortSam(s: S9): Boolean = true
      |  def sortInit(s: S10): Boolean = true
      |  def pin(p: Pin): Boolean = true
      |  def mkPin(x: Boolean, y: Boolean): Pin = ???
      |  def shut(s: Shut): Boolean = true
      |  def typing(t: Typing): Boolean = true
      |  def pack(p: Pack): Boolean = true
      |  def packOut(b: Boolean): Pack = ???
      |  def two(p: Pack, q: Pack): Boolean = true
      |  def path(p: Pack): p.T = p.x
      |  def bounded(b: Bounded): Boolean = true
      |  def guards(g: Guards): Boolean = true
      |  def get[F[_], A](e: Ex2[F], fa: F[A]): A = e.extract(fa)
      |  def noA[F[_], A](c: Cof2[F], fa: F[A]): A = ???
      |  def run[F[_], A, B](c: Cof2[F], k: F[A] => B): B = ???
      |}
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Records.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name == s"rec.Use.$name")
      .get
      .count

  // A trait is any implementation of its abstract members. `read` can only apply `get` to `s`:
  // 1; `noSource` has no S to apply it to: 0. A concrete member computes from the others: `pick`
  // returns `x`, 1. `twice` returns `a`, `empty`, or any combination of them: ω. `log` builds
  // `Unit`, which needs no member: `ask` is a Boolean computed from `id`, 2^2. A member named like
  // one of `Any`'s with other parameters is a member like any: `over` is a Boolean computed from
  // `b` (2 values) and the function `equals` holds (2^(2 × 2) = 16 values): 2^(2 × 16) = 2^32.
  private def open =
    (count("read") must_== Count.Finite(1))
      .and(count("noSource") must_== Count.Finite(0))
      .and(count("pick") must_== Count.Finite(1))
      .and(count("twice") must_== Count.Countable)
      .and(count("ask") must_== Count.Finite(4))
      .and(count("over") must_== Count.Finite(BigInt(2).pow(32)))

  // A final class holds its public fields: `fromBox` returns `a` or `b`, 2, and `makeBox` builds
  // one of the 2 × 2 pairs of `x` and `y`. A parameter a public member returns as it is observed:
  // `Hid` is its `secret`, through `reveal`, so `hid` is one of 2^2. A final class whose
  // constructor the site may not call is one of what its factories build (rule B): `Pin.one`
  // builds 2 of its 2 × 2 values, so `pin` is one of 2^2 and `mkPin` one of the 2^(2 × 2)
  // functions from two Booleans to those 2. A class written `final abstract` has no value: `never`
  // is the one function from it, and `make` has no total body. `HidA[A]` is its `secret`, an A, so
  // `fromHidA` has the one A it holds. `Get2.get` applies `read` to its own parameter (an η-alias),
  // so `Get2[S, A]` is an S => A: `getter` has the one, `f`, and `useGet` is one of the 2^(4 × 2)
  // functions of a Boolean => Boolean and a Boolean. `Feed` passes `n` to `Held`'s public `held`:
  // it is that Boolean, and `feed` one of 2^2.
  private def fixed =
    (count("fromBox") must_== Count.Finite(2))
      .and(count("makeBox") must_== Count.Finite(4))
      .and(count("never") must_== Count.Finite(1))
      .and(count("make") must_== Count.Finite(0))
      .and(count("hid") must_== Count.Finite(4))
      .and(count("fromHidA") must_== Count.Finite(1))
      .and(count("getter") must_== Count.Finite(1))
      .and(count("useGet") must_== Count.Finite(256))
      .and(count("feed") must_== Count.Finite(4))
      .and(count("pin") must_== Count.Finite(4))
      .and(count("mkPin") must_== Count.Finite(16))

  // A record holds what it inherits as a receiver does. `Grown` is its own `more` and the `seed` it
  // inherits: 2 × 2 values, so `grown` is one of 2^4. `Derived`'s `other` computes from `seed`:
  // `derived` is one of 2^2. `Passed` passes `true` to `Field`'s `p`, a constant, and holds `q` and
  // `r`: 2^4. `Free` passes nothing, so `p` is a value each instance chooses beside `q`: 2^4.
  // `OvSub` overrides `Ov.f`, so `f` varies beside `g`: 2^4. A member restating one of
  // `Product`'s (`canEqual`) is outside the model: `Prod` is its `x`, 2^2. A record value is another
  // instance than the site's: `SameI.neqv` never reaches `same`, so `neqv` is a capability beside
  // `eqv`, each one of the 2^2 functions of a Boolean: `Same` is one of 4 × 4 values, and `same`
  // one of 2^16. An unbounded abstract type member, own or inherited, is the witness of the
  // package each value carries: `Typing` is ∃T. (t: T, b: Boolean), and nothing tells a `T` apart,
  // so `typing` is a Boolean computed from `b`, 2^2; `Pack` is ∃T. (x: T, same: (T, T) => Boolean),
  // whose one observation is `same(x, x)`, so `pack` is one of 2^2. Each value carries its own
  // witness: `q.same` takes no `p.x`, so `two` sees `p.same(p.x, p.x)` and `q.same(q.x, q.x)`, one
  // of 2^(2 × 2). A type a path names (`p.T`) is not read. A goal of a package needs a
  // witness chosen, which the solver refuses (`packOut`), as the old front end does.
  private def inherited =
    (count("grown") must_== Count.Finite(16))
      .and(count("derived") must_== Count.Finite(4))
      .and(count("passed") must_== Count.Finite(16))
      .and(count("ov") must_== Count.Finite(16))
      .and(count("prod") must_== Count.Finite(4))
      .and(count("free") must_== Count.Finite(16))
      .and(count("same") must_== Count.Finite(65536))
      .and(count("typing") must_== Count.Finite(4))
      .and(count("pack") must_== Count.Finite(4))
      .and(count("two") must_== Count.Finite(16))
      .and(count("path") must beLike { case Count.Unresolved(_) => ok })
      .and(count("packOut") must beLike {
        case Count.Unresolved(List(why)) =>
          why must startWith("existential wildcard result construction")
      })
      // `show` inherits a member of its own name and arity, but the object `Use` is no `Shows`:
      // that member is never the definition (`NamesSpec`). One Boolean member, 2^2.
      .and(count("show") must_== Count.Finite(4))
      // `FbF.k` applies a function its constructor was given, but nothing names `fb`, so no
      // behavior calls it (closed world): `k` is a Boolean capability and `j` computes from it,
      // so `Fb` is 2 values and `fb` one of 2^2.
      .and(count("fb") must_== Count.Finite(4))
      // `Sub` extends `Base` and overrides nothing: a `Base` is its public field `b`, every member
      // computing from it, so 2 values and `base` one of 2^2.
      .and(count("base") must_== Count.Finite(4))
      // `OpenSub` overrides `m`, which so varies across instances of `Open`: a capability beside
      // the field `b`, and `n` computes from it. 2^2 values, `opened` one of 2^4.
      .and(count("opened") must_== Count.Finite(16))

  // Not read yet, each by its reason: an extension member; a class whose only overriding subclass
  // is never built (`DormSub`, D26: `m` is `b` on every `Dorm` there is); a parameter a member
  // observes on part of its domain alone (`Narrow.at` widens a Byte), one a parent's field holds
  // as a wider type (`Wide.w: Any`, one value in an input) or one a parent's secondary constructor
  // takes (`Sec`'s `this(x: Int)`, whose field is `x > 0`); one with a
  // bounded abstract type member (a proof about each value's own witness); one whose inherited
  // member the site may not call; a final class that nothing the site may call builds; and an
  // overridden member a behavior of which reaches the site: `BackI.k` calls `target`; `OrdI.m`
  // gives `OrdI` to `sorted`, which calls its `compare`, which calls `ord`. Each `sort…` row's
  // record has an `m` that hands code outside the module an `Ordering` of the module whose
  // `compare` calls the site (each `m` is otherwise a Boolean beside `n = m`, which would count
  // 2^2): one it builds, a field's, a constructor parameter's (`SrtD.m` selects its `compare`
  // itself), one beside `this`, one whose `compare` is a trait's method that implements
  // `Ordering.compare` only in `CmpX`. `SrtN.m` builds a class whose body calls the site; `SrtL.m`
  // hands `exists` a function literal that does; `SrtS.m` calls a `Sam` that a function literal
  // calling the site implements; `SrtO.m` first accesses `Z10`, whose body calls the site.
  // An inherited member with type parameters of its own is read as a declared one is. `get` applies
  // `extract` to `fa`, its one F value: 1. Nothing of `Cof2` returns a bare A or takes an F apart, so
  // `noA` has no body: 0. `run` holds no F value at all: a B comes only from `k`, which needs an
  // F[A], and every member of `c` needs an F value to start from: 0. (`void`'s F[Unit] is the result
  // of a call no argument makes possible, so it fixes no instance of `coflatten`; reading it as
  // supplied let `coflatten` build F[F[Unit]], F[F[F[Unit]]], … without end.)
  private def polymorphic =
    (count("get") must_== Count.Finite(1))
      .and(count("noA") must_== Count.Finite(0))
      .and(count("run") must_== Count.Finite(0))

  private def later = {
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    def reason(name: String) = count(name) match {
      case Count.Unresolved(List(why)) => why.stripPrefix(prefix).takeWhile(_ != ':')
      case other                       => other.toString
    }
    val sorts =
      List("sortBuilt", "sortField", "sortArg", "sortDirect", "sortSelf", "sortNew", "sortLambda")
        ++ List("sortMixed", "sortSam", "sortInit")
    (sorts.map(n => n -> reason(n)) must_== sorts.map(
      _ -> s"${Receivers.Reenters} among the members of a capability record"
    )).and(
      List(
        "tagged",
        "narrow",
        "widen",
        "viaSec",
        "dorm",
        "bounded",
        "guards",
        "shut",
        "target",
        "ord"
      )
        .map(reason) must_== List(
        "an extension method among the members of a capability record",
        "a capability record with a parameter that is no public field",
        "a capability record with a parameter that is no public field",
        "a capability record with a parameter that is no public field",
        "a capability record of a class something extends that is never built",
        "a capability record with a bounded abstract type member",
        "a capability record with a member hidden at the use site",
        "a capability record with no constructor or factory the site may call",
        s"${Receivers.Reenters} among the members of a capability record",
        s"${Receivers.Reenters} among the members of a capability record"
      )
    )
  }

  // `Bounded.t` counts where the old front end refuses a bounded abstract type member: `t: T`
  // has no input of `T` (the bound `T <: Int` gives an Int from a T, not a T), so no body: 0.
  private def agrees = {
    val compared = Differential.run(input, Differential.answer(ScalametaFrontEnd, input.old))
    val derived =
      compared.compared.filter(_.row.name == "rec.Bounded.t").map(_.row.key -> "0").toMap
    ((compared.failed, compared.noted) must_== (Nil, Nil))
      .and(derived.size must_== 1)
      .and(Differential.failures(compared.compared, derived) must_== Nil)
  }

}
