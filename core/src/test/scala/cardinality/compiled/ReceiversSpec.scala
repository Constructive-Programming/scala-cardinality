package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.ScalametaFrontEnd
import org.specs2.Specification

/** Slice S4 of the compiled front end (docs/plans/compiled-own-types.md): a member of a class reads
  * what its receivers hold. Every expected count is derived in its example's comment, and the
  * scalameta front end is asked for the same rows.
  */
class ReceiversSpec extends Specification {

  def is = s2"""
  A member of a class reads what the instance holds
    every constructor parameter is a value of the environment     $fields
    an abstract member is a capability, a concrete one nothing    $capabilities
    a class around the class or its object supplies the same      $nested
    a template that inherits, or a sum, reads what it holds       $inherited
    a local definition reads the methods it is written in         $locals
    a receiver this slice does not read is uncovered              $later
    the scalameta front end agrees on every row measured          $agrees
  """

  private val text =
    """package recv
      |
      |class Env[A](captured: A) { def choose(x: A, y: A): A = x }
      |final class Pair(a: Boolean, val b: Boolean) { def get: Boolean = a }
      |class Shadow[A](val x: A) { def pick(x: A): A = x }
      |class Poly[A](captured: A) { def choose[A](x: A, y: A): A = x }
      |class Derived(b: Boolean) { val twice: Boolean = b; def get: Boolean = b }
      |abstract class Cap[A] { def seed: A; def pick(): A = seed }
      |abstract class Step[A] { def seed: A; def step(a: A): A; def pick(): A = seed }
      |abstract class Log { def log(b: Boolean): Unit; def id(b: Boolean): Boolean = b }
      |abstract class Held { val flag: Boolean; def get: Boolean = flag }
      |class Outer[A](a: A) { class Inner { def x: A = a } }
      |class Holder[A](seed: A) { object In { def get: A = seed } }
      |class Base(b: Boolean) { def get: Boolean = b }
      |class Sub(c: Boolean) extends Base(c) { def more: Boolean = c }
      |trait Shown { def show(b: Boolean): Boolean }
      |class Impl(flag: Boolean) extends Shown { def show(b: Boolean): Boolean = b; def get: Boolean = flag }
      |object Single extends Shown { def show(b: Boolean): Boolean = b; def other(b: Boolean): Boolean = b }
      |abstract class Half extends Shown { def get: Boolean = true }
      |abstract final class Never extends Shown { def get: Boolean = true }
      |sealed class Closed(b: Boolean) { def get: Boolean = b }
      |trait Late { inline def late: Boolean; def now: Boolean = true; inline def soon: Boolean = late }
      |object Dead { val z: Boolean = ???; class In(b: Boolean) { def get: Boolean = b } }
      |class Host(x: Boolean) { class Part { def px: Boolean = x } }
      |object Guest { val o: Host = new Host(true); final class K extends o.Part { def g: Boolean = true } }
      |object Loc { def outer(y: Boolean): Boolean = { def inner(x: Boolean): Boolean = x; inner(y) } }
      |class LocC(z: Boolean) { def outer[A](y: A): A = { def inner(x: A): A = x; inner(y) } }
      |object Hide { def outer(x: Boolean): Boolean = { def inner(x: Boolean): Boolean = x; inner(x) } }
      |object Lit { def outer(y: Boolean): Boolean = List(y).map { v => def inner: Boolean = v; inner }.head }
      |object Pat { def outer(y: Option[Boolean]): Boolean = y match { case Some(v) => def inner: Boolean = v; inner; case None => true } }
      |class Ctor(val p: Boolean) { locally { def hc(): Boolean = p; hc() } }
      |trait Fn { def run(b: Boolean): Boolean }
      |object Mk { def mk(y: Boolean): Fn = new Fn { def run(b: Boolean): Boolean = b } }
      |trait Help { def helper: Boolean = true; def get: Boolean }
      |object Hid { def mk(helper: Boolean): Help = new Help { def get: Boolean = true } }
      |object Deep { def mk(y: Boolean): Fn = new Fn { object H { def h: Boolean = y }; def run(b: Boolean): Boolean = b } }
      |object DeepHid { def mk(helper: Boolean): Fn = new Fn { object H extends Help { def get: Boolean = true }; def run(b: Boolean): Boolean = b } }
      |object Self { def mk(y: Boolean): Fn = new Fn { y => def run(b: Boolean): Boolean = b } }
      |enum Color(val rgb: Int) { case Red extends Color(1); case Green extends Color(2); def f: Boolean = true }
      |object LocalK { def f(y: Boolean): Boolean = { class K { def get: Boolean = y }; new K().get } }
      |trait Wished[A] { inline def wish: A; def get: Boolean = true }
      |trait Kept { def kept: Boolean; def get: Boolean = kept }
      |object KeptOne extends Kept { def kept: Boolean = true }
      |given shown: Shown with { def show(b: Boolean): Boolean = b }
      |class Wide(b: Boolean) { def get: Boolean = b; def more: Boolean = b }
      |class Narrow extends Wide(true) { override def more: Boolean = false }
      |sealed abstract class Fam { def get: Boolean = true }
      |case object One extends Fam
      |trait Self { self: Base => def me: Boolean = true }
      |class Own(b: Boolean) { own => def get: Boolean = own.b }
      |class Gen[A](a: A) { g => def pick(x: A): A = g.a }
      |trait Narrowed[A] { self: Env[A] => def get: Boolean = true }
      |trait Deferred { inline val flag: Boolean; def m: Boolean = true }
      |class Thrown(b: Boolean) { val z: Boolean = ???; def get: Boolean = b }
      |trait Member { type T; def seed: T; def get(t: T): T = t }
      |trait Bounded { type T <: AnyRef; def get(t: T): T = t }
      |class Pinned private () { type T; def get(t: T, u: T): T = t }
      |final class Fin { type T; def get(t: T, u: T): T = t }
      |trait WishedM { type T; inline def wish: T; def get: Boolean = true }
      |trait Kinded { type F[_]; def get(x: F[Int]): F[Int] = x }
      |trait CoKinded { type F[+X]; def none: F[Nothing]; def get: F[Int] = none }
      |trait Typed { type T }
      |abstract class Holds extends Typed { def seed: T; final class Fixed extends Typed { def get(t: T): T = t } }
      |abstract class Twice extends Typed { abstract class Again extends Typed { def get(t: T): T = t } }
      |trait Low { type T >: Int }
      |abstract class Mixed extends Low with Typed { def get(t: T): T = t }
      |class Maker[A](a: A) { final class Two(x: A, y: A); final class Flag(b: Boolean) }
      |object Built extends Base(true) { final class Q(b: Boolean, c: Boolean) }
      |object TA { final case class P(b: Boolean); trait Base[R] { def f(p: P): R } }
      |object TB { final case class P(b: Boolean); trait Mid[R] extends TA.Base[R] { def f(p: P): R } }
      |trait Twin[R] extends TB.Mid[R] { def g(): R = ??? }
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Receivers.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

  private def entry(name: String) =
    rows.sources.flatMap(_.toOption.toList.flatMap(_.methods)).find(_.name == name).get

  private def count(name: String): Count = entry(name).count

  // `Env.choose` returns one of three As: the instance's `captured`, `x` or `y`. `Pair.get` is a
  // Boolean computed from the two the instance holds, `val` or not: 2^(2 × 2) = 16. A parameter
  // that hides a field's name hides no value: `Shadow.pick` returns `this.x` or `x`. A method's own
  // type parameter is another type than the class's of that name: `Poly.choose` cannot return
  // `captured`, 2. A row's captures are the names of what its receivers supply. A self alias
  // without a type is of the class's own type, which adds nothing: `Own.get` is a Boolean computed
  // from `b`, 2^2, and `Gen.pick` returns `a` (read through the alias) or `x`, 2.
  private def fields =
    (count("recv.Env.choose") must_== Count.Finite(3))
      .and(count("recv.Pair.get") must_== Count.Finite(16))
      .and(count("recv.Shadow.pick") must_== Count.Finite(2))
      .and(count("recv.Poly.choose") must_== Count.Finite(2))
      .and(entry("recv.Env.choose").captures must_== List("captured"))
      .and(entry("recv.Pair.get").captures must_== List("a", "b"))
      .and(count("recv.Own.get") must_== Count.Finite(4))
      .and(count("recv.Gen.pick") must_== Count.Finite(2))

  // An abstract member is whatever a subclass supplies. `Cap.pick` can only return `seed`: 1.
  // `Step.pick` returns `seed`, `step(seed)`, `step(step(seed))`, …: ω. `Log.log` builds `Unit`,
  // which every body builds without it: `id` is one of the 2^2 functions. An abstract `val` is a
  // value: `Held.get` is a Boolean computed from one Boolean, 2^2. A concrete member computes from
  // what the environment already holds: `Derived.twice` adds nothing to `get`, 2^2, and `twice`
  // itself is computed from `b`, 2^2. A deferred inline method is called from inline code alone
  // (the compiler rejects any other call): `Late.now` is one of 2 Booleans, the inline `soon` a
  // Boolean computed from `late`, 2^2. An abstract member an object implements is still what a
  // subclass supplies: `Kept.get` is computed from `kept`, 2^2.
  private def capabilities =
    (count("recv.Cap.pick") must_== Count.Finite(1))
      .and(count("recv.Step.pick") must_== Count.Countable)
      .and(count("recv.Log.id") must_== Count.Finite(4))
      .and(count("recv.Held.get") must_== Count.Finite(4))
      .and(count("recv.Derived.get") must_== Count.Finite(4))
      .and(count("recv.Derived.twice") must_== Count.Finite(4))
      .and(count("recv.Late.now") must_== Count.Finite(2))
      .and(count("recv.Late.soon") must_== Count.Finite(4))
      .and(count("recv.Kept.get") must_== Count.Finite(4))

  // An inner instance holds its outer one, and so does an object in a class: the one A in reach is
  // the outer class's. A class in a class is built on an instance of it: a constructor position
  // over a type parameter takes the caller's value or the instance's, so `Maker.Two` places one
  // of `x`, `y` and `a` in each of its two positions, 3^2 = 9; a ground position is its value
  // space, `Maker.Flag` 2, whatever the instance holds. An object holds no such value, parents or
  // not: `Built.Q` is its two Booleans, 2^2. An abstract type member is an atom on the instance:
  // `Member.get` returns `t` or `seed`, 2, and one of kind `F[_]` is a rigid constructor:
  // `Kinded.get` can only return `x`, 1.
  private def nested =
    (count("recv.Outer.Inner.x") must_== Count.Finite(1))
      .and(count("recv.Holder.In.get") must_== Count.Finite(1))
      .and(count("recv.Maker.Two.<init>") must_== Count.Finite(9))
      .and(entry("recv.Maker.Two.<init>").captures must_== List("a"))
      .and(count("recv.Maker.Flag.<init>") must_== Count.Finite(2))
      .and(count("recv.Built.Q.<init>") must_== Count.Finite(4))
      .and(count("recv.Member.get") must_== Count.Finite(2))
      .and(count("recv.Kinded.get") must_== Count.Finite(1))

  // A template whose inherited members are all implemented and can none be overridden holds its
  // own state alone: each inherited member computes from it. `Impl` is concrete and nothing
  // extends it (the closed world's final): `get` is a Boolean computed from `flag`, 2^2, and so is
  // `show` from `flag` and `b`, 2^(2 × 2). An object holds nothing: `Single.other` is one of the
  // 2^2 functions. A class something extends is read alike where nothing overrides a member of
  // it: `Base.get` is computed from `b`, 2^2. A template that may be extended, or is abstract,
  // reads what it inherits as it resolves it: `Half` and `Never` inherit the abstract
  // `show: Boolean => Boolean`, one of 2^2 functions, so `get` is a Boolean computed from it,
  // 2^(2^2) = 16 (`Never` is `abstract final`: it implements nothing and has no instance, and its
  // body is read as written against the abstract `show`). `Twin` inherits two abstract `f`s that
  // do not match, `TA.Base.f(TA.P)` and `TB.Mid.f(TB.P)`: each gives 2 `R`s, one per Boolean in
  // its `P`, so `g` returns one of 2 + 2 = 4 (one capability for both would give 2). A concrete
  // member a subclass overrides varies across instances, so it is a capability: `Wide.more`, which
  // `Narrow` overrides, is a Boolean beside `b`, and `get` is a Boolean computed from both,
  // 2^(2 × 2) = 16. `Sub` passes its `c` to `Base`, so what `Base` holds is `c`: `more` is a
  // Boolean computed from `c`, 2^2.
  // A sealed family or an enum is one value of its sum as a receiver: which case, and what the
  // case holds. `Fam` has the one case `One`, so `get` is one of 2 Booleans; `Color` has two
  // cases, so `f` is one of the 2^2 functions of them. The row captures the receiver as a whole.
  private def inherited =
    (count("recv.Impl.get") must_== Count.Finite(4))
      .and(count("recv.Impl.show") must_== Count.Finite(16))
      .and(count("recv.Single.other") must_== Count.Finite(4))
      .and(count("recv.Base.get") must_== Count.Finite(4))
      .and(count("recv.shown.show") must_== Count.Finite(4))
      .and(count("recv.Fam.get") must_== Count.Finite(2))
      .and(count("recv.Color.f") must_== Count.Finite(4))
      .and(entry("recv.Color.f").captures must_== List("<receiver recv.Color>"))
      .and(count("recv.Half.get") must_== Count.Finite(16))
      .and(count("recv.Never.get") must_== Count.Finite(16))
      .and(count("recv.Twin.g") must_== Count.Finite(4))
      .and(count("recv.Wide.get") must_== Count.Finite(16))
      .and(count("recv.Sub.more") must_== Count.Finite(4))

  // A local method counts over its own parameters and those of the methods around it: `Loc`'s
  // `inner` is a Boolean computed from `x` and `y`, 2^(2 × 2) = 16, and `LocC`'s returns `x` or
  // `y` as the class's Boolean `z` decides, 2^2 = 4. Its captures are `y`, and `y` and `z`. A member of an
  // anonymous class, which is final, reads the same: `Mk`'s `run` is a Boolean computed from `b`
  // and the `y` of the method the instance is made in, 16; an object inside such a class reads `y`
  // too (`Deep`: a Boolean computed from it, 2^2); a parameter named like a member the class
  // inherits cannot be named inside it, and is left (`Hid`; `DeepHid`, where an object in the class inherits it), and so is one the class's self alias
  // takes (`Self`). Left for later: a name an
  // inner definition takes again (`Hide`), a definition in a function literal, whose parameter
  // may be a generator's binder (`Lit`), a member of a local class, which a subclass in the same
  // body may override (`LocalK`), one in a case clause (`Pat`) and one in a constructor's body, which runs before the instance
  // holds what its members read (`Ctor`).
  private def locals = {
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    def left(name: String) = count(name) match {
      case Count.Unresolved(List(reason)) => reason.stripPrefix(prefix).takeWhile(_ != ':')
      case other                          => other.toString
    }
    (count("recv.Loc.outer.inner") must_== Count.Finite(16))
      .and(count("recv.LocC.outer.inner") must_== Count.Finite(4))
      .and(entry("recv.Loc.outer.inner").captures must_== List("y"))
      .and(entry("recv.LocC.outer.inner").captures must_== List("y", "z"))
      .and(left("recv.Hide.outer.inner") must_== "a capture a nearer scope binds again")
      .and(count("recv.Mk.mk.<new#1>.run") must_== Count.Finite(16))
      .and(entry("recv.Mk.mk.<new#1>.run").captures must_== List("y"))
      .and(left("recv.LocalK.f.K.get") must_== "a local class or object")
      .and(left("recv.Hid.mk.<new#1>.get") must_== "a capture an inherited member hides")
      .and(count("recv.Deep.mk.<new#1>.H.h") must_== Count.Finite(4))
      .and(left("recv.DeepHid.mk.<new#1>.H.get") must_== "a capture an inherited member hides")
      .and(left("recv.Self.mk.<new#1>.run") must_== "a capture a nearer scope binds again")
      .and(
        rows.sources
          .flatMap(_.toOption.toList.flatMap(_.methods))
          .filter(m => m.name.startsWith("recv.Ctor.") && m.name.endsWith(".hc"))
          .map(m => left(m.name)) must_== List("a definition in a constructor's body")
      )
      .and(
        rows.sources
          .flatMap(_.toOption.toList.flatMap(_.methods))
          .filter(m => m.name.startsWith("recv.Lit.") || m.name.startsWith("recv.Pat."))
          .filter(_.name.endsWith(".inner"))
          .map(m => left(m.name)) must_==
          List("a definition in a function literal", "a definition in a case clause")
      )
  }

  // What S4's first step leaves, each row uncovered for its own reason: a parent of the module with constructor parameters or in
  // a class (its members may read state the class's own code may not); a concrete sealed class or an enum case; a self
  // type narrower than the class; a deferred `inline val`, and a deferred inline method where a type parameter is in
  // scope; a class whose construction always throws, or in an object whose does (S6); an abstract
  // type member of a class only its own scope may extend, whose types are those
  // the program picks, one two enclosing instances hold, one another declaration bounds, and
  // `this.T` of a fixed class, which holds no type member of its own, inside one that holds it.
  // (`Bounded.get` is read now: `T <: AnyRef` says nothing of the values, so it returns `t`, 1.)
  private def later = {
    val expected = List(
      "Guest.K.g" -> "a parent with constructor parameters or in a class",
      "Closed.get" -> "an enclosing concrete sealed class or enum case",
      "Wished.get" -> "a deferred inline method over a type parameter",
      "Dead.In.get" -> "an object whose construction always throws or binds a pattern",
      "Self.me" -> "an enclosing class with a self type",
      "Narrowed.get" -> "an enclosing class with a self type",
      "Deferred.m" -> "a deferred inline value of an enclosing class",
      "Thrown.get" -> "an enclosing class whose construction always throws or binds a pattern",
      "Bounded.get" -> "Finite(1)",
      "CoKinded.get" -> "an abstract type member that is an alias, variant, or of another kind",
      "Pinned.get" -> "an abstract type member of a class no class written elsewhere extends",
      "Fin.get" -> "an abstract type member of a class no class written elsewhere extends",
      "WishedM.get" -> "a deferred inline method over a type parameter",
      "Holds.Fixed.get" -> "a type",
      "Twice.Again.get" -> "an abstract type member two enclosing instances hold",
      "Mixed.get" -> "an abstract type member another declaration of its name bounds or hides"
    )
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    expected.map { (name, _) =>
      count(s"recv.$name") match {
        case Count.Unresolved(List(reason)) => reason.stripPrefix(prefix).takeWhile(_ != ':')
        case other                          => other.toString
      }
    } must_== expected.map(_._2)
  }

  // No row the compiled front end measures differs from the scalameta front end's, and none it
  // counts past a model limit of the old reading is left without a derivation.
  // `Bounded.get` is derived above (1); the old front end refuses a bounded abstract type member.
  private def agrees = {
    val compared = Differential.run(input, Differential.answer(ScalametaFrontEnd, input.old))
    val derived =
      compared.compared.filter(_.row.name == "recv.Bounded.get").map(_.row.key -> "1").toMap
    ((compared.failed, compared.noted) must_== (Nil, Nil))
      .and(derived.size must_== 1)
      .and(Differential.failures(compared.compared, derived) must_== Nil)
  }

}
