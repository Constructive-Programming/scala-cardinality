package cardinality.compiled

import cardinality.Compiled
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.{BooleanSize, Definition, IntSize, TinySize, UnitSize}
import cardinality.compiled.TastyFrontEndSpec.{subject, tag}
import cardinality.reporting.{FrontEnd, Report}
import cardinality.tasty.CompiledTypes
import java.nio.file.{Files, Path}
import org.specs2.Specification

/** The compiled front end's first slice (phase 4 of docs/plans/compiled-own-types.md). Every
  * expected count is derived in its example's comment.
  */
class TastyFrontEndSpec extends Specification {

  def is = s2"""
  The compiled front end reads rows from symbols and typed trees
    an object's method counts over its own parameters            $methods
    a definition over base types multiplies and adds them         $definitions
    a member of an object that is never built has one body        $unbuilt
    what the counting model refuses is a row of its own           $refused
    classes compiled from another text refuse the source          $stale
    a source without definitions is empty, a bound name is a row  $sources
    a class of the module is the product or sum it declares       $products
    a type the model does not read is refused, as before          $refusals
    a type no slice reads yet is uncovered, or counted where noted $later
    a definition row is what it stores, its reasons, or a dash    $stored
    a definition the stored side does not read yet is uncovered   $unstored
    a class's row is the product of its constructor's positions   $constructors
    an inline method counts unless a call site decides it         $inlines
  """

  private val text =
    """package demo
      |
      |object O {
      |  def pick[A](x: A, y: A): A = x
      |  def both(b: Boolean): Boolean = b
      |  val n: Int = 1
      |}
      |object K { val z: Boolean = ???; def g(b: Boolean): Boolean = !b }
      |object KE { val z: Boolean = sys.error("no"); def g(b: Boolean): Boolean = !b }
      |object KT { val z: Boolean = throw new RuntimeException; def g(b: Boolean): Boolean = !b }
      |object KL { lazy val z: Boolean = ???; def g(b: Boolean): Boolean = !b }
      |final case class P(a: Boolean, b: Boolean)
      |final class W(x: Int, y: Boolean)
      |enum Suit { case Hearts, Spades, Clubs }
      |class C(x: Int) { def m(b: Boolean): Boolean = b }
      |object Refused { def arr[A](xs: Array[A]): Int = 0 }
      |object In {
      |  import scala.compiletime.erasedValue
      |  inline def pick[A](x: A, y: A): A = x
      |  inline def typed[A](x: A, y: A): A = inline erasedValue[A] match {
      |    case _: Int => x
      |    case _      => y
      |  }
      |  inline def one[A](x: A): Unit = inline erasedValue[A] match { case _ => () }
      |  inline def param(inline b: Boolean): Boolean = b
      |  transparent inline def narrow(b: Boolean): Unit = ()
      |}
      |object KI { val z: Boolean = ???; inline def g(b: Boolean): Boolean = !b }
      |""".stripMargin

  // What the compiler desugars or adds, each with the name the scalameta front end gives its row:
  // scopes of a body, nameless definitions, constructors, and members nobody wrote.
  private val shapes =
    """package shapes
      |
      |trait Show[A] { def show(a: A): Boolean }
      |trait Plain
      |trait Params(val n: Int)
      |final case class Pair(left: Boolean, right: Boolean = false)
      |enum Tree { case Leaf(b: Boolean); case Node(l: Tree, r: Tree) }
      |type Alias[X] = List[X]
      |type Lambda = [X] =>> List[X]
      |opaque type Flag = Boolean
      |trait Bound[A <: (Int, Int)]
      |@main def start(b: Boolean): Unit = ()
      |object Holder { class start }
      |object Context {
      |  def implied: Int ?=> Boolean = { def inner(x: Boolean): Boolean = x; inner(true) }
      |  def run(b: Int ?=> Boolean): Boolean = true
      |  def caller: Boolean = run { def passed(x: Boolean): Boolean = x; passed(true) }
      |  def literal: Int ?=> Boolean = (c: Int) ?=> { def lit(x: Boolean): Boolean = x; lit(true) }
      |}
      |class Outer(x: Int) {
      |  def this() = this(0)
      |  class Inner { def m(b: Boolean): Boolean = b }
      |  var state: Boolean = false
      |}
      |object Members {
      |  given named: Show[Int] with { def show(a: Int): Boolean = true }
      |  given Show[Boolean] with { def show(a: Boolean): Boolean = a }
      |  given listed[A](using a: Show[A]): Show[List[A]] with { def show(a: List[A]): Boolean = true }
      |  extension (b: Boolean) def flip: Boolean = !b
      |  def defaults(b: Boolean = true): Boolean = b
      |  def scopes(b: Boolean): Boolean = {
      |    def local(x: Boolean): Boolean = x
      |    val made = new Plain { def here(x: Boolean): Boolean = x }
      |    val f = (x: Boolean) => { def inner(y: Boolean): Boolean = y; inner(x) }
      |    if (b) { def nested(x: Boolean): Boolean = x; nested(b) }
      |    else b match { case true => def arm(x: Boolean): Boolean = x; arm(b); case false => f(b) }
      |  }
      |}
      |""".stripMargin

  // The types of the module slice S1 reads, and beside them those it refuses or leaves.
  private val types =
    """package types
      |
      |final case class Pair(a: Boolean, b: Boolean)
      |final case class Box[A](value: A)
      |enum Color { case Red, Green, Blue }
      |enum Opt[+T] { case Sm(t: T); case Nn }
      |sealed trait Figure
      |case object Dot extends Figure
      |final case class Line(thick: Boolean) extends Figure
      |final class Solid(val a: Boolean, val b: Boolean) extends Figure
      |type Drawn = Figure
      |sealed trait Res[+E, +A]
      |final case class Ok[+A](a: A) extends Res[Nothing, A]
      |final case class Err[+E](e: E) extends Res[E, Nothing]
      |sealed trait Nope { def no: Nothing }
      |case object Never extends Nope { def no: Nothing = ??? }
      |trait Marker[A] { type T }
      |object Tokens { opaque type Token = Boolean; def inside(t: Token): Token = t }
      |sealed trait Hidden
      |final class Secret(x: Boolean) extends Hidden
      |sealed trait Loop
      |final case class Step(next: Loop) extends Loop
      |sealed trait Anon
      |object Anons { val one: Anon = new Anon {} }
      |sealed trait Thrown
      |case object Bad extends Thrown { val z: Int = ??? }
      |enum Arg(val n: Int) { case One extends Arg(???) }
      |sealed trait Index[A]
      |case object OfInt extends Index[Int]
      |final case class Record(f: Boolean) { def more: Int = 1 }
      |final case class Veiled(private val b: Boolean)
      |final case class Shut private (b: Boolean)
      |final case class Curried(a: Boolean)(val b: Boolean)
      |class Outer(val k: Boolean) { enum In { case A, B; def key: Boolean = k } }
      |sealed trait Giv
      |given Giv with {}
      |sealed trait Inn
      |class Encl { case object Inner extends Inn }
      |sealed trait Opn
      |trait Opn1 extends Opn
      |sealed trait Ext
      |class ExtA(val b: Boolean) extends Ext
      |class ExtB extends ExtA(true)
      |sealed trait Cls
      |final case class Cls2(a: Boolean)(val b: Boolean) extends Cls
      |sealed trait Rst
      |final case class RstC private (b: Boolean) extends Rst
      |sealed trait Fld
      |final case class FldC(private val b: Boolean) extends Fld
      |sealed trait Bare
      |final class BareC extends Bare
      |sealed trait Under
      |case object U1 extends Under
      |enum Mixed { case M1 extends Mixed with Under; case M2 }
      |sealed trait Two[A, B]
      |final case class Twice[A](a: A) extends Two[A, A]
      |sealed trait Co[+A]
      |final case class Inv[A](a: A) extends Co[A]
      |sealed trait Cov[+A]
      |case object Whole extends Cov[Any]
      |sealed trait Bounded[A]
      |final case class Small[A <: Unit](a: A) extends Bounded[A]
      |sealed trait Diamond
      |sealed trait Facet1 extends Diamond
      |sealed trait Facet2 extends Diamond
      |final case class Both(b: Boolean) extends Facet1 with Facet2
      |sealed trait Holder
      |enum Held extends Holder { case H }
      |sealed trait Split
      |case object Parts extends Split { val (x, y): (Int, Int) = (1, 2) }
      |sealed abstract class Rep(xs: Int*)
      |case object RepA extends Rep(1, ???)
      |sealed abstract class Nam(x: Int)
      |case object NamA extends Nam(x = ???)
      |sealed abstract class Ord(x: Int, y: Int)
      |case object OrdA extends Ord(y = 1, x = ???)
      |sealed abstract class Cur(x: Int)(y: Int)
      |case object CurA extends Cur(1)(???)
      |sealed trait Asc
      |case object AscA extends Asc { val z: Int = (??? : Int) }
      |enum Der derives CanEqual { case D1, D2 }
      |sealed trait Lazy
      |case object Lz extends Lazy { lazy val (p, q) = (true, false) }
      |trait Base { def no: Nothing }
      |sealed trait Heir extends Base
      |case object Sole extends Heir { def no: Nothing = ??? }
      |sealed trait Absurd { def no(n: Nothing): Nothing }
      |case object Vacuous extends Absurd { def no(n: Nothing): Nothing = n }
      |sealed trait Mid[+A]
      |sealed trait Low[+B] extends Mid[B]
      |final case class Tip[+C](c: C) extends Low[C]
      |case object End extends Low[Nothing]
      |sealed trait Sink[-A]
      |case object Drain extends Sink[Any]
      |final case class Twin(a: Boolean) { def this() = this(true) }
      |final case class P1(b: Boolean)
      |final case class P2(b: Boolean)
      |object Knot { opaque type T = Option[Tie]; final case class Tie(t: T); def none: T = None }
      |opaque type Top = Boolean
      |def top(t: Top): Top = t
      |object Use {
      |  def pair(p: Pair): Boolean = p.a
      |  def box[A](b: Box[A]): A = b.value
      |  def color: Color = Color.Red
      |  def opt[A](o: Opt[A]): Opt[A] = o
      |  def figure(b: Boolean): Figure = Dot
      |  def drawn: Drawn = Dot
      |  def swap[E, A](r: Res[E, A]): Res[A, E] = ???
      |  def nope(n: Nope): Boolean = true
      |  def marker(m: Marker[Int]): Boolean = true
      |  def token(t: Tokens.Token): Tokens.Token = t
      |  def never(n: Nothing): Boolean = n
      |  def hidden: Hidden = ???
      |  def loop: Loop = ???
      |  def anon: Anon = ???
      |  def thrown: Thrown = ???
      |  def arg: Arg = ???
      |  def sm(s: Opt.Sm[Boolean]): Boolean = true
      |  def index[A](i: Index[A]): Boolean = true
      |  def record(r: Record): Boolean = true
      |  def line(l: Line): Boolean = true
      |  def veiled(v: Veiled): Boolean = true
      |  def shut(s: Shut): Boolean = true
      |  def curried(c: Curried): Boolean = true
      |  def nested(x: Outer#In): Boolean = true
      |  def giv: Giv = ???
      |  def inn: Inn = ???
      |  def opn: Opn = ???
      |  def ext: Ext = ???
      |  def cls: Cls = ???
      |  def rst: Rst = ???
      |  def fld: Fld = ???
      |  def bare: Bare = ???
      |  def under: Under = ???
      |  def mixed: Mixed = ???
      |  def twice(t: Two[Boolean, Unit]): Boolean = true
      |  def inv[A](c: Co[A]): Boolean = true
      |  def rep: Rep = ???
      |  def nam: Nam = ???
      |  def ord: Ord = ???
      |  def cur: Cur = ???
      |  def asc: Asc = ???
      |  def whole(c: Cov[Int]): Boolean = true
      |  def der: Der = ???
      |  def lz: Lazy = ???
      |  def heir: Heir = ???
      |  def small[A](b: Bounded[A]): Boolean = true
      |  def diamond: Diamond = ???
      |  def holder: Holder = ???
      |  def split: Split = ???
      |  def absurd: Absurd = ???
      |  def mid[A](m: Mid[A]): Mid[A] = m
      |  def sink(s: Sink[Int]): Boolean = true
      |  def twin: Twin = ???
      |  def outside(t: Top): Top = t
      |  def same(ev: P1 =:= P2): Boolean = true
      |}
      |""".stripMargin

  // The definition rows slice S2 reads, and those it leaves.
  private val sizes =
    """package sizes
      |
      |final case class Pt(x: Boolean, y: Boolean)
      |final class Wide(val f: Option[Boolean] => Boolean, o: Option[Boolean]) extends Open
      |final case class Gen[A](a: A)
      |final case class Phantom[A](b: Boolean)
      |final class Hk[F[_], A](x: F[A])
      |trait Open
      |abstract class Base(x: Int)
      |final case class Holds(o: Open)
      |final case class Uses(g: Gen[Boolean], p: Pt)
      |final case class Fails(e: Exception)
      |trait HasM { type M; final case class In(m: M) }
      |enum Tri { case A, B; case C(b: Boolean) }
      |enum Box[+T] { case Full(t: T); case Empty }
      |final case class Boxed(b: Box[Boolean])
      |enum Lvl(val n: Int) { case Lo extends Lvl(1) with Open; case Hi extends Lvl(2) }
      |enum Idx[A] { case OfInt extends Idx[Int] }
      |type Two = (Boolean, Option[Boolean])
      |type Fn[X] = X => Boolean
      |type Ctor = [X] =>> List[X]
      |type Un = Boolean | Unit
      |type Same = Boolean | Boolean
      |type Meet = Matchable & Boolean
      |type Strs = List[String]
      |type Fns = List[String => String]
      |type Never = Nothing => Boolean
      |type Opt = Option[Boolean]
      |type Mp = Map[Boolean, Unit]
      |type Pf = PartialFunction[Boolean, Unit]
      |type St = Set[Option[Boolean]]
      |type Ei = Either[Boolean, Unit]
      |type Arr = Array[Nothing]
      |type Lit = 3
      |opaque type Hid = Boolean
      |final case class Sees(h: Hid)
      |type Ident[A] = A
      |object Scope {
      |  opaque type T = Boolean
      |  final class Wrap(x: T)
      |  type Al = T
      |  type Id[X] = X
      |  final class Via(x: Ident[T])
      |  final class Held(x: Holder#Me)
      |  opaque type Up = Inner.Low
      |  object Inner { opaque type Low = Boolean; final class Deep(x: Up) }
      |}
      |class Holder { type Me = Scope.T }
      |object UseAl { def al(a: Scope.Al): Boolean = true }
      |final class SeesWrap(w: Scope.Wrap, a: Scope.Al)
      |final class SeesT(t: Scope.T, i: Scope.Id[Scope.T])
      |class Prj[A] { type Me = A }
      |final class Seen(x: Prj[Unit]#Me)
      |val top: Boolean = true
      |val empty: EmptyTuple = EmptyTuple
      |val none: Nothing => Boolean = ???
      |val meet: Boolean & Matchable = true
      |sealed trait Fam
      |case object F1 extends Fam
      |final case class HasFam(f: Fam, b: Boolean)
      |sealed abstract class Ior[+A, +B]
      |object Ior {
      |  final case class Left[+A](a: A) extends (A Ior Nothing)
      |  final case class Right[+B](b: B) extends (Nothing Ior B)
      |  final case class Both[+A, +B](a: A, b: B) extends (A Ior B)
      |}
      |final case class Iors(i: Ior[Boolean, Boolean])
      |object UseIor { def split(i: Ior[Boolean, Boolean]): Boolean = true }
      |sealed trait Thr
      |case object T1 extends Thr { val z: Int = ??? }
      |sealed trait Wide2
      |class Leaf2(val b: Boolean) extends Wide2
      |class Under2 extends Leaf2(true)
      |sealed trait Loose
      |case class Free(b: Boolean) extends Loose { def more: Int = 1 }
      |sealed trait Kept { def b: Boolean }
      |case class Plain(b: Boolean) extends Kept { override def toString: String = "p" }
      |sealed trait Twice2
      |sealed trait T2a extends Twice2
      |sealed trait T2b extends Twice2
      |case object T2c extends T2a with T2b
      |sealed abstract class Lvl2
      |sealed trait Lvl2a extends Lvl2
      |case object Lvl2b extends Lvl2a
      |case object Lvl2c extends Lvl2
      |sealed trait Dbl
      |sealed trait Dbl1 extends Dbl
      |case object DblBoth extends Dbl with Dbl1
      |sealed trait Impl { def k: Int }
      |case class ImplC(i: Boolean) extends Impl { def k: Int = 1 }
      |sealed trait Outer
      |case class OuterC(b: Boolean) extends RuntimeException with Outer
      |sealed trait Mixed
      |final case class MixedC(f: String => String) extends Mixed
      |enum MixedE { case M1 extends MixedE with Mixed; case M2 }
      |final case class HasMixed(m: Mixed)
      |sealed trait Deep
      |sealed trait DeepL extends Deep
      |enum DeepE { case D1 extends DeepE with DeepL }
      |sealed trait Prot
      |case class ProtC(b: Boolean) extends Prot { protected def more: Int = 1 }
      |final case class Rec(next: Option[Rec])
      |final class Priv private (x: Int)
      |type Shape = { def area: Int }
      |type Dep = (x: Boolean) => x.type
      |type Wild = List[?]
      |type Path = java.nio.file.Path
      |val made: Pt = Pt(true, true)
      |""".stripMargin

  // The constructor rows slice S3 reads, and those it leaves.
  private val built =
    """package built
      |
      |final case class Pt(x: Boolean, y: Boolean)
      |final class Walk(val cur: Any, val n: Int)
      |final case class Pair[A](x: A, y: A)
      |final class Txt(s: String)
      |final class Fun(f: String => String)
      |final class Nul(x: Null)
      |trait Open
      |final class Holds(o: Open)
      |final class Absurd(f: Nothing => Int)
      |final class Mixed[A](x: (A, Int))
      |final case class Rec(next: Option[Rec])
      |final class Foo { type T = Byte }
      |object Paths { val x: Foo = new Foo }
      |final class Via(t: Paths.x.T)
      |final class Using(x: Int)(using o: Open)
      |final class Fns(fs: List[String => String])
      |final class Out(f: Unit => Any)
      |final case class Anything(x: Any)
      |final class Holder(w: Anything)
      |final class Vals(x: AnyVal)
      |final case class Ph[A](t: Throwable)
      |final class Rest[A](xs: Ph[A]*)
      |type Top = Any
      |final class Aliased(x: Top)
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled(
    "Demo.scala" -> text,
    "Shapes.scala" -> shapes,
    "Types.scala" -> types,
    "Sizes.scala" -> sizes,
    "Built.scala" -> built
  )

  private lazy val rows = TastyFrontEnd.rows(input)
  private lazy val source = rows.sources.collect { case Right(source) => source }.head

  private def count(name: String): Count = {
    val methods = rows.sources.flatMap(_.toOption.toList.flatMap(_.methods))
    methods.filter(_.name == name).head.count
  }

  // `pick` returns x or y: 2. `both` is a function Boolean => Boolean: 2^2. `n` is any Int: 2^32.
  private def methods =
    (count("demo.O.pick") must_== Count.Finite(2))
      .and(count("demo.O.both") must_== Count.Finite(4))
      .and(count("demo.O.n") must_== Count.Finite(BigInt(2).pow(32)))

  // An inline method counts like a `def` (AGENTS.md, "Inline methods count as methods"): `pick`
  // returns x or y, 2, and `param`, whose inline parameter has its argument's value, is one of
  // the 2^2 functions. `typed` matches on its type argument at compile time, so each expansion
  // may differ: the row is refused with the model's reason, not left to a later slice. `one`
  // does the same and returns `Unit`: a goal with one value is 1 before the body is read, but
  // after rule X, which refuses the `transparent` `narrow` whose declared `Unit` is no singleton.
  // A call of an inline method is its expansion and builds no object: `KI.g` is one of the 2^2,
  // though `KI` is never built.
  private def inlines =
    (count("demo.In.pick") must_== Count.Finite(2))
      .and(count("demo.In.param") must_== Count.Finite(4))
      .and(count("demo.In.one") must_== Count.Finite(1))
      .and(count("demo.KI.g") must_== Count.Finite(4))
      .and(count("demo.In.narrow") must beLike {
        case Count.Unresolved(List(reason)) => reason must startWith("transparent inline result")
      })
      .and(count("demo.In.typed") must beLike {
        case Count.Unresolved(List(reason)) => reason must startWith("inline method: ")
      })

  // An object is one value; `P` holds 2 × 2; `W` 2^32 × 2, a capacity; `Suit` 1 + 1 + 1.
  private def definitions = {
    val sizes = source.definitions.map(d => d.name -> d.size).toMap
    (sizes("demo.O") must_== Some(UnitSize))
      .and(sizes("demo.P") must_== Some(TinySize(4)))
      .and(sizes("demo.W") must_== Some(IntSize * BooleanSize))
      .and(sizes("demo.Suit") must_== Some(TinySize(3)))
  }

  // `K`'s constructor evaluates `z`'s initializer, which throws (`???`, `sys.error`, `throw`): no
  // `K` is built, so no call of `g` returns and every body of it is the same one (register D26).
  // `z` itself is the choice being counted: one of 2 Booleans. A lazy `z` is not evaluated by the
  // constructor: `KL` is built, and its `g` is one of the 2^2 functions.
  private def unbuilt =
    (List("K", "KE", "KT").map(k => count(s"demo.$k.g")) must_== List.fill(3)(Count.Finite(1)))
      .and(count("demo.K.z") must_== Count.Finite(2))
      .and(count("demo.KL.g") must_== Count.Finite(4))

  // `Pair` is 2 × 2, so `pair` is one of 2^4 functions. A `Box[A]` holds one A: `box` returns it.
  // `Color` has 3 values. `Opt[A]` is A + 1: `opt` sends `Nn` to `Nn` (it has no A) and `Sm(a)` to
  // `Sm(a)` or `Nn`, 2. `Figure` is 1 + 2 + 2 × 2 = 7 (an object, a case class, a class nothing
  // extends with public fields), so `figure` is one of 7^2 and `drawn`, through the alias, one of
  // 7. `Res[E, A]` is A + E, each case passing its parameter to one slot and pinning the other
  // with `Nothing`: `swap` sends `Ok(a)` to `Err(a)` and `Err(e)` to `Ok(e)`, 1. Every `Nope`
  // would implement `no: Nothing`, so there is none and `nope` has the one empty body. A `Marker`
  // has only a type member: one value, and `marker` picks one of 2 Booleans. `Token` is a Boolean
  // inside `Tokens` (`inside` is one of 2^2) and one value outside it (rule F): `token` returns it.
  // `Nothing` has no value, so `never` is the one function from it. `Mid[A]` is A + 1 through the
  // sealed level `Low`, which passes its parameter on: `mid` is `opt` again, 2. `Sink[Int]` is its
  // one object, pinned with `Any` in a contravariant slot: `sink` picks one of 2 Booleans. `Twin`
  // is its one Boolean, the secondary constructor adding no value: 2. `Top` is a Boolean for the
  // definitions of its file's top level (`top` is one of 2^2) and one value in `Use` (`outside`).
  // `Der` has 2 values.
  // `Line`, a final case class with a parent, is its one Boolean: `line` is one of 2^2.
  private def products = {
    val expected = List(
      "pair" -> 16,
      "box" -> 1,
      "color" -> 3,
      "opt" -> 2,
      "figure" -> 49,
      "drawn" -> 7,
      "swap" -> 1,
      "nope" -> 1,
      "marker" -> 2,
      "token" -> 1,
      "never" -> 1,
      "mid" -> 2,
      "sink" -> 2,
      "twin" -> 2,
      "outside" -> 1,
      "der" -> 2,
      "mixed" -> 2,
      "line" -> 4
    )
    (expected.map((name, _) => count(s"types.Use.$name")) must_==
      expected.map((_, n) => Count.Finite(n)))
      .and(count("types.Tokens.inside") must_== Count.Finite(4))
      .and(count("types.top") must_== Count.Finite(4))
      // An input `Ior[Boolean, Boolean]` splits into its 8 values: 2^8 functions to Boolean.
      .and(count("sizes.UseIor.split") must_== Count.Finite(256))
      .and(count("sizes.Iors.<init>") must_== Count.Finite(8))
  }

  // Each row's `?`, without what it names: the model's reason, or what a later slice reads. A row
  // on the other side answers its whole count.
  private def reasons(uncovered: Boolean, names: List[String]): List[String] = {
    val prefix = s"${TastyFrontEnd.Uncovered}: "
    names.map(name =>
      count(s"types.Use.$name") match {
        case Count.Unresolved(List(reason)) if reason.startsWith(prefix) == uncovered =>
          reason.stripPrefix(prefix).takeWhile(_ != ':')
        case other => other.toString
      }
    )
  }

  // A plain class's parameter without `val` is no member: values that differ only there may be
  // one. A type that reaches itself (through a class, or through an opaque alias read in its
  // scope), a family with an anonymous instance, an enum under a sealed family and an enum's class
  // case as a type are not read by the counting model: each is its `?`, with the model's reason.
  // So is a family with a case its file does not enumerate whole: a given object, an object nested
  // in a class, an open trait, a class something extends, a class with two parameter lists.
  private def refusals = {
    val expected = List(
      "hidden" -> "sealed subtype hides a parameter",
      "loop" -> "recursive type not read",
      "anon" -> "sealed family has local or anonymous subclasses in its file",
      "holder" -> "sealed subtype outside the closed fragment",
      "sm" -> "enum case type not read",
      "giv" -> "sealed family has a given instance outside the fragment",
      "inn" -> "sealed subtype is an inner or local class",
      "opn" -> "sealed family has an open abstract subtype",
      "ext" -> "sealed subtype outside the closed fragment",
      "cls" -> "sealed subtype shape unreadable"
    )
    (reasons(false, expected.map(_._1)) must_== expected.map(_._2))
      .and(count("types.Knot.none") must beLike {
        case Count.Unresolved(List(reason)) => reason must startWith("recursive type not read")
      })
  }

  // What no slice reads yet, each for its own reason, and what decision D5 now counts.
  //   - A capability record with a private field no member exposes is not read yet, nor one with a
  //     private constructor and no factory (rule B; `record` and `curried`, whose fields are
  //     public, are read: `RecordsSpec`); and a member of an enum nested in a class may read the
  //     instance around it (`key` reads `k`).
  //   - A case that pins its parent's parameter to a type holds values of `Index[Int]` only; one
  //     that passes a parameter to two slots equates them; `Any` in a covariant slot is no bottom
  //     pin; a case with a bounded parameter is no case at other types.
  //   - A case under two sealed levels would be summed twice. An enum's value case under a family
  //     is the enum's to read first (the enum itself reads it: `mixed` is one of 2). A case with a private constructor
  //     or field is rule B's to read. Read now: `inv` 2 (`Co[A]` holds `Inv[S]`, `S <: A`, whose
  //     `a` is an `A`: an atom, so a constant); `bare` 1 (no field; the old reading refuses it).
  //   - A case whose construction throws (an object's body, there ascribed too, an enum case's
  //     argument, a repeated or a named argument of a parent, in order or not, in any clause) is
  //     never built and adds no summand (register D26): each of those families is empty, and a
  //     method returning one has no implementation, 0. A pattern definition is an ordinary
  //     binding, a lazy one too: `Split` and `Lazy` have their one object, 1. A `Nothing` member
  //     inherited from an open trait, which leaves `Heir` no value, is not read yet.
  //   - `no(n: Nothing)` has a total body.
  //   - `P1` and `P2` have one shape and are two types: no evidence equates them.
  private def later = {

    val slots = "a sealed subtype that does not pass its own parameters to its parent"
    val expected = List(
      "veiled" -> "a capability record with a parameter that is no public field",
      "shut" -> "a capability record with no constructor or factory the site may call",
      "nested" -> "a class nested in a class or a body",
      "index" -> slots,
      "twice" -> slots,
      "inv" -> "Finite(2)",
      "whole" -> slots,
      "small" -> slots,
      "diamond" -> "a sealed subtype reached twice",
      "under" -> "an enum case under a sealed family",
      "rst" -> "a restricted constructor",
      "fld" -> "a restricted field",
      "bare" -> "Finite(1)",
      "thrown" -> "Finite(0)",
      "arg" -> "Finite(0)",
      "rep" -> "Finite(0)",
      "nam" -> "Finite(0)",
      "ord" -> "Finite(0)",
      "cur" -> "Finite(0)",
      "asc" -> "Finite(0)",
      "split" -> "Finite(1)",
      "lz" -> "Finite(1)",
      "heir" -> "a construction that inherits a member of the empty type",
      "absurd" -> "an abstract member of the empty type with parameters",
      "same" -> "a type of the module in an evidence type"
    )
    reasons(true, expected.map(_._1)) must_== expected.map(_._2)
  }

  // A definition row of `sizes`: its size, and the reasons it is no number, each with its kind.
  private def row(name: String): (Option[String], List[String]) = {
    val found = rows.sources
      .flatMap(_.toOption.toList.flatMap(_.definitions))
      .filter(_.name == s"sizes.$name")
      .head
    (found.size.map(_.render), found.unbound.map(reason => s"${reason.tag}:${reason.subject}"))
  }

  //   - `Pt` is 2 × 2. `Wide` is (2^3) × (1 + 2) = 24, its parent and the missing `val` no matter
  //     to what its constructor stores. `Gen` stores an A: no one size, the reason its parameter;
  //     `Phantom` stores none, 2; `Hk` stores an `F[A]`, the reason its higher-kinded parameter.
  //   - An open trait and an abstract class have no value of their own: a dash. A field of one may
  //     hold any subtype's value: the reason is the open type, as for an `Exception`. A field of
  //     an abstract type member is what each instance chooses.
  //   - `Uses` is `Gen` at Boolean, 2, times `Pt`, 4. `Tri` is 1 + 1 + 2. `Box` stores a T in one
  //     case, and `Box[Boolean]` is 2 + 1. `Lvl` is its two cases, its constructor's argument
  //     shared state and a case's other parent no matter.
  //   - A tuple multiplies, 2 × 3. `Fn` is a function of its parameter; a constructor alias is a
  //     dash. A union adds, 2 + 1, once where both sides are one type, 2; an intersection is its
  //     smaller side, 2. A list of strings is ω and one of functions over them ε₀; a function
  //     from `Nothing` is the algebra's 0. `Option` adds one, 3; a map is (1 + 1)^2 = 4 and so is
  //     a partial function; a set is 2^3 = 8; `Either` is 2 + 1; an array of nothing is the empty
  //     one; a literal type is one value. `Rec` is R = 1 + R: from 0, 1, then 2, and a cycle that
  //     keeps growing widens to its tier, ω (`Fixpoint`).
  //   - An opaque alias's row shows its representation, 2, and a class outside its scope stores
  //     one value of it (rule F). A type is read where it is written: `Scope.Wrap` stores its
  //     Boolean and `Scope.Al` is that Boolean too, 2 × 2 in `SeesWrap`. `Scope.T` named outside
  //     is one value, also as the argument of an alias written inside (`SeesT`, 1 × 1), and it is
  //     a Boolean named inside as the argument of an alias written outside (`Scope.Via`, 2).
  //     `Scope.Up` is an `Inner.Low` written outside `Inner`: one value, also where `Inner.Deep`
  //     names it.
  //   - `Prj[Unit]#Me` is `Prj`'s A as its prefix fixes it: Unit, 1.
  //   - A sealed family is the sum of its cases: `Fam` is its one object, and `HasFam` 1 × 2.
  //     `Ior[A, B]` is A + B + A × B, no one size, the reasons its parameters; at Boolean and
  //     Boolean it is 2 + 2 + 4 = 8 (`Iors`). (Its row shows 3ω: each unbound parameter is ω, and
  //     ω + ω + ω × ω is 3ω in the size algebra.) A class nothing extends is final in the closed
  //     world, written so or not, whatever members it declares: `Kept`, `Loose`, `Prot` and
  //     `Impl` are each their one case class's 2 (the old estimate refuses the last three). A case under a
  //     sealed trait that extends a sealed class is the trait's: `Lvl2` is 1 + 1.
  //     An enum's value case that extends a family makes the row the old estimate's refusal,
  //     whatever its other cases hold: `Mixed` has no size and that reason alone.
  //   - A top-level value is one value, the empty tuple and a built `Pt` (`made`) too. One of a type
  //     read as 0, or of an intersection, whose size does not say a value exists, is undecided.
  private def stored = {
    def sized(size: String) = (Some(size), Nil)
    def unbound(reason: String, size: String = "ω") = (Some(size), List(reason))
    val expected = List[(String, (Option[String], List[String]))](
      "Pt" -> sized("4"),
      "Wide" -> sized("24"),
      "Gen" -> unbound("param:A"),
      "Phantom" -> sized("2"),
      "Hk" -> unbound("param:F[_]"),
      "Open" -> (None, Nil),
      "Base" -> (None, Nil),
      "Holds" -> unbound("open:Open"),
      "Fails" -> unbound("open:Exception"),
      "HasM#In" -> unbound("member:M"),
      "Uses" -> sized("8"),
      "Tri" -> sized("4"),
      "Box" -> unbound("param:T", "ω + 1"),
      "Boxed" -> sized("3"),
      "Lvl" -> sized("2"),
      "Two" -> sized("6"),
      "Fn" -> unbound("param:X"),
      "Ctor" -> (None, Nil),
      "Un" -> sized("3"),
      "Same" -> sized("2"),
      "Meet" -> sized("2"),
      "Strs" -> sized("ω"),
      "Fns" -> sized("ε₀"),
      "Rec" -> sized("ω"),
      "Never" -> sized("0"),
      "Thr" -> sized("0"),
      "Opt" -> sized("3"),
      "Mp" -> sized("4"),
      "Pf" -> sized("4"),
      "St" -> sized("8"),
      "Ei" -> sized("3"),
      "Arr" -> sized("1"),
      "Lit" -> sized("1"),
      "Hid" -> sized("2"),
      "Sees" -> sized("1"),
      "SeesWrap" -> sized("4"),
      "Scope.Via" -> sized("2"),
      "Scope.Inner.Deep" -> sized("1"),
      "SeesT" -> sized("1"),
      "Seen" -> sized("1"),
      "Fam" -> sized("1"),
      "HasFam" -> sized("2"),
      "Ior" -> (Some("3ω"), List("param:A", "param:B")),
      "Iors" -> sized("8"),
      "Kept" -> sized("2"),
      "Loose" -> sized("2"),
      "Prot" -> sized("2"),
      "Impl" -> sized("2"),
      "Lvl2" -> sized("2"),
      "Mixed" ->
        (None, List("refusal:sealed family not summed: Mixed (an enum case extends it: M1)")),
      "top" -> sized("1"),
      "made" -> sized("1"),
      "empty" -> sized("1"),
      "none" ->
        unbound("refusal:value not decided: Function1[Nothing, Boolean]", "1"),
      "meet" -> unbound("refusal:value not decided: Boolean & Matchable", "1")
    )
    expected.map((name, _) => name -> row(name)) must_== expected
  }

  // Uncovered on the stored side, each for its own reason: a sealed family with a subtype
  // something extends (`Wide2`) or that inherits from a class outside the module, whose state it
  // holds too (`Outer`), or with a case under two of its levels (`Twice2`; `Dbl`, whose clause
  // names the family and its level both), or with an enum's value case under a sealed level
  // (`Deep`) or behind a reference (`HasMixed`); a class's alias read as its prefix sees it from
  // inside an opaque type's scope, where what it names was written elsewhere; a private
  // constructor (rule B); an enum case that pins its enum's parameter to a type; a refinement
  // written without a type, not told from one of `Object`; a dependent function type, whose result
  // its parent does not hold; a wildcard argument; a class the algebra lacks.
  private def unstored = {
    val expected = List(
      "Wide2" -> "a family with a subtype something extends",
      "Twice2" -> "a subtype reached twice",
      "Dbl" -> "a subtype reached twice",
      "Deep" -> "an enum case under a sealed family",
      "HasMixed" -> "an enum case under a sealed family",
      "Outer" ->
        "a family with a subtype not written final whose parent is outside the module",
      "Scope.Held" -> "an alias of a class beside an opaque type's scope",
      "Priv" -> "a restricted constructor",
      "Idx" -> "an enum case that does not pass its own parameters on",
      "Shape" -> "a refinement of no written type",
      "Dep" -> "a function type refined by its apply",
      "Wild" -> "a wildcard type argument",
      "Path" -> "a class the algebra does not model"
    )
    val prefix = s"refusal:${TastyFrontEnd.Uncovered}: "
    expected.map((name, _) =>
      row(name)._2.map(reason =>
        if (reason.startsWith(prefix)) reason.stripPrefix(prefix).takeWhile(_ != ':') else reason
      )
    ) must_== expected.map((_, why) => List(why))
  }

  // A constructor row of `built`, as the report renders it: its count, or each reason up to what
  // it names (an uncovered one with what it needs).
  private def constructed(name: String): String = count(s"built.$name.<init>") match {
    case Count.Unresolved(reasons) =>
      val prefix = s"${TastyFrontEnd.Uncovered}: "
      reasons
        .map(reason =>
          (if (reason.startsWith(prefix)) prefix else "") +
            reason.stripPrefix(prefix).takeWhile(_ != ':')
        )
        .mkString("; ")
    case counted => counted.render
  }

  // A constructor's positions are value spaces: `Pt` is 2 × 2; `Walk` is 1 × 2^32, a top type in
  // an input being one value, `AnyVal` too (`Vals`), also behind an alias (`Aliased`); `Txt` is ω; `Fun` the tier of functions over
  // strings, and a list of them saturates at ω as the method side does (`Fns`); a top in an
  // output is ω (`Out`); the type `Null` is empty on the method side (`Nul`). `Via` names a Byte
  // through a value's path: 2^8, where the old reading reports the estimate's bound, `≤2^8`. A
  // type parameter's slot is the shape model's: `Pair` builds (x, x), (x, y), (y, x) or (y, y).
  // The estimate gives an open trait a reason, so `Holds` has no number; the two algebras read a
  // function from `Nothing` as 1 and as 0, and `Anything` as 1 where the estimate stores an `Any`
  // as ω (`Holder`): refused. A position mixing a type parameter with an `Int` has no agreed
  // reading. A reason of the estimate declines a ground position only: `Rest` names its type
  // parameter, under a repeated type, and keeps the method side's ω. A recursive position takes
  // the estimate's size, as the old reading does: `Option[Rec]` is 1 + ω, `ω + 1`. A `using` parameter with a name is a position like any
  // other: `Using` holds an `Open`, as `Holds` does.
  private def constructors = {
    val expected = List(
      "Pt" -> "4",
      "Walk" -> "2^32",
      "Vals" -> "1",
      "Aliased" -> "1",
      "Pair" -> "4",
      "Txt" -> "ω",
      "Fun" -> "ε₀",
      "Fns" -> "ω",
      "Rest" -> "ω",
      "Anything" -> "1",
      "Out" -> "ω",
      "Nul" -> "0",
      "Via" -> "2^8",
      "Holds" -> "open to implementations",
      "Absurd" -> "constructor position readings disagree",
      "Holder" -> "constructor position readings disagree",
      "Mixed" -> "constructor position mixes type parameters with transparent ground values",
      "Rec" -> "ω + 1",
      "Using" -> "open to implementations"
    )
    expected.map((name, _) => name -> constructed(name)) must_== expected
  }

  // An array of a type parameter is refused by the standard model for good (building one needs a
  // `ClassTag`): a `?` of the front end's own, which the gate compares, not a row to come.
  private def refused = count("demo.Refused.arr") must beLike {
    case Count.Unresolved(List(reason)) =>
      (reason must contain("Array")).and(reason must not(startWith(TastyFrontEnd.Uncovered)))
  }

  // Every definition moved by one character: no name is where the classes say it is.
  private def stale = {
    val moved = input.copy(sources = List("Demo.scala" -> (" " + text)))
    TastyFrontEnd.rows(moved).sources must beLike {
      case List(Left(error)) => error.message must startWith("compiled classes do not match")
    }
  }

  // A source that defines nothing has no classes and no rows; a top-level definition of two names
  // is a definition row and a method row per name.
  private def sources = {
    val module = TastyFrontEndSpec
      .compiled(
        "Empty.scala" -> "package demo\nimport scala.util.Try\n",
        "Two.scala" -> "val a, b = true\n"
      )
    TastyFrontEnd.rows(module).sources must beLike {
      case List(Right(empty), Right(two)) =>
        ((empty.methods, empty.definitions) must_== (Nil, Nil))
          .and(two.definitions.map(_.name) must_== List("a", "b"))
          .and(two.methods.map(_.name) must_== List("a", "b"))
    }
  }

}

object TastyFrontEndSpec {

  extension (reason: Definition.Unbound) {

    /** What a definition row's reason is about: a member's name, or the reason as it reads. */
    def subject: String = reason match {
      case Definition.Unbound.Member(name) => name
      case other                           => other.render
    }

    /** The kind of a definition row's reason (`unknown`, `param`, `refusal`, …). */
    def tag: String = reason match {
      case _: (Definition.Unbound.Parameter | Definition.Unbound.HigherKinded) => "param"
      case _: Definition.Unbound.Member                                        => "member"
      case _: Definition.Unbound.Open                                          => "open"
      case _: Definition.Unbound.Unknown                                       => "unknown"
      case _: Definition.Unbound.Syntax                                        => "syntax"
      case _: Definition.Unbound.Refusal                                       => "refusal"
    }

  }

  /** `sources` compiled in-process against the Scala library, as a build hands a module over. */
  def compiled(sources: (String, String)*): FrontEnd.Module = {
    val root = Compiled.temporary("compiled-module")
    val classes = Files.createDirectories(root.resolve("classes"))
    val files = sources.map { (path, text) =>
      val file = root.resolve("src").resolve(path)
      Files.createDirectories(file.getParent)
      Files.writeString(file, text).toString
    }
    val libraries = sys.props
      .getOrElse("cardinality.test.classpath", sys.props("java.class.path"))
      .split(java.io.File.pathSeparator)
      .toList
      .filter { entry =>
        val name = Path.of(entry).getFileName.toString
        name.startsWith("scala-library") || name.startsWith("scala3-library")
      }
    val classpath = libraries.mkString(java.io.File.pathSeparator)
    val arguments = List("-d", classes.toString, "-classpath", classpath) ++ files
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    if (dotty.tools.dotc.Main.process(arguments.toArray, reporter).hasErrors)
      sys.error(s"fixture does not compile: ${reporter.allErrors.map(_.message).mkString("; ")}")
    val compiled = CompiledTypes.Classes(List(classes), libraries.map(Path.of(_)))
    FrontEnd.Module(sources.toList, Report.Environment(classes = Some(compiled)))
  }

}
