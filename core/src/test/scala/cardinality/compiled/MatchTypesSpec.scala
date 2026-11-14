package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.compiled.TastyFrontEndSpec.subject
import org.specs2.Specification

/** Match types in the compiled front end: what the compiler reduces one to (at the arguments a
  * class was named with, for its own parameters), or a stuck projection of an unbounded type
  * parameter (`StuckProjections`, members MT3). Every expected count is derived in its example's
  * comment.
  */
class MatchTypesSpec extends Specification {

  def is = s2"""
  A match type is read as the compiler reduces it
    a reduced match type is its result                        $reduced
    a stuck projection of a type parameter is an atom         $stuck
    projections no single instantiation reduces are refused   $refused
    a class's parameter is the argument the class is named    $argument
    in a constructor's position, at the alias's arguments     $positions
    a stored value of the class, at the arguments it is named $storedValues
    a tuple a single case projects reduces whatever it holds  $projected
    any other stuck match type is not read yet, nor an uninhabited one $later
  """

  private val text =
    """package mt
      |
      |type Fst[T] = T match { case (f, s) => f }
      |type Snd[T] = T match { case (f, s) => s }
      |type Fst3[T] = T match { case (f, s, t) => f }
      |type IsInt[T] = T match { case Int => Boolean; case Any => Unit }
      |type IsStr[T] = T match { case String => Boolean; case Any => Unit }
      |type K[T] = T match { case _ => Option[T] }
      |object M {
      |  def pair(x: Fst[(Boolean, Int)]): Boolean = x
      |  def unit(x: IsInt[String]): IsInt[String] = x
      |  def flag(x: IsInt[Int]): IsInt[Int] = x
      |  def keep[A](x: Fst[A]): Fst[A] = x
      |  def two[A](x: Fst[A], y: Fst[A], z: Snd[A]): Fst[A] = x
      |  def none[A](x: Snd[A]): Fst[A] = ???
      |  def mixed[A](x: Fst[A], y: Fst3[A]): Fst[A] = x
      |  def open[A](x: IsInt[A]): IsInt[A] = x
      |  def bounded[T <: (Int, String)](x: Fst[T]): Fst[T] = x
      |  def nul(x: IsStr[Null]): IsStr[Null] = x
      |}
      |final class P[A](val x: Fst[A], val y: Fst3[A])
      |final case class Miss[A](fst: Fst[A])
      |final class UInt(h: IsInt[Int])
      |final class UStr(h: IsInt[String])
      |sealed trait Af[A, +B]
      |object Af {
      |  final case class Miss[A](fst: Fst[A]) extends Af[A, Nothing]
      |  final case class Hit[A, +B](snd: Snd[A], b: B) extends Af[A, B]
      |}
      |final case class UsesAf(a: Af[(Boolean, Boolean), Nothing])
      |final case class Pin[X](a: Af[(X, Boolean), Unit])
      |final case class UsePin(p: Pin[Unit])
      |val miss: Af.Miss[(Unit, Boolean)] = Af.Miss(())
      |type G[T] = T match { case Int => R[Int]; case Any => Unit }
      |final case class R[A](g: Option[G[A]])
      |final case class UR(r: R[Int])
      |object C {
      |  def m2(x: Boolean): Miss[(Boolean, Unit)] = ???
      |  def m6[A](m: Miss[(A, Unit)]): A = ???
      |  def proj[A](x: A, y: A): Fst[(A, Unit)] = x
      |  def nested[A, B, C](x: A, y: A): ((A, B), C) match { case ((f, s), t) => f } = x
      |  def wild[A](x: A, y: A): K[A] = None
      |  def twice[A, B, C](x: A, y: A): Fst[Fst[((A, B), C)]] = x
      |  def firstOf[A](x: IsInt[(A, A)]): IsInt[(A, A)] = x
      |  def bound[A](x: Fst[A] match { case Any => Unit }): Boolean = true
      |}
      |trait H { type X }
      |object E {
      |  def t1(b: Boolean): Snd[(Nothing, Boolean)] = ???
      |  def t2[A <: Nothing](b: Boolean): Snd[(A, Boolean)] = ???
      |  def t4(b: Boolean): ((Nothing, Int), Boolean) match { case ((f, s), t) => t } = ???
      |  def t5(p: H { type X <: Nothing }, b: Boolean): Snd[(p.X, Boolean)] = ???
      |  def t6(x: Snd[(Nothing, Boolean)], y: Boolean): Boolean = y
      |}
      |final case class Sd(x: Snd[(Nothing, Boolean)])
      |val missed: Miss[(Unit, Boolean)] = Miss(())
      |val flagged: Miss[(Boolean, Unit)] = Miss(true)
      |final case class HoldsFlag(m: Miss[(Boolean, Unit)])
      |final case class HoldsUnit(m: Miss[(Unit, Boolean)])
      |final class Pick[T, U](val x: T match { case Int => U; case Any => Unit })
      |final case class HoldsPick(p: Pick[Int, Boolean])
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("MatchTypes.scala" -> text)
  private lazy val rows = TastyFrontEnd.rows(input)

  // A definition row as the report shows it: its size, or its first reason.
  private def stored(name: String): String = {
    val found = rows.sources
      .flatMap(_.toOption.toList.flatMap(_.definitions))
      .find(_.name == s"mt.$name")
      .get
    found.unbound.headOption.fold(found.size.fold("—")(_.render))(_.subject)
  }

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name == s"mt.$name")
      .get
      .count

  // `Fst[(Boolean, Int)]` is `Boolean`: the identity's argument, 2 results of 2 inputs, 4 functions
  // (`pair` is one of them). `IsInt[String]` is `Unit`, since `String` is provably not an `Int`:
  // a terminal result, 1. `IsInt[Int]` is `Boolean`: `Boolean => Boolean`, 4.
  private def reduced =
    List("pair", "unit", "flag").map(n => count(s"M.$n")) must_== List(
      Count.Finite(4),
      Count.Finite(1),
      Count.Finite(4)
    )

  // `Fst[A]` on an abstract A is stuck: a value of it comes only from one of the same type. `keep`
  // has its x, 1; `two` has x or y (z is the other component), 2; `none` has no `Fst[A]`, 0.
  private def stuck =
    List("keep", "two", "none").map(n => count(s"M.$n")) must_== List(
      Count.Finite(1),
      Count.Finite(2),
      Count.Finite(0)
    )

  // `Fst[A]` beside the three-way `Fst3[A]`: no instantiation of A reduces both, so the model has
  // no reading of the pair (the old reason); likewise `P`'s two positions over its parameter.
  private def refused =
    List("M.mixed", "P.<init>")
      .map(count(_).toString)
      .must(contain(contain("have no common reducing instantiation")).forall)

  // A class's field over its parameter is read at the argument the class is named with:
  // `Miss[(Boolean, Unit)]` holds a `Fst[(Boolean, Unit)]`, a `Boolean`, so `m2` is
  // `Boolean => Boolean`, 4; `Miss[(A, Unit)]` holds an A, so `m6` has `m.fst`, 1. In a
  // constructor's position: `Fst[A]` is the stuck projection of the class's parameter, one slot the
  // caller fills, 1.
  private def argument =
    List("C.m2", "C.m6", "Miss.<init>").map(count) must_== List(
      Count.Finite(4),
      Count.Finite(1),
      Count.Finite(1)
    )

  // A constructor's position over a match type is read at the alias's arguments by the stored
  // estimate too, which agrees: `IsInt[Int]` is `Boolean`, 2; `IsInt[String]` is `Unit`, 1.
  // A family's case over the family's argument too: `Af[(Boolean, Boolean), Nothing]` is a `Miss`
  // of a `Fst`, a `Boolean` (a `Hit` holds a `Nothing`), 2; `Pin[Unit]`'s
  // `Af[(Unit, Boolean), Unit]` is a `Miss` of a `Unit` or a `Hit` of a `Boolean` and a `Unit`,
  // 1 + 2 = 3; `miss` holds a `Miss[(Unit, Boolean)]`, 1. Both sides read them. `R[Int]` reaches
  // itself through `G[Int]`: a system, whose classes are keyed by their arguments' sizes, so the
  // match over `A` is not read there (refused, not a deeper recursion).
  private def positions =
    (List("UInt.<init>", "UStr.<init>", "UsesAf.<init>", "UsePin.<init>").map(count) must_== List(
      Count.Finite(2),
      Count.Finite(1),
      Count.Finite(2),
      Count.Finite(3)
    )).and(List("UsesAf", "UsePin", "miss").map(stored) must_== List("2", "3", "1"))
      .and(stored("UR") must contain("a type: (A match"))

  // The stored estimate reads a class's field over its parameter at the argument the type names
  // too: `Miss[(Unit, Boolean)]` holds a `Unit`, one value, and `Miss[(Boolean, Unit)]` a
  // `Boolean`. A top-level value row is min(1, |T|) (register D30): both types have a value, so
  // both are 1 (read, where they were refused). A class stores the size itself: `HoldsUnit` 1,
  // `HoldsFlag` 2. `Pick[Int, Boolean]`'s field reduces to the class's other parameter `U`, read
  // as the position bound it: `Boolean`, so `HoldsPick` is 2.
  private def storedValues =
    List("missed", "flagged", "HoldsUnit", "HoldsFlag", "HoldsPick").map(stored) must_==
      List("1", "1", "1", "2", "2")

  // `(A, Unit) match { case (f, s) => f }` is A for every A (scalac reduces a tuple of the
  // pattern's arity whatever its components): `proj` has x or y, 2. So for a first case whose
  // pattern matches whatever the parts are: a nested tuple of binders (`nested`, A: x or y, 2), a
  // match scalac reduces as the scrutinee (`twice`: `Fst[((A, B), C)]` is `(A, B)`, so A, 2), or
  // `_` (`K[A]` is `Option[A]`: `None`, `Some(x)`, `Some(y)`, 3; a stuck `Fst[A]` matched by `Any`
  // is `Unit`, so `bound` is a Boolean, 2). `IsInt[(A, A)]` tests `Int` first: stuck, not read.
  private def projected =
    (List("C.proj", "C.nested", "C.twice", "C.wild", "C.bound").map(count) must_== List(
      Count.Finite(2),
      Count.Finite(2),
      Count.Finite(2),
      Count.Finite(3),
      Count.Finite(2)
    )).and(count("C.firstOf").toString must contain("a type: (Tuple2[A, A] match"))

  // `IsInt[A]` on an abstract A needs the compiler's disjointness of A from `Int`: not read yet.
  // Nor one tasty-query reduces on an abstract scrutinee:
  // it reads `Fst[T]` for a `T <: (Int, String)` as `Int`, where scalac keeps it stuck (an `Int`
  // result for `x` does not compile), so its 2^(2^37) would be a wrong number; likewise `IsStr[Null]`,
  // which tasty-query reads as `Boolean` (`Null` is below `String`) and scalac keeps stuck.
  // Nor one whose scrutinee scalac finds uninhabited (a part below `Nothing`), where it leaves the
  // match stuck: no body returns, and no argument passes, a value of it (`Sd(true)` does not
  // compile), so tasty-query's `Boolean` would give 4, 16 and 2 that no program has.
  private def later =
    List("E.t1", "E.t2", "E.t4", "E.t5", "E.t6", "Sd.<init>")
      .map(row => count(row).toString must contain("a type: ("))
      .reduce(_ and _)
      .and(stored("Sd") must contain("a type: ("))
      .and(count("M.open").toString must contain("a type: (A match"))
      .and(count("M.bounded").toString must contain("a type: (T match"))
      .and(count("M.nul").toString must contain("a type: (Null match"))

}
