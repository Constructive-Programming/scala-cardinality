package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
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
    a tuple a single case projects reduces whatever it holds  $projected
    any other stuck match type is not read yet                $later
  """

  private val text =
    """package mt
      |
      |type Fst[T] = T match { case (f, s) => f }
      |type Snd[T] = T match { case (f, s) => s }
      |type Fst3[T] = T match { case (f, s, t) => f }
      |type IsInt[T] = T match { case Int => Boolean; case Any => Unit }
      |type IsStr[T] = T match { case String => Boolean; case Any => Unit }
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
      |object C {
      |  def m2(x: Boolean): Miss[(Boolean, Unit)] = ???
      |  def m6[A](m: Miss[(A, Unit)]): A = ???
      |  def proj[A](x: A, y: A): Fst[(A, Unit)] = x
      |}
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("MatchTypes.scala" -> text)
  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)

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
  private def positions =
    List("UInt.<init>", "UStr.<init>").map(count) must_== List(Count.Finite(2), Count.Finite(1))

  // `(A, Unit) match { case (f, s) => f }` is A for every A (scalac reduces a tuple of the
  // pattern's arity whatever its components): `proj` has x or y, 2.
  private def projected = count("C.proj") must_== Count.Finite(2)

  // `IsInt[A]` on an abstract A needs the compiler's disjointness of A from `Int`: not read yet.
  // Nor one tasty-query reduces on an abstract scrutinee:
  // it reads `Fst[T]` for a `T <: (Int, String)` as `Int`, where scalac keeps it stuck (an `Int`
  // result for `x` does not compile), so its 2^(2^37) would be a wrong number; likewise `IsStr[Null]`,
  // which tasty-query reads as `Boolean` (`Null` is below `String`) and scalac keeps stuck.
  private def later =
    (count("M.open").toString must contain("a type: (A match"))
      .and(count("M.bounded").toString must contain("a type: (T match"))
      .and(count("M.nul").toString must contain("a type: (scala.Null match"))

}
