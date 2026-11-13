package cardinality.compiled

import org.specs2.Specification

/** Definitions that reach themselves in the compiled front end: a system of equations solved by the
  * fixed point the old estimate solves its own with (`Fixpoint`; docs/type-arithmetic.md §8). Every
  * expected size is derived in its example's comment.
  */
class StoredRecursionSpec extends Specification {

  def is = s2"""
  A definition that reaches itself is a solved system
    the least fixed point, widened to its tier                $least
    the coinductive completion of a lazy cycle                $lazyCycles
    a constructor position takes the estimate's size          $positions
    a recursion with no count is refused                      $refusals
    a class read at other arguments is another class          $instantiations
    a value of a class holds one where one is built           $values
  """

  private val text =
    """package rec
      |
      |final case class Loop(l: Loop)
      |sealed trait Nat
      |case object Z extends Nat
      |final case class S(n: Nat) extends Nat
      |final case class Use(n: Nat, b: Boolean)
      |enum Expr { case Lit(v: Boolean); case Add(l: Expr, r: Expr) }
      |final case class Str(s: String, next: Option[Str])
      |final class Lz(l: => Lz)
      |final class Bits(val h: Boolean, t: => Bits)
      |final case class Neg(f: Neg => Boolean)
      |final class GS[A](val h: A, t: => GS[A])
      |sealed trait Two[A]
      |final case class T1[A]() extends Two[A]
      |final case class T2[A]() extends Two[A]
      |final case class X[A](n: Option[X[A] & Two[A]])
      |final case class C[A](a: A, n: Option[C[Int]])
      |final case class R1(x: C[Int], y: C[Boolean])
      |final case class R2(y: C[Boolean], x: C[Int])
      |final case class N[A](a: A, n: Option[N[(A, A)]])
      |final case class UN(n: N[Boolean])
      |final case class Fn(f: Boolean => Fn)
      |final case class Rose(kid: Option[Rose])
      |final case class Box[A](a: A)
      |final case class Ta(b: Option[Box[Ta]])
      |final case class W[A](a: A, w: Option[W[A]])
      |final case class Wt(w: Option[W[Wt]])
      |final class Lbox[A](a: => A)
      |final class Lb(b: Lbox[Lb])
      |final case class LzB(b: Box[() => LzB])
      |final case class Nb(b: Box[Boolean => Nb])
      |final case class Lc(b: Box[Boolean ?=> Lc])
      |final case class AbsR(f: Nothing => AbsR)
      |final case class HoldsAbs(r: AbsR)
      |final class Thr(x: Boolean) { val z: Int = ??? }
      |final class Pat(x: Boolean) { val (a, b) = (x, x) }
      |val nat: Nat = Z
      |val thr: Thr = ???
      |val pat: Pat = ???
      |val lz: Lz = ???
      |abstract class Base { val z: Int = ??? }
      |final class Kid(val x: Boolean) extends Base
      |class Dp(x: Int = ???)
      |final class Dk extends Dp
      |final case class Rt(n: Option[Rt]) { val z: Int = ??? }
      |enum Ep(x: Int) { case A extends Ep(1) }
      |val kid: Kid = ???
      |val dflt: Dk = ???
      |val rt: Rt = ???
      |val ep: Ep = ???
      |""".stripMargin

  private lazy val rows =
    Differential.answer(TastyFrontEnd, TastyFrontEndSpec.compiled("Rec.scala" -> text).next)

  // A definition row as the report shows it: its size, or its first reason up to what it names.
  private def definition(name: String): String = {
    val found = rows.sources
      .flatMap(_.toOption.toList.flatMap(_.definitions))
      .find(_.name == s"rec.$name")
      .get
    found.unbound.headOption.fold(found.size.fold("—")(_.render)) { reason =>
      reason.subject.stripPrefix(s"${TastyFrontEnd.Uncovered}: ").takeWhile(_ != ':')
    }
  }

  private def constructor(name: String): String =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name == s"rec.$name.<init>")
      .get
      .count
      .render

  // `Loop` needs a `Loop`: L = L from 0 stays 0. `Nat` is N = 1 + N: 1, then 2, a cycle that
  // keeps growing widens to its tier, ω, and so does each subtype that holds one (`S`); `Use` is
  // ω × 2, ω in the size algebra. `Expr` is E = 2 + E × E: ω. `Ta` reads `Box` at itself, in place
  // (its argument holds the system's value): T = 1 + T, ω.
  // `Str` is S = String × (1 + S), a
  // list of strings in effect, the ε₀ tier. `AbsR` is A = A^0, the algebra's 0 for a function
  // from `Nothing` (the stored side's convention), whatever A is.
  private def least =
    List("Loop", "Nat", "S", "Use", "Expr", "Ta", "Str", "AbsR").map(definition) must_==
      List("0", "ω", "ω", "ω", "ω", "ω", "ε₀", "0")

  // A by-name field does not demand its value: `Lz` has no finite value (μ is 0) and the one lazy
  // knot `lazy val l: Lz = Lz(l)`, νX.X = 1. `Bits` is an endless Boolean stream: the per-lap label
  // space 2 raised to ω, ω. `Fn` has no finite value (F = F^2 from 0 stays 0), and its function
  // field branches on two inputs: the completion saturates at ω. `Rose` is R = 1 + R, ω, and its
  // strict `Option` branches too: ω.
  private def lazyCycles =
    List("Lz", "Bits", "Fn", "Rose").map(definition) must_== List("1", "ω", "ω", "ω")

  // The method side refuses a recursive position; the row takes the estimate's size: `Loop`'s
  // position is 0, `Lz`'s 1, `Str`'s String × Option[Str] = ω × (1 + ε₀), ε₀. `AbsR` reads 0, the
  // algebra's value of a function from `Nothing`, which does not say that no value exists (the
  // absurd function is one): `HoldsAbs` keeps the method side's refusal.
  private def positions =
    List("Loop", "Lz", "Str", "HoldsAbs").map(constructor) must_== List("0", "1", "ε₀", "?")

  // `Neg` reaches itself through a function's domain, where the numeric equation is no count
  // (Cantor). `GS[A]` continues through `=> GS[A]`, a class named with arguments, which the
  // completion does not follow: its μ value counts the finite values only. `X` is
  // X = 1 + min(X, 2): its rounds grow 1, 2, 3 while `Two` settles, so it is widened to ω, which its
  // equation does not reach again (1 + min(ω, 2) = 3). `N` and `UN` read `N` at Boolean, then at
  // pairs of them, 4 values, then 16, ...: no end. `Wt` reads `W` at itself, and `W` reads itself
  // at that argument again; `Lb` reads `Lbox` at itself, whose by-name field the ν completion would
  // not see read in place (Lb is the `Lz` knot, 1); `LzB`, `Nb`, `Lc` read `Box` at a function
  // type of themselves, whose knot (`LzB` is `Lz`, 1; `Nb` is ω) the ν completion would not see in
  // place either (it read 0): each a class a system reads through a type argument, refused.
  private def refusals =
    List("Neg", "GS", "X", "N", "UN", "Wt", "Lb", "LzB", "Nb", "Lc").map(definition) must_== List(
      "recursion through a function's domain",
      "lazy cycle not completed",
      "a recursion whose widening is not settled",
      "a class a system reads with ever new type arguments",
      "a class a system reads with ever new type arguments",
      "a class a system reads through a type argument",
      "a class a system reads through a type argument",
      "a class a system reads through a type argument",
      "a class a system reads through a type argument",
      "a class a system reads through a type argument"
    )

  // A class read with two argument lists, one below the other, is two classes of the system:
  // `C[Int]` is 2^32 × (1 + C[Int]), widened to ω; `C[Boolean]` is 2 × (1 + ω); `R1` and `R2` hold
  // one of each, whichever comes first: countably many, ω. `C[A]` holds an A beside an
  // `Option[C[Int]]`: no one size, the reason its parameter `A`.
  private def instantiations =
    List("R1", "R2", "C").map(definition) must_== List("ω", "ω", "A")

  // A top-level value holds one value where its type has one and an instance is built: `nat` (Nat
  // is ω), `lz` (1, the lazy knot). `Thr` stores 2 but its construction always throws, so whether
  // `thr` holds a value is undecided (the old `Builds`); so for `kid`, whose parent `Base` throws,
  // `dflt`, whose parent's default throws, and `rt`, a recursive class that throws. `Pat` binds a
  // pattern, an ordinary binding: it is built, 1. `Ep`'s value case passes an argument not read here.
  private def values =
    List("nat", "lz", "thr", "kid", "dflt", "rt", "pat", "ep").map(definition) must_== List(
      "1",
      "1",
      "value not decided",
      "value not decided",
      "value not decided",
      "value not decided",
      "1",
      "a value of an enum with parameters and a value case"
    )

}
