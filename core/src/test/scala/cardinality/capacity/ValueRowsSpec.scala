package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// What a top-level value holds (docs/research/pure-code.md §2, register D30): `min(1, |T|)` for its
// declared type, read as inhabitation — the absurd function is a value, an intersection is an upper
// bound, and a class has a value only if one is built. Each number is derived beside it; fixtures
// checked with scalac 3.9.0 (the integrated review's probes, z1b/fx and z1bfix2/fx).
class ValueRowsSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    A value row holds one value when its declared type has one
      an empty declared type holds no value                           $emptyValues
      an inhabited, undeclared or unresolved type                     $valueReasons
      a witness decides it whatever the rest of the type              $valueWitnesses
      a class, enum or module no construction is read for             $unbuiltValues
      a sum drops the summands that provably build nothing            $sumDropsEmptySummand
      a singleton type holds what its path denotes                    $singletonValues
    """

  // `Nothing` has no value, so `x`, `v` (a `var` is its value) and `z` hold none; nor does
  // `Either[Nothing, Nothing]` (0 + 0), an alias of `Nothing`, or `Int => Nothing` (an `Int` exists,
  // and no result for it). The initializer (a throw, ignored) is never read. The object row is its
  // one value.
  def emptyValues = {
    val found = rows(
      """|package vals
         |type X = Nothing
         |val x: Nothing = ???
         |var v: Nothing = ???
         |lazy val z: Nothing = ???
         |val e: Either[Nothing, Nothing] = ???
         |val x0: X = ???
         |val g: Int => Nothing = ???
         |object O { val w: Nothing = ??? }
         |""".stripMargin
    )
    (found(("vals.x", Value)) === "0")
      .and(found(("vals.v", Value)) === "0")
      .and(found(("vals.z", Value)) === "0")
      .and(found(("vals.e", Value)) === "0")
      .and(found(("vals.x0", Value)) === "0")
      .and(found(("vals.g", Value)) === "0")
      .and(found(("vals.O", Object)) === "1")
  }

  // `Int`, `Unit` and `Either[Nothing, Boolean]` (0 + 2) are inhabited: 1. A value with no
  // declared type holds one. A function from `Nothing` has one value, the absurd function — the
  // algebra counts it 0 since it never runs (docs/type-arithmetic.md §3), which is no emptiness:
  // 1, as the method side counts the same `val`. A class holding one may be inhabited the same
  // way, which the algebra's 0 does not decide: refused. The algebra's intersection is an upper
  // bound (`Int & String` is empty, its bound 2^32): refused. An unresolved declared type may be
  // empty: its reason, not 1 — `Foo => Nothing` too, inhabited exactly when `Foo` is empty. Read
  // from another package, through a wildcard import or a path, the class and the alias are refused
  // alike: the reading that meets them is the value row's own.
  def valueReasons = {
    val files = List(
      "vr/a.scala" ->
        """|package vr
           |val y: Int = 3
           |val u: Unit = ()
           |val ok: Either[Nothing, Boolean] = Right(true)
           |val n = 3
           |val f: Nothing => Int = (n: Nothing) => 1
           |final case class P(f: Nothing => Int)
           |val p: P = P((n: Nothing) => 1)
           |val q: Foo = ???
           |val h: Foo => Nothing = ???
           |val vint: Int & String = ???
           |type I = Int & String
           |""".stripMargin,
      "vb/b.scala" ->
        """|package vb
           |import vr.*
           |val p: P = ???
           |val i: I = ???
           |val p2: vr.P = ???
           |""".stripMargin
    )
    val found = rowsOf(files*)
    (found(("vr.y", Value)) === "1")
      .and(found(("vr.u", Value)) === "1")
      .and(found(("vr.ok", Value)) === "1")
      .and(found(("vr.n", Value)) === "1")
      .and(found(("vr.f", Value)) === "1")
      .and(constructor("vr.f", files*) === Count.Finite(1))
      .and(
        found(
          ("vr.p", Value)
        ) === "unresolved[value not decided: P holds a function from an empty type]"
      )
      .and(found(("vr.q", Value)) === "unresolved[Foo]")
      .and(found(("vr.h", Value)) === "unresolved[Foo]")
      .and(
        found(("vr.vint", Value)) ===
          "unresolved[value not decided: Int & String holds an intersection or refinement]"
      )
      .and(
        found(("vb.p", Value)) ===
          "unresolved[value not decided: P holds a function from an empty type]"
      )
      .and(
        found(("vb.i", Value)) ===
          "unresolved[value not decided: I holds an intersection or refinement]"
      )
      .and(
        found(("vb.p2", Value)) ===
          "unresolved[value not decided: vr.P holds a function from an empty type]"
      )
  }

  // A witness decides a value whatever the rest of the type: the top and marker types, `BigInt`
  // and the comparisons, the throwables (`new Exception`) and the containers with an empty one are
  // inhabited (they read `?` with an unresolved or open name: `AnyRef`, `Throwable`, `Json` in
  // `Option[Json]`); so is a sealed family with a case object a member of every instantiation
  // (`V[Json]` holds `Empty` by covariance, as eo's `PSVec[Json]` holds `PSVec.Empty`), and an
  // enum with a parameterless case (`E.None`). A source that binds the name takes it from the
  // standard library: `Product` here is the source's empty enum, 0, and a wildcard over a namespace
  // no source supplies may bind `Option` to anything: refused, not the library's witness. A union
  // with an inhabited branch is inhabited.
  def valueWitnesses = {
    val found = rows(
      """|package vw
         |sealed trait V[+B]
         |case object Empty extends V[Nothing]
         |final case class One[B](b: B) extends V[B]
         |enum E[+A] { case None; case Some(a: A) }
         |val a1: AnyRef = ""
         |val a2: Matchable = 1
         |val a3: java.io.Serializable = ""
         |val a4: Comparable[String] = ""
         |val a5: BigInt = BigInt(1)
         |val a6: Ordering[Json] = ???
         |val a7: Throwable = new Exception
         |val a8: Option[Json] = None
         |val a9: List[Json] = Nil
         |val w: V[Json] = Empty
         |val e: E[Json] = E.None
         |val t: (Nothing => Int, Boolean) = ((n: Nothing) => 1, true)
         |val un: Json | Int = 1
         |enum Product { case P(n: Nothing) }
         |val pr: Product = ???
         |""".stripMargin
    )
    val doubted = rows("package vz\nimport zz.*\nval o: Option[Int] = ???")
    List("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "w", "e", "t", "un")
      .map(name => (found((s"vw.$name", Value)) === "1").updateMessage(s"vw.$name: " + _))
      .reduce(_.and(_))
      .and(found(("vw.pr", Value)) === "0")
      .and(doubted(("vz.o", Value)) === "unresolved[Option (may be imported by zz.*)]")
  }

  // A value of a class exists only if one is built, which runs every ancestor's constructor
  // (registers D26, D30; CROSSCHECK X6): `K` can only be built through `Base(???)`, and a throw
  // supplies nothing, so no `K` exists, whatever `K` stores (1). The same holds for `V`'s only case
  // object (`V(???)`), for `En`'s case (`new En` runs `Base(???)`), for `K2` (trait argument `???`),
  // for `Deep` (its parent `Mid` throws), `W` (a strict field throws), `Dd` (its parent's default
  // throws) and `C2` (its companion, which builds the case, throws). `P`'s constructor is private
  // and no caller is read; `Q`'s parent takes a context argument implicit search would supply; `Z`
  // is built on a parent no source supplies. Each is refused — never 1, and never 0 for `K =>
  // Nothing`, whose value is the absurd function if `K` is empty. Built from literals or its own
  // parameters (`Ok`, `Lit1`, `E2`, `Fine`), or on a parent the stdlib model knows
  // (`RuntimeException`), a class has a value: 1, and `Ok => Nothing` none, 0.
  def unbuiltValues = {
    val found = rows(
      """|package ub
         |abstract class Base(n: Nothing)
         |final case class K() extends Base(???)
         |sealed abstract class V(n: Nothing)
         |case object E extends V(???)
         |enum En extends Base(???) { case X }
         |sealed trait Tp(n: Nothing)
         |final case class K2() extends Tp(???)
         |final class P private (i: Int)
         |abstract class Mid extends Base2(throw new Exception)
         |abstract class Base2(n: Int)
         |final case class Deep() extends Mid
         |final class W { val x: Int = ??? }
         |abstract class D(n: Int = ???)
         |final case class Dd() extends D
         |trait Ctx(using o: Ordering[Int])
         |final case class Q() extends Ctx
         |final case class Z() extends zz.Base
         |enum C2 { case A }
         |object C2 { val boom: Int = ??? }
         |final case class Ok(i: Int) extends Base2(i)
         |final case class Lit1() extends Base2(1)
         |sealed abstract class V2(n: Int)
         |case object E2 extends V2(1)
         |final case class R() extends RuntimeException
         |enum Fine extends Base2(2) { case Y }
         |val k: K = ???
         |val fk: K => Nothing = ???
         |val v: V = ???
         |val fv: V => Nothing = ???
         |val en: En = ???
         |val fen: En => Nothing = ???
         |val k2: Tp = ???
         |val p: P = ???
         |val deep: Deep = ???
         |val w: W = ???
         |val dd: Dd = ???
         |val q: Q = ???
         |val z: Z = ???
         |val c2: C2 = ???
         |val ok: Ok = ???
         |val fok: Ok => Nothing = ???
         |val lit: Lit1 = ???
         |val v2: V2 = ???
         |val r: R = ???
         |val fine: Fine = ???
         |""".stripMargin
    )
    def refused(name: String, holds: String) =
      (found((s"ub.$name", Value)) === s"unresolved[value not decided: $holds]")
        .updateMessage(s"ub.$name: " + _)
    // A construction that evaluates a throw builds nothing (D-pure): the type holds no value and
    // the row reads 0, whatever it stores; a function from it holds the absurd value, 1. A private
    // constructor, a context argument or an unread parent may hide a value built where this
    // reading does not look, so those rows stay undecided instead.
    List("k", "v", "en", "k2", "deep", "w", "dd", "c2")
      .map(name => (found((s"ub.$name", Value)) === "0").updateMessage(s"ub.$name: " + _))
      .reduce(_.and(_))
      .and(found(("ub.fk", Value)) === "1")
      .and(found(("ub.fv", Value)) === "1")
      .and(found(("ub.fen", Value)) === "1")
      .and(refused("p", "P holds P, whose constructor is private"))
      .and(refused("q", "Q holds Q, whose parent Ctx takes context arguments not written"))
      .and(refused("z", "Z holds Z, built on unsupplied parent zz.Base"))
      .and(
        List("ok", "lit", "v2", "r", "fine")
          .map(name => (found((s"ub.$name", Value)) === "1").updateMessage(s"ub.$name: " + _))
          .reduce(_.and(_))
      )
      .and(found(("ub.fok", Value)) === "0")
      .and(found(("ub.K", Class)) === "1")
  }

  // A sum drops the summands that provably build nothing: S1's parent's constructor throws, so a
  // value of S is S2's alone - 1, not the refusal any unread summand gave (follow-up 25).
  def sumDropsEmptySummand = {
    val found = rows(
      """|package uf
         |abstract class Base(n: Int)
         |sealed trait S
         |case class S1(i: Int) extends Base(???)
         |case object S2 extends S
         |val s: S = ???
         |""".stripMargin
    )
    found(("uf.s", Value)) === "1"
  }

  // `n.type` holds the value `n` denotes: `n: Nothing` holds none, so neither does `sn` — not the 1
  // a singleton's capacity is. A value's declared type is read where the value is declared, which
  // the path does not carry: refused. An object with no parents whose body evaluates no throw is
  // built: `O.type` 1. One with parents, or whose body throws, is refused, as is a path the sources
  // do not read (`None`), and a class that holds a singleton type (`Wo(o: O.type)`).
  def singletonValues = {
    val found = rows(
      """|package sg
         |abstract class Base(n: Int)
         |object O
         |object OB extends Base(1)
         |object T { val x: Int = ??? }
         |final case class Wo(o: O.type)
         |val n: Nothing = ???
         |val sn: n.type = n
         |val so: O.type = O
         |val sob: OB.type = OB
         |val st: T.type = T
         |val nn: None.type = None
         |val wo: Wo = Wo(O)
         |""".stripMargin
    )
    def refused(name: String, why: String) =
      (found((s"sg.$name", Value)) === s"unresolved[value not decided: $why]")
        .updateMessage(s"sg.$name: " + _)
    (found(("sg.n", Value)) === "0")
      .and(found(("sg.so", Value)) === "1")
      .and(refused("sn", "n.type, a value's singleton type"))
      .and(refused("sob", "OB.type, whose object's parents are not read"))
      .and(refused("st", "T.type, whose object's body throws"))
      .and(refused("nn", "None.type, whose path is not read"))
      .and(refused("wo", "Wo holds the singleton type O.type"))
  }

}
