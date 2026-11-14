package cardinality.compiled

import cardinality.compiled.TastyFrontEndSpec.subject
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
    a function's result inside a field defers the cycle      $nestedLazyCycles
    a name held strictly and lazily in one field is refused  $uncompleted
    an empty arm or factor holds no edge                      $emptyParts
    a sum's arm or a field outside its cycle at its ν value   $lazyArms
    a parent held in a cycle holds its children outside it    $heldParents
    an arm's holes decide what a lap holds                    $holes
    a constructor position takes the estimate's size          $positions
    a recursive position over hidden parameters, by rule B   $hiddenPositions
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
      |final case class Ltd(p: (Boolean, () => Ltd))
      |final case class Lu(p: (Unit, () => Lu))
      |final case class Lw(p: (() => Lw, () => Lw))
      |final case class Lo(b: Boolean, p: Option[() => Lo])
      |final case class Lby(t: () => (Boolean, Lby))
      |final case class Lcf(p: (Boolean, Boolean ?=> Lcf))
      |final case class Lsd(p: (Boolean, String => Lsd))
      |final case class Lst(p: List[() => Lst])
      |final case class Out(p: In)
      |final case class In(b: Boolean, f: () => Out)
      |sealed trait Tp
      |final case class Ta1(t: () => Tp) extends Tp
      |final case class Tb1(b: Boolean, a: Ta1) extends Tp
      |sealed trait Pt
      |final class Pa(t: => Pt) extends Pt
      |final class Pb(h: Boolean, t: => Pb) extends Pt
      |sealed trait Qt
      |final class Qa(t: => Qt) extends Qt
      |final class Qb(h: Boolean, t: => Qt) extends Qt
      |sealed trait Ut
      |sealed trait Ui extends Ut
      |final class Ua(t: => Ut) extends Ut
      |final class Ub(h: Boolean, t: => Ub) extends Ui
      |sealed trait Mt
      |final class Ma(t: => Mt) extends Mt
      |final class Mb(h: Boolean, t: => Mb) extends Mt
      |final case class Mc(b: Boolean) extends Mt
      |final class Ra(t: => Ra, c: Rc)
      |final case class Rc(b: Rb)
      |final class Rb(h: Boolean, t: => Rb)
      |sealed trait Bq
      |final class Bq1(h: Boolean, t: => Bq1) extends Bq
      |final class Bq2(t: => Bq2, b: Bq) extends Bq
      |sealed trait Bo
      |final case class Bo1(s: String) extends Bo
      |final class Bo2(t: => Bo2, b: Bo) extends Bo
      |final class Rx(t: => Rx, c: Rp)
      |sealed trait Rp
      |final case class Rp1(s: String) extends Rp
      |final case class Rp2(r: Rx) extends Rp
      |sealed trait Np
      |sealed trait Nq extends Np
      |final case class Nqo(s: String) extends Nq
      |final class Nc(t: => Nc, p: Np) extends Nq
      |sealed trait Op
      |final case class Op1(s: String) extends Op
      |final case class Op2(h: Oh) extends Op
      |final class Oh(t: => Oh, o: Option[Op])
      |sealed trait Up
      |final case class Up1(s: String) extends Up
      |final case class Up2(h: Uh) extends Up
      |final class Uh(t: => Uh, u: => Up)
      |sealed trait Fp
      |final case class Fp1(s: String) extends Fp
      |final case class Fp2(h: Fh) extends Fp
      |final class Fh(t: => Fh, f: Boolean => Fp)
      |enum En { case A(s: String); case B(t: () => En, e: En) }
      |enum Em { case A; case B(t: () => Em, e: Em) }
      |sealed trait Cq
      |final case class Cq1(s: String) extends Cq
      |final case class Sh(c: Cq) extends Cq
      |final class Lh(t: => Cq) extends Cq
      |final class Tt(l: => Tt, r: => Tt)
      |final class Nf(f: Int => Nf)
      |final class Rbt(b: Boolean, l: => Rbt, r: => Rbt)
      |sealed trait Bqi
      |final class Bqs(h: Boolean, t: => Bqs) extends Bqi
      |final class Bi(t: => Bi, x: Int, b: Bqi) extends Bqi
      |sealed trait Gp
      |final case class Go(s: String) extends Gp
      |final class Gl(t: => Gr) extends Gp
      |final case class Gr(x: Int, p: Gp)
      |final case class Nd(v: Int, next: Option[() => Nd])
      |final case class Ck(p: (Int, () => Ck))
      |sealed trait Ci
      |final case class Ci1(i: Int) extends Ci
      |final case class Si(n: Int, c: Ci) extends Ci
      |final class Li(t: => Ci) extends Ci
      |final class Eb(t: => Eb, e: Either[Long => Boolean, Eb])
      |final class Sl(t: => Sl, e: Either[Set[Long], Sl])
      |final case class Ed(e: Either[Long => Boolean, (() => Ed, () => Ed)])
      |final class Ek7(t: => Ek7, e: Either[Int, (Boolean, Option[Ek7])])
      |final class Ek8(t: => Ek8, e: Either[Int, Option[Ek8]])
      |final class Sa(t: => Sb)
      |final case class Sb(s: String, x: Set[Sa])
      |final case class Kx(kids: List[Kx], f: () => Kx)
      |final case class Ks(p: (Ks, () => Ks))
      |final case class Za(h: Boolean, f: () => Za, g: () => Zb)
      |enum Zb { case Leaf; case Never(n: Nothing, a: Za) }
      |final case class Ia(h: Boolean, g: () => Ia, o: Option[(Nothing, Ib)])
      |final case class Ib(s: String, a: () => Ia)
      |final case class Fd(p: (Boolean, (Int => Boolean) => Fd))
      |final class K9(k: K9, m: => K9)
      |final case class AbsR(f: Nothing => AbsR)
      |final class Ha(h: Boolean, b: => Hb)
      |final class Hb(x: Int, a: => Ha)
      |sealed trait Hz
      |final class Hn(h: Boolean, next: => Hz) extends Hz
      |sealed trait He
      |final class Ho(e: => He) extends He
      |final case class Ht(i: Int => He) extends He
      |final class Hw(v: Hv)
      |final class Hm(h: Boolean, b: => Hk)
      |final class Hk(x: Nothing, a: => Hm)
      |final class Hc(h: Boolean, d: Hd)
      |final class Hd(x: Int, c: Hc)
      |final class Hm1(h: Boolean, b: => Option[Hk1])
      |final class Hk1(x: Nothing, a: => Hm1)
      |final class Hm2(h: Boolean, b: => List[Hk2])
      |final class Hk2(x: Nothing, a: => Hm2)
      |final class Hm3(h: Boolean, b: => Either[Boolean, Hk3])
      |final class Hk3(x: Nothing, a: => Hm3)
      |final class Shut private () { final case class In(b: Boolean) }
      |final class Hp(h: Boolean, p: (Hq, Shut#In))
      |final class Hq(x: Int, a: => Hp)
      |final class Hp2(h: Boolean, p: (Shut#In, Hq2))
      |final class Hq2(x: Int, a: => Hp2)
      |object Op { opaque type Th = () => Q; final case class Q(b: Boolean, t: Th) }
      |object Op2 {
      |  opaque type Th[A] = () => A
      |  final case class Bx[A](a: Th[A])
      |  final case class Q(b: Boolean, p: Bx[Q])
      |}
      |final class Hv(x: Int)
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
    TastyFrontEndSpec.rows("Rec.scala" -> text)

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
  // field holds no label: two holes are no choice, νX.X^2 = 1 (it read ω, a wrong number: every
  // `Fn` is the one knot `lazy val f: Fn = Fn(_ => f)`). `Rose` is R = 1 + R, ω, and its strict
  // `Option` is one hole: the one endless tower beside the finite ones, ω.
  private def lazyCycles =
    List("Lz", "Bits", "Fn", "Rose").map(definition) must_== List("1", "ω", "1", "ω")

  // A field holding the cycle only under a function's result, at any depth, defers it too; each
  // lap reads the field with the cycle as one value (the label space). `Ltd` holds (Boolean, () =>
  // Ltd): 2 per lap, one hole, 2^ω = ω (it read 0: a wrong number, in both front ends). `Lu` (Unit,
  // () => Lu): 1 per lap, the one knot, 1. `Lw` has two holes and no label: the one knot, 1 (it
  // read ω as binary trees, which have no labels to tell apart). `Lo` holds Boolean and Option[()
  // => Lo]: 2 × 2 per lap, ω. `Lby` is () => (Boolean, Lby): 2 per lap, ω. `Lcf` a context function's result
  // over two inputs: one continues, the other holds any `Lcf` (ω of them), ε₀. `Lsd` over String
  // inputs and `Fd` over `Int => Boolean` ones likewise, ε₀ (the three read ω before the owner's
  // ruling that a hole off the path is a label, §8). `Lst` a list of thunks: per lap List[Unit] = ω arities, ε₀. `Out(p: In)` and
  // `In(b, f: () => Out)` cycle through a strict field and a lazy one: 1 × 2 per lap, ω each. `Ta1`'s
  // thunk returns a `Tp`, `Ta1` or `Tb1(b, Ta1)`: the parent's two children branch, ω each, `Tp`
  // too. `Kx` cycles strictly through its list beside its thunk: `Kx(Nil, () => k)` ties it, ω
  // arities per lap, ε₀. `K9` demands itself strictly beside its by-name field: no value, 0. `Ks`
  // names itself strictly and lazily in one field, which the reading does not tell apart: no number.
  private def nestedLazyCycles =
    List(
      "Ltd",
      "Lu",
      "Lw",
      "Lo",
      "Lby",
      "Lcf",
      "Lsd",
      "Fd",
      "Lst",
      "Out",
      "In",
      "Tp",
      "Ta1",
      "Tb1",
      "Kx",
      "K9"
    )
      .map(definition) must_==
      List("ω", "1", "1", "ω", "ω", "ε₀", "ε₀", "ε₀", "ε₀", "ω", "ω", "ω", "ω", "ω", "ε₀", "0")

  // An arm with no value continues nothing: `Zb.Never` holds a `Nothing`, so `Za` is the endless
  // stream of its Boolean, 2^ω = ω (the empty arm in the cycle read 0). A name under an empty factor
  // is no edge: `Ia`'s `o` is always `None`, so `Ia` = νX. 2·X = ω and `Ib` = String × Ia = ω (the
  // `Ib` inside it read as a label: ε₀).
  private def emptyParts =
    List("Za", "Ia", "Ib").map(definition) must_== List("ω", "ω", "ω")

  // A sum's arm outside its cycle that holds only lazy values has μ 0 and its ν value beside it.
  // `Pb` is an endless Boolean stream, 2^ω = ω, and `Pt` holds every `Pb`: with `Pb` at ω, Pt = Pa +
  // ω and Pa = Pt is ω, so `Pt` and `Pa` are ω (they read 1: the one `Pa` tower, a wrong number).
  // `Qb` continues the parent's own cycle: two children branch, ω each. `Ub` sits under a nested
  // sum `Ui` outside the cycle: Ui = Ub = ω, Ut = Ua + Ui = ω (`Ut`, `Ua` read 1). `Mc` is a strict
  // child beside the lazy `Mb`: Mt = Ma + Mb + 2 = ω whichever way. `Ra` holds `Rc`, a definition
  // outside its cycle over the stream `Rb`: Rc = ω, so `Ra` is a stream of ω labels, ω^ω = ε₀ (it
  // read 0: `Rc` at its μ value emptied the lap).
  private def lazyArms = expect(
    "Pt" -> "ω",
    "Pa" -> "ω",
    "Pb" -> "ω",
    "Qt" -> "ω",
    "Qa" -> "ω",
    "Qb" -> "ω",
    "Ut" -> "ω",
    "Ui" -> "ω",
    "Ua" -> "ω",
    "Ub" -> "ω",
    "Mt" -> "ω",
    "Ma" -> "ω",
    "Mb" -> "ω",
    "Mc" -> "2",
    "Ra" -> "ε₀",
    "Rc" -> "ω",
    "Rb" -> "ω"
  )

  // A field that holds a sealed parent of its cycle strictly holds, each lap, either a child in the
  // cycle or one outside it. `Bq2(t: => Bq2, b: Bq)`: `b` is a `Bq1` (an endless Boolean stream, ω)
  // or a `Bq2`, so `Bq2` is a stream of ω labels that branches, ε₀, and `Bq` holds it, ε₀ (both read
  // ω: the parent read as one value per lap). `Bo2` likewise over `Bo1(s: String)`, ε₀. `Rx(t: =>
  // Rx, c: Rp)` with `Rp` = `Rp1(s: String)` | `Rp2(Rx)`: ε₀, and `Rp`, `Rp2` hold it, ε₀.
  private def heldParents = expect(
    "Bq" -> "ε₀",
    "Bq1" -> "ω",
    "Bq2" -> "ε₀",
    "Bo" -> "ε₀",
    "Bo1" -> "ω",
    "Bo2" -> "ε₀",
    "Rx" -> "ε₀",
    "Rp" -> "ε₀",
    "Rp1" -> "ω",
    "Rp2" -> "ε₀"
  )

  // An arm with two holes continues through one and holds any value of its node in the others,
  // the cycle's own included (owner's ruling, §8): a label like any other. `Nc(t: => Nc, p: Np)`:
  // `p` is any `Np` (an `Nqo(s: String)` under the sealed `Nq`, or an `Nc`), ω labels or more per
  // lap, ε₀, and so are `Np`, `Nq` (they read ω). `Oh`, `Uh`, `Fh` hold their cycle's parent in a
  // strict `Option`, a by-name field and a function field beside their own tail: any `Op` per lap,
  // ε₀ (they read ω). `En.B(t, e: En)` and `Em.B(t, e: Em)`: `e` is any `En` (`Em`), ω of them, ε₀
  // (they read ω). `Rbt(b, l, r)`: `r` is any `Rbt`, of which there are ω (Boolean-labelled), so ε₀.
  // `Bi(t: => Bi, x: Int, b: Bqi)`: `b` beside the tail is a second hole, any `Bqi`, ε₀. A capacity
  // label leaves the degree readable (the fields' width grows once per hole): `Gr(x: Int, p: Gp)`
  // with `Gp` = `Go(s: String)` | `Gl(t: => Gr)` holds `p` once, 2^32 labels per lap, ω; `Nd(v:
  // Int, next: Option[() => Nd])` and `Ck(p: (Int, () => Ck))` one hole each, ω; `Ci` is `Cq` with
  // `Int` labels, ω each. A wide constant beside the cycle does not hide a second hole: `Eb(t: =>
  // Eb, e: Either[Long => Boolean, Eb])` and `Sl` over `Set[Long]` hold any `Eb` (`Sl`) beside the
  // tail, ε₀; `Ed(e: Either[Long => Boolean, (() => Ed, () => Ed)])` two holes, any `Ed` off the
  // path, ε₀. Nor does it hide that a field holds the cycle: `Ek8(t: => Ek8, e: Either[Int,
  // Option[Ek8]])` and `Ek7` (with a Boolean beside the `Option`) hold any of themselves beside the
  // tail, ε₀ (they read ω: the sum's width hid `e`'s name). `Sb(s: String, x: Set[Sa])` beside
  // `Sa(t: => Sb)`: a String label and a set of any `Sa` per lap, ε₀. A one-hole
  // arm keeps one value per lap: `Sh(c: Cq)` ends the unfolding when `c` is a `Cq1`, so `Cq` is
  // Lh/Sh words, ω, not ε₀. Holes alone are no choice: `Tt(l: => Tt, r: => Tt)` and `Nf(f: Int =>
  // Nf)` are the one knot each, νX.X·X = 1 (they read ω).
  private def holes = expect(
    "Np" -> "ε₀",
    "Nq" -> "ε₀",
    "Nqo" -> "ω",
    "Nc" -> "ε₀",
    "Op" -> "ε₀",
    "Oh" -> "ε₀",
    "Uh" -> "ε₀",
    "Fh" -> "ε₀",
    "En" -> "ε₀",
    "Em" -> "ε₀",
    "Cq" -> "ω",
    "Sh" -> "ω",
    "Lh" -> "ω",
    "Tt" -> "1",
    "Nf" -> "1",
    "Rbt" -> "ε₀",
    "Bqi" -> "ε₀",
    "Bi" -> "ε₀",
    "Gr" -> "ω",
    "Nd" -> "ω",
    "Ck" -> "ω",
    "Ci" -> "ω",
    "Si" -> "ω",
    "Li" -> "ω",
    "Eb" -> "ε₀",
    "Sl" -> "ε₀",
    "Ed" -> "ε₀",
    "Ek7" -> "ε₀",
    "Ek8" -> "ε₀",
    "Sa" -> "ε₀",
    "Sb" -> "ε₀"
  )

  private def expect(expected: (String, String)*) =
    expected.map((name, _) => name -> definition(name)) must_== expected

  // `Op.Q` holds an opaque alias of a thunk of itself, read inside its scope (rule F): an endless
  // Boolean stream, ω, which the completion does not take apart; it read 0 (`Q` demanded strictly).
  // `Op2.Q` holds it under a class argument: no in-place reading of `Bx` (it read 0 too).
  private def uncompleted =
    (definition("Ks"), definition("Op.Q"), constructor("Op.Q"), definition("Op2.Q")) must_==
      (
        "lazy cycle not completed",
        "lazy cycle not completed",
        "?",
        "a class a system reads through a type argument"
      )

  // The method side refuses a recursive position; the row takes the estimate's size: `Loop`'s
  // position is 0, `Lz`'s 1, `Str`'s String × Option[Str] = ω × (1 + ε₀), ε₀. `AbsR` reads 0, the
  // algebra's value of a function from `Nothing`, which does not say that no value exists (the
  // absurd function is one): `HoldsAbs` keeps the method side's refusal.
  private def positions =
    List("Loop", "Lz", "Str", "HoldsAbs").map(constructor) must_== List("0", "1", "ε₀", "?")

  // A recursive position the method side refuses because a class does not show a parameter: under
  // rule B the constructor observes no plain parameter of another instance, so such a class holds
  // its observed fields only. `Ha(h: Boolean, b: => Hb)`: `Hb` shows nothing, one value, 2 × 1 = 2;
  // `Hb(x: Int, a: => Ha)` 2^32 × 1 (the estimate's capacity). `Hn(h: Boolean, next: => Hz)`: `Hz`'s
  // one subtype `Hn` shows nothing, 2 × 1. `Ho(e: => He)`: `He` is `Ho` (1) or the case class
  // `Ht(i: Int => He)`, whose `i` is a field: finitely He = 1 + He^(2^32), ω, and endlessly each
  // `Ht` lap continues through one input while the others hold any `He`, ω labels, ε₀ (owner's
  // ruling, §8; it read ω); `Ht`'s position `Int => He` is ε₀. `Hw(v: Hv)` reaches no class again:
  // the method side's refusal stands. A class with no value has none whatever the constructor
  // observes: `Hk` holds a `Nothing`, so `Hm`'s position is empty, 0, and so is `Hk`'s (no `Hm`
  // without a `Hk`); `Hc` and `Hd` need each other strictly, no value, 0 each. A part beside the
  // cycle the method side refuses keeps the refusal in either order: `Shut` is never built, so
  // `Shut#In` is refused in `Hp` and `Hp2`, and in `Hq` and `Hq2`, which reach them. An empty class
  // under a wrapper leaves the wrapper's own values: `Hm1`'s `b` is `None`, 2 × 1; `Hm2`'s `Nil`,
  // 2; `Hm3`'s a `Left`, 2 × 2 (they read 4, ω, 6: the observed `Hk` as one value).
  private def hiddenPositions =
    List(
      "Ha",
      "Hb",
      "Hn",
      "Ho",
      "Ht",
      "Hw",
      "Hm",
      "Hk",
      "Hc",
      "Hd",
      "Hp",
      "Hp2",
      "Hm1",
      "Hm2",
      "Hm3"
    )
      .map(constructor) must_==
      List("2", "≤2^32", "2", "ε₀", "ε₀", "?", "0", "0", "0", "0", "?", "?", "2", "2", "4")

  // `Neg` reaches itself through a function's domain, where the numeric equation is no count
  // (Cantor). `GS[A]` continues through `=> GS[A]`, a class named with arguments: a field the
  // completion reads with the cycle as one value, A × 1 per lap, the reason its parameter `A`. `N`
  // and `UN` read `N` at Boolean, then at pairs of them, 4 values, then 16, ...: no end. `Wt` reads `W` at itself, and `W` reads itself
  // at that argument again; `Lb` reads `Lbox` at itself, whose by-name field the ν completion would
  // not see read in place (Lb is the `Lz` knot, 1); `LzB`, `Nb`, `Lc` read `Box` at a function
  // type of themselves, whose knot (`LzB` is `Lz`, 1; `Nb` is ω) the ν completion would not see in
  // place either (it read 0): each a class a system reads through a type argument, refused.
  // `X` is counted: it holds `Option[X[A] & Two[A]]`, and no class is both an `X` and a `Two` (the
  // closed world), so X = 1 + 0 = 1 (it read 1 + min(X, 2), widened and refused).
  private def refusals =
    (definition("X") === "1").and(
      List("Neg", "GS", "N", "UN", "Wt", "Lb", "LzB", "Nb", "Lc").map(definition) must_== List(
        "recursion through a function's domain",
        "A",
        "a class a system reads with ever new type arguments",
        "a class a system reads with ever new type arguments",
        "a class a system reads through a type argument",
        "a class a system reads through a type argument",
        "a class a system reads through a type argument",
        "a class a system reads through a type argument",
        "a class a system reads through a type argument"
      )
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
