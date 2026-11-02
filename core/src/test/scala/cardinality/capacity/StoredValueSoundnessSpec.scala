package cardinality.capacity

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// Stored-value soundness (docs/research/stored-value-families.md): sealed families are tagged sums
// over their file's subtypes or a named refusal (explicit imports: `ImportClausesSpec`); a
// reference carries the reasons of what it reads; context bounds are stored evidence; a
// finite collection keeps its element's tier; an enclosing template's abstract member and a match
// type stuck on the definition's own parameters wait for the instantiation; a floating-point
// stand-in stays marked; and a row's reasons render by class. Each number is derived beside it
// (fixtures checked with scalac 3.9.0).
class StoredValueSoundnessSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    Sealed families are tagged sums
      children in the companion, recursion included              $companionFamily
      a parameterised family instantiates through its slots       $parameterisedFamily
      an instantiation is its own least fixed point               $instantiationFixedPoint
      sealed intermediates are summed once                        $intermediates
      a match type in a subtype reduces on the parent's arguments $matchThroughFamily

    A family that is not closed is refused, and says why
      an anonymous or given instance, open or inner subtypes       $notClosed
      unpinned, pinned or bounded subtype parameters               $notUniform
      a bound reads alike only beside its parent                   $boundScope
      a subtype through an alias, or reached twice                 $aliasesAndOverlap
      a subtype through a renamed import                           $renamedParent
      a parent name an object rebinds names another type          $rebinding
      a subtype varies as the slot it binds, or is refused        $slotVariance
      a bottom pin is the standard library's Nothing              $shadowedBottom
      a data constructor is the standard library's                $shadowedData
      an enum case pinned to one instantiation is refused         $enumCases

    Recursion and match types the model does not count stay unknown
      recursion through a function's domain has no count          $negativeRecursion
      a constructor's slots are its declaration's, never its name $constructorSlots
      a case is passed over only when provably disjoint           $matchDisjointness
      literals and tuple elements are told apart                  $matchLiterals
      a pattern matches by spelling only where both mean alike    $matchScopes
      a cycle through a value's type member has no count          $unseenCycle

    Reasons, evidence, containers, members, match types and marks
      a reference carries the reasons of what it reads            $taint
      context bounds are constructor inputs                       $contextBounds
      a named context bound is the bound's evidence               $namedBound
      a finite collection keeps its element's tier                $containers
      an enclosing template's abstract member is a parameter      $abstractMember
      a match type stuck on its own parameters is theirs           $stuckMatch
      a floating-point stand-in survives sums and products        $lossy
      every class of reasons renders, unresolved first             $reasonClasses
    """

  // |Tree| = μX. 1 + X·X = ω (Leaf and Node sit in the companion, Scala 3's usual shape).
  // `Light` sums two case objects: 2.
  def companionFamily = {
    val found = rows(
      """|sealed trait Tree
         |object Tree {
         |  case object Leaf extends Tree
         |  final case class Node(l: Tree, r: Tree) extends Tree
         |}
         |final case class Forest(t: Tree)
         |sealed trait Light
         |object Light { case object Red extends Light; case object Green extends Light }
         |final case class UsesLight(l: Light)
         |""".stripMargin
    )
    (found(("Tree", Abstract)) === "ω")
      .and(found(("Tree.Node", Class)) === "ω")
      .and(found(("Forest", Class)) === "ω")
      .and(found(("UsesLight", Class)) === "2")
  }

  // |Opt[Boolean]| = |No| + |Yes[Boolean]| = 1 + 2 (`Nothing` pins the covariant slot bottom-up).
  // `Fl[X, Y] extends Flip[Y, X]`: Flip[Boolean, Unit] binds Y = Boolean, X = Unit: 1·2 = 2.
  // `Sum[Boolean]` through a sealed abstract class's companion: 1 + 2 = 3. The parents' own rows
  // wait for their parameters.
  def parameterisedFamily = {
    val found = rows(
      """|sealed trait Opt[+A]
         |case object No extends Opt[Nothing]
         |final case class Yes[+A](a: A) extends Opt[A]
         |final case class U(o: Opt[Boolean])
         |sealed trait Flip[A, B]
         |final case class Fl[X, Y](x: X, y: Y) extends Flip[Y, X]
         |final case class UF(f: Flip[Boolean, Unit])
         |sealed abstract class Sum[+B]
         |object Sum {
         |  case object Empty extends Sum[Nothing]
         |  final case class One[+B](b: B) extends Sum[B]
         |}
         |final case class UsesSum(s: Sum[Boolean])
         |""".stripMargin
    )
    (found(("U", Class)) === "3")
      .and(found(("UF", Class)) === "2")
      .and(found(("UsesSum", Class)) === "3")
      .and(found(("Opt", Abstract)) === "instantiation[A]")
      .and(found(("Flip", Abstract)) === "instantiation[A, B]")
  }

  // An instantiation solves its own recursion from 0: |L[Nothing]| = μX. 1 + 0·X = 1,
  // |L[Unit]| = μX. 1 + X = ω, |T[Nothing]| = μX. 0 + X·X = 0 (the template's ω would not do).
  // Through `&`, read as `min`, |M[Boolean]| = μX. 1 + min(X, 2) grows 1, 2, 3 and settles: the
  // widened ω is no bound the equation keeps (it falls to 3), so a reason, never ω. `Ev[A]` read
  // again inside `Od[A]` inside `Ev[A]` has no joint fixed point solved: a reason that says so.
  def instantiationFixedPoint = {
    val code =
      """|sealed trait L[+A]
         |case object Nil0 extends L[Nothing]
         |final case class Cons[+A](h: A, t: L[A]) extends L[A]
         |final case class LN(l: L[Nothing])
         |final case class LU(l: L[Unit])
         |enum T[A] { case Leaf(a: A); case Node(l: T[A], r: T[A]) }
         |final case class TN(t: T[Nothing])
         |sealed trait M[A]
         |final case class Stop[A]() extends M[A]
         |final case class Go[A](next: M[A] & Two[A]) extends M[A]
         |sealed trait Two[A]
         |final case class T1[A]() extends Two[A]
         |final case class T2[A]() extends Two[A]
         |final case class UM(m: M[Boolean])
         |sealed trait Ev[+A]
         |object Ev { final case class Even[A](o: Od[A]) extends Ev[A]; case object EZ extends Ev[Nothing] }
         |sealed trait Od[+A]
         |object Od { final case class Odd[A](e: Ev[A]) extends Od[A] }
         |final case class UE(e: Ev[Boolean])
         |""".stripMargin
    val found = rows(code)
    def refusedWith(name: String, reason: String) =
      constructor(name, "A.scala" -> code) must beLike {
        case Count.Unresolved(reasons) => reasons.exists(_.contains(reason)) must beTrue
      }
    (found(("LN", Class)) === "1")
      .and(found(("LU", Class)) === "ω")
      .and(found(("TN", Class)) === "0")
      .and(found(("UM", Class)) === "unresolved[recursive instantiation not settled: M]")
      .and(found(("UE", Class)) === "unresolved[recursive instantiation not solved: Ev]")
      .and(refusedWith("UM.<init>", "recursive instantiation not settled: M"))
      .and(refusedWith("UE.<init>", "recursive instantiation not solved: Ev"))
  }

  // |E| = |Z| + |O| with the sealed intermediate O = μX. |S| + 1 = μX. X + 1 = ω: ω + 1.
  // |Pair[Boolean, Unit]| = |Both| + |Sub| = 2·1 + |Left0[Boolean, Unit]| = 2 + 2 = 4.
  def intermediates = {
    val found = rows(
      """|sealed trait E
         |sealed trait O extends E
         |final case class Z() extends E
         |final case class S(p: O) extends O
         |case object One extends O
         |final case class UE(e: E)
         |sealed trait Pair[A, B]
         |object Pair {
         |  final case class Both[A, B](a: A, b: B) extends Pair[A, B]
         |  sealed trait Sub[A, B] extends Pair[A, B]
         |  final case class Left0[A, B](a: A) extends Sub[A, B]
         |}
         |final case class UP(p: Pair[Boolean, Unit])
         |""".stripMargin
    )
    (found(("UE", Class)) === "ω + 1").and(found(("UP", Class)) === "4")
  }

  // `Miss[A](fst: Fst[A])` read for `Af[(Boolean, Boolean), Nothing]` reduces `Fst` on the tuple
  // as the reference wrote it, in the reference's scope: 2 at the top level, 1 inside `Inner`,
  // where `Boolean` is `Unit` (`Hit` holds a `Nothing`). `Pin[X]` holds |X| + |Boolean|·1 values:
  // a function of X, and |Pin[Unit]| = 3. `Fused` (eo's shape) depends on its parameters only.
  // `Fst[Int]` matches no case: a reason of its own.
  def matchThroughFamily = {
    val found = rows(
      """|type Fst[T] = T match { case (f, s) => f }
         |type Snd[T] = T match { case (f, s) => s }
         |sealed trait Af[A, +B]
         |object Af {
         |  final case class Miss[A](fst: Fst[A]) extends Af[A, Nothing]
         |  final case class Hit[A, +B](snd: Snd[A], b: B) extends Af[A, B]
         |}
         |object Inner {
         |  type Boolean = Unit
         |  final case class UsesAf(a: Af[(Boolean, Boolean), Nothing])
         |}
         |final case class UsesAf2(a: Af[(Boolean, Boolean), Nothing])
         |final class Fused[S, T, A, B, M0](keep: S => Af[(T, M0), A], restore: (M0, B) => T)
         |final case class Pin[X](a: Af[(X, Boolean), Unit])
         |final case class UsePin(p: Pin[Unit])
         |final case class Stuck(a: Af[Int, Unit])
         |""".stripMargin
    )
    (found(("UsesAf2", Class)) === "2")
      .and(found(("Inner.UsesAf", Class)) === "1")
      .and(found(("Pin", Class)) === "instantiation[X]")
      .and(found(("UsePin", Class)) === "3")
      .and(parameters(found(("Fused", Class))) === Set("S", "T", "A", "B", "M0"))
      .and(found(("Stuck", Class)) === "unresolved[a match type]")
  }

  private def refused(code: String): String = rows(code)(("Use", Class))

  // The parameters an instantiation-dependent row waits for, in any order.
  private def parameters(verdict: String): Set[String] =
    if (!verdict.startsWith("instantiation[")) Set(verdict)
    else verdict.stripPrefix("instantiation[").stripSuffix("]").split(", ").toSet

  private def notSummed(family: String, why: String) =
    s"unresolved[sealed family not summed: $family ($why)]"

  // Each family has values no named subtype the estimate reads enumerates: an anonymous instance,
  // a given, a plain class any file may extend, an inner class (one per enclosing instance), an
  // abstract subtype that is not sealed. Its users get no number (before: 1, 2, …).
  def notClosed =
    (refused(
      "sealed trait W\ncase object W1 extends W\nobject Anon { val extra: W = new W {} }\nfinal case class Use(w: W)"
    ) === notSummed("W", "an anonymous instance in its file"))
      .and(
        refused("sealed trait W\nclass Q(x: Boolean) extends W\nfinal case class Use(w: W)") ===
          notSummed("W", "a subtype open to extension: Q")
      )
      .and(
        refused("sealed trait W\ngiven W with {}\nfinal case class Use(w: W)") ===
          notSummed("W", "a given instance in its file")
      )
      .and(
        refused(
          "sealed trait W\nclass Outer { final case class In(x: Boolean) extends W }\nfinal case class Use(w: W)"
        ) === notSummed("W", "a subtype outside its body and objects: In")
      )
      .and(
        refused(
          "sealed trait W\ntrait Mid extends W\ncase object K extends W\nfinal case class Use(w: W)"
        ) ===
          notSummed("W", "an abstract subtype open to extension: Mid")
      )

  // `Fused[S, M0] extends W[S]` leaves M0 to range over every type; `IntW extends W[Int]` (and an
  // invariant `Nothing`) pins a slot, a GADT index: |W[Boolean]| holds no `IntW`; `C[A <: Int]` is
  // a member of `W[t]` only for `t <: Int`, so `W[Boolean]` holds none (the sum would say 2). Each
  // reason names the subtype and only the parameter at fault.
  def notUniform =
    (refused(
      "sealed abstract class W[S]\nobject W { final class Fused[S, M0](keep: S => M0) extends W[S] }\nfinal case class Use(w: W[Boolean])"
    ) === notSummed(
      "W",
      "sealed subtype binds a parameter to no slot of its parent: Fused (M0, of W)"
    ))
      .and(
        refused(
          "sealed trait W[A, B]\nfinal case class Twice[A](a: A) extends W[A, A]\nfinal case class Use(w: W[Boolean, Unit])"
        ) === notSummed(
          "W",
          "sealed subtype binds a parameter to several slots of its parent: Twice (A, of W)"
        )
      )
      .and(
        refused(
          "sealed trait W[A]\nfinal case class IntW(i: Int) extends W[Int]\nfinal case class Use(w: W[Boolean])"
        ) === notSummed(
          "W",
          "sealed subtype leaves a parameter of its parent unpinned: IntW (Int, of W)"
        )
      )
      .and(
        refused(
          "sealed trait W[A]\ncase object Z extends W[Nothing]\nfinal case class Use(w: W[Boolean])"
        ) === notSummed(
          "W",
          "sealed subtype leaves a parameter of its parent unpinned: Z (Nothing, of W)"
        )
      )
      .and(
        refused(
          "sealed trait W[A]\nfinal case class C[A <: Int](a: A) extends W[A]\nfinal case class Use(w: W[Boolean])"
        ) === notSummed(
          "W",
          "sealed subtype parameter is bounded otherwise than its slot: C.A (of W)"
        )
      )
      .and(
        rows(
          "sealed trait W[A <: Int]\nfinal case class C[A <: Int](a: A) extends W[A]\nfinal case class Use(w: W[Int])"
        )(("Use", Class)) === "≤2^32"
      )

  // A bound spelled as its slot's means what the slot's does only where both read alike: `O.C[A <:
  // X]` reads `O.X = Nothing`, so `C[Boolean]` is ill-formed and P[Boolean] holds nothing (scalac:
  // `O.C[Boolean](true)` is E057) — refused, not 2. An F-bound naming the parameter its own slot
  // binds reads alike everywhere: |Q[Boolean, Boolean]| = |D[Boolean, Boolean]| = 2·2 = 4; one
  // naming another slot's parameter does not.
  def boundScope =
    (refused(
      "type X = Any\nsealed trait P[A <: X]\nobject O {\n  type X = Nothing\n  final case class C[A <: X](a: A) extends P[A]\n}\nfinal case class Use(p: P[Boolean])"
    ) === notSummed(
      "P",
      "sealed subtype parameter is bounded otherwise than its slot: C.A (of P)"
    ))
      .and(
        rows(
          "sealed trait Q[A <: B, B]\nfinal case class D[A <: B, B](a: A, b: B) extends Q[A, B]\nfinal case class Use(q: Q[Boolean, Boolean])"
        )(("Use", Class)) === "4"
      )
      .and(
        refused(
          "sealed trait Q[A <: B, B, C]\nfinal case class D[A <: B, B <: C, C](a: A) extends Q[A, C, B]\nfinal case class Use(q: Q[Boolean, Boolean, Boolean])"
        ) === notSummed(
          "Q",
          "sealed subtype parameter is bounded otherwise than its slot: D.A (of Q)"
        )
      )

  // `X extends WA` with `type WA = W` is a subtype the parent's name does not spell (the sum would
  // say 1, the truth is 1 + 2); so is `new WA {}`. `Both` reaches `D` through `D1` and `D2`: a
  // summand counted twice. The method side refuses the same constructors.
  def aliasesAndOverlap = {
    val alias =
      "sealed trait W\ntype WA = W\nfinal case class X(b: Boolean) extends WA\ncase object Y extends W\nfinal case class Use(w: W)"
    val anonymous =
      "sealed trait W\ntype WA = W\ncase object Y extends W\nobject M { val w: W = new WA {} }\nfinal case class Use(w: W)"
    (refused(alias) === notSummed("W", "a subtype extends it through an alias: X"))
      .and(refused(anonymous) === notSummed("W", "an anonymous instance in its file"))
      .and(
        refused(
          "sealed trait D\nsealed trait D1 extends D\nsealed trait D2 extends D\nfinal case class Both(b: Boolean) extends D1 with D2\nfinal case class Use(d: D)"
        ) === notSummed("D", "its subtype Both extends it more than once")
      )
      .and(constructor("Use.<init>", "A.scala" -> alias) must beLike {
        case Count.Unresolved(reasons) => reasons.exists(_.contains("through an alias")) must beTrue
      })
  }

  // `import x.{P => Q}` (or `P as Q`, in another package clause) lets `Z extends Q` extend `P`
  // under a name no alias spells (scalac: `List[P](A, O.Z)` compiles): |P| = 1 + 1 = 2, never the
  // 1 that omits Z — refused, and the method side's constructor with it.
  def renamedParent = {
    val renamed = "x/A.scala" ->
      "package x\nsealed trait P\ncase object A extends P\nobject O {\n  import x.{P => Q}\n  case object Z extends Q\n}\nfinal case class Use(p: P)"
    val clauses = "x2/A.scala" ->
      "package x2 {\n  sealed trait P\n  case object A extends P\n  final case class Use(p: P)\n}\npackage y2 {\n  import x2.P as Q\n  case object Z extends Q\n}"
    val reason = notSummed("P", "a subtype extends it through a renamed import: Z")
    (rowsOf(renamed)(("x.Use", Class)) === reason)
      .and(rowsOf(clauses)(("x2.Use", Class)) === reason)
      .and(constructor("x.Use.<init>", renamed) must beLike { case Count.Unresolved(_) => ok })
      .and(constructor("x2.Use.<init>", clauses) must beLike { case Count.Unresolved(_) => ok })
  }

  // An object's parent or export binds the parent's name inside it (scalac: `val y: P = O.Z`
  // fails, `val x: Base.P = O.Z` compiles): the top-level `P` has the one subtype `A`, so |Use| =
  // 1 (the sum said 2). `Base#P`'s subtype sits in `O`, outside its body: refused. A parent the
  // file does not declare may bind it: refused. A same-file parent without a `P` changes nothing.
  def rebinding = {
    val inherited =
      "sealed trait P\ncase object A extends P\ntrait Base { sealed trait P }\nobject O extends Base { case object Z extends P }\nfinal case class Use(p: P)"
    val found = rows(inherited)
    val exported = rows(
      "sealed trait P\ncase object A extends P\nobject Other { sealed trait P }\nobject O { export Other.P; case object Z extends P }\nfinal case class Use(p: P)"
    )
    (found(("Use", Class)) === "1")
      .and(
        found(("Base#P", Abstract)) === notSummed("P", "a subtype outside its body and objects: Z")
      )
      .and(constructor("Use.<init>", "A.scala" -> inherited) === Count.Finite(1))
      .and(exported(("Use", Class)) === "1")
      .and(
        refused(
          "sealed trait P\ncase object A extends P\nobject O extends zz.Base { case object Z extends P }\nfinal case class Use(p: P)"
        ) === notSummed("P", "a subtype whose clause may name another P: Z")
      )
      .and(
        refused(
          "sealed trait P\ntrait Low\nobject P extends Low { case object A extends P; case object B extends P }\nfinal case class Use(p: P)"
        ) === "2"
      )
  }

  // `P[-A]` holds `C[u]` for every `u >: t`: `Use(C[Any]("x"))` is a `Use` whatever `C`'s field
  // says, so |Use| ≥ |String| (the sum said 2) — refused unless `C` is contravariant, when `C[u] <:
  // C[t]` and |Q[Boolean]| = |D[Boolean]| = 2^2 = 4. A covariant slot holds `E[u]` for `u <: t`,
  // whose `u => Boolean` is no field of `E[t]`: refused; `Have[A]` holds its `A` as data (|S[Boolean]|
  // = 1 + 2·|List[Boolean]|).
  def slotVariance = {
    val code =
      """|sealed trait Q[-A]
         |final case class D[-A](f: A => Boolean) extends Q[A]
         |final case class UseQ(q: Q[Boolean])
         |sealed trait R[+A]
         |final case class E[A](f: A => Boolean) extends R[A]
         |final case class UseR(r: R[Boolean])
         |sealed trait S[+A]
         |final case class Have[A](value: A, more: List[A]) extends S[A]
         |case object Missing extends S[Nothing]
         |final case class UseS(s: S[Boolean])
         |""".stripMargin
    val found = rows(code)
    (refused(
      "sealed trait P[-A]\nfinal case class C[A](a: A) extends P[A]\nfinal case class Use(p: P[Boolean])"
    ) ===
      notSummed(
        "P",
        "sealed subtype parameter of a contravariant slot is not contravariant: C.A (of P)"
      ))
      .and(found(("UseQ", Class)) === "4")
      .and(
        found(("UseR", Class)) ===
          notSummed(
            "R",
            "sealed subtype parameter of a covariant slot is not covariant: E.A (of R)"
          )
      )
      .and(found(("UseS", Class)) === "ω + 1")
      .and(constructor("UseR.<init>", "A.scala" -> code) must beLike {
        case Count.Unresolved(_) => ok
      })
  }

  // With `type Nothing = Boolean`, `Z extends P[Nothing]` is a `P[Boolean]` only (scalac: `val no:
  // P[Unit] = Z` fails): |Use| = |C[Unit]| = 1, not 2. The pin is read where `Z` is written.
  def shadowedBottom =
    refused(
      "type Nothing = Boolean\nsealed trait P[+A]\ncase object Z extends P[Nothing]\nfinal case class C[+A](a: A) extends P[A]\nfinal case class Use(p: P[Unit])"
    ) === notSummed("P", "a subtype pins it with Nothing, which a source binds: Z")

  // `List` is the standard library's data constructor only where no source binds it: with `type
  // List[A] = A => Boolean` in another file of the package, `E[A](xs: List[A])` is the refused
  // `E[A](f: A => Boolean)` (scalac: `val x: R[Boolean] = E[Nothing](_ => true)` compiles), not 4.
  // A covariant `Box` the file declares holds data wherever its name denotes it.
  def shadowedData = {
    val shadowed = rowsOf(
      "p/L.scala" -> "package p\ntype List[A] = A => Boolean",
      "p/R.scala" -> "package p\nsealed trait R[+A]\nfinal case class E[A](xs: List[A]) extends R[A]\nfinal case class UseR(r: R[Boolean])",
    )
    (shadowed(("p.UseR", Class)) ===
      notSummed("R", "a subtype holds a parameter of it through List, which a source binds: E"))
      .and(
        rows(
          "final case class Box[+A](a: A)\nsealed trait R[+A]\nfinal case class E[A](b: Box[A]) extends R[A]\ncase object N extends R[Nothing]\nfinal case class UseR(r: R[Boolean])"
        )(("UseR", Class)) === "3"
      )
  }

  // `case Y extends E[Int]` is an `E[Int]` only (scalac: `val y: P[Boolean] = E.Y` fails): |E
  // [Boolean]| = |X[Boolean]| = 2, which a sum over every case says is 3 — directly, and as a sealed
  // family's subtype. A bottom pin is a member of every instantiation: |H[Boolean]| = 1 + 2.
  def enumCases = {
    val gadt =
      "enum E[A] {\n  case X(a: A)\n  case Y extends E[Int]\n}\nfinal case class Use(e: E[Boolean])"
    val reason =
      "unresolved[enum case not summed: E.Y (sealed subtype leaves a parameter of its parent " +
        "unpinned: Y (Int, of E))]"
    (refused(gadt) === reason)
      .and(
        refused(
          "sealed trait P[A]\nenum E[A] extends P[A] {\n  case X(a: A)\n  case Y extends E[Int]\n}\nfinal case class Use(p: P[Boolean])"
        ) === reason
      )
      .and(
        refused(
          "enum H[+A] { case N extends H[Nothing]; case V(a: A) }\nfinal case class Use(h: H[Boolean])"
        ) ===
          "3"
      )
  }

  // `MC(f: M => Boolean)` from 0 reads 0 (the empty domain), yet `MC(_ => true)` is a closed value,
  // and X ≅ 2^X has no solution: no count, directly, through a generic family (`N[A]`) or through
  // another definition's domain (`Pred[K]`). A recursion through a function's result is counted.
  def negativeRecursion = {
    val code =
      """|sealed trait M
         |final case class MC(f: M => Boolean) extends M
         |final case class UseM(m: M)
         |sealed trait N[A]
         |final case class NC[A](f: N[A] => Boolean) extends N[A]
         |final case class UseN(n: N[Boolean])
         |final case class Pred[A](f: A => Boolean)
         |sealed trait K
         |final case class KC(p: Pred[K]) extends K
         |case object K0 extends K
         |final case class UseK(k: K)
         |final case class Res(k: Int => Res, b: Boolean)
         |""".stripMargin
    val found = rows(code)
    def negative(names: String*) =
      names
        .map(n => s"recursion through a function's domain: $n")
        .mkString("unresolved[", ", ", "]")
    (found(("UseM", Class)) === negative("M", "MC"))
      .and(found(("UseN", Class)) === negative("N"))
      .and(found(("UseK", Class)) === negative("K", "KC"))
      .and(found(("Res", Class)) === "ω")
      .and(constructor("UseN.<init>", "A.scala" -> code) must beLike {
        case Count.Unresolved(_) => ok
      })
  }

  // A constructor's slots are what its declaration makes them, wherever its name is written: a
  // source `List[A](f: A => Boolean)` (or alias `Option[A] = A => Boolean`) in another file of the
  // package makes `P(l: List[P])` the negative P ≅ 2^P (scalac: `P(List(_ => true))` and `P(List(_
  // => false))` differ), not the 0 its name's standard reading gave; a supplied positive `Box` in
  // another file keeps T = μX. 1 + X = ω; a constructor no source supplies is not read at all.
  def constructorSlots = {
    val list = rowsOf(
      "n/A.scala" -> "package n\nfinal case class List[A](f: A => Boolean)",
      "n/B.scala" -> "package n\nfinal case class P(l: List[P])\nfinal case class UseP(p: P, b: Boolean)",
    )
    val option = rowsOf(
      "q/O.scala" -> "package q\ntype Option[A] = A => Boolean",
      "q/P.scala" -> "package q\nfinal case class P(o: Option[P])",
    )
    val box = rowsOf(
      "b/Box.scala" -> "package b\nfinal case class Box[A](a: A)",
      "b/T.scala" -> "package b\nsealed trait T\nfinal case class N(kids: Box[T]) extends T\ncase object L extends T\nfinal case class UseT(t: T)",
    )
    val foreign = rows(
      "import zz.Chunk\nsealed trait T\nfinal case class N(kids: Chunk[T]) extends T\ncase object L extends T"
    )
    (list(("n.UseP", Class)) === "unresolved[recursion through a function's domain: P]")
      .and(option(("q.P", Class)) === "unresolved[recursion through a function's domain: P]")
      .and(box(("b.UseT", Class)) === "ω")
      .and(
        foreign(("T", Abstract)) must contain(
          "recursion through a constructor the estimate does not read: T (through Chunk)"
        )
      )
  }

  // Scala passes over a case only where the scrutinee is provably disjoint from its pattern:
  // `H[Any]` is stuck on `case Int` (scalac: "cannot be shown to be disjoint"), directly and through
  // a family's subtype (the spelling-only reading said 1); `String`, a tuple and a function are
  // disjoint from `Int` and reach `case _` (1), and `H[Int]` is `Boolean` (2).
  def matchDisjointness = {
    val found = rows(
      """|type H[T] = T match {
         |  case Int => Boolean
         |  case _ => Unit
         |}
         |sealed trait P[A]
         |final case class C[A](h: H[A]) extends P[A]
         |final case class UAny(h: H[Any])
         |final case class UFamily(p: P[Any])
         |final case class UString(h: H[String])
         |final case class UTuple(h: H[(Int, Int)])
         |final case class UFunction(h: H[String => String])
         |final case class UInt(h: H[Int])
         |""".stripMargin
    )
    (found(("UAny", Class)) === "unresolved[a match type]")
      .and(found(("UFamily", Class)) === "unresolved[a match type]")
      .and(
        List("UString", "UTuple", "UFunction").map(n => found((n, Class))) === List("1", "1", "1")
      )
      .and(found(("UInt", Class)) === "2")
  }

  // `x: q.X` reads class `C`'s member through the value `q`, which the solver reads over an earlier
  // solve of the body and never iterates from 0: with `X = D => Boolean`, D ≅ 2^D has no count
  // (scalac: `D(c)(_ => true)` and `D(c)(_ => false)` are values; the old reading said ω); with
  // `X = D` there is no value at all and with `X = Option[D]` there are ω, neither of which the
  // earlier solve's ω placeholder says: each is a reason, never the number.
  def unseenCycle = {
    val negative = rows(
      "final class C { type X = D => Boolean }\nfinal case class D(q: C)(x: q.X)\nfinal case class UseD(d: D)"
    )
    val hidden = "unresolved[recursion through a type member of a value: D]"
    (negative(("UseD", Class)) === "unresolved[recursion through a function's domain: D]")
      .and(
        rows("final class C { type X = D }\nfinal case class D(q: C)(x: q.X)")(
          ("D", Class)
        ) === hidden
      )
      .and(
        rows("final class C { type X = Option[D] }\nfinal case class D(q: C)(x: q.X)")(
          ("D", Class)
        ) === hidden
      )
  }

  // A literal pattern matches the same literal and is disjoint from another (`K[2]` = Unit, 1;
  // `K[1]` = Boolean, 2) but not from its own class (`K[Int]` is stuck); a tuple pattern is
  // disjoint from a tuple when one element pair is (`(Int, Boolean)` vs `(Int, String)`: 1); a
  // literal is disjoint from another final class (`"a"` vs `Int`: 1).
  def matchLiterals = {
    val found = rows(
      """|type K[T] = T match {
         |  case 1 => Boolean
         |  case _ => Unit
         |}
         |type E[T] = T match {
         |  case (Int, Boolean) => Boolean
         |  case _ => Unit
         |}
         |type S[T] = T match {
         |  case Int => Boolean
         |  case _ => Unit
         |}
         |final case class K2(k: K[2])
         |final case class K1(k: K[1])
         |final case class KI(k: K[Int])
         |final case class ET(e: E[(Int, String)])
         |final case class SL(s: S["a"])
         |""".stripMargin
    )
    (List("K2", "K1", "ET", "SL").map(n => found((n, Class))) === List("1", "2", "1", "1"))
      .and(found(("KI", Class)) === "unresolved[a match type]")
  }

  // A pattern spelled as the argument matches only where every name of it means one declaration
  // as the pattern and the argument read it: `F[Int]` written where `type Int = String` is `F`'s
  // `case _` (String is disjoint from scala.Int; scalac warns that `true` is discarded as Unit), and
  // so is `H.F[Int]` against `H`'s own `Int`: 1, never the 2 a spelling match said — stuck, `?`.
  def matchScopes =
    (rows(
      "type F[X] = X match {\n  case Int => Boolean\n  case _ => Unit\n}\nobject O { type Int = String; final case class C(f: F[Int]) }"
    )(("O.C", Class)) === "unresolved[a match type]")
      .and(
        rows(
          "object H {\n  type Int = String\n  type F[X] = X match {\n    case Int => Boolean\n    case _ => Unit\n  }\n}\nfinal case class C(f: H.F[Int])"
        )(("C", Class)) === "unresolved[a match type]"
      )

  // Every reader of a definition resting on an unknown name rests on it too — through a generic
  // instantiation, a field, an `Either`, two hops, and a product with an empty factor (0·? is not
  // claimed as 0); a `Throwable` is an open class, left to its implementations. A reference that
  // supplies a definition's parameters owes none of them: |Gen[Boolean]| = 2; `Gen[B]` waits for
  // B; an open member keeps the reader open.
  def taint = {
    val files = List(
      "a/A.scala" -> "package a\nfinal case class Foo[A](s: Schema, a: A)\nenum Failure { case Boom(cause: Throwable); case Code(n: Int) }\nfinal case class Gen[A](a: A)\ntrait Open\nfinal case class HasOpen(o: Open)",
      "b/B.scala" -> "package b\nimport a.*\nfinal case class Bar(f: Foo[Boolean])\nfinal case class Q(s: Schema)\nfinal case class Local(q: Q)\ntype Res = Either[Failure, Boolean]\ntype Res2 = Either[Q, Boolean]\nfinal case class G1(g: Gen[Boolean])\nfinal case class G2[B](g: Gen[B])\nfinal case class UsesOpen(h: HasOpen)\nfinal case class Zero(n: Nothing, q: Q)\nfinal case class Nested(l: Local)",
    )
    val found = rowsOf(files*)
    (found(("b.Bar", Class)) === "unresolved[Schema]")
      .and(found(("b.Local", Class)) === "unresolved[Schema]")
      .and(found(("b.Nested", Class)) === "unresolved[Schema]")
      .and(found(("b.Res", Alias)) === "open[Throwable]")
      .and(found(("b.Res2", Alias)) === "unresolved[Schema]")
      .and(found(("b.Zero", Class)) === "unresolved[Schema]")
      .and(found(("b.G1", Class)) === "2")
      .and(found(("b.G2", Class)) === "instantiation[B]")
      .and(found(("b.UsesOpen", Class)) === "open[Open]")
      .and(constructor("b.Local.<init>", files*) must beLike { case Count.Unresolved(_) => ok })
  }

  // `C[T: TC]` stores a `TC[T]`. |Sorted[Boolean]| = |Boolean|·|Ord[Boolean]| = 2·|Boolean =>
  // Boolean| = 2·4 = 8; |Two[Unit]| = 1·|Ord[Unit]|² = 2·2 = 4 (one field per bound). An unknown
  // type class is a reason: `TT` holds a `Traverse[T]` (before: 1).
  def contextBounds = {
    val code =
      """|final class TT[T[_]: Traverse, A, B]
         |final case class TB[F[_]: Functor, B](b: B)
         |sealed trait Ord[A]
         |object Ord { final case class ByBool[A](f: A => Boolean) extends Ord[A] }
         |final case class Sorted[A: Ord](a: A)
         |final case class UsesSorted(s: Sorted[Boolean])
         |final case class Two[A: Ord: Ord](a: A)
         |final case class UsesTwo(t: Two[Unit])
         |""".stripMargin
    val found = rows(code)
    (found(("TT", Class)) === "unresolved[Traverse]")
      .and(found(("TB", Class)) === "unresolved[B, Functor]")
      .and(found(("UsesSorted", Class)) === "8")
      .and(found(("UsesTwo", Class)) === "4")
      .and(constructor("UsesTwo.<init>", "A.scala" -> code) === Count.Finite(4))
  }

  // `T: Ord as o` names the evidence `o`; the field is still an `Ord[T]`: |U3| = 1·|Ord[Unit]| = 2.
  def namedBound =
    rows(
      "sealed trait Ord[A]\nfinal case class ByBool[A](f: A => Boolean) extends Ord[A]\nfinal case class C3[T: Ord as o](t: T)\nfinal case class U3(c: C3[Unit])"
    )(("U3", Class)) === "2"

  // |C[A]| ≥ |A| (the one-element collections): over an ε₀-tier element (`String => String`,
  // `Any`) a List, Array, Set, Vector, a Map's keys or values, or a repeated parameter is ε₀; over
  // a finite or countable one ω (`Set[Boolean]` = 2² = 4); over `Nothing` 1 (`Map[Boolean,
  // Nothing]` = 1¹… the empty map). The method side keeps its own saturating reading (ω).
  def containers = {
    val code =
      """|type L = List[String => String]
         |type A = Array[Any]
         |type S = Set[String => String]
         |type LB = List[Boolean]
         |type LN = List[Nothing]
         |type SB = Set[Boolean]
         |type SS = Set[String]
         |type MK = Map[String => String, Boolean]
         |type MV = Map[Boolean, String => String]
         |type MV0 = Map[Boolean, Nothing]
         |type VV = Vector[Vector[Any]]
         |final case class Rep(xs: (String => String)*)
         |""".stripMargin
    val found = rows(code)
    def sizes(names: String*) = names.map(n => found((n, Alias)))
    (sizes("L", "A", "S", "MK", "MV", "VV") === List.fill(6)("ε₀"))
      .and(sizes("LB", "LN", "SB", "SS", "MV0") === List("ω", "1", "4", "ω", "1"))
      .and(found(("Rep", Class)) === "ε₀")
      .and(constructor("Rep.<init>", "A.scala" -> code) === Count.Bounded(EffectiveOmega))
  }

  // Inside `Opt[T]`, `M` is `this.M`, which each instance chooses: `X = (T, M)` is a function of
  // that choice and of T.
  def abstractMember =
    rows("sealed abstract class Opt[T] { type M; type X = (T, M) }")(("Opt#X", Alias)) ===
      "instantiation[T, member M]"

  // `Fst[T]` and `Miss[A](fst: Fst[A])` are stuck on their own parameters: each instantiation
  // reduces them (`Fst[(Boolean, Int)]` = 2). `Deep[T] = Fst[Fst[T]]` too; applied to a concrete
  // tuple it would need the inner match reduced to syntax first, which is not modelled: a reason.
  def stuckMatch = {
    val found = rows(
      """|type Fst[T] = T match { case (f, s) => f }
         |final class Miss[A](val fst: Fst[A])
         |type Reduced = Fst[(Boolean, Int)]
         |type Deep[T] = Fst[Fst[T]]
         |final case class UD(d: Deep[((Boolean, Unit), Unit)])
         |""".stripMargin
    )
    (found(("Fst", Alias)) === "instantiation[T]")
      .and(found(("Miss", Class)) === "instantiation[A]")
      .and(found(("Reduced", Alias)) === "2")
      .and(found(("Deep", Alias)) === "instantiation[T]")
      .and(found(("UD", Class)) === "unresolved[a match type]")
  }

  // |Double| stands in as at most 2^64 (lossy): 2^64 + 2^64 rounds to 2^65, 2^64·2^32 to 2^96,
  // both bounds, both still marked.
  def lossy = {
    val found = rows(
      "enum Shape { case C(r: Double); case S(s: Double) }\nfinal case class P(a: Double, b: Int)"
    )
    (found(("Shape", Enum)) === "≤2^65 (lossy)").and(found(("P", Class)) === "≤2^96 (lossy)")
  }

  // An unknown name is not hidden behind an open abstraction, nor an open one behind a parameter:
  // every class renders, unresolved first.
  def reasonClasses = {
    val found = Counter
      .definitions(
        dialects
          .Scala3(
            "trait Open\nfinal case class Mixed(o: Open, s: Schema)\nfinal case class Gen[A](o: Open, a: A)"
          )
          .parse[Source]
          .get
      )
      .map(d => d.name -> d)
      .toMap
    (Definition.reasons(found("Mixed")) === "unresolved: Schema; open to implementations (Open)")
      .and(
        Definition.reasons(found("Gen")) ===
          "open to implementations (Open); depends on its instantiation (A)"
      )
      .and(Definition.gap(found("Mixed")) === Some(Definition.Gap.Unresolved))
      .and(Definition.gap(found("Gen")) === Some(Definition.Gap.Open))
  }

}
