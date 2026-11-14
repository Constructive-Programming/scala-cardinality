package cardinality.analysis.inhabitation

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import org.specs2.Specification
import org.specs2.execute.Result

// Elimination of supplied sum- and ground-valued callables (capabilities design, C10) and the
// restricted ω-saturation (C11 as decision D-capabilities and cross-check X3 restrict it). Every
// fixture is valid Scala 3.9.0 (compiled with scalac; session fixtures b4/spec sums1, c11,
// b4fix/spec sat, low and keys, b4fix2/spec c11 and named; the c11 fixture extends the
// cross-check's c11). Each count is derived in the comment above its check.
class NeutralSaturationSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Neutrals eliminated once per argument tuple and path (C10)
      an outer observation's cases enable the inner one             $composed
      a write reads the cases of the read it shares an argument with $fromFunctions
      a prism's modify and replace                                  $prism
      an argument a producer builds is a neutral argument too       $builtArgument
      a Boolean result is a two-valued case                         $ground
      an absurd case collapses to one                               $absurdCase
      neutrals are told apart by structure, not by spelled names    $structuredKeys
    Argument spaces not enumerated keep a reason
      a producer cycle feeds the arguments                          $unbounded
      the cases feed back the callable's own argument               $payloadCycle
      a ground argument space is not enumerated                     $literals
      a higher-order argument keeps the opaque reason               $higherOrder
      a sum argument space is not enumerated                        $sumArgument
      an argument type a lower bound builds is not enumerated       $lowerBoundArgument
      an argument type a polymorphic member builds is not enumerated $polymorphicArgument
      a record's callable is named by its field                     $recordField
      a callable is named by its source name, whatever it spells    $sourceNames
    ω with reasons elsewhere (C11, restricted by X3)
      a reason-free cycle beside a reasoned producer is ω           $reasonedCycle
      the same below a split and below a product                    $vouchedBelow
      a branch absurd only through a reasoned rule is never ω       $branchAbsurd
      a sibling that may truly be empty is never ω                  $siblingZero
      a context absurd through a reasoned rule is never ω           $reasonedAbsurd
      a cycle through a polymorphic instantiation is never ω        $quotientCycle
    The same shapes without reasons (X3's controls)
      a reason-free cycle in a consistent branch                    $branchCycle
      a branch the solver proves absurd collapses                   $branchAbsurdPlain
      an empty sibling empties the product                          $siblingEmpty
  """

  private def counts(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Sums.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refused(found: Count, fragment: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  private def refusedExactly(found: Count, reason: String): Result =
    found must beLike { case Count.Unresolved(reasons) => reasons must contain(reason) }

  private val sums = counts("""package sums1
    |final class FF[S, T, A, B](read: S => Either[T, A], write: (S, B) => T)
    |object U {
    |  def compose[S, A, B](outer: S => Option[A], inner: A => Option[B]): S => Option[B] =
    |    s => outer(s).flatMap(inner)
    |  def modify[S, T, A, B](tear: S => Either[T, A], mend: B => T, f: A => B): S => T =
    |    s => tear(s).fold(identity, a => mend(f(a)))
    |  def replace[S, T, A, B](tear: S => Either[T, A], mend: B => T, b: B): S => T =
    |    s => tear(s).fold(identity, _ => mend(b))
    |  def rebuilt[S, A, B](pick: S => Option[A], mend: B => S, b: B): S => S = s => s
    |  def choose[A](a: A, b: A, p: A => Boolean): A = if p(a) then a else b
    |  def flag[A](a: A, p: A => Boolean): Boolean = p(a)
    |  def right[A, B](p: A => Either[Nothing, B], a: A): B = p(a) match { case Right(b) => b }
    |  def first[S, A](s: S, step: S => S, test: S => Option[A]): Option[A] = test(s)
    |  def again[S](p: S => Option[S], s: S): Option[S] = p(s)
    |  def literal[A](g: A => Int, h: Int => A, a: A): Int = g(a)
    |  def higher[A, B](p: (A => A) => Option[B]): Option[B] = p(identity)
    |}
    |""".stripMargin)

  // Memo E3: outer(s) None gives None (1); Some(a) enables inner(a): None (1), Some(b) — None or
  // Some(b) (2) — so 1 · (1 · 2) = 2: λs. None and λs. outer(s).flatMap(inner).
  def composed = sums("sums1.U.compose") === Count.Finite(2)

  // Memo E5: `S ⇒ Either[T, A]` is `read` (each case rebuilt: 1); `(S, B) ⇒ T` gives t or write(s, b)
  // when read(s) is Left(t), write(s, b) when Right (2) — 2.
  def fromFunctions = sums("sums1.FF.<init>") === Count.Finite(2)

  // modify: Left(t) gives t; Right(a) gives mend(f(a)) — 1. replace: Left(t) gives t or mend(b) (2),
  // Right gives mend(b) (1) — 2.
  def prism =
    (sums("sums1.U.modify") === Count.Finite(1)).and(sums("sums1.U.replace") === Count.Finite(2))

  // The S values are s and mend(b), so pick(s) and pick(mend(b)) are both cased on: each of the four
  // case combinations answers s or mend(b) — 2^4 = 16.
  def builtArgument = sums("sums1.U.rebuilt") === Count.Finite(16)

  // choose: a or b for each of the four values of (p(a), p(b)) — 16. flag: true, false, p(a), !p(a).
  def ground =
    (sums("sums1.U.choose") === Count.Finite(16)).and(sums("sums1.U.flag") === Count.Finite(4))

  // p(a) is Left(_: Nothing), absurd (1), or Right(b), answered by b (1) — 1.
  def absurdCase = sums("sums1.U.right") === Count.Finite(1)

  // Memo A14: test(s), test(step(s)), … is an unbounded family of neutrals.
  def unbounded =
    refused(sums("sums1.U.first"), "sum elimination over an unbounded argument space: test")

  // p(s) binds an S in its Some case, which p takes again.
  def payloadCycle =
    refused(sums("sums1.U.again"), "sum elimination over an unbounded argument space: p")

  // g's arguments include h(n) for every Int literal n.
  def literals =
    refused(
      sums("sums1.U.literal"),
      "sum elimination over an argument space not enumerated: g"
    )

  def higherOrder =
    refused(
      sums("sums1.U.higher"),
      "Elimination of an opaque sum-producing callable is unsupported"
    )

  // `x` and `y` are named so that the old string keys of q(x, c) and q(a, y) were one string —
  // `(input/<scope>:a,input/<scope>:b,input/<scope>:c)` — and one neutral went unsplit
  // (2^(2^15)). Four A values make 16 neutrals q(·, ·), each Boolean answered per case: 2^(2^16),
  // as for the plainly named twin.
  def structuredKeys = {
    val head = "package keys\nobject U {\n  "
    val scope = s"Keys.scala:${head.length}"
    val code = head +
      s"def odd[A](q: (A, A) => Boolean, a: A, `a,input/$scope:b`: A, c: A, " +
      s"`b,input/$scope:c`: A): Boolean = true\n" +
      "  def plain[A](q: (A, A) => Boolean, a: A, x: A, c: A, y: A): Boolean = true\n}\n"
    val found = MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Keys.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap
    (found("keys.U.odd") === found("keys.U.plain"))
      .and(found("keys.U.plain").render === "2^(2^16)")
  }

  private val sat = counts("""package sat
    |trait Wrap[F[_]] { def wrap[M](m: M): F[M] }
    |trait Lift[F[_]] { def lift[M](m: M): F[M] }
    |final class Picker[S, A](val pick: S => Option[A], val mend: A => S)
    |object U {
    |  def lowOption[A, B >: Option[A]](p: B => Boolean, a: A): Boolean = true
    |  def lowPair[A, B >: (A, A)](p: B => Boolean, a: A): Boolean = true
    |  def built[F[_], A](w: Wrap[F], p: F[A] => Boolean, a: A): Boolean = true
    |  def optArg[A](p: Option[A] => Boolean, a: A): Boolean = true
    |  def loop[F[_], A](k: Lift[F], g: F[A] => A, a: A): F[A] = k.lift(a)
    |  def field[S, A](inner: Picker[S, A], s: S): Option[A] = None
    |}
    |""".stripMargin)

  // p's arguments are the Option[A] terms None and Some(a), built, not neutral: 16 in truth, refused.
  def sumArgument =
    refused(sat("sat.U.optArg"), "sum elimination over an argument space not enumerated: p")

  // B's values are built through its lower bound (None, Some(a); (a, a)): 16 and 4 in truth.
  def lowerBoundArgument =
    refused(sat("sat.U.lowOption"), "sum elimination over an argument space not enumerated: p")
      .and(
        refused(sat("sat.U.lowPair"), "sum elimination over an argument space not enumerated: p")
      )

  // wrap at M := A builds an F[A], p(wrap(a)): 4 in truth, but an instantiation is no neutral this
  // enumerates.
  def polymorphicArgument =
    refused(sat("sat.U.built"), "sum elimination over an argument space not enumerated: p")

  // pick's Some(a) reaches mend's S, which pick takes again; the reason names the callable as the
  // record field it is read from: `inner`'s first field.
  def recordField =
    refused(sat("sat.U.field"), "sum elimination over an unbounded argument space: inner#0")

  private val named = counts("""package named
    |object U {
    |  def first[S, A](s: S, step: S => S, `te:st`: S => Option[A]): Option[A] = None
    |  def lit[A](`g:h`: A => Int, h: Int => A, a: A): Int = 0
    |}
    |""".stripMargin)

  // As `unbounded` and `literals`: the reason names the eliminated callable as its source spells
  // it, never a piece of its binding's identity (which `g:h`'s colon once split, naming `h`).
  def sourceNames =
    refusedExactly(
      named("named.U.first"),
      "sum elimination over an unbounded argument space: te:st"
    )
      .and(
        refusedExactly(
          named("named.U.lit"),
          "sum elimination over an argument space not enumerated: g:h"
        )
      )

  // lift(a), lift(g(lift(a))), … is a cycle through an instantiation of lift at M := A, which free
  // theorems may merge: the cycle is not vouched for, so ω is never claimed (true count ω).
  def quotientCycle =
    refused(
      sat("sat.U.loop"),
      "polymorphic capability binder: M (free-theorem quotient not modelled for sat.Lift.lift)"
    )

  private val c11 = counts("""package c11
    |object T {
    |  def branchAbsurd[A, B, C](y: Either[(A => Nothing, B, B => B), B], u: (C => C) => A): B = ???
    |  def branchAbsurdPlain[A, B, C](y: Either[(A => Nothing, B, B => B), B], u: C => A, c: C): B = ???
    |  def branchCycle[A, B](y: Either[(B, B => B), B]): B = ???
    |  def siblingZero[B, D, E](y: Either[(B, B => B), (D => Either[B, E], D, D => D)]): B = ???
    |  def siblingEmpty[B, D](y: Either[(B, B => B), D]): B = ???
    |  def reasonedCycle[A, B](b: B, step: B => B, h: (A => A) => B): B = b
    |  def reasonedAbsurd[A, B](b: B, step: B => B, k: A => Nothing, h: (B => B) => A): B = b
    |  def branchCycleReasoned[A, B](y: Either[(B, B => B), B], h: (A => A) => B): B = ???
    |  def pairCycleReasoned[A, B, C](b: B, step: B => B, c: C, h: (A => A) => B): (B, C) = (b, c)
    |}
    |""".stripMargin)

  // Left: b, step(b), … (ω); Right: its b (1) — ω · 1.
  def branchCycle = c11("c11.T.branchCycle") === Count.Countable

  // Left is absurd through u(identity), a higher-order application the solver does not vouch for,
  // so its cycle may collapse (true count 1): never ω.
  def branchAbsurd = c11("c11.T.branchAbsurd") must beLike {
    case Count.Unresolved(reasons) =>
      reasons.exists(_.contains("Higher-order application is unsupported")) must beTrue
  }

  // With u: C ⇒ A and c the solver proves Left absurd: 1 · 1.
  def branchAbsurdPlain = c11("c11.T.branchAbsurdPlain") === Count.Finite(1)

  // Right's only B comes from an unbounded family of eliminations, which may never answer Left
  // (true count 0): ω · 0 is never claimed.
  def siblingZero =
    refused(c11("c11.T.siblingZero"), "sum elimination over an unbounded argument space")

  // Right has no B: ω · 0 = 0.
  def siblingEmpty = c11("c11.T.siblingEmpty") === Count.Finite(0)

  // h's application carries a reason, but b, step(b), … is a reason-free cycle in a context nothing
  // makes absurd: ω.
  def reasonedCycle = c11("c11.T.reasonedCycle") === Count.Countable

  // The vouched path runs through a split (Left: b, step(b), … beside h(…); Right: its own b — ω ·
  // ≥ 1) and through a product (the B field cycles, the C field is c) with h's reasoned rule at
  // every B goal: ω both.
  def vouchedBelow =
    (c11("c11.T.branchCycleReasoned") === Count.Countable)
      .and(c11("c11.T.pairCycleReasoned") === Count.Countable)

  // k(h(step)) may make the context absurd (true count 1), and only a reasoned rule says so.
  def reasonedAbsurd =
    refused(c11("c11.T.reasonedAbsurd"), "Higher-order application is unsupported")

}
