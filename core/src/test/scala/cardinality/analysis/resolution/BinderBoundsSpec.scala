package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Subtype bounds read together (docs/research/higher-kinded-binders.md, R5): a view's fields are
  * read through their own bounds, bounds that refer back to their binder are refused — as invalid
  * programs where Scala 3.9.0 rejects them (E140), as fixpoints the shapes do not decide where it
  * accepts them — `AnyRef`, `AnyVal` and `Matchable` above a structural lower bound are checked
  * against the bound's class, and the refusals of interactions the solver does not count. Every
  * fixture marked valid compiles with 3.9.0; the invalid ones were checked to fail.
  */
class BinderBoundsSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Views of views
      a view's field is read through its own bound                  $fieldViews
      the reading does not depend on a later goal                   $lateGoals
      a bound views a field that two values share as one case       $fieldCases
      a value's reading is marked apart from its path               $viewedPath
    Bounds that refer back
      an F-bound through a structure is refused                     $recursiveBounds
      a recursion through a subtype proof is refused                $evidenceRecursion
      an F-bound through a rigid application is a view              $keyedFBound
      bounds referring back through bounds alone are invalid        $cyclicBounds
      an invalid clause refuses a one-valued result too             $invalidTerminal
      a sandwich and a structure below an F-bound stay counted      $legalReferences
    Class bounds
      AnyRef, AnyVal and Matchable against a lower bound's class    $classBounds
      a class bound reached through a bound or a proof              $reachedClassBounds
      a declared AnyRef is no class bound                           $declaredAnyRef
      a top bound a channel may rebind is a bound to read           $rebindableTops
    Interactions
      a subtype bound beside a callable                             $boundBesideCallable
      a Unit goal is not refused for what it never reads            $unitGoal
      a callable producing a structurally bounded value             $boundedProducer
      two structurally bounded values producing their own type      $sharedCase
      structural bounds above and below one binder                  $bothBounds
      structural bounds meeting through evidence                    $meetingBounds
      structural bounds no type satisfies                           $inconsistentBounds
      an upper bound to an open trait                               $openBound
  """

  private def entries(code: String, path: String = "Bounds.scala"): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def count(code: String, name: String): Count = entries(code)(name)

  private def unresolved(code: String, name: String, fragment: String) =
    count(code, name) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(fragment)) must beTrue
    }

  private def refused(counts: Map[String, Count], name: String, reason: String) =
    counts(name) must beLike {
      case Count.Unresolved(reasons) => reasons must contain(reason)
    }

  private val flag =
    "sealed trait Flag\ncase object On extends Flag\ncase object Off extends Flag\n"

  private val products = "final case class P[X](x: X)\nfinal case class P2[X](x: X, n: Unit)\n"

  // `a` is `P2(b, ())` with `b` a B, so `a.x` is a C: exact 1 (before: 0, a.x kept B's bound
  // unread). Viewed as `P[B]` with `B <: Flag`, `a` is known by `a.x`'s case: |Flag → Flag| = 4
  // (before: 2); `(B, B)` gives every function {On, Off}² → Flag, 2^4 = 16 (before: 2).
  def fieldViews = {
    val counts = entries(flag + products + """
      object U {
        def viewEdge[A <: P2[B], B <: C, C](a: A): C = a.x
        def viewRigid[F[_], A <: P2[B], B <: F[Int]](a: A): F[Int] = a.x
        def unary[A <: P[B], B <: Flag](a: A): Flag = a.x
        def pairNested[A <: (B, B), B <: Flag](a: A): Flag = a._1
        def goalB[A <: P2[B], B <: Flag](a: A): B = a.x
        def sumNested[A <: Option[B], B <: Flag](a: A): Flag = On
      }
    """)
    (counts("U.viewEdge") === Count.Finite(1))
      .and(counts("U.viewRigid") === Count.Finite(1))
      .and(counts("U.unary") === Count.Finite(4))
      .and(counts("U.pairNested") === Count.Finite(16))
      .and(counts("U.goalB") === Count.Finite(1))
      .and(counts("U.sumNested") === Count.Finite(8))
  }

  // The field's view appeared only once a split or a product goal recorded another environment: a
  // Flag goal said 2, a (Flag, Boolean) goal 16 (= 4 · 4). Exact: 4 and 16.
  def lateGoals = {
    val counts = entries(flag + """
      object U {
        def lateUnit[A <: (B, Unit), B <: Flag](a: A): Flag = a._1
        def lateGoalPair[A <: (B, Unit), B <: Flag](a: A): (Flag, Boolean) = (a._1, true)
      }
    """)
    (counts("U.lateUnit") === Count.Finite(4)).and(counts("U.lateGoalPair") === Count.Finite(16))
  }

  // Two fields of one viewed type are one value whenever their cases agree (exact 4 for Flag, 1 for
  // P[Unit]); refused like two such parameters.
  def fieldCases = {
    val counts = entries(flag + products + """
      object U {
        def twoFields[A <: (B, B), B <: Flag](a: A): B = a._1
        def unitFields[A <: (B, B), B <: P[Unit]](a: A): B = a._1
      }
    """)
    val reason = "unsupported subtype evidence interaction: values a bound views as a structural " +
      "type may be the same case"
    refused(counts, "U.twoFields", reason).and(refused(counts, "U.unitFields", reason))
  }

  // A value's path starts with its binding id, which starts with the source path: a source named
  // `viewed…` read every input as already viewed (before: 2).
  def viewedPath = {
    val code = flag + "object U { def f[A <: Flag](a: A): Flag = a }"
    (entries(code, "viewedFlag.scala")("U.f") === Count.Finite(4))
      .and(entries(code, "viewed")("U.f") === Count.Finite(4))
  }

  // Every A holds a smaller A: for an invariant final P no value exists (the context is vacuous,
  // exact 1), `A <: Option[A]` admits None. Before: 2, 4, 2^32. The order-swapped chain is legal
  // Scala (`[A <: B, B <: Option[A]]` is E046 in 3.9.0, order-dependent).
  def recursiveBounds = {
    val counts = entries(products + """
      object R {
        def fbound[A <: P[A]](a: A): A = a.x
        def toBool[A <: P[A]](a: A, n: Boolean): Boolean = n
        def toInt[A <: P[A]](a: A): Int = 0
        def mutual[A <: P[B], B <: P[A]](a: A): A = a.x.x
        def option[A <: Option[A]](a: A): A = a
        def chain[B <: Option[A], A <: B](a: A): A = a
      }
    """)
    def recursive(name: String, bound: String) =
      refused(counts, name, s"recursive bound not read: $bound")
    recursive("R.fbound", "A <: P[A]")
      .and(recursive("R.toBool", "A <: P[A]"))
      .and(recursive("R.toInt", "A <: P[A]"))
      .and(recursive("R.mutual", "B <: P[A]"))
      .and(recursive("R.option", "A <: Option[A]"))
      .and(recursive("R.chain", "B <: Option[A]"))
  }

  def evidenceRecursion =
    count(
      products + "object R { def f[B <: P[A], A](a: A)(using A <:< B): Boolean = true }",
      "R.f"
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(
          "unsupported subtype evidence interaction: a structural view holds a value of its own type"
        )
    }

  // An application's arguments are keys: `A <: F[A]` views a value as F[A] and holds no A.
  def keyedFBound =
    count("object R { def f[F[_], A <: F[A]](a: A): F[A] = a }", "R.f") === Count.Finite(1)

  // 3.9.0: E140 for each (`illegal cyclic type reference`); the clause is invalid even where the
  // binder is never read. Before: 1, 1, ω, 4.
  def cyclicBounds = {
    val counts = entries("""
      object I {
        def eqCycle[A <: B, B <: A](a: A): B = a
        def mutualLow[A >: B, B >: A](a: A): B = a
        def lowOpt[A >: Option[A]](n: Boolean): A = None
        def unused[A >: Option[A]](n: Boolean): Boolean = n
        def union[A <: B | Int, B <: A](a: A): A = a
      }
    """)
    def cyclic(name: String, bound: String) =
      refused(counts, name, s"cyclic type bound (an invalid program): $bound")
    cyclic("I.eqCycle", "A <: B")
      .and(cyclic("I.mutualLow", "B >: A"))
      .and(cyclic("I.lowOpt", "A >: Option[A]"))
      .and(cyclic("I.unused", "A >: Option[A]"))
      .and(cyclic("I.union", "A <: B | Int"))
  }

  // An invalid clause makes the signature no program (3.9.0: E140 for `eqCycle` and `unused`, a
  // view bound is a syntax error), so a `Unit` result has no implementation to count either: the
  // one-value rule (D12) does not apply. An F-bound is legal, and a `Unit` goal has one value: 1.
  def invalidTerminal = {
    val counts = entries("""
      object I {
        def eqCycle[A <: B, B <: A](a: A): Unit = ()
        def unused[A >: Option[A]](n: Boolean): Unit = ()
        def view[A <% Int](a: A): Unit = ()
        def fbound[A <: Option[A]](a: A): Unit = ()
      }
      class C[A >: Option[A]] { def k: Unit = () }
    """)
    refused(counts, "I.eqCycle", "cyclic type bound (an invalid program): A <: B")
      .and(refused(counts, "I.unused", "cyclic type bound (an invalid program): A >: Option[A]"))
      .and(refused(counts, "I.view", "view bound is not Scala 3 (an invalid program): A <% Int"))
      .and(refused(counts, "C.k", "cyclic type bound (an invalid program): A >: Option[A]"))
      .and(counts("I.fbound") === Count.Finite(1))
  }

  // `[B, A >: B <: B]` refers from A to B twice and never back: A and B are one type, a or b (2).
  // A structural lower bound below an F-bound reads no view: `a` (1).
  def legalReferences = {
    val counts = entries(products + """
      object C {
        def sandwich[B, A >: B <: B](a: A, b: B): A = b
        def lowerOnly[A >: P[B], B <: P[A]](a: A): A = a
      }
    """)
    (counts("C.sandwich") === Count.Finite(2)).and(counts("C.lowerOnly") === Count.Finite(1))
  }

  private val classReason = "structural lower bound under a class bound requires class analysis: "

  // `[A >: Flag <: AnyVal]` and `[A >: Boolean <: AnyRef]`: 3.9.0 accepts both and rejects every
  // call (E057), a vacuous context (before: 3 each). A conforming lower bound leaves the class
  // bound a no-op: x, On, Off (3); x, true, false (3). A trait is an AnyRef through a parent that is
  // one (`Sub` has no cases: x alone; `Explicit`: x or E1); one extending only a universal trait is
  // not (`U1 <: U0 <: Any`). Without a lower bound: a or b.
  // A wildcard over a namespace no source supplies never rebinds a builtin name (the never-shadowed
  // assumption), so `<: AnyRef` and `<: Any` under `import foreign.*` are the root imports' tops,
  // as without the wildcard: the twins are the class bound and the no-op, x, On, Off (3).
  def rebindableTops = {
    val code = flag + """
      object W {
        import foreign.*
        def ok[A >: Flag <: AnyRef](x: A): A = x
        def obj[A >: Flag <: Object](x: A): A = x
        def top[A >: Flag <: Any](x: A): A = x
      }
      object K {
        def ok[A >: Flag <: AnyRef](x: A): A = x
        def top[A >: Flag <: Any](x: A): A = x
      }
    """
    val counts = entries(code)
    (counts("W.ok") === Count.Finite(3))
      .and(counts("W.obj") === Count.Finite(3))
      .and(counts("W.top") === Count.Finite(3))
      .and(counts("K.ok") === Count.Finite(3))
      .and(counts("K.top") === Count.Finite(3))
  }

  def classBounds = {
    val counts = entries(flag + """
      sealed trait Sub extends Flag
      sealed trait U0 extends Any
      sealed trait U1 extends U0
      case object U1a extends U1
      sealed trait Explicit extends AnyRef
      case object E1 extends Explicit
      object K {
        def anyval[A >: Flag <: AnyVal](x: A): A = x
        def anyref[A >: Boolean <: AnyRef](x: A): A = x
        def ok[A >: Flag <: AnyRef](x: A): A = x
        def boolVal[A >: Boolean <: AnyVal](x: A): A = x
        def matchable[A >: Flag <: Matchable](x: A): A = x
        def withParents[A >: Sub <: AnyRef](x: A): A = x
        def explicitRef[A >: Explicit <: AnyRef](x: A): A = x
        def universal[A >: U1 <: AnyRef](x: A): A = x
        def noLower[A <: AnyVal](a: A, b: A): A = a
      }
    """)
    refused(counts, "K.anyval", classReason + "A >: Flag <: AnyVal")
      .and(refused(counts, "K.anyref", classReason + "A >: Boolean <: AnyRef"))
      .and(counts("K.ok") === Count.Finite(3))
      .and(counts("K.boolVal") === Count.Finite(3))
      .and(counts("K.matchable") === Count.Finite(3))
      .and(counts("K.withParents") === Count.Finite(1))
      .and(counts("K.explicitRef") === Count.Finite(2))
      .and(refused(counts, "K.universal", classReason + "A >: U1 <: AnyRef"))
      .and(counts("K.noLower") === Count.Finite(2))
  }

  // B >: Flag lies below A <: AnyVal through B's own bound or through a proof: no type fits.
  def reachedClassBounds = {
    val counts = entries(flag + """
      object K {
        def cross[A <: AnyVal, B >: Flag <: A](x: B): A = x
        def viaEv[A <: AnyVal, B >: Flag](x: B)(using ev: B <:< A): A = ev(x)
      }
    """)
    refused(counts, "K.cross", classReason + "A <: AnyVal")
      .and(refused(counts, "K.viaEv", classReason + "A <: AnyVal"))
  }

  // The source `AnyRef` is a capability record now; a structural bound beside a lower one is the
  // evidence analysis's limit, still refused.
  def declaredAnyRef =
    unresolved(
      flag + "class AnyRef { def get: Int = 0 }\nobject K { def f[A >: Flag <: AnyRef](x: A): A = x }",
      "K.f",
      "unsupported subtype evidence interaction: a structural lower bound meets an upper one"
    )

  // `g` takes B, and a contravariant use of A <: B is not read: refused.
  def boundBesideCallable =
    count("object T { def f[A <: B, B, C](a: A, g: B => C): C = g(a) }", "T.f") must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(
          "unsupported subtype evidence interaction: only atomic values and products are supported"
        )
    }

  // One construction whatever the context holds (before: the callable refused it).
  def unitGoal = {
    val counts = entries(flag + """
      object U {
        def callable[A <: B, B](g: B => A): Unit = ()
        def shared[A <: Flag](a: A, b: A): Unit = ()
      }
    """)
    (counts("U.callable") === Count.Finite(1)).and(counts("U.shared") === Count.Finite(1))
  }

  // `k(())` is an A, eliminated as a Flag: an opaque output's case analysis, refused.
  def boundedProducer =
    unresolved(
      flag + "object U { def f[A <: Flag](k: Unit => A): Flag = ??? }",
      "U.f",
      "a callable produces a value its bound views as a structural type"
    )

  // Split by case, `a` and `b` are one value whenever their cases agree: the exact count is
  // 1 · 1 · 2 · 2 = 4, not the 2^4 two independent producers would give. Refused.
  def sharedCase =
    unresolved(
      flag + "object U { def f[A <: Flag](a: A, b: A): A = a }",
      "U.f",
      "values a bound views as a structural type may be the same case"
    )

  private val meet =
    "unsupported subtype evidence interaction: a structural lower bound meets an upper one"

  private def meets(counts: Map[String, Count], name: String) =
    counts(name) must beLike {
      case Count.Unresolved(reasons) => reasons must contain(meet)
    }

  // A value viewed through the upper bound and a construction through the lower one can be the
  // same value. `[A >: Flag <: Flag](x: A): A` makes A Flag: |Flag → Flag| = 4, where reading x
  // beside On and Off gave 3^2 = 9 (before). With Big = {One, Two: Sub; Three}, A >: Sub <: Big
  // gives 2 · 2 · 3 = 12 (x is One or Two among the Sub constructions, or a third value), not 27.
  def bothBounds = {
    val counts = entries(
      flag + """
      sealed trait Big
      sealed trait Sub extends Big
      case object One extends Sub
      case object Two extends Sub
      case object Three extends Big
      object U {
        def both[A >: Flag <: Flag](x: A): A = x
        def between[A >: Sub <: Big](x: A): A = x
        def pinned[A >: Boolean <: Boolean](x: A): A = x
      }
    """
    )
    meets(counts, "U.both").and(meets(counts, "U.between")).and(meets(counts, "U.pinned"))
  }

  // The two bounds meet through an equality, or above both binders through subtype evidence (x is
  // P2(x.x, x.y), one of the four P2 constructions: exact 4, not 5).
  def meetingBounds = {
    val counts = entries(
      flag + """
      final case class P2[X](x: X, y: X)
      object U {
        def edge[B <: Flag, A >: Flag](b: B)(using B =:= A): A = ???
        def below[A <: P2[B], T >: P2[B], B](x: A)(using A <:< T): T = ???
      }
    """
    )
    meets(counts, "U.edge").and(meets(counts, "U.below"))
  }

  // No type lies between Flag and Boolean (3.9.0 accepts the declaration and rejects every call,
  // E057): the context is vacuous, exact count 1. Before: 4.
  def inconsistentBounds =
    unresolved(flag + "object U { def f[A >: Flag <: Boolean](x: A): Boolean = ??? }", "U.f", meet)

  // The bound is the record of Open (capabilities C3): a's `get` is an Int, and nothing builds an
  // A but a — 1.
  def openBound =
    count("trait Open { def get: Int }\nobject V { def f[A <: Open](a: A): A = a }", "V.f") ===
      Count.Finite(1)

}
