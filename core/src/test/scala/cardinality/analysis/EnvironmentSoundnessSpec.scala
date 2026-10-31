package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Readings of an environment or a family that used to count values a body cannot reach, or miss
  * ones it can. Every fixture compiles with Scala 3.9.0 (bodies included, where the file gives one
  * here as `???`); each expected number is derived in the comment beside it.
  */
class EnvironmentSoundnessSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Anonymous contextual parameters
      keep two anonymous parameters apart                          $anonymousApart
      keep anonymous parameters of two owners apart                $anonymousOwners
      count unrelated anonymous evidence once each                 $anonymousDistinct
      refuse same-typed evidence a search cannot tell apart        $anonymousAmbiguous
      read an extension's clauses at its method's level            $anonymousExtension
      refuse evidence a nearer or same-level value shadows         $anonymousShadowed
      refuse evidence a later given of the block shadows           $anonymousLaterGiven
      leave a shallower given out of the question                  $anonymousOuterGiven
      refuse evidence an implicit, import or unread parent may beat $anonymousUnseen
      refuse evidence a named import or an export may beat         $anonymousNamedImports
      refuse an inherited given as an overlap, not an ambiguity    $anonymousInherited
      refuse evidence of a type related to another's               $anonymousRelated
      count evidence of distinct final classes that share a shape   $anonymousSameShape
      tell tuples apart by arity, not when a component relates     $anonymousTuples
      count named evidence of one type by name                     $namedEvidence
    Inherited declarations
      substitute through two levels of inheritance                 $twoLevels
      read a diamond's shared parent once                          $diamond
      keep an unrelated instantiation empty                        $unrelatedInstantiation
      refuse a parent reached with two instantiations              $conflictingInstantiations
      give each receiver its own copy of a shared parent           $twoReceivers
      read a parent that is an inherited member class              $memberClassParent
      read an inherited declaration a final template implements    $finalImplementation
      read inherited vals and givens a final template implements   $finalValues
      leave an abstract final class's declarations open            $abstractFinal
      keep a capture a final implementation reads past a shadow    $hiddenCapture
      keep abstract overloads as separate capabilities             $overloads
      keep an overload across inheritance levels                   $crossLevelOverload
      read a restated declaration as one member                    $restated
      tell a restatement from an overload by declaration           $overloadIdentity
      keep same-shaped parameter classes of two scopes apart       $sameShapeOverloads
      read a refined result as the template's own member           $refinedResult
      keep a lexically enclosing member apart from an inner one    $enclosingMember
      read a type alias the template inherits before an outer one  $inheritedAlias
      keep nearer declarations and private members as they were    $inheritedAliasPrecedence
      refuse a name an unsupplied parent may declare               $unsuppliedParentName
      leave an override through an inherited name open unless final $inheritedOverride
    Templates and families
      refuse a `var` constructor parameter as a mutable capture    $varParameter
      refuse a self type this reading does not enumerate           $selfType
      refuse an enum case pinned to one instantiation              $enumPin
      read enum cases that bind or bottom-pin the parameters       $enumBinds
      read enum cases that take parameters through `extends`       $enumBindsByExtends
      refuse a sealed family with anonymous instances              $sealedAnonymous
      refuse a sealed family extended through an alias             $sealedAlias
      refuse a sealed family with an inner or local child          $sealedInner
      refuse a sealed family with a child in any block             $sealedBlockChild
      read a sealed family whose children live in an object        $sealedInObject
      read a family an alias names but nothing extends through it  $sealedAliasUnused
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def counts(sources: (String, String)*): Map[String, Count] =
    MethodAnalysis.analyze(inputs(sources*)).map(e => e.name -> e.count).toMap

  private def count(code: String, name: String): Count = counts("Example.scala" -> code)(name)

  private def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(fragment)) must beTrue
    }

  // Every anonymous `using` parameter has the empty name; keyed by name, the second replaced the
  // first. `anon2`: f(x), f(g(f(x))), … — ω, as for named f and g. `lost`: x, or the value in
  // `Some(a)` of the first evidence — 2; the `B` evidence is unrelated to the goal.
  def anonymousApart = {
    val found = counts("Anon.scala" -> """object T {
      |  def anon2[A, B](x: A)(using A => B, B => A): B = ???
      |  def lost[A, B](x: A)(using Option[A], B): A = ???
      |}""".stripMargin)
    (found("T.anon2") === Count.Countable).and(found("T.lost") === Count.Finite(2))
  }

  // Two owners of one chain — a method and its local def or class, an extension and its method —
  // each with an anonymous parameter at index 0: keyed by index alone, the inner one replaced the
  // outer. `summon[A]` selects the outer evidence (B is unrelated), the only value of A: 1 each.
  // With `A => B` outside and `B => A` inside: f(x), f(g(f(x))), … — ω.
  def anonymousOwners = {
    val found = counts("Owners.scala" -> """object O {
      |  def outer[A](using A): Unit = { def inner[B](using B): A = ???; () }
      |  def two[A, C](x: C)(using A): Unit = { def inner[B](y: B)(using B): A = ???; () }
      |}
      |object Ext { extension [A](using A)(u: Unit) def pick[B](using B): A = ??? }
      |object Loc {
      |  def anon[A, B](using A => B): Unit = {
      |    class In { def f(using B => A)(x: A): B = ??? }
      |    ()
      |  }
      |  def local[A, B](using A => B): Unit = { def g(using B => A)(x: A): B = ???; () }
      |}
      |object X { extension [A, B](using A => B)(x: A) def go(using B => A): B = ??? }
      |""".stripMargin)
    (found("O.outer.inner") === Count.Finite(1))
      .and(found("O.two.inner") === Count.Finite(1))
      .and(found("Ext.<extension@5>.pick") === Count.Finite(1))
      .and(found("Loc.anon.In.f") === Count.Countable)
      .and(found("Loc.local.g") === Count.Countable)
      .and(found("X.<extension@13>.go") === Count.Countable)
  }

  // `summon[A]` and `summon[B]` each select their own parameter: two unrelated binders. One pair.
  def anonymousDistinct =
    count("object T { def both[A, B](using A, B): (A, B) = ??? }", "T.both") === Count.Finite(1)

  // Two candidates of one type at one level make `summon[Option[A]]` an ambiguity error, so the
  // anonymous one is unreachable: the exact count is 1 (`x`), and the reading refuses rather than
  // say 2. The clauses of one method share a level, so a named sibling is just as ambiguous.
  def anonymousAmbiguous = {
    val found = counts("Ambiguous.scala" -> """object T {
      |  def same[A](x: A)(using Option[A], Option[A]): A = ???
      |  def named[A](x: A)(using o: Option[A])(using Option[A]): A = ???
      |}""".stripMargin)
    refused(found("T.same"), "ambiguous anonymous evidence: Option[A]")
      .and(refused(found("T.named"), "ambiguous anonymous evidence: Option[A]"))
  }

  // An extension's clauses are clauses of its method: `pick[A](x: A)(using o: Option[A])(using
  // Option[A])`, so `summon[Option[A]]` is ambiguous between `o` and the anonymous parameter (3.9.0:
  // E172), which no body can reach. The exact count is 2 (x, or o's content); 12 counted it.
  def anonymousExtension =
    refused(
      count(
        "object E { extension [A](x: A)(using o: Option[A]) def pick(using Option[A]): A = ??? }",
        "E.<extension@1>.pick"
      ),
      "ambiguous anonymous evidence: Option[A]"
    )

  // A class's anonymous evidence competes with the method's own (nested deeper, it wins every
  // search) and with a given of its own body (same level: ambiguous). Neither is counted.
  def anonymousShadowed = {
    val found = counts("Shadowed.scala" -> """class C[A](using Option[A]) {
      |  given Option[A] = None
      |  def f(x: A): A = ???
      |}
      |class D[A](using Option[A]) { def f(x: A)(using Option[A]): A = ??? }""".stripMargin)
    refused(found("C.f"), "ambiguous anonymous evidence: Option[A]")
      .and(refused(found("D.f"), "anonymous evidence selection not resolved: Option[A]"))
  }

  // A block's given is in scope before its own position: `inner`'s `summon[A]` selects the given
  // written after it (3.9.0, run: `outer[String](using "evidence")("param")` prints `param`), so
  // the outer evidence is unreachable — exact 1 (a2); reading only earlier statements said 2.
  def anonymousLaterGiven =
    refused(
      count(
        """object O {
          |  def outer[A](using A)(a2: A): A = { def inner(): A = ???; given A = a2; inner() }
          |}""".stripMargin,
        "O.outer.inner"
      ),
      "anonymous evidence selection not resolved: A (overlaps given A = a2)"
    )

  // A given of the enclosing object sits outside the method: the parameter, nested deeper, wins.
  // x, or the value inside the evidence: 2.
  def anonymousOuterGiven =
    count(
      "object T { given Unit = (); def f[A](x: A)(using Option[A]): A = ??? }",
      "T.f"
    ) === Count.Finite(2)

  // An implicit value of the parameter's type in the class body is a same-level competitor; a
  // `given` import, or a parent outside the source set, brings candidates of unread types.
  def anonymousUnseen = {
    val found = counts("Unseen.scala" -> """import lib.Base
      |object Helpers { given Unit = () }
      |class G[A](using Option[A]) { implicit val other: Option[A] = None; def f(x: A): A = ??? }
      |class H[A](using Option[A]) { import Helpers.given; def f(x: A): A = ??? }
      |class E[A](using Option[A]) extends Base { def f(x: A): A = ??? }""".stripMargin)
    refused(found("G.f"), "ambiguous anonymous evidence: Option[A]")
      .and(refused(found("H.f"), "(overlaps import Helpers.given)"))
      .and(refused(found("E.f"), "(overlaps unsupplied parent Base)"))
  }

  // `import H.s` and `export H.s` bring the given `s` into the class body (3.9.0: E172 against the
  // class's `x$1` for both), so the anonymous evidence is unreachable — exact 1 (x, since s is
  // None). A named selector is read as a candidate of unread type.
  def anonymousNamedImports = {
    val found = counts("Named.scala" -> """class C4[A](using Option[A]) {
      |  object H { given s: Option[A] = None }
      |  import H.s
      |  def f(x: A): A = ???
      |}
      |class C6[A](using Option[A]) {
      |  object H { given s: Option[A] = None }
      |  export H.s
      |  def f(x: A): A = ???
      |}""".stripMargin)
    refused(found("C4.f"), "(overlaps import H.s)").and(
      refused(found("C6.f"), "(overlaps export H.s)")
    )
  }

  // A given inherited from a parent loses to the class's own parameter (3.9.0, run: `flat:
  // evidence`) — no ambiguity; the true count is 2. The ranking is not modelled, so the reading
  // refuses, naming the overlap rather than claiming an ambiguity.
  def anonymousInherited = {
    val found = count(
      """trait GivesOpt[A] { given Option[A] = None }
        |class Flat[A](using Option[A]) extends GivesOpt[A] { def f(x: A): A = ??? }""".stripMargin,
      "Flat.f"
    )
    refused(found, "anonymous evidence selection not resolved: Option[A] (overlaps given")
      .and(found must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("ambiguous")) must beFalse
      })
  }

  // `A => B <: Nothing => B` (functions are contravariant in the argument), so a search for
  // `Nothing => B` finds both and the second may lose it: refused. `A => B` and `B => A`, with
  // unrelated binders on either side, are not related, and count ω as in `anonymousApart`.
  def anonymousRelated =
    refused(
      count("object T { def f[A, B](x: A)(using A => B, Nothing => B): B = ??? }", "T.f"),
      "anonymous evidence selection not resolved: Nothing => B"
    )

  // `(A, B)` and `(A, B, B)` differ in arity: each search selects its own, and A has x and the two
  // first components — 3. `(A, Nothing) <: (A, B)`: related, refused.
  def anonymousTuples = {
    val found = counts("Tup.scala" -> """object Tup {
      |  def arity[A, B](x: A)(using (A, B), (A, B, B)): A = ???
      |  def related[A, B](x: A)(using (A, B), (A, Nothing)): A = ???
      |}""".stripMargin)
    (found("Tup.arity") === Count.Finite(3))
      .and(refused(found("Tup.related"), "anonymous evidence selection not resolved: (A, B)"))
  }

  // `P1[A]` and `P2[A]` are unrelated final classes with one shape: each `summon` selects its own
  // evidence (a 3.9.0 run prints `p2` for `summon[P2[A]].a`), so x, p1.a and p2.a — 3. They were
  // refused as one ambiguous type.
  def anonymousSameShape =
    count(
      """final case class P1[A](a: A)
        |final case class P2[A](a: A)
        |object T { def f[A](x: A)(using P1[A], P2[A]): A = ??? }""".stripMargin,
      "T.f"
    ) === Count.Finite(3)

  // Named parameters are reachable by name whatever implicit search makes of them: x, and the
  // contents of each option — per branch 1, 2, 2, 3 choices, 12 functions.
  def namedEvidence =
    count(
      "object T { def f[A](x: A)(using o: Option[A], p: Option[A]): A = ??? }",
      "T.f"
    ) === Count.Finite(12)

  private val chain = """trait A1[X] { def seed: X }
    |trait B1[X] extends A1[X]
    |trait C1[X] extends A1[X]
    |""".stripMargin

  // `A1.X := B1.X := E1.X`, so `seed: X`: the goal `X` has one value. Through `B1[Option[X]]`,
  // `seed: Option[X]`: None or seed itself (a case split rebuilds it) — 2.
  def twoLevels = {
    val found = counts(
      "Chain.scala" -> (chain +
        "trait E1[X] extends B1[X] { def pick: X = ??? }\n" +
        "trait K1[X] extends B1[Option[X]] { def pick: Option[X] = ??? }")
    )
    (found("E1.pick") === Count.Finite(1)).and(found("K1.pick") === Count.Finite(2))
  }

  // Both paths reach `A1[X]`: one `seed`, one value.
  def diamond =
    counts("Chain.scala" -> (chain + "trait D1[X] extends B1[X] with C1[X] { def pick: X = ??? }"))(
      "D1.pick"
    ) === Count.Finite(1)

  // `seed: Unit` says nothing about `X`: no value of `X` is in scope, 0.
  def unrelatedInstantiation =
    counts("Chain.scala" -> (chain + "trait F2[X] extends B1[Unit] { def pick: X = ??? }"))(
      "F2.pick"
    ) === Count.Finite(0)

  // Covariant `CovA` reached as `CovA[Y]` and `CovA[Option[Y]]`: Scala gives `seed` the type
  // `Y & Option[Y]`, which neither path alone describes. Refused, not read through the first.
  def conflictingInstantiations =
    refused(
      count(
        """trait CovA[+X] { def seed: X }
          |trait CovB[Y] extends CovA[Y]
          |trait CovC[Y] extends CovA[Option[Y]]
          |trait CovD[Y] extends CovB[Y] with CovC[Y] { def pick: Y = ??? }""".stripMargin,
        "CovD.pick"
      ),
      "inherited parent instantiated twice"
    )

  // `seed` (Inner.this) and `Outer.this.seed` are two receivers' slots: 2, as for the declared
  // members of `enclosingMember`. With `Inner2 extends P[(A, A)]`: Outer2.this.seed and the two
  // components of Inner2's seed — 3; the two instantiations belong to two receivers, so nothing is
  // instantiated twice. The `val` twin binds the same two slots as `Inner`.
  def twoReceivers = {
    val found = counts("Shared.scala" -> """trait P[X] { def seed: X }
      |trait Outer[A] extends P[A] { trait Inner extends P[A] { def go: A = ??? } }
      |trait Outer2[A] extends P[A] { trait Inner2 extends P[(A, A)] { def go: A = ??? } }
      |trait PV[X] { val seed: X }
      |trait OuterV[A] extends PV[A] { trait InnerV extends PV[A] { def go: A = ??? } }
      |""".stripMargin)
    (found("Outer.Inner.go") === Count.Finite(2))
      .and(found("Outer2.Inner2.go") === Count.Finite(3))
      .and(found("OuterV.InnerV.go") === Count.Finite(2))
  }

  // `A1` is a member class `Outer2` inherits from `Outer`; `B extends A1[Y]` reads its `seed: Y` —
  // 1 (it was skipped as a parent outside the source set: 0). Through `OuterG[T]`, `T := U`: 1.
  def memberClassParent = {
    val found = counts("Member.scala" -> """trait Outer { trait A1[X] { def seed: X } }
      |trait Outer2 extends Outer { trait B[Y] extends A1[Y] { def go: Y = ??? } }
      |trait OuterG[T] { trait A1 { def seed: T } }
      |trait OuterG2[U] extends OuterG[U] { trait B extends A1 { def go: U = ??? } }
      |""".stripMargin)
    (found("Outer2.B.go") === Count.Finite(1)).and(found("OuterG2.B.go") === Count.Finite(1))
  }

  // A final class implementing `get` as `a` has one value of A, at any inheritance depth: 1 each
  // (the declaration was a second, free capability: 2). `D2.get` is that implementation: a — 1.
  // The open twin's `get` can be overridden by a subclass holding another A: a and get — 2.
  def finalImplementation = {
    val found = counts("Final.scala" -> """trait P[A] { def get: A }
      |trait Q[A] extends P[A]
      |final class C1[A](a: A) extends P[A] { def get: A = a; def other: A = ??? }
      |final class D2[A](a: A) extends Q[A] { def get: A = ???; def other: A = ??? }
      |class O3[A](a: A) extends P[A] { def get: A = a; def other: A = ??? }
      |""".stripMargin)
    (found("C1.other") === Count.Finite(1))
      .and(found("D2.other") === Count.Finite(1))
      .and(found("D2.get") === Count.Finite(1))
      .and(found("O3.other") === Count.Finite(2))
  }

  // An inherited `val` or `given` a final template implements is that member too: `get = a`, so
  // `other` has one value (each read 2), and `pick.other` only `a`. `OB.flag = true` is no second
  // Boolean: true, false — 2 (read 4). The open twin `OpenV` can be overridden: a and get — 2.
  def finalValues = {
    val found = counts("FinalVal.scala" -> """trait PV[A] { val get: A }
      |final class FV[A](a: A) extends PV[A] { val get: A = a; def other: A = ??? }
      |final class FP[A](val get: A) extends PV[A] { def other: A = ??? }
      |final case class FC[A](get: A) extends PV[A] { def other: A = ??? }
      |trait PG[A] { given g: A }
      |final class FG[A](a: A) extends PG[A] { given g: A = a; def other: A = ??? }
      |trait PD[A] { def get: A }
      |object GivenInst { given pick[A](using a: A): PD[A] with { def get: A = a; def other: A = ??? } }
      |class OpenV[A](a: A) extends PV[A] { val get: A = a; def other: A = ??? }
      |""".stripMargin)
    (found("FV.other") === Count.Finite(1))
      .and(found("FP.other") === Count.Finite(1))
      .and(found("FC.other") === Count.Finite(1))
      .and(
        count(
          "trait PB { val flag: Boolean }\nobject OB extends PB { val flag: Boolean = true; def other: Boolean = ??? }",
          "OB.other"
        ) === Count.Finite(2)
      )
      .and(found("FG.other") === Count.Finite(1))
      .and(found("GivenInst.pick.other") === Count.Finite(1))
      .and(found("OpenV.other") === Count.Finite(2))
  }

  // `abstract final` is legal and implements nothing: `P.f(x: A)` is open (its own `f(x: Int)` is
  // an overload), so m has x, a, f(x), f(a), f(f(x)), … — ω, as before rule 5 (it read 2).
  def abstractFinal =
    count(
      """trait P[A] { def f(x: A): A }
        |abstract final class F[A](a: A) extends P[A] { def f(x: Int): A = a; def m(x: A): A = ??? }
        |""".stripMargin,
      "F.m"
    ) === Count.Countable

  // Inside `m(a1: A)` the parameter hides `mk`'s a1, which no prefix can name; `seed` is that a1.
  // So m has x, a2 and seed — 3 (mk(1, 2).m(3) can give 3, 2 or 1; dropping seed read 2). A local
  // object alike. A body that only computes from the hidden capture is refused (true count 3). The
  // class-member twin reaches `G.this.a1` anyway — 3; `H`'s object reads it through `seed` — 2.
  def hiddenCapture = {
    val found = counts("Hidden.scala" -> """trait P[A] { def seed: A }
      |object M2 {
      |  def id[A](a: A): A = a
      |  def mk[A](a1: A, a2: A): P[A] = {
      |    final class F extends P[A] { def seed: A = a1; def m(a1: A): A = ??? }
      |    new F
      |  }
      |  def mo[A](a1: A, a2: A): P[A] = {
      |    object F extends P[A] { def seed: A = a1; def m(a1: A): A = ??? }
      |    F
      |  }
      |  def mi[A](a1: A, a2: A): P[A] = {
      |    final class F extends P[A] { def seed: A = id(a1); def m(a1: A): A = ??? }
      |    new F
      |  }
      |}
      |final class G[A](a1: A) {
      |  final class F(a: A) extends P[A] { def seed: A = a1; def m(a1: A): A = ??? }
      |}
      |final class H[A](a1: A) { object F extends P[A] { def seed: A = a1; def m(a1: A): A = ??? } }
      |""".stripMargin)
    (found("M2.mk.F.m") === Count.Finite(3))
      .and(found("M2.mo.F.m") === Count.Finite(3))
      .and(refused(found("M2.mi.F.m"), "final implementation reads a hidden capture: a1"))
      .and(found("G.F.m") === Count.Finite(3))
      .and(found("H.F.m") === Count.Finite(2))
  }

  // `OvCap`: conv: A => B and conv: B => A — conv(a), conv(conv(conv(a))), … ω (the second
  // overload needs `@targetName`, since the two erase alike). `Ov1`: only conv: A => B reaches B;
  // `conv(u: Unit): Unit` adds nothing — 1. Keyed by name, the last overload replaced the first.
  def overloads = {
    val found = counts("Over.scala" -> """import scala.annotation.targetName
      |trait OvCap[A, B] {
      |  def conv(a: A): B
      |  @targetName("convBack") def conv(b: B): A
      |  def go(a: A): B = ???
      |}
      |trait Ov1[A, B] { def conv(a: A): B; def conv(u: Unit): Unit; def go(a: A): B = ??? }
      |""".stripMargin)
    (found("OvCap.go") === Count.Countable).and(found("Ov1.go") === Count.Finite(1))
  }

  // A parent's conv: A => B and the child's own conv: B => A are both members: ω, as above.
  def crossLevelOverload =
    count(
      """import scala.annotation.targetName
        |trait PC[A, B] { def conv(a: A): B }
        |trait QD[A, B] extends PC[A, B] {
        |  @targetName("convBack") def conv(b: B): A
        |  def go(a: A): B = ???
        |}""".stripMargin,
      "QD.go"
    ) === Count.Countable

  // A child restating its parent's abstract member declares the same slot: `get` once, 1 — not
  // two values of A. With the binder renamed (`P2[X]`), a parameterless member still matches.
  def restated = {
    val found = counts("Restated.scala" -> """trait P[A] { def get: A }
      |trait Q[A] extends P[A] { def get: A; def other: A = ??? }
      |trait P2[X] { def get: X }
      |trait Q2[A] extends P2[A] { def get: A; def other: A = ??? }""".stripMargin)
    (found("Q.other") === Count.Finite(1)).and(found("Q2.other") === Count.Finite(1))
  }

  // `f(x: X)` restated as `f(x: A)` under `PO[A]`: both parameters are QO's binder A, so one member
  // (it was refused by spelling) — go has only a: 1. Through the parameterized alias `Two[A]` the
  // identity is not read: refused (the true count is 1).
  def overloadIdentity = {
    val found = counts("Restate.scala" -> """trait PO[X] { def f(x: X): Unit }
      |trait QO[A] extends PO[A] { def f(x: A): Unit; def go(a: A): A = ??? }
      |type Two[X] = (X, X)
      |trait PT[A] { def f(x: Two[A]): Unit }
      |trait QT[A] extends PT[A] { def f(x: Two[A]): Unit; def go(a: A): A = ??? }
      |""".stripMargin)
    (found("QO.go") === Count.Finite(1))
      .and(refused(found("QT.go"), "overloaded declaration identity not resolved: f"))
  }

  // `A.P` and `B.P` are two classes of one shape (3.9.0: implementing only one is E231, `a.P does
  // not match b.P`), so f_A and f_B are both capabilities: f_A(P(true)), f_A(P(false)), f_B(…) — 4
  // (merged as a restatement: 2). Two aliases of `Boolean` are one type: one member — 2.
  def sameShapeOverloads = {
    val found = counts(
      "Shapes.scala" -> """object A { final case class P(b: Boolean); trait Base[R] { def f(p: P): R } }
      |object B { final case class P(b: Boolean); trait Mid[R] extends A.Base[R] { def f(p: P): R } }
      |trait Use[R] extends B.Mid[R] { def g(): R = ??? }
      |object A2 { type P = Boolean; trait Base[R] { def f(p: P): R } }
      |object B2 { type P = Boolean; trait Mid[R] extends A2.Base[R] { def f(p: P): R } }
      |trait Use2[R] extends B2.Mid[R] { def g(): R = ??? }
      |""".stripMargin
    )
    (found("Use.g") === Count.Finite(4)).and(found("Use2.g") === Count.Finite(2))
  }

  // A covariant result refinement with equal parameters is an override (3.9.0 compiles it): the
  // template's own `get(a: A): Option[Nothing]` is the member, not an overload identity question;
  // what remains is the solver's limit on eliminating it (true count 1: None). Met only between
  // two inherited levels, which is more derived is not read: refused.
  def refinedResult = {
    val found = counts("Refined.scala" -> """trait P3[A] { def get(a: A): Option[A] }
      |trait Q3[A] extends P3[A] { def get(a: A): Option[Nothing]; def other(a: A): Option[A] = ??? }
      |trait R3[A] extends Q3[A] { def again(a: A): Option[A] = ??? }
      |""".stripMargin)
    (found("Q3.other") === Count.Unresolved(
      List("Elimination of an opaque sum-producing callable is unsupported")
    )).and(refused(found("R3.again"), "refined inherited declaration not resolved: get"))
  }

  // `Outer.this.value` and `Inner.this.value` are two receivers' members: 2.
  def enclosingMember =
    count(
      "trait Outer[A] { def value: A; trait Inner { def value: A; def go: A = ??? } }",
      "Outer.Inner.go"
    ) === Count.Finite(2)

  // Inside `C`'s body, `X` is the inherited `Boolean`, not the package's `Unit`: g has 2, h 4.
  // Through two levels, `P = Option[(Z, Z)]`: None or Some((z, z)) — 2. An inherited case class
  // member `Y(b: Boolean)` has 2 values.
  def inheritedAlias = {
    val found = counts("W1.scala" -> """package w
      |type X = Unit
      |trait HasX { type X = Boolean }
      |final class C extends HasX { def g(): X = ???; def h(x: X): X = ??? }
      |trait HasP[A] { type P = Option[A] }
      |trait Mid[B] extends HasP[(B, B)]
      |final class D[Z] extends Mid[Z] { def g(z: Z): P = ??? }
      |trait HasY { final case class Y(b: Boolean) }
      |final class F extends HasY { def g(): Y = ??? }""".stripMargin)
    (found("w.C.g") === Count.Finite(2))
      .and(found("w.C.h") === Count.Finite(4))
      .and(found("w.D.g") === Count.Finite(2))
      .and(found("w.F.g") === Count.Finite(2))
  }

  // A local alias and a method's own binder are nearer than the template's members; a private
  // member is not inherited at all, so the package's `Unit` stays the meaning. Each: 1.
  def inheritedAliasPrecedence = {
    val found = counts("W1.scala" -> """package w
      |type X = Unit
      |trait HasX { type X = Boolean }
      |final class H extends HasX { def k(): Unit = { type X = Unit; def g(): X = ??? } }
      |final class Q extends HasX { def g[X](x: X): X = ??? }
      |trait Priv { private type X = Boolean }
      |final class R extends Priv { def g(): X = ??? }""".stripMargin)
    (found("w.H.k.g") === Count.Finite(1))
      .and(found("w.Q.g") === Count.Finite(1))
      .and(found("w.R.g") === Count.Finite(1))
  }

  // `lib.Base` is not supplied and may declare `X` (3.9.0: with `trait Base { type X = Boolean }`,
  // `def g(): X = true` compiles beside the package's `type X = Unit`), so `X` is refused rather
  // than read as `Unit`; so are the builtin `Boolean` and a name `import q.Y` binds, which a member
  // `Boolean` or `Y` would shadow just as well. A declaration of an enclosing object keeps its
  // meaning — an inherited one there would be ambiguous (E049): Box(true), Box(false) — 2. A
  // marker parent declares no type member: 2. But `import lib.Serializable` binds that name to an
  // unsupplied type, not the marker (3.9.0: with `trait Serializable { type X = Boolean }` there,
  // `def g(): X = true` compiles), so it is refused like any unsupplied parent.
  def unsuppliedParentName = {
    val found = counts(
      "Unread.scala" -> """package w
        |import lib.Base
        |type X = Unit
        |final class C extends Base { def g(): X = ???; def b(): Boolean = ??? }
        |object Holder {
        |  final case class Box(b: Boolean)
        |  final class Impl extends Base { def g(): Box = ??? }
        |}
        |final class I extends Base { import q.Y; def g(): Y = ??? }""".stripMargin,
      "q.scala" -> "package q\ntype Y = Unit",
      "Marker.scala" -> """package v
        |import lib.Serializable
        |type X = Unit
        |final class S extends Serializable { def g(): X = ??? }""".stripMargin
    )
    refused(found("w.C.g"), "inherited type member not resolved: X (unsupplied parent Base)")
      .and(
        refused(found("w.C.b"), "inherited type member not resolved: Boolean (unsupplied parent")
      )
      .and(
        refused(found("w.I.g"), "inherited type member not resolved: Y (unsupplied parent Base)")
      )
      .and(found("w.Holder.Impl.g") === Count.Finite(2))
      .and(
        count("final class M extends Serializable { def g(): Boolean = ??? }", "M.g") ===
          Count.Finite(2)
      )
      .and(
        refused(
          found("v.S.g"),
          "inherited type member not resolved: X (unsupplied parent Serializable)"
        )
      )
  }

  // `G.get` implements `HasO.get`, but its `X` is the inherited member, which override keying does
  // not follow: the identity stays open (the true count is 2) rather than keyed through the
  // package's `Unit`. In the final twin `E` the declaration is implemented by `get` itself, so no
  // identity is asked: true, false — 2.
  def inheritedOverride = {
    val found = counts("Over.scala" -> """package w
      |type X = Unit
      |trait HasO { type X = Boolean; def get(): X }
      |class G extends HasO { def get(): X = ??? }
      |final class E extends HasO { def get(): X = ??? }""".stripMargin)
    refused(found("w.G.get"), "inherited override identity not resolved: get")
      .and(found("w.E.get") === Count.Finite(2))
  }

  // A `var` parameter is a field the instance reassigns: like the `var` member twin, a mutable
  // capture. The constructor still chooses its two fields from a and b: 4.
  def varParameter = {
    val found = counts("Var.scala" -> """class VarParam[A](var a: A, b: A) { def get: A = ??? }
      |class VarField[A](b: A) { var a: A = b; def get: A = ??? }""".stripMargin)
    refused(found("VarParam.get"), "mutable capture")
      .and(refused(found("VarField.get"), "mutable capture"))
      .and(found("VarParam.<init>") === Count.Finite(4))
  }

  // `self.get` is a value of `A` the reading does not enumerate (the true count is 1). A self
  // alias without a type adds nothing; a constructor's own self type is not yet a `this`.
  def selfType = {
    val found = counts("Self.scala" -> """trait Cap[A] { def get: A }
      |trait SelfT[A] { self: Cap[A] => def again: A = ??? }
      |trait Alias[A] { outer => def same(a: A): A = ??? }
      |class SelfC[A](val a: A) { self: Cap[A] => }""".stripMargin)
    refused(found("SelfT.again"), "self type not resolved: Cap[A]")
      .and(found("Alias.same") === Count.Finite(1))
      .and(found("SelfC.<init>") === Count.Finite(1))
  }

  // `I2: G2[Unit]` is no value of `G2[A]` for an abstract A, so the enum is not the sum of all its
  // cases at every instantiation — as the sealed twin says. `Lc(a: A)` takes its own A (Scala
  // gives the case the enum's parameter its fields mention) and pins the enum's to Int.
  def enumPin = {
    val found = counts("Gadt.scala" -> """enum G2[A] { case I2 extends G2[Unit]; case S2(a: A) }
      |enum L[A] { case Lc(a: A) extends L[Int]; case Ln(a: A) }
      |object Use { def g2[A](a: A): G2[A] = ???; def l[A](a: A): L[A] = ??? }""".stripMargin)
    refused(found("Use.g2"), "sealed subtype leaves a parameter of G2 unpinned: Unit")
      .and(refused(found("Use.l"), "sealed subtype leaves a parameter of L unpinned: Int"))
  }

  // `Sw[X, Y](x: X, y: Y) extends P[Y, X]` is `P[A, B]` with x: B, y: A: only `Sw(b, a)`, 1.
  // `N extends H[Nothing]` is in every covariant `H[A]`: N or V(a), 2. `Kc(a: A) extends K[A]`
  // binds its implicit A: 1.
  def enumBinds = {
    val found = counts("Gadt.scala" -> """enum P[A, B] { case Sw[X, Y](x: X, y: Y) extends P[Y, X] }
      |enum H[+A] { case N extends H[Nothing]; case V(a: A) }
      |enum K[A] { case Kc(a: A) extends K[A] }
      |object Use {
      |  def sw[A, B](a: A, b: B): P[A, B] = ???
      |  def h[A](a: A): H[A] = ???
      |  def k[A](a: A): K[A] = ???
      |}""".stripMargin)
    (found("Use.sw") === Count.Finite(1))
      .and(found("Use.h") === Count.Finite(2))
      .and(found("Use.k") === Count.Finite(1))
  }

  // `K(n: Boolean) extends G[A]` takes `A` through its `extends` clause (3.9.0 compiles
  // `G.K[A](true)`): K(true), K(false) — 2. `Kh(a: A) extends H[A, B]` takes both: Kh(a) — 1.
  def enumBindsByExtends = {
    val found = counts("Extends.scala" -> """enum G[A] { case K(n: Boolean) extends G[A] }
      |enum H[A, B] { case Kh(a: A) extends H[A, B] }
      |object Use { def g[A](): G[A] = ???; def h[A, B](a: A): H[A, B] = ??? }""".stripMargin)
    (found("Use.g") === Count.Finite(2)).and(found("Use.h") === Count.Finite(1))
  }

  // `Arb` is only ever `new Arb[T] {…}`: read as the sum of its named subtypes it was empty (0
  // and 1); `W` has `W1` and an anonymous instance (read as 1). Both refused.
  def sealedAnonymous = {
    val found = counts("Anon.scala" -> """sealed abstract class Arb[T] { def arbitrary: T }
      |object Arb { def apply[T](g: T): Arb[T] = new Arb[T] { def arbitrary: T = g } }
      |object Use { def mk[T](x: T): Arb[T] = ???; def read[T](a: Arb[T]): Option[T] = ??? }
      |sealed trait W
      |case object W1 extends W
      |object Extra { val extra: W = new W {} }
      |object UseW { def makeW(): W = ??? }""".stripMargin)
    val reason = "sealed family has anonymous instances in its file"
    refused(found("Use.mk"), s"$reason: Arb")
      .and(refused(found("Use.read"), s"$reason: Arb"))
      .and(refused(found("UseW.makeW"), s"$reason: W"))
  }

  // Children are registered by the parent name their clause spells: `new SA {}` through `type SA =
  // S` is a value of S no case enumerates (S1 and it: ≥ 2, read as 1), and `R2(b: Boolean)
  // extends RA` a child (R1, R2(true), R2(false): 3, read as 1). Both refused.
  def sealedAlias = {
    val found = counts("Alias.scala" -> """sealed trait S
      |case object S1 extends S
      |type SA = S
      |object Inst { val s: S = new SA {} }
      |object UseS { def mk(): S = ??? }
      |sealed trait R
      |case object R1 extends R
      |type RA = R
      |final case class R2(b: Boolean) extends RA
      |object UseR { def mk(): R = ??? }""".stripMargin)
    refused(found("UseS.mk"), "sealed family is extended through an alias: S")
      .and(refused(found("UseR.mk"), "sealed family is extended through an alias: R"))
  }

  // `o1.In(true)` and `o2.In(true)` are values of different classes; a local `Loc()` is a class
  // per declaration site that no use site can name. Neither family is the sum it was read as.
  def sealedInner = {
    val found = counts("Inner.scala" -> """sealed trait S
      |final case class S1(x: Boolean) extends S
      |class Outer { final case class In(x: Boolean) extends S }
      |object UseS { def makeS(): S = ??? }
      |sealed trait L
      |final case class L1(x: Boolean) extends L
      |object Local { def make(): Unit = { final case class Loc() extends L; () } }
      |object UseL { def makeL(): L = ??? }""".stripMargin)
    refused(found("UseS.makeS"), "sealed subtype is an inner or local class: In")
      .and(refused(found("UseL.makeL"), "sealed subtype is an inner or local class: Loc"))
  }

  // A child declared in a val, lazy val, given, lambda, if arm or top-level val block is a class per
  // evaluation, as in a method body: f can return A1 or the local object, which a match on `case
  // A1` tells apart — at least 2 (each read 1). Refused like the method-body child.
  def sealedBlockChild = {
    val found = counts("Blocks.scala" -> """sealed trait S1
      |case object A1 extends S1
      |object H1 { lazy val v: S1 = { case object Loc1 extends S1; Loc1 } }
      |object U1 { def f(): S1 = ??? }
      |sealed trait S2
      |case object A2 extends S2
      |object H2 { given S2 = { case object Loc2 extends S2; Loc2 } }
      |object U2 { def f(): S2 = ??? }
      |sealed trait S3
      |case object A3 extends S3
      |object H3 { val g: Unit => S3 = _ => { case object Loc3 extends S3; Loc3 } }
      |object U3 { def f(): S3 = ??? }
      |sealed trait S4
      |case object A4 extends S4
      |object H4 { val v: S4 = { final case class Loc4(b: Boolean) extends S4; Loc4(true) } }
      |object U4 { def f(): S4 = ??? }
      |sealed trait S5
      |case object A5 extends S5
      |val top5: S5 = { case object Loc5 extends S5; Loc5 }
      |object U5 { def f(): S5 = ??? }
      |sealed trait S6
      |case object A6 extends S6
      |object H6 { def mk(b: Boolean): S6 = if (b) { case object Loc6 extends S6; Loc6 } else A6 }
      |object U6 { def f(): S6 = ??? }
      |""".stripMargin)
    val reason = "sealed subtype is an inner or local class: Loc"
    refused(found("U1.f"), s"${reason}1")
      .and(refused(found("U2.f"), s"${reason}2"))
      .and(refused(found("U3.f"), s"${reason}3"))
      .and(refused(found("U4.f"), s"${reason}4"))
      .and(refused(found("U5.f"), s"${reason}5"))
      .and(refused(found("U6.f"), s"${reason}6"))
  }

  // Members of an object are one class each: M1(x: Boolean) gives 2, M2 one more — 3.
  def sealedInObject =
    count(
      """sealed trait M
        |object Cases { final case class M1(x: Boolean) extends M; case object M2 extends M }
        |object UseM { def makeM(): M = ??? }""".stripMargin,
      "UseM.makeM"
    ) === Count.Finite(3)

  // An alias of a family that no child or instance extends through changes nothing: S1, S2 — 2
  // through either name. An anonymous instance of an unrelated trait in the family's file is no
  // value of the family: R1, R2 — 2.
  def sealedAliasUnused = {
    val found = counts("Unused.scala" -> """sealed trait S
      |case object S1 extends S
      |case object S2 extends S
      |type SA = S
      |object Use { def mk(): S = ???; def mk2(): SA = ??? }
      |sealed trait R
      |case object R1 extends R
      |case object R2 extends R
      |trait Other
      |object Inst { val o: Other = new Other {} }
      |object UseR { def mk(): R = ??? }""".stripMargin)
    (found("Use.mk") === Count.Finite(2))
      .and(found("Use.mk2") === Count.Finite(2))
      .and(found("UseR.mk") === Count.Finite(2))
  }

}
