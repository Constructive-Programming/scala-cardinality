package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Readings of an environment or a family that used to count values a body cannot reach, or miss
  * ones it can. Every fixture compiles with Scala 3.9.0 (bodies included, where the file gives one
  * here as `???`); each expected number is derived in the comment beside it.
  */
class EnvironmentSoundnessSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Anonymous contextual parameters
      refuse same-typed evidence a search cannot tell apart        $anonymousAmbiguous
      read an extension's clauses at its method's level            $anonymousExtension
      refuse evidence a later given of the block shadows           $anonymousLaterGiven
      leave a shallower given out of the question                  $anonymousOuterGiven
      refuse evidence a named import or an export may beat         $anonymousNamedImports
      refuse an inherited given as an overlap, not an ambiguity    $anonymousInherited
      refuse evidence of a type related to another's               $anonymousRelated
      count evidence of distinct final classes that share a shape   $anonymousSameShape
      count named evidence of one type by name                     $namedEvidence
    Inherited declarations
      substitute through two levels of inheritance                 $twoLevels
      read a diamond's shared parent once                          $diamond
      keep an unrelated instantiation empty                        $unrelatedInstantiation
      refuse a parent reached with two instantiations              $conflictingInstantiations
      read a parent that is an inherited member class              $memberClassParent
      read an inherited declaration a final template implements    $finalImplementation
      leave an abstract final class's declarations open            $abstractFinal
      keep a capture a final implementation reads past a shadow    $hiddenCapture
      keep abstract overloads as separate capabilities             $overloads
      keep an overload across inheritance levels                   $crossLevelOverload
      read a restated declaration as one member                    $restated
      keep same-shaped parameter classes of two scopes apart       $sameShapeOverloads
      keep a lexically enclosing member apart from an inner one    $enclosingMember
      read a type alias the template inherits before an outer one  $inheritedAlias
      keep nearer declarations and private members as they were    $inheritedAliasPrecedence
      refuse a name an unsupplied parent may declare               $unsuppliedParentName
      key an override through an inherited name as the member $inheritedOverride
    Templates and families
      read a `var` as the value it holds                           $varParameter
      refuse an enum case pinned to one instantiation              $enumPin
      read enum cases that bind or bottom-pin the parameters       $enumBinds
      read enum cases that take parameters through `extends`       $enumBindsByExtends
      refuse a sealed family extended through an alias             $sealedAlias
      refuse a sealed family with an inner or local child          $sealedInner
      refuse a sealed family with a child in any block             $sealedBlockChild
      read a sealed family whose children live in an object        $sealedInObject
      read a family an alias names but nothing extends through it  $sealedAliasUnused
  """

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
  // unrelated binders on either side, are not related.
  def anonymousRelated =
    refused(
      count("object T { def f[A, B](x: A)(using A => B, Nothing => B): B = ??? }", "T.f"),
      "anonymous evidence selection not resolved: Nothing => B"
    )

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

  // `G.get` implements `HasO.get`: both signatures key `X` as the receiver's member (members M2),
  // the inherited alias Boolean, never the package's `Unit`, so the identity is found: true, false
  // — 2. In the final twin `E` the declaration is implemented by `get` itself: 2.
  def inheritedOverride = {
    val found = counts("Over.scala" -> """package w
      |type X = Unit
      |trait HasO { type X = Boolean; def get(): X }
      |class G extends HasO { def get(): X = ??? }
      |final class E extends HasO { def get(): X = ??? }""".stripMargin)
    (found("w.G.get") === Count.Finite(2)).and(found("w.E.get") === Count.Finite(2))
  }

  // The model excludes mutation, so a `var` holds the value it was given: the parameter `a` is the
  // caller's value beside `b` (2), and the field `a` is `b` (1). No subclass overrides a `var`
  // (E164), so the open class does not leave it a capability. The constructor still chooses its
  // two fields from a and b: 4.
  def varParameter = {
    val found = counts("Var.scala" -> """class VarParam[A](var a: A, b: A) { def get: A = ??? }
      |class VarField[A](b: A) { var a: A = b; def get: A = ??? }""".stripMargin)
    (found("VarParam.get") === Count.Finite(2))
      .and(found("VarField.get") === Count.Finite(1))
      .and(found("VarParam.<init>") === Count.Finite(4))
  }

  // `I2: G2[Unit]` is no value of `G2[A]` for an abstract A, so the enum is not the sum of all its
  // cases at every instantiation — as the sealed twin says. `Lc(a: A)` takes its own A (Scala
  // gives the case the enum's parameter its fields mention) and pins the enum's to Int.
  def enumPin = {
    val found = counts("Gadt.scala" -> """enum G2[A] { case I2 extends G2[Unit]; case S2(a: A) }
      |enum L[A] { case Lc(a: A) extends L[Int]; case Ln(a: A) }
      |object Use { def g2[A](a: A): G2[A] = ???; def l[A](a: A): L[A] = ??? }""".stripMargin)
    refused(
      found("Use.g2"),
      "sealed subtype leaves a parameter of its parent unpinned: I2 (Unit, of G2)"
    )
      .and(
        refused(
          found("Use.l"),
          "sealed subtype leaves a parameter of its parent unpinned: Lc (Int, of L)"
        )
      )
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
