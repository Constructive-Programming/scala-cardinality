package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** The no-side-effects assumption (register D26): a `var` reads as a `val`, a throw supplies
  * nothing, and a throwable is data. Every fixture is valid Scala 3.9.0 (compiled with scalac)
  * except the singleton type of a `var`, which Scala rejects (E083) — the refusal under test — and
  * the two that import from `foo`, a package deliberately not supplied (`Blocked`, `W`). Each
  * expected number is derived in the comment beside it. A fixture about an overridable member's
  * default is read in the open world (`openRows`), where the member is open as one a supplied
  * template overrides is in the closed world.
  */
class PureCodeSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    A var is a val
      a var field of an open or final class is a field             $varFields
      a private var field stays hidden state                        $privateVarField
      an abstract var is a record member, a concrete one a value    $varMembers
      var fields of a sealed family or case class are its product   $varProducts
      a private var field of a sealed subtype stays hidden          $privateVarProduct
      a plain parameter of a sealed leaf is no field                $plainLeafParameter
      a var is no stable path, by Scala's typing rule               $varPath
    A throw supplies nothing
      a right-hand side that only throws is a Nothing               $inferredNothing
      a throwing branch joins nothing to the other branch           $throwingBranch
      a rebound ??? or sys is no throw                              $reboundNames
      a module helper that only throws inhabits no type             $throwingHelper
      an open member that only throws stays an open capability      $throwingOpenMember
      one of an empty type reads as its abstract twin: vacuous      $unimplementable
      at the record's or receiver's type arguments                  $appliedTwins
      a subclass's instance is its parent's: both vacuous           $subclassTwins
      a bound may supply a binder: such a member is kept            $boundedBinder
      a member without a result type that may override is kept      $overridingResult
      an empty type is read from its structure                      $emptyTypes
      a constructor that only throws builds no instance             $construction
      an alias nested out of reach does not stand for a parent      $constructionReach
      a fixed member that only throws is concrete code              $fixedThrowing
      top-level vals are built with their file's definitions        $topLevel
      a member object carries the instance it belongs to            $memberObjects
      a level's body binds names in its own scope                   $bindingScope
      a sealed case or enum built by a throwing body is refused      $sealedConstruction
      a template an object encloses does not build the object       $staticNesting
      an empty block is no throw                                    $emptyBlock
      a throw this reading does not recognize stays unresolved      $throwLimits
      its abstract twin still makes the receiver vacuous            $abstractTwin
      a final class's throwing member is its concrete code          $finalThrowing
      a record keeps a member no override can implement             $throwingRecord
      a throw's operand reads no hidden capture                     $throwOperand
    A throwable is data
      a throwable is built from any message                         $throwableGoals
      a supplied throwable is transparent data                      $throwableInputs
      a throwable irrelevant to the goal changes nothing            $irrelevantThrowable
      Throwable is its own rigid-constructor key                    $throwableKeys
      an open subclass's overridable members are reasons            $openSubclass
      a final subclass is the record of its parameters              $finalSubclass
      a source class named Throwable shadows the model              $shadowedThrowable
      an unsupplied wildcard import refuses the name                $wildcardThrowable
      a union with Exception has no runtime class to test           $exceptionUnion
  """

  private def each(found: Map[String, Count], expected: Count)(names: String*) =
    names.map(name => found(name) === expected).reduce(_ and _)

  // A Boolean input observed through one Boolean field: x, !x, true, false — 4; built from b, a
  // field is any function of b — 4. A public `var` parameter is a field holding its argument, as a
  // `val` one is (it read "open class with hidden constructor state" / "final class field not
  // observable injectively").
  def varFields = {
    val found = rows("""class RVP(var x: Boolean)
      |final class FVP(var x: Boolean)
      |object Use {
      |  def open(v: RVP): Boolean = ???
      |  def openGoal(b: Boolean): RVP = ???
      |  def fixed(v: FVP): Boolean = ???
      |  def fixedGoal(b: Boolean): FVP = ???
      |}""".stripMargin)
    (found("Use.open") === Count.Finite(4))
      .and(found("Use.openGoal") === Count.Finite(4))
      .and(found("Use.fixed") === Count.Finite(4))
      .and(found("Use.fixedGoal") === Count.Finite(4))
  }

  // Privacy, not mutation, hides the field: no member observes it, so a goal could not be told
  // apart by it while an input still holds it — refused, never 2 or 4. In the closed world RVH is
  // final, and x a field no member observes: refused all the same.
  def privateVarField = {
    val code = "class RVH(private var x: Boolean)\nobject Use { def f(v: RVH): Boolean = ??? }"
    refused(openRows(code)("Use.f"), "open class with hidden constructor state: RVH.x")
      .and(refused(rows(code)("Use.f"), "final class field not observable injectively: RVH.x"))
  }

  // RAV's record is its getter y (the setter returns Unit, an effect): 4 as input and goal (an
  // anonymous `new RAV { var y = … }` realizes any choice; it read "capability record with a
  // mutable member not read"). RCV's concrete `var z` can never be overridden (E164), so it is
  // derived, not a free Boolean: the record is (w) — 4, not 16.
  def varMembers = {
    val found = rows("""trait RAV { var y: Boolean }
      |trait RCV { var z: Boolean = true; def w: Boolean }
      |object Use {
      |  def abstractVar(v: RAV): Boolean = ???
      |  def abstractGoal(b: Boolean): RAV = ???
      |  def concreteVar(v: RCV): Boolean = ???
      |}""".stripMargin)
    (found("Use.abstractVar") === Count.Finite(4))
      .and(found("Use.abstractGoal") === Count.Finite(4))
      .and(found("Use.concreteVar") === Count.Finite(4))
  }

  // |S| = |S1| + |S2| + |S3| = 2 + 2 + 1 = 5, so S => Boolean has 2^5 = 32 (it read "sealed subtype
  // hides a parameter: S1"); CC(var x) is the product (x): 4 each way (it read "abstract type or
  // method-valued representation").
  def varProducts = {
    val found = rows("""sealed trait S
      |final case class S1(var x: Boolean) extends S
      |final class S2(var y: Boolean) extends S
      |case object S3 extends S
      |case class CC(var x: Boolean)
      |object Use {
      |  def family(s: S): Boolean = ???
      |  def caseVar(c: CC): Boolean = ???
      |  def caseGoal(b: Boolean): CC = ???
      |}""".stripMargin)
    (found("Use.family") === Count.Finite(32))
      .and(found("Use.caseVar") === Count.Finite(4))
      .and(found("Use.caseGoal") === Count.Finite(4))
  }

  def privateVarProduct =
    refused(
      rows(
        "sealed trait H\nfinal case class H1(private var x: Boolean) extends H\n" +
          "object Use { def f(h: H): Boolean = ??? }"
      )("Use.f"),
      "sealed subtype hides a parameter: H1"
    )

  // H2's `code` is no member (E173: only H2's own code reads it), so new H2(true) and new H2(false)
  // may be one value: S has 2 or 3 and f 4 or 8 — refused, never 8. A `var` parameter is a field:
  // T = J2(code) + J3, 3 values, g = 2^3 = 8.
  def plainLeafParameter = {
    val found = rows("""sealed trait S
      |final class H2(code: Boolean) extends S
      |case object H3 extends S
      |sealed trait T
      |final class J2(var code: Boolean) extends T
      |case object J3 extends T
      |object U { def f(s: S): Boolean = ???; def g(t: T): Boolean = ??? }""".stripMargin)
    refused(found("U.f"), "sealed subtype hides a parameter: H2")
      .and(found("U.g") === Count.Finite(8))
  }

  // Not valid Scala (E083, "not an immutable path"): the refusal names the typing rule.
  def varPath = {
    val found = rows(
      "class P(var p: String) { def t: p.type = ??? }\n" +
        "object Q { var q: String = \"\"; def t: q.type = ??? }"
    )
    refused(found("P.t"), "not a stable path: p is a var")
      .and(refused(found("Q.t"), "not a stable path: q is a var"))
  }

  // Scala types `throw e`, `???` and `sys.error(…)` as Nothing, and a block, match or conditional
  // whose every result throws as well: no implementation of Nothing exists — 0 (each read "inferred
  // result type not resolved").
  def inferredNothing = {
    val found = rows("""object Inferred {
      |  def boom = throw new Exception("x")
      |  def q = ???
      |  def s = sys.error("x")
      |  def full = scala.sys.error("x")
      |  def block = { val n = 1; throw new Exception(n.toString) }
      |  def matched(x: Int) = x match { case 0 => ???; case _ => sys.error("e") }
      |  def predef = Predef.???
      |  def scalaPredef = scala.Predef.???
      |  def rooted = _root_.scala.sys.error("x")
      |}""".stripMargin)
    List("boom", "q", "s", "full", "block", "matched", "predef", "scalaPredef", "rooted")
      .map(name => found(s"Inferred.$name") === Count.Finite(0))
      .reduce(_ and _)
  }

  // `if (c) throw … else true` is a Boolean: Boolean => Boolean, 4; `if (c) 1 else throw …` an Int:
  // (2^32)^2 = 2^64 (both read "conditional").
  def throwingBranch = {
    val found = rows("""object Inferred {
      |  def thenThrow(c: Boolean) = if (c) throw new Exception("no") else true
      |  def elseThrow(c: Boolean) = if (c) 1 else throw new Exception("no")
      |}""".stripMargin)
    (found("Inferred.thenThrow") === Count.Finite(4))
      .and(found("Inferred.elseThrow").render === "2^64")
  }

  // A source `???` is the method called: an Int, 2^32 — never 0. A source `sys` object makes
  // `sys.error` its member's call, which this reading does not type — never 0; so does a wildcard
  // import inside the block, which may bind `???` (`foo` is not supplied), and a `???` the block
  // defines before its result.
  def reboundNames = {
    val found = rows("""object Shadowed { def ??? : Int = 3; def f = ??? }
      |object Rebound { object sys { def error(s: String): Int = 1 }; def f = sys.error("e") }
      |object Blocked { def f = { import foo.*; ??? } }
      |object Local { def f = { def ??? : Int = 1; ??? } }
      |""".stripMargin)
    (found("Shadowed.f").render === "2^32")
      .and(refused(found("Rebound.f"), "inferred result type not resolved: application"))
      .and(refused(found("Blocked.f"), "inferred result type not resolved: block"))
      .and(refused(found("Local.f"), "inferred result type not resolved: block"))
  }

  // `any[A]` has no pure total implementation; read as a producer it would inhabit every B. It is
  // concrete code that only throws: f[B]: B has none — 0, imported or not, never 1.
  def throwingHelper = {
    val found = rows("""object Helpers { def any[A]: A = throw new Exception("x") }
      |object UseAny { def f[B]: B = ??? }
      |object UseImported { import Helpers.*; def f[B]: B = ??? }""".stripMargin)
    (found("UseAny.f") === Count.Finite(0)).and(found("UseImported.f") === Count.Finite(0))
  }

  // A subclass may override `get` with any Boolean: m ranges over functions of (b, get), 2^4 = 16,
  // as for the abstract twin — reading the throwing default as absent would give 4. In the closed
  // world nothing overrides it: OpenGet is final, get supplies nothing, m is b's functions — 4.
  def throwingOpenMember = {
    val found = openRows(
      """class OpenGet { def get: Boolean = ???; def m(b: Boolean): Boolean = ??? }
      |abstract class AbsGet { def get: Boolean; def m(b: Boolean): Boolean }""".stripMargin
    )
    (found("OpenGet.m") === Count.Finite(16))
      .and(found("AbsGet.m") === Count.Finite(16))
      .and(
        rows("class OpenGet { def get: Boolean = ???; def m(b: Boolean): Boolean = ??? }")(
          "OpenGet.m"
        ) === Count.Finite(4)
      )
  }

  // A default that only throws is no behavior, so the member reads as its abstract twin: every
  // instance, `new OpenBoom` included, must give `boom: Nothing` a behavior, and no pure code can —
  // no instance, m vacuous: 1, as AbsBoom.m. Reading the default instance as a value instead (m =
  // 4) would make OpenGet.m 4 too: its `get` throws on that instance. An open member's inferred
  // result is not read. `fail[A](msg: String): A` is ∀A. String => A, which register D24 reads with
  // no generator as String => Nothing: empty, so OpenFail, like its abstract twin AbsFail, has no
  // instance — 1 each, never 4.
  def unimplementable = {
    val found = openRows("""class OpenBoom {
      |  def boom: Nothing = throw new Exception("x")
      |  def m(b: Boolean): Boolean = ???
      |}
      |class OpenBoomInferred { def boom = sys.error("x"); def m(b: Boolean): Boolean = ??? }
      |class K1 { def k(x: Int): Nothing = ???; def m(b: Boolean): Boolean = ??? }
      |class OpenFail {
      |  def fail[A](msg: String): A = throw new IllegalStateException(msg)
      |  def m(b: Boolean): Boolean = ???
      |}
      |abstract class AbsFail { def fail[A](msg: String): A; def m(b: Boolean): Boolean }
      |""".stripMargin)
    each(found, Count.Finite(1))("OpenBoom.m", "K1.m", "OpenFail.m", "AbsFail.m")
      .and(refused(found("OpenBoomInferred.m"), "overridable member's result type is inferred"))
  }

  // K2[Int] is K1 after A := Int, and K4 extends it: their k is Int => Nothing, vacuous — 1 like
  // K1, never 4 for one and 1 for the other. With a: A, K5's and v3's A => Nothing is empty: 1.
  // Gen[Boolean] is Mono: no value (0 as a goal, 1 as an input); a Boolean-observing Mono has none
  // either.
  def appliedTwins = {
    val found = openRows(
      """class K1 { def k(x: Int): Nothing = ???; def m(b: Boolean): Boolean = ??? }
      |class K2[A] { def k(x: A): Nothing = ??? }
      |class K4 extends K2[Int] { def m(b: Boolean): Boolean = ??? }
      |class K5[A](val a: A) { def k(x: A): Nothing = ???; def m(b: Boolean): Boolean = ??? }
      |class Mono { def k: Boolean => Nothing = ???; def m(a: Boolean): Boolean = ??? }
      |class Gen[A] { def k: A => Nothing = ???; def m(a: A): Boolean = ??? }
      |object V {
      |  def v1(k: K1, b: Boolean): Boolean = ???
      |  def v2(k: K2[Int], b: Boolean): Boolean = ???
      |  def v3[A](k: K2[A], a: A, b: Boolean): Boolean = ???
      |  def v4(k: K4, b: Boolean): Boolean = ???
      |  def v5(k: K5[Int], b: Boolean): Boolean = ???
      |  def mkMono(b: Boolean): Mono = ???
      |  def mkGen(b: Boolean): Gen[Boolean] = ???
      |  def useMono(g: Mono, b: Boolean): Boolean = ???
      |  def useGen(g: Gen[Boolean], b: Boolean): Boolean = ???
      |}""".stripMargin
    )
    each(found, Count.Finite(1))(
      "K1.m",
      "K4.m",
      "K5.m",
      "Gen.m",
      "V.v1",
      "V.v2",
      "V.v3",
      "V.v4",
      "V.v5",
      "V.useMono",
      "V.useGen"
    ).and(each(found, Count.Finite(0))("V.mkMono", "V.mkGen"))
  }

  // Every Q is a P. Q's throwing k reads as P's abstract one, so neither has an instance: 1 each,
  // never 1 for P and 4 for Q. K3's throwing default reads as K3a's abstract k: both are the same
  // polymorphic capability ∀A. A => Nothing, empty at A = Unit (register D24), so neither has an
  // instance: 1 for both, never 2^(2^33) for K3.
  def subclassTwins = {
    val found = openRows("""abstract class P { def k(x: Int): Nothing }
      |class Q extends P { def k(x: Int): Nothing = ??? }
      |class K3 { def k[A](x: A): Nothing = ???; def use(a: Int, b: Boolean): Boolean = ??? }
      |abstract class K3a { def k[A](x: A): Nothing; def use(a: Int, b: Boolean): Boolean }
      |object U { def viaP(p: P, b: Boolean): Boolean = ???; def viaQ(q: Q, b: Boolean): Boolean = ??? }
      |""".stripMargin)
    each(found, Count.Finite(1))("U.viaP", "U.viaQ", "K3.use", "K3a.use")
  }

  // `zero[A >: Int]` has an override (`1`), and `unit[A: Ordering]` may read its evidence. `pick[A,
  // B <: A](x: B, y: B): A` returns x or y (B is an A), `first[A, B <: (A, A)](b: B): A` b._1 or
  // b._2, and `pick[A, B >: A <: A]` x or y: each is a Boolean an override chooses, so Use.g ranges
  // over all 2^(2·2) = 16 functions of (pick, a). None is dropped, so each keeps the bounded binder's
  // reason — never 4. `fail[A](a: A): A` is ∀A. A => A, the identity (register D24): kept, never
  // dropped as empty, and it builds no Boolean, so Mention.m is the 4 functions of b, as its abstract
  // twin AMention.m — never 1.
  def boundedBinder = {
    val found = openRows(
      """class Lower { def zero[A >: Int]: A = ???; def m(b: Boolean): Boolean = ??? }
      |class Evidence { def unit[A: Ordering]: A = ???; def m(b: Boolean): Boolean = ??? }
      |class Pick { def pick[A, B <: A](x: B, y: B): A = ??? }
      |class Pair2 { def first[A, B <: (A, A)](b: B): A = ??? }
      |class EqB { def pick[A, B >: A <: A](x: B, y: B): A = ??? }
      |class RecvPick { def pick[A, B <: A](x: B, y: B): A = ???; def m(a: Boolean): Boolean = ??? }
      |class Mention { def fail[A](a: A): A = throw new Exception("x"); def m(b: Boolean): Boolean = ??? }
      |abstract class AMention { def fail[A](a: A): A; def m(b: Boolean): Boolean }
      |object Use {
      |  def g(p: Pick, a: Boolean): Boolean = ???
      |  def h(p: Pair2, a: Boolean): Boolean = ???
      |  def eqb(p: EqB, a: Boolean): Boolean = ???
      |}""".stripMargin
    )
    refused(found("Lower.m"), "polymorphic capability binder")
      .and(refused(found("Evidence.m"), "polymorphic capability binder"))
      .and(refused(found("Use.g"), "polymorphic capability binder: B"))
      .and(refused(found("Use.h"), "polymorphic capability binder: B"))
      .and(refused(found("Use.eqb"), "polymorphic capability binder: B"))
      .and(refused(found("RecvPick.m"), "polymorphic capability binder: B"))
      .and(each(found, Count.Finite(4))("Mention.m", "AMention.m"))
  }

  // Scala gives `def get = ???` overriding `get: Boolean` the type Boolean (a 3.9.0 run overrides
  // it with true and with false), so an override may implement it: f is 4 and Recv.m 16, never the
  // 2 and 4 of a dropped member. This reading does not type an open member's inferred result:
  // refused.
  def overridingResult = {
    val found = openRows("""abstract class Base { def get: Boolean }
      |class Impl extends Base { def get = ??? }
      |trait TBase { def get: Boolean }
      |class TImpl extends TBase { def get = sys.error("no") }
      |class Recv extends Base { def get = ???; def m(b: Boolean): Boolean = ??? }
      |class BC { def get: Boolean = true }
      |class IC extends BC { override def get = ??? }
      |object Use {
      |  def f(i: Impl): Boolean = ???
      |  def ft(i: TImpl): Boolean = ???
      |  def ic(p: IC): Boolean = ???
      |}""".stripMargin)
    List("Use.f", "Use.ft", "Use.ic", "Recv.m")
      .map(name => refused(found(name), "overridable member's result type is inferred"))
      .reduce(_ and _)
  }

  // Int => Nothing, (Nothing, Int) and Either[Nothing, Nothing] have no value: as abstract twins
  // they leave no instance, so m and u1, u3 are vacuous — 1. Nothing => Nothing has the identity:
  // H7's f is a capability that builds no Boolean, m the 4 functions of b. The abstract twins
  // (`AH1`, `AH5`, `AH6`, `AH7`) count the same. Register D24 reads the polymorphic ones:
  // ∀A. Int => (A, Int) is Int => (Nothing, Int), empty, so H5.m is vacuous — 1; ∀A. Option[A] is
  // None alone, which builds no Boolean, so H6.m is the 4 functions of b.
  def emptyTypes = {
    val found = openRows(
      """class H1 { def handler: Int => Nothing = ???; def m(b: Boolean): Boolean = ??? }
      |class H2 { def pair: (Nothing, Int) = ???; def m(b: Boolean): Boolean = ??? }
      |class H3 { def e: Either[Nothing, Nothing] = ???; def m(b: Boolean): Boolean = ??? }
      |class H5 { def poly[A](x: Int): (A, Int) = ???; def m(b: Boolean): Boolean = ??? }
      |class H6 { def poly[A]: Option[A] = ???; def m(b: Boolean): Boolean = ??? }
      |class H7 { def f(n: Nothing): Nothing = ???; def m(b: Boolean): Boolean = ??? }
      |abstract class AH1 { def handler: Int => Nothing; def m(b: Boolean): Boolean }
      |abstract class AH5 { def poly[A](x: Int): (A, Int); def m(b: Boolean): Boolean }
      |abstract class AH6 { def poly[A]: Option[A]; def m(b: Boolean): Boolean }
      |abstract class AH7 { def f(n: Nothing): Nothing; def m(b: Boolean): Boolean }
      |object Use { def u1(h: H1, b: Boolean): Boolean = ???; def u3(h: H3, b: Boolean): Boolean = ??? }
      |""".stripMargin
    )
    each(found, Count.Finite(1))(
      "H1.m",
      "H2.m",
      "H3.m",
      "H5.m",
      "AH1.m",
      "AH5.m",
      "Use.u1",
      "Use.u3"
    )
      .and(each(found, Count.Finite(4))("H6.m", "AH6.m", "H7.m", "AH7.m"))
  }

  // A strict `val` or `var` initializer, a statement and a parent's argument run in every
  // constructor, a subclass's included (3.9.0 runs: `new IV { override val x = true }` throws).
  // A template one of them only throws for is never built: as an input vacuous (1), as a goal
  // empty (0), and its members are never called (1). A lazy val or a method runs only when
  // called: LK2 and DK2 are the record (boom, m) of two Booleans, 2^4 = 16. A member named `???`
  // is no throw: Mem.m ranges over functions of (b, x, ???), never 1.
  // The construction check reads a parent by its simple name, so an alias of that name may stand
  // for any type - but only an alias a bare name can denote there: one nested in an object of
  // another file is named only through that object (an import of it, or an export).
  def constructionReach = {
    val family = "trait Base\nsealed trait S extends Base\ncase object S1 extends S\n" +
      "object U { def s(x: S): Boolean = ??? }"
    val compat = "lib.scala" -> "package nlib\nobject Compat { type Base = Any }"
    val apart = counts(compat, "a.scala" -> s"package na\n$family")
    val imported = counts(compat, "a.scala" -> s"package na\nimport nlib.Compat.*\n$family")
    (apart("na.U.s") === Count.Finite(2))
      .and(refused(imported("na.U.s"), "sealed family construction not resolved: S"))
  }

  def construction = {
    val found = openRows("""class OK2 { val boom: Nothing = ???; def m: Boolean = true }
      |class LK2 { lazy val boom: Boolean = ???; def m: Boolean = true }
      |class DK2 { def boom: Boolean = ???; def m: Boolean = true }
      |class OK1 { val x: Boolean = ???; def m(b: Boolean): Boolean = ??? }
      |final class FK3(val b: Boolean) { val x: Int = ??? }
      |trait TK { val x: Boolean = ??? }
      |abstract class BV { val x: Boolean }
      |class IV extends BV { val x = ??? }
      |class PA(n: Int)
      |class CA extends PA(???) { def m(b: Boolean): Boolean = ??? }
      |class V0 { var v: Int = ???; def m(b: Boolean): Boolean = ??? }
      |class Stmt { ???; def m(b: Boolean): Boolean = ??? }
      |class Mem { def ??? : Int = 1; val x: Int = ???; def m(b: Boolean): Boolean = b }
      |class St { println("x"); def m(b: Boolean): Boolean = ??? }
      |object Use {
      |  def ok2(k: OK2): Boolean = ???
      |  def lk2(k: LK2): Boolean = ???
      |  def dk2(k: DK2): Boolean = ???
      |  def ok1(k: OK1): Boolean = ???
      |  def fk3(k: FK3): Boolean = ???
      |  def fk3g(b: Boolean): FK3 = ???
      |  def tk(k: TK): Boolean = ???
      |  def iv(k: IV): Boolean = ???
      |}""".stripMargin)
    List("Use.ok2", "Use.ok1", "Use.fk3", "Use.tk", "Use.iv")
      .map(name => found(name) === Count.Finite(1))
      .reduce(_ and _)
      .and(found("Use.lk2") === Count.Finite(16))
      .and(found("Use.dk2") === Count.Finite(16))
      .and(found("Use.fk3g") === Count.Finite(0))
      .and(found("FK3.<init>") === Count.Finite(2))
      .and(
        List("OK2.m", "OK1.m", "CA.m", "V0.m", "Stmt.m")
          .map(name => found(name) === Count.Finite(1))
          .reduce(_ and _)
      )
      .and(found("St.m") === Count.Finite(4))
      .and(found("Mem.m").render === "2^(2^65)")
  }

  // A member nothing overrides is concrete code (D19): a throw there supplies nothing, so an object
  // or a final class with one is built and its members count as usual — O.f, FK.f and FQ2.f are
  // the 4 functions of b. That holds where it implements a declaration of an empty type too (OQ,
  // FQ: 4), while their parent P, read through its contract, has no instance (1): the two readings
  // part only for code no pure program contains (owner question, notes C4).
  def fixedThrowing = {
    val found = rows("""abstract class P { def k(x: Int): Nothing }
      |object OQ extends P { def k(x: Int): Nothing = ???; def f(b: Boolean): Boolean = ??? }
      |final class FQ extends P { def k(x: Int): Nothing = ???; def f(b: Boolean): Boolean = ??? }
      |abstract class P2[A] { def k(x: A): Nothing }
      |final class FQ2[A] extends P2[A] { def k(x: A): Nothing = ???; def f(b: Boolean): Boolean = ??? }
      |object O { def fail(msg: String): Nothing = throw new Exception(msg); def f(b: Boolean): Boolean = ??? }
      |final class FK { def k(x: Int): Nothing = ???; def f(b: Boolean): Boolean = ??? }
      |object U { def viaP(p: P, b: Boolean): Boolean = ???; def viaFQ(q: FQ, b: Boolean): Boolean = ??? }
      |""".stripMargin)
    each(found, Count.Finite(4))("O.f", "FK.f", "FQ2.f", "OQ.f", "FQ.f", "U.viaFQ")
      .and(found("U.viaP") === Count.Finite(1))
  }

  // Top-level definitions live in their file's synthetic object, built with every strict val of
  // the file in that package (a 3.9.0 run: calling f throws): f is never called — 1. A top-level
  // class is built without it (4), and another file's object is another one: g is 4. A package
  // object is an object like any: po.f 1, and with a lazy val pq.g 4.
  def topLevel = {
    val found = counts(
      "A.scala" -> """package tp
        |val boom: Int = ???
        |def f(b: Boolean): Boolean = ???
        |class TopC { def m(b: Boolean): Boolean = ??? }
        |""".stripMargin,
      "B.scala" -> "package tp\nlazy val later: Int = ???\ndef g(b: Boolean): Boolean = ???\n",
      "C.scala" -> """package object po { val boom: Int = ???; def f(b: Boolean): Boolean = ??? }
        |package object pq { lazy val boom: Int = ???; def g(b: Boolean): Boolean = ??? }
        |""".stripMargin
    )
    each(found, Count.Finite(1))("tp.f", "po.f")
      .and(each(found, Count.Finite(4))("tp.TopC.m", "tp.g", "pq.g"))
  }

  // A member object of a class or trait is built from the instance it belongs to (3.9.0 runs):
  // C.O.D needs a C, whose `x` throws, and T.O's own `y` throws — 1. A static object's are built
  // without it: S.O.D and S.O.P are 4.
  def memberObjects = {
    val found = rows("""class C {
      |  val x: Int = ???
      |  object O { class D { def m(b: Boolean): Boolean = ??? }; object P { def m(b: Boolean): Boolean = ??? } }
      |}
      |object S {
      |  val x: Int = ???
      |  object O { class D { def m(b: Boolean): Boolean = ??? }; object P { def m(b: Boolean): Boolean = ??? } }
      |}
      |trait T { object O { val y: Int = ???; class D { def m(b: Boolean): Boolean = ??? } } }
      |""".stripMargin)
    each(found, Count.Finite(1))("C.O.D.m", "C.O.P.m", "T.O.D.m")
      .and(each(found, Count.Finite(4))("S.O.D.m", "S.O.P.m"))
  }

  // A level's body sees its own scope, not a subclass's (3.9.0 runs): TK's `???` throws in every
  // Sh, so TK, Sh and their members are 1 though Sh defines `???`. An import binds from its
  // position on: L's `x` runs before it (1), K's after it (K is the record (x, m): m ranges over
  // functions of (b, x), 2^(2^33)). An export binds throughout its template (O.E: 2^(2^33)), and a
  // renaming import does not bind the original name (R: 1).
  def bindingScope = {
    val found = openRows("""object Helpers { def ??? : Int = 1 }
      |trait TK { val x: Boolean = ??? }
      |class Sh extends TK { def ??? : Boolean = true; def m(b: Boolean): Boolean = ??? }
      |class K { import Helpers.???; val x: Int = ???; def m(b: Boolean): Boolean = b }
      |class L { val x: Int = ???; import Helpers.???; def m(b: Boolean): Boolean = b }
      |object O { export Helpers.???; class E { val x: Int = ???; def m(b: Boolean): Boolean = b } }
      |class R { import Helpers.{??? => q}; val x: Int = ???; def m(b: Boolean): Boolean = b }
      |object V { def tk(t: TK): Boolean = ???; def sh(s: Sh): Boolean = ??? }
      |""".stripMargin)
    each(found, Count.Finite(1))("V.tk", "V.sh", "Sh.m", "L.m", "R.m")
      .and(found("K.m").render === "2^(2^33)")
      .and(found("O.E.m").render === "2^(2^33)")
  }

  // A sealed case, family or enum whose construction only throws has no value — A's case is empty,
  // R and E have none — but the family reading resolves no term names, so it refuses for that gap
  // rather than count 2^3 = 8 (or 2^2 for E). Building a case runs every template it extends: M1
  // mixes in Boom, and G1 extends G with it, which also fails every singleton case of G (3.9.0
  // runs: `G.G2` throws). Singleton cases are vals of the companion, whose initializer throws for
  // F (a 3.9.0 run: `F.D` throws); a class case is built without it: C's family is C1(x), 4. A lazy
  // val builds nothing: L = L1(b) + L2, 3 values, 8.
  def sealedConstruction = {
    val found = rows("""sealed trait S
      |final case class A(b: Boolean) extends S { val z: Int = ??? }
      |case object B extends S
      |sealed trait R { val z: Int = ??? }
      |case object R1 extends R
      |sealed trait Q
      |case object Q1 extends Q { ??? }
      |case object Q2 extends Q
      |enum E { case E1, E2; val x: Int = ??? }
      |trait Boom { val z: Int = ??? }
      |sealed trait M
      |final case class M1(b: Boolean) extends M with Boom
      |case object M2 extends M
      |enum G { case G1 extends G with Boom; case G2 }
      |enum F { case D }
      |object F { val boom: Int = ??? }
      |enum C { case C1(x: Boolean) }
      |object C { val boom: Int = ??? }
      |sealed trait L
      |final case class L1(b: Boolean) extends L { lazy val z: Int = ??? }
      |case object L2 extends L
      |object U {
      |  def s(x: S): Boolean = ???
      |  def r(x: R): Boolean = ???
      |  def q(x: Q): Boolean = ???
      |  def e(x: E): Boolean = ???
      |  def m(x: M): Boolean = ???
      |  def g(x: G): Boolean = ???
      |  def f(x: F): Boolean = ???
      |  def fg(b: Boolean): F = ???
      |  def c(x: C): Boolean = ???
      |  def l(x: L): Boolean = ???
      |}""".stripMargin)
    refused(found("U.s"), "sealed family construction not resolved: A")
      .and(refused(found("U.r"), "sealed family construction not resolved: R"))
      .and(refused(found("U.q"), "sealed family construction not resolved: Q1"))
      .and(refused(found("U.e"), "enum construction not resolved: E"))
      .and(refused(found("U.m"), "sealed family construction not resolved: M1"))
      .and(refused(found("U.g"), "enum construction not resolved: G"))
      .and(refused(found("U.f"), "enum construction not resolved: F"))
      .and(refused(found("U.fg"), "enum construction not resolved: F"))
      .and(found("U.c") === Count.Finite(4))
      .and(found("U.l") === Count.Finite(8))
  }

  // A class or object an object encloses is built without the object (a 3.9.0 run calls O0.C and
  // O0.I while O0's initializer throws): 4. Calling O0.f builds O0, and In needs its outer
  // instance: 1.
  def staticNesting = {
    val found = rows("""object O0 {
      |  val x: Int = ???
      |  class C { def m(b: Boolean): Boolean = ??? }
      |  object I { def m(b: Boolean): Boolean = ??? }
      |  def f(b: Boolean): Boolean = ???
      |}
      |class Outer0 { val x: Int = ???; class In { def m(b: Boolean): Boolean = ??? } }
      |""".stripMargin)
    (found("O0.C.m") === Count.Finite(4))
      .and(found("O0.I.m") === Count.Finite(4))
      .and(found("O0.f") === Count.Finite(1))
      .and(found("Outer0.In.m") === Count.Finite(1))
  }

  // `{}` is the Unit value: A's noop is no throw and m the 4 functions of b; O.f's block stays
  // unread, never a failed analysis.
  def emptyBlock = {
    val found = rows(
      "class A { def noop(): Unit = {}; def m(b: Boolean): Boolean = ??? }\n" +
        "object O { def f = {} }"
    )
    (found("A.m") === Count.Finite(4))
      .and(refused(found("O.f"), "inferred result type not resolved: block"))
  }

  // A match mixing a throwing and a returning case, a `try`, a parameter named `sys` and a source
  // `Predef` are not recognized as throws: each stays unresolved with its form named — never 0.
  def throwLimits = {
    val found = rows("""object Rebound { object sys { def error(s: String): Int = 1 } }
      |object Limits {
      |  def mixed(c: Boolean) = c match { case true => throw new Exception("x"); case false => 1 }
      |  def caught = try { 1 } catch { case e: Exception => 2 }
      |  def param(sys: Rebound.sys.type) = sys.error("e")
      |}
      |object Pre { object Predef { def ??? : Int = 1 }; def f = Predef.??? }""".stripMargin)
    refused(found("Limits.mixed"), "inferred result type not resolved: match")
      .and(refused(found("Limits.caught"), "inferred result type not resolved: expression"))
      .and(refused(found("Limits.param"), "inferred result type not resolved: application"))
      .and(refused(found("Pre.f"), "inferred result type not resolved: selection"))
  }

  // Every instance must implement an abstract `boom: Nothing`, which no pure code can: the receiver
  // is impossible and m is vacuous — 1 (register D20, unchanged by D26).
  def abstractTwin =
    rows("abstract class AbsBoom { def boom: Nothing; def m(b: Boolean): Boolean }")(
      "AbsBoom.m"
    ) === Count.Finite(1)

  // A final class's `any` is concrete code nothing overrides: dropped, m is 4.
  def finalThrowing =
    rows("final class FinalAny { def any[A]: A = ???; def m(b: Boolean): Boolean = ??? }")(
      "FinalAny.m"
    ) === Count.Finite(4)

  // OpenBoom's record is (boom, m) with boom: Nothing, as its abstract twin's: no value — vacuous as
  // an input (1), empty as a goal (0). OpenGet's is (get, m): functions of get, m(true), m(false) —
  // 2^8 = 256.
  def throwingRecord = {
    val found = openRows("""class OpenGet { def get: Boolean = ???; def m(b: Boolean): Boolean = ??? }
      |class OpenBoom { def boom: Nothing = throw new Exception("x"); def m(b: Boolean): Boolean = ??? }
      |object Use {
      |  def f(o: OpenBoom): Boolean = ???
      |  def mk(b: Boolean): OpenBoom = ???
      |  def g(o: OpenGet): Boolean = ???
      |}""".stripMargin)
    (found("Use.f") === Count.Finite(1))
      .and(found("Use.mk") === Count.Finite(0))
      .and(found("Use.g") === Count.Finite(256))
  }

  // F's `seed` reads the hidden a1 only to build the exception it throws: no result carries it.
  // seed is F's concrete code (F is final); m has its own a1 and a2 — 2 (it read "final
  // implementation reads a hidden capture: a1").
  def throwOperand =
    rows("""trait P[A] { def seed: A }
      |object M2 {
      |  def mk[A](a1: A, a2: A): P[A] = {
      |    final class F extends P[A] {
      |      def seed: A = throw new IllegalStateException(a1.toString)
      |      def m(a1: A): A = ???
      |    }
      |    new F
      |  }
      |}""".stripMargin)("M2.mk.F.m") === Count.Finite(2)

  // `new Exception(s)` for every string s, which `getMessage` tells apart: ω for each throwable,
  // `Either[Throwable, A]` (Left of any, or Right(a)) included.
  def throwableGoals = {
    val found = rows("""object G {
      |  def g1(): Throwable = ???
      |  def g2(b: Boolean): Exception = ???
      |  def g3[A](a: A): Either[Throwable, A] = ???
      |  def g4(name: String): IllegalArgumentException = ???
      |  def g5(e: java.lang.Throwable): scala.Throwable = ???
      |}""".stripMargin)
    List("g1", "g2", "g3", "g4", "g5")
      .map(name => found(s"G.$name") === Count.Countable)
      .reduce(_ and _)
  }

  // A supplied throwable is observed through its message: a Boolean of it is any decidable
  // predicate on strings — ω, as for a String (it read "open stdlib type with unmodelled members").
  // Two As told apart by it: ω. With k: Throwable => Nothing the context is absurd: 1.
  def throwableInputs = {
    val found = rows("""object I {
      |  def i1(e: Throwable): Boolean = ???
      |  def i2[A](e: Throwable, x: A, y: A): A = ???
      |  def i3(e: NoSuchElementException, f: IllegalStateException): Boolean = ???
      |  def i4[A](k: Throwable => Nothing, e: Throwable): A = ???
      |}""".stripMargin)
    (found("I.i1") === Count.Countable)
      .and(found("I.i2") === Count.Countable)
      .and(found("I.i3") === Count.Countable)
      .and(found("I.i4") === Count.Finite(1))
  }

  // No observation of a throwable builds an A: a's one choice (1), None or Some(a) for a Right and
  // None for a Left (2), the one Unit (1), and no Nothing at all (0).
  def irrelevantThrowable = {
    val found = rows("""object R {
      |  def r1[A](e: Throwable, a: A): A = ???
      |  def r2[A](e: Either[Throwable, A]): Option[A] = ???
      |  def r3(e: Throwable): Unit = ???
      |  def r4(e: Throwable): Nothing = ???
      |  def r5[A](e: IllegalArgumentException): Option[A] = ???
      |}""".stripMargin)
    (found("R.r1") === Count.Finite(1))
      .and(found("R.r2") === Count.Finite(2))
      .and(found("R.r3") === Count.Finite(1))
      .and(found("R.r4") === Count.Finite(0))
      .and(found("R.r5") === Count.Finite(1))
  }

  // A rigid F tells its arguments apart by declaration: F[Throwable] from x is x (1), while an
  // invariant F[Exception] is no F[Throwable] (0).
  def throwableKeys = {
    val found = rows("""object K {
      |  def same[F[_]](x: F[Throwable]): F[Throwable] = ???
      |  def other[F[_]](x: F[Exception]): F[Throwable] = ???
      |}""".stripMargin)
    (found("K.same") === Count.Finite(1)).and(found("K.other") === Count.Finite(0))
  }

  // MyErr overrides getMessage, but a subclass of it may still override getCause and
  // getLocalizedMessage, which this reading does not enumerate as capabilities: refused with the
  // member named.
  def openSubclass = {
    val found = openRows("""class MyErr(msg: String) extends Exception(msg) {
      |  override def getMessage: String = "x"
      |}
      |object U { def u(e: MyErr): Boolean = ??? }""".stripMargin)
    refused(found("U.u"), "inherited stdlib member a subclass may override: java.lang.Exception")
  }

  // A final subclass is its constructor arguments (its message and cause are fixed functions of
  // them): code ranges over Int — 2^(2^32) predicates; built from b, any function of b to an Int —
  // (2^32)^2 = 2^64.
  def finalSubclass = {
    val found = rows("""final class Coded(val code: Int) extends RuntimeException("c")
      |object U { def u(e: Coded): Boolean = ???; def mk(b: Boolean): Coded = ??? }""".stripMargin)
    (found("U.u").render === "2^(2^32)").and(found("U.mk").render === "2^64")
  }

  // A source class named Throwable is that class: one Boolean field, 4 — never the model's ω.
  def shadowedThrowable =
    rows("final class Throwable(val b: Boolean)\nobject S { def s(t: Throwable): Boolean = ??? }")(
      "S.s"
    ) === Count.Finite(4)

  def wildcardThrowable =
    refused(
      rows("import foo.*\nobject W { def w(t: Throwable): Boolean = ??? }")("W.w"),
      "builtin name Throwable may be shadowed through a reference not supplied: foo"
    )

  // A class test cannot yet tell an open stdlib class apart from a source class: the union stays
  // unresolved, with the stdlib class named.
  def exceptionUnion =
    refused(
      rows(
        "final class Plan(val n: Boolean)\nobject U { def u(b: Boolean): Exception | Plan = ??? }"
      )(
        "U.u"
      ),
      "Exception has no runtime class this reading tells apart (stdlib model)"
    )

}
