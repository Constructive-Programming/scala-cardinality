package cardinality.analysis

import cardinality.Pending
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Inferred results are the compiler's (rule T, `CompiledTypes`): every fixture is compiled first
  * (`cardinality.Compiled`), and a member written without a type reads the type scalac gave it. A
  * fixture the compiler rejects (a name outside the sources is the point) is read as written: its
  * untyped members refuse, the compiler's inference is not imitated. Each expected number is
  * derived from the compiled type in the comment beside it.
  */
class InferredResultsSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Inferred results
      a literal is its class, whatever names are in scope            $literals
      an ascription to a plain type is that type                     $ascriptions
      an overriding member takes the overridden result               $overrides
      an overload or a private parent member is not overridden       $overloads
      an overridden abstract type member is the subclass's           $overriddenMember
      an abstract var's accessors or a given type their implementer  $variableOverrides
      a parent not supplied refuses the source's untyped members     ${Pending.rejectedFixture(
      unsuppliedOverride
    )}
      an implicit parent's member is typed by the parent             $implicitParents
      a dependent, extension or context-bound override               ${Pending.knownLoss(
      overrideRefusals
    )}
      a call of a declared method with explicit type arguments       $calls
      a call needing inference reads the inferred result             $callRefusals
      a source that does not compile refuses its untyped members     ${Pending.rejectedFixture(
      callBindings
    )}
      a creator application reads the case class it creates          $creatorApplications
      a companion apply or a nearer term is the callee               ${Pending.knownLoss(
      creatorRefusals
    )}
      a singleton ascription, written or aliased, is widened         $singletonAscriptions
      a call's singleton result is widened                           $singletonCalls
      an instance of a class outside the sources is read as written  ${Pending.knownLoss(
      outsideCreation
    )}
      a transparent inline method's result is refused                ${Pending.oldBehavior(
      transparentInline
    )}
  """

  // `true` is a scala.Boolean even where `Boolean` names Int (2); `()` is Unit (1); `3` an Int;
  // `null` a `Null`, which reads as no value, as a written `Null` result does (0).
  def literals = {
    val found = rows("""object I { def lit = true; def unit() = (); def int = 3; def nul = null }
      |object Sh { type Boolean = Int; def lit = false }""".stripMargin)
    (found("I.lit") === Count.Finite(2))
      .and(found("I.unit") === Count.Finite(1))
      .and(found("I.int") === Count.Finite(BigInt(2).pow(32)))
      .and(found("Sh.lit") === Count.Finite(2))
      .and(found("I.nul") === Count.Finite(0))
  }

  // `(x: A)` is an A: x (1). A union ascription is kept by the compiler: `Int | String`, an output
  // with a `String` side, is ω.
  def ascriptions = {
    val found = rows("object I { def asc[A](x: A) = (x: A); def u = (1: Int | String) }")
    (found("I.asc") === Count.Finite(1))
      .and(found("I.u") === Count.Countable)
  }

  // Scala 3.9.0 types an overriding member by the member it overrides (fixtures: `Some(1)`
  // overriding `Option[Int]` is an `Option[Int]`). eitherAccessor.getOption: Left gives None (1),
  // Right(a) gives None or Some(a) (2) — 1 × 2. O.k is an Option[Int]: 1 + 2^32. O.f is an Any
  // result, an output: ω (v1.md §2; it was ε₀).
  def overrides = {
    val found = rows(
      """trait PartialAccessor[F[_, _]] { def getOption[X, A](fa: F[X, A]): Option[A] }
      |given eitherAccessor: PartialAccessor[Either] with
      |  def getOption[X, A](fa: Either[X, A]) = fa.toOption
      |trait P { def f: Any; def k: Option[Int] }
      |object O extends P { def f = true; def k = None }""".stripMargin
    )
    (found("eitherAccessor.getOption") === Count.Finite(2))
      .and(found("O.k") === Count.Finite(BigInt(2).pow(32) + 1))
      .and(found("O.f").render === "ω")
  }

  // `Ext` may declare `f`; Scala intersects every base class's declaration, so one `F` reads (`Int =>
  // Boolean`'s `apply`, `BooleanSupplier`'s `getAsBoolean`) leaves the result unknown too (true
  // counts 2^(2^32) and 2, which only the intersection gives).
  // Scala matches parameter types too: `D.g(Boolean)` overloads `Q.g(Int)` and is a Boolean =>
  // Boolean (4); `C.f(String)` overloads `P.f(Int)`: Boolean predicates on a String (ω). A
  // private member is not inherited: `E.h` is a Boolean (2). scalac types each by its body.
  def overloads = {
    val found = rows("""trait P { def f(x: Int): Any = 1 }
      |final class C extends P { def f(x: String) = true }
      |trait Q { def g(x: Int): Any }
      |final class D extends Q { def g(x: Int): Any = 1; def g(x: Boolean) = true }
      |class PP { private def h: Any = 1 }
      |final class E extends PP { def h = true }""".stripMargin)
    (found("C.f") === Count.Countable)
      .and(found("D.g") === Count.Finite(4))
      .and(found("E.h") === Count.Finite(2))
  }

  // The overridden `T` is read as seen from the subclass: `O.T = Boolean` (2), not `P`'s abstract
  // `T` (0). A class parameter instantiated by the chain is still read: `C2.f` is an Option[Boolean]
  // (None, Some(true), Some(false): 3).
  def overriddenMember = {
    val found = rows("""trait P { type T; def f: T }
      |object O extends P { type T = Boolean; def f = true }
      |final class C extends P { type T = Boolean; def f = true }
      |trait P2[A] { def f: A }
      |trait Q2[B] extends P2[Option[B]]
      |final class C2 extends Q2[Boolean] { def f = None }""".stripMargin)
    (found("O.f") === Count.Finite(2))
      .and(found("C.f") === Count.Finite(2))
      .and(found("C2.f") === Count.Finite(3))
  }

  // An abstract `var` declares a getter and a setter `f_=(x: Any): Unit`, and a given a method; a
  // member implementing one is typed by it (3.9.0 fixtures: `def f = true` there is an `Any`, `def
  // f_=(x: Any) = true` a `Unit`, value discarded): never the body's Boolean 2. The Any results are
  // outputs, ω (v1.md §2; they were ε₀); the setter's Unit is 1.
  def variableOverrides = {
    val found = rows("""trait P { var f: Any }
      |final class C extends P { def f = true; def f_=(x: Any) = true }
      |trait G { given f: Any; given h(using x: Int): Any }
      |final class D extends G { def f = true; def h(using x: Int) = true }""".stripMargin)
    (found("C.f_=") === Count.Finite(1))
      .and(List("C.f", "D.f", "D.h").map(found(_).render === "ω").reduce(_ and _))
  }

  // Every template extends `Any` and `Object`; a case class or object `Product` and `Equals`, an enum
  // `scala.reflect.Enum` with both: scalac types `productElement` as `Any`, `clone` as `Object` and
  // `toString` as `String` (outputs: ω each), `canEqual` as `Boolean` (2) — never by the body.
  def implicitParents = {
    val found = rows("""final case class G(a: Int) { def productElement(n: Int) = true }
      |case object CO { def productElement(n: Int) = true }
      |final class K { override def clone() = new K }
      |enum E { case A; def canEqual(x: Any) = true }
      |object R { override def toString = "r" }""".stripMargin)
    (found("G.productElement") === Count.Countable)
      .and(found("CO.productElement") === Count.Countable)
      .and(found("K.clone") === Count.Countable)
      .and(found("E.canEqual") === Count.Finite(2))
      .and(found("R.toString") === Count.Countable)
  }

  // scalac types `OD.f` by the dependent `x.type`: x (1); `O`'s extension `f` by `P`'s `Any`, an
  // output: ω; `OB.g` by `Any` too, but its context bound names `Ordering`, which no source or the
  // stdlib model declares: refused, as with the result written.
  def overrideRefusals = {
    val found = rows("""trait PD { def f(x: Any): x.type }
      |object OD extends PD { def f(x: Any) = x }
      |trait P { extension (x: Int) def f: Any }
      |object O extends P { extension (x: Int) def f = true }
      |trait PB { def g[A: Ordering](x: A): Any }
      |object OB extends PB { def g[A: Ordering](x: A) = true }""".stripMargin)
    (found("OD.f") === Count.Finite(1))
      .and(refused(found("OB.g"), "declaration not supplied: Ordering"))
      .and(found("O.<extension@4>.f") === Count.Countable)
  }

  def unsuppliedOverride = {
    val found = rows("""object Q extends Ext { def f = 3 }
      |trait P2 { def apply(x: Int): Any }
      |final class F extends P2 with (Int => Boolean) { def apply(x: Int) = true }
      |trait P3 { def getAsBoolean(): Any }
      |final class G extends P3 with java.util.function.BooleanSupplier { def getAsBoolean() = true }
      |""".stripMargin)
    // `Ext` is not supplied, so the source does not compile: no untyped member has a type.
    val reason =
      "inferred result type not resolved: written without a type, in a source not compiled"
    refused(found("Q.f"), reason)
      .and(refused(found("F.apply"), reason))
      .and(refused(found("G.getAsBoolean"), reason))
  }

  // The callee's declared result under the explicit arguments: id[A](x) is an A (1), opt[A](x) an
  // Option[A] (None or Some(x): 2), curried[A](x)(x) an A (1), a member call inside a class reads
  // the class's binder (1).
  def calls = {
    val found = rows("""def id[A](x: A): A = x
      |def opt[A](x: A): Option[A] = Some(x)
      |def curried[A](x: A)(y: A): A = x
      |def call2[A](x: A) = id[A](x)
      |def call3[A](x: A) = opt[A](x)
      |def call6[A](x: A) = curried[A](x)(x)
      |class Wrap[A](a: A) { def same = id[A](a) }""".stripMargin)
    (found("call2") === Count.Finite(1))
      .and(found("call3") === Count.Finite(2))
      .and(found("call6") === Count.Finite(1))
      .and(found("Wrap.same") === Count.Finite(1))
  }

  // scalac infers each: `id(x)` an A (x: 1); `curried[A](x)` an `A => A` (y => x or y => y: 2);
  // `Ov.h` the `Int` overload; `C.h` the inherited `Base.g`'s `Int`, which outranks the package's
  // `g` of another file (2^32 each).
  def callRefusals = {
    val found = counts(
      "a.scala" -> ("package p\ndef id[A](x: A): A = x\ndef curried[A](x: A)(y: A): A = x\n" +
        "def call[A](x: A) = id(x)\ndef call5[A](x: A) = curried[A](x)\n" +
        "object Ov { def g(x: Int): Int = x; def g(x: String): String = x; def h = g(1) }\n" +
        "def g(x: Int): Option[Int] = None"),
      "b.scala" -> "package p\ntrait Base { def g(x: Int): Int = x }\nclass C extends Base { def h = g(1) }"
    )
    (found("p.call") === Count.Finite(1))
      .and(found("p.call5") === Count.Finite(2))
      .and(found("p.Ov.h") === Count.Finite(BigInt(2).pow(32)))
      .and(found("p.C.h") === Count.Finite(BigInt(2).pow(32)))
  }

  // `nowhere` is no name the sources declare, so the source does not compile: every member written
  // without a type refuses, whatever the callee (the inherited overload, an import, an export, an
  // extension, a dependent or recursive result, a parameter's application).
  def callBindings = {
    val found = counts(
      "a.scala" -> "package cb\ndef g(x: Int): Int = 1",
      "b.scala" -> """package cb
        |trait P { def f(s: String): Boolean = true }
        |final class C extends P { def f(x: Int): Int = x; def g2 = f("s") }
        |object A { def f(x: Int): Boolean = true; def g(x: Int): Boolean = true }
        |object Outer {
        |  def f(x: Int): Int = x
        |  object Imp { import A.f; def h = f(1) }
        |  object Exp { export A.f; def h = f(1) }
        |  object Ext { extension (x: Int) def f: Boolean = true; def h = f(1) }
        |}
        |object Use { import A.g; def f = g(1) }
        |object Dep { def id(x: Any): x.type = x; def k(y: Any) = id(y); def r = r; def u = nowhere(1) }
        |object Par { def k(h: Int => Boolean) = h(1) }""".stripMargin
    )
    val reason =
      "inferred result type not resolved: written without a type, in a source not compiled"
    List("cb.C.g2", "cb.Outer.Imp.h", "cb.Outer.Exp.h", "cb.Outer.Ext.h", "cb.Use.f", "cb.Dep.k")
      .appendedAll(List("cb.Dep.r", "cb.Dep.u", "cb.Par.k"))
      .map(name => refused(found(name), reason))
      .reduce(_ and _)
  }

  // `CBox[A](a)` and `new CBox[A](a)` are a `CBox[A]`: its one field holds `a` (1). `Box[A](a)` and
  // `new Box[A](a)` are a `Box[A]`, the final record of its public `val a` (capabilities C4): `a` (1).
  def creatorApplications = {
    val found = rows("""final case class CBox[A](a: A)
      |object CJ { def mk[A](a: A) = CBox[A](a); def mk2[A](a: A) = new CBox[A](a) }
      |final class Box[A](val a: A)
      |object J { def mk[A](a: A) = Box[A](a); def mk2[A](a: A) = new Box[A](a) }""".stripMargin)
    (found("CJ.mk") === Count.Finite(1))
      .and(found("CJ.mk2") === Count.Finite(1))
      .and(found("J.mk") === Count.Finite(1))
      .and(found("J.mk2") === Count.Finite(1))
  }

  // A companion `apply` or a nearer term `Box` is the callee: `UseF.mk` is the `Fact[A]` the companion
  // builds and `UseC.mk` a `Box[A]`, whose field no public member observes: refused, as with the
  // result written; `UseB.mk` is the nearer `Box`'s `Option[A]` (None or Some(a): 2).
  def creatorRefusals = {
    val found = rows("""class Fact[A](a: A)
      |object Fact { def apply[A](a: A): Fact[A] = new Fact(a) }
      |object UseF { def mk[A](a: A) = Fact[A](a) }
      |final class Box[A](a: A)
      |object UseB { object Box { def apply[A](a: A): Option[A] = Some(a) }; def mk[A](a: A) = Box[A](a) }
      |object UseC { def mk[A](a: A) = new Box(a) }""".stripMargin)
    val hidden = "final class field not observable injectively"
    refused(found("UseF.mk"), s"$hidden: Fact.a")
      .and(found("UseB.mk") === Count.Finite(2))
      .and(refused(found("UseC.mk"), s"$hidden: Box.a"))
  }

  // Scala widens a singleton in an inferred result (scalac rejects `val y: 2 = O.f`): `(1: One)` and
  // `(1: 1)` are an Int (2^32), `(v: V)` a Boolean (2); `v`, a final `true`, adds nothing.
  def singletonAscriptions = {
    val found = rows(
      """type One = 1
      |object O { val v: Boolean = true; type V = v.type; def f = (1: One); def g = (v: V); def h = (1: 1) }""".stripMargin
    )
    (found("O.f") === Count.Finite(BigInt(2).pow(32)))
      .and(found("O.g") === Count.Finite(2))
      .and(found("O.h") === Count.Finite(BigInt(2).pow(32)))
  }

  // Scala widens a call's singleton result: `one: 1`, the alias `One` and `1 | 2` give an Int
  // (2^32); `this.type` a `C`, two values (a Boolean field) split over the captured `a`: 2^2 = 4;
  // an enum case's type its enum (A or B: 2); an object's singleton stays itself (1); a union of
  // classes is kept (Int | String: ω).
  def singletonCalls = {
    val found = rows("""enum E { case A, B }
      |object M
      |final class C(val a: Boolean) { def self: this.type = this; def g = self }
      |object O {
      |  type One = 1
      |  def one: 1 = 1; def h = one
      |  def aliased: One = 1; def k = aliased
      |  def u: 1 | 2 = 1; def u2 = u
      |  def a: E.A.type = E.A; def ga = a
      |  def m: M.type = M; def gm = m
      |  def keep: Int | String = 1; def kk = keep
      |}""".stripMargin)
    (found("O.h") === Count.Finite(BigInt(2).pow(32)))
      .and(found("C.g") === Count.Finite(4))
      .and(found("O.k") === Count.Finite(BigInt(2).pow(32)))
      .and(found("O.u2") === Count.Finite(BigInt(2).pow(32)))
      .and(found("O.ga") === Count.Finite(2))
      .and(found("O.gm") === Count.Finite(1))
      .and(found("O.kk") === Count.Countable)
  }

  // `new Object` is an Object (a top in an output: ω, v1.md §2); `new StringBuilder` a
  // `scala.collection.mutable.StringBuilder`, which no source or the stdlib model declares; `new
  // Some(1)` a `Some[Int]` (2^32).
  def outsideCreation = {
    val found =
      rows("object O { def f = new Object; def g = new StringBuilder; def h = new Some(1) }")
    (found("O.f").render === "ω")
      .and(
        refused(found("O.g"), "declaration not supplied: scala.collection.mutable.StringBuilder")
      )
      .and(found("O.h") === Count.Finite(BigInt(2).pow(32)))
  }

  def transparentInline =
    refused(
      count("object Tn { transparent inline def t = true }", "Tn.t"),
      "inferred result type not resolved: transparent inline method"
    )

}
