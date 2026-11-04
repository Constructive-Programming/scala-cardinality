package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Inferred results, typed only where syntax fixes them: overridden declarations, literals,
  * ascriptions, explicit construction and calls of declared methods (environment completeness §5).
  * Every fixture is valid Scala 3.9.0 except where a name outside the sources is the point; each
  * expected number is derived in the comment beside it.
  */
class InferredResultsSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Inferred results
      a literal is its class, whatever names are in scope            $literals
      an ascription to a plain type is that type                     $ascriptions
      an overriding member takes the overridden result               $overrides
      an overload or a private parent member is not overridden       $overloads
      an overridden abstract type member is the subclass's           $overriddenMember
      an abstract var's accessors or a given type their implementer  $variableOverrides
      an override of a declaration not supplied is refused           $unsuppliedOverride
      an implicit parent's member is refused                         $implicitParents
      a dependent or extension override is refused                   $overrideRefusals
      a call of a declared method with explicit type arguments       $calls
      a call needing inference or another reading is refused         $callRefusals
      a call an import, export, extension or parent may bind         $callBindings
      a creator application reads the case class it creates          $creatorApplications
      a creator application that may call another term is refused    $creatorRefusals
      a singleton ascription, written or aliased, is refused         $singletonAscriptions
      a call's singleton result is widened or refused                $singletonCalls
      an instance of a class outside the sources is read as written  $outsideCreation
      a transparent inline method's result is refused                $transparentInline
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def counts(sources: (String, String)*): Map[String, Count] =
    MethodAnalysis.analyze(inputs(sources*)).map(e => e.name -> e.count).toMap

  private def rows(code: String): Map[String, Count] = counts("Example.scala" -> code)

  private def count(code: String, name: String): Count = rows(code)(name)

  private def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  // `true` is a scala.Boolean even where `Boolean` names Int (2); `()` is Unit (1); `3` an Int.
  def literals = {
    val found = rows("""object I { def lit = true; def unit() = (); def int = 3; def nul = null }
      |object Sh { type Boolean = Int; def lit = false }""".stripMargin)
    (found("I.lit") === Count.Finite(2))
      .and(found("I.unit") === Count.Finite(1))
      .and(found("I.int") === Count.Finite(BigInt(2).pow(32)))
      .and(found("Sh.lit") === Count.Finite(2))
      .and(refused(found("I.nul"), "inferred result type not resolved: literal null"))
  }

  // `(x: A)` is an A: x (1). A union ascription is kept by the compiler (fixture imp/c.scala); this
  // reading refuses it, which costs only precision.
  def ascriptions = {
    val found = rows("object I { def asc[A](x: A) = (x: A); def u = (1: Int | String) }")
    (found("I.asc") === Count.Finite(1))
      .and(refused(found("I.u"), "inferred result type not resolved: union ascription"))
  }

  // Scala 3.9.0 types an overriding member by the member it overrides (fixtures: `Some(1)`
  // overriding `Option[Int]` is an `Option[Int]`). eitherAccessor.getOption: Left gives None (1),
  // Right(a) gives None or Some(a) (2) — 1 × 2. O.k is an Option[Int]: 1 + 2^32. O.f is an Any (1).
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
      .and(found("O.f") === Count.Finite(1))
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
  // f_=(x: Any) = true` a `Unit`, value discarded): 1 each, never the body's Boolean 2.
  def variableOverrides = {
    val found = rows("""trait P { var f: Any }
      |final class C extends P { def f = true; def f_=(x: Any) = true }
      |trait G { given f: Any; given h(using x: Int): Any }
      |final class D extends G { def f = true; def h(using x: Int) = true }""".stripMargin)
    List("C.f", "C.f_=", "D.f", "D.h").map(found(_) === Count.Finite(1)).reduce(_ and _)
  }

  // Every template extends `Any` and `Object`; a case class or object `Product` and `Equals`, an enum
  // `scala.reflect.Enum` with both: scalac types `productElement` as `Any` (1), `clone` as `Object`
  // and `canEqual` as `Boolean` — never by the body.
  def implicitParents = {
    val found = rows("""final case class G(a: Int) { def productElement(n: Int) = true }
      |case object CO { def productElement(n: Int) = true }
      |final class K { override def clone() = new K }
      |enum E { case A; def canEqual(x: Any) = true }
      |object R { override def toString = "r" }""".stripMargin)
    val reason = "inferred result type not resolved: a member that may override a declaration of"
    refused(found("G.productElement"), s"$reason Product")
      .and(refused(found("CO.productElement"), s"$reason Product"))
      .and(refused(found("K.clone"), s"$reason Any"))
      .and(refused(found("E.canEqual"), s"$reason Equals"))
      .and(refused(found("R.toString"), s"$reason Any"))
  }

  // A dependent overridden result; an extension method of a template with a parent (scalac types
  // `O`'s `f` by `P`'s `Any`); a context bound, whose evidence clause is not compared.
  def overrideRefusals = {
    val found = rows("""trait PD { def f(x: Any): x.type }
      |object OD extends PD { def f(x: Any) = x }
      |trait P { extension (x: Int) def f: Any }
      |object O extends P { extension (x: Int) def f = true }
      |trait PB { def g[A: Ordering](x: A): Any }
      |object OB extends PB { def g[A: Ordering](x: A) = true }""".stripMargin)
    refused(found("OD.f"), "inferred result type not resolved: dependent result of an overridden f")
      .and(refused(found("OB.g"), "overriding member g, whose parameter types are not matched"))
      .and(
        refused(
          found("O.<extension@4>.f"),
          "inferred result type not resolved: an extension method that may override an inherited one"
        )
      )
  }

  def unsuppliedOverride = {
    val found = rows("""object Q extends Ext { def f = 3 }
      |trait P2 { def apply(x: Int): Any }
      |final class F extends P2 with (Int => Boolean) { def apply(x: Int) = true }
      |trait P3 { def getAsBoolean(): Any }
      |final class G extends P3 with java.util.function.BooleanSupplier { def getAsBoolean() = true }
      |""".stripMargin)
    val reason = "inferred result type not resolved: a member that may override a declaration of"
    refused(found("Q.f"), s"$reason Ext")
      .and(refused(found("F.apply"), s"$reason Int => Boolean"))
      .and(refused(found("G.getAsBoolean"), s"$reason java.util.function.BooleanSupplier"))
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

  // `C` inherits `Base.g`, which outranks the package's `g` of another file: the lookup of a term
  // through a template with parents is not attempted.
  def callRefusals = {
    val found = counts(
      "a.scala" -> ("package p\ndef id[A](x: A): A = x\ndef curried[A](x: A)(y: A): A = x\n" +
        "def call[A](x: A) = id(x)\ndef call5[A](x: A) = curried[A](x)\n" +
        "object Ov { def g(x: Int): Int = x; def g(x: String): String = x; def h = g(1) }\n" +
        "def g(x: Int): Option[Int] = None"),
      "b.scala" -> "package p\ntrait Base { def g(x: Int): Int = x }\nclass C extends Base { def h = g(1) }"
    )
    refused(found("p.call"), "call of id needing type-argument inference")
      .and(refused(found("p.call5"), "partial application of curried"))
      .and(refused(found("p.Ov.h"), "call of g, not one declared method"))
      .and(refused(found("p.C.h"), "call of g, which a template may inherit"))
  }

  // Each callee is bound by something other than the one declaration the walk would reach: the
  // overload `P.f(String)` C inherits (scalac: Boolean, true 2), an explicit import, an export and an
  // extension method outranking `Outer.f`, an import outranking another file's package member, and
  // a dependent result.
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
    val reason = "inferred result type not resolved: call of"
    refused(found("cb.C.g2"), s"$reason f, which a template may inherit from P")
      .and(refused(found("cb.Outer.Imp.h"), s"$reason f, which an import may bind"))
      .and(refused(found("cb.Outer.Exp.h"), s"$reason f, which an export may define"))
      .and(refused(found("cb.Outer.Ext.h"), s"$reason f, an extension method"))
      .and(refused(found("cb.Use.f"), s"$reason g, which an import may bind"))
      .and(refused(found("cb.Dep.k"), "inferred result type not resolved: dependent result of id"))
      .and(refused(found("cb.Dep.r"), "inferred result type not resolved: recursive call"))
      .and(refused(found("cb.Dep.u"), s"$reason nowhere, not a method an enclosing scope declares"))
      .and(
        refused(found("cb.Par.k"), "inferred result type not resolved: application of a parameter")
      )
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

  // A companion `apply` or a nearer term `Box` would be the callee, not the constructor.
  def creatorRefusals = {
    val found = rows("""class Fact[A](a: A)
      |object Fact { def apply[A](a: A): Fact[A] = new Fact(a) }
      |object UseF { def mk[A](a: A) = Fact[A](a) }
      |final class Box[A](a: A)
      |object UseB { object Box { def apply[A](a: A): Option[A] = Some(a) }; def mk[A](a: A) = Box[A](a) }
      |object UseC { def mk[A](a: A) = new Box(a) }""".stripMargin)
    val other = "which may name another term"
    refused(found("UseF.mk"), other)
      .and(refused(found("UseB.mk"), other))
      .and(refused(found("UseC.mk"), "instance creation needing type arguments"))
  }

  // Scala widens a singleton in an inferred result (scalac rejects `val y: 2 = O.f`): `(1: One)` is
  // an Int, `(v: V)` a Boolean. The singleton is refused, written or behind an alias.
  def singletonAscriptions = {
    val found = rows(
      """type One = 1
      |object O { val v: Boolean = true; type V = v.type; def f = (1: One); def g = (v: V); def h = (1: 1) }""".stripMargin
    )
    val reason = "inferred result type not resolved: singleton ascription"
    refused(found("O.f"), reason)
      .and(refused(found("O.g"), reason))
      .and(refused(found("O.h"), reason))
  }

  // Scala widens a call's singleton result (fixtures: `h` is an Int, `g` a `C`, `u2` an Int, `ga`
  // an `E`): `one: 1` gives an Int (2^32); `this.type`, an enum case's type, an alias of a literal
  // and a union of literals are refused; an object's singleton widens to itself (1); a union of
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
    val reason = "inferred result type not resolved: singleton result of a call of"
    (found("O.h") === Count.Finite(BigInt(2).pow(32)))
      .and(refused(found("C.g"), s"$reason self"))
      .and(refused(found("O.k"), s"$reason aliased"))
      .and(refused(found("O.u2"), s"$reason u"))
      .and(refused(found("O.ga"), s"$reason a"))
      .and(found("O.gm") === Count.Finite(1))
      .and(found("O.kk") === Count.Countable)
  }

  // `new Object` is an Object (terminal: 1); `new StringBuilder` names its absent declaration; a
  // stdlib class the model gives type parameters needs them.
  def outsideCreation = {
    val found =
      rows("object O { def f = new Object; def g = new StringBuilder; def h = new Some(1) }")
    (found("O.f") === Count.Finite(1))
      .and(refused(found("O.g"), "declaration not supplied: StringBuilder"))
      .and(refused(found("O.h"), "inferred result type not resolved: instance creation needing"))
  }

  def transparentInline =
    refused(
      count("object Tn { transparent inline def t = true }", "Tn.t"),
      "inferred result type not resolved: transparent inline method"
    )

}
