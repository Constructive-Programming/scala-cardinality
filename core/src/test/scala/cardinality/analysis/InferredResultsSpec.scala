package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Inferred results, typed only where syntax fixes them: overridden declarations, literals,
  * ascriptions, explicit construction and calls of declared methods (environment completeness §5).
  * Every fixture is valid Scala 3.9.0 except where a name outside the sources is the point; each
  * expected number is derived in the comment beside it.
  */
class InferredResultsSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Inferred results
      an overriding member takes the overridden result               $overrides
      an overload or a private parent member is not overridden       $overloads
      an overridden abstract type member is the subclass's           $overriddenMember
      an abstract var's accessors or a given type their implementer  $variableOverrides
      an override of a declaration not supplied is refused           $unsuppliedOverride
      an implicit parent's member is refused                         $implicitParents
      a dependent or extension override is refused                   $overrideRefusals
      a call of a declared method with explicit type arguments       $calls
      a creator application reads the case class it creates          $creatorApplications
  """

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

}
