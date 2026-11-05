package cardinality.analysis.resolution

import scala.compiletime.testing.typeChecks
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.analysis.inhabitation.NaturalSyntax.pow
import org.specs2.Specification
import org.specs2.execute.Result

// The boundary of the type-member reading (design/members.md, review of B2): which `this` a path
// names, which members are free, which refinements describe a value. Every fixture is valid Scala
// 3.9.0; the compiler facts the refusals rest on are pinned against `memberfixture`.
class TypeMemberBoundarySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Paths
      a parameter hides an enclosing template's self alias          $selfShadow
      a value fixed by its own definition is no choice              $localValue
    Members
      a type-only template with no constructor input is one value  $typesOnly
    Refinements
      the compiler rejects the refused refinements                  $refinementFacts
    Stuck projections
      a member scrutinee is spelled by its path                     $spelledMember
      a type pattern before `case _` still needs a proof            $typePattern
    Gate G1
      a `var` of an intermediate parent implements a `def`          $varImplements
      a local implementer's captures are refused                    $localImplementer
    Closures
      a parent clause's constructor is read with its file's imports $closureImports
  """

  private def entries(sources: (String, String)*): Map[String, Count] =
    MethodAnalysis
      .analyze(
        sources.toList.map((name, code) =>
          MethodAnalysis.Input(name, dialects.Scala3(code).parse[Source].get)
        )
      )
      .map(entry => entry.name -> entry.count)
      .toMap

  private def entries(code: String): Map[String, Count] = entries("Members.scala" -> code)

  private def refused(found: Count, reason: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(reason)) must beTrue
    }

  // The parameter `self: R` hides T's self alias, so `self.X` is R's member (scalac rejects `= x`:
  // Found self.X, Required T.this.X): only y — 1, as with the parameter renamed.
  def selfShadow = {
    val found = entries("""
      trait R:
        type X
      trait T:
        self =>
        type X
        final def f(self: R)(x: self.X, y: X): X = ???
        final def g(other: R)(x: other.X, y: X): X = ???
    """)
    (found("T.f") === Count.Finite(1)).and(found("T.g") === Count.Finite(1))
  }

  // A local `val p: R = ???` is no one's choice: no caller supplies p.X — refused (true count 1).
  def localValue =
    refused(
      entries("""
        trait R:
          type X
        def outer(): Int =
          val p: R = ???
          def inner(x: p.X, y: p.X): p.X = ???
          0
      """)("outer.inner"),
      "abstract type member p#X is fixed: p is a local value"
    )

  // A trait with no term member has the empty record: h — 1 (the result is one value), k — the 4
  // functions of b. Every template a parameter, parent, self type or constructor modifier may give
  // a term member — or no instance at all (`CNothing(n: Nothing)`, CROSSCHECK X6) — keeps the
  // abstract-representation reason.
  def typesOnly = {
    val found = entries("""
      trait T
      def h(t: T, u: T): T = ???
      def k(t: T, b: Boolean): Boolean = ???
      class CNothing(n: Nothing)
      def nothing(c: CNothing): Boolean = ???
      abstract class Abs(n: Nothing)
      def abs(c: Abs): Boolean = ???
      class Priv private ()
      def priv(c: Priv): Boolean = ???
      trait Base:
        def b: Int
      trait Sub extends Base:
        type X
      def sub(c: Sub): Boolean = ???
      trait Selfy:
        self: Base =>
        type X
      def selfy(c: Selfy): Boolean = ???
    """)
    // Each keeps the capability record's own reason (CNothing has no instance — X6 — and its
    // hidden parameter is what the record cannot witness). Sub's unbounded X opens as the witness
    // of the package each value carries: the record's observable content is `b`'s Int, and sub's
    // implementations are the functions from it to Boolean — 2^(2^32); the witness is unobservable
    // and adds nothing.
    val reasons = List(
      "open class with hidden constructor state: CNothing.n",
      "open class with hidden constructor state: Abs.n",
      "capability record of a template only its scope instantiates: Priv",
      "self type not resolved: Base"
    )
    val open = List("nothing", "abs", "priv", "selfy")
      .zip(reasons)
      .map((name, reason) => found(name) === Count.Unresolved(List(reason)))
    (found("h") === Count.Finite(1))
      .and(found("k") === Count.Finite(4))
      .and(found("sub") === Count.Symbolic(pow(2, BigInt("4294967296"))))
      .and(found("CNothing.<init>") === Count.Finite(0))
      .and(open.reduce(_ and _))
  }

  def refinementFacts =
    (typeChecks("val c: Boolean { type X } = false") must beFalse)
      .and(
        typeChecks(
          "val d: cardinality.analysis.resolution.memberfixture.FinalX { type X = String } = " +
            "new cardinality.analysis.resolution.memberfixture.FinalX"
        ) must beFalse
      )
      .and(
        typeChecks(
          "val e = new cardinality.analysis.resolution.memberfixture.OpenAlias " +
            "{ override type X = String }"
        ) must beFalse
      )
      .and(
        typeChecks("class W extends cardinality.analysis.resolution.memberfixture.Value(1)") must
          beFalse
      )
      .and(
        typeChecks("val p = new cardinality.analysis.resolution.memberfixture.Private()") must
          beFalse
      )

  private val projections =
    "type Fst[T] = T match { case (f, s) => f }\n" +
      "type Fst3[T] = T match { case (a, b, c) => a }\n"

  // No instantiation of M's X is both a pair and a triple: refused, the member named by its path
  // (the atom's key spelled a source offset, `100#X`).
  def spelledMember =
    refused(
      entries(projections + """
        trait M:
          type X
          def pick(x: Fst[X], y: Fst[X], z: Fst3[X]): Fst[X] = ???
      """)("M.pick"),
      "stuck match types on M.this#X have no common reducing instantiation: arities 2, 3"
    )

  // `case Int` before `case _` needs a disjointness proof the reading does not have (MT2 reduces
  // only a first `case _`): with A = Int, K2[A] is Boolean, otherwise Option[A].
  def typePattern =
    refused(
      entries("""
        type K2[T] = T match
          case Int => Boolean
          case _   => Option[T]
        def later[A](x: A): K2[A] = ???
      """)("later"),
      "inert match type"
    )

  // Mid's `var get: A = a` holds a (mutation is excluded), so in the final D it is that code, as
  // Mid2's `val` is: other has a — 1 (left free, get: A gave 2).
  def varImplements = {
    val found = entries("""
      trait Q[A]:
        def get: A
      abstract class Mid[A](a: A) extends Q[A]:
        var get: A = a
      final class D[A](a: A) extends Mid[A](a):
        def other: A = ???
      abstract class Mid2[A](a: A) extends Q[A]:
        val get: A = a
      final class D2[A](a: A) extends Mid2[A](a):
        def other: A = ???
    """)
    (found("D.other") === Count.Finite(1)).and(found("D2.other") === Count.Finite(1))
  }

  // Mid's to reads mk's a, which D.other cannot name: refused rather than dropped.
  def localImplementer =
    refused(
      entries("""
        trait Q[A]:
          def to(a: A): A
        def mk[A](a: A, b: A): Int =
          abstract class Mid extends Q[A]:
            def to(c: A): A = a
          final class D extends Mid:
            def other(c: A): A = ???
          0
      """)("mk.D.other"),
      "inherited declaration to is implemented in local Mid, whose captures are not read"
    )

  // `extends Optic[S, A, Affine, Unit]` with `import q.Affine`: the closure for F is applied with
  // Opt.scala's imports, as the qualified `q.Affine` is — never `unresolved type: Affine`. `to(s)`
  // is the one-case sum `Miss(())`, eliminated once (capabilities C10): it binds no `A`, so `get`
  // has only `None` — 1.
  def closureImports = {
    val affine = "q.scala" -> ("package q\nsealed trait Affine[A, +B]\n" +
      "final case class Miss[A](a: Unit) extends Affine[A, Nothing]")
    def get(parent: String) = entries(
      affine,
      "p.scala" -> s"""package p
        |import q.Affine
        |trait Optic[S, A, F[_, _], X]:
        |  def to(s: S): F[X, A]
        |abstract class Opt[S, A] extends Optic[S, A, $parent, Unit]:
        |  def get(s: S): Option[A] = ???
        |""".stripMargin
    )("p.Opt.get")
    (get("Affine") === Count.Finite(1)).and(get("q.Affine") === Count.Finite(1))
  }

}
