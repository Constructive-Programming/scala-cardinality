package cardinality.analysis.resolution

import scala.compiletime.testing.typeChecks
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// The scope a member reading binds names in (second review of B2): refinement bodies, fields read
// for a receiver, templates only some scope may extend. Every fixture is valid Scala 3.9.0 unless
// marked (`$S/probes/b2fix2/spec`); the compiler facts are pinned against `memberfixture`.
class TypeMemberScopeSpec extends Specification {
  import Inhabitation.Count
  import NaturalSyntax.pow

  def is = s2"""
    Refinement bodies
      `this` and the refined value's own members are the value's   $refinedThis
      a name neither the refinement nor its base declares is outer  $refinedOuter
      a projection through `this` or an owner's member is refused   $refinedProjection
    Fields in member aliases
      a field is the receiver's, at its most derived type           $receiverField
    Templates only some scope may extend
      private, local and final abstract templates fix their members $closedScopes
      an ancestor's private constructor fixes a member               $ancestorConstructor
      a sealed family with an open case does not fix its member      $openFamily
      a refinement no value of its base may have is refused         $unsatisfiable
      an opaque alias spelled through `this` is not followed          $opaqueThroughThis
    Types-only values
      two bounds or `final abstract` keep the template open          $boundedTemplates
    Stuck projections
      type evidence about the scrutinee refuses                     $evidence
    Refinements with term members
      a base's own term keeps the erasure reason; a new one reflects $termRefinement
    Override identity
      a member keys alike however it is spelled                     $spelledOverride
    Compiler facts                                                  $facts
  """

  private def entries(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Scopes.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refused(found: Count, reason: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(reason)) must beTrue
    }

  private def refusal(reason: String): Count = Count.Unresolved(List(reason))

  private val members = "trait T { type X; type Y }\n"

  // Scala types `T { … }` as a class extending T whose `this` is the refined value, so `this.Y` and
  // `Y` there are p.Y, not the enclosing Boolean (scalac: Found p.X, Required Boolean — `facts`).
  // viaThis: x, y — 2 (it read 16 with X = Boolean); viaThisY: the 4 functions of y; own and
  // ownThis: X = Unit — 1; C.f as viaThis — 2; outer: W is A's Boolean — 16; structural: 1.
  def refinedThis = {
    val found = entries(members + """
      object A:
        type Y = Boolean
        def viaThis(p: T { type X = this.Y }, x: p.X, y: p.X): p.X = ???
        def viaThisY(p: T { type X = this.Y }, x: p.X, y: Boolean): Boolean = ???
        def own(p: T { type Y = Unit; type X = Y }, x: p.X, y: p.X): p.X = ???
        def ownThis(p: T { type Y = Unit; type X = this.Y }, x: p.X, y: p.X): p.X = ???
        def structural(p: { type Y = Unit; type X = Y }, x: p.X, y: p.X): p.X = ???
      abstract class C:
        type Y = Boolean
        def f(p: T { type X = this.Y }, x: p.X, y: p.X): p.X
    """)
    (found("A.viaThis") === Count.Finite(2))
      .and(found("A.viaThisY") === Count.Finite(4))
      .and(found("A.own") === Count.Finite(1))
      .and(found("A.ownThis") === Count.Finite(1))
      .and(found("A.structural") === Count.Finite(1))
      .and(found("C.f") === Count.Finite(2))
  }

  // W is declared by neither, so it is A's Boolean: (Boolean, Boolean) => Boolean — 16. A term
  // member of the base (`q`) starts a path through the refined value, which is not followed.
  def refinedOuter = {
    val found = entries(members + """
      trait V:
        type X
        val q: T
      object A:
        type W = Boolean
        def outer(p: T { type X = W }, x: p.X, y: p.X): p.X = ???
        def valMember(p: V { type X = q.Y }, x: p.X, y: p.X): p.X = ???
    """)
    (found("A.outer") === Count.Finite(16))
      .and(refused(found("A.valMember"), "path through member q of refined p"))
  }

  // In a projection `R#X` the refined value has no path: R#X is R#Y = Unit (scalac accepts `: Unit
  // = x`), and Q#X is P's Y. Read in the enclosing scope both gave Boolean — refused instead.
  def refinedProjection = {
    val found = entries(members + """
      trait P:
        type Y = Unit
      trait Q extends P:
        type X = Y
      object A:
        type Y = Boolean
        type R = T { type Y = Unit; type X = this.Y }
        def proj(x: R#X, y: R#X): R#X = ???
        def projQ(x: Q#X, y: Q#X): Q#X = ???
    """)
    (found("A.proj") === refusal("refined member X refers to its own value: this.Y"))
      .and(found("A.projQ") === refusal("refined member X refers to its owner's member Y"))
  }

  private val withField = "trait U { type Y }\nabstract class T { val p: U; type X = p.Y }\n"

  // X = p.Y is read on the receiver's p: In.this.p.Y and T.this.p.Y are two types (scalac: Found
  // T.this.X, Required In.this.X), so In.f has x only — 1 (one key for both read 2). G narrows p
  // (`facts`): X = Boolean — 16 (read through T's `val p: U` it was 2). F is final and fixes p
  // (`= u`): no caller supplies its X — refused (true count 1), as the same type spelled p.Y is.
  def receiverField = {
    val found = entries(withField + """
      abstract class O extends T:
        abstract class In extends T:
          def f(x: X, y: O.this.X): X
      abstract class G extends T:
        val p: U { type Y = Boolean }
        def f(x: X, y: X): X
      final class F(u: U) extends T:
        val p: U = u
        def f(x: X, y: X): X = ???
    """)
    (found("O.In.f") === Count.Finite(1))
      .and(found("G.f") === Count.Finite(16))
      .and(refused(found("F.f"), "abstract type member p#Y is fixed: p is not overridable in F"))
  }

  // Only O may extend P, R or Q's T (scalac: "can only be accessed from object O"), only m's body
  // may extend L, and nothing extends FA: no subclass picks X, so each f is vacuously one, where
  // the atom gave 2. A `private[pk]` template is open to its whole package: x, y — 2.
  def closedScopes = {
    val found = entries("""
      package pk
      object O:
        private class P:
          type X
          def f(x: X, y: X): X = ???
        private object Q:
          trait T:
            type X
            def g(x: X, y: X): X
        private[O] class R:
          type X
          def h(x: X, y: X): X = ???
        private[pk] class Open:
          type X
          def k(x: X, y: X): X = ???
      final abstract class FA:
        type X
        def f(x: X, y: X): X
      object M:
        def m(): Int =
          trait L:
            type X
            def k(x: X, y: X): X
          0
    """)
    (found("pk.O.P.f") === refusal("abstract type member X of P is fixed: P is private"))
      .and(found("pk.O.Q.T.g") === refusal("abstract type member X of T is fixed: Q is private"))
      .and(found("pk.O.R.h") === refusal("abstract type member X of R is fixed: R is private"))
      .and(found("pk.O.Open.k") === Count.Finite(2))
      .and(
        found("pk.FA.f") === refusal(
          "abstract type member X of final template FA is never instantiated"
        )
      )
      .and(found("pk.M.m.L.k") === refusal("abstract type member X of L is fixed: L is local"))
  }

  // A subclass of Below must call Base's private constructor (`facts`): only Base's own scope may
  // extend it — vacuously 1, where the atom gave 2.
  def ancestorConstructor =
    entries("""
      class Base private ()
      object Base:
        def make: Base = new Base()
      trait Below extends Base:
        type X
        def m(x: X, y: X): X = ???
    """)("Below.m") === refusal(
      "abstract type member X of Below is fixed: the constructor of Base is private"
    )

  // Any file may extend Open and pick X (a second file `class Elsewhere extends Open { type X =
  // Boolean }` compiles beside it): X is not fixed by S's cases. Only the open case's reason is true.
  def openFamily =
    entries("""
      sealed trait S:
        type X
      trait Open extends S
      def h(p: S, x: p.X, y: p.X): p.X = ???
    """)("h") === refusal("sealed subtype outside the closed fragment: Open")

  // A sealed base's cases, a private constructor and a higher-kinded refined member are refused at
  // the refinement; so is a refinement of a template only its own scope may extend.
  def unsatisfiable = {
    val found = entries("""
      sealed trait SS:
        type X
      final class S1 extends SS:
        type X = Int
      class Priv private ():
        type X
      trait R:
        type X
      def onSealed(p: SS { type X = Int }, b: Boolean): Boolean = ???
      def onPriv(p: Priv { type X = Int }, b: Boolean): Boolean = ???
      def hk(p: R { type F[_] }, b: Boolean): Boolean = ???
      def hkAlias(p: R { type F[A] = Option[A] }, b: Boolean): Boolean = ???
    """)
    val below = entries("""
      class Base private ()
      object Base:
        def make: Base = new Base()
      trait Below extends Base:
        type X
      def below(p: Below { type X = Int }, b: Boolean): Boolean = ???
    """)("below")
    refused(found("onSealed"), "SS { type X = Int } (SS is sealed)")
      .and(refused(found("onPriv"), "Priv { type X = Int } (Priv has a private constructor)"))
      .and(found("hk") === refusal("higher-kinded refined member: F"))
      .and(found("hkAlias") === refusal("higher-kinded or opaque refined member: F"))
      .and(refused(below, "Below { type X = Int } (Base has a private constructor)"))
  }

  // Inside O the opaque X is Int: g reads it so — (2^32)^(2^64) = 2^(2^69). Spelled `this.X` it is a member
  // alias the path reading does not follow: refused rather than read another way.
  def opaqueThroughThis = {
    val found = entries("""
      object O:
        opaque type X = Int
        def f(x: this.X, y: this.X): this.X = ???
        def g(x: X, y: X): X = ???
    """)
    (found("O.f") === refusal("type member alias not followed: O.this#X"))
      .and(found("O.g") === Count.Symbolic(pow(2, pow(2, 69))))
  }

  // KT and K have a member with two bounds and FA no instance (`facts`): g and make have no
  // implementation and f one (the bounds witness A <: B), where the empty record gave 1, 1, 0.
  // One-sided bounds are met by Nothing and Any: OneSided is one value — 1.
  def boundedTemplates = {
    val found = entries("""
      trait KT:
        type X >: Int <: String
      def g(): KT = ???
      trait K[A, B]:
        type X >: A <: B
      def f[A, B](k: K[A, B], a: A): B = ???
      final abstract class Never
      def make(): Never = ???
      trait OneSided:
        type X <: String
        type Y >: Int
      def one(o: OneSided, p: OneSided): OneSided = ???
    """)
    (found("g") === refusal("abstract type or method-valued representation: KT"))
      .and(found("f") === refusal("abstract type or method-valued representation: K"))
      .and(found("make") === refusal("abstract type or method-valued representation: Never"))
      .and(found("one") === Count.Finite(1))
  }

  // `e.substituteCo[Fst](x)` is a Fst[U] (`facts`), which the atom proj(T) hides from `=:=`: 1, not
  // 0 — refused.
  def evidence =
    entries("""
      type Fst[T] = T match { case (f, s) => f }
      def ev[T, U](x: Fst[T], e: T =:= U): Fst[U] = ???
    """)("ev") === refusal("stuck match types on T meet type evidence that mentions it")

  // `bar` is Foo's own member, selected without reflection: the refinement's constraint is what is
  // not read. `baz` is no member of Foo: selecting it needs reflection, which the model excludes.
  def termRefinement = {
    val found = entries("""
      trait Foo:
        def bar: Any
      def narrowed(p: Foo { def bar: Int }, b: Boolean): Boolean = ???
      def added(p: Foo { def baz: Int }, b: Boolean): Boolean = ???
    """)
    (found("narrowed") === refusal(
      "refined representation unavailable (constraints not erased): Foo { def bar: Int }"
    )).and(
      found("added") === refusal("structural refinement with term members: Foo { def baz: Int }")
    )
  }

  // `this.Member` and `Member` are one member of the receiver, so either spelling overrides Base's
  // get: x — 1 (the selected spelling was left unresolved).
  def spelledOverride = {
    val found = entries("""
      trait Base[A]:
        type Member
        def get(a: Member): A
      abstract class Instance[A](x: A) extends Base[A]:
        def get(a: Member): A = ???
      abstract class Other[A](x: A) extends Base[A]:
        def get(a: this.Member): A = ???
    """)
    (found("Instance.get") === Count.Finite(1)).and(found("Other.get") === Count.Finite(1))
  }

  def facts =
    (typeChecks(
      "def f(p: cardinality.analysis.resolution.memberfixture.TY { type X = this.Y }, x: p.X): " +
        "p.Y = x"
    ) must beTrue)
      .and(
        typeChecks(
          "abstract class G extends cardinality.analysis.resolution.memberfixture.WithVal { " +
            "val p: cardinality.analysis.resolution.memberfixture.U { type Y = Boolean }; " +
            "def f(x: X): Boolean = x }"
        ) must beTrue
      )
      .and(
        typeChecks(
          "def e[T, U](x: cardinality.analysis.resolution.memberfixture.Fst[T], c: T =:= U): " +
            "cardinality.analysis.resolution.memberfixture.Fst[U] = " +
            "c.substituteCo[cardinality.analysis.resolution.memberfixture.Fst](x)"
        ) must beTrue
      )
      .and(
        typeChecks(
          "val a = new cardinality.analysis.resolution.memberfixture.FinalAbstract {}"
        ) must beFalse
      )
      .and(
        typeChecks(
          "class Sub extends cardinality.analysis.resolution.memberfixture.BelowClosed { type X = Int }"
        ) must beFalse
      )
      .and(
        typeChecks(
          "class S extends cardinality.analysis.resolution.memberfixture.Hidden.P"
        ) must beFalse
      )
      .and(
        typeChecks(
          "val b = new cardinality.analysis.resolution.memberfixture.Bounded {}"
        ) must beFalse
      )

}
