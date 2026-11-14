package cardinality.analysis.resolution

import scala.compiletime.testing.typeChecks
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.analysis.inhabitation.NaturalSyntax.pow
import org.specs2.Specification
import org.specs2.execute.Result

// The boundary of the type-member reading (type-members design, review of B2): which `this` a path
// names, which members are free, which refinements describe a value. Every fixture is valid Scala
// 3.9.0; the compiler facts the refusals rest on are pinned against `memberfixture`.
class TypeMemberBoundarySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Paths
      an outer `O.this` or self alias stays the enclosing instance  $outerThis
      a parameter hides an enclosing template's self alias          $selfShadow
      a value fixed by its own definition is no choice              $localValue
    Members
      a member some level declares as a class is no atom            $classMember
      a value class or a private constructor fixes its members      $unsubclassable
      a parent's private alias is that parent's own                 $privateAlias
      a type-only template with no constructor input is one value  $typesOnly
      a type projection of an abstract member has no path           $projection
    Refinements
      a refinement no value of its base may have is refused         $unsatisfiable
      the compiler rejects the refused refinements                  $refinementFacts
    Stuck projections
      a member scrutinee is spelled by its path                     $spelledMember
      a bound mentioning the scrutinee refuses                      $boundedScrutinee
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

  // `O.this.X` and `outer.X` in P name the enclosing O's member, never the receiver C's, though C
  // extends O (scalac: Found O.this.X, Required C.this.X): only y has C.this.X — 1 (reading P's
  // `O.this` as C's `this` gave 2). The lexical `X` and a nested template's own `I.this` agree.
  def outerThis = {
    def pick(declared: String, self: String = "") = entries(s"""
      trait O:
        $self
        type X
        trait P:
          def get: $declared
        trait C extends P with O:
          def pick(y: C.this.X): C.this.X = ???
      trait N:
        type X
        trait I extends N:
          def f(x: I.this.X, y: N.this.X): I.this.X = ???
    """)
    val viaThis = pick("O.this.X")
    (viaThis("O.C.pick") === Count.Finite(1))
      .and(pick("outer.X", "outer =>")("O.C.pick") === Count.Finite(1))
      .and(pick("X")("O.C.pick") === Count.Finite(1))
      .and(viaThis("N.I.f") === Count.Finite(1))
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

  // A class member cannot be overridden (scalac: "class definitions cannot be overridden"), so
  // C.this.X is always the class: never a free atom (it read 0 for `new X(true)`, 2 for `a, b`).
  // The class read by its own name is the class: X has no field — 1.
  def classMember = {
    val found = entries("""
      trait T:
        type X
      class C extends T:
        class X
        def viaThis(a: this.X, b: this.X): this.X = ???
        def viaName(a: X, b: X): X = ???
      class D extends T:
        final class X(val n: Boolean)
        def fresh: this.X = ???
    """)
    refused(found("C.viaThis"), "type member is a class: C.this#X")
      .and(found("C.viaName") === Count.Finite(1))
      .and(refused(found("D.fresh"), "type member is a class: D.this#X"))
  }

  // A value class is final ("cannot extend final class V") and a private constructor admits no
  // subclass outside C and its companion (E173): X is never picked, f never applied — vacuously 1,
  // where the atom gave 2^(2^32) and 2. Refused, as for an explicit `final class`.
  def unsubclassable = {
    val found = entries("""
      class V(val n: Int) extends AnyVal:
        type X
        def f(x: X, y: X): X = ???
      class C private ():
        type X
        def f(x: X, y: X): X = ???
      object C:
        def make: C = new C()
    """)
    refused(found("V.f"), "abstract type member X of final template V is never instantiated")
      .and(
        refused(found("C.f"), "abstract type member X of C is fixed: its constructor is private")
      )
  }

  // P's `private type X = Boolean` is not inherited: P's get is a Boolean (scalac: Found Boolean,
  // Required C.this.X), and C's X is Q's. same: y — 1; pick: true, false, get, !get — 4 (it read 2,
  // 2 with get as C.this.X). C2's own `to(x: X)` is another overload than P2's `to(x: Boolean)`:
  // to(true), to(false) — 2.
  def privateAlias = {
    val found = entries("""
      trait Q:
        type X
      abstract class P:
        private type X = Boolean
        def get: X
      abstract class C extends P with Q:
        final def pick(y: X): Boolean = ???
        def same(y: X): X = ???
      abstract class P2[A]:
        private type X = Boolean
        def to(x: X): A
      abstract class C2[A] extends P2[A] with Q:
        def to(x: X): A
    """)
    (found("C.pick") === Count.Finite(4))
      .and(found("C.same") === Count.Finite(1))
      .and(found("C2.to") === Count.Finite(2))
  }

  // A trait with no term member has the empty record: h — 1 (the result is one value), k — the 4
  // functions of b. Every template a parameter, parent, self type or constructor modifier may give
  // a term member — or no instance at all (`CNothing(n: Nothing)`, cross-check X6) — keeps the
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
    // hidden parameter is what the record cannot witness; nothing extends it, so it is final in the
    // closed world; Priv is final too, and no factory builds it: rule B). Sub's unbounded X opens as
    // the witness
    // of the package each value carries: the record's observable content is `b`'s Int, and sub's
    // implementations are the functions from it to Boolean — 2^(2^32); the witness is unobservable
    // and adds nothing.
    val reasons = List(
      "final class field not observable injectively: CNothing.n",
      "open class with hidden constructor state: Abs.n",
      "receiver has no public constructor: Priv",
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

  def projection =
    refused(
      entries("trait P:\n  type X\ndef h(x: P#X, y: P#X): P#X = ???")("h"),
      "type projection of abstract member P#X: no stable path"
    )

  // Each parameter type has no value (scalac rejects true, false, new F, and every subclass or
  // instance of the bad bounds — `refinementFacts`): every implementation is vacuously one, where
  // reading the base gave 16, 4, ω, 4, 4. Refused, as is `String <: X <: Int` across L and its
  // refinement. A refinement an open template may satisfy stays its base: p.X = A — 2; `X <: Tuple`
  // is met by Nothing, so p is an R (one value) beside b — 4.
  def unsatisfiable = {
    val found = entries("""
      final class F:
        type X = Int
      trait T:
        type X = Int
      trait R:
        type X
      def onBoolean(p: Boolean { type X }, b: Boolean): Boolean = ???
      def onFinal(p: F { type X = String }, b: Boolean): Boolean = ???
      def onFinalPath(p: F { type X = String }, s: String): p.X = ???
      def conflict(p: T { type X = String }, s: String): p.X = ???
      def structural(p: { type X >: Int <: String }, b: Boolean): Boolean = ???
      def based(p: R { type X >: Int <: String }, b: Boolean): Boolean = ???
      def open[A](p: R { type X = A }, a: A, x: p.X): A = ???
      trait L:
        type X >: String
      def upper(p: R { type X <: Tuple }, b: Boolean): Boolean = ???
      def clash(p: L { type X <: Int }, b: Boolean): Boolean = ???
    """)
    refused(found("onBoolean"), "Boolean { type X } (Boolean is not a supplied template)")
      .and(refused(found("onFinal"), "F { type X = String } (F is final)"))
      .and(refused(found("onFinalPath"), "(F is final)"))
      .and(refused(found("conflict"), "T { type X = String } (T already defines X)"))
      .and(refused(found("structural"), "non-exact bounds: type X >: Int <: String"))
      .and(refused(found("based"), "non-exact bounds: type X >: Int <: String"))
      .and(found("open") === Count.Finite(2))
      .and(found("upper") === Count.Finite(4))
      .and(refused(found("clash"), "L { type X <: Int } (L already defines X)"))
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

  // U <: T constrains T's instantiations: the projection's freedom is not the atom's.
  def boundedScrutinee =
    refused(
      entries(projections + "def b1[T, U <: T](x: Fst[T], u: U): Fst[T] = ???")("b1"),
      "stuck match types on T meet a bound that mentions it"
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
