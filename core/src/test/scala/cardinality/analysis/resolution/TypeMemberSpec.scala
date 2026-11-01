package cardinality.analysis.resolution

import scala.compiletime.testing.typeChecks
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// Members M0–M5, MT1′–MT3 and gate G1 (design/members.md). Every fixture is valid Scala 3.9.0
// unless marked; the compiler facts the readings rest on are pinned against `memberfixture`.
class TypeMemberSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Abstract members on stable paths
      a trait's own abstract member is a rigid atom              $ownAtom
      a member atom counts as a class type parameter (parity)    $parity
      two paths never share a member                             $distinctPaths
      a singleton alias, `this` and a self alias share the path   $samePath
      a refinement pins a parameter's member                     $refinementPins
      a type-only structural value has one value                 $structural
      a bounded or higher-kinded member stays a diagnostic       $boundedMember
      a member no one may pick stays a diagnostic                $fixedMember
      a value path is a choice only where it can be overridden    $valuePaths
      the compiler keeps paths apart and aliases together         $pathFacts
    Members of the receiver
      inherited declarations read the receiver's alias            $inheritedAlias
      an inherited alias body reads the receiver's members        $aliasThroughReceiver
      a parent's alias reads the receiver's instantiation         $parentInstantiation
      an inherited member shadows an outer alias                  $precedence
      an unsupplied parent may define the member                 $foreignParent
      override identity keys a member as the receiver's           $overrideIdentity
    Stuck projection match types
      a stuck projection is its own atom                         $projections
      alias-equal projections are one type                       $projCanonical
      projections without a common instantiation stay open       $projArity
      an irrefutable first case reduces                          $irrefutable
      projections on a member path                               $projOnPath
      a sealed family with projected fields                      $affineFamily
      tuple arguments reduce through the family (MT1′)           $affineConcrete
      nominal, bounded and bounded-scrutinee projections stay open $stuckLimits
      the compiler reads projections as the atoms do              $projectionFacts
    Gates
      a sealed receiver is not left unsplit (G2)                 $sealedReceiver
      a final root drops what a parent implements (G1)           $implementedInherited
  """

  private def entries(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Members.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def count(code: String, name: String): Count = entries(code)(name)

  private def refused(found: Count, reason: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(reason)) must beTrue
    }

  private val mono = """
    trait Mono[S, A]:
      type X
      def to(s: S): (X, A)
      def from(p: (X, A)): S
      def get(s: S): A = ???
  """

  // to: nothing but `to` yields an X — 0; from: nothing yields an S — 0; get: to(s)._2,
  // to(from(to(s)))._2, … — ω.
  def ownAtom = {
    val found = entries(mono)
    (found("Mono.to") === Count.Finite(0))
      .and(found("Mono.from") === Count.Finite(0))
      .and(found("Mono.get") === Count.Countable)
  }

  // A subclass picks X as a caller picks a type argument: the counts are the parameter's.
  def parity = {
    val asParameter = entries("""
      trait Mono[S, A, X]:
        def to(s: S): (X, A)
        def from(p: (X, A)): S
        def get(s: S): A = ???
    """)
    val asMember = entries(mono)
    List("Mono.to", "Mono.from", "Mono.get")
      .map(n => asMember(n) === asParameter(n))
      .reduce(_ and _)
  }

  private val member = "trait R { type X }\n"

  // A member-only trait has the empty record. f: nothing yields q.X — 0 (keyed by member name
  // alone it would read 1); g: x, y — 2; w: x, s(x), s(s(x)), … — ω.
  def distinctPaths = {
    val found = entries(
      member + """
      def f(p: R, q: R, x: p.X): q.X = ???
      def g(p: R, x: p.X, y: p.X): p.X = ???
      def w(p: R, s: p.X => p.X, x: p.X): p.X = ???
      """
    )
    (found("f") === Count.Finite(0))
      .and(found("g") === Count.Finite(2))
      .and(found("w") === Count.Countable)
  }

  // q: p.type denotes p, so q.X is p.X: x and y — 2 (unshared paths would read 1). Inside a trait
  // `this.X`, `outer.X` and `X` are one member: x, y — 2.
  def samePath =
    (count(member + "def h(p: R)(q: p.type)(x: q.X, y: p.X): q.X = ???", "h") === Count.Finite(2))
      .and(
        count(
          """
          trait Self[A]:
            outer =>
            type X
            def g(x: outer.X, y: this.X): X = ???
          """,
          "Self.g"
        ) === Count.Finite(2)
      )

  // p.X = A transparently: a and x — 2 (read rigidly, 1).
  def refinementPins =
    count(member + "def r[A](p: R { type X = A }, a: A, x: p.X): A = ???", "r") ===
      Count.Finite(2)

  // `{ type X }` asks nothing of a value: p adds no Boolean, so s has the 4 functions of b; t: x, y
  // — 2. A term member is selected through reflection, which the model excludes.
  def structural = {
    val found = entries("""
      def s(p: { type X }, b: Boolean): Boolean = ???
      def t(p: { type X }, x: p.X, y: p.X): p.X = ???
      def u(p: { def mk: Int }, b: Boolean): Boolean = ???
    """)
    (found("s") === Count.Finite(4))
      .and(found("t") === Count.Finite(2))
      .and(refused(found("u"), "structural refinement with term members"))
  }

  def boundedMember = {
    val found = entries("""
      trait B:
        type X <: Tuple
      def f(p: B, x: p.X): p.X = ???
      trait H:
        type F[_]
      def g(p: H, x: p.F[Int]): p.F[Int] = ???
      trait P:
        type X
      def h(x: P#X, y: P#X): P#X = ???
    """)
    refused(found("f"), "bounded abstract type member p#X (<: Tuple)")
      .and(refused(found("g"), "higher-kinded abstract type member p#F"))
      .and(refused(found("h"), "type projection of abstract member P#X: no stable path"))
  }

  // A final class or object never instantiates its member (`memberfixture.Never` compiles), so x
  // and y could never be supplied — the atom's 2 would overcount the vacuous 1. A sealed family's
  // cases fix it (S1 makes it Unit: 1, not 2).
  def fixedMember = {
    val found = entries("""
      trait HasX:
        type X
      final class F extends HasX:
        def g(x: X, y: X): X = ???
      sealed trait S:
        type X
      final class S1 extends S:
        type X = Unit
      def onSealed(p: S, x: p.X, y: p.X): p.X = ???
    """)
    refused(found("F.g"), "abstract type member X of final template F is never instantiated")
      .and(refused(found("onSealed"), "is fixed by the cases of sealed S"))
  }

  // An object's `val p: R` is fixed by its definition: no one can supply its X — refused (true
  // count 1, vacuous). An open template's abstract or `val` field may be narrowed by a subclass
  // (`override val p: R { type X = Int }`): x, y — 2. A final class's field cannot.
  def valuePaths = {
    val found = entries(member + """
      object Vals:
        val p: R = ???
        def v(x: p.X, y: p.X): p.X = ???
      trait Open:
        val p: R
        def v(x: p.X, y: p.X): p.X = ???
      class C(val p: R):
        def v(x: p.X, y: p.X): p.X = ???
      final class F(val p: R):
        def v(x: p.X, y: p.X): p.X = ???
      """)
    refused(found("Vals.v"), "p is not overridable in Vals")
      .and(found("Open.v") === Count.Finite(2))
      .and(found("C.v") === Count.Finite(2))
      .and(refused(found("F.v"), "p is not overridable in F"))
  }

  def pathFacts =
    (typeChecks(
      "def h(p: cardinality.analysis.resolution.memberfixture.R, " +
        "q: cardinality.analysis.resolution.memberfixture.R)(x: p.X): q.X = x"
    ) must beFalse)
      .and(
        typeChecks(
          "def h(p: cardinality.analysis.resolution.memberfixture.R)(q: p.type)(x: q.X): p.X = x"
        ) must beTrue
      )
      .and(
        typeChecks("val n = new cardinality.analysis.resolution.memberfixture.Never") must beTrue
      )

  // U's alias X = A is the meaning of the inherited to: A => X, so pick iterates: a, to(a), … — ω.
  // With X = Unit, to adds no A — 1.
  def inheritedAlias = {
    def pick(x: String) = count(
      s"""
      trait Q[A]:
        type X
        def to(a: A): X
      trait U[A] extends Q[A]:
        type X = $x
        def pick(a: A): A = ???
      """,
      "U.pick"
    )
    (pick("A") === Count.Countable).and(pick("Unit") === Count.Finite(1))
  }

  // Mid's X = (Unit, M) is read with Leaf's M = M0: to(s)._2 — 1 (read with Mid's own M, 0). Where
  // Leaf2 leaves M abstract it is Leaf2's: to(s)._2 again — 1.
  def aliasThroughReceiver = {
    val found = entries("""
      trait Base[S]:
        type X
        def to(s: S): X
      trait Mid[S] extends Base[S]:
        type M
        type X = (Unit, M)
      trait Leaf[S, M0] extends Mid[S]:
        type M = M0
        def pick(s: S): M0 = ???
      trait Leaf2[S] extends Mid[S]:
        def pick(s: S): M = ???
    """)
    (found("Leaf.pick") === Count.Finite(1)).and(found("Leaf2.pick") === Count.Finite(1))
  }

  // P's Y = (A, A) is a member of R[B], whose A is B: get._1, get._2 — 2 (read with P's own A, 0).
  def parentInstantiation =
    count(
      """
      trait P[A]:
        type Y = (A, A)
        def get: Y
      trait R[B] extends P[B]:
        def other: B = ???
      """,
      "R.other"
    ) === Count.Finite(2)

  // Scala reads the inherited alias (compiled: `memberfixture.Reads`): true, false — 2 (W1 read
  // the package's Unit: 1). An inherited abstract member shadows it too: nothing yields it — 0.
  def precedence = {
    def g(declaration: String, template: String) = count(
      s"""
      type X = Unit
      trait HasX:
        $declaration
      $template C extends HasX:
        def g(): X = ???
      """,
      "C.g"
    )
    (g("type X = Boolean", "final class") === Count.Finite(2))
      .and(g("type X", "abstract class") === Count.Finite(0))
  }

  // An unsupplied parent may declare X, or define the member a supplied parent leaves abstract
  // (`foreign.Base` is a dependency the sources do not supply).
  def foreignParent = {
    val found = entries("""
      import foreign.Base
      type X = Unit
      trait HasX:
        type X
      abstract class C extends Base:
        def g(): X = ???
      abstract class D extends HasX with Base:
        def g(x: X, y: X): X = ???
    """)
    refused(found("C.g"), "unsupplied parent Base")
      .and(refused(found("D.g"), "type member X of D.this may be defined by unsupplied parent"))
  }

  // Mid.to overrides Q.to: both signatures key X as Mid's alias A, so the override is found. Mid is
  // open, so its own `from` stays an overridable capability: b, a, from(b), … — ω. Without `from`
  // the twin tells a found override (b, a — 2) from Q2.to read as a second capability A => A (ω).
  def overrideIdentity =
    (count(
      """
      trait Q[A]:
        type X
        def to(a: A): X
        def from(x: X): A
      abstract class Mid[A](a: A) extends Q[A]:
        type X = A
        def to(b: A): X = ???
        def from(x: X): A = x
      """,
      "Mid.to"
    ) === Count.Countable).and(
      count(
        """
        trait Q2[A]:
          type X
          def to(a: A): X
        abstract class Mid2[A](a: A) extends Q2[A]:
          type X = A
          def to(b: A): X = ???
        """,
        "Mid2.to"
      ) === Count.Finite(2)
    )

  private val projectionTypes =
    "type Fst[T] = T match { case (f, s) => f }\n" +
      "type Snd[T] = T match { case (f, s) => s }\n"

  // One atom per (arity, component, scrutinee): x — 1; x, y — 2; nothing yields Fst[T] from a T
  // (the compiler rejects `x`) — 0; nor Snd[T] from Fst[T] — 0.
  def projections = {
    val found = entries(projectionTypes + """
      def one[T](x: Fst[T]): Fst[T] = x
      def two[T](x: Fst[T], y: Fst[T]): Fst[T] = x
      def free[T](x: T): Fst[T] = ???
      def other[T](x: Fst[T]): Snd[T] = ???
    """)
    (found("one") === Count.Finite(1))
      .and(found("two") === Count.Finite(2))
      .and(found("free") === Count.Finite(0))
      .and(found("other") === Count.Finite(0))
  }

  def projCanonical =
    count(
      projectionTypes + "type First[T] = T match { case (a, b) => a }\n" +
        "def pick[T](x: First[T]): Fst[T] = x",
      "pick"
    ) === Count.Finite(1)

  // No instantiation of T is both a pair and a triple: no caller supplies all three, so every
  // implementation is vacuously one — the atoms' 2 would overcount.
  def projArity =
    refused(
      count(
        projectionTypes + "type Fst3[T] = T match { case (a, b, c) => a }\n" +
          "def pick[T](x: Fst[T], y: Fst[T], z: Fst3[T]): Fst[T] = ???",
        "pick"
      ),
      "stuck match types on T have no common reducing instantiation: arities 2, 3"
    )

  // K[A] = Option[A] for every A: None, Some(x), Some(y) — 3.
  def irrefutable =
    count(
      "type K[T] = T match { case _ => Option[T] }\ndef pick[A](x: A, y: A): K[A] = None",
      "pick"
    ) === Count.Finite(3)

  // x — 1; Fst[q.X] has no producer — 0.
  def projOnPath = {
    val found = entries(member + projectionTypes + """
      def f(p: R, x: Fst[p.X], y: Snd[p.X]): Fst[p.X] = x
      def g(p: R, q: R, x: Fst[p.X]): Fst[q.X] = ???
    """)
    (found("f") === Count.Finite(1)).and(found("g") === Count.Finite(0))
  }

  private val affine = projectionTypes + """
    sealed trait Affine[A, +B]
    final class Miss[A](val fst: Fst[A]) extends Affine[A, Nothing]
    final class Hit[A, +B](val snd: Snd[A], val b: B) extends Affine[A, B]
    object Ops:
      def map[X, A, B](fa: Affine[X, A], f: A => B): Affine[X, B] = ???
      def getOption[X, A](fa: Affine[X, A]): Option[A] = ???
      def swap[X, A](fa: Affine[X, A]): Affine[(Snd[X], Fst[X]), A] = ???
      def same[A](fa: Affine[(A, A), A]): Affine[(A, A), A] = ???
      def back[S, A](fa: Affine[(S, Unit), A], s: S): S = ???
  """

  // Each field is its own position's only value — 1, 1. map: Miss(fst) — 1; Hit(snd, f(b)) — 1:
  // 1. getOption: Miss → None; Hit → None or Some(b): 1 · 2. swap: a Miss carries a Fst[X], and
  // the result needs a Snd[X] for a Miss or an A for a Hit — 0.
  def affineFamily = {
    val found = entries(affine)
    (found("Miss.<init>") === Count.Finite(1))
      .and(found("Hit.<init>") === Count.Finite(1))
      .and(found("Ops.map") === Count.Finite(1))
      .and(found("Ops.getOption") === Count.Finite(2))
      .and(found("Ops.swap") === Count.Finite(0))
  }

  // same: Miss(a) → Miss(a), Hit(a, a) — 2; Hit(s, a) → Miss over {s, a}, Hit over {s, a}² — 6;
  // 2 · 6 = 12 (stuck components would read 1). back: Miss(fs) → fs, s; Hit → s: 2 · 1.
  def affineConcrete = {
    val found = entries(affine)
    (found("Ops.same") === Count.Finite(12)).and(found("Ops.back") === Count.Finite(2))
  }

  def stuckLimits = {
    val found = entries(projectionTypes + """
      case class Pair[A, B](x: A, y: B)
      def nominal[A, B](x: Fst[Pair[A, B]]): Fst[Pair[A, B]] = ???
      type BFst[T] <: Any = T match { case (f, s) => f }
      def boundedMatch[T](x: BFst[T]): BFst[T] = x
      def boundedScrutinee[T <: (Int, String)](x: Fst[T]): Fst[T] = x
    """)
    refused(found("nominal"), "inert match type")
      .and(refused(found("boundedMatch"), "a bounded match type stays stuck on T"))
      .and(refused(found("boundedScrutinee"), "inert match type"))
  }

  def projectionFacts =
    (typeChecks("def pick[T](x: T): cardinality.analysis.resolution.memberfixture.Fst[T] = x") must
      beFalse)
      .and(
        typeChecks(
          "def pick[T](x: cardinality.analysis.resolution.memberfixture.Fst[T]): " +
            "cardinality.analysis.resolution.memberfixture.Snd[T] = x"
        ) must beFalse
      )
      .and(
        typeChecks(
          "def pick[T](x: cardinality.analysis.resolution.memberfixture.First[T]): " +
            "cardinality.analysis.resolution.memberfixture.Fst[T] = x"
        ) must beTrue
      )

  // fold's derived count is 1 (split this: onMiss(fst), onHit(snd, b)); unsplit, the projections
  // would read 0.
  def sealedReceiver =
    refused(
      count(
        projectionTypes + """
        sealed trait Affine[A, +B]:
          def fold[C](onMiss: Fst[A] => C, onHit: (Snd[A], B) => C): C = ???
        final class Miss[A](val fst: Fst[A]) extends Affine[A, Nothing]
        final class Hit[A, +B](val snd: Snd[A], val b: B) extends Affine[A, B]
        """,
        "Affine.fold"
      ),
      "receiver of sealed family Affine"
    )

  // Mid implements Q's to and from (identities on A), so in the final D they are that code: other
  // has a — 1 (left free, to: A => A gives ω). D2 implements them itself — 1. The open O may be
  // extended with other to and from: a, to(a), … — ω.
  def implementedInherited = {
    val found = entries("""
      trait Q[A]:
        type X
        def to(a: A): X
        def from(x: X): A
      abstract class Mid[A](a: A) extends Q[A]:
        type X = A
        def to(b: A): X = b
        def from(x: X): A = x
      final class D[A](a: A) extends Mid[A](a):
        def other: A = ???
      class O[A](a: A) extends Mid[A](a):
        def other: A = ???
      final class D2[A](a: A) extends Q[A]:
        type X = A
        def to(b: A): X = b
        def from(x: X): A = x
        def other: A = ???
    """)
    (found("D.other") === Count.Finite(1))
      .and(found("O.other") === Count.Countable)
      .and(found("D2.other") === Count.Finite(1))
  }

}
