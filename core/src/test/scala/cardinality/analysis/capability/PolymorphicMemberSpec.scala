package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// Polymorphic members of supplied capabilities (design/capabilities.md C8, PB0–PB4). Every fixture
// is valid Scala 3.9.0 (compiled with scalac: probes/b4/spec poly1–poly4, probes/b4fix/spec inl and
// bounds, probes/b4fix2/spec aliases and paths). Each count is derived in the comment above its
// check.
class PolymorphicMemberSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Readings at the member, exact for an input and a goal (PB0, PB1)
      a binder the member never mentions is irrelevant              $phantom
      a phantom binder whose bounds may admit no type is refused    $phantomTwoSided
      a fold's binder is its focus: ∀M. (A → M) → S → M ≅ S → A    $fold
      a chooser is a tag: ∀M. M → M → M ≅ 2, as input and as goal   $chooser
      a binder only consumed is a constant: ∀M. M → Boolean ≅ 2     $constant
      a covariant result: ∀M. (A → M) → Option[M] ≅ Option[A]       $covariantResult
      a known identity adds nothing                                 $knownIdentity
      ∀C. C is empty: a supplied one is absurd, a goal one has none $absurd
      a tuple generator's domain is its product; a tuple parameter spreads $tupleGenerators
      a template alias is read at the record's own arguments       $templateAliases
      a path no binder reaches is an ordinary producer (PB0)        $monomorphicPaths
      records of unrelated traits are distinct evidence             $unrelatedEvidence
    Instantiation at the goal (PB2–PB4)
      a binder the goal fixes counts 0 or 1                         $resultDetermined
      more than one instantiated term is refused, never counted     $quotient
      a result no goal meets is irrelevant                          $irrelevant
      a binder the result does not fix is refused                   $undetermined
      an algebra premise (Monoid-like evidence) is never reduced    $algebra
      a goal's binders are fresh rigid atoms                        $skolem
      bounded and higher-kinded binders keep their reasons          $boundedAndKinded
      a context-bound binder is bounded                              $contextBound
      a bare-binder result that grows every goal exhausts the budget $growingChain
      a bare-binder result's compound instantiations are not searched $compoundInstantiation
    Deferred inline members (no term of a body that is not inline)
      one inhabited under every instantiation is dropped            $deferredInline
      one whose type may be empty is refused                        $deferredInlineEmpty
      an inline body calls the override of each expansion           $deferredInlineExpansion
  """

  private def counts(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Poly.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refused(found: Count, fragment: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  // `get[M](a: A): A` is `A ⇒ A` whatever M is (bounded or not): a free endomap — a, get(a), … — ω.
  def phantom = {
    val found = counts("""package poly1
      |trait Phantom[A] { def get[M](a: A): A }
      |trait BoundedPhantom[A] { def get[M <: AnyRef](a: A): A }
      |object U {
      |  def f[A](a: A, p: Phantom[A]): A = a
      |  def g[A](a: A, p: BoundedPhantom[A]): A = a
      |}
      |""".stripMargin)
    (found("poly1.U.f") === Count.Countable).and(found("poly1.U.g") === Count.Countable)
  }

  private val bounds = counts("""package bounds
    |trait T[A, X] { def f[M >: X <: String](a: A): A }
    |trait V[A] { def f[M >: Int <: String](a: A): A }
    |trait L[A] { def f[M >: Int](a: A): A }
    |trait Ord[M] { def le(x: M, y: M): Boolean }
    |trait CB { def fold[M: Ord](m: M): M }
    |trait Mk { def make[M](f: (Int, Boolean) => M): Option[M] }
    |trait Pairs { def choose[M](both: (M, M)): M }
    |trait Seesaw[F[_]] { def wrap[A](a: A): F[A]; def unwrap[A](fa: F[A]): A }
    |object U {
    |  def t[A, X](c: T[A, X], a: A): A = a
    |  def v[A](c: V[A], a: A): A = a
    |  def l[A](c: L[A], a: A): A = a
    |  def useCb[A](c: CB, a: A): A = a
    |  def made(m: Mk): Option[(Int, Boolean)] = None
    |  def pick[B](c: Pairs, a: B, b: B): B = a
    |  def cyc[F[_], B](w: Seesaw[F], b: B): F[B] = w.wrap(b)
    |}
    |""".stripMargin)

  // No M has Int <: M <: String, and none provably has X <: M <: String for a rigid X: scalac
  // rejects every call (probes/review-B4/pb0c), so the only A is `a` — true count 1, never the ω of
  // a callable f. A one-sided bound always has a solution (M = Int): f is a free endomap — ω.
  def phantomTwoSided =
    refused(bounds("bounds.U.t"), "polymorphic capability binder: M (bounded)")
      .and(refused(bounds("bounds.U.v"), "polymorphic capability binder: M (bounded)"))
      .and(bounds("bounds.U.l") === Count.Countable)

  // `make ≅ Option[Int × Boolean]` (the generator's two parameters are one domain): a supplied
  // closed value of 2^33 + 1, split uniformly; each branch builds Option[(Int, Boolean)] from its
  // 2^33 + 1 literals — (2^33 + 1)^(2^33 + 1). `choose[M](both: (M, M)): M` spreads to M → M → M,
  // a tag: split (2), each case a or b (2) — 4.
  def tupleGenerators =
    (bounds("bounds.U.made").render === "8589934593^8589934593")
      .and(bounds("bounds.U.pick") === Count.Finite(4))

  // fold needs an Ord[M] the member's clauses do not spell: refused (true count 1: no Ord in scope).
  def contextBound =
    refused(bounds("bounds.U.useCb"), "polymorphic capability binder: M (bounded)")

  // unwrap's result A meets every goal, F[B] first: at A := F[B] it needs F[F[B]], and so on — the
  // goals never repeat. True count 1 (unwrap ∘ wrap is ∀A. A → A, the identity, so wrap(b) is the
  // only F[B]); the budget refusal is the documented limit, never a number.
  def growingChain =
    refused(bounds("bounds.U.cyc"), "Inhabitation state budget exceeded")

  private val inl = counts("""package inl
    |trait T { inline def f[M](x: M): Boolean }
    |trait U { def f[M](x: M): Boolean }
    |trait T2 { inline def g(b: Boolean): Boolean }
    |trait T3[A] { inline def get: A }
    |trait T4[A] {
    |  inline def pick[M](f: A => M): M
    |  def m: A = ???
    |}
    |trait T5 {
    |  inline def choose[M](x: M, y: M): M
    |  def m[B](a: B, b: B): B = ???
    |}
    |trait N[A] { inline def neg: A => Nothing }
    |trait V { inline val flag: Boolean }
    |object W {
    |  def t[A](c: T, a: A): Boolean = true
    |  def u[A](c: U, a: A): Boolean = true
    |  def t2(c: T2): Boolean = true
    |  def t3[A](c: T3[A]): A = ???
    |  def t4[A](c: T4[A]): A = ???
    |  def n[A](c: N[A], a: A): Boolean = true
    |  def v(c: V): Boolean = true
    |  inline def i[A](c: T, a: A): Boolean = true
    |}
    |""".stripMargin)

  // scalac rejects `c.f(a)`, `c.g(true)`, `c.flag`, `this.choose(a, b)` in a body that is not inline
  // (probes/review-B4/inlc, inlc2, inl2c), so the members add nothing; their types (Boolean,
  // Boolean ⇒ Boolean, a tag) are built from nothing, so every receiver has an instance: t, t2, v
  // are true/false (2), T5.m is a or b (2). The ordinary twin `u` reads f ≅ Boolean: 4.
  // `V2.m` reads its own receiver's deferred `flag` (built from nothing, dropped): true or false — 2.
  def deferredInline =
    (inl("inl.W.t") === Count.Finite(2))
      .and(inl("inl.W.u") === Count.Finite(4))
      .and(inl("inl.W.t2") === Count.Finite(2))
      .and(inl("inl.T5.m") === Count.Finite(2))
      .and(inl("inl.W.v") === Count.Finite(2))
      .and(paths("paths.V2.m") === Count.Finite(2))

  // get: A and pick ≅ A have no value from nothing, and neg: A ⇒ Nothing beside a: A has none at all
  // (n's true count is 1: no instance of N[A] exists when an A does). Whether such a member is
  // implementable together with the rest is not decided, so each is refused (t3, t4, T4.m, V3.m —
  // its own receiver's `inline val a: A` — true 0).
  def deferredInlineEmpty = {
    val reason = "deferred inline member whose type may be empty: "
    refused(inl("inl.W.t3"), reason + "inl.T3.get")
      .and(refused(inl("inl.W.t4"), reason + "inl.T4.pick"))
      .and(refused(inl("inl.T4.m"), reason + "inl.T4.pick"))
      .and(refused(inl("inl.W.n"), reason + "inl.N.neg"))
      .and(refused(paths("paths.V3.m"), reason + "paths.V3.a"))
  }

  // An inline body may call c.f: each expansion calls the override of the receiver's static type
  // there (probes/b4fix/inl b.scala), which this reading does not see.
  def deferredInlineExpansion =
    refused(inl("inl.W.i"), "deferred inline member resolved at each expansion: inl.T.f")

  private val poly2 = counts("""package poly2
    |trait Fold[S, A] { def foldMap[M](f: A => M)(s: S): M }
    |trait Chooser { def choose[M](x: M, y: M): M }
    |trait Test { def test[M](m: M): Boolean }
    |trait Mk[A] { def make[M](f: A => M): Option[M] }
    |object U {
    |  def first[S, A](s: S, F: Fold[S, A]): A = F.foldMap(identity)(s)
    |  def pick[B](c: Chooser, a: B, b: B): B = c.choose(a, b)
    |  def chooser(): Chooser = new Chooser { def choose[M](x: M, y: M): M = x }
    |  def flag[A](t: Test, a: A): Boolean = t.test(a)
    |  def made[A](m: Mk[A]): Option[A] = m.make(identity)
    |}
    |""".stripMargin)

  // Memo P10: naive instantiation at M = A cycles (foldMap(a => foldMap(_ => a)(s))(s), …: ω), but
  // every parametric foldMap is f ∘ h for one h: S → A, and all those terms are h(s) — 1.
  def fold = poly2("poly2.U.first") === Count.Finite(1)

  // Memo P11: the tag t: 2 is split; each case picks a or b — 2 · 2 = 4. A Chooser to build is a
  // tag to choose: first or second — 2.
  def chooser =
    (poly2("poly2.U.pick") === Count.Finite(4)).and(poly2("poly2.U.chooser") === Count.Finite(2))

  // `test` cannot inspect its argument: a constant Boolean c — true, false, c, !c.
  def constant = poly2("poly2.U.flag") === Count.Finite(4)

  // `make ≅ Option[A]`, a supplied value split: None (1), Some(a): None or Some(a) (2) — 2.
  def covariantResult = poly2("poly2.U.made") === Count.Finite(2)

  // Memo A9: `create[C](c: C): C` is `1`, so `pick`'s only implementation is its value.
  def knownIdentity =
    counts("""trait Ids[A] { def pick[B](value: B): B; def create[C](c: C): C }
      |abstract class UsesIds[A] extends Ids[A] { def pick[B](value: B): B = value }
      |""".stripMargin)("UsesIds.pick") === Count.Finite(1)

  // Memo A10 (SYNTHESIS D-capabilities): `create[C](): C` has the empty type, so a receiver with one
  // is a vacuous environment — 1; a Maker to build needs a value of the empty type — 0. Vacuity, not
  // the target, makes `flip` and `any` 1: a context that admitted a Maker would give `flip` its four
  // Boolean functions and `any` no A at all.
  def absurd = {
    val found = counts("""trait Absurd[A] { def pick[B](value: B): B; def create[C](): C }
      |abstract class UsesAbsurd[A] extends Absurd[A] { def pick[B](value: B): B = value }
      |trait Maker { def create[C](): C }
      |object U { def maker(): Maker = ??? }
      |""".stripMargin)
    (found("UsesAbsurd.pick") === Count.Finite(1))
      .and(found("U.maker") === Count.Finite(0))
      .and(paths("paths.U.flip") === Count.Finite(1))
      .and(paths("paths.U.any") === Count.Finite(1))
  }

  private val aliases = counts("""package aliases
    |trait TagC[A] { type T = A; def pick[B](x: B, y: B, t: T): B }
    |trait TagC2[A] { type T = A; def p(t: T): Boolean }
    |trait Box[A] { type T = A; def get[A](t: T): A }
    |trait Tag[A] { type T = A; def pick[A](x: A, y: A, t: T): A }
    |trait Base[A] { type T = A }
    |trait TagD[B] extends Base[B] { def p(t: T): Boolean }
    |object U {
    |  def fc[X](t: TagC[X], x: X): Boolean = true
    |  def fcp[X](t: TagC[X], x: t.T): Boolean = true
    |  def fc2[X](t: TagC2[X], x: X): Boolean = true
    |  def shadowAlias[X](b: Box[X], x: X, y: X): X = x
    |  def shadowed[X](t: Tag[X], x: X): Boolean = true
    |  def first[X](t: TagD[X], x: X): Boolean = true
    |  def second[Y](t: TagD[Y], y: Y): Boolean = true
    |  def unit(t: TagD[Unit]): Boolean = true
    |}
    |""".stripMargin)

  // `T` is the class parameter as the record's arguments instantiate it, never an atom of its own
  // nor a member binder of the same name (scalac types `b.get[String](1)` on a `Box[Int]`). So
  // `pick ≅ X ⇒ Boolean` (Yoneda at B := 1 + 1) and `p: X ⇒ Boolean`: p(x) has two cases, a
  // Boolean each — 4 (fc, fcp, fc2, shadowed). `get ≅ ∀A'. X ⇒ A'` is the empty type once an X
  // exists, so `shadowAlias` has a vacuous context — 1. Two records of one template at two
  // arguments (`first`, `second`) are each read at their own (4, 4), and `TagD[Unit]` reads
  // `p: Unit ⇒ Boolean`, one Boolean cased — 4.
  def templateAliases =
    List("fc", "fcp", "fc2", "shadowed", "first", "second", "unit")
      .map(name => aliases(s"aliases.U.$name") === Count.Finite(4))
      .reduce(_ and _)
      .and(aliases("aliases.U.shadowAlias") === Count.Finite(1))

  // A class type conforms to another only through a base class, so evidence of two traits neither of
  // which is a base of the other never competes, whatever the arguments: scalac 3.9.0 compiles
  // `summon[Acc[F]].get(fa)` and `summon[Mark[A]].mark(a)` beside the other (probes/b4fix2/overlap
  // o3). `apart` counts tag(a) and mark(a), each case a Boolean: 2^4; `both` keeps only its member's
  // own reason. `Sub[A] extends Tag[A]` is a candidate for a search of `Tag[A]`: refused, and so is
  // `Wide`, whose record is the wider product (o4: 3.9.0 selects the `T1` evidence, a ranking the
  // reading never relies on).
  def unrelatedEvidence = {
    val found = counts("""trait Acc[F[_, _]] { def get[X, A](fa: F[X, A]): A }
      |trait RevAcc[F[_, _]] { def rev[X, A](a: A): F[X, A] }
      |trait Tag[A] { def tag(a: A): Boolean }
      |trait Mark[A] { def mark(a: A): Boolean }
      |trait Sub[A] extends Tag[A]
      |trait T1 { def a: Boolean }
      |trait Wide extends T1 { def b: Boolean }
      |object T {
      |  def both[F[_, _], A](fa: F[Unit, A])(using Acc[F], RevAcc[F]): A = ???
      |  def apart[A](a: A)(using Tag[A], Mark[A]): Boolean = ???
      |  def sub[A](a: A)(using Tag[A], Sub[A]): Boolean = ???
      |  def wide(using T1, Wide): Boolean = ???
      |}""".stripMargin)
    (found("T.apart") === Count.Finite(16))
      .and(found("T.both") must beLike {
        case Count.Unresolved(reasons) => reasons.exists(_.contains("overlaps")) must beFalse
      })
      .and(
        refused(
          found("T.sub"),
          "anonymous evidence selection not resolved: Tag[A] (overlaps Sub[A])"
        )
      )
      .and(
        refused(found("T.wide"), "anonymous evidence selection not resolved: T1 (overlaps Wide)")
      )
  }

  private val paths = counts("""package paths
    |trait Mixed3[A] { def f[M](a: A): (M => M, A) }
    |trait Mixed4[A] { def f[M](a: A): (M => M, Boolean) }
    |trait Church { def run[M](f: M => M, x: M): M }
    |trait Maker { def create[C](): C }
    |trait V2 { inline val flag: Boolean; def m: Boolean = ??? }
    |trait V3[A] { inline val a: A; def m: A = ??? }
    |object U {
    |  def g[A](c: Mixed3[A], a: A): A = a
    |  def h[A](c: Mixed4[A], a: A): Boolean = true
    |  def par[B](c: Church, b1: B, b2: B): B = c.run[(B, B)](p => (p._2, p._1), (b1, b2))._1
    |  def zero[A](c: Church): A = ???
    |  def flip(m: Maker, b: Boolean): Boolean = b
    |  def any[A](m: Maker): A = m.create[A]()
    |}
    |""".stripMargin)

  // `f(a)._2: A` mentions no binder: a free endomap, a, f(a)._2, … — ω. Its Boolean twin is cased
  // like any callable's, but an A argument may also come from `f(a)._1` at M := A, an instantiation
  // the argument enumeration does not read, and M := Boolean meets the goal: refused (true 4 —
  // `f(a)._1` is the identity — f(a)._2's two cases, a Boolean each).
  def monomorphicPaths =
    (paths("paths.U.g") === Count.Countable)
      .and(
        refused(
          paths("paths.U.h"),
          "polymorphic capability binder: M (free-theorem quotient not modelled for paths.Mixed4.f)"
        )
      )
      .and(
        refused(paths("paths.U.h"), "sum elimination over an argument space not enumerated: c#0")
      )

  // `run[(B, B)](swap, (b1, b2))._1` is b1 for even numerals and b2 for odd ones, a term no
  // instantiation of `run` at a goal represents; `zero` (true 0: no A from a numeral alone) is
  // refused for the same reason, and `par` never gets a number.
  def compoundInstantiation =
    refused(
      paths("paths.U.zero"),
      "polymorphic capability binder: M (instantiations at compound types not searched for " +
        "paths.Church.run)"
    ).and(paths("paths.U.par") must beLike { case Count.Unresolved(_) => ok })

  private val poly4 = counts("""package poly4
    |trait Seeded[F[_]] {
    |  def make[A](a: A): F[A]
    |  def seed[A]: F[A]
    |}
    |trait Lift[F[_]] { def lift[A](a: A): F[A] }
    |trait Mon[M] { def empty: M; def combine(x: M, y: M): M }
    |trait Folds[S, A] { def foldMap[M](f: A => M)(s: S)(using m: Mon[M]): M }
    |trait K[S, A] { def use[M](m: M, g: M => M, f: M => A): A }
    |trait Church { def use[M](m: M, g: M => M): M }
    |trait Bounded { def m[M <: AnyRef](x: M): M }
    |trait Kinded { def m[G[_]](x: G[Int]): G[Int] }
    |object U {
    |  def twice[F[_], B](q: Lift[F], b1: B, b2: B): F[B] = q.lift(b1)
    |  def unused[F[_], B](q: Lift[F], b: B): B = b
    |  def head[S, A](s: S, F: Folds[S, A]): Option[A] = None
    |  def t[S, A](s: S, k: S => A, c: K[S, A]): A = k(s)
    |  def church(): Church = new Church { def use[M](m: M, g: M => M): M = g(m) }
    |  def bounded[B <: AnyRef](b: B, c: Bounded): B = b
    |  def kinded(c: Kinded, n: Option[Int]): Option[Int] = n
    |}
    |""".stripMargin)

  // `F` is rigid, so `seed[A]: F[A]` is no Yoneda reduction. The goal F[A] fixes its binder: `seed`
  // is the one term of `make` (1); `seed` itself needs an A for `make` — 0.
  def resultDetermined =
    (poly4("poly4.Seeded.make") === Count.Finite(1))
      .and(poly4("poly4.Seeded.seed") === Count.Finite(0))

  // lift(b1), lift(b2): free theorems may identify instantiated terms, so a naive 2 is no count.
  def quotient =
    refused(
      poly4("poly4.U.twice"),
      "polymorphic capability binder: A (free-theorem quotient not modelled for poly4.Lift.lift)"
    )

  // `lift`'s result `F[A]` meets no goal of `B` (nor the empty one): b alone — 1.
  def irrelevant = poly4("poly4.U.unused") === Count.Finite(1)

  // Memo A12: `use` is not reducible (g: M ⇒ M is invariant) and its result A fixes no M.
  def undetermined =
    refused(
      poly4("poly4.U.t"),
      "polymorphic capability binder: M (not determined by the result of poly4.K.use)"
    )

  // Memo A11: `Mon[M]` is an algebra premise — `combine` meets M on both sides — so no reduction;
  // at the goal Option[A] the naive count is ω and must not be reported.
  def algebra =
    refused(
      poly4("poly4.U.head"),
      "polymorphic capability binder: M (free-theorem quotient not modelled for poly4.Folds.foldMap)"
    )

  // A Church to build is `use` for a fresh rigid M: m, g(m), g(g(m)), … — the numerals, ω.
  def skolem = poly4("poly4.U.church") === Count.Countable

  def boundedAndKinded =
    refused(poly4("poly4.U.bounded"), "polymorphic capability binder: M (bounded)")
      .and(
        refused(
          poly4("poly4.U.kinded"),
          "polymorphic capability binder: G (the result instantiates the binder)"
        )
      )

}
