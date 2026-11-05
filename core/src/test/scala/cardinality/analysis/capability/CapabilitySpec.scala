package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// Capability records, member classification, target views and override identity
// (design/capabilities.md C0–C7, C9). Every fixture is valid Scala 3.9.0 (compiled with scalac:
// probes/b3/spec, probes/b3fix2/spec). Each count is derived in the comment above its check.
class CapabilitySpec extends Specification {
  import Inhabitation.Count
  import NaturalSyntax.pow

  def is = s2"""
    The enclosing receiver (C5, C6)
      an overridable concrete sibling is a capability (B3)          $openSibling
      a final, private or inline sibling is fixed concrete code     $fixedSibling
      a subclass may make an open step distinguish every iterate     $openSteps
      a sibling whose default re-enters the declaration is a view   $definiteView
      a re-entry under a branch or an escaping this is a reason     $possibleView
      an open class's implementation that re-enters is its view (B2) $openImplementation
      an enclosing template's overridable member is a capability    $outerMember
      a member that only builds Unit is irrelevant                  $unitMember
      the reviewed views keep place 1, modify 0, singletonTo 0      $reviewedViews
      a public subclass of a private-constructor class is open      $ancestorPrivate
      a top-level private template is extended package-wide         $topLevelPrivate
      a local template's member another local class overrides       $fileOverride
      givens and member objects are final                           $givens
      universal members of `Any` are no capabilities                $universalReceiver
      an inherited overload is its own member                       $inheritedOverload
      a package-private constructor is reached package-wide         $packageConstructor
      a constructor `val` of another class of the file overrides    $fileFieldOverride
      a member is overridden by the levels before it in linearization $diamond
      a body `var` implements an inherited declaration              $varImplemented
    Override identity as seen from the receiver (C9)
      a grandparent's declaration is matched through the chain      $multiLevel
      an imported parent's declaration is matched by its binding (B1) $importedParent
      a covariant result is the same slot, a parameter another     $covariant
      another arity is an overload                                  $arity
      a binder spelled `foreign` keys no foreign declaration        $tentativeKeys
    Records of source types (C0–C4)
      a trait's members are its observations                        $traitInputs
      an open trait goal picks every overridable member             $traitGoal
      one member declared by two parents is one observation         $restatedMember
      a refined restatement is read at its nearest declaration      $refinedMember
      a final class is its observable constructor arguments         $finalRecord
      a composed-getter law reads its records                       $laws
      a `val` and a parameterless `def` of one name are one member  $oneSlot
      only `Any`'s members are universal (`canEqual` is not)        $universalRecord
    Records the reading refuses
      a member hidden at the use site                               $hiddenMember
      a field no member observes injectively                        $unexposedField
      hidden constructor state of an open class (X6)                $unwitnessed
      a `final abstract` class has no value                         $noInstance
      abstract type members and self types                          $existentials
      an unbounded record witness opens per value                   $existentialRecords
      a member that may be the measured declaration on its receiver $selfView
      an inferred overridable result and a recursive record         $inferredAndRecursive
  """

  private def counts(files: (String, String)*): Map[String, Count] =
    MethodAnalysis
      .analyze(
        files.toList.map((path, text) =>
          MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
        )
      )
      .map(entry => entry.name -> entry.count)
      .toMap

  private def one(code: String): Map[String, Count] = counts("Capability.scala" -> code)

  private def refused(count: Count, reason: String): Result =
    count must beLike { case Count.Unresolved(reasons) => reasons must contain(reason) }

  // T's y is overridable: get is x or y — 2 (it read 1, skipping y). OpenEnv may be extended with
  // any `id`: x, id(x), id(id(x)), … — ω; its `id` reads x, y, a and the overridable pick — 4.
  def openSibling = {
    val found = one("""
      trait T[A] { def x: A; def y: A = x; def get: A }
      class OpenEnv[A](x: A, y: A) { def id(a: A): A = a; def pick(): A = x }
    """)
    (found("T.get") === Count.Finite(2))
      .and(found("OpenEnv.pick") === Count.Countable)
      .and(found("OpenEnv.id") === Count.Finite(4))
  }

  // An open step distinguishes every iterate. step's default reads only t, but a subclass may
  // override it: step[C](f: C => B, t: T, c: C): T is (B, T) => T by parametricity, and an
  // override g makes t, g(b, t), g(b, g(b, t)), … pairwise distinct — place(b) is ω (OpenStep,
  // OpenStepB, OpenPoly; FreeSib's transfer likewise). OpenStepCalls' step calls place: a definite
  // view, so place(b) is the identity — 1. FL2.fromList is never 0 — an implementation may
  // delegate to other — and other's binder A keeps it `?`.
  def openSteps = {
    val found = one("""
      trait OpenStep[T, B] { def place(b: B): T => T; def step(t: T): T = t }
      trait OpenStepB[T, B] { def place(b: B): T => T; def step(b: B, t: T): T = t }
      trait OpenStepCalls[T, B] { def place(b: B): T => T; def step(b: B, t: T): T = place(b)(t) }
      trait OpenPoly[T, B] { def place(b: B): T => T; def step[C](f: C => B, t: T, c: C): T = t }
      trait FreeSib[T, B] { def place(b: B): T => T; def transfer[C](f: C => B): T => C => T = t => c => t }
      trait FL2[F[_]] { def fromList[A](xs: List[A]): F[A]; def other[A](arr: Array[Any], from: Int, size: Int): F[A] = ??? }
    """)
    List("OpenStep", "OpenStepB", "OpenPoly", "FreeSib")
      .map(t => found(s"$t.place") === Count.Countable)
      .reduce(_ and _)
      .and(found("OpenStepCalls.place") === Count.Finite(1))
      .and(found("FL2.fromList") must beLike {
        case Count.Unresolved(reasons) =>
          reasons must contain(
            "polymorphic capability binder: A (free-theorem quotient not modelled for FL2.other)"
          )
      })
  }

  // No instance replaces a final, private or inline member, nor a member of a final class: they
  // are concrete code over the environment. Fin.get: x — 1; FinalEnv.pick: x, y — 2.
  def fixedSibling = {
    val found = one("""
      trait Fin[A] { def x: A; final def y: A = x; private def z: A = x; inline def w: A = x; def get: A }
      final class FinalEnv[A](x: A, y: A) { def id(a: A): A = a; def pick(): A = x }
    """)
    (found("Fin.get") === Count.Finite(1)).and(found("FinalEnv.pick") === Count.Finite(2))
  }

  // y's default evaluates x whenever y is evaluated: x = y re-enters x, so T.x is only get — 1.
  // viaGet's default is get: W.get has no producer — 0.
  def definiteView = {
    val found = one("""
      trait T[A] { def x: A; def y: A = x; def get: A }
      trait W[A] { def get: A; def viaGet: A = get }
    """)
    (found("T.x") === Count.Finite(1)).and(found("W.get") === Count.Finite(0))
  }

  // other(true) re-enters get, other(false) does not: whether other re-enters x depends on its
  // argument (the model answer for x is 3); leak hands `this` to code the reading does not follow.
  def possibleView = {
    val found = one("""
      trait V[A] { def x: A; def get(b: Boolean): A; def other(b: Boolean): A = if b then get(b) else x }
      trait E[A] { def x: A; def get: A; def leak: A = E.run(this) }
      object E { def run[A](e: E[A]): A = e.x }
    """)
    refused(found("V.x"), "overridable member may re-enter the measured declaration: V.other")
      .and(
        refused(found("E.get"), "overridable member may re-enter the measured declaration: E.leak")
      )
  }

  // OpenImpl's y implements Base's y by evaluating x: a view of x (x is a — 1); x implements
  // Base's x with a, target-free, so y reads a and the overridable x — 2. The final twin: 1, 1.
  def openImplementation = {
    val found = one("""
      trait Base[A] { def x: A; def y: A }
      class OpenImpl[A](a: A) extends Base[A] { def x: A = a; def y: A = x }
      final class FinalImpl[A](a: A) extends Base[A] { def x: A = a; def y: A = x }
    """)
    (found("OpenImpl.x") === Count.Finite(1))
      .and(found("OpenImpl.y") === Count.Finite(2))
      .and(found("FinalImpl.x") === Count.Finite(1))
      .and(found("FinalImpl.y") === Count.Finite(1))
  }

  // Inner reads Outer's instance, which may be of a subclass overriding y: a or y — 2.
  def outerMember =
    one("class Outer[A](a: A) { def y: A = a; class Inner { def x: A = a } }")(
      "Outer.Inner.x"
    ) === Count.Finite(2)

  // collect builds only Unit, which has one construction whatever the environment: its parameter
  // type (an unsupplied dependency) is never read, and from has no A — 0.
  def unitMember =
    one("""
      import ext.Builder
      trait Sink[A] { def collect(s: A, b: Builder): Unit = (); def from(x: Int): A }
    """)("Sink.from") === Count.Finite(0)

  // The reviewed eo counts: transfer's returned lambdas apply place, replace applies modify and
  // collect's first statement evaluates singletonTo — each a definite view.
  def reviewedViews = {
    val found = one("""
      trait CanPlace[T, B] { def place(b: B): T => T; def transfer[C](f: C => B): T => C => T = t => c => place(f(c))(t) }
      trait CanModifyP[S, T, A, B] { def modify(f: A => B): S => T; def replace(b: B): S => T = modify(_ => b) }
      trait Single[S, X, A] { def singletonTo(s: S): (X, A); def collect(s: S): Unit = { val (x, a) = singletonTo(s); () } }
    """)
    (found("CanPlace.place") === Count.Finite(1))
      .and(found("CanPlace.transfer") === Count.Countable)
      .and(found("CanModifyP.modify") === Count.Finite(0))
      .and(found("CanModifyP.replace") === Count.Finite(1))
      .and(found("Single.singletonTo") === Count.Finite(0))
  }

  // Get's S and A reach G3 through Mid's clause: the declaration is G3.get's own slot — read(s), 1.
  def multiLevel =
    one("""
      trait Get[S, A] { def get(s: S): A }
      trait Mid[S, A] extends Get[S, A]
      class G3[S, A](read: S => A) extends Mid[S, A] { def get(s: S): A = read(s) }
    """)("G3.get") === Count.Finite(1)

  // Src is bound by an import (it read 1 before C9, dropping the parent, and `?` after A3): get is
  // Src's own slot, x a declaration Impl leaves open — a or x, 2.
  def importedParent =
    counts(
      "a.scala" -> "package p.api\ntrait Src[A] { def x: A; def get: A }",
      "b.scala" -> "package p.impl\nimport p.api.Src\nabstract class Impl[A](a: A) extends Src[A] { def get: A = a }"
    )("p.impl.Impl.get") === Count.Finite(2)

  // Parameters match: `get(b: Boolean): Boolean` overrides `get(b: Boolean): Boolean | Unit` (SLS
  // 5.1.3), one slot: b, !b, true, false — 4 (read as an overload, a free Boolean => Boolean | Unit
  // would join). OvlU2's get(b: Int) is an overload: it reads b (2^32 values) and the inherited free
  // get at true and at false (3 values each): 2^(9 · 2^32) = 2^38654705664.
  def covariant = {
    val found = one("""
      trait BaseU { def get(b: Boolean): Boolean | Unit }
      abstract class CovU2 extends BaseU { def get(b: Boolean): Boolean }
      abstract class OvlU2 extends BaseU { def get(b: Int): Boolean }
    """)
    (found("CovU2.get") === Count.Finite(4))
      .and(found("OvlU2.get") === Count.Symbolic(pow(2, BigInt("38654705664"))))
  }

  // Two parameters cannot be one: Two.get is a supplied (A, A) => A — a, get(a, a), … ω.
  def arity =
    one("""
      trait Two[A] { def get(a: A, b: A): A }
      abstract class Arity[A] extends Two[A] { def get(a: A): A = a }
    """)("Arity.get") === Count.Countable

  // g.get(s) — 1; g2.get(g1.get(s)) — 1; no S, no A — 0. Src's overridable y is an observation:
  // t.x or t.y — 2; a final y is x again — 1.
  def traitInputs = {
    val found = one("""
      trait CanGet[S, A] { def get(s: S): A }
      trait Src[A] { def x: A; def y: A = x }
      trait SrcF[A] { def x: A; final def y: A = x }
      object U {
        def read[S, A](s: S)(using g: CanGet[S, A]): A = g.get(s)
        def compose[S, A, B](s: S)(using g1: CanGet[S, A], g2: CanGet[A, B]): B = g2.get(g1.get(s))
        def noSource[S, A](using g: CanGet[S, A]): A = ???
        def pick[A](t: Src[A]): A = t.x
        def pickF[A](t: SrcF[A]): A = t.x
      }
    """)
    (found("U.read") === Count.Finite(1))
      .and(found("U.compose") === Count.Finite(1))
      .and(found("U.noSource") === Count.Finite(0))
      .and(found("U.pick") === Count.Finite(2))
      .and(found("U.pickF") === Count.Finite(1))
  }

  // An anonymous subclass picks x and y: 2 · 2 = 4; with y final, x alone: 2.
  def traitGoal = {
    val found = one("""
      trait Two[A] { def x: A; def y: A = x }
      trait TwoF[A] { def x: A; final def y: A = x }
      object U {
        def make[A](a: A, b: A): Two[A] = ???
        def makeF[A](a: A, b: A): TwoF[A] = ???
      }
    """)
    (found("U.make") === Count.Finite(4)).and(found("U.makeF") === Count.Finite(2))
  }

  // A class has one member per name and signature: C1's x is one value — 1, not 2.
  def restatedMember =
    one("""
      trait A1[X] { def x: X }
      trait B1[X] { def x: X }
      trait C1[X] extends A1[X] with B1[X]
      object U { def dedupe[X](c: C1[X]): X = c.x }
    """)("U.dedupe") === Count.Finite(1)

  // Box2 is (a, b): b.a or b.b — 2, and four boxes; Hidden's secret is observed by `reveal` —
  // 1; Getter's read by `get` — one Getter per S => A, 1.
  def finalRecord = {
    val found = one("""
      final class Box2[A](val a: A, val b: A)
      final class Hidden[A](secret: A) { def reveal: A = secret }
      final class Getter[S, A](read: S => A) { def get(s: S): A = read(s) }
      object U {
        def fromBox[A](b: Box2[A]): A = b.a
        def makeBox[A](x: A, y: A): Box2[A] = new Box2(x, y)
        def fromHidden[A](h: Hidden[A]): A = h.reveal
        def getter[S, A](get: S => A): Getter[S, A] = new Getter(get)
      }
    """)
    (found("U.fromBox") === Count.Finite(2))
      .and(found("U.makeBox") === Count.Finite(4))
      .and(found("U.fromHidden") === Count.Finite(1))
      .and(found("U.getter") === Count.Finite(1))
  }

  // composedGet evaluates all three getters strictly, so it is a view of each: getComposed is
  // λs. inner.get(outer.get(s)) — 1; getOuter has no A, getInner no source of A — 0, 0.
  def laws = {
    val found = counts(
      "CanGet.scala" -> "trait CanGet[S, A] { def get(s: S): A }",
      "Laws.scala" -> """trait Laws[S, A, B] {
        def getOuter: CanGet[S, A]
        def getInner: CanGet[A, B]
        def getComposed: CanGet[S, B]
        def composedGet(s: S): Boolean = getComposed.get(s) == getInner.get(getOuter.get(s))
      }"""
    )
    (found("Laws.getComposed") === Count.Finite(1))
      .and(found("Laws.getOuter") === Count.Finite(0))
      .and(found("Laws.getInner") === Count.Finite(0))
  }

  // reveal observes the protected s; a second hidden member would be observed by nothing.
  def hiddenMember =
    refused(
      one("""
        trait Prot[A] { protected def s: A; final def reveal: A = s }
        object U { def prot[A](p: Prot[A]): A = p.reveal }
      """)("U.prot"),
      "capability record with members hidden at the use site: Prot"
    )

  // twice reads secret but is no alias of it; get observes read only on Ints, not injectively.
  def unexposedField = {
    val found = one("""
      final class Unexposed[A](secret: A) { def twice: (A, A) = (secret, secret) }
      final class Narrow[A](read: Any => A) { def get(s: Int): A = read(s) }
      object U {
        def unexposed[A](h: Unexposed[A]): A = h.twice._1
        def narrow[A](n: Narrow[A]): A = n.get(0)
      }
    """)
    refused(found("U.unexposed"), "final class field not observable injectively: Unexposed.secret")
      .and(refused(found("U.narrow"), "final class field not observable injectively: Narrow.read"))
  }

  // No CNothing exists (every subclass passes a Nothing): reading its empty record would give 2,
  // the true count is 1 (vacuous) — refused (crosscheck X6).
  def unwitnessed =
    refused(
      one("class CNothing(n: Nothing)\nobject U { def nothing(c: CNothing): Boolean = true }")(
        "U.nothing"
      ),
      "open class with hidden constructor state: CNothing.n"
    )

  // A `final abstract` class is neither instantiated nor extended: Never has no value, so a
  // method returning one has no implementation — 0.
  def noInstance =
    one("final abstract class Never\nobject U { def never(): Never = ??? }")("U.never") ===
      Count.Finite(0)

  // A goal of Opt is a result construction: the implementation picks the witness the package
  // carries, which this model does not choose (existential-inputs boundary 1). A Selfy value
  // also has CanGet's members, which this reading does not enumerate: refused.
  def existentials = {
    val found = one("""
      trait CanGet[S, A] { def get(s: S): A }
      trait Opt[S, A] { type X; def to(s: S): (X, A) }
      trait Selfy { self: CanGet[Int, Int] => def x: Int }
      object U {
        def opt[S, A](s: S, a: A): Opt[S, A] = ???
        def selfy(s: Selfy): Int = s.x
      }
    """)
    refused(
      found("U.opt"),
      "existential wildcard result construction and repackaging requires witness-choice analysis"
    ).and(refused(found("U.selfy"), "self type not resolved: CanGet[Int, Int]"))
  }

  // An unbounded abstract type member of an open record is the witness of the package each value
  // carries (∃X. members-over-X): an input opens it — the members read over a witness fresh per
  // value, so p's and q's never mix. The witness itself is unobservable (r3's record of one value
  // alone leaves the literals alone), but consume's behavior at the value the body can supply is
  // one observable bit, so run's implementations are the four Boolean functions of it. both: two
  // independent packages, never crossed: 2^4. A goal of the record is a result construction:
  // refused. touch spells p.X, so the record's witness and the path atom would have to be one:
  // refused.
  def existentialRecords = {
    val found = one("""
      trait Pair { type X; def value: X; def consume(x: X): Boolean }
      trait Source { type X; def value: X }
      object U {
        def run(p: Pair): Boolean = ???
        def solo(p: Source): Boolean = ???
        def both(p: Pair, q: Pair): Boolean = ???
        def make(): Pair = ???
        def touch(p: Pair, x: p.X): Boolean = ???
      }
    """)
    (found("U.run") === Count.Finite(4))
      .and(found("U.solo") === Count.Finite(2))
      .and(found("U.both") === Count.Finite(16))
      .and(
        refused(
          found("U.make"),
          "existential wildcard result construction and repackaging requires witness-choice analysis"
        )
      )
      .and(
        refused(found("U.touch"), "capability record with abstract type member not read: Pair.X")
      )
  }

  // A subclass may mix Wrap and Pick: `other` may be this, and other.pick re-enter Wrap.pick. Two
  // unrelated classes share no instance: D's t is a capability of C.t, never C.t itself — a, d.b,
  // d.t, 3. A trait may be mixed into a subclass of D, so C2's d may be this: refused.
  def selfView = {
    val found = one("""
      trait Pick[A] { def pick(x: A, y: A): A }
      trait Wrap[A] { def other: Pick[A]; def pick(x: A, y: A): A }
      trait B[A] { def t: A }
      class C[A](a: A, d: D[A]) extends B[A] { def t: A = d.t }
      class D[A](val b: A) extends B[A] { def t: A = b }
      trait C2[A] extends B[A] { def a: A; def d: D[A]; def t: A = d.t }
    """)
    refused(
      found("Wrap.pick"),
      "capability record member may be the measured declaration on its receiver: Pick.pick"
    ).and(found("C.t") === Count.Finite(3))
      .and(
        refused(
          found("C2.t"),
          "capability record member may be the measured declaration on its receiver: D.t"
        )
      )
  }

  // B's own constructor is public, so a class in another file overrides m (X does): m is a free
  // Boolean beside b and the free target: 2^(2 · 2 · 4) = 2^16 (it read 2^8, m derived).
  def ancestorPrivate =
    counts(
      "a.scala" -> """package v6
        abstract class A private () { def target(b: Boolean): Boolean }
        object A { abstract class B extends A() { def m: Boolean = true; def target2(b: Boolean): Boolean } }""",
      "b.scala" -> """package v6
        class X extends A.B { override def m: Boolean = false; def target(b: Boolean): Boolean = b; def target2(b: Boolean): Boolean = b }"""
    )("v6.A.B.target2") === Count.Finite(BigInt(2).pow(16))

  // A top-level `private` object is visible in its whole package: X, in another file, overrides m —
  // functions of (b, m), 16 (it read 4).
  def topLevelPrivate =
    counts(
      "a.scala" -> """package v13
        private object O { abstract class B { def m: Boolean = true; def target(b: Boolean): Boolean } }""",
      "b.scala" -> """package v13
        class X extends O.B { override def m: Boolean = false; def target(b: Boolean): Boolean = b }"""
    )("v13.O.B.target") === Count.Finite(16)

  // Only f's block extends Loc. Sub overrides y there, so y may be either literal: refused (this
  // reading does not enumerate the block's subclasses). Without the override y is always true,
  // derived: functions of (a, b), 16.
  def fileOverride = {
    def local(sub: String) = one(s"""
      object O {
        def f(a: Boolean): Boolean = {
          abstract class Loc { def y: Boolean = true; def target(b: Boolean): Boolean }
          class Sub extends Loc { $sub def target(b: Boolean): Boolean = b }
          a
        }
      }""")("O.f.Loc.target")
    refused(
      local("override def y: Boolean = false;"),
      "member of a template only its file extends may be overridden there: O.f.Loc.y"
    ).and(local("") === Count.Finite(16))
  }

  // Givens and member objects are final (scalac: "cannot override final member"): g is the
  // constant true, and o implements P.o for good — b alone, 4 (they read 16). Each fixture stands
  // alone: a module one of them declares would be a reachable module of the others. As a record, C
  // observes nothing: c.g is always true (f: 2, it read 4), and C has one value (mk: 1, it read 2).
  def givens = {
    val record = one(
      "trait C { given g: Boolean = true }\n" +
        "object Use { def f(c: C): Boolean = c.g; def mk: C = new C {} }"
    )
    val parents = "trait Q { def v: Boolean }\ntrait P { def o: Q }\n"
    def target(owner: String) = one(parents + owner)
    (target("trait G { given g: Boolean = true; def target(b: Boolean): Boolean }")("G.target") ===
      Count.Finite(4))
      .and(
        target(
          "trait WithGiven extends P { given o: Q with { def v: Boolean = true }; def target(b: Boolean): Boolean }"
        )("WithGiven.target") === Count.Finite(4)
      )
      .and(
        target(
          "trait WithObject extends P { object o extends Q { def v: Boolean = true }; def target(b: Boolean): Boolean }"
        )("WithObject.target") === Count.Finite(4)
      )
      .and(record("Use.f") === Count.Finite(2))
      .and(record("Use.mk") === Count.Finite(1))
  }

  // An overridden hashCode or equals is runtime hashing and equality, outside the model: h2 is any
  // Int, 2^32; eqq a Boolean, 2 (they read 2^(2^37) and 4).
  def universalReceiver = {
    val found = one("""
      trait H { def h2: Int; override def hashCode: Int = 1 }
      trait Q { def eqq: Boolean; override def equals(that: Any): Boolean = true }
    """)
    (found("H.h2") === Count.Finite(BigInt(2).pow(32))).and(found("Q.eqq") === Count.Finite(2))
  }

  // Ovl3's final get(b: Int) is an overload, not Base2.get: other reads b and the free inherited
  // get at true and false — 2^(2 · 4) = 2^8 (it was refused as "implemented"). Ovl2's open get(b:
  // Int) is a free Int => Boolean besides: 2^(2 · 4 · 2^(2^32)).
  def inheritedOverload = {
    val found = one("""
      trait Base2 { def get(b: Boolean): Boolean }
      abstract class Ovl2 extends Base2 { def get(b: Int): Boolean = true; def other(b: Boolean): Boolean }
      abstract class Ovl3 extends Base2 { final def get(b: Int): Boolean = true; def other(b: Boolean): Boolean }
    """)
    (found("Ovl3.other") === Count.Finite(256))
      .and(found("Ovl2.other") === Count.Symbolic(pow(2, pow(2, BigInt(2).pow(32) + 3))))
  }

  // A `private[v7]` constructor is reached by every file of v7: X overrides m — functions of (b, m),
  // 16 (it read 4, m derived).
  def packageConstructor =
    counts(
      "a.scala" -> """package v7
        abstract class A private[v7] () { def m: Boolean = true; def target(b: Boolean): Boolean }""",
      "b.scala" -> """package v7
        class X extends A() { override def m: Boolean = false; def target(b: Boolean): Boolean = b }"""
    )("v7.A.target") === Count.Finite(16)

  // Sub overrides y with its constructor parameter, so a Loc's y is any Boolean: refused (the true
  // count is 16), never 4.
  def fileFieldOverride =
    refused(
      one("""
        object O {
          private abstract class Loc { def y: Boolean = true; def target(b: Boolean): Boolean }
          private class Sub(override val y: Boolean) extends Loc { def target(b: Boolean): Boolean = b }
        }""")("O.Loc.target"),
      "member of a template only its file extends may be overridden there: O.Loc.y"
    )

  // A val restating a parameterless def, or a constructor val implementing one, is one member: the
  // record is one Boolean — c.m: m, !m, true, false; mk: b, !b, true, false — 4 each (they read 16).
  // C.target reads b and the one m: 16 (it was refused, the identity unresolved).
  def oneSlot = {
    val restated = one("""
      trait P { def m: Boolean }
      trait C extends P { val m: Boolean }
      object Use { def f(c: C): Boolean = c.m; def mk(b: Boolean): C = new C { val m = b } }
    """)
    val receiver = one("""
      trait P { def m: Boolean }
      trait C extends P { val m: Boolean; def target(b: Boolean): Boolean }
    """)
    val field = one("""
      trait P { def x: Boolean }
      class C(val x: Boolean) extends P
      object Use { def f(c: C): Boolean = c.x; def mk(b: Boolean): C = new C(b) }
    """)
    (restated("Use.f") === Count.Finite(4))
      .and(restated("Use.mk") === Count.Finite(4))
      .and(receiver("C.target") === Count.Finite(16))
      .and(field("Use.f") === Count.Finite(4))
      .and(field("Use.mk") === Count.Finite(4))
  }

  // canEqual is scala.Equals', not Any's: a free Any => Boolean, which observes nothing of its
  // argument (Any is terminal), so one Boolean — true, false, c(x), !c(x): 4, as `(x: A, c: Any =>
  // Boolean)` gives (it read 2).
  def universalRecord =
    one("""
      trait Eq2[A] { def x: A; def canEqual(that: Any): Boolean }
      object U { def eq2[A](e: Eq2[A]): Boolean = ??? }
    """)("U.eq2") === Count.Finite(4)

  // y's result is inferred, and y is open: its type is unknown. Rec's records nest without end.
  def inferredAndRecursive = {
    val found = one("""
      trait Inf2[A] { def x: A; def z: A; def y = z }
      trait Rec[A] { def next: Rec[A]; def x: A }
      object U { def rec[A](t: Rec[A]): A = ??? }
    """)
    refused(found("Inf2.x"), "overridable member's result type is inferred: y")
      .and(refused(found("U.rec"), "recursive type not read: Rec"))
  }

  // L(R) = R, B, C, A (R extends C with B): B's final m overrides A's, although a breadth-first walk
  // reaches A from C first. m is a derived constant, so R.target ranges over b: 4 (it read 16, A.m
  // a second free Boolean); R2 = R2, C, B, A agrees. f's t observes only target: 2^4 = 16. Q's m is
  // OB's open override: one member — g = functions of m, 4; mk picks m, 2.
  def diamond = {
    val found = one("""
      trait A { def m: Boolean = true }
      trait B extends A { final override def m: Boolean = false }
      trait C extends A
      trait R extends C with B { def target(b: Boolean): Boolean }
      trait R2 extends B with C { def target(b: Boolean): Boolean }
      def f(t: R): Boolean = t.m
      trait OB extends A { override def m: Boolean = false }
      trait Q extends C with OB
      def g(t: Q): Boolean = t.m
      def mk: Q = new Q {}
    """)
    (found("R.target") === Count.Finite(4))
      .and(found("R2.target") === Count.Finite(4))
      .and(found("f") === Count.Finite(16))
      .and(found("g") === Count.Finite(4))
      .and(found("mk") === Count.Finite(2))
  }

  // L(T) = T, B, A: T's n is B's, `Some[Boolean]` — two values, Some(true) and Some(false). f: the
  // functions of n, 4; mk: 2 (it read A's Option[Boolean]: 8 and 3).
  def refinedMember = {
    val found = one("""
      trait A { def n: Option[Boolean] }
      trait B extends A { def n: Some[Boolean] }
      trait T extends A with B
      object U { def f(t: T): Boolean = t.n.value; def mk: T = ??? }
    """)
    (found("U.f") === Count.Finite(4)).and(found("U.mk") === Count.Finite(2))
  }

  // B's var x implements A's x and cannot be overridden: under the model it is its initializer,
  // a derived constant, so R.target ranges over b alone — 4 (it read 16, x a free Boolean).
  def varImplemented =
    one("""
      trait A { def x: Boolean }
      trait B extends A { var x: Boolean = true }
      trait R extends B { def target(b: Boolean): Boolean }
    """)("R.target") === Count.Finite(4)

  // Impl.get takes an Option[foreign[Int]], Base.get a foreign[Int]: an overload whatever the
  // binder is called, so Impl.get reaches the solver's own limit, as the F-named twin does (it
  // read `inherited override identity not resolved`, its key spelling `foreign:`).
  def tentativeKeys = {
    val found = one("""
      trait Base[foreign[_]] { def get(x: foreign[Int]): Boolean }
      abstract class Impl[foreign[_]] extends Base[foreign] { def get(x: Option[foreign[Int]]): Boolean }
      trait BaseF[F[_]] { def get(x: F[Int]): Boolean }
      abstract class ImplF[F[_]] extends BaseF[F] { def get(x: Option[F[Int]]): Boolean }
    """)
    (found("Impl.get") === found("ImplF.get"))
      .and(
        found("Impl.get") must not(
          beEqualTo(Count.Unresolved(List("inherited override identity not resolved: get")))
        )
      )
  }

}
