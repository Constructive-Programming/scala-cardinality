package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.resolution.RuntimeClasses
import cardinality.capacity.EffectiveOmega
import org.specs2.Specification

/** Top types as the terminal type, goals with one value, and unions read as tagged sums only where
  * a class test is their eliminator (docs/research/top-types-unions.md).
  */
class TopTypesUnionSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Any is the terminal type of the model
      a result of type Any has one implementation                    $anyOut
      an Any input carries no information                            $anyIn
      no equality on Any, exactly as on a type parameter             $noEquality
      a consumer of Any is constant                                  $consumer
      two consumers of Any give two values                           $twoConsumers
      feeding a value to an Any consumer adds nothing                $feed
      a cycle through Any is not a productive family                 $loop
      Any is always inhabited, so f(()) seeds a result               $unseeded
      structural constructors over Any count Unit's cases            $structural
      the other top names are terminal too                           $otherTops
      qualified spellings reach the builtin                          $qualified
      a repeated Any still has an observable length                  $repeated
    Shadowing and boundaries keep their meaning
      a source alias named Any is that alias                         $shadowAlias
      a type binder named Any is that binder                         $shadowBinder
      an import of Any refuses the builtin                           $shadowImport
      an inherited member named Any is that member                   $shadowInherited
      a wildcard function is still uncallable                        $existential
    A channel to declarations the sources do not supply keeps Any open
      a self type outside the sources                                $foreignSelf
      a supplied self type with a parent outside the sources         $selfUnsuppliedParent
      a supplied self type read in full                              $suppliedSelf
      a wildcard-imported object with a parent outside the sources   $importedObject
      a package object with a parent outside the sources             $packageObject
      a wildcard import or export of an unsupplied namespace         $foreignWildcard
      a supplied wildcard-imported object read in full               $suppliedObject
      a constructor's types are read outside its class's parents      $ctorOutsideParents
      a member's capture of a field is typed outside the template     $fieldCapture
      the outer clause of a chained package with nothing supplied     $chainedPackage
      evidence with an Any endpoint is not reflexivity               $evidence
    A goal with one value has one implementation in any environment
      Unit, Any and their products beside an unresolved name         $terminalResults
      a module singleton and a one-valued closed type                $terminalValues
      a result with more values keeps the environment's reason       $nonTerminal
      a given selector binds no type name                            $rebinding
      a given selector spelling Any or Null binds neither            $rebindingTops
      a constructor's positions are value spaces, not goals          $constructorGoal
    Lattice identities apply before resolution
      Any absorbs a union, even an unresolvable alternative          $absorbAny
      Any is the unit of intersection                                $meetAny
      a subtype alternative is absorbed by its sealed parent         $absorbSubtype
      one top-like class spelled twice is that class                 $topLikeTwice
    Constructor slots read the implementation model
      an Any slot is one value, not the epsilon tier                 $anyBoxCtor
      an Any slot beside an Int is the Int's space                   $walkCtor
      an Any array beside them is a countable length                 $walkResCtor
      Null is empty and a union slot is its sum                      $nullCtor
      a top behind an alias is read on the method side               $aliasedTopCtor
      a binder named Nothing does not capture the empty slot         $nothingBinderCtor
      a source type named Nothing does not capture it either         $nothingAliasCtor
      a top inside a container that merges equal elements is refused $mergingCtor
      a rewritten slot is read where its constructor is written      $rewrittenScope
    Discriminable disjoint unions are sums
      a result adds its alternatives                                 $sumOut
      an input multiplies its branches                               $sumIn
      a module singleton and a tuple are disjoint                    $singletonTuple
      nested unions flatten                                          $nested
      a union alias reads as the same sum                            $alias
      a union alias nested in a union flattens                       $aliasNested
      Null alternatives vanish before the sum is formed              $nullMix
      one class spelled twice is one alternative                     $sameClass
      distinct builtin classes are disjoint                          $builtins
      sealed traits whose cases are apart are disjoint               $sealedApart
      an enum and a sealed trait are disjoint case by case           $enumApart
      a foreign parent cannot extend the sources                     $foreignParent
    Unions that are not sums stay unresolved
      a type-parameter alternative                                   $param
      two alternatives of one runtime class                          $sameRuntimeClass
      an opaque alternative outside its scope                        $opaqueOutside
      a top-like alternative                                         $topLike
      an alternative whose declaration was not supplied              $missingDecl
      a callable that returns a union                                $callableSum
      a case shared by two sealed families                           $sharedCase
      a case extending through an alias                              $aliasChild
      an enum case that extends the other alternative                $enumCase
      a parent an unread parent may declare                          $unreadParent
      unenumerable families on both sides                            $anonymousBoth
      a family whose shape misses an enum-case subclass              $unenumeratedChild
      a family a child extends through a parameterised alias         $aliasApplicationChild
      one union alias applied twice to one runtime class             $aliasTwice
      the pair check refuses what the shape reading refuses first    $openPair
  """

  private def entry(code: String, name: String): MethodAnalysis.Entry =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Top.scala", dialects.Scala3(code).parse[Source].get)))
      .find(_.name == name)
      .get

  private def count(code: String, name: String): Count = entry(code, name).count

  private def countIn(files: (String, String)*)(name: String): Count =
    MethodAnalysis
      .analyze(files.toList.map { (path, code) =>
        MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)
      })
      .find(_.name == name)
      .get
      .count

  private def reasonContains(code: String, name: String, fragments: String*) =
    count(code, name) must beLike {
      case Count.Unresolved(reasons) =>
        fragments.forall(f => reasons.exists(_.contains(f))) must beTrue
    }

  private val colours =
    "final case class Red(level: Boolean)\nfinal case class Blue()\n"

  private val fail =
    "sealed trait Fail\nfinal case class Missing() extends Fail\ncase object Broken extends Fail\n"

  private val detail = "overlap and discrimination proof"

  def anyOut = count("def anyOut[A](a: A): Any = a", "anyOut") === Count.Finite(1)
  def anyIn = count("def anyIn[A](x: Any, a: A): A = a", "anyIn") === Count.Finite(1)

  def noEquality =
    (count("def eqAny(x: Any, y: Any): Boolean = true", "eqAny") === Count.Finite(2))
      .and(count("def eqParam[A](a: A, b: A): Boolean = true", "eqParam") === Count.Finite(2))

  def consumer = count("def call[R](f: Any => R): R = f(())", "call") === Count.Finite(1)

  def twoConsumers =
    count("def callTwo[R](f: Any => R, g: Any => R): R = f(1)", "callTwo") === Count.Finite(2)

  def feed = count("def feed[A, R](a: A, f: Any => R): R = f(a)", "feed") === Count.Finite(1)

  // Terminal Any makes f constant: a and f(()) — not the ω an opaque X would give.
  def loop =
    (count("def loop[A](a: A, f: Any => A, g: A => Any): A = a", "loop") === Count.Finite(2))
      .and(count("def loop[A, X](a: A, f: X => A, g: A => X): A = a", "loop") === Count.Countable)

  // () always inhabits Any — not the 0 an uninhabited X would give.
  def unseeded =
    (count("def u[A](f: Any => A, g: A => Any): A = f(())", "u") === Count.Finite(1))
      .and(count("def u[A, X](f: X => A, g: A => X): A = ???", "u") === Count.Finite(0))

  def structural =
    (count("def opt: Option[Any] = None", "opt") === Count.Finite(2))
      .and(count("def e: Either[Any, Any] = Left(())", "e") === Count.Finite(2))
      .and(count("def f[A](a: A): A => Any = _ => a", "f") === Count.Finite(1))
      .and(
        count("case class Box(x: Any)\ndef same(b: Box, c: Box): Box = b", "same") === Count.Finite(
          1
        )
      )

  def otherTops =
    (count("def r: AnyRef = \"\"", "r") === Count.Finite(1))
      .and(count("def m: Matchable = ()", "m") === Count.Finite(1))
      .and(count("def v: AnyVal = ()", "v") === Count.Finite(1))
      .and(count("def o: Object = \"\"", "o") === Count.Finite(1))

  def qualified =
    (count("def q: scala.Any = ()", "q") === Count.Finite(1))
      .and(count("def j: java.lang.Object = \"\"", "j") === Count.Finite(1))

  // Only the length of `xs` is observable: finitely describable predicates on it are ω many.
  def repeated = count("def f(xs: Any*): Boolean = true", "f") === Count.Countable

  def shadowAlias =
    count("type Any = Boolean\ndef shadow(): Any = true", "shadow") === Count.Finite(2)

  def shadowBinder = count("def p[Any](x: Any, y: Any): Any = x", "p") === Count.Finite(2)

  def shadowImport =
    reasonContains(
      "import ext.Any\ndef imp(): Any = ???",
      "imp",
      "declaration not supplied: ext.Any (named import Any)"
    )

  // Inside O, `Any` is Base's Boolean: no absorption, no unit of `&`, no terminal result. Red |
  // Boolean has 4 values over Red's 2 → 4^2; a Boolean over 2 inputs → 2^2.
  def shadowInherited = {
    val code = colours + "trait Base { type Any = Boolean }\nobject O extends Base { " +
      "def f(x: Red): Red | Any = x; def g(x: Red): Any = x; def h(x: Red): Red & Any = x }"
    (count(code, "O.f") === Count.Finite(16))
      .and(count(code, "O.g") === Count.Finite(4))
      .and(reasonContains(code, "O.h", "unsupported intersection relationship: Red & Any"))
  }

  def existential =
    (count("def m[R](consume: Function1[?, R]): R = ???", "m") === Count.Finite(0))
      .and(count("def m[R](consume: Function1[Any, R]): R = ???", "m") === Count.Finite(1))

  // Ext could declare `type Unit = Boolean` and `type Any = Boolean` (g and h would have 2), but a
  // declaration outside the sources is assumed to rebind no builtin name (the never-shadowed
  // assumption): both results are terminal, one implementation each, so is a supplied self type's
  // unsupplied parent.
  def foreignSelf = {
    val code = "import ext.Ext\ntrait T { self: Ext => def g(): Unit; def h(): Any }"
    (count(code, "T.g") === Count.Finite(1)).and(count(code, "T.h") === Count.Finite(1))
  }

  def selfUnsuppliedParent =
    count(
      "trait Base extends ext.X\ntrait T { self: Base => def g(): Unit; def h(): Any }",
      "T.g"
    ) === Count.Finite(1)

  def suppliedSelf =
    count("trait Base { def x: Int }\ntrait T { self: Base => def g(): Unit }", "T.g") ===
      Count.Finite(1)

  // With `trait X { type Any = Boolean }`, k is Boolean => Boolean: 4, not 2.
  // `import Defs.*` brings what `Defs` inherits from `ext.X`, which may declare `Any` or `Unit`: the
  // lookup refuses the name before any builtin reading.
  def importedObject = {
    val code =
      "object Defs extends ext.X\nobject W { import Defs.*; def k(x: Any): Boolean = true\n" +
        "  def f(s: ext.Schema): Unit = () }"
    reasonContains(code, "W.k", "inherited type member not resolved: Any (unsupplied parent ext.X)")
      .and(
        reasonContains(
          code,
          "W.f",
          "inherited type member not resolved: Unit (unsupplied parent ext.X)"
        )
      )
  }

  // The package object's unsupplied parent rebinds no builtin name (the never-shadowed assumption):
  // k reads a top as (), so it is a Boolean, 2; r's result is terminal, 1.
  def packageObject = {
    val code = "package p\npackage object p2 extends ext.X\npackage p2 {\n" +
      "  object M { def k(x: Any): Boolean = true; def r(b: Boolean): Any = b } }"
    (count(code, "p.p2.M.k") === Count.Finite(2)).and(count(code, "p.p2.M.r") === Count.Finite(1))
  }

  // An unsupplied wildcard import rebinds neither `Any` nor `Unit` (the never-shadowed
  // assumption): both results are terminal, one implementation each, whatever `Schema` is.
  def foreignWildcard = {
    val imported = "import ext.*\ndef g(): Any = ()\ndef f(s: Schema): Unit = ()"
    val exported = "object E { export ext.defs.*; def f(s: ext.Schema): Unit = () }"
    (count(imported, "g") === Count.Finite(1))
      .and(count(imported, "f") === Count.Finite(1))
      .and(
        reasonContains(
          exported,
          "E.f",
          "through ext.defs, which is not supplied (export of E)",
          "wildcard export not supplied: Unit (wildcard export of ext.defs)"
        )
      )
  }

  // Scala 3.9.0 reads `final class E(val a: Any) extends ext.X` with `scala.Any` even when X
  // declares `type Any = Boolean`; the members inside see X's. The member is read first here: its
  // result may be X's member, while its capture `a` is still `scala.Any`.
  def ctorOutsideParents = {
    val code = "final class E(val a: Any, val n: Boolean) extends ext.X { def k(): Any = a }"
    (count(code, "E.<init>") === Count.Finite(2))
      .and(
        reasonContains(code, "E.k", "inherited type member not resolved: Any (unsupplied parent")
      )
      .and(count(code, "E.k") must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("builtin name Any may be shadowed")) must beFalse
      })
  }

  // Inside S2, `Any` is X's Boolean, but the field `a` is typed outside the template: scala.Any, one
  // value. get2 reads a (1) and b (2): 2^2. A body alias is out of reach of the parameters too: in
  // C, x is scala.Any and m's result is the body's Boolean, so m is 2^2 and C.<init> is 1 · 2.
  def fieldCapture = {
    val parents = "trait X { type Any = Boolean }\n" +
      "final class S2(val a: Any, val b: Boolean) extends X { def get2: Boolean = b }"
    val body =
      "final class C(val x: Any, val b: Boolean) { type Any = Boolean; def m(): Any = true }"
    (count(parents, "S2.get2") === Count.Finite(4))
      .and(count(body, "C.m") === Count.Finite(4))
      .and(count(body, "C.<init>") === Count.Finite(2))
  }

  // `package extp; package inner` opens extp's members; with no unit of extp supplied, one could be
  // `type Any = Boolean` (Scala 3.9.0 compiles `val b: Boolean = extp.inner.U.f()` beside it), but
  // none is assumed (the never-shadowed assumption): both results are terminal, 1.
  def chainedPackage = {
    val chained = "package extp\npackage inner\n" +
      "object U { def f(): Any = true; def g(x: Foo): Unit = () }"
    val supplied = "package extp\ntrait Foo"
    (count(chained, "extp.inner.U.f") === Count.Finite(1))
      .and(
        count(chained, "extp.inner.U.g") === Count.Finite(1)
      )
      .and(
        countIn("A.scala" -> chained, "B.scala" -> supplied)("extp.inner.U.f") === Count.Finite(1)
      )
      .and(
        countIn("A.scala" -> chained, "B.scala" -> supplied)("extp.inner.U.g") === Count.Finite(1)
      )
  }

  def suppliedObject =
    count("object Defs\nobject W { import Defs.*; def k(x: Any): Boolean = true }", "W.k") ===
      Count.Finite(2)

  def evidence =
    reasonContains(
      "def ev[A](x: A)(using e: A <:< Any): A = x",
      "ev",
      "unsupported evidence relationship"
    )

  private val codec =
    "import ext.Schema\ntrait Codec[A] { def schema: Schema; def encode(a: A): Any; " +
      "def run(a: A): Unit; def both(a: A): (Unit, Any); def bad(a: A): Schema }"

  // An opaque type outside its scope is unreadable here, but adds no second Unit.
  def terminalResults =
    (count(codec, "Codec.encode") === Count.Finite(1))
      .and(count(codec, "Codec.run") === Count.Finite(1))
      .and(count(codec, "Codec.both") === Count.Finite(1))
      .and(
        count(
          "object Owner { opaque type Leak = Boolean }\n" +
            "def consume[A](xs: Owner.Leak*): Unit = ()",
          "consume"
        ) === Count.Finite(1)
      )

  // `Nothing => Boolean` has one value, the empty function; so has `Option[Nothing]`.
  def terminalValues = {
    val code = "import ext.Schema\ncase object Green\ndef g(s: Schema): Green.type = Green\n" +
      "def h(s: Schema): Option[Nothing] = None\ndef k(s: Schema): Nothing => Boolean = x => x"
    (count(code, "g") === Count.Finite(1))
      .and(count(code, "h") === Count.Finite(1))
      .and(count(code, "k") === Count.Finite(1))
  }

  def nonTerminal =
    reasonContains(codec, "Codec.bad", "declaration not supplied: ext.Schema (named import Schema)")
      .and(
        reasonContains("import ext.Schema\ndef b(s: Schema): Boolean = true", "b", "Schema")
      )

  // A `given` selector imports given instances, which are terms: `import x.y.{given Unit}` binds no
  // type name, so `Unit` is the root one and the goal has one value (Scala 3.9.0 compiles `def y():
  // Unit = ()` after `import Defs.{given Unit}` with `Defs.Unit = Boolean`).
  def rebinding =
    count("import ext.Schema\nimport x.y.{given Unit}\ndef y(): Unit = ()", "y") === Count.Finite(1)

  def rebindingTops =
    (count("import x.y.{given Any}\ndef r(): Any = ()", "r") === Count.Finite(1))
      .and(count("import x.y.{given Null}\ndef r(): Unit = ()", "r") === Count.Finite(1))

  def constructorGoal =
    reasonContains("import ext.Schema\ncase class C(s: Schema, u: Unit)", "C.<init>", "Schema")

  def absorbAny =
    (count("def a[A](a: A): A | Any = a", "a") === Count.Finite(1))
      .and(count(colours + "def b(r: Red): Red | Null | Any = r", "b") === Count.Finite(1))
      .and(count("import ext.Schema\ndef s(): Schema | Any = ()", "s") === Count.Finite(1))
      .and(
        count("type Top = Any\nimport ext.Schema\ndef t(s: Schema): Schema | Top = s", "t") ===
          Count.Finite(1)
      )

  def meetAny = count("def m[A](a: A): A & Any = a", "m") === Count.Finite(1)

  // Fail | Missing is Fail: 2 values, and f splits in 2 → 2^2.
  def absorbSubtype =
    (count(fail + "def a(f: Fail): Fail | Missing = f", "a") === Count.Finite(4))
      .and(
        count(fail + "def a(f: Fail): Fail | Missing = f", "a") === count(
          fail + "def b(f: Fail): Fail = f",
          "b"
        )
      )

  def topLikeTwice =
    count("def t(x: AnyRef): AnyRef | Object = x", "t") === Count.Finite(1)

  def anyBoxCtor = count("case class AnyBox(x: Any)", "AnyBox.<init>") === Count.Finite(1)

  def walkCtor =
    count("final class Walk(val cur: Any, val n: Int)", "Walk.<init>") ===
      Count.Finite(BigInt(2).pow(32))

  // Before: the estimate read `cur: Any` as everything (ε₀). Now 1 × lengths × 2^32.
  def walkResCtor = {
    val walk = count(
      "final class WalkRes(val cur: Any, val parents: Array[Any], val parentsLen: Int)",
      "WalkRes.<init>"
    )
    (walk.isCountable must beTrue).and(walk.render === "ω")
  }

  def nullCtor = {
    val code = colours + "case class N(x: Null)\ncase class NB(x: Boolean | Null)\n" +
      "case class R(x: Red | Blue)\ncase class T(x: Option[Any], y: Any => Boolean)"
    (count(code, "N.<init>") === Count.Finite(0))
      .and(count(code, "NB.<init>") === Count.Finite(2))
      .and(count(code, "R.<init>") === Count.Finite(3))
      .and(count(code, "T.<init>") === Count.Finite(4))
  }

  // The method side reads the alias's Any as (); the estimate would read it as everything. Through
  // `Arr`, `Array[Any]` is the sequences of () — one per length, countably many: an infinite closed
  // position keeps the estimate's tier, so the row is the bound ω (`Bounded`), not a count.
  def aliasedTopCtor = {
    val code = "type Top = Any\nfinal case class TA(x: Top)\ntype Fn = Any => Boolean\n" +
      "final case class TC(f: Fn)\ntype Arr = Array[Any]\nfinal case class TB(a: Arr)"
    val slots = "type Fn[A] = Any => A\nfinal case class W[A](f: Fn[A])\n" +
      "final case class V[A](f: Any => A)"
    (count(code, "TA.<init>") === Count.Finite(1))
      .and(count(code, "TC.<init>") === Count.Finite(2))
      .and(count(code, "TB.<init>") === Count.Bounded(EffectiveOmega))
      .and(count(slots, "W.<init>") === count(slots, "V.<init>"))
  }

  def nothingBinderCtor =
    count("final case class KN[Nothing](x: Null, b: Boolean)", "KN.<init>") === Count.Finite(0)

  // An Array[Null] holds only the empty array (`new Array[Null](0)` needs no ClassTag the type
  // lacks: Scala 3.9.0 fixture), so the slot is 1 and the case class 2. A source alias named
  // `Nothing` does not reach `Null`: still 2.
  def nothingAliasCtor = {
    val slot = "final case class NA(x: Array[Null], b: Boolean)"
    (count(slot, "NA.<init>") === Count.Finite(2))
      .and(count("type Nothing = Boolean\n" + slot, "NA.<init>") === Count.Finite(2))
  }

  // Set(1, "a") and Set(1) differ in size, though Any reads as (): Set[Any] is not Set[Unit]'s 2.
  // A Map's values and a sequence's elements are held by position (Map[Boolean, ()]: 2^2 key sets),
  // and a source class the method side reads whole needs no estimate (Box[Any]: 1).
  def mergingCtor = {
    val code = "final case class S1(x: Set[Any])\nfinal case class M1(m: Map[Any, Boolean])\n" +
      "final case class MV(m: Map[Boolean, Any])\nfinal case class O1(x: Option[Any])\n" +
      "final case class ST(x: Set[(Any, Boolean)])\nfinal case class Bag[A](s: Set[A])\n" +
      "final case class WBag(b: Bag[Any])\nfinal case class Box[A](a: A)\n" +
      "final case class WBox(b: Box[Any])\nfinal case class SU(x: Set[Unit])"
    reasonContains(code, "S1.<init>", "top type inside Set, which may tell its elements apart: x")
      .and(count(code, "S1.<init>") must_!= Count.Finite(2))
      .and(reasonContains(code, "M1.<init>", "top type inside Map"))
      .and(reasonContains(code, "ST.<init>", "top type inside Set"))
      .and(reasonContains(code, "WBag.<init>", "top type inside Bag"))
      .and(count(code, "MV.<init>") === Count.Finite(4))
      .and(count(code, "O1.<init>") === Count.Finite(2))
      .and(count(code, "WBox.<init>") === Count.Finite(1))
      .and(count(code, "SU.<init>") === Count.Finite(2))
  }

  // The rewrite builds a new tree, which has no position of its own: the estimate reads it in the
  // scope of the slot it was rebuilt from, where Red is O's one value, not the package's two.
  def rewrittenScope = {
    val code =
      "package p\nfinal case class Red(b: Boolean)\nobject O {\n  final case class Red()\n" +
        "  final case class Box(f: (Any, Red))\n" +
        "  final case class Opt(f: Option[(Any, Red)], k: Boolean)\n}"
    (count(code, "p.O.Box.<init>") === Count.Finite(1))
      .and(count(code, "p.O.Opt.<init>") === Count.Finite(4))
  }

  // Red: 2, Blue: 1 → 3 results per branch of r.level → 3^2.
  def sumOut =
    count(colours + "def out(r: Red, b: Blue): Red | Blue = r", "out") === Count.Finite(9)

  // 3 input values → Boolean^3.
  def sumIn =
    count(colours + "def in(u: Red | Blue): Boolean = true", "in") === Count.Finite(8)

  // 1 + 4 values per branch, p splits into 4 → 5^4.
  def singletonTuple =
    count(
      "case object Green\ndef t(p: (Boolean, Boolean)): Green.type | (Boolean, Boolean) = Green",
      "t"
    ) === Count.Finite(625)

  // 2 + 1 + 2 results; r.level × f → 4 branches → 5^4.
  def nested =
    count(colours + fail + "def n(r: Red, b: Blue, f: Fail): Red | (Blue | Fail) = r", "n") ===
      Count.Finite(625)

  // r: 4 cases × fb.level 2 → 8 branches, 2 Reds each → 2^8.
  def alias =
    count(colours + fail + "type Result = Fail | Red\ndef g(r: Result, fb: Red): Red = fb", "g") ===
      Count.Finite(256)

  // Red | Blue | Green: 4 values over Red's 2 → 4^2, through a plain or a parameterised alias,
  // an alias of an application, or an application on its own (Red | Blue: 3^2).
  def aliasNested = {
    val code = colours + "case object Green\ntype RB = Red | Blue\ntype Or[A, B] = A | B\n" +
      "type ORB = Or[Red, Blue]\n"
    (count(code + "def a(r: Red): RB | Green.type = r", "a") === Count.Finite(16))
      .and(count(code + "def c(r: Red): Or[Red, Blue] | Green.type = r", "c") === Count.Finite(16))
      .and(count(code + "def e(r: Red): ORB | Green.type = r", "e") === Count.Finite(16))
      .and(count(code + "def f(r: Red): Or[RB, Green.type] = r", "f") === Count.Finite(16))
      .and(count(code + "def b(r: Red): Or[Red, Blue] = r", "b") === Count.Finite(9))
  }

  def nullMix = count(colours + "def nm(r: Red): Red | Blue | Null = r", "nm") === Count.Finite(9)

  def sameClass =
    count(
      "package p\nfinal case class Red(level: Boolean)\ndef s(r: Red): Red | p.Red = r",
      "p.s"
    ) ===
      Count.Finite(4)

  // Unit | Boolean: 3 values; Option[Boolean] | Either[Unit, Unit]: 5 values over 3 inputs.
  def builtins =
    (count("def ue(x: Unit): Unit | Boolean = x", "ue") === Count.Finite(3))
      .and(
        count("def oe(x: Option[Boolean]): Option[Boolean] | Either[Unit, Unit] = x", "oe") ===
          Count.Finite(125)
      )

  private val families =
    "sealed trait P\nsealed trait Q\ncase object P1 extends P\ncase object Q1 extends Q\n"

  def sealedApart = count(families + "def pq(p: P): P | Q = p", "pq") === Count.Finite(2)

  // Color: 2 cases, Q: 1 → 3 results over 2 inputs.
  def enumApart =
    count(
      "enum Color { case R, G }\nsealed trait Q\ncase object Q1 extends Q\n" +
        "def cq(c: Color): Color | Q = c",
      "cq"
    ) === Count.Finite(9)

  // A declaration outside the sources — named, or imported from a path not supplied — cannot
  // extend the sources that depend on it.
  def foreignParent =
    (count(
      "sealed trait P\ncase object P1 extends P with ext.Mixin\n" +
        "sealed trait Q\ncase object Q1 extends Q with ext.Other\ndef pq(p: P): P | Q = p",
      "pq"
    ) === Count.Finite(2)).and(
      count(
        "import ext.{Mixin, Other}\nsealed trait P\ncase object P1 extends P with Mixin\n" +
          "sealed trait Q\ncase object Q1 extends Q with Other\ndef pq(p: P): P | Q = p",
        "pq"
      ) === Count.Finite(2)
    )

  def param =
    reasonContains(colours + "def p[A](a: A, r: Red): A | Red = r", "p", detail, "type parameter")

  def sameRuntimeClass =
    reasonContains(
      "final case class Box[T](t: T)\n" +
        "def b(a: Box[Boolean], c: Box[Unit]): Box[Boolean] | Box[Unit] = a",
      "b",
      "share runtime class Box"
    ).and(
      reasonContains(
        "def l(x: (Int, Int)): (Int, Int) | (String, String) = x",
        "l",
        "share runtime class scala.Tuple2"
      )
    ).and(
      reasonContains(
        "def o(x: Option[Boolean]): Option[Boolean] | Option[Unit] = x",
        "o",
        "share runtime class scala.Option"
      )
    )

  // Inside the scope, Age is Boolean: one alternative, 4 branches, 2 results each.
  def opaqueOutside = {
    val code = "object S { opaque type Age = Boolean; def mk(b: Boolean): Age = b; " +
      "def inside(a: Age, b: Boolean): Age | Boolean = b }\n" +
      "def u(a: S.Age, b: Boolean): S.Age | Boolean = b"
    reasonContains(code, "u", "opaque representation is not visible at use site")
      .and(count(code, "S.inside") === Count.Finite(16))
  }

  def topLike =
    reasonContains("def t(x: AnyRef, i: Boolean): AnyRef | Boolean = i", "t", "top-like")

  def missingDecl =
    reasonContains(
      "import ext.Json\nfinal case class Red(level: Boolean)\ndef m(j: Json, r: Red): Json | Red = r",
      "m",
      "union (Json | Red): declaration not supplied: ext.Json (named import Json)"
    )

  // `f(a)` is eliminated once (capabilities C10): `Red(level)` splits `level` (2) and `Blue` is one
  // case, and each of the three cases answers any of the three values `Red(true)`, `Red(false)`,
  // `Blue` — 3^3 = 27.
  def callableSum =
    count(colours + "def c[A](a: A)(f: A => Red | Blue): Red | Blue = f(a)", "c") ===
      Count.Finite(27)

  def sharedCase =
    reasonContains(
      "sealed trait P\nsealed trait Q\ncase object P1 extends P\ncase object PQ extends P with Q\n" +
        "def pq(p: P): P | Q = p",
      "pq",
      "P | Q — PQ derives from Q"
    )

  // The family reading refuses first; the class reading would find X through the alias too.
  def aliasChild =
    reasonContains(
      "sealed trait P\ntype PA = P\ncase object X extends PA with Q\nsealed trait Q\n" +
        "case object Q1 extends Q\ncase object P1 extends P\ndef pq(p: P): P | Q = p",
      "pq",
      "sealed family is extended through an alias: P"
    )

  def enumCase =
    reasonContains(
      "sealed trait Q\ncase object Q1 extends Q\nenum Color { case R; case G extends Color with Q }\n" +
        "def cq(c: Color): Color | Q = c",
      "cq",
      "a case of Color derives from Q"
    )

  // Inside O, `Mixin` may be a member of the unread `ext.Base`, which may be a supplied class.
  def unreadParent =
    reasonContains(
      "import ext.Base\nobject O extends Base {\n  sealed trait P\n  case object P1 extends P with Mixin\n" +
        "  sealed trait Q\n  case object Q1 extends Q with Mixin\n  def pq(p: P): P | Q = p\n}",
      "O.pq",
      "P | Q — ancestry of P1 not resolved: Mixin"
    )

  def anonymousBoth =
    reasonContains(
      families + "val a: P = new P {}\nval b: Q = new Q {}\ndef pq(q: Q): P | Q = q",
      "pq",
      "sealed family has anonymous instances in its file"
    )

  // P is {P1, E.A}, but its shape reads P1 alone: Q's side proves the pair apart, so only the
  // family check stops the sum from counting 2 instead of 3^2.
  def unenumeratedChild =
    reasonContains(
      "sealed trait P\ncase object P1 extends P\nenum E { case A extends E with P }\n" +
        "sealed trait Q\ncase object Q1 extends Q\ndef pq(p: P): P | Q = p",
      "pq",
      "sealed family P has a subclass this reading does not enumerate"
    )

  // `type Id[X] = X; case class C() extends Id[S]` is a child of S (Scala 3.9.0 warns that a match
  // on S1 alone misses C()), so S | Q has 3 values, not 2. A supplied class applied to the family
  // (`Visitor[R]`) cannot be dealiased: R | Q is R1 + Q1.
  def aliasApplicationChild = {
    val code = "type Id[X] = X\ntrait Visitor[A]\nsealed trait S\nfinal case class S1() extends S\n" +
      "final case class C() extends Id[S]\nsealed trait Q\nfinal case class Q1() extends Q\n" +
      "sealed trait R\nfinal case class R1() extends R\nfinal case class V() extends Visitor[R]\n" +
      "def u(): S | Q = ???\ndef w(): R | Q = ???"
    reasonContains(code, "u", "sealed family S may be extended through Id[S]")
      .and(count(code, "w") === Count.Finite(2))
  }

  // `class C` and an open `trait T` have no shape, and two spellings of one class are absorbed
  // before pairs are formed, so no count reaches these refusals; the pair check keeps them.
  def openPair = {
    val source = dialects.Scala3("class C\ntrait T\nfinal case class F()").parse[Source].get
    val index = new MethodAnalysis.Index(
      List(MethodAnalysis.Input("Open.scala", source)),
      MethodAnalysis.Limits()
    )
    val classes = index.runtimeClasses
    val root = ResolutionContext(index.peers(Nil).head, Map.empty, Set.empty)
    def runtime(name: String) = classes.alternative(Type.Name(name), root).toOption.get.runtime
    val option = RuntimeClasses.Builtin("scala.Option")
    (classes.disjoint(runtime("C"), runtime("T")) ===
      Left("C and T may overlap (non-final class and open trait)"))
      .and(
        classes.disjoint(option, option) === Left(
          "two alternatives share runtime class scala.Option"
        )
      )
      .and(classes.disjoint(runtime("F"), runtime("T")) === Right(()))
      .and(classes.disjoint(option, runtime("T")) === Right(()))
  }

  def aliasTwice =
    reasonContains(
      "final case class Box[T](t: T)\nfinal case class Crate()\ntype Boxed[A] = Box[A] | Crate\n" +
        "def b(x: Crate): Boxed[Boolean] | Boxed[Unit] = x",
      "b",
      "two alternatives share runtime class Box"
    )

}
