package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
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
    A channel to declarations the sources do not supply keeps Any open
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
      a result with more values keeps the environment's reason       $nonTerminal
      a given selector binds no type name                            $rebinding
      a given selector spelling Any or Null binds neither            $rebindingTops
      a constructor's positions are value spaces, not goals          $constructorGoal
    Constructor slots read the implementation model
      an Any slot is one value, not the epsilon tier                 $anyBoxCtor
      an Any slot beside an Int is the Int's space                   $walkCtor
      an Any array beside them is a countable length                 $walkResCtor
      a binder named Nothing does not capture the empty slot         $nothingBinderCtor
      a source type named Nothing does not capture it either         $nothingAliasCtor
      a rewritten slot is read where its constructor is written      $rewrittenScope
    Unions that are not sums stay unresolved
      an alternative whose declaration was not supplied              $missingDecl
      a case extending through an alias                              $aliasChild
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

  def selfUnsuppliedParent =
    reasonContains(
      "trait Base extends ext.X\ntrait T { self: Base => def g(): Unit; def h(): Any }",
      "T.g",
      "builtin name Unit may be shadowed through a reference not supplied: ext.X (self type of T)"
    )

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

  def packageObject = {
    val code = "package p\npackage object p2 extends ext.X\npackage p2 {\n" +
      "  object M { def k(x: Any): Boolean = true; def r(b: Boolean): Any = b } }"
    reasonContains(code, "p.p2.M.k", "parents of package object p.p2")
      .and(reasonContains(code, "p.p2.M.r", "parents of package object p.p2"))
  }

  def foreignWildcard = {
    val imported = "import ext.*\ndef g(): Any = ()\ndef f(s: Schema): Unit = ()"
    val exported = "object E { export ext.defs.*; def f(s: ext.Schema): Unit = () }"
    reasonContains(imported, "g", "reference not supplied: ext (wildcard import of ext)")
      .and(
        reasonContains(
          imported,
          "f",
          "declaration not supplied: Schema",
          "builtin name Unit may be shadowed through a reference not supplied: ext"
        )
      )
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

  // `package extp; package inner` opens extp's members; with no unit of extp supplied, one may be
  // `type Any = Boolean` (Scala 3.9.0 compiles `val b: Boolean = extp.inner.U.f()` beside it).
  def chainedPackage = {
    val chained = "package extp\npackage inner\n" +
      "object U { def f(): Any = true; def g(x: Foo): Unit = () }"
    val supplied = "package extp\ntrait Foo"
    reasonContains(
      chained,
      "extp.inner.U.f",
      "builtin name Any may be shadowed through a reference not supplied: extp (package clause extp)"
    ).and(
      reasonContains(
        chained,
        "extp.inner.U.g",
        "builtin name Unit may be shadowed through a reference not supplied: extp",
        "declaration not supplied: Foo"
      )
    ).and(
      countIn("A.scala" -> chained, "B.scala" -> supplied)("extp.inner.U.f") === Count.Finite(1)
    ).and(
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

  def missingDecl =
    reasonContains(
      "import ext.Json\nfinal case class Red(level: Boolean)\ndef m(j: Json, r: Red): Json | Red = r",
      "m",
      "union (Json | Red): declaration not supplied: ext.Json (named import Json)"
    )

  // The family reading refuses first; the class reading would find X through the alias too.
  def aliasChild =
    reasonContains(
      "sealed trait P\ntype PA = P\ncase object X extends PA with Q\nsealed trait Q\n" +
        "case object Q1 extends Q\ncase object P1 extends P\ndef pq(p: P): P | Q = p",
      "pq",
      "sealed family is extended through an alias: P"
    )

}
