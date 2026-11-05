package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** An inline method counts as a method when its body uses no compile-time operation
  * (docs/research/syntax-features.md, register D27). Every fixture is valid Scala 3.9.0 except
  * where a name outside the sources is the point (fixture a6 `r`, run: the printed values are the
  * ones the comments quote); each expected number is derived beside it.
  */
class InlineMethodsSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    An inline method without compile-time operations counts as a method
      forwarding, choosing and comparing bodies                      $plainBodies
      an inline parameter read as a value, twice, or passed on       $inlineParameters
      an inline extension and an inline method calling one           $inlineCalls
      inline vals and inline givens named in the body                $inlineValues
      definitions nested in a passing inline method                  $nestedPassing
      library members, extractors, named arguments, overriding       $libraryAndSyntax
    Compile-time operations refuse, wherever they hide
      scala.compiletime renamed, qualified, aliased or relative      $compileTimeNames
      a passing-looking method calling a failing one                 $transitive
      an inline parameter inspected by a callee                      $inspectedParameter
      a runtime type test on a type the expansion instantiates       $typeTests
      a given left to implicit search, summon, valueOf               $searches
      an abstract inline declaration and its implementation          $abstractInline
      the A5 fixtures stay refused                                   $a5Fixtures
      names the reading cannot resolve                               $unresolved
      a package's synthetic object inherits Any's members            $packageMembers
      compile-time operations in types the body writes               $compileTimeTypes
      a member that does not apply falls back to an extension        $extensionFallback
      an extension restating the member is never taken (P2)          $restatedExtension
      an extension on its own type parameter may answer anything     $binderExtension
      a member an argument's or a subclass's class may override      $overriddenMembers
      implicit search through extensions, context functions, eta     $searchForms
      a type test on an array reads its element type                 $arrayTests
    Code the compiler may insert around the body
      a channel outside the sources refuses all but identities       $channels
      an inline conversion of the sources refuses every body         $inlineConversion
      a conversion is read by its type, wherever it may be found     $conversionTypes
      a conversion local to another method is found nowhere else     $conversionScope
      an exported or a local conversion is found where it is visible $conversionVisibility
    Verdicts and reasons
      a cycle of inline calls is decided as one                      $cycles
      reasons quote what they name                                   $reasonPhrases
  """

  private def pow2(exponent: Int): Count =
    Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, exponent)))

  // fwd: every Int => Int, (2^32)^(2^32) = 2^(2^37); pick: x or y (2); digit: every Char =>
  // Boolean, 2^(2^16); plainIf: `if` is no `inline if`, so a function of x, 2^(2^37); checked: what
  // it throws is no value (D26), and the codeOf inside the throw changes nothing counted.
  def plainBodies = {
    val found = rows("""object Pos {
      |  inline def fwd(x: Int): Int = x
      |  inline def pick[A](x: A, y: A): A = x
      |  inline def digit(c: Char): Boolean = c >= '0' && c <= '9'
      |  inline def plainIf(x: Int): Int = if x == 1 then 10 else 20
      |  inline def checked(x: Int): Int =
      |    if x >= 0 then x else throw new IllegalArgumentException(scala.compiletime.codeOf(x))
      |}""".stripMargin)
    (found("Pos.fwd") === pow2(37))
      .and(found("Pos.checked") === pow2(37))
      .and(found("Pos.pick") === Count.Finite(2))
      .and(found("Pos.digit") === pow2(16))
      .and(found("Pos.plainIf") === pow2(37))
  }

  // An inline parameter is its argument substituted by name; with no side effects that is its
  // value (fixture a6 r prints twice(true) = true, passOn(false) = false, value(1) = 1). twice and
  // passOn: every Boolean => Boolean (4); value: 2^(2^37).
  def inlineParameters = {
    val found = rows("""object Pos {
      |  inline def twice(inline x: Boolean): Boolean = x && x
      |  inline def value(inline x: Int): Int = x
      |  inline def passOn(inline x: Boolean): Boolean = twice(x)
      |}""".stripMargin)
    (found("Pos.twice") === Count.Finite(4))
      .and(found("Pos.value") === pow2(37))
      .and(found("Pos.passOn") === Count.Finite(4))
  }

  // flip: Boolean => Boolean (4); callsPassing calls the passing isDigit: every Char => Boolean.
  def inlineCalls = {
    val found = rows("""object Pos {
      |  extension (b: Boolean) inline def flip: Boolean = !b
      |  inline def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
      |  inline def callsPassing(c: Char): Boolean = isDigit(c) || c == '_'
      |}""".stripMargin)
    (found("Pos.<extension@2>.flip") === Count.Finite(4))
      .and(found("Pos.callsPassing") === pow2(16))
  }

  // useLimit: every Int => Boolean, 2^(2^32); viaGiven names the passing inline given gv (an Int:
  // 2^32); viaBad names one whose summonFrom reads the call site (fixture a6 r: 0, and 4 under a
  // given "abcd").
  def inlineValues = {
    val found = rows("""import scala.compiletime.summonFrom
      |object Pos {
      |  inline val limit = 3
      |  inline def useLimit(x: Int): Boolean = x > limit
      |  inline def local(x: Int): Boolean = { inline val k = 3; x > k }
      |  inline given gv: Int = 3
      |  inline def viaGiven: Int = gv
      |  inline given bad: Int = summonFrom { case s: String => s.length; case _ => 0 }
      |  inline def viaBad: Int = bad
      |}""".stripMargin)
    (found("Pos.useLimit") === pow2(32))
      .and(found("Pos.local") === pow2(32))
      .and(found("Pos.viaGiven") === Count.Finite(BigInt(2).pow(32)))
      .and(
        refused(
          found("Pos.viaBad"),
          "calls inline method bad, which uses scala.compiletime.summonFrom"
        )
      )
  }

  // nested: x or y (2); its local g reads a, b, x and y like any local method (4).
  def nestedPassing = {
    val found = rows("""object Pos {
      |  inline def nested[A](x: A, y: A): A = { def g(a: A, b: A): A = a; g(x, y) }
      |}""".stripMargin)
    (found("Pos.nested") === Count.Finite(2)).and(found("Pos.nested.g") === Count.Finite(4))
  }

  // first: Int^(Int × Int) = 2^(32·2^64) = 2^(2^69); mk: P has 4 values, 4^2 = 16; opt and mtch: an
  // Option[Boolean] has 3 values, 2^3 = 8; lam applies a lambda literal, no argument's class: every
  // Boolean => Boolean, 4; typedCase tests the fixed class String: Any is one value, 2; Over.f
  // overrides a method, 2^(2^37).
  def libraryAndSyntax = {
    val found = rows("""final case class P(a: Boolean, b: Boolean)
      |class Base { def f(x: Int): Int = x }
      |class Over extends Base { override inline def f(x: Int): Int = x }
      |object L {
      |  inline def first(t: (Int, Int)): Int = t._1
      |  inline def mk(x: Boolean): P = P(b = x, a = x)
      |  inline def opt(o: Option[Boolean]): Boolean = o.getOrElse(false)
      |  inline def mtch(o: Option[Boolean]): Boolean = o match { case Some(b) => b; case None => false }
      |  inline def lam(b: Boolean): Boolean = ((x: Boolean) => !x)(b)
      |  inline def typedCase(x: Any): Boolean = x match { case _: String => true; case _ => false }
      |  inline def head(t: (Int, Int)): Int = t.head
      |}""".stripMargin)
    (found("L.first") === pow2(69))
      .and(found("L.mk") === Count.Finite(16))
      .and(found("L.opt") === Count.Finite(8))
      .and(found("L.mtch") === Count.Finite(8))
      .and(found("L.lam") === Count.Finite(4))
      .and(found("L.typedCase") === Count.Finite(2))
      .and(found("Over.f") === pow2(37))
      .and(
        refused(found("L.head"), "selects head on Tuple2, which the library table does not list")
      )
  }

  // Fixture a6 r: renamed[3] = 3, rooted[4] = 4, aliased[5] = 5, relative[6] = 6 — constValue
  // reads a type argument, however the path to it is spelled.
  def compileTimeNames = {
    val found = rows("""import scala.compiletime.{constValue as cv}
      |import scala.compiletime as ct
      |object Neg {
      |  inline def renamed[N <: Int]: Int = cv[N]
      |  inline def qualified[N <: Int]: Int = scala.compiletime.constValue[N]
      |  inline def rooted[N <: Int]: Int = _root_.scala.compiletime.constValue[N]
      |  inline def aliased[N <: Int]: Int = ct.constValue[N]
      |  inline def relative[N <: Int]: Int = compiletime.constValue[N]
      |}""".stripMargin)
    val uses = "inline method: uses scala.compiletime.constValue"
    refused(found("Neg.renamed"), uses)
      .and(refused(found("Neg.qualified"), uses))
      .and(refused(found("Neg.rooted"), uses))
      .and(refused(found("Neg.aliased"), uses))
      .and(refused(found("Neg.relative"), uses))
  }

  // Fixture a6 r: wrapper[Int](1) is 1, and 7 under a call-site `given Int = 7`.
  def transitive = {
    val found = rows("""import scala.compiletime.summonFrom
      |object Neg {
      |  inline def leaf[A](x: A): A = summonFrom { case g: A => g; case _ => x }
      |  inline def wrapper[A](x: A): A = leaf(x)
      |}""".stripMargin)
    refused(
      found("Neg.wrapper"),
      "calls inline method leaf, which uses scala.compiletime.summonFrom"
    )
  }

  // Fixture a6 r: passes(1) = 1, passes(-1) = 2; viaMatch(1) = 10 but viaMatch(one) = 20 with
  // one == 1 — even a by-value parameter's constant argument reaches the callee's inline match.
  def inspectedParameter = {
    val found = rows("""object Neg {
      |  inline def inspects(inline x: Int): Int = inline if x > 0 then 1 else 2
      |  inline def passes(inline y: Int): Int = inspects(y)
      |  inline def matches(inline x: Int): Int = inline x match { case 1 => 10; case _ => 20 }
      |  inline def viaMatch(y: Int): Int = matches(y)
      |}""".stripMargin)
    refused(found("Neg.passes"), "calls inline method inspects, which uses inline if")
      .and(refused(found("Neg.viaMatch"), "calls inline method matches, which uses inline match"))
  }

  // Fixture a6 b1: isA[String](1) is false where the method `isAd[String](1)` is true — Scala
  // substitutes the type argument into an expansion, so the test reads it.
  def typeTests = {
    val found = rows("""object Neg {
      |  inline def isA[A](x: Any): Boolean = x.isInstanceOf[A]
      |  inline def caseA[A](x: Any): Int = x match { case _: A => 1; case _ => 2 }
      |  def isAd[A](x: Any): Boolean = x.isInstanceOf[A]
      |}""".stripMargin)
    val tests = "at run time, which each expansion instantiates"
    refused(found("Neg.isA"), tests)
      .and(refused(found("Neg.caseA"), tests))
      .and(found("Neg.isAd") === Count.Finite(2))
  }

  // Fixture a6 b1: viaGiven prints "default", and "callsite" under a call-site given String: the
  // inline given the body's search chose is expanded at each call site. explicit passes its given
  // and reads as its def twin, refused for the solver's own reason (review-A6 q pins2).
  def searches = {
    val found = rows("""trait Show[A] { def show(a: A): String }
      |object Show { given anyShow[A]: Show[A] = (a: A) => "any" }
      |def useShow[A](a: A)(using s: Show[A]): String = s.show(a)
      |object Neg {
      |  inline def implicitly1[A](a: A): String = useShow(a)
      |  inline def explicit[A](a: A)(using s: Show[A]): String = useShow(a)(using s)
      |  def explicitD[A](a: A)(using s: Show[A]): String = useShow(a)(using s)
      |  inline def summoned[A](using A): A = summon[A]
      |  inline def v[A](using ValueOf[A]): A = valueOf[A]
      |}""".stripMargin)
    refused(found("Neg.implicitly1"), "leaves a given of useShow to implicit search")
      .and(found("Neg.explicit") === found("Neg.explicitD"))
      .and(refused(found("Neg.summoned"), "resolves a given with summon"))
      .and(refused(found("Neg.v"), "calls scala.Predef.valueOf, an inline method not supplied"))
  }

  // Fixture a6 r compiles both; Impl.abs prints 3. An abstract inline declaration counts as an
  // abstract `def` does, and its implementation through its own body: every Int — 2^32 each (both
  // were refused, the declaration having no body to check).
  def abstractInline = {
    val found = rows("""trait Abs { inline def abs: Int }
      |object Impl extends Abs { inline def abs: Int = 3 }
      |""".stripMargin)
    (found("Abs.abs") === Count.Finite(BigInt(2).pow(32)))
      .and(found("Impl.abs") === Count.Finite(BigInt(2).pow(32)))
  }

  // Fixture il (A5): byVal(1) = 10, byVal(a) = 20 with a == 1; pick[Int] and pick[String] choose
  // different arguments; summoned applies a call-site given. Fixture a5-fix1 r3: g's summonFrom
  // prints 1, and 7 under a call-site given — the local definition is refused with its owner.
  def a5Fixtures = {
    val found = rows("""import scala.compiletime.*
      |object T {
      |  inline def byVal(x: Int): Int = inline x match { case 1 => 10; case _ => 20 }
      |  inline def pick[A](x: A, y: A): A = inline erasedValue[A] match { case _: Int => y; case _ => x }
      |  trait Endo[A] { def apply(a: A): A }
      |  inline def summoned[A](x: A): A = summonInline[Endo[A]](x)
      |  inline def f[A](x: A, y: A): A = {
      |    def g(a: A, b: A): A = summonFrom { case given_a: A => given_a; case _ => a }
      |    g(x, y)
      |  }
      |}""".stripMargin)
    refused(found("T.byVal"), "inline method: uses inline match")
      .and(refused(found("T.pick"), "inline method: uses inline match"))
      .and(refused(found("T.summoned"), "inline method: uses scala.compiletime.summonInline"))
      .and(refused(found("T.f"), "inline method: uses scala.compiletime.summonFrom"))
      .and(
        refused(
          found("T.f.g"),
          "definition inside an inline method: f uses scala.compiletime.summonFrom"
        )
      )
  }

  // A name bound outside the sources, an extension and an unlisted library member are not
  // resolved, so the body is not shown free of compile-time operations.
  def unresolved = {
    val found = rows("""import ext.Lib
      |object L {
      |  inline def foreign(x: Int): Int = Lib.g(x)
      |  inline def ext(x: Int): Int = x.plusOne
      |  extension (x: Int) def plusOne: Int = x + 1
      |  inline def local(x: Int): Int = { import Lib.*; x }
      |}""".stripMargin)
    refused(found("L.foreign"), "reads Lib, a declaration not supplied: ext.Lib (import of Lib)")
      .and(refused(found("L.ext"), "selects plusOne on scala.Int"))
      .and(refused(found("L.local"), "imports inside the body"))
  }

  // Top-level definitions live in a synthetic object, whose members include `Any`'s: `toString` is
  // that object's, bound before the root imports — the order `TermBindings.scope` gives a package
  // scope. The inline method counts as its def twin: countably many Strings (review-A6 q pins1).
  def packageMembers = {
    val found = rows("""inline def shown(x: Int): String = toString
      |def shownD(x: Int): String = toString""".stripMargin)
    (found("shown") === found("shownD")).and(found("shown") === Count.Countable)
  }

  // Fixture a6 spec s3 compiles all three; S[0] is the literal type 1.
  def compileTimeTypes = {
    val found = rows("""object Ops {
      |  import scala.compiletime.ops.int.S
      |  inline def succ(x: Int): Int = { val y: S[0] = 1; x + y }
      |}
      |object OpsQualified {
      |  inline def succ(x: Int): Int = { val y: scala.compiletime.ops.int.S[0] = 1; x + y }
      |}
      |object Plain {
      |  inline def succ(x: Int): Int = { val y: Int = 1; x + y }
      |}""".stripMargin)
    refused(found("Ops.succ"), "inline method: uses scala.compiletime.ops")
      .and(refused(found("OpsQualified.succ"), "inline method: uses scala.compiletime.ops"))
      .and(found("Plain.succ") === pow2(37))
  }

  // Fixture a6 fb: `r.m(5)` takes the extension, R's member m wanting a String — call prints 2,
  // and 9 under a call-site `given Int = 9`. A member found is the one applied only when it
  // accepts the arguments, which this reading does not type.
  def extensionFallback = {
    val found = rows("""import scala.compiletime.summonFrom
      |final class R { def m(x: String): Int = 1 }
      |object R {
      |  extension (r: R) inline def m(x: Int): Int = summonFrom { case n: Int => n; case _ => 2 }
      |}
      |final class Q { def k(x: Int): Int = x }
      |object Use {
      |  inline def call(r: R): Int = r.m(5)
      |  inline def plain(q: Q, x: Int): Int = q.k(x)
      |}""".stripMargin)
    refused(found("Use.call"), "applies m, which an extension of that name may answer")
      .and(found("Use.plain") === pow2(37))
  }

  // An extension that restates the member (same parameter names and types under the receiver's
  // base type, an invariant receiver) never answers: the member applies wherever it would — the
  // declaration of CanGet the member implements only adds an alternative. callR is m's Int => Int
  // at 5 — 2^32; andThen builds Getter(s => inner.get(get(s))), a final class's record S => B
  // with S, B rigid and no other B — 1 (eo's Getter.andThen). Swapped's base Opt[A, S] gives the
  // extension s: A against the member's S, PolyGetter's get takes its own binder, Named's names
  // its parameter t, Hidden's is private, Q's member wants a String, CoOpt is covariant: the
  // extension may answer — `?`.
  def restatedExtension = {
    val found = rows("""import scala.compiletime.summonFrom
      |final class R { def m(x: Int): Int = 1 }
      |object R { extension (r: R) inline def m(x: Int): Int = summonFrom { case n: Int => n; case _ => 2 } }
      |final class Q { def m(x: String): Int = 1 }
      |object Q { extension (q: Q) inline def m(x: Int): Int = summonFrom { case n: Int => n; case _ => 2 } }
      |trait Opt[S, A] { def to(s: S): A }
      |object Opt { extension [S, A](self: Opt[S, A]) inline def get(s: S): A = summonFrom { case _ => self.to(s) } }
      |trait CanGet[S, A] { def get(s: S): A }
      |final class Getter[S, A](read: S => A) extends Opt[S, A], CanGet[S, A] {
      |  def to(s: S): A = read(s)
      |  def get(s: S): A = read(s)
      |  inline def andThen[B](inner: Getter[A, B]): Getter[S, B] = new Getter(s => inner.get(get(s)))
      |}
      |final class Swapped[S, A](read: S => A, back: A => S) extends Opt[A, S] {
      |  def to(a: A): S = back(a)
      |  def get(s: S): A = read(s)
      |  inline def twice(inner: Swapped[S, A], s: S): A = inner.get(s)
      |}
      |final class PolyGetter[S, A](read: S => A) extends Opt[S, A] {
      |  def to(s: S): A = read(s)
      |  def get[X](s: S): A = read(s)
      |  inline def again(p: PolyGetter[S, A], s: S): A = p.get(s)
      |}
      |final class Named[S, A](read: S => A) extends Opt[S, A] {
      |  def to(s: S): A = read(s)
      |  def get(t: S): A = read(t)
      |  inline def call(p: Named[S, A], s: S): A = p.get(s)
      |}
      |final class Hidden[S, A](read: S => A) extends Opt[S, A] {
      |  def to(s: S): A = read(s)
      |  private def get(s: S): A = read(s)
      |  inline def call(p: Hidden[S, A], s: S): A = p.get(s)
      |}
      |trait CoOpt[+S]
      |object CoOpt { extension [S](self: CoOpt[S]) inline def get(s: S): String = summonFrom { case n: String => n; case _ => "e" } }
      |final class CoGetter[S] extends CoOpt[S] { def get(s: S): String = "m" }
      |object Use {
      |  inline def callR(r: R): Int = r.m(5)
      |  inline def callQ(q: Q): Int = q.m(5)
      |  inline def same(q: CoGetter[Int]): String = q.get(1)
      |}""".stripMargin)
    (found("Use.callR") === Count.Finite(BigInt(2).pow(32)))
      .and(found("Getter.andThen") === Count.Finite(1))
      .and(refused(found("Swapped.twice"), "applies get, which an extension of that name may"))
      .and(refused(found("Use.callQ"), "applies m, which an extension of that name may answer"))
      .and(refused(found("Use.same"), "applies get, which an extension of that name may answer"))
      .and(
        List("PolyGetter.again", "Named.call", "Hidden.call")
          .map(name =>
            refused(found(name), "applies get, which an extension of that name may answer")
          )
          .reduce(_ and _)
      )
  }

  // The extension's receiver type is its own binder `Other`, not the class of that name: it applies
  // to any value, so where C's get(s: Int) rejects an A it answers, and its inline match on B = A
  // reveals A (3.9.0 run: U.f[Int] and U.f[String] print true and false). Refused, never 2.
  def binderExtension = {
    val found = rows("""import scala.compiletime.erasedValue
      |class Other
      |final class C { def get(s: Int): Int = s }
      |extension [Other](x: Other)
      |  inline def get[B](s: B): Boolean = inline erasedValue[B] match { case _: Int => true; case _ => false }
      |object U { inline def f[A](c: C, a: A): Boolean = c.get(a) }""".stripMargin)
    refused(found("U.f"), "applies get, which an extension of that name may answer")
  }

  // An unsupplied wildcard may bring a conversion the compiler inserts where a type does not
  // conform. same and wrap return a parameter of the result's own type (an opaque alias unfolded
  // in its scope): nothing is inserted, 1 each. call and pair have positions this reading does not
  // type.
  def channels = {
    val found = rows("""import ext.syntax.*
      |opaque type Box[A] = A
      |object Box { inline def wrap[A](a: A): Box[A] = a }
      |object W {
      |  inline def same[A](a: A): A = a
      |  inline def pair[A](a: A): (A, A) = (a, a)
      |}""".stripMargin)
    val channel = "may expand an implicit conversion from outside the sources"
    (found("Box.wrap") === Count.Finite(1))
      .and(found("W.same") === Count.Finite(1))
      .and(refused(found("W.pair"), channel))
  }

  // conv may be inserted wherever an expression's type does not conform (first), and answers an
  // applied selection no member accepts (inc); same returns its parameter of the result's type.
  // The closed world supplies every conversion, typed where the body is written: one whose own
  // expansion passes is a call like any other, so first and inc count as their `def` twins; one
  // that uses a compile-time operation is refused with it.
  def inlineConversion = {
    def found(body: String) = rows(s"""inline given conv: Conversion[Int, String] = $body
      |object C {
      |  inline def same[A](a: A): A = a
      |  inline def first(t: (Int, Int)): Int = t._1
      |  def firstD(t: (Int, Int)): Int = t._1
      |  inline def inc(x: Int): Int = x + 1
      |  def incD(x: Int): Int = x + 1
      |}""".stripMargin)
    val plain = found("_.toString")
    val compileTime = found("(x: Int) => scala.compiletime.codeOf(x)")
    (plain("C.same") === Count.Finite(1))
      .and(plain("C.first") === plain("C.firstD"))
      .and(plain("C.inc") === plain("C.incD"))
      .and(
        refused(
          compileTime("C.first"),
          "may expand the inline conversion conv, which uses scala.compiletime.codeOf"
        )
      )
      .and(refused(compileTime("C.inc"), "the implicit conversion conv may answer"))
  }

  // An expansion selects a member on its receiver's class at the call site, so an inline override
  // there runs instead (fixtures review-A6 h3, h7, h8: 5 without a given, 9 under `given Int = 9`).
  // A parameter, `this` of an open template, a lambda's parameter (fixture a6-fix1 d1, viaLambda: 9)
  // are refused where a supplied template overrides the member
  // inline with an expansion that differs at each call site (Sub, TwiceO, SrcI); a final class's
  // member, a local or an ascription (whose type the body
  // fixes: d1 prints 5) count as their `def` twins. So does a function parameter's `apply`: no
  // supplied inline `apply` differs at each call site. In the closed world, with no override supplied,
  // Base is final: onParam counts as onFinal does, and so does abstractMember as its `def` twin.
  def overriddenMembers = {
    val code = """class Base { def f(x: Int): Int = x }
      |final class Fixed { def f(x: Int): Int = x }
      |trait Src { def next(x: Int): Int }
      |trait Twice { def step(x: Int): Int = x; inline def twice(x: Int): Int = step(step(x)) }
      |final class Closed { def step(x: Int): Int = x; inline def twice(x: Int): Int = step(step(x)) }
      |object D {
      |  inline def onParam(b: Base, x: Int): Int = b.f(x)
      |  inline def onFinal(b: Fixed, x: Int): Int = b.f(x)
      |  def onFinalD(b: Fixed, x: Int): Int = b.f(x)
      |  inline def viaLocal(b: Base, x: Int): Int = { val y = b; y.f(x) }
      |  def viaLocalD(b: Base, x: Int): Int = { val y = b; y.f(x) }
      |  inline def viaAscription(b: Base, x: Int): Int = (b: Base).f(x)
      |  inline def viaLambda(b: Base, x: Int): Int = ((c: Base) => c.f(x))(b)
      |  inline def applyParam(g: Int => Int, x: Int): Int = g(x)
      |  def applyParamD(g: Int => Int, x: Int): Int = g(x)
      |  inline def applyLiteral(x: Int): Int = ((y: Int) => y + 1)(x)
      |  inline def abstractMember(s: Src, x: Int): Int = s.next(x)
      |  def abstractMemberD(s: Src, x: Int): Int = s.next(x)
      |}""".stripMargin
    val found = rows(
      code + """
      |class Sub extends Base { override inline def f(x: Int): Int = scala.compiletime.codeOf(x).length }
      |trait TwiceO extends Twice { override inline def step(x: Int): Int = scala.compiletime.codeOf(x).length }
      |class SrcI extends Src { inline def next(x: Int): Int = scala.compiletime.codeOf(x).length }""".stripMargin
    )
    val closed = rows(code)
    val overrides = "which a call site may type as a subclass of"
    refused(found("D.onParam"), s"selects f on b, $overrides Base that overrides f inline")
      .and(refused(found("Twice.twice"), s"selects step on this, $overrides Twice"))
      .and(refused(found("D.viaLambda"), s"selects f on c, $overrides Base"))
      .and(found("D.applyParam") === found("D.applyParamD"))
      .and(refused(found("D.abstractMember"), s"selects next on s, $overrides Src"))
      .and(found("D.onFinal") === found("D.onFinalD"))
      .and(found("D.onFinal") === pow2(37))
      .and(found("D.viaLocal") === found("D.viaLocalD"))
      .and(found("D.viaAscription") === found("D.viaLocalD"))
      .and(found("Closed.twice") === pow2(37))
      .and(found("D.applyLiteral") === pow2(37))
      .and(closed("D.onParam") === closed("D.onFinal"))
      .and(closed("Twice.twice") === pow2(37))
      .and(closed("D.abstractMember") === closed("D.abstractMemberD"))
  }

  // Fixtures review-A6 h4 and h7: the given chosen where the body is written is an inline one
  // whose summonFrom reads the call site ("plain", then "abcd"; 1, then 5): an extension group's
  // using clause, a context function read as its result, an eta-expanded method with a using
  // clause, and a callee taking a context function all leave a given to implicit search. With its
  // given written, viaExtensionExplicit is countably many Strings (review-A6 q pins3).
  def searchForms = {
    val found = rows("""import scala.compiletime.summonFrom
      |trait Show[A] { def show(a: A): String }
      |object Givens {
      |  inline given anInt: Int = summonFrom { case s: String => s.length; case _ => 0 }
      |}
      |import Givens.given
      |object E {
      |  extension (x: Int)(using s: Show[Int]) def shown: String = s.show(x)
      |  inline def viaExtension(x: Int): String = shown(x)
      |  inline def viaExtensionExplicit(x: Int, s: Show[Int]): String = shown(x)(using s)
      |  inline def viaContextFunction(f: Int ?=> Int): Int = f
      |  def plus(x: Int)(using n: Int): Int = x + n
      |  inline def viaEta(x: Int): Int = { val h: Int => Int = plus; h(x) }
      |  inline def explicitPlus(x: Int): Int = plus(x)(using 1)
      |  def twice(f: (Int ?=> Int) => Int): Int = f(1)
      |  inline def contextCallee(x: Int): Int = twice(g => x)
      |  inline def literal(x: Int): Int = { val g = (n: Int) ?=> n + x; g(using 1) }
      |}""".stripMargin)
    refused(found("E.viaExtension"), "leaves a given of shown to implicit search")
      .and(refused(found("E.viaContextFunction"), "reads f, of the context function type"))
      .and(refused(found("E.viaEta"), "leaves a given of plus to implicit search"))
      .and(
        refused(found("E.contextCallee"), "calls twice, whose signature has the context function")
      )
      .and(refused(found("E.literal"), "writes a context function literal"))
      .and(found("E.explicitPlus") === pow2(37))
      .and(found("E.viaExtensionExplicit") === Count.Countable)
  }

  // Fixture review-A6 h5: `isArr[Int](Array("a"))` is false and `isArr[String]` true — an array's
  // runtime class is its element type's, an alias's what it stands for in its class.
  def arrayTests = {
    val found = rows("""object O {
      |  inline def isArr[A](x: Any): Boolean = x.isInstanceOf[Array[A]]
      |  inline def nested[A](x: Any): Boolean = x.isInstanceOf[Array[Array[A]]]
      |  inline def strings(x: Any): Boolean = x.isInstanceOf[Array[String]]
      |}
      |class C[A] {
      |  type TA = Array[A]
      |  inline def viaMember(x: Any): Boolean = x.isInstanceOf[TA]
      |}""".stripMargin)
    val tests = "at run time, which each expansion instantiates"
    refused(found("O.isArr"), s"tests the type Array[A] $tests")
      .and(refused(found("O.nested"), tests))
      .and(refused(found("C.viaMember"), s"tests the type TA $tests"))
      .and(found("O.strings") === Count.Finite(2))
  }

  // Fixture review-A6 h10 (tag prints "none", then "abcd" under a given String): a `given … with`
  // whose apply is inline, an inline given spelled through an alias or a renaming import, and a
  // non-inline implicit object are conversions; `ConversionRate` is no conversion. The inline ones
  // here use a compile-time operation, so each refuses; the implicit object is typed where the body
  // is written, a call like any other: succ is every Int => Int, as with no conversion.
  def conversionTypes = {
    def file(code: String): Map[String, Count] = rows(code)
    val withApply = file("""import scala.compiletime.summonFrom
      |final class Tag(val s: String)
      |object Conv {
      |  given toTag: Conversion[Int, Tag] with
      |    inline def apply(x: Int): Tag = summonFrom { case s: String => new Tag(s); case _ => new Tag("none") }
      |}
      |import Conv.given
      |object U { inline def tag(x: Int): Tag = x }""".stripMargin)
    val aliased = file("""type Into[A, B] = Conversion[A, B]
      |inline given Into[Int, String] = (x: Int) => scala.compiletime.codeOf(x)
      |object U { inline def f(x: Int): String = x }""".stripMargin)
    val renamed = file("""import scala.{Conversion as C}
      |inline given C[Int, String] = (x: Int) => scala.compiletime.codeOf(x)
      |object U { inline def f(x: Int): String = x }""".stripMargin)
    val implicitObject = file("""object Conv {
      |  implicit object toLen extends Conversion[String, Int] { def apply(s: String): Int = s.length }
      |}
      |object U { inline def succ(x: Int): Int = x + 1 }""".stripMargin)
    val named = file("""trait ConversionRate
      |object U {
      |  given ConversionRate = new ConversionRate {}
      |  inline def succ(x: Int): Int = x + 1
      |}""".stripMargin)
    refused(withApply("U.tag"), "may expand the inline conversion toTag")
      .and(
        refused(withApply("Conv.toTag.apply"), "inline method: uses scala.compiletime.summonFrom")
      )
      .and(refused(aliased("U.f"), "may expand the inline conversion given Into[Int, String]"))
      .and(refused(renamed("U.f"), "may expand the inline conversion given C[Int, String]"))
      .and(implicitObject("U.succ") === pow2(37))
      .and(named("U.succ") === pow2(37))
  }

  // A given local to another method's body is in no scope the inline body reads (a6-quality
  // global: `x + 1` was refused for it).
  def conversionScope = {
    val found = rows("""package user
      |object Elsewhere {
      |  def local(): Unit = { given c: Conversion[String, Int] = _.length; () }
      |}
      |object Use { inline def succ(x: Int): Int = x + 1 }""".stripMargin)
    found("user.Use.succ") === pow2(37)
  }

  // Fixture a6-fix1b ex2: `r.m(5)` answered by RichR's inline m through an exported given and a
  // given local to the body (2, then 9 under `given Int = 9`); a method around an inline method is
  // a scope of it, and an object that only exports another's givens is no implicit scope. Each c
  // here uses a compile-time operation, so where it is visible it refuses.
  def conversionVisibility = {
    val found = rows("""package ex
      |object Hidden { inline given c: Conversion[String, Int] = (s: String) => scala.compiletime.codeOf(s).length }
      |object Pub { export Hidden.given }
      |object Use { export Hidden.given; inline def succ(x: Int): Int = x + 1 }
      |object Use2 { import Pub.given; inline def succ(x: Int): Int = x + 1 }
      |object Use3 { inline def succ(x: Int): Int = x + 1 }
      |object Local {
      |  def outer(y: Int): Int = {
      |    inline given c: Conversion[String, Int] = (s: String) => scala.compiletime.codeOf(s).length
      |    inline def succ(x: Int): Int = x + 1
      |    succ(y)
      |  }
      |}""".stripMargin)
    val answers = "the implicit conversion c may answer"
    refused(found("ex.Use.succ"), answers)
      .and(refused(found("ex.Use2.succ"), answers))
      .and(refused(found("ex.Local.outer.succ"), answers))
      .and(found("ex.Use3.succ") === pow2(37))
  }

  // ping and pong call each other and nothing else: every Int => Int each; tick and tock call each
  // other, and tock's summonFrom refuses both, whichever is read first.
  def cycles = {
    val found = rows("""import scala.compiletime.summonFrom
      |object Cy {
      |  inline def ping(x: Int): Int = if x > 0 then pong(x - 1) else x
      |  inline def pong(x: Int): Int = ping(x)
      |  inline def tick(x: Int): Int = if x > 0 then tock(x - 1) else x
      |  inline def tock(x: Int): Int = summonFrom { case n: Int => tick(n); case _ => x }
      |}""".stripMargin)
    (found("Cy.ping") === pow2(37))
      .and(found("Cy.pong") === pow2(37))
      .and(
        refused(
          found("Cy.tick"),
          "calls inline method tock, which uses scala.compiletime.summonFrom"
        )
      )
      .and(refused(found("Cy.tock"), "uses scala.compiletime.summonFrom"))
  }

  // Fixture a6-quality leak: a reason names a companion, an eta-expanded library function, an
  // anonymous given by its type, and quotes a tree as one line.
  def reasonPhrases = {
    val found = rows("""case class Foo(x: Int)
      |object Use {
      |  inline def f(y: Int): Foo = { val c = Foo; c(y) }
      |  inline def g(y: Int): Int = { val h = identity[Int]; h(y) }
      |  inline def k(y: Int): Int = (if (y > 0) {
      |    1
      |  } else 2).+(y)
      |}""".stripMargin)
    val anonymous = rows(
      """inline given Conversion[Int, String] = (x: Int) => scala.compiletime.codeOf(x)
      |object Use { inline def s(x: Int): String = x }""".stripMargin
    )
    refused(found("Use.f"), "a value whose type is not read: the companion of Foo")
      .and(refused(found("Use.g"), "a value whose type is not read: the function identity"))
      .and(refused(found("Use.k"), "not read: if (y > 0) { 1 } else 2"))
      .and(refused(anonymous("Use.s"), "the inline conversion given Conversion[Int, String]"))
  }

}
