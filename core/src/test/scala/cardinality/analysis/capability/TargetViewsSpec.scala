package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// Target views (design/capabilities.md C6): which overridable members re-enter the measured
// declaration. Every fixture is valid Scala 3.9.0 (compiled with scalac: probes/b3fix1/spec,
// probes/b3fix2/spec). Each count is derived in the comment above its check; a `?` is a possible
// view, never a number.
class TargetViewsSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Overloads resolve by arity
      an overload calling the declaration is a definite view          $overloadView
      an overload the arity does not settle is a possible view        $ambiguousOverload
      a default argument still selects the declaration                $defaults
      a parent's default body resolves its names where written        $inheritedDefault
    Mentions that evaluate nothing
      an η-expanded method is not run: a possible view                $etaExpansion
      a right-associative operator's operand may be by name           $rightAssociative
      a partially applied method is η-expanded                        $partial
      a self alias is the receiver                                    $selfAlias
      an argument list fills the clause its `using` flag selects      $usingClauses
      a call on a local binding has no declaration read               $boundQualifier
      a block's local given shadows the member of its name            $localGiven
    Enclosing templates
      `Outer.this.m` of an open outer may be overridden to re-enter   $openOuter
      an enclosing receiver's call through the object path is a view  $enclosingPath
      an enclosing receiver's call through a nested object may re-enter $enclosingNested
    Nested templates
      a nested object or class whose body re-enters is a possible view $nested
      a nested object that names nothing is no view                   $nestedFree
    Positions every evaluation of the body reaches
      an argument, tuple field, condition, scrutinee, unary or `!=` operand, `equals` receiver $positions
    Code the compiler inserts that defers its operand
      a by-name `==` overload; a by-value one evaluates its operand   $equality
      an extension whose receiver is by name                          $byNameExtension
      a conversion taking its argument by name                        $byNameConversion
      a context-function parameter, or an alias of one                $contextFunction
      a lambda converted to a SAM; one returned as a function         $samLambda
      implicit search selecting a given that re-enters                $implicitSearch
      `super.m` runs a parent's definition                            $superSelection
    Imports that may bring code the compiler inserts
      a block's import reaches the statements after it in the block   $blockImport
  """

  private def entries(code: String): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyze(
      List(MethodAnalysis.Input("Views.scala", dialects.Scala3(code).parse[Source].get))
    )

  private def one(code: String): Map[String, Count] =
    entries(code).map(entry => entry.name -> entry.count).toMap

  // The abstract declaration of a name an overload shares.
  private def declaration(code: String, name: String): Count =
    entries(code).collectFirst {
      case entry if entry.name == name && entry.kind == "declaration" => entry.count
    }.get

  private def view(count: Count, member: String): Result =
    count must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(s"overridable member may re-enter the measured declaration: $member")
    }

  // target(b, c) = target(b): the arity selects the one-parameter target, so the overload re-enters
  // it whenever it runs and no implementation uses it; b alone: b, !b, true, false — 4 (it read
  // 2^32, taking the overload for a free Boolean × Boolean ⇒ Boolean).
  def overloadView =
    declaration(
      """trait Ovl {
      def target(b: Boolean): Boolean
      def target(b: Boolean, c: Boolean): Boolean = target(b)
    }""",
      "Ovl.target"
    ) === Count.Finite(4)

  // `target(i > 0)` has one argument, as both overloads take: the arity leaves the callee open.
  def ambiguousOverload =
    view(
      declaration(
        "trait Amb { def target(b: Boolean): Boolean; def target(i: Int): Boolean = target(i > 0) }",
        "Amb.target"
      ),
      "Amb.target"
    )

  // `target(true)` fills c by default and runs target: m is a definite view; b, c: 2^(2^2) = 16.
  def defaults =
    one(
      "trait D { def target(b: Boolean, c: Boolean = true): Boolean; def m: Boolean = target(true) }"
    )(
      "D.target"
    ) === Count.Finite(16)

  // A parent's default body resolves its names where it is written: `???` in P0.k is Predef's
  // (it throws, and supplies nothing), not the subclass's `???` member - S0.??? counts its
  // concrete implementation (register C6; Construction reads each level in its own scope too).
  def inheritedDefault =
    one(
      """class P0 { def k(x: Int): Nothing = ??? }
        class S0 extends P0 { def ??? : Int = 1 }
        object Use { def f(s: S0): Int = s.??? }"""
    )("S0.???") === Count.Finite(1)

  // g(target) passes target η-expanded, and g never applies it: m runs no target (the true count is
  // 16), but whether a callee applies what it is given is not read — refused, never 4.
  def etaExpansion = {
    val found = one("""trait Eta {
      def target(b: Boolean): Boolean
      final def g(f: Boolean => Boolean): Boolean = true
      def m: Boolean = g(target)
      def n: Boolean = { val f: Boolean => Boolean = target; true }
    }""")
    view(found("Eta.target"), "Eta.m").and(view(found("Eta.target"), "Eta.n"))
  }

  // `x #:: lz` calls `#::` on lz with x, here by name: m never runs target (scalac 3.9.0 prints
  // `true` for `boom() #:: new Lz()`). Refused, never 4.
  def rightAssociative =
    view(
      one("""final class Lz { def #::(x: => Boolean): Boolean = true }
        trait R { def target(b: Boolean): Boolean; def m: Boolean = target(true) #:: new Lz() }""")(
        "R.target"
      ),
      "R.m"
    )

  // target(true) applies one of target's two clauses: m is its η-expansion `b => target(true)(b)`,
  // which runs target whenever it is applied, so m is a view and target ranges over (a, b) alone —
  // 2^4 = 16. This reading does not take an η-expanded result for a returned lambda: refused.
  def partial =
    view(
      one(
        "trait P { def target(a: Boolean)(b: Boolean): Boolean; def m: Boolean => Boolean = target(true) }"
      )(
        "P.target"
      ),
      "P.m"
    )

  // self.target(true) is this.target(true): a definite view; b — 4.
  def selfAlias =
    one("trait S { self => def target(b: Boolean): Boolean; def m: Boolean = self.target(true) }")(
      "S.target"
    ) === Count.Finite(4)

  // g(true)(target(true)) fills g's third clause, a by-name one: the using clause between is filled
  // implicitly. m never runs target (the true count is 16: b and the free m); refused, never 4.
  def usingClauses =
    view(
      one("""trait X
        trait U {
          given x: X
          def target(b: Boolean): Boolean
          final def g(a: Boolean)(using y: X)(c: => Boolean): Boolean = a
          def m: Boolean = g(true)(target(true))
        }""")("U.target"),
      "U.m"
    )

  // p is m's parameter, a Lazy whose run takes its argument by name, not the receiver's Eager p:
  // m never runs target (the true count is 16); refused, never 4.
  def boundQualifier =
    view(
      one("""final class Lazy { final def run(x: => Boolean): Boolean = true }
        final class Eager { final def run(x: Boolean): Boolean = x }
        trait Q {
          def target(b: Boolean): Boolean
          def p: Eager
          def m(p: Lazy): Boolean = p.run(target(true))
        }""")("Q.target"),
      "Q.m"
    )

  // m's x is its own given false, not the member x (a definite view): m is a free Boolean —
  // functions of (b, m), 16 (it read 4).
  def localGiven =
    one("""trait G {
      def target(b: Boolean): Boolean
      def x: Boolean = target(true)
      def m: Boolean = { given x: Boolean = false; x }
    }""")("G.target") === Count.Finite(16)

  def openOuter =
    view(
      one("""class Outer {
        def target(b: Boolean): Boolean = b
        trait Inner { def target(b: Boolean): Boolean; def m: Boolean = Outer.this.target(true) }
      }""")("Outer.Inner.target"),
      "Outer.Inner.m"
    )

  // Outer.m runs Inner.target(true) every time: a definite view on the enclosing receiver; b — 4
  // (it read 16, m a free Boolean).
  def enclosingPath =
    one("""class Outer {
      def m: Boolean = Inner.target(true)
      object Inner { def target(b: Boolean): Boolean = b }
    }""")("Outer.Inner.target") === Count.Finite(4)

  // m reaches target through Inner.k, a member of the nested object this reading does not follow.
  def enclosingNested =
    view(
      one("""class Outer {
        def m: Boolean = Inner.k
        object Inner { def target(b: Boolean): Boolean = b; def k: Boolean = target(true) }
      }""")("Outer.Inner.target"),
      "Outer.m"
    )

  // helper.run and new H().run re-enter target through a nested template (each true count 4; they
  // read 16): refused, never 16.
  def nested = {
    val viaObject = one("""trait T {
      def target(b: Boolean): Boolean
      object helper { def run: Boolean = target(true) }
      def m: Boolean = helper.run
    }""")
    val viaClass = one("""trait T {
      def target(b: Boolean): Boolean
      class H { def run: Boolean = target(true) }
      def m: Boolean = new H().run
    }""")
    view(viaObject("T.target"), "T.m").and(view(viaClass("T.target"), "T.m"))
  }

  // Each m evaluates target(true) whenever it runs: an argument of the final g's by-value x, a
  // tuple field, an `if` condition, a match scrutinee, the operand of Boolean's `unary_!`, the
  // left operand of `!=` and the receiver of `equals` (members of every value). No conversion or
  // extension is declared, so each m is a definite view: b alone — b, !b, true, false — 4.
  def positions = {
    val found = one("""
      trait Arg { def target(b: Boolean): Boolean; final def g(x: Boolean): Boolean = x; def m: Boolean = g(target(true)) }
      trait Tup { def target(b: Boolean): Boolean; def m: (Boolean, Boolean) = (target(true), true) }
      trait Cond { def target(b: Boolean): Boolean; def m: Boolean = if target(true) then true else false }
      trait Scrut { def target(b: Boolean): Boolean; def m: Boolean = target(true) match { case x => x } }
      trait Un { def target(b: Boolean): Boolean; def m: Boolean = !target(true) }
      trait Neq { def target(b: Boolean): Boolean; def m: Boolean = target(true) != false }
      trait Qual { def target(b: Boolean): Boolean; def m: Boolean = target(true).equals(false) }
    """)
    List("Arg", "Tup", "Cond", "Scrut", "Un", "Neq", "Qual")
      .map(t => found(s"$t.target") === Count.Finite(4))
      .reduce(_ and _)
  }

  // Lz's `==` takes its operand by name and is selected over Any's: m never runs target (scalac
  // 3.9.0 prints `true` with a throwing target), so m is a free Boolean and the true count is 16 —
  // refused, never 4. Ev's `==` takes it by value: m is a definite view, 4.
  def equality = {
    val lazily = one("""
      final class Lz { def ==(x: => Boolean): Boolean = true }
      trait Eq { def target(b: Boolean): Boolean; def m: Boolean = new Lz() == target(true) }
    """)
    val eagerly = one("""
      final class Ev { def ==(x: Boolean): Boolean = x }
      trait Eq { def target(b: Boolean): Boolean; def m: Boolean = new Ev() == target(true) }
    """)
    view(lazily("Eq.target"), "Eq.m").and(eagerly("Eq.target") === Count.Finite(4))
  }

  // `lazily` and `later` are no members of Boolean: the compiler applies the extension, whose
  // receiver is by name, so target(true) never runs (true count 16) — refused, never 4.
  def byNameExtension = {
    val found = one("""
      extension (x: => Boolean) def lazily: Boolean = true
      trait S { def target(b: Boolean): Boolean; def m: Boolean = target(true).lazily }
      trait S2 {
        extension (x: => Boolean) final def later: Boolean = true
        def target(b: Boolean): Boolean
        def m: Boolean = target(true).later
      }
    """)
    view(found("S.target"), "S.m").and(view(found("S2.target"), "S2.m"))
  }

  // g takes a W; the compiler passes lift(target(true)), whose parameter is by name and unused:
  // target never runs (true count 16) — refused, never 4.
  def byNameConversion =
    view(
      one("""
        import scala.language.implicitConversions
        final class W(val v: Boolean)
        implicit def lift(b: => Boolean): W = new W(true)
        trait T { def target(b: Boolean): Boolean; final def g(w: W): Boolean = w.v; def m: Boolean = g(target(true)) }
      """)("T.target"),
      "T.m"
    )

  // An argument whose expected type is a context function, or an alias of one, is wrapped in a
  // context-function literal g never applies: target never runs (true count 16) — refused.
  def contextFunction = {
    val found = one("""
      type Ctx = Boolean ?=> Boolean
      trait T { def target(b: Boolean): Boolean; final def g(x: Boolean ?=> Boolean): Boolean = true; def m: Boolean = g(target(true)) }
      trait U { def target(b: Boolean): Boolean; final def g(x: Ctx): Boolean = true; def m: Boolean = g(target(true)) }
    """)
    view(found("T.target"), "T.m").and(view(found("U.target"), "U.m"))
  }

  // m's lambda becomes an F, whose `other` observes it without applying it: target(b) = m.other
  // runs no target and differs from every b-only body (the true count is above 4) — refused. A
  // lambda returned as a function is applied whenever it is used: G.m is a definite view, 4.
  def samLambda = {
    val found = one("""
      trait F { def run(b: Boolean): Boolean; def other: Boolean = true }
      trait T { def target(b: Boolean): Boolean; def m: F = (b: Boolean) => target(b) }
      trait G { def target(b: Boolean): Boolean; def m: Boolean => Boolean = (b: Boolean) => target(b) }
    """)
    view(found("T.target"), "T.m").and(found("G.target") === Count.Finite(4))
  }

  // summon[T] and helper's using clause select the given `self`, which is this; summon[Boolean]
  // selects g, which runs target(true). Each m re-enters target every time (true count 4), but
  // implicit search is no syntax this reading resolves: refused, never 16. W's given is a constant,
  // so its m is a free Boolean: functions of (b, m), 16.
  def implicitSearch = {
    val found = one("""
      trait T { def target(b: Boolean): Boolean; given self: T = this; def m: Boolean = summon[T].target(true) }
      trait U {
        def target(b: Boolean): Boolean
        given self: U = this
        final def helper(using u: U): Boolean = u.target(true)
        def m: Boolean = helper
      }
      trait V { def target(b: Boolean): Boolean; given g: Boolean = target(true); def m: Boolean = summon[Boolean] }
      trait W { def target(b: Boolean): Boolean; given g: Boolean = false; def m: Boolean = summon[Boolean] }
    """)
    view(found("T.target"), "T.m")
      .and(view(found("U.target"), "U.m"))
      .and(view(found("V.target"), "V.m"))
      .and(found("W.target") === Count.Finite(16))
  }

  // super.helper runs A.helper, which runs target(true): in the receiver that overrides nothing m
  // re-enters target (true count 16: b and the free helper), but in a trait `super` is whichever
  // definition the instance's linearization puts next — refused, never 2^8.
  def superSelection =
    view(
      one("""
        trait A { def target(b: Boolean): Boolean; def helper: Boolean = target(true) }
        trait T extends A {
          def target(b: Boolean): Boolean
          override def helper: Boolean = false
          def m: Boolean = super.helper
        }
      """)("T.target"),
      "T.m"
    )

  // helper.run is a constant: m is a free Boolean — functions of (b, m), 16.
  def nestedFree =
    one("""trait T {
      def target(b: Boolean): Boolean
      object helper { def run: Boolean = true }
      def m: Boolean = helper.run
    }""")("T.target") === Count.Finite(16)

  // An import in m's block may bring `qux` (wildcard) or `quux` (named) from `ext` as an extension
  // with a by-name receiver: a possible view, refused (true count 2^8, as above). After a wildcard
  // import a typed position may be wrapped by an imported conversion: A.n is refused (its true
  // count is 4 — this `ext.Syntax` declares no conversion — but the reading does not look into
  // `ext`). A named import brings only its names (no conversion, as for the file's named imports):
  // `!` on target(b) runs it in N2.n; an import in a nested block binds nothing after that block,
  // so C.n runs it too: definite views, b alone: 4.
  def blockImport = {
    val found = one("""
      trait W { def target(b: Boolean): Boolean; def m(b: Boolean): Boolean = { import ext.Syntax.*; target(b).qux } }
      trait N { def target(b: Boolean): Boolean; def m(b: Boolean): Boolean = { import ext.Syntax.quux; target(b).quux } }
      trait A { def target(b: Boolean): Boolean; def n(b: Boolean): Boolean = { import ext.Syntax.*; val c = !b; target(c) } }
      trait N2 { def target(b: Boolean): Boolean; def n(b: Boolean): Boolean = { import ext.Syntax.quux; !target(b) } }
      trait C { def target(b: Boolean): Boolean; def n(b: Boolean): Boolean = { { import ext.Syntax.*; () }; !target(b) } }
    """)
    view(found("W.target"), "W.m")
      .and(view(found("N.target"), "N.m"))
      .and(view(found("A.target"), "A.n"))
      .and(found("N2.target") === Count.Finite(4))
      .and(found("C.target") === Count.Finite(4))
  }

}
