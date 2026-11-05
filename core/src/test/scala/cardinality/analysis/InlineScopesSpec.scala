package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** What an inline body reaches besides its own calls: the definitions written inside it, the
  * templates `this` names there, the clauses an application applies to a callee's result, the
  * classes an expansion reifies, and conversions behind aliases (register D27). Every fixture is
  * valid Scala 3.9.0 (probes/a6-fix2, run: the printed values are the ones the comments quote).
  */
class InlineScopesSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Definitions inside an inline body are read where they are written
      a local method's using clause, context bound and default         $localClauses
      a local method or given read like a supplied one                  $localPassing
      this in an anonymous class, a local class, a local given          $nestedThis
      a qualified this names its own template                           $qualifiedThis
      a local object's members are its own                              $localObject
      a local type that hides an outer one refuses                      $localTypes
    Applications are read to their last clause
      a clause a callee does not take applies its result's apply        $furtherClauses
      library and Predef results applied again                          $libraryClauses
      clauses a callee takes are its own                                $ownClauses
    Runtime classes, alias chains and verdicts
      classOf of an array of a type variable reifies the variable       $reifiedClasses
      a conversion behind any number of aliases is found                $aliasChains
      a given whose type a wildcard may hide is read as its declaration  $doubtedConversions
      a listed namespace's wildcard binds its own terms only            $listedTerms
      a reading cut short decides nothing                               $interrupted
      a deferred inline member is an inline body's capability          $deferredMembers
      a passing body is eliminated by the solver like its def twin      $solverTwins
  """

  private def parsed(code: String): MethodAnalysis.Input =
    MethodAnalysis.Input("Example.scala", dialects.Scala3(code).parse[Source].get)

  private def pow2(exponent: Int): Count =
    Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, exponent)))

  private val flagGiven = """import scala.compiletime.summonFrom
    |final class Flag(val b: Boolean)
    |object Gs {
    |  inline given Flag = summonFrom { case x: Boolean => new Flag(x); case _ => new Flag(true) }
    |  inline def fromGiven: Int = summonFrom { case n: Int => n; case _ => 5 }
    |}
    |import Gs.given
    |trait Has[T] { def get: Boolean }
    |""".stripMargin

  // Fixture locals: pick prints true, then false under a call-site `given Boolean = false`; bound
  // true, then false; viaDefault 5, then 9 under `given Int = 9` — the given implicit search fills
  // at the local call, and the default evaluated there, are expanded at each call site. The def
  // twin pickD prints true at both.
  def localClauses = {
    val found = rows(flagGiven + """object U {
      |  inline def pick(b: Boolean): Boolean = {
      |    def read(using f: Flag): Boolean = f.b
      |    read
      |  }
      |  inline def bound(b: Boolean): Boolean = {
      |    def read[T: Has](t: T)(using h: Has[T]): Boolean = h.get
      |    read(b)
      |  }
      |  inline def viaDefault(x: Int): Int = {
      |    def h(t: Int = Gs.fromGiven): Int = t
      |    h()
      |  }
      |  def pickD(b: Boolean): Boolean = {
      |    def read(using f: Flag): Boolean = f.b
      |    read
      |  }
      |}""".stripMargin)
    refused(found("U.pick"), "inline method: leaves a given of read to implicit search")
      .and(refused(found("U.bound"), "inline method: leaves a given of read to implicit search"))
      .and(refused(found("U.viaDefault"), "calls inline method fromGiven, which uses"))
      .and(refused(found("U.viaDefault.h"), "definition inside an inline method: viaDefault"))
      .and(found("U.pickD") === Count.Finite(4))
  }

  // Fixture locals: plain(1) = 2 and localGiven(false) = false at both sites. plain is every
  // Int => Int, 2^(2^37); localGiven every Boolean => Boolean, 4.
  def localPassing = {
    val found = rows(flagGiven + """object U {
      |  inline def plain(x: Int): Int = {
      |    def h(y: Int, z: Int = 1): Int = y + z
      |    h(x)
      |  }
      |  inline def localGiven(b: Boolean): Boolean = {
      |    given flag: Flag = new Flag(b)
      |    def read(using f: Flag): Boolean = f.b
      |    read(using flag)
      |  }
      |}""".stripMargin)
    (found("U.plain") === pow2(37)).and(found("U.localGiven") === Count.Finite(4))
  }

  private val mid = """import scala.compiletime.summonFrom
    |abstract class Mid {
    |  inline def h: Int = summonFrom { case n: Int => n; case _ => 5 }
    |  def k: Int
    |}
    |trait Plain { def k: Int; def base: Int = 3 }
    |""".stripMargin

  // Fixture templates: anon, local and viaGiven print 5, then 9 under `given Int = 9` — `this` in
  // each template is the new Mid, whose inline h is expanded at the call site, not U's own h.
  // plainAnon(1) = 4 at both: every Int => Int.
  def nestedThis = {
    val found = rows(mid + """object U {
      |  def h: Int = 1
      |  def run(m: Mid): Int = m.k
      |  def runPlain(p: Plain): Int = p.k
      |  inline def anon(x: Int): Int = run(new Mid { def k: Int = this.h })
      |  inline def local(x: Int): Int = {
      |    class L extends Mid { def k: Int = this.h }
      |    run(new L)
      |  }
      |  inline def viaGiven(x: Int): Int = {
      |    given m: Mid with { def k: Int = this.h }
      |    run(m)
      |  }
      |  inline def plainAnon(x: Int): Int = runPlain(new Plain { def k: Int = this.base + x })
      |}""".stripMargin)
    val expands = "calls inline method h, which uses scala.compiletime.summonFrom"
    refused(found("U.anon"), expands)
      .and(refused(found("U.local"), expands))
      .and(refused(found("U.viaGiven"), expands))
      .and(found("U.plainAnon") === pow2(37))
  }

  // Fixture templates: outer prints 5, then 9 — `Outer.this.f` is re-selected on the call site's
  // SubOuter, whose f is inline; In's own f is not the one named. A final Shut has no subclass:
  // every Int => Int.
  def qualifiedThis = {
    val found = rows("""import scala.compiletime.summonFrom
      |class Outer {
      |  def f(x: Int): Int = x
      |  final class In {
      |    def f(x: Int): Int = x + 100
      |    inline def g(x: Int): Int = Outer.this.f(x)
      |  }
      |}
      |final class SubOuter extends Outer {
      |  override inline def f(x: Int): Int = summonFrom { case n: Int => n; case _ => x }
      |}
      |final class Shut {
      |  def f(x: Int): Int = x
      |  final class In { inline def g(x: Int): Int = Shut.this.f(x) }
      |}""".stripMargin)
    refused(
      found("Outer.In.g"),
      "selects f on Outer.this, which a call site may type as a subclass of Outer that overrides f"
    ).and(found("Shut.In.g") === pow2(37))
  }

  // Fixture templates: localObject(4) = 4 at both sites. The local object's g is read like a
  // supplied object's member, so f counts as its def twin (review-A6 q localobj named it "a library
  // function").
  def localObject = {
    val found = rows("""object Use {
      |  inline def f(x: Int): Int = { object O { def g(y: Int): Int = y }; O.g(x) }
      |  def fD(x: Int): Int = { object O { def g(y: Int): Int = y }; O.g(x) }
      |  inline def other(x: Int): Int = { class K; K.g(x) }
      |}""".stripMargin)
    (found("Use.f") === pow2(37))
      .and(found("Use.f") === found("Use.fD"))
      .and(
        refused(found("Use.other"), "reads the local K, which has no companion this reading reads")
      )
  }

  // A local class named like a class outside the body would be read as the outer one, whose members
  // are not the local one's.
  def localTypes = {
    val found = rows("""final class Fn { def apply(x: Int): Int = x }
      |object U {
      |  inline def hidden(x: Int): Int = { final class Fn { def apply(y: Int): Int = y }; (new Fn)(x) }
      |  inline def fresh(x: Int): Int = { final class Gn { def apply(y: Int): Int = y }; (new Gn)(x) }
      |}""".stripMargin)
    refused(found("U.hidden"), "declares the local type Fn, which hides a type of that name")
      .and(refused(found("U.fresh"), "creates an instance of a class not read"))
  }

  private val fn = """import scala.compiletime.summonFrom
    |final class Fn {
    |  inline def apply(x: Int): Int = summonFrom { case n: Int => n; case _ => x }
    |}
    |object Obj {
    |  def g: Fn = new Fn
    |  def m(y: Int): Fn = new Fn
    |  val v: Fn = new Fn
    |  def inc: Int => Int = _ + 1
    |  def add(a: Int)(b: Int): Int = a + b
    |}
    |""".stripMargin

  // Fixture clauses: every one prints 5, then 9 under `given Int = 9` (the def twin viaDefD 5 at
  // both) — the clause the callee does not take applies the result's inline apply.
  def furtherClauses = {
    val found = rows(fn + """object U {
      |  inline def viaDef(x: Int): Int = Obj.g(x)
      |  inline def viaCurried(x: Int): Int = Obj.m(0)(x)
      |  inline def viaVal(x: Int): Int = Obj.v(x)
      |  inline def viaLocal(x: Int): Int = {
      |    def mk: Fn = new Fn
      |    mk(x)
      |  }
      |}""".stripMargin)
    val expands = "calls inline method apply, which uses scala.compiletime.summonFrom"
    refused(found("U.viaDef"), expands)
      .and(refused(found("U.viaCurried"), expands))
      .and(refused(found("U.viaVal"), expands))
      .and(refused(found("U.viaLocal"), expands))
  }

  // Fixture clauses: viaGet, viaOrElse and viaIdentity print 5, then 9. A library member's or
  // Predef function's result is not read, so the apply a further clause selects is refused.
  def libraryClauses = {
    val found = rows(fn + """object U {
      |  inline def viaGet(x: Int): Int = Some(new Fn).get(x)
      |  inline def viaOrElse(o: Option[Fn], x: Int): Int = o.getOrElse(new Fn)(x)
      |  inline def viaIdentity(x: Int): Int = identity(new Fn)(x)
      |}""".stripMargin)
    val unread = "selects apply on a value whose type is not read: the result of"
    refused(found("U.viaGet"), s"$unread get on Some")
      .and(refused(found("U.viaOrElse"), s"$unread getOrElse on scala.Option"))
      .and(refused(found("U.viaIdentity"), s"$unread identity"))
  }

  // A function result's apply is Function1's, both clauses of a curried method are its own, and
  // Option.fold takes two: every Int => Int, and every Option[Int] => Int (2^(32·(2^32 + 1))).
  def ownClauses = {
    val found = rows(fn + """object U {
      |  inline def viaFunction(x: Int): Int = Obj.inc(x)
      |  inline def viaAdd(x: Int): Int = Obj.add(1)(x)
      |  inline def viaFold(o: Option[Int]): Int = o.fold(0)((n: Int) => n + 1)
      |  def viaFoldD(o: Option[Int]): Int = o.fold(0)((n: Int) => n + 1)
      |}""".stripMargin)
    (found("U.viaFunction") === pow2(37))
      .and(found("U.viaAdd") === pow2(37))
      .and(found("U.viaFold") === found("U.viaFoldD"))
  }

  // Fixture reified: intArr[Int] is true and intArr[String] false, sameArr(1) true and
  // sameArr(true) false, where a method's erased Array[A] is never Array[Int]'s class. A fixed
  // class and a generic class (whose runtime class ignores its argument) are the same in every
  // expansion: [A]: Boolean has 2 implementations.
  def reifiedClasses = {
    val found = rows("""object C {
      |  def isIntArr(c: Any): Boolean = true
      |  inline def intArr[A]: Boolean = isIntArr(classOf[Array[A]])
      |  inline def sameArr[A](x: A): Boolean = isIntArr(classOf[Array[A]])
      |  inline def fixedArr[A]: Boolean = isIntArr(classOf[Array[Int]])
      |  inline def listOf[A]: Boolean = isIntArr(classOf[List[A]])
      |}""".stripMargin)
    val reifies = "reifies with classOf the class of Array[A] at run time"
    refused(found("C.intArr"), reifies)
      .and(refused(found("C.sameArr"), reifies))
      .and(found("C.fixedArr") === Count.Finite(2))
      .and(found("C.listOf") === Count.Finite(2))
  }

  // Fixture aliases: f(1) prints "none", then "abcd" under a call-site given String — nine aliases
  // deep (review-A6 q depth8: eight were past the old cut-off and counted).
  def aliasChains = {
    val aliases = (2 to 9).map(n => s"type A$n[X, Y] = A${n - 1}[X, Y]").mkString("\n")
    val found = rows(s"""import scala.compiletime.summonFrom
      |type A1[X, Y] = Conversion[X, Y]
      |$aliases
      |inline given A9[Int, String] = (x: Int) => summonFrom { case s: String => s; case _ => "none" }
      |object U { inline def f(x: Int): String = x }""".stripMargin)
    refused(found("U.f"), "may expand the inline conversion given A9[Int, String]")
  }

  // `forgetful.*` is in doubt (its `forgetful` may be `zzz.forgetful`), so the given's type name is
  // not read; the declaration it would otherwise be is: a plain trait leaves `U.f` its number (4),
  // one extending `Conversion` makes the inline-apply given a conversion every body may meet
  // (probes/a7fix1/conv, valid Scala 3.9.0). The alternative, a type outside the sources, is not
  // read as a conversion (D27's boundary). U.scala itself imports nothing.
  def doubtedConversions = {
    val use =
      "p/U.scala" -> "package p\nobject U {\n  enum B { case T, F }\n  def id(b: B): B = b\n  inline def f(x: B): B = id(x)\n}"
    val plain = counts(
      "p/forgetful.scala" -> "package p\npackage forgetful\ntrait ForgetfulFunctor[F[_]]",
      "p/given.scala" -> "package p\nimport zzz.*\nimport forgetful.*\ngiven g: ForgetfulFunctor[List] with {}",
      use
    )
    val converting = counts(
      "p/forgetful.scala" ->
        "package p\npackage forgetful\ntrait ForgetfulFunctor[A, C] extends Conversion[A, C]",
      "p/given.scala" ->
        "package p\nimport zzz.*\nimport forgetful.*\ngiven g: ForgetfulFunctor[Int, Long] with { inline def apply(i: Int): Long = i.toLong }",
      use
    )
    (plain("p.U.f") === Count.Finite(4))
      .and(refused(converting("p.U.f"), "may expand the inline conversion g"))
  }

  // `scala.quoted.*` declares the terms `Varargs` and `quotes` and no term `ExprMap` (a trait
  // alone): it binds `Varargs` over another file's object of the name, and leaves `ExprMap` the
  // package's (probes/a7fix1/listed, valid Scala 3.9.0).
  def listedTerms = {
    val found = counts(
      "p/defs.scala" ->
        "package p\nobject Varargs { val tag: Boolean = true }\nobject ExprMap { val tag: Boolean = true }",
      "p/u.scala" ->
        """|package p
           |import scala.quoted.*
           |object U {
           |  inline def viaVarargs(x: Boolean): Boolean = { val v = Varargs; x }
           |  inline def viaExprMap(x: Boolean): Boolean = { val v = ExprMap; x }
           |}""".stripMargin
    )
    refused(found("p.U.viaVarargs"), "reads Varargs, which the wildcard import scala.quoted.*")
      .and(found("p.U.viaExprMap") === Count.Finite(4))
  }

  // A budget that stops reading `caller` anywhere inside `leaf` leaves `leaf` unjudged, not open:
  // read again with the budget restored it is still refused for its inline match.
  def interrupted = {
    val input = parsed("""object Other { def k(x: Int): Int = x }
      |object Cut {
      |  inline def leaf(x: Int): Int = { val y: Int = Other.k(x); inline y match { case 1 => 1; case _ => 2 } }
      |  inline def mid(x: Int): Int = leaf(x)
      |  inline def caller(x: Int): Int = mid(x)
      |}""".stripMargin)
    val afterCuts: List[Count] = (1 to 300).toList.flatMap { quota =>
      val work = new AnalysisWork(Long.MaxValue, MethodAnalysis.Limits().maxTypeDepth)
      val index = new MethodAnalysis.Index(List(input), MethodAnalysis.Limits(), Some(work))
      val targets = index.selected(Set(input.path), Set("Cut.caller", "Cut.leaf"))
      work.target(quota)
      targets.filter(_.name == "Cut.caller").foreach(index.measure)
      work.target(Long.MaxValue)
      targets.filter(_.name == "Cut.leaf").map(target => index.measure(target).count)
    }
    val stillRefused = afterCuts.filter {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inline method: uses inline match"))
      case _ => false
    }
    (afterCuts.size === 300).and(stillRefused.size === 300)
  }

  // Fixture a6-land dip: g(new A) prints true and g(new B) false — each expansion calls the static
  // type's implementation of the deferred f, and g at a T does not compile. An abstract inline
  // member counts as an abstract `def` does: a capability of an inline body, so g is t.f, !t.f,
  // true or false — 4 (it was refused), and h the functions of (t.f, b) — 16; its def twin d,
  // where f is no term, is b's — 4.
  def deferredMembers = {
    val found = rows("""trait T { inline def f: Boolean }
      |final class A extends T { inline def f: Boolean = true }
      |object Use {
      |  inline def g(t: T): Boolean = t.f
      |  inline def h(t: T, b: Boolean): Boolean = b
      |  def d(t: T, b: Boolean): Boolean = b
      |}""".stripMargin)
    (found("Use.g") === Count.Finite(4))
      .and(found("Use.h") === Count.Finite(16))
      .and(found("Use.d") === Count.Finite(4))
  }

  // Fixture a6-land sums (eo's MendTearPrism, PickMendPrism and PickFold shapes): a passing body
  // reaches the solver's sum elimination (register D25) exactly as its def twin. getOption is None
  // or tear(s).toOption, 2; MT.replace is mend(b) or t on a Left, 2; PM.replace picks s or mend(b)
  // for each pair (pick(s) defined, pick(mend(b)) defined), 2^4 = 16; andThen is new PF(_ => None)
  // or the flatMap, 2.
  def solverTwins = {
    val found = rows("""final class MT[S, T, A, B](val tear: S => Either[T, A], val mend: B => T) {
      |  inline def getOption(s: S): Option[A] = tear(s).toOption
      |  inline def replace(b: B): S => T = s => tear(s) match { case Right(_) => mend(b); case Left(t) => t }
      |  def replaceD(b: B): S => T = s => tear(s) match { case Right(_) => mend(b); case Left(t) => t }
      |}
      |final class PM[S, A, B](val pick: S => Option[A], val mend: B => S) {
      |  inline def replace(b: B): S => S = s => pick(s) match { case Some(_) => mend(b); case None => s }
      |  def replaceD(b: B): S => S = s => pick(s) match { case Some(_) => mend(b); case None => s }
      |}
      |final class PF[S, A](val pick: S => Option[A]) {
      |  inline def andThen[C](inner: PF[A, C]): PF[S, C] = new PF(s => pick(s).flatMap(inner.pick))
      |  def andThenD[C](inner: PF[A, C]): PF[S, C] = new PF(s => pick(s).flatMap(inner.pick))
      |}""".stripMargin)
    (found("MT.getOption") === Count.Finite(2))
      .and(found("MT.replace") === Count.Finite(2))
      .and(found("MT.replaceD") === Count.Finite(2))
      .and(found("PM.replace") === Count.Finite(16))
      .and(found("PM.replaceD") === Count.Finite(16))
      .and(found("PF.andThen") === Count.Finite(2))
      .and(found("PF.andThenD") === Count.Finite(2))
  }

}
