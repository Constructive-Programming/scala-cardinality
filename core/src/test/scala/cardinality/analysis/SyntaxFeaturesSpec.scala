package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Named tuples, literal types, context function types and compile-time evidence
  * (docs/research/syntax-features.md). Every fixture is valid Scala 3.9.0 except where a name
  * outside the sources is the point; each expected number is derived in the comment beside it.
  */
class SyntaxFeaturesSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Named tuples are the products of their fields
      fields are read as a tuple's                                   $namedFields
      a named tuple inside a type-lambda alias                       $namedAlias
      names are part of a rigid argument's key                       $namedKeys
      a named tuple and its tuple share a runtime class              $namedUnion
      a dependent function's named parameter is not a field          $dependentFunction
    Literal types have one value
      a literal goal has one implementation                          $literalGoals
      a literal is its own key, by class and constant                $literalKeys
      a floating zero is not read, its sign being lost               $floatingZero
    Context functions are functions whose binders are givens
      an input applies like a function                               $contextInputs
      a result builds a literal with named binders                   $contextResults
      a whole-body binder hides outer evidence of its type           $contextShadowing
      a nested context function wraps only its part                  $contextNested
      a binder behind a reading that stops is refused                $contextUnread
      a call returning a context function is applied                 $contextCalls
      an ascription to a context function is applied                 $contextAscription
    Compile-time and type-revealing evidence is refused
      an inline method counts unless its expansion reads its call    $inlineMethods
      a definition inside an inline method is expanded with it       $inlineNested
      a one-valued inline goal keeps its one implementation          $inlineTerminal
      a transparent named tuple is refused unless it is a singleton  $inlineNamedTuple
      runtime type evidence reveals its type argument                $revealingEvidence
      quoted code is compile-time reflection                         $quoted
  """

  // pair: each field from its one input (1); first: p.x (1); same: p.x or p.y (2); bool: every
  // function of two Booleans (2^4 = 16); one: the single field (1); nest: p.x.a or p.x.b (2).
  def namedFields = {
    val found = rows("""object N {
      |  def pair[A, B](a: A, b: B): (x: A, y: B) = (x = a, y = b)
      |  def first[A, B](p: (x: A, y: B)): A = p.x
      |  def same[A](p: (x: A, y: A)): A = p.x
      |  def bool(p: (x: Boolean, y: Boolean)): Boolean = p.x
      |  def one[A](p: (x: A)): A = p.x
      |  def nest[A](p: (x: (a: A, b: A), y: Unit)): A = p.x.a
      |}""".stripMargin)
    (found("N.pair") === Count.Finite(1))
      .and(found("N.first") === Count.Finite(1))
      .and(found("N.same") === Count.Finite(2))
      .and(found("N.bool") === Count.Finite(16))
      .and(found("N.one") === Count.Finite(1))
      .and(found("N.nest") === Count.Finite(2))
  }

  // eo's `GlassF`: the context comes from fa.context alone, and each index i gives f(fa.values(i)),
  // i being the only index: 1.
  def namedAlias =
    rows("""type G[I] = [X, A] =>> (context: X, values: I => A)
      |object M { def map[I, X, A, B](fa: G[I][X, A], f: A => B): G[I][X, B] = ??? }
      |""".stripMargin)("M.map") === Count.Finite(1)

  // `(x: A, y: A)` is the opaque `NamedTuple[("x", "y"), (A, A)]`: not `(A, A)`, nor `(y: A, x: A)`,
  // so a rigid F relates none of them (0); the same names, spelled either way, are one type (1).
  def namedKeys = {
    val found = rows("""object K {
      |  def plain[F[_], A](x: F[(x: A, y: A)]): F[(A, A)] = ???
      |  def same[F[_], A](x: F[(x: A, y: A)]): F[(x: A, y: A)] = x
      |  def swapped[F[_], A](x: F[(x: A, y: A)]): F[(y: A, x: A)] = ???
      |  def spelled[F[_], A](x: F[(x: A, y: A)]): F[scala.NamedTuple.NamedTuple[("x", "y"), (A, A)]] = x
      |}""".stripMargin)
    (found("K.plain") === Count.Finite(0))
      .and(found("K.same") === Count.Finite(1))
      .and(found("K.swapped") === Count.Finite(0))
      .and(found("K.spelled") === Count.Finite(1))
  }

  // A named tuple's runtime class is its tuple's, so no class test separates the alternatives.
  def namedUnion =
    refused(
      rows("object U { def un(p: (x: Int, y: Int) | (Int, Int)): Boolean = true }")("U.un"),
      "two alternatives share runtime class scala.Tuple2"
    )

  def dependentFunction =
    refused(
      rows("object D { def dep[A]: (x: A) => A = x => x }")("D.dep"),
      "unsupported type: x: A"
    )

  // One value each: the constant itself.
  def literalGoals = {
    val found = rows("""object L {
      |  def one: 1 = 1; def str: "a" = "a"; def ch: 'c' = 'c'; def half: 0.5 = 0.5
      |  def long: 1L = 1L; def yes: true = true; def neg: -1 = -1
      |}""".stripMargin)
    List("one", "str", "ch", "half", "long", "yes", "neg").map(n => found(s"L.$n")).distinct ===
      List(Count.Finite(1))
  }

  // F[1] and F[2], F[1L], F[-1] are unrelated types under a rigid F (0); F[1] is itself (1). A
  // floating literal is its value at its own width: `1.0` and `1.00` are one Double (1), `1.0f` a
  // Float (0); `0x10` is the Int 16 (1), and 'a' a Char, never the Int 97 (0).
  def literalKeys = {
    val found = rows("""object K {
      |  def k[F[_]](x: F[1]): F[2] = ???
      |  def same[F[_]](x: F[1]): F[1] = x
      |  def long[F[_]](x: F[1]): F[1L] = ???
      |  def str[F[_]](x: F["a"]): F["b"] = ???
      |  def neg[F[_]](x: F[-1]): F[1] = ???
      |  def dbl[F[_]](x: F[1.0]): F[1.00] = x
      |  def flt[F[_]](x: F[1.0]): F[1.0f] = ???
      |  def hex[F[_]](x: F[0x10]): F[16] = x
      |  def chr[F[_]](x: F['a']): F[97] = ???
      |}""".stripMargin)
    (found("K.k") === Count.Finite(0))
      .and(found("K.same") === Count.Finite(1))
      .and(found("K.long") === Count.Finite(0))
      .and(found("K.str") === Count.Finite(0))
      .and(found("K.neg") === Count.Finite(0))
      .and(found("K.dbl") === Count.Finite(1))
      .and(found("K.flt") === Count.Finite(0))
      .and(found("K.hex") === Count.Finite(1))
      .and(found("K.chr") === Count.Finite(0))
  }

  // scalameta reads `-0.0` as `0.0`, and the two are different types: the zero is not read.
  def floatingZero =
    refused(rows("object Z { def zero: 0.0 = 0.0 }")("Z.zero"), "floating zero not read")

  // ctxIn: a, f(using a), f(using f(using a)), … (ω); ctxIn2: f(using a) alone (1).
  def contextInputs = {
    val found = rows("""object I {
      |  def ctxIn[A](f: A ?=> A, a: A): A = f(using a)
      |  def ctxIn2[A, B](f: A ?=> B, a: A): B = f(using a)
      |}""".stripMargin)
    (found("I.ctxIn") === Count.Countable).and(found("I.ctxIn2") === Count.Finite(1))
  }

  // ctxRes: `(y: A) ?=> a` or `(y: A) ?=> y` (2); twoBinders: either binder of
  // `(x: A) ?=> (y: A) ?=> …` (2); unit: one value whatever the evidence (1).
  def contextResults = {
    val found = rows("""object R {
      |  def ctxRes[A](a: A): A ?=> A = a
      |  def twoBinders[A]: A ?=> A ?=> A = summon[A]
      |  def unit[A](using A): A ?=> Unit = ()
      |}""".stripMargin)
    (found("R.ctxRes") === Count.Finite(2))
      .and(found("R.twoBinders") === Count.Finite(2))
      .and(found("R.unit") === Count.Finite(1))
  }

  // Fixture cf: `g(using 1)(using 2)` prints 2, the binder hiding the outer anonymous evidence,
  // so g has 1 implementation, never the 2 a plain function would give (h); the reading refuses.
  // An alias of an alias applied to a context function wraps the body too (fixture p6 prints the
  // binder's 5).
  def contextShadowing = {
    val found = rows("""type L[B] = B
      |type K[A] = L[A]
      |object S {
      |  def g[A](using A): A ?=> A = summon[A]
      |  def h[A](using A): A => A = y => summon[A]
      |  def curried[A](using A): A ?=> A ?=> A = summon[A]
      |  def nested[A](using A): K[A ?=> A] = summon[A]
      |}""".stripMargin)
    refused(found("S.g"), "overlaps context function binder A")
      .and(found("S.h") === Count.Finite(2))
      .and(refused(found("S.curried"), "overlaps context function binder A"))
      .and(refused(found("S.nested"), "overlaps context function binder A"))
  }

  // Fixture cf3: the tuple's first component is wrapped, yet `val o = summon[A]` before it reaches
  // the outer evidence (`hoisted` prints 1): binder or outer value there, the outer one after (2).
  def contextNested =
    rows("object N { def nested[A](using A): (A ?=> A, A) = (summon[A], summon[A]) }")(
      "N.nested"
    ) === Count.Finite(2)

  // Where the reading of an enclosing result stops — a match type that may reduce to a context
  // function, `ContextFunction1` by name, an applied inherited alias, a path, a name not supplied
  // or not declared at all — a local definition's environment is unknown. Beside `plain`'s `Int`
  // result, lp reads b and the captured i: every Boolean of a Boolean and an Int, 2^(2·2^32) =
  // 2^(2^33).
  def contextUnread = {
    val found = rows("""import ext.J
      |type M[T] = T match { case Int => Int ?=> Int }
      |trait P2 { type KA[X] = X ?=> X }
      |trait Holder { type T = Int ?=> Int }
      |object O {
      |  def viaPath(h: Holder): h.T = { def lv(b: Boolean): Boolean = b; ??? }
      |  def viaMatch(x: Int): M[Int] = { def lm(b: Boolean): Boolean = b; ??? }
      |  def byName: ContextFunction1[Int, Int] = { def ln(b: Boolean): Boolean = b; ??? }
      |  def foreign(i: Int): J = { def lf(b: Boolean): Boolean = b; ??? }
      |  def absent(i: Int): ext.Unknown = { def la(b: Boolean): Boolean = b; ??? }
      |  def plain(i: Int): Int = { def lp(b: Boolean): Boolean = b; i }
      |}
      |final class C2 extends P2 { def h2[Y]: KA[Y] = { def lka: Y = summon[Y]; lka } }
      |""".stripMargin)
    refused(found("O.viaMatch.lm"), "context function behind a match type not read: M[Int]")
      .and(refused(found("O.byName.ln"), "context function spelled by name: ContextFunction1"))
      .and(refused(found("O.foreign.lf"), "declaration not supplied: ext.J"))
      .and(refused(found("O.absent.la"), "declaration not supplied: ext.Unknown"))
      .and(found("O.plain.lp") === Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 33))))
      .and(refused(found("C2.h2.lka"), "context function behind an inherited alias not read: KA"))
      .and(refused(found("O.viaPath.lv"), "context function result through a path not read: h.T"))
  }

  // Without an expected type Scala applies the returned context function to a given it finds, so
  // t2's type is A, not `A ?=> A`. An explicit type argument is read as written (fixture a5-fix1
  // r9: viaCtx prints `false`, applied where it is defined): `id[Boolean => Boolean](f)` is that
  // function, every Boolean of f's 4 values and a Boolean (2^8), `id[Boolean ?=> Boolean](g)` is
  // applied.
  def contextCalls = {
    val found = rows("""object T {
      |  given Boolean = true
      |  def ctx[A](a: A): A ?=> A = a
      |  def t2[A](a: A)(using A) = ctx[A](a)
      |  def id[X](x: X): X = x
      |  def viaFn(f: Boolean => Boolean) = id[Boolean => Boolean](f)
      |  def viaCtx(g: Boolean ?=> Boolean) = id[Boolean ?=> Boolean](g)
      |}""".stripMargin)
    val applied = "whose context function result is applied"
    refused(found("T.t2"), applied)
      .and(found("T.viaFn") === Count.Finite(256))
      .and(refused(found("T.viaCtx"), s"call of id, $applied"))
  }

  // Fixture review-A5 inf/i3: `def asc = (k: Boolean ?=> Boolean)` compiles to `boolean asc()` and
  // prints `false` with no given at the use site: Scala applied k where asc is defined, so asc is
  // a Boolean, never the 4 of `Boolean => Boolean`. A plain function ascription is that function.
  def contextAscription = {
    val found = rows("""object A {
      |  given Boolean = true
      |  val k: Boolean ?=> Boolean = !summon[Boolean]
      |  val k2: Boolean => Boolean = b => !b
      |  def asc = (k: Boolean ?=> Boolean)
      |  def asc2 = (k2: Boolean => Boolean)
      |}""".stripMargin)
    refused(found("A.asc"), "ascription, whose context function result is applied")
      .and(found("A.asc2") === Count.Finite(4))
  }

  // Fixture a5-fix1 r3: `f[Int](1, 2)` prints 1, and 7 under a call-site `given Int = 7`: the
  // `summonFrom` of the local g is expanded with f, so g's parametric 4 (a or b, of x or y) is
  // not its count; the same for a method of a class local to the inline method. A one-valued
  // local goal keeps its 1, and a local definition of an ordinary method its count (4).
  def inlineNested = {
    val found = rows("""import scala.compiletime.summonFrom
      |object I {
      |  inline def f[A](x: A, y: A): A = {
      |    def g(a: A, b: A): A = summonFrom { case given_a: A => given_a; case _ => a }
      |    g(x, y)
      |  }
      |  inline def outer[A](x: A, y: A): A = {
      |    class Local { def pick(a: A, b: A): A = summonFrom { case given_a: A => given_a; case _ => a } }
      |    new Local().pick(x, y)
      |  }
      |  inline def unit[A](x: A): Unit = { def u(a: A): Unit = (); u(x) }
      |  def plain[A](x: A, y: A): A = { def h(a: A, b: A): A = a; h(x, y) }
      |}""".stripMargin)
    val inside = "definition inside an inline method"
    refused(found("I.f.g"), inside)
      .and(refused(found("I.outer.Local.pick"), inside))
      .and(found("I.outer.Local.<init>") === Count.Finite(1))
      .and(found("I.unit.u") === Count.Finite(1))
      .and(found("I.plain.h") === Count.Finite(4))
  }

  // Fixture il: `byVal(1)` is 10 and `byVal(a)` 20 with a == 1 under `inline x match`; `pick[Int]`
  // and `pick[String]` choose different arguments under `inline erasedValue[A] match`: refused.
  // The same signatures with bodies free of compile-time operations count as methods (register
  // D27): every Int => Int, 2^(2^37); x or y, 2. A transparent `None` is typed `None.type` where it
  // is called (fixture review-A6 h11): rule X refuses it.
  def inlineMethods = {
    val inline = "inline method: uses inline match"
    val found = rows("""import scala.compiletime.erasedValue
      |object T {
      |  inline def byVal(x: Int): Int = inline x match { case 1 => 10; case _ => 20 }
      |  inline def pick[A](x: A, y: A): A = inline erasedValue[A] match { case _: Int => y; case _ => x }
      |  inline def plainVal(x: Int): Int = x
      |  inline def plainPick[A](x: A, y: A): A = x
      |  transparent inline def closed(b: Boolean): Option[Boolean] = None
      |  def plain(x: Int): Int = x
      |}""".stripMargin)
    refused(found("T.byVal"), inline)
      .and(refused(found("T.pick"), inline))
      .and(found("T.plainVal") === Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37))))
      .and(found("T.plainPick") === Count.Finite(2))
      .and(refused(found("T.closed"), "not the declared Option[Boolean]"))
      .and(found("T.plain") === Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37))))
  }

  // Every expansion of a Unit or Any goal yields its one value.
  def inlineTerminal = {
    val found = rows("""object T {
      |  inline def unit[A](x: A): Unit = ()
      |  inline def top(x: Any): Any = x
      |}""".stripMargin)
    (found("T.unit") === Count.Finite(1)).and(found("T.top") === Count.Finite(1))
  }

  // A transparent result's call site sees the expansion's type: an `Any` field may be narrowed to
  // `Int`, and a one-valued named tuple to the singleton type of a stable path holding one (fixture
  // a6-fix1 t1) — rule X counts only a declared singleton type.
  def inlineNamedTuple = {
    val found = rows("""final case class Fin()
      |object T {
      |  transparent inline def nt(x: Int): (a: Any, b: Unit) = (a = 1, b = ())
      |  transparent inline def nf(x: Int): (a: Fin, b: Unit) = (a = Fin(), b = ())
      |}""".stripMargin)
    val narrowed = "transparent inline result: the call site observes the expansion's type"
    refused(found("T.nt"), narrowed).and(refused(found("T.nf"), narrowed))
  }

  // A ClassTag[A] matches `case a: A`, telling A = Int from A = String: x or y by A, not 2. Every
  // Mirror names its type's cases or fields (`Mirror.Sum.ordinal`), and a Manifest is a ClassTag.
  def revealingEvidence = {
    val found = rows("""import scala.reflect.{ClassTag, TypeTest}
      |import scala.deriving.Mirror
      |object E {
      |  def tagged[A](x: A, y: A)(using ClassTag[A]): A = x
      |  def bound[A: ClassTag](x: A, y: A): A = x
      |  def test[A](x: Any, y: A)(using TypeTest[Any, A]): A = y
      |  def mirror[A](x: A, y: A)(using Mirror.Of[A]): A = x
      |  def sum[A](x: A, y: A)(using Mirror.Sum): A = x
      |  def product[A](x: A, y: A)(using Mirror.Product): A = x
      |  def manifest[A](x: A, y: A)(using scala.reflect.Manifest[A]): A = x
      |  def unit[A: ClassTag](x: A): Unit = ()
      |}""".stripMargin)
    val revealing = "type-revealing evidence"
    refused(found("E.tagged"), s"scala.reflect.ClassTag ($revealing")
      .and(refused(found("E.bound"), s"scala.reflect.ClassTag ($revealing"))
      .and(refused(found("E.test"), s"scala.reflect.TypeTest ($revealing"))
      .and(refused(found("E.mirror"), s"scala.deriving.Mirror.Of ($revealing"))
      .and(refused(found("E.sum"), s"scala.deriving.Mirror.Sum ($revealing"))
      .and(refused(found("E.product"), s"scala.deriving.Mirror.Product ($revealing"))
      .and(refused(found("E.manifest"), s"scala.reflect.Manifest ($revealing"))
      .and(found("E.unit") === Count.Finite(1))
  }

  def quoted =
    refused(
      rows("""import scala.quoted.*
        |object Mac { def impl[A: Type](x: Expr[A])(using Quotes): Expr[A] = x }""".stripMargin)(
        "Mac.impl"
      ),
      "compile-time reflection"
    )

}
