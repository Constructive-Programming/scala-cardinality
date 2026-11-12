package cardinality.compiled

import cardinality.tasty.CompiledTypes
import java.nio.file.{Files, Path}
import org.specs2.Specification
import org.specs2.execute.Result
import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*

/** Which inline methods count as methods, read from compiled classes (`Inlines`; AGENTS.md, "Inline
  * methods count as methods"; the plan's decisions D2 and D4). Each fixture is compiled by scalac,
  * and each verdict is derived in the comment beside it from what an expansion of the method may
  * decide at its call site.
  */
class InlinesSpec extends Specification {

  def is = s2"""
    An inline method counts as a method
      when its body has no compile-time operation                      $plain
      and what it throws is not read                                   $thrown
    A compile-time operation refuses it
      inline if, inline match, scala.compiletime, a quote or a splice  $operations
      in a nested definition, a lambda, an argument, a default         $wherever
      and each definition written inside it                            $inside
      in an inline method it calls, however far                        $calls
      in a cycle of inline calls, decided as one                       $cycles
    A transparent result (rule X)
      is refused unless its declared type is a singleton               $transparent
      an export's forwarder is the member it forwards to               $forwarders
    What the compiler resolved is read as a call (D2, D4)
      a conversion or extension refuses only where the body applies it $conversions
      a given refuses only where it is inline with such an operation   $givens
    A call site may type a receiver as a subclass
      an inline override is expanded on a parameter or on `this`       $overrides
      and so is one a dependency declares                              $dependencyOverrides
    A runtime class that depends on a type parameter
      a type test, an array's class, `classOf`                         $reified
    What the classes read hold
      a dependency's inline body is read like the module's own         $dependencies
      the library's inline methods are read from its classes           $library
      a callee the classes read do not hold refuses                    $missing
    The module
      imports nothing of the scalameta reading                         $imports
  """

  private val libraries: List[Path] = sys.props
    .getOrElse("cardinality.test.classpath", sys.props("java.class.path"))
    .split(java.io.File.pathSeparator)
    .toList
    .map(Path.of(_))
    .filter { entry =>
      val name = entry.getFileName.toString
      name.startsWith("scala-library") || name.startsWith("scala3-library")
    }

  // One source compiled by scalac against `classpath` and the Scala library: its class directory.
  private def compiled(code: String, classpath: List[Path] = Nil): Path = {
    val root = Files.createTempDirectory("inlines")
    sys.addShutdownHook(cardinality.Compiled.deleted(root))
    val file = Files.writeString(root.resolve("Example.scala"), code)
    val classes = Files.createDirectories(root.resolve("classes"))
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    val arguments = List(
      "-d",
      classes.toString,
      "-classpath",
      (classpath ++ libraries).mkString(java.io.File.pathSeparator),
      file.toString
    )
    if (dotty.tools.dotc.Main.process(arguments.toArray, reporter).hasErrors)
      sys.error(s"fixture does not compile: ${reporter.allErrors.map(_.message).mkString("; ")}")
    classes
  }

  // Each method of a compiled module by its name, with why it is not counted as a method, if it is
  // not. `read` are the dependencies the reading is given: the ones compiled against, by default.
  private def verdicts(
      code: String,
      classpath: List[Path] = Nil,
      read: Option[List[Path]] = None
  ): Map[String, Option[String]] = {
    val classes = CompiledTypes.Classes(
      List(compiled(code, classpath)),
      read.getOrElse(classpath) ++ libraries
    )
    val (context, own, _) = CompiledTypes.loaded(classes)
    given Context = context
    val inlines = new Inlines(own.toSet, CompiledTypes.dependencies(classes)._1)
    val found = List.newBuilder[(String, Option[String])]
    own.distinct.flatMap(_.tree).foreach { top =>
      new TreeTraverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case d: DefDef
                if d.symbol.kind == TermSymbolKind.Def && !d.name.toString.contains("$") =>
              found += d.name.toString -> inlines.refusal(d.symbol)
            case _ => ()
          }
          super.traverse(tree)
        }
      }.traverse(top)
    }
    // A name two methods of a fixture share names neither.
    found.result().groupMap(_._1)(_._2).collect { case (name, List(only)) => name -> only }
  }

  private def counts(found: Map[String, Option[String]])(names: String*): Result =
    names
      .map(name => (found(name) must beNone).setMessage(s"$name: ${found(name)}"))
      .reduce(_ and _)

  private def refuses(found: Map[String, Option[String]], reason: String)(names: String*): Result =
    names.map(name => found(name) must beSome(contain(reason))).reduce(_ and _)

  // The compile-time operations the fixtures share: `era` reduces on its type argument.
  private val prelude =
    """import scala.compiletime.*
      |object Ops {
      |  inline def era[A](x: A): Int = inline erasedValue[A] match { case _: Int => 1; case _ => 2 }
      |}
      |import Ops.era
      |""".stripMargin

  // Every expansion of each is the same function of its values: a parameter returned, an `inline`
  // parameter (its argument, by name, which no effect tells apart), a plain `if`, a plain match.
  def plain =
    counts(verdicts("""object M {
      |  inline def pick[A](x: A, y: A): A = x
      |  inline def twice(inline x: Int): Int = x + x
      |  inline def branch(x: Int): Int = if x == 1 then 10 else 20
      |  inline def matched(x: Option[Int]): Int = x match { case Some(n) => n; case None => 0 }
      |}""".stripMargin))("pick", "twice", "branch", "matched")

  // `codeOf(x)` differs at each call site, but only in what is thrown, which is no value of the
  // method (AGENTS.md, "Ignore throwing"): the operand of a `throw`, the message of `sys.error`.
  def thrown =
    counts(verdicts("""import scala.compiletime.codeOf
      |object M {
      |  inline def checked(x: Int): Int =
      |    if x >= 0 then x else throw new IllegalArgumentException(codeOf(x))
      |  inline def failing(x: Int): Int = if x >= 0 then x else sys.error(codeOf(x))
      |}""".stripMargin))("checked", "failing")

  // Each is decided where the method is expanded: by a constant, by the static type of an
  // argument, by the givens or the code at the call site, by a macro.
  def operations = {
    val found = verdicts("""import scala.compiletime.*
      |import scala.quoted.*
      |trait Show[A] { def show(a: A): Boolean }
      |object M {
      |  inline def onConstant(inline b: Boolean): Int = inline if b then 1 else 2
      |  inline def onType[A](x: A): Int = inline erasedValue[A] match { case _: Int => 1; case _ => 2 }
      |  inline def summoned[A](x: A): Boolean = summonInline[Show[A]].show(x)
      |  inline def spelled(x: Int): String = codeOf(x)
      |  inline def stopped(x: Int): Int = error("no")
      |  def impl(using Quotes): Expr[Int] = '{ 1 }
      |  inline def spliced: Int = ${ impl }
      |  inline def sized[N <: Int](x: Int): Int = constValue[N]
      |  inline def typed[N <: Int](x: List[ops.int.S[N]] | Int): Int = 0
      |}""".stripMargin)
    refuses(found, "inline method: uses inline if")("onConstant")
      .and(refuses(found, "inline method: uses inline match")("onType"))
      .and(refuses(found, "summonInline")("summoned"))
      .and(refuses(found, "codeOf")("spelled"))
      .and(refuses(found, "uses scala.compiletime.error")("stopped"))
      .and(refuses(found, "a quote or a splice")("spliced"))
      .and(refuses(found, "uses scala.compiletime.constValue")("sized"))
      .and(refuses(found, "uses scala.compiletime.ops.int.S")("typed"))
      .and(counts(found)("impl"))
  }

  // The operation is in the method's definition wherever it is written: an expansion copies a
  // local method and a lambda, evaluates an argument (by name or not, of a transparent call or
  // not), and a default argument is written in the definition (the compiler keeps it as a method
  // beside this one). A caller of `dflt` brings its definition along.
  def wherever =
    refuses(
      verdicts(prelude + """object M {
      |  inline def nested(b: Boolean): Int = { def in(x: Boolean): Int = era(x); in(b) }
      |  inline def lambda(b: Boolean): Boolean = List(b).map(x => era(x) == 1).head
      |  def later(x: => Int): Int = 1
      |  inline def byName(b: Boolean): Int = later(era(b))
      |  transparent inline def seen(b: Boolean): Any = if b then 1 else "s"
      |  inline def argument(b: Boolean): Any = seen(era(b) == 1)
      |  inline def dflt(b: Boolean, c: Int = era(1)): Boolean = b
      |  inline def defaulted(b: Boolean): Boolean = dflt(b)
      |}""".stripMargin),
      "calls inline method Ops.era, which uses inline match"
    )("nested", "lambda", "byName", "argument", "dflt", "defaulted")

  // Every expansion copies a definition written inside the method, so it is the method's verdict
  // (Scala allows no inline method there); inside a method that counts it is a method like any other.
  def inside = {
    val found = verdicts(prelude + """object M {
      |  inline def refused(b: Boolean): Int = { def copied(x: Boolean): Int = era(x); copied(b) }
      |  inline def counted(b: Boolean): Boolean = { def kept(x: Boolean): Boolean = x; kept(b) }
      |}""".stripMargin)
    refuses(found, "definition inside an inline method: refused calls inline method Ops.era")(
      "copied"
    ).and(counts(found)("counted", "kept"))
  }

  // An inline call brings the callee's body to the call site too: `far` expands `near`, which
  // expands `era`. A call of a method that is not inline is a call, whatever its body does.
  def calls = {
    val found = verdicts(prelude + """object M {
      |  inline def near(b: Boolean): Int = era(b)
      |  inline def far(b: Boolean): Int = near(b)
      |  def method(b: Boolean): Int = era(b)
      |  inline def viaMethod(b: Boolean): Int = method(b)
      |}""".stripMargin)
    refuses(found, "calls inline method Ops.era, which uses inline match")("near", "far")
      .and(counts(found)("method", "viaMethod"))
  }

  // ping and pong expand each other, and pong expands `era`: neither counts. loopA and loopB
  // expand each other and nothing else: both count, as one.
  def cycles = {
    val found = verdicts(prelude + """object M {
      |  inline def ping(b: Boolean): Int = pong(b)
      |  inline def pong(b: Boolean): Int = if b then ping(false) else era(b)
      |  inline def loopA(b: Boolean): Boolean = loopB(b)
      |  inline def loopB(b: Boolean): Boolean = if b then loopA(false) else b
      |}""".stripMargin)
    refuses(found, "calls inline method Ops.era")("ping", "pong").and(
      counts(found)("loopA", "loopB")
    )
  }

  // The call site of a transparent method types the expansion by what its body returns: `wide`
  // may be an Int or a String to an overload. A declared singleton type leaves nothing narrower.
  // A method that calls `wide` has its own declared result: it counts.
  def transparent = {
    val found = verdicts("""object M {
      |  transparent inline def wide(b: Boolean): Any = if b then 1 else "s"
      |  transparent inline def one: 1 = 1
      |  inline def caller(b: Boolean): Any = wide(b)
      |}""".stripMargin)
    refuses(found, "transparent inline result")("wide").and(counts(found)("one", "caller"))
  }

  // An export makes a forwarder, an inline method the compiler marks transparent: it expands to
  // the member it forwards to, and is that member's verdict, rule X included.
  def forwarders = {
    val found = verdicts(prelude + """object Hidden {
      |  inline def clean(x: Int): Int = x
      |  inline def reduced(x: Int): Int = era(x)
      |  transparent inline def wide(b: Boolean): Any = if b then 1 else "s"
      |}
      |object Pub { export Hidden.{clean as cleanForward, reduced as reducedForward, wide as wideForward} }
      |""".stripMargin)
    counts(found)("clean", "cleanForward")
      .and(refuses(found, "Ops.era")("reduced", "reducedForward"))
      .and(refuses(found, "transparent inline result")("wide", "wideForward"))
  }

  // D2. `conv` is in scope of every method here, and its expansion is decided at the call site.
  // `unused` adds two Ints, which no conversion answers: the compiler applied none, so it counts.
  // `used` needs a String from an Int: the compiler wrote `conv.apply(x)`, an inline call that
  // brings `codeOf` along. The same for the extension `shown`: applied in `extended`, not in
  // `member`, which selects a member of its own receiver.
  def conversions = {
    val found = verdicts("""import scala.compiletime.codeOf
      |import scala.language.implicitConversions
      |final class Box(val n: Int) { def shown: String = "box" }
      |object M {
      |  inline given conv: Conversion[Int, String] = (i: Int) => codeOf(i)
      |  extension (i: Int) inline def shown: String = codeOf(i)
      |  inline def unused(x: Int): Int = x + 1
      |  inline def used(x: Int): String = { val s: String = x; s }
      |  inline def extended(x: Int): String = x.shown
      |  inline def member(b: Box): String = b.shown
      |}""".stripMargin)
    counts(found)("unused", "member")
      .and(
        refuses(found, "calls inline method M.conv, which uses scala.compiletime.codeOf")("used")
      )
      .and(refuses(found, "calls inline method M.shown, which uses")("extended"))
  }

  // D4. The given the compiler resolved is an argument of the tree. `ord` is an ordinary value:
  // `viaSummon`, `viaUsing` and `viaContext` pass it, and count. `fresh` is an inline given whose
  // expansion reads the call site's code: `viaInline` passes its expansion, and does not count.
  def givens = {
    val found = verdicts("""import scala.compiletime.codeOf
      |trait Show[A] { def show(a: A): String }
      |object M {
      |  given ord: Show[Int] = new Show[Int] { def show(a: Int): String = "n" }
      |  inline given fresh: Show[Boolean] = { val code = codeOf(1); new Show[Boolean] { def show(a: Boolean): String = code } }
      |  def shown[A](a: A)(using s: Show[A]): String = s.show(a)
      |  inline def viaSummon(x: Int): String = summon[Show[Int]].show(x)
      |  inline def viaUsing(x: Int): String = shown(x)
      |  inline def viaContext(x: Int): String = { val f: Show[Int] ?=> String = shown(x); f }
      |  inline def viaInline(b: Boolean): String = shown(b)
      |}""".stripMargin)
    counts(found)("viaSummon", "viaUsing", "viaContext").and(
      refuses(found, "calls inline method M.fresh, which uses scala.compiletime.codeOf")(
        "viaInline"
      )
    )
  }

  // An expansion selects a member on its receiver's class at the call site: where that is `Sub`,
  // its inline `f` is expanded, and `f` spells the call site's code. So `onParam`, `onThis` (a
  // member named alone is selected on `this`), `onLambda` (an expansion binds the lambda's
  // parameter to its argument), `onHeld` (an override of `held` may return a `Sub`) and `onFunction` (`Wrap` overrides `apply` inline) do not count.
  // A receiver whose type names the method's type parameter (`onTyped`) or is a parameter's
  // singleton (`onSingleton`) or a member of its path (`onPath`) is typed by the expansion too.
  // An ascription and a local value fix the receiver's type, `Fixed` is final and `Other.f` has no
  // inline override: each counts.
  def overrides = {
    val found = verdicts("""import scala.compiletime.codeOf
      |class Base { def f(x: Int): Int = x; inline def onThis(x: Int): Int = f(x) }
      |final class Sub extends Base { override inline def f(x: Int): Int = codeOf(x).length }
      |final class Fixed { def f(x: Int): Int = x }
      |class Holder { def held: Base = new Base }
      |class Other { def f(x: Int): Int = x }
      |abstract class Path { type T <: Base; def get: T }
      |final class Wrap extends (Int => Int) { inline def apply(x: Int): Int = codeOf(x).length }
      |object M {
      |  inline def onParam(b: Base, x: Int): Int = b.f(x)
      |  inline def onLambda(b: Base, x: Int): Int = ((c: Base) => c.f(x))(b)
      |  inline def onHeld(h: Holder, x: Int): Int = h.held.f(x)
      |  inline def onFunction(g: Int => Int, x: Int): Int = g(x)
      |  def head[A](xs: List[A]): A = xs.head
      |  inline def onTyped[A <: Base](xs: List[A], x: Int): Int = head[A](xs).f(x)
      |  inline def onSingleton(b: Base, x: Int): Int = { val y: b.type = b; y.f(x) }
      |  inline def onPath(h: Path, x: Int): Int = { val y: h.T = h.get; y.f(x) }
      |  inline def ascribed(b: Base, x: Int): Int = (b: Base).f(x)
      |  inline def local(b: Base, x: Int): Int = { val y = b; y.f(x) }
      |  inline def onFinal(b: Fixed, x: Int): Int = b.f(x)
      |  inline def onOther(o: Other, x: Int): Int = o.f(x)
      |}""".stripMargin)
    refuses(found, "calls inline method Sub.f, which uses")(
      "onParam",
      "onThis",
      "onLambda",
      "onHeld",
      "onTyped",
      "onSingleton",
      "onPath"
    )
      .and(refuses(found, "calls inline method Wrap.apply, which uses")("onFunction"))
      .and(counts(found)("ascribed", "local", "onFinal", "onOther"))
  }

  // The dependencies are part of the program: `dep.Sub` overrides `f` inline there, and a call
  // site may pass one. Nothing overrides `Plain.g`.
  def dependencyOverrides = {
    val dependency = compiled("""package dep
      |import scala.compiletime.codeOf
      |class Base { def f(x: Int): Int = x }
      |final class Sub extends Base { override inline def f(x: Int): Int = codeOf(x).length }
      |class Plain { def g(x: Int): Int = x }
      |""".stripMargin)
    val found = verdicts(
      """object M {
      |  inline def onParam(b: dep.Base, x: Int): Int = b.f(x)
      |  inline def onPlain(b: dep.Plain, x: Int): Int = b.g(x)
      |}""".stripMargin,
      List(dependency)
    )
    refuses(found, "calls inline method dep.Sub.f, which uses")("onParam")
      .and(counts(found)("onPlain"))
  }

  // Scala substitutes an expansion's type arguments into the body, so a class that depends on a
  // type parameter tells instantiations apart: `A` itself, an array of it (an array's class is
  // its element's), a class's own parameter through a member. A class has one runtime class
  // whatever its arguments: `String`, `Array[String]`, `List[A]`.
  def reified = {
    val found = verdicts("""object M {
      |  inline def isA[A](x: Any): Boolean = x.isInstanceOf[A]
      |  inline def caseA[A](x: Any): Int = x match { case _: A => 1; case _ => 2 }
      |  inline def isArray[A](x: Any): Boolean = x.isInstanceOf[Array[A]]
      |  inline def classA[A]: Class[?] = classOf[Array[A]]
      |  inline def isString(x: Any): Boolean = x.isInstanceOf[String]
      |  inline def isStrings(x: Any): Boolean = x.isInstanceOf[Array[String]]
      |  inline def isList[A](x: Any): Boolean = x.isInstanceOf[List[A]]
      |  inline def caseInt(x: Any): Int = x match { case _: Int => 1; case _ => 2 }
      |}
      |class C[A] { type TA = Array[A]; inline def viaMember(x: Any): Boolean = x.isInstanceOf[TA] }
      |class D { inline def unqualified[A]: Boolean = isInstanceOf[A] }
      |""".stripMargin)
    refuses(found, "tests the type")("isA", "caseA", "isArray", "viaMember", "unqualified")
      .and(refuses(found, "reifies the class of")("classA"))
      .and(counts(found)("isString", "isStrings", "isList", "caseInt"))
  }

  // The dependency is compiled apart and is no class of the module: its inline bodies are in its
  // classes, and are read. `clean` returns its argument; `reduced` reduces on its type argument.
  def dependencies = {
    val dependency = compiled("""package dep
      |import scala.compiletime.*
      |object Lib {
      |  inline def clean[A](x: A): A = x
      |  inline def reduced[A](x: A): Int = inline erasedValue[A] match { case _: Int => 1; case _ => 2 }
      |}""".stripMargin)
    val found = verdicts(
      """object M {
      |  inline def viaClean(x: Int): Int = dep.Lib.clean(x)
      |  inline def viaReduced(x: Int): Int = dep.Lib.reduced(x)
      |}""".stripMargin,
      List(dependency)
    )
    counts(found)("viaClean").and(
      refuses(found, "calls inline method dep.Lib.reduced, which uses inline match")("viaReduced")
    )
  }

  // `summon` returns the given it is passed and `assert` throws or returns: their bodies are in
  // the Scala library's classes and hold no compile-time operation. `summonFrom` is one, which the
  // compiler writes as an inline match without a selector. A Java method is no inline method.
  def library = {
    val found = verdicts("""object M {
      |  inline def summoned(x: Int): Ordering[Int] = summon[Ordering[Int]]
      |  inline def asserted(x: Int): Int = { assert(x > 0); x }
      |  inline def maximum(x: Int): Int = java.lang.Math.max(x, 0)
      |  inline def searched(x: Int): Int = scala.compiletime.summonFrom { case n: Int => n; case _ => x }
      |}""".stripMargin)
    counts(found)("summoned", "asserted", "maximum").and(refuses(found, "inline match")("searched"))
  }

  // The module was compiled against `dep.Lib`, and is read without it: nothing says whether
  // `gone` is inline, so it is not taken for a method that is not.
  def missing = {
    val dependency = compiled("package dep\nobject Lib { def gone(x: Int): Int = x }")
    val found = verdicts(
      "object M { inline def viaGone(x: Int): Int = dep.Lib.gone(x) }",
      List(dependency),
      read = Some(Nil)
    )
    refuses(found, "which the classes read do not hold")("viaGone")
  }

  // The module reads symbols and typed trees alone: of this project it imports nothing, and it
  // names no scalameta tree and no row of the scalameta front end.
  def imports = {
    val lines = Files
      .readString(Path.of("src/main/scala/cardinality/compiled/Inlines.scala"))
      .linesIterator
      .toList
    val imported = lines.map(_.trim).filter(_.startsWith("import ")).map(_.stripPrefix("import "))
    (imported.filterNot(i => i.startsWith("tastyquery.") || i == "scala.collection.mutable") must
      beEmpty)
      .and(lines.filter(_.contains("scala.meta")) must beEmpty)
      .and(
        lines.filter(_.contains("cardinality.")) === List("package cardinality.compiled")
      )
  }

}
