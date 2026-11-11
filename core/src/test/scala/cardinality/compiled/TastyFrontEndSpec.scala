package cardinality.compiled

import cardinality.analysis.MethodAnalysis.Entry
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.{BooleanSize, IntSize, TinySize, UnitSize}
import cardinality.reporting.{FrontEnd, Report, ScalametaFrontEnd}
import cardinality.tasty.CompiledTypes
import java.nio.file.{Files, Path}
import org.specs2.Specification

import Differential.Verdict

/** The compiled front end's spike and the differential gate that judges it (phase 4 of
  * docs/plans/compiled-own-types.md). Every expected count is derived in its example's comment.
  */
class TastyFrontEndSpec extends Specification {

  def is = s2"""
  The compiled front end reads rows from symbols and typed trees
    an object's method counts over its own parameters            $methods
    a definition over base types multiplies and adds them         $definitions
    a member of an object that is never built has one body        $unbuilt
    a row no slice reads yet is uncovered, never a count          $uncovered
    what the counting model refuses is a row of its own           $refused
    classes compiled from another text refuse the source          $stale
    it imports and names nothing of the scalameta reading         $imports
    a source without definitions is empty, a bound name is a row  $sources

  The differential gate
    agrees with the scalameta front end on every row measured     $agrees
    finds every row under the scalameta front end's name and line $named
    classifies identical, better, worse and uncovered rows        $verdicts
    leaves a row only one side has unpaired                       $unpaired
    pairs a row by position where its scope is named otherwise    $position
    does not pair a lost row with an extra one of another owner   $owners
    fails a better row past a model limit until it is derived     $limits
    fails a row measured before, uncovered or counted otherwise   $memory
  """

  private val text =
    """package demo
      |
      |object O {
      |  def pick[A](x: A, y: A): A = x
      |  def both(b: Boolean): Boolean = b
      |  val n: Int = 1
      |}
      |object K { val z: Boolean = ???; def g(b: Boolean): Boolean = !b }
      |object KE { val z: Boolean = sys.error("no"); def g(b: Boolean): Boolean = !b }
      |object KT { val z: Boolean = throw new RuntimeException; def g(b: Boolean): Boolean = !b }
      |object KL { lazy val z: Boolean = ???; def g(b: Boolean): Boolean = !b }
      |final case class P(a: Boolean, b: Boolean)
      |final class W(x: Int, y: Boolean)
      |enum Suit { case Hearts, Spades, Clubs }
      |class C(x: Int) { def m(b: Boolean): Boolean = b }
      |object Refused { def arr[A](xs: Array[A]): Int = 0 }
      |object Pattern { val (a, b) = (true, false); def g(x: Boolean): Boolean = x }
      |object Nameless { val _ = ???; def g(x: Boolean): Boolean = x }
      |""".stripMargin

  // What the compiler desugars or adds, each with the name the scalameta front end gives its row:
  // scopes of a body, nameless definitions, constructors, and members nobody wrote.
  private val shapes =
    """package shapes
      |
      |trait Show[A] { def show(a: A): Boolean }
      |trait Plain
      |trait Params(val n: Int)
      |final case class Pair(left: Boolean, right: Boolean = false)
      |enum Tree { case Leaf(b: Boolean); case Node(l: Tree, r: Tree) }
      |type Alias[X] = List[X]
      |type Lambda = [X] =>> List[X]
      |opaque type Flag = Boolean
      |trait Bound[A <: (Int, Int)]
      |@main def start(b: Boolean): Unit = ()
      |object Holder { class start }
      |object Context {
      |  def implied: Int ?=> Boolean = { def inner(x: Boolean): Boolean = x; inner(true) }
      |  def run(b: Int ?=> Boolean): Boolean = true
      |  def caller: Boolean = run { def passed(x: Boolean): Boolean = x; passed(true) }
      |  def literal: Int ?=> Boolean = (c: Int) ?=> { def lit(x: Boolean): Boolean = x; lit(true) }
      |}
      |class Outer(x: Int) {
      |  def this() = this(0)
      |  class Inner { def m(b: Boolean): Boolean = b }
      |  var state: Boolean = false
      |}
      |object Members {
      |  given named: Show[Int] with { def show(a: Int): Boolean = true }
      |  given Show[Boolean] with { def show(a: Boolean): Boolean = a }
      |  given listed[A](using a: Show[A]): Show[List[A]] with { def show(a: List[A]): Boolean = true }
      |  extension (b: Boolean) def flip: Boolean = !b
      |  def defaults(b: Boolean = true): Boolean = b
      |  def scopes(b: Boolean): Boolean = {
      |    def local(x: Boolean): Boolean = x
      |    val made = new Plain { def here(x: Boolean): Boolean = x }
      |    val f = (x: Boolean) => { def inner(y: Boolean): Boolean = y; inner(x) }
      |    if (b) { def nested(x: Boolean): Boolean = x; nested(b) }
      |    else b match { case true => def arm(x: Boolean): Boolean = x; arm(b); case false => f(b) }
      |  }
      |}
      |""".stripMargin

  private lazy val input =
    TastyFrontEndSpec.compiled("Demo.scala" -> text, "Shapes.scala" -> shapes)

  private lazy val rows = Differential.answer(TastyFrontEnd, input.next)
  private lazy val source = rows.sources.collect { case Right(source) => source }.head

  private def count(name: String): Count = source.methods.find(_.name == name).get.count

  // `pick` returns x or y: 2. `both` is a function Boolean => Boolean: 2^2. `n` is any Int: 2^32.
  private def methods =
    (count("demo.O.pick") must_== Count.Finite(2))
      .and(count("demo.O.both") must_== Count.Finite(4))
      .and(count("demo.O.n") must_== Count.Finite(BigInt(2).pow(32)))

  // An object is one value; `P` holds 2 × 2; `W` 2^32 × 2, a capacity; `Suit` 1 + 1 + 1.
  private def definitions = {
    val sizes = source.definitions.map(d => d.name -> d.size).toMap
    (sizes("demo.O") must_== Some(UnitSize))
      .and(sizes("demo.P") must_== Some(TinySize(4)))
      .and(sizes("demo.W") must_== Some(IntSize * BooleanSize))
      .and(sizes("demo.Suit") must_== Some(TinySize(3)))
  }

  // `K`'s constructor evaluates `z`'s initializer, which throws (`???`, `sys.error`, `throw`): no
  // `K` is built, so no call of `g` returns and every body of it is the same one (register D26).
  // `z` itself is the choice being counted: one of 2 Booleans. A lazy `z` is not evaluated by the
  // constructor: `KL` is built, and its `g` is one of the 2^2 functions.
  private def unbuilt =
    (List("K", "KE", "KT").map(k => count(s"demo.$k.g")) must_== List.fill(3)(Count.Finite(1)))
      .and(count("demo.K.z") must_== Count.Finite(2))
      .and(count("demo.KL.g") must_== Count.Finite(4))

  // A member of a class, a constructor, and a member beside a pattern definition, named or not
  // (which values its object's constructor evaluates is not read yet).
  private def uncovered = {
    val rows = List("demo.C.m", "demo.C.<init>", "demo.Pattern.g", "demo.Nameless.g")
    rows.map(count) must contain(beLike[Count] {
      case Count.Unresolved(List(reason)) => reason must startWith(TastyFrontEnd.Uncovered)
    }).forall
  }

  // An array of a type parameter is refused by the standard model for good (building one needs a
  // `ClassTag`): a `?` of the front end's own, which the gate compares, not a row to come.
  private def refused = count("demo.Refused.arr") must beLike {
    case Count.Unresolved(List(reason)) =>
      (reason must contain("Array")).and(reason must not(startWith(TastyFrontEnd.Uncovered)))
  }

  // Every definition moved by one character: no name is where the classes say it is.
  private def stale = {
    val moved = input.next.copy(sources = List("Demo.scala" -> (" " + text)))
    Differential.answer(TastyFrontEnd, moved).sources must beLike {
      case List(Left(error)) => error.message must startWith("compiled classes do not match")
    }
  }

  // What the front end may use of the rest: the row types, the solver and the shape half of
  // `TopTypes` it calls, the size algebra, the standard model's tables, the class loader and a
  // list helper. Anything else would be the scalameta reading.
  private val allowed = Set(
    "cardinality.analysis.BuiltinTypes",
    "cardinality.analysis.MethodAnalysis.Entry",
    "cardinality.analysis.inhabitation.Inhabitation",
    "cardinality.analysis.inhabitation.Inhabitation.Count",
    "cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}",
    "cardinality.analysis.resolution.TopTypes",
    "cardinality.capacity.Definition",
    "cardinality.capacity.Definition.Kind",
    "cardinality.capacity.{BaseTypes, NothingSize, Size, UnitSize}",
    "cardinality.reporting.{FrontEnd, Report}",
    "cardinality.tasty.CompiledTypes",
    "cardinality.types.Traversals"
  )

  // What the allowed imports would still let a row be handed to: the report's own entry points,
  // the scalameta front end, a scalameta tree, the source rewriting.
  private val delegating =
    """Report\.(of|query)|ScalametaFrontEnd|\bmeta\.|CompiledTypes\.ascribe""".r

  private def imports = {
    val files = Files.list(Path.of("src/main/scala/cardinality/compiled"))
    val lines =
      try
        files.toArray.toList.flatMap(file =>
          Files.readString(file.asInstanceOf[Path]).linesIterator
        )
      finally files.close()
    val imported = lines
      .map(_.trim)
      .filter(_.startsWith("import "))
      .map(_.stripPrefix("import ").stripPrefix("_root_."))
    (imported.filter(_.startsWith("cardinality.")).toSet must_== allowed)
      .and(lines.filter(_.contains("scala.meta")) must_== Nil)
      .and(lines.filter(delegating.findFirstIn(_).nonEmpty) must_== Nil)
      .and(
        lines.filter(_.contains("cardinality.")).filterNot(_.trim.startsWith("import ")) must_==
          List("package cardinality.compiled").flatMap(List.fill(4)(_))
      )
  }

  private lazy val compared =
    Differential.run(input, Differential.answer(ScalametaFrontEnd, input.old))

  private def agrees = {
    val verdicts = compared.compared.map(_.verdict).toSet
    ((compared.failed, compared.noted) must_== (Nil, Nil))
      .and(Differential.failures(compared.compared) must_== Nil)
      .and(verdicts must contain(Verdict.Identical))
  }

  // Every row of both sources pairs under one name, kind and line: nothing in `shapes` is paired
  // by position or under another kind.
  private def named =
    compared.compared.filter(_.by != Differential.Strict).map(_.row.name) must_== Nil

  private def entry(name: String, count: Count, line: Int = 1): Entry =
    Entry("A.scala", name, name, line, "method", count, Nil)

  private def answer(entries: Entry*): FrontEnd.Rows =
    FrontEnd.Rows(List(Right(Report.Source("A.scala", Nil, entries.toList))), Nil)

  private val unknown = Count.Unresolved(List("unresolved type: T"))
  private val declined = Count.Unresolved(List(s"${TastyFrontEnd.Uncovered}: a class"))

  private def verdicts = {
    val old = answer(
      entry("same", Count.Finite(2)),
      entry("gained", unknown),
      entry("lost", Count.Finite(2)),
      entry("moved", Count.Finite(2)),
      entry("declined", Count.Finite(2)),
      entry("unknown", unknown)
    )
    val next = answer(
      entry("same", Count.Finite(2)),
      entry("gained", Count.Finite(2)),
      entry("lost", Count.Unresolved(List("a reason of its own"))),
      entry("moved", Count.Finite(3)),
      entry("declined", declined),
      entry("unknown", Count.Unresolved(List("another reason")))
    )
    Differential.compare(old, next).map(c => c.row.name -> c.verdict).toMap must_== Map(
      "same" -> Verdict.Identical,
      "gained" -> Verdict.Better,
      "lost" -> Verdict.Worse,
      "moved" -> Verdict.Worse,
      "declined" -> Verdict.Uncovered,
      "unknown" -> Verdict.Identical
    )
  }

  private def unpaired =
    Differential
      .compare(answer(entry("gone", Count.Finite(2))), answer(entry("made", Count.Finite(2), 7)))
      .map(c => c.row.name -> c.verdict) must contain(
      exactly("gone" -> Verdict.OnlyOld, "made" -> Verdict.OnlyNew)
    )

  private def position =
    Differential.compare(
      answer(entry("f.<for#1>.g", Count.Finite(2), 3)),
      answer(entry("f.<lambda#1>.<case#1>.g", Count.Finite(2), 3))
    ) must beLike {
      case List(row) => (row.verdict, row.by) must_== (Verdict.Identical, "position")
    }

  // `trait T(); trait U[A: Ordering]` on one line: a constructor row lost for `T` and one made for
  // `U` are two failures, not one pair.
  private def owners =
    Differential
      .compare(answer(entry("T.<init>", unknown)), answer(entry("U.<init>", declined)))
      .map(c => c.row.name -> c.verdict) must contain(
      exactly("T.<init>" -> Verdict.OnlyOld, "U.<init>" -> Verdict.OnlyNew)
    )

  // The old front end refused `named` for a name it could not resolve, which the compiler knows:
  // a number is expected. It refused `modelled` because the counting model does not read a
  // recursive type, and `transparent` by rule X: a number there counts what the model refuses,
  // until someone derives it, with that number.
  private def limits = {
    val recursive = Count.Unresolved(List("recursive type not read: T"))
    val ruleX =
      Count.Unresolved(List("inferred result type not resolved: transparent inline method"))
    val compared = Differential.compare(
      answer(
        entry("named", unknown),
        entry("modelled", recursive, 2),
        entry("transparent", ruleX, 3)
      ),
      answer(
        entry("named", Count.Finite(2)),
        entry("modelled", Count.Finite(2), 2),
        entry("transparent", Count.Finite(2), 3)
      )
    )
    def derived(count: String) =
      compared.filter(_.row.name != "named").map(_.row.key -> count).toMap
    (Differential.failures(compared).map(_.row.name) must_== List("modelled", "transparent"))
      .and(Differential.failures(compared, derived("2")) must_== Nil)
      .and(Differential.failures(compared, derived("3")).size must_== 2)
      .and(Differential.samples(compared).map(_._1) must_== List("unresolved type"))
  }

  // `lost` was measured and is uncovered now, `refused` was the front end's own `?` and is
  // uncovered now, `moved` is counted otherwise; `fresh` is uncovered and was never measured;
  // `kept` is as before. `twin` is two rows of one key, one of them uncovered: the key cannot tell
  // them apart, so it is not remembered. What is written is read back as it was.
  private def memory = {
    val refusal = Count.Unresolved(List("recursive type not read: T"))
    val compared = Differential.compare(
      answer(
        entry("kept", Count.Finite(2)),
        entry("lost", Count.Finite(2), 2),
        entry("moved", unknown, 3),
        entry("fresh", Count.Finite(2), 4),
        entry("refused", refusal, 5),
        entry("twin", Count.Finite(2), 6),
        entry("twin", Count.Finite(2), 6)
      ),
      answer(
        entry("kept", Count.Finite(2)),
        entry("lost", declined, 2),
        entry("moved", Count.Finite(3), 3),
        entry("fresh", declined, 4),
        entry("refused", declined, 5),
        entry("twin", Count.Finite(2), 6),
        entry("twin", declined, 6)
      )
    )
    def key(name: String) = compared.find(_.row.name == name).get.row.key
    val before =
      Map(key("kept") -> "2", key("lost") -> "2", key("moved") -> "2", key("refused") -> "?")
    val now = Differential.measured(compared)
    (Differential.regressed(compared, before).map(_.row.name) must contain(
      exactly("lost", "moved", "refused")
    )).and(now must contain(exactly(key("kept") -> "2", key("moved") -> "3")))
      .and(Differential.listed(now.map(Differential.line)) must_== now.toMap)
      .and(
        Differential.listed(List("method F.scala:3 | [method] | 1: its rule")) must_==
          Map("method F.scala:3 | [method]" -> "1")
      )
  }

  // A source that defines nothing has no classes and no rows; a top-level definition of two names
  // is a definition row and a method row per name.
  private def sources = {
    val module = TastyFrontEndSpec
      .compiled(
        "Empty.scala" -> "package demo\nimport scala.util.Try\n",
        "Two.scala" -> "val a, b = true\n"
      )
      .next
    Differential.answer(TastyFrontEnd, module).sources must beLike {
      case List(Right(empty), Right(two)) =>
        ((empty.methods, empty.definitions) must_== (Nil, Nil))
          .and(two.definitions.map(_.name) must_== List("a", "b"))
          .and(two.methods.map(_.name) must_== List("a", "b"))
    }
  }

}

object TastyFrontEndSpec {

  /** `sources` compiled in-process against the Scala library, as a build hands a module over. */
  def compiled(sources: (String, String)*): Differential.Input = {
    val root = Files.createTempDirectory("compiled-module")
    val classes = Files.createDirectories(root.resolve("classes"))
    val files = sources.map { (path, text) =>
      val file = root.resolve("src").resolve(path)
      Files.createDirectories(file.getParent)
      Files.writeString(file, text).toString
    }
    val libraries = sys.props
      .getOrElse("cardinality.test.classpath", sys.props("java.class.path"))
      .split(java.io.File.pathSeparator)
      .toList
      .filter { entry =>
        val name = Path.of(entry).getFileName.toString
        name.startsWith("scala-library") || name.startsWith("scala3-library")
      }
    val classpath = libraries.mkString(java.io.File.pathSeparator)
    val arguments = List("-d", classes.toString, "-classpath", classpath) ++ files
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    if (dotty.tools.dotc.Main.process(arguments.toArray, reporter).hasErrors)
      sys.error(s"fixture does not compile: ${reporter.allErrors.map(_.message).mkString("; ")}")
    val compiled = CompiledTypes.Classes(List(classes), libraries.map(Path.of(_)))
    val module = FrontEnd.Module(sources.toList, Report.Environment(classes = Some(compiled)))
    Differential.Input("", module, module)
  }

}
