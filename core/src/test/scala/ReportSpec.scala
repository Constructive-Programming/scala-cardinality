package cardinality

import scala.meta.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.specs2.Specification
import org.specs2.execute.Result

// The definitions a source introduces and the report over them: what a reader sees, and what the
// walk has to keep straight to make the numbers mean anything — nested definitions, enum cases
// counted once, aliases that name a size without holding one.
class ReportSpec extends Specification {

  def is = s2"""
    Definitions
      packages qualify the names they hold      $qualifiedNames
      kinds carry their own sizes               $kinds
      nested definitions are their own rows     $nested
      enum cases are counted by their enum      $enumCases
      values inside a body add nothing          $bodyValues
      unresolved names are the reason           $unresolved
      a definition resolves inside its body     $enclosing
      a forward reference resolves through the body  $forward
      entry points size one node at a time      $entryPoints
      package objects qualify their members     $packageObjects
      enum bodies contribute their cases        $enumBodies
      a companion object keeps its own value    $companions
      a source counts its top-level values       $sourceTotals
      signatures carry their parameters         $signatures

    Report
      reads a directory of sources              $directory
      reads a library's sources jar             $jar
      reads a single file                       $singleFile
      reports a path it cannot read             $missing
      reports a source it cannot parse          $unparsable
      renders the numbers it found              $renders
      orders rows by cardinality                $orders
      renders an empty report                   $empty

    Counting the branch gained since the report was cut
      a solved recursion reports its fixed point  $recursiveRows
      a lazy hole counts ω, and is no question    $lazyRow
      the ε₀ tier arrives through modelled types  $tierRow
      a function space still names its blocker    $blockedRow

    Targets: what the estimate cannot read yet (docs/baselines/eo-core-0.16.0-unresolved.md)
      an applied user type substitutes arguments  $targetApplied
      a type from another source resolves         $targetCrossSource
      a match type reduces on a known scrutinee   $targetMatchType
    """

  private def definitions(code: String): List[Definition] =
    Counter.definitions(dialects.Scala3(code).parse[Source].get)

  private def definition(code: String, name: String): Definition =
    definitions(code)
      .find(_.name == name)
      .getOrElse(throw new AssertionError(s"no definition $name"))

  private def temporary(files: (String, String)*): Path = {
    val root = Files.createTempDirectory("cardinality")
    files.foreach { case (name, content) => Files.writeString(root.resolve(name), content) }
    root
  }

  def qualifiedNames = {
    val found = definitions("package a\npackage b\nobject O { case class C(x: Boolean) }")
    found.map(_.name) === List("a.b.O", "a.b.O.C")
  }

  def kinds = {
    val found = definitions(
      """|sealed trait Light
         |case object Red extends Light
         |case class Dot(on: Boolean) extends Light
         |enum Color { case Red, Green, Blue }
         |object Module
         |type Flag = Boolean
         |opaque type Id = Byte
         |val top: Boolean = true
         |""".stripMargin,
    )
    found.map(d => (d.name, d.kind, d.size)) === List(
      ("Light", Definition.Kind.Abstract, None),
      ("Red", Definition.Kind.Object, Some(UnitSize)),
      ("Dot", Definition.Kind.Class, Some(BooleanSize)),
      ("Color", Definition.Kind.Enum, Some(TinySize(3))),
      ("Module", Definition.Kind.Object, Some(UnitSize)),
      ("Flag", Definition.Kind.Alias, Some(BooleanSize)),
      ("Id", Definition.Kind.Opaque, Some(UnitSize)),
      ("top", Definition.Kind.Value, Some(UnitSize)),
    )
  }

  def nested = {
    val found = definitions(
      "sealed trait Light {\n  case object Red extends Light\n  case object Green extends Light\n}",
    )
    (found.map(_.name) === List("Light", "Light.Red", "Light.Green"))
      .and(found.map(_.size) === List(None, Some(UnitSize), Some(UnitSize)))
  }

  def enumCases = definitions("enum Color { case Red, Green, Blue }").map(_.name) === List("Color")

  def bodyValues =
    definitions("class Derived(a: Boolean) { val b: Boolean = !a }").map(_.name) === List(
      "Derived"
    )

  def unresolved = {
    (definition("case class Holder(a: String, b: Boolean)", "Holder").unresolved === List("String"))
      .and(definition("case class Generic[A](a: A)", "Generic").unresolved === List("A"))
      .and(
        definition("case class Wrapped(a: Option[String])", "Wrapped").unresolved === List("String")
      )
      .and(
        // A modelled collection is no longer a reason: since the solver counts unbounded
        // lengths, `List[Boolean]` is ω exactly, and an unresolved element type reports itself.
        definition("case class Counted(a: List[Boolean])", "Counted").unresolved === Nil
      )
      .and(
        definition("case class Elements(a: List[String])", "Elements").unresolved === List(
          "String"
        )
      )
      .and(
        definition("type Fst[T] = T match { case (a, b) => a }", "Fst").unresolved === List(
          "a match type"
        )
      )
  }

  def enclosing = {
    val found = definition("class Outer(a: Boolean) { case class Inner(o: Outer) }", "Outer.Inner")
    found.size === Some(BooleanSize)
  }

  def companions = {
    val found = definitions(
      """|sealed trait Light
         |case class Spot(on: Boolean) extends Light
         |object Spot
         |case class Uses(s: Spot)
         |""".stripMargin,
    )
    // A companion object shares its name with its class but not its value space: the type claims
    // the name — `Uses` reads `Spot` as the class — while the module is one value of its own.
    (found.map(d => (d.name, d.kind, d.size)) === List(
      ("Light", Definition.Kind.Abstract, None),
      ("Spot", Definition.Kind.Class, Some(BooleanSize)),
      ("Spot", Definition.Kind.Object, Some(UnitSize)),
      ("Uses", Definition.Kind.Class, Some(BooleanSize)),
    ))
      .and(found.flatMap(_.unresolved) === Nil)
  }

  def entryPoints = {
    val source = dialects.Scala3("case class Pair(a: Boolean, b: Boolean)").parse[Source].get
    val pair = source.stats.collectFirst { case definition: Defn.Class => definition }.get
    val packageClause = dialects.Scala3("package p { case class X(a: Boolean) }").parse[Stat].get
    val packageObject =
      dialects.Scala3("package object p { case class X(a: Boolean) }").parse[Stat].get
    (Counter.stat(pair) === TinySize(4))
      .and(Counter.defn(pair) === TinySize(4))
      .and(Counter.ctor(pair.ctor) === TinySize(4))
      .and(Counter.param(pair.ctor.paramClauses.head.values.head) === BooleanSize)
      .and(Counter.stat(packageClause) === BooleanSize)
      .and(Counter.stat(packageObject) === BooleanSize)
  }

  def packageObjects =
    definitions("package object p { case class X(a: Boolean) }").map(_.name) === List("p.X")

  def sourceTotals = {
    val source = dialects
      .Scala3("val flag: Boolean = true\ncase class Pair(a: Boolean, b: Boolean)")
      .parse[Source]
      .get
    (Counter.source(source) === TinySize(5))
      .and(
        Counter.source(dialects.Scala3("val flag: Boolean = true").parse[Source].get) === UnitSize
      )
  }

  def signatures = {
    val found = definitions("case class Pair[A, B](a: A, b: B)")
    found.map(Definition.signature) === List("Pair[A, B]")
  }

  def enumBodies = {
    val found = definitions("enum E[A] { case One(a: A)\n  def flag: Boolean = true }")
    found.map(d => (d.name, d.params, d.size)) === List(("E", List("A"), Some(EffectiveOmega)))
  }

  def forward = {
    // Since the branch point the solver replaced the walk's read-as-you-go names: a body's
    // definitions are solved together, so a forward reference resolves instead of falling back to
    // ω. Both classes read two values, and no name is left unresolved.
    val code = "case class A(b: B)\ncase class B(b: Boolean)"
    (definition(code, "A").size === Some(BooleanSize))
      .and(definition(code, "A").unresolved === Nil)
      .and(definition(code, "B").size === Some(BooleanSize))
  }

  def directory = {
    val root = temporary(
      "Holder.scala" -> "package p\ncase class Holder(a: String)",
      "Light.scala" -> "package p\nenum Light { case Red, Green }",
    )
    val report = Report.of(List(root))
    (report.sources.map(_.path) === List(
      root.resolve("Holder.scala").toString,
      root.resolve("Light.scala").toString
    ))
      .and(report.definitions.map(_.name) === List("p.Holder", "p.Light"))
      .and(report.errors === Nil)
  }

  def jar = {
    val archive = Files.createTempFile("cardinality", ".jar")
    val out = new ZipOutputStream(Files.newOutputStream(archive))
    try {
      out.putNextEntry(new ZipEntry("README.md"))
      out.write("not a source".getBytes(UTF_8))
      out.closeEntry()
      out.putNextEntry(new ZipEntry("p/Light.scala"))
      out.write("package p\nenum Light { case Red, Green }".getBytes(UTF_8))
      out.closeEntry()
    } finally out.close()
    val report = Report.of(List(archive))
    (report.sources.map(_.path) === List("p/Light.scala"))
      .and(report.definitions.map(_.name) === List("p.Light"))
      .and(report.errors === Nil)
  }

  def singleFile = {
    val root = temporary("Light.scala" -> "enum Light { case Red, Green }")
    Report.of(List(root.resolve("Light.scala"))).definitions.map(_.name) === List("Light")
  }

  def missing = {
    val path = Files.createTempDirectory("cardinality").resolve("missing.scala")
    val report = Report.of(List(path))
    (report.sources === Nil).and(report.errors.map(_.path) === List(path.toString))
  }

  def unparsable = {
    val root = temporary(
      "Broken.scala" -> "case class Broken(a: Boolean",
      "Fine.scala" -> "case class Fine(a: Boolean)",
    )
    val report = Report.of(List(root))
    (report.definitions.map(_.name) === List("Fine"))
      .and(report.errors.map(_.path) === List(root.resolve("Broken.scala").toString))
      .and(report.errors.map(_.message).mkString must contain("line"))
      .and(report.render must contain(s"could not read ${root.resolve("Broken.scala")}"))
  }

  def orders = {
    val root = temporary(
      "Types.scala" ->
        """|case class Huge(f: String => String)
           |case class Many(xs: List[Boolean])
           |case class Partial[A, B](a: A, b: Boolean)
           |case class Lossy(d: Double)
           |case class Big(n: Int)
           |type Zero = Nothing
           |""".stripMargin,
    )
    val rendered = Report.of(List(root)).render
    val table = rendered.linesIterator.toList
      .dropWhile(_ != "Stored-value estimates (constructor inputs; `?` = unresolved)")
      .drop(2)
    // What the calculator could not bound leads the table — `Huge` (the unresolved `String`) and
    // `Partial` (the unresolved `A`) — then the bounded sizes from the largest down: the countable
    // `Many`, the lossy `Lossy`, the capacity `Big`, and the empty `Zero` last. A size whose tier
    // is known is not a question: `Many` is ω exactly.
    (table(0) must contain("Huge"))
      .and(table(1) must contain("Partial"))
      .and(table(2) must contain("Many"))
      .and(table(3) must contain("Lossy"))
      .and(table(4) must contain("Big"))
      .and(table(5) must contain("Zero"))
      .and(table(0) must contain("?"))
      .and(table(1) must contain("?"))
      .and(table(2) must contain("ω"))
      .and(rendered must contain("1 with no values"))
  }

  def empty = {
    val rendered = Report.of(Nil).render
    rendered === """scala-cardinality — 0 sources, 0 definitions
                  |  stored-value estimates: constructor inputs only; finite capacities are upper bounds
                  |  ?: unresolved, not a proof of infinity; opaque representations are not singletons""".stripMargin
  }

  def renders = {
    val root = temporary(
      "Types.scala" ->
        """|package p
           |sealed trait Shape
           |case class Dot(on: Boolean) extends Shape
           |case class Box[A](a: A) extends Shape
           |object Shape
           |type Flag = Boolean
           |""".stripMargin,
    )
    val rendered = Report.of(List(root)).render
    val lines = rendered.linesIterator.toList
    val estimate = lines
      .dropWhile(_ != "Stored-value estimates (constructor inputs; `?` = unresolved)")
      .drop(2)
    (lines.head === "Generic method / constructor implementation cardinalities")
      .and(rendered must contain("scala-cardinality — 1 source, 5 definitions"))
      .and(rendered must contain("stored-value estimates: constructor inputs only"))
      .and(rendered must contain("1  p.Box.<init>  Box[A](a: A)  Types.scala:4  [constructor]"))
      .and(estimate(0) must contain("?  p.Box[A]"))
      .and(estimate(0) must contain("Types.scala:4"))
      .and(estimate(0) must contain("unresolved: A"))
      .and(estimate.last must contain("abstract"))
      .and(estimate.last must not(contain("unresolved:")))
  }

  // A rule the estimate does not have yet, pinned as an expectation that fails today: specs2
  // reports it as pending and turns it into a failure once the rule lands, so the marker cannot
  // go stale. The ledger in `docs/baselines/eo-core-0.16.0-unresolved.md` tracks these, and the
  // reason is what a rule would have to do.
  private def target(actual: => Definition, expected: Size, reason: String): Result = {
    val row = actual
    pendingUntilFixed(s"$reason: this row should read $expected, the estimate returns ${row.size}")(
      row.size === Some(expected)
    )
  }

  private def crossSource: Definition = {
    val root = temporary(
      "Box.scala" -> "package p\ncase class Box(a: Boolean)",
      "Use.scala" -> "package p\ncase class Use(b: Box)",
    )
    Report
      .of(List(root))
      .definitions
      .find(_.name == "p.Use")
      .getOrElse(throw new AssertionError("no definition p.Use"))
  }

  def targetApplied = target(
    definition("case class Pair[A](a: A, b: A)\ncase class Use(p: Pair[Boolean])", "Use"),
    TinySize(4),
    "an applied user type needs its arguments substituted into its definition",
  )

  def targetCrossSource = target(
    crossSource,
    BooleanSize,
    "the estimate reads one source at a time; the report holds all of them",
  )

  def targetMatchType = target(
    definition(
      "type Fst[T] = T match { case (f, s) => f }\ntype Two = Fst[(Boolean, Boolean)]",
      "Two",
    ),
    BooleanSize,
    "a match type needs reduction on a known scrutinee",
  )

  def recursiveRows = {
    val found = definitions(
      "sealed trait Nat\ncase object Zero extends Nat\ncase class Succ(n: Nat) extends Nat",
    )
    // The body's equations are solved together, so `Succ` is worth what a reference to it is worth:
    // `Nat` is the sum of its cases and the family is countably infinite. The row carries the same
    // number as the reference, and nothing about it is unresolved.
    (found.map(d => (d.name, d.kind, d.size)) === List(
      ("Nat", Definition.Kind.Abstract, None),
      ("Zero", Definition.Kind.Object, Some(UnitSize)),
      ("Succ", Definition.Kind.Class, Some(EffectiveOmega)),
    ))
      .and(found.flatMap(_.unresolved) === Nil)
  }

  def lazyRow = {
    val root = temporary(
      "Timeline.scala" ->
        "package p\ncase class Timeline(head: Boolean, tail: => Timeline)",
    )
    val rendered = Report.of(List(root)).render
    val table = rendered.linesIterator.toList
      .dropWhile(_ != "Stored-value estimates (constructor inputs; `?` = unresolved)")
      .drop(2)
    // A hole the constructor never demands counts its infinite values as well, so `ω` is an
    // answer: the row shows the size, and the summary does not count it as unresolved.
    (definition("case class Timeline(head: Boolean, tail: => Timeline)", "Timeline").size ===
      Some(EffectiveOmega))
      .and(table(0) must contain("ω  p.Timeline"))
      .and(rendered must contain("1 with more than one value"))
  }

  def tierRow = {
    val grid = definition("case class Grid(cells: LazyList[LazyList[Unit]])", "Grid")
    // ω^ω lands on the analysis' ε₀ tier, and every step of that route is a type the calculator
    // models: the row is a size, not a question.
    (grid.size === Some(EffectiveEpsilon0)).and(grid.unresolved === Nil)
  }

  def blockedRow = {
    val reducer = definition("case class Reducer(run: String => String)", "Reducer")
    (reducer.size === Some(EffectiveEpsilon0))
      .and(reducer.unresolved === List("String"))
  }

}
