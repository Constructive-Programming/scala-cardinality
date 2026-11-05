package cardinality

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.capacity.*
import cardinality.reporting.Report
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Comparator
import org.specs2.Specification
import org.specs2.specification.AfterAll

// The artifacts the opt-in checkout runner writes are read by tools and by the delta between two
// runs, so each row must stay one line, carry a stable identity, and read back as it was written;
// the delta must join an older run's rows to a newer run's and say exactly what moved.
class EoArtifactsSpec extends Specification with AfterAll {

  def is = s2"""
    Fields
      a field is one line with no tab                        $field
      a list keeps its separator out of its items            $list
      a refinement's `; ` stays in its reason                 $refinementFields
      a table reads back the fields it was written from      $roundTrip

    Definitions table
      a row carries identity, size, class and reasons         $definitionRow
      every kind of reason has its tag                        $tags
      the table reads back row by row                         $definitionsRoundTrip
      input order does not move a stored value                $inputOrder
      a class and its companion name their members apart      $companionMembers
      so do a class `+` and an object `+#`                    $operatorOwners
      a value named by a pattern is named on one line         $patternNames

    Delta between runs
      a run compared with itself has moved nothing            $selfDelta
      an older run's signature joins its one-line spelling    $canonicalSignature
      rows of an older run are joined to a newer one          $legacyDelta
      definitions join on their full identity when both have it  $fullDelta
      a repeated identity is refused, not merged              $repeated
    """

  private def parse(code: String): Source = dialects.Scala3(code).parse[Source].get

  // A tab or line break, with the whitespace around it, is one space; a run of spaces is what a row
  // printed, a literal type's among them, and stays.
  def field = Tsv.field("a\n  b\tc  d\r\n") === "a b c  d"

  // Inside braces `; ` is Scala's own separator (a refinement's members), and inside a literal it
  // is the literal's: neither is written `, ` or splits an item. A brace an item leaves open (a
  // reason cut short) does not hold the separators after it.
  def list = {
    val refined = "refined: Two { type X = Int; type Y = Boolean }"
    (Tsv.list(List("x; y", "z")) === "x, y; z")
      .and(Tsv.items(Tsv.list(List("A", "F[_, _]"))) === List("A", "F[_, _]"))
      .and(Tsv.items(Tsv.list(List(refined, "z"))) === List(refined, "z"))
      .and(Tsv.items(Tsv.list(List("x: \"a; b\"", "z"))) === List("x: \"a; b\"", "z"))
      .and(Tsv.items(Tsv.list(List("new X { def f(", "z"))) === List("new X { def f(", "z"))
      .and(Tsv.items("") === Nil)
  }

  // The same refinement, written with `;` or with line breaks, reads one way in the signature and
  // reason cells of its row, and the reason reads back as one item.
  def refinementFields = {
    val code = "package r\ntrait Two { type X; type Y }\nobject R {\n" +
      "  def f(b: Boolean): Two { type X = Int; type Y = Boolean } = ???\n" +
      "  def h(b: Boolean): Two {\n    type X = Int\n    type Y = Boolean\n  } = ???\n}\n"
    val written = MethodAnalysis
      .analyze(List(MethodAnalysis.Input("r/R.scala", parse(code))))
      .map(entry => EoCheckout.row(entry, ""))
    val refined = "Two { type X = Int; type Y = Boolean }"
    Tsv
      .rows(Tsv.table(EoCheckout.MethodColumns, written))
      .map(row => (row("signature"), Tsv.items(row("unresolved_reasons")))) === List(
      (
        s"f(b: Boolean): $refined",
        List(s"refined representation unavailable (constraints not erased): $refined")
      ),
      (
        s"h(b: Boolean): $refined",
        List(s"refined representation unavailable (constraints not erased): $refined")
      )
    )
  }

  def roundTrip = {
    val rows = List(List("core", "a\tb", ""), List("avro", "c\nd", "e; f"))
    Tsv.rows(Tsv.table(List("module", "x", "y"), rows)) === List(
      Map("module" -> "core", "x" -> "a b", "y" -> ""),
      Map("module" -> "avro", "x" -> "c d", "y" -> "e; f")
    )
  }

  private val shapes =
    """|package p
       |sealed trait Shape
       |final case class Dot(on: Boolean) extends Shape
       |final case class Box[A](a: A) extends Shape
       |object Shape
       |case class Missing(s: Schema)
       |""".stripMargin

  private def table(path: String, code: String): List[Map[String, String]] = {
    val tree = parse(code)
    val report =
      Report(List(Report.Source(path, Counter.definitions(tree, Library.of(List(tree))))), Nil)
    Tsv.rows(
      Tsv.table(EoCheckout.DefinitionColumns, report.definitionRows.map(EoCheckout.definitionRow))
    )
  }

  def definitionRow = {
    val rows = table("p/src/Types.scala", shapes)
      .map(row => (row("qualified_name"), row("kind")) -> row)
      .toMap
    val box = rows(("p.Box", "class"))
    val missing = rows(("p.Missing", "class"))
    (box === Map(
      "module" -> "p",
      "source" -> "p/src/Types.scala",
      "qualified_name" -> "p.Box",
      "kind" -> "class",
      "params" -> "A",
      "line" -> "4",
      "size" -> "?",
      "class" -> "instantiation-dependent",
      "reasons" -> "depends on its instantiation (A)",
      "reason_kinds" -> "param:A"
    ))
      .and(missing("reason_kinds") === "unknown:Schema")
      .and(missing("class") === "unresolved")
      .and(rows(("p.Dot", "class"))("size") === "2")
      .and(rows(("p.Shape", "object"))("class") === "with one value")
      .and(rows(("p.Shape", "abstract"))("line") === "2")
  }

  def tags = {
    import Definition.Unbound.*
    List(
      Parameter("A"),
      HigherKinded("F", 1),
      Member("M"),
      Open("O"),
      Unknown("U"),
      Imported("I", "imported from p.I, not supplied"),
      Syntax("a match type"),
      Refusal("sealed family not summed: P")
    ).map(reason => s"${reason.tag}:${reason.subject}") === List(
      "param:A",
      "param:F[_]",
      "member:M",
      "open:O",
      "unknown:U",
      "imported:I (imported from p.I, not supplied)",
      "syntax:a match type",
      "refusal:sealed family not summed: P"
    )
  }

  def definitionsRoundTrip = {
    val tree = parse(shapes)
    val report = Report(
      List(Report.Source("p/Types.scala", Counter.definitions(tree, Library.of(List(tree))))),
      Nil
    )
    val written = report.definitionRows.map(EoCheckout.definitionRow)
    Tsv
      .rows(Tsv.table(EoCheckout.DefinitionColumns, written))
      .map(row => EoCheckout.DefinitionColumns.map(row)) === written.map(_.map(Tsv.field))
  }

  // A family read from another file, in both orders: the stored values must not depend on which
  // file the library met first. (Scala requires a sealed family's subtypes in its own file.)
  def inputOrder = {
    val inputs = List(
      "a/Parent.scala" -> ("package a\nsealed trait P\nobject P { case object One extends P }\n" +
        "final case class Two(on: Boolean) extends P"),
      "b/Child.scala" -> "package a\ncase class Use(p: P)"
    ).map((path, code) => MethodAnalysis.Input(path, parse(code)))
    val definitions = EoCheckout.definitionsOf(inputs)
    (EoCheckout.definitionsOf(inputs.reverse) === definitions)
      .and(
        definitions("b/Child.scala").map(found => found.name -> found.size.map(_.render)) ===
          List("a.Use" -> Some("3"))
      )
  }

  // `class O { type T = Int }` and `object O { type T = String }` declare two members `T`: one of
  // each instance, `O#T`, and one of the object, `O.T`. They are two rows with two identities.
  def companionMembers = {
    val code = "package d\nclass O { type T = Int }\nobject O { type T = String }"
    val (_, rows) =
      EoCheckout.checkedDefinitions(List(MethodAnalysis.Input("d/O.scala", parse(code))))
    rows.map(row => (row.definition.name, row.definition.kind.toString, row.size)) === List(
      ("d.O", "Class", "1"),
      ("d.O#T", "Alias", "≤2^32"),
      ("d.O", "Object", "1"),
      ("d.O.T", "Alias", "ω")
    )
  }

  // `+#` is a name (`#` is an operator character), so the instance mark is no suffix of the text:
  // the class `+`'s member reads `+#T`, the object `+#`'s `+#.T`, and the identities stay apart.
  def operatorOwners = {
    val code = "package h\nclass + { type T = Int }\nobject +# { type T = Boolean }"
    val (_, rows) =
      EoCheckout.checkedDefinitions(List(MethodAnalysis.Input("h/H.scala", parse(code))))
    rows.map(row => (row.definition.name, row.definition.kind.toString, row.size)) === List(
      ("h.+", "Class", "1"),
      ("h.+#T", "Alias", "≤2^32"),
      ("h.+#", "Object", "1"),
      ("h.+#.T", "Alias", "2")
    )
  }

  // A value bound by a pattern laid out over two lines is named on one, in the report's table and
  // in the definitions TSV alike.
  def patternNames = {
    val tree = parse("package tv\nval (first,\n  second) = (1, true)\n")
    val report = Report(List(Report.Source("tv/V.scala", Counter.definitions(tree))), Nil)
    val named = report.definitionRows.map(row => row.definition.name)
    (named === List("tv.(first, second)"))
      .and(report.render.linesIterator.count(_.contains("tv.(first, second)")) === 1)
  }

  private val scratch = Files.createTempDirectory("eo-artifacts")

  def afterAll(): Unit =
    Files.walk(scratch).sorted(Comparator.reverseOrder()).forEach(path => Files.delete(path))

  private def run(files: (String, String)*): Path = {
    val root = Files.createTempDirectory(scratch, "eo-run")
    files.foreach((name, content) => Files.writeString(root.resolve(name), content, UTF_8))
    root
  }

  private val methodsHeader = Tsv.table(EoCheckout.MethodColumns, Nil)

  private def methodRow(fields: String*): String = fields.mkString("\t")

  // An older run: a reason that spans lines breaks its TSV row, its signature keeps the layout's
  // spaces, an anonymous owner carries its line, and definitions exist only as the report's table.
  private val older = run(
    "eo-checkout-methods.tsv" -> (methodsHeader + List(
      methodRow(
        "core",
        "core/A.scala",
        "p.A.f",
        "method",
        "",
        "f[A]( x: A, ): A",
        "?",
        "refined representation unavailable (constraints not erased): R {\n  type X = A\n}"
      ),
      methodRow(
        "core",
        "core/A.scala",
        "p.<extension@3>.g",
        "method",
        "extension (x: Int)",
        "g: Int",
        "2^32",
        ""
      )
    ).mkString("", "\n", "\n")),
    "eo-checkout.txt" ->
      """|scala-cardinality — 1 source, 3 definitions
         |Stored-value estimates (constructor inputs; `?` = unresolved)
         |
         |?     p.Box[A]  class   Types.scala:4  depends on its instantiation (A)
         |2^32  p.Big     class   Types.scala:5
         |1     p.Shape   object  Types.scala:6
         |""".stripMargin
  )

  private def definitionsTable(rows: List[String]*): String =
    Tsv.table(EoCheckout.DefinitionColumns, rows.toList)

  private val newer = run(
    "eo-checkout-methods.tsv" -> (methodsHeader + List(
      methodRow("core", "core/A.scala", "p.A.f", "method", "", "f[A](x: A): A", "1", ""),
      methodRow(
        "core",
        "core/A.scala",
        "p.<extension@4>.g",
        "method",
        "extension (x: Int)",
        "g: Int",
        "2^32",
        ""
      ),
      methodRow("core", "core/B.scala", "p.B.h", "method", "", "h: Unit", "1", "")
    ).mkString("", "\n", "\n")),
    "eo-checkout-definitions.tsv" -> definitionsTable(
      List(
        "core",
        "core/p/Types.scala",
        "p.Box",
        "class",
        "A",
        "4",
        "?",
        "instantiation-dependent",
        "depends on its instantiation (A)",
        "param:A"
      ),
      List(
        "core",
        "core/p/Types.scala",
        "p.Big",
        "class",
        "",
        "5",
        "≤2^32",
        "with more than one value",
        "",
        ""
      ),
      List(
        "core",
        "core/p/Types.scala",
        "p.Shape",
        "object",
        "",
        "6",
        "?",
        "unresolved",
        "unresolved: Q",
        "unknown:Q"
      )
    )
  )

  def selfDelta = {
    val lines = EoDelta.report(newer, newer).linesIterator.toList
    (lines must contain("== methods: 3 -> 3 rows; +0 -0 ~0"))
      .and(lines must contain("== stored values: 3 -> 3 rows; +0 -0 ~0"))
      .and(lines must contain("== stored-value classes: 0 rows moved"))
  }

  // An older run collapsed a signature's whitespace but kept the spaces just inside its brackets
  // and its trailing comma; the join reads both spellings as one.
  def canonicalSignature =
    (EoDelta.canonical(
      "apply[S, A]( getOrModify: S => Either[S, A], reverseGet: A => S, ): <inferred>"
    ) === "apply[S, A](getOrModify: S => Either[S, A], reverseGet: A => S): <inferred>")
      .and(EoDelta.canonical("f[F[_, _]](x: F[A, B]): A") === "f[F[_, _]](x: F[A, B]): A")

  def legacyDelta = {
    val lines = EoDelta.report(older, newer).linesIterator.toList
    (lines must contain("== methods: 2 -> 3 rows; +1 -0 ~1"))
      .and(lines must contain("  ~ core/A.scala | p.A.f | method |  | f[A](x: A): A"))
      .and(
        lines must contain(
          "      ? [refined representation unavailable (constraints not erased): R { type X = A }]"
        )
      )
      .and(lines must contain("   -> 1 []"))
      .and(lines must contain("  + core/B.scala | p.B.h | method |  | h: Unit  1 []"))
      .and(lines must contain("  1 changed: ? -> n"))
      .and(lines must contain("== stored values: 3 -> 3 rows; +0 -0 ~2"))
      .and(lines must contain("  ~ p.Big | class | Types.scala"))
      .and(lines must contain("  1 changed: n -> n"))
      .and(lines must contain("  1 changed: n -> ?"))
      .and(lines must contain("== stored-value classes: 1 rows moved"))
      .and(lines must contain("  1 with one value -> unresolved"))
  }

  def fullDelta = {
    val moved = run(
      "eo-checkout-methods.tsv" -> methodsHeader,
      "eo-checkout-definitions.tsv" -> definitionsTable(
        List(
          "core",
          "core/p/Types.scala",
          "p.Box",
          "class",
          "A",
          "9",
          "?",
          "instantiation-dependent",
          "depends on its instantiation (A)",
          "param:A"
        ),
        List(
          "avro",
          "avro/p/Types.scala",
          "p.Big",
          "class",
          "",
          "5",
          "≤2^32",
          "with more than one value",
          "",
          ""
        )
      )
    )
    val lines = EoDelta.report(newer, moved).linesIterator.toList
    // The line is not part of a row's identity; the source is, so the same name in another
    // module's file of the same name is another row.
    (lines must contain("== stored values: 3 -> 2 rows; +1 -2 ~0"))
      .and(lines must contain("  + avro/p/Types.scala | p.Big | class |   ≤2^32 []"))
      .and(lines must contain("  - core/p/Types.scala | p.Big | class |   ≤2^32 []"))
  }

  def repeated = {
    val twice = run(
      "eo-checkout-methods.tsv" -> methodsHeader,
      "eo-checkout-definitions.tsv" -> definitionsTable(
        List(
          "core",
          "core/X.scala",
          "p.X",
          "class",
          "",
          "1",
          "2",
          "with more than one value",
          "",
          ""
        ),
        List("core", "core/X.scala", "p.X", "class", "", "7", "1", "with one value", "", "")
      )
    )
    EoDelta.report(twice, twice) must throwA[IllegalArgumentException](message =
      "repeat an identity"
    )
  }

}
