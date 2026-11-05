package cardinality.reporting

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.*
import cardinality.types.Spelling
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.specs2.Specification
import org.specs2.specification.AfterAll

// What a report and its artifacts quote has to stay one line and say what the source says: a
// signature or reason laid out over several lines, an internal placeholder, a category cut at the
// wrong colon, two sources that share a file name, or a capacity printed as if it were exact would
// each make a row read as something it is not.
class ReportHygieneSpec extends Specification with AfterAll {

  def is = s2"""
    One-line spelling
      a broken parameter clause reads on one line           $brokenClause
      a refinement's body reads on one line                 $refinement
      comments between tokens are layout                    $comments
      a literal keeps its own spaces                        $literal
      a line break between statements in braces reads `;`   $statementBreaks
      an operator at a line end continues only an operand   $trailingOperators
      text the tokenizer refuses keeps its characters       $untokenizable
      a character Scala does not have keeps its spaces      $invalidCharacter
      spelling is idempotent                                $idempotent

    Method rows
      a refinement reason quotes the source, on one line    $refinementReason
      so does one behind aliases of aliases                 $nestedAliases
      a source name shaped like a placeholder is its own    $placeholderName
      so is one that only starts like a placeholder         $placeholderPrefix
      a literal type reads as written in rows and reasons   $literalRows
      a refinement reads one way however it is laid out     $refinementLayouts
      an operator name stays apart from its result's colon  $operatorNames
      a signature drops its layout and trailing comma       $signatureLayout
      curried clauses read one way however laid out         $curriedClauses
      a literal shaped like a placeholder is the literal    $placeholderLiteral
      every reason spells a refinement as the row does      $refinedReasons
      so does one whose result is inferred                  $inferredResult

    Blocker categories
      the category ends at the first separator              $separator
      a bound or context bound is not a separator           $bounds
      neither is the colon of an operator name              $operatorColons
      but the colon after a name ending in `_` is           $underscoreColons
      every subtype, name and namespace tallies once        $instances
      a union counts under the reason it stopped on         $unions
      whatever its alternatives are spelled like            $unionAlternatives
      prose and its punctuation are kept                    $prose
      a report tallies a category once per signature        $tally

    Locations
      two sources with one file name keep apart             $sameName
      how the inputs were named does not move a row         $namedInputs
      a single source prints its file name                  $singleSource
      jar entries and directory files read relative to both $mixedInputs
      a source that cannot be read is placed as a row is    $failedInputs

    Capacities
      a capacity prints as the bound it is                  $capacityRow
      an exact count prints its digits                      $exactRow
    """

  private def parse(code: String): Source = dialects.Scala3(code).parse[Source].get

  private def entries(code: String): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyze(List(MethodAnalysis.Input("Example.scala", parse(code))))

  private def reasons(entry: MethodAnalysis.Entry): List[String] = entry.count match {
    case Count.Unresolved(found) => found
    case _                       => Nil
  }

  def brokenClause =
    Spelling.flat("BijectionIso[S, T, A, B](\n    read: S => A,\n    build: B => T,\n)") ===
      "BijectionIso[S, T, A, B](read: S => A, build: B => T)"

  def refinement = {
    val tpe = dialects.Scala3("Indexed[S, T] { type X = (A, B) }").parse[Type].get
    // Scalameta prints a refinement's body on lines of its own; the row quotes it on one.
    (tpe.syntax must contain("\n")).and(Spelling.of(tpe) === "Indexed[S, T] { type X = (A, B) }")
  }

  def comments =
    Spelling.flat("f(\n  a: Int, // the count\n  /* flag */ b: Boolean\n): Int") ===
      "f(a: Int, b: Boolean): Int"

  // Collapsing whitespace in the text would rewrite the literal type `"a  b"` into another one; a
  // reason's prose is collapsed around it.
  def literal =
    (Spelling.flat("f(x: \"a  b\" ): Int") === "f(x: \"a  b\"): Int")
      .and(
        Spelling.line(
          "unsupported\n  type: \"a  b\"  or 'x'"
        ) === "unsupported type: \"a  b\" or 'x'"
      )
      .and(
        Spelling.line(
          "`a  b`  and the bound's  capabilities"
        ) === "`a  b` and the bound's capabilities"
      )

  // Inside braces a line break separates two statements, as `;` does: one declaration, two layouts.
  def statementBreaks =
    (Spelling.flat("Two {\n  type X = Int\n  type Y = Boolean\n}") ===
      "Two { type X = Int; type Y = Boolean }")
      .and(
        Spelling.flat(
          "Two { type X = Int; type Y = Boolean }"
        ) === "Two { type X = Int; type Y = Boolean }"
      )
      .and(
        Spelling.flat("T match {\n  case Int => String\n  case _ => Int\n}") ===
          "T match { case Int => String; case _ => Int }"
      )
      .and(
        Spelling.flat(
          "f(x: Int = {\n  val a = 1\n  a\n}): Int"
        ) === "f(x: Int = { val a = 1; a }): Int"
      )

  // A line that ends in an operator name continues into an operand, never into a definition or a
  // case.
  def trailingOperators =
    (Spelling.flat("Two {\n  type F = ~>\n  type G = Int\n}") ===
      "Two { type F = ~>; type G = Int }")
      .and(
        Spelling.flat("T match {\n  case Int => ~>\n  case _ => Int\n}") ===
          "T match { case Int => ~>; case _ => Int }"
      )
      .and(Spelling.flat("{\n  a +:\n    b\n}") === "{ a +: b }")

  def untokenizable = Spelling.flat("the bound's\n  capabilities") === "the bound's capabilities"

  // Scalameta's tokenizer reads `—` as an invalid token that swallows the space before it; text
  // its tokens do not reproduce is only collapsed, never rejoined.
  def invalidCharacter =
    (Spelling.flat("P | Q — PQ derives from Q") === "P | Q — PQ derives from Q")
      .and(Spelling.line("a\n  —  b") === "a — b")

  def idempotent = {
    val text = "extension [S, F[_, _]](\n  o: Optic[S, F]\n)(using FF: Fold[F])"
    Spelling.flat(Spelling.flat(text)) === Spelling.flat(text)
  }

  // `Aux` is a transparent alias, so the normalizer freezes its arguments behind placeholders
  // before it reads the refinement it expands to. The reason names what the source wrote there.
  def refinementReason = {
    val found = entries(
      """|package r
         |trait Optic[A] { type X }
         |object Optic {
         |  type Aux[A, X0] = Optic[A] { type X = X0 }
         |  def make[A, B](a: A, b: B): Aux[A, (A, B)] = ???
         |}
         |""".stripMargin
    ).find(_.name == "r.Optic.make").get
    (reasons(found) must contain(
      "refined representation unavailable (constraints not erased): Optic[A] { type X = (A, B) }"
    )).and(reasons(found).exists(_.contains("$cardinality")) must beFalse)
  }

  // `PGrate` passes its arguments to `Aux`, whose normalizer freezes them again: the placeholders
  // of the outer expansion are leaves of the inner one, and both are spelled back.
  def nestedAliases = {
    val found = entries(
      """|package g
         |trait Optic[S, A] { type X }
         |object Optic {
         |  type Aux[S, A, X0] = Optic[S, A] { type X = X0 }
         |  type PGrate[S, A] = Aux[S, A, Unit]
         |  type Grate[S] = PGrate[S, S]
         |  def iso[S, A](f: S => A): PGrate[S, A] = ???
         |  def unit[A]: Grate[Option[A]] = ???
         |}
         |""".stripMargin
    )
    def reasonsOf(name: String) = reasons(found.find(_.name == name).get)
    (reasonsOf("g.Optic.iso") must contain(
      "refined representation unavailable (constraints not erased): Optic[S, A] { type X = Unit }"
    )).and(
      reasonsOf("g.Optic.unit") must contain(
        "refined representation unavailable (constraints not erased): " +
          "Optic[Option[A], Option[A]] { type X = Unit }"
      )
    )
  }

  // Placeholders never take a name the sources use, so a type the source itself named like one is
  // quoted as that name — backquoted, as scalameta spells it — not as what a placeholder stood for.
  def placeholderName = {
    val found = entries(
      """|package d
         |trait Optic[A] { type X }
         |final case class `$cardinalityProjection1`(on: Boolean)
         |object Optic {
         |  type Aux[A, X0] = Optic[A] { type X = X0 }
         |  def make[A](a: A): Aux[A, `$cardinalityProjection1`] = ???
         |}
         |""".stripMargin
    ).find(_.name == "d.Optic.make").get
    reasons(found) must contain(
      "refined representation unavailable (constraints not erased): " +
        "Optic[A] { type X = `$cardinalityProjection1` }"
    )
  }

  // A source name that only begins with a placeholder's text is not a placeholder: it is quoted as
  // the source wrote it, never with the start of it replaced (`Inta`).
  def placeholderPrefix = {
    val found = entries(
      """|package d
         |trait Optic[A] { type X }
         |final case class `$cardinalityProjection1a`(on: Boolean)
         |object Optic {
         |  type Aux[A, X0] = Optic[A] { type X = X0 }
         |  def make[A](a: A): Aux[Int, `$cardinalityProjection1a`] = ???
         |}
         |""".stripMargin
    ).find(_.name == "d.Optic.make").get
    reasons(found) must contain(
      "refined representation unavailable (constraints not erased): " +
        "Optic[Int] { type X = `$cardinalityProjection1a` }"
    )
  }

  // Two overloads that differ only in a literal's spaces (a target name keeps their erasures apart)
  // are two rows, each naming its own type. A literal type is read, so the reason that quotes it is
  // an intersection's, which stays refused.
  def literalRows = {
    val found = entries(
      """|package l
         |import scala.annotation.targetName
         |object O {
         |  def f(x: "a  b" & Int): Int = 1
         |  @targetName("f2") def f(x: "a b" & Int): Int = 2
         |}
         |""".stripMargin
    )
    (found.map(_.signature) === List("f(x: \"a  b\" & Int): Int", "f(x: \"a b\" & Int): Int"))
      .and(
        found.map(reasons) === List(
          List("unsupported intersection relationship: \"a  b\" & Int"),
          List("unsupported intersection relationship: \"a b\" & Int")
        )
      )
  }

  // The same refinement written with `;`, with line breaks, or reached through an alias that
  // scalameta prints over lines reads one way in signatures and reasons.
  def refinementLayouts = {
    val found = entries(
      """|package r
         |trait Two { type X; type Y }
         |object R {
         |  def f(b: Boolean): Two { type X = Int; type Y = Boolean } = ???
         |  type Aux[X0, Y0] = Two { type X = X0; type Y = Y0 }
         |  def g(b: Boolean): Aux[Int, Boolean] = ???
         |  def h(b: Boolean): Two {
         |    type X = Int
         |    type Y = Boolean
         |  } = ???
         |}
         |""".stripMargin
    ).map(entry => entry.name -> entry).toMap
    val refined = "Two { type X = Int; type Y = Boolean }"
    val reason = s"refined representation unavailable (constraints not erased): $refined"
    (found("r.R.f").signature === s"f(b: Boolean): $refined")
      .and(found("r.R.h").signature === s"h(b: Boolean): $refined")
      .and(
        List("r.R.f", "r.R.g", "r.R.h")
          .map(name => reasons(found(name))) === List.fill(3)(List(reason))
      )
  }

  // `+:` followed by `: Boolean` would read as the name `+::`; a name ending in `_` takes a colon
  // the same way; a name the source must quote stays quoted.
  def operatorNames = {
    val found = entries(
      """|package o
         |abstract class P {
         |  def +: : Boolean
         |  def :: : Boolean
         |  def x_ : Boolean
         |  def `type`: Boolean
         |  def ++(b: Boolean): Boolean
         |}
         |""".stripMargin
    )
    found.map(_.signature) === List(
      "+: : Boolean",
      ":: : Boolean",
      "x_ : Boolean",
      "`type`: Boolean",
      "++(b: Boolean): Boolean",
      // An abstract class has a primary constructor too, its subclasses' call.
      "P"
    )
  }

  def signatureLayout = {
    val found = entries(
      """|package s
         |object O {
         |  def pick[A](
         |      x: A,
         |      y: A,
         |  ): A = x
         |}
         |""".stripMargin
    ).find(_.name == "s.O.pick").get
    (found.signature === "pick[A](x: A, y: A): A").and(found.count === Count.Finite(2))
  }

  // Clauses are spelled one by one, so a line break between curried clauses is no space, in a
  // signature and in the enclosing context of an extension's identity.
  def curriedClauses = {
    val found = entries(
      "package c\nobject O {\n  def f[A](x: A)\n      (y: A): A = x\n  def g[A](x: A)(y: A): A = x\n}\n"
    )
    val code =
      "package e\nextension [A](x: A)\n    (using o: Ordering[A])\n  def big(y: A): Boolean = true\n"
    val tree = parse(code)
    val big = MethodAnalysis.analyze(List(MethodAnalysis.Input("e/E.scala", tree))).head
    (found.map(_.signature) === List("f[A](x: A)(y: A): A", "g[A](x: A)(y: A): A"))
      .and(
        cardinality.EoBaseline
          .identity(big, tree)
          ._4 === "extension [A](x: A)(using o: Ordering[A])"
      )
  }

  // A literal type's text is what the source wrote, even shaped like a placeholder: restored, it
  // would quote another type (`"Int"`, the first frozen leaf's spelling).
  def placeholderLiteral = {
    val found = entries(
      """|package p
         |trait Optic[A] { type X }
         |object Optic {
         |  type Aux[A, X0] = Optic[A] { type X = X0 }
         |  def second(a: Int): Aux[Int, "$cardinalityProjection1"] = ???
         |}
         |""".stripMargin
    ).find(_.name == "p.Optic.second").get
    reasons(found) must contain(
      "refined representation unavailable (constraints not erased): " +
        "Optic[Int] { type X = \"$cardinalityProjection1\" }"
    )
  }

  // A union's and a self type's reasons spell a refinement with its `;`, as the signature does.
  def refinedReasons = {
    val found = entries(
      """|package s
         |import ext.Ext
         |trait T { self: Ext {
         |    type X = Int
         |    type Y = Int
         |  } => def g(): Boolean }
         |object O {
         |  def u(x: Ext {
         |    type X = Int
         |    type Y = Int
         |  } | Boolean): Boolean = true
         |}
         |""".stripMargin
    ).map(entry => entry.name -> entry).toMap
    val refined = "Ext { type X = Int; type Y = Int }"
    (reasons(found("s.T.g")) must contain(s"self type not resolved: $refined"))
      .and(
        reasons(found("s.O.u")) must contain(
          s"union ($refined | Boolean): refined representation unavailable " +
            s"(constraints not erased): $refined"
        )
      )
  }

  def inferredResult = {
    val found = entries(
      """|package s
         |object O {
         |  def pick[A](
         |      x: A,
         |      y: A,
         |  ) = x
         |}
         |""".stripMargin
    ).find(_.name == "s.O.pick").get
    found.signature === "pick[A](x: A, y: A): <inferred>"
  }

  def separator =
    (Blockers.category("unresolved type: Schema (imported from org.apache.avro.Schema)") ===
      "unresolved type")
      .and(Blockers.category("mutable capture") === "mutable capture")
      .and(
        Blockers.category(
          "refined representation unavailable (constraints not erased): Optic[A] { type X: B }"
        ) === "refined representation unavailable (constraints not erased)"
      )

  // The cut that split `A <: S` at its colon read "bounded or higher-kinded parameter A <".
  def bounds =
    (Blockers.category("bounded or higher-kinded parameter A <: S: Type") ===
      "bounded or higher-kinded parameter _")
      .and(
        Blockers.category(
          "bounded higher-kinded parameter requires its bound's capabilities: " +
            "CC[X] <: SeqOps[X, CC, CC[X]]"
        ) === "bounded higher-kinded parameter requires its bound's capabilities"
      )
      .and(Blockers.category("evidence [A: Ord] not found: A") === "evidence _ not found")

  // `::` and `+:` are names: their colons are no separator, so the subtype is one code word and the
  // two families tally as one kind; an `_` before a colon joins it to a name as well (`x_:`).
  def operatorColons =
    (List(
      "sealed subtype :: binds A to no slot of Lst",
      "sealed subtype Some2 binds A to no slot of Opt",
      "sealed subtype +: binds A to no slot of Lst"
    ).map(Blockers.category).distinct === List("sealed subtype _ binds _ to no slot of _"))
      .and(Blockers.category("bound ≤: of A: B") === "bound _ of _")

  // After a name ending in `_`, a colon is the separator the producer wrote: `Optional_: x` is the
  // name `Optional_` and a detail, not the name `Optional_:`.
  def underscoreColons =
    (Blockers.category("sealed subtype Fused binds M0 to no slot of Optional_: x") ===
      "sealed subtype _ binds _ to no slot of _")
      .and(
        Blockers.category("union (Boolean | Missing_): unresolved type: Missing_") ===
          "union _: unresolved type"
      )

  def instances = {
    // Producers name the subtype after the separator, so a lower-case one tallies with the rest.
    val subtypes = List(
      "sealed subtype binds a parameter to no slot of its parent: Fused (M0, of Optional)",
      "sealed subtype binds a parameter to no slot of its parent: cons (A, of lst)"
    ).map(Blockers.category).distinct
    // A lower-case namespace (`cats`, `zio`) follows the separator, so it tallies once with a path.
    val shadowed = List(
      "builtin name Any may be shadowed through a reference not supplied: " +
        "scala.jdk.CollectionConverters (wildcard import of scala.jdk.CollectionConverters)",
      "builtin name Int may be shadowed through a reference not supplied: cats " +
        "(wildcard import of cats)",
      "builtin name Any may be shadowed through a reference not supplied: zio (wildcard import of zio)"
    ).map(Blockers.category).distinct
    (subtypes === List("sealed subtype binds a parameter to no slot of its parent"))
      .and(
        shadowed === List("builtin name _ may be shadowed through a reference not supplied")
      )
      .and(
        Blockers.category(
          "recursion through a constructor the estimate does not read: K (through list)"
        ) ===
          "recursion through a constructor the estimate does not read"
      )
  }

  def unions =
    (Blockers.category(
      "union (Json | String): unresolved type: Json (imported from io.circe.Json)"
    ) ===
      "union _: unresolved type")
      .and(
        Blockers.category(
          "union (RecordLevel | Null): abstract type or method-valued representation: R"
        ) ===
          "union _: abstract type or method-valued representation"
      )

  // The union is code however it is spelled — lower-case names, literals holding `: ` or a bracket,
  // a refinement's or a match type's keywords — so unions that stopped on one reason tally once,
  // read from sources as from reasons. A case class with a body that no `final` closes is the
  // alternative that stops (an abstract class with a method reads as a capability record and an
  // empty open trait as one value, either leaving the union to the overlap proof).
  def unionAlternatives = {
    val why = "case class open to extension"
    val written = List(
      "json | Missing",
      "Data | Missing",
      "\"a: b\" | Missing",
      "\"(\" | Missing",
      "Ext { type X = Int; type Y = Int } | Boolean",
      "T match { case Int => String } | Null",
      "F[A] { def run: Int } | Null",
      "A with B | Null",
      "t | Null"
    ).map(union => Blockers.category(s"union ($union): $why: x")).distinct
    val code =
      """|package ul
         |case class json(n: Int) { def run: Int = n }
         |case class Data(n: Int) { def run: Int = n }
         |trait Missing
         |object L {
         |  def f(x: json | Missing): Boolean = true
         |  def g(x: Data | Missing): Boolean = true
         |  def h(x: ((y: Int) => Int) | Missing): Boolean = true
         |}
         |""".stripMargin
    val rendered = Report(List(Report.Source("Example.scala", Nil, entries(code))), Nil).render
    (written === List(s"union _: $why"))
      .and(rendered must contain(s"  2 unresolved on: union _: $why\n"))
      .and(rendered must contain("  1 unresolved on: union _: unsupported type\n"))
  }

  def prose =
    (Blockers.category("Elimination of an opaque sum-producing callable is unsupported") ===
      "Elimination of an opaque sum-producing callable is unsupported")
      .and(
        Blockers.category(
          "inherited type member not resolved: Int (unsupplied parent InputStream)"
        ) ===
          "inherited type member not resolved"
      )
      .and(
        Blockers.category(
          "inherited declaration implemented, not modelled as a capability: pick (implemented in Q)"
        ) === "inherited declaration implemented, not modelled as a capability"
      )

  def tally = {
    def blocked(name: String, reason: String) =
      MethodAnalysis.Entry(
        "p/A.scala",
        name,
        s"$name: Unit",
        1,
        "method",
        Count.Unresolved(List(reason)),
        Nil
      )
    val report = Report(
      List(
        Report.Source(
          "p/A.scala",
          Nil,
          List(
            blocked("p.a", "sealed subtype binds a parameter to no slot of its parent: Fused"),
            blocked("p.b", "sealed subtype binds a parameter to no slot of its parent: Hit")
          )
        )
      ),
      Nil
    )
    report.render must contain(
      "  2 unresolved on: sealed subtype binds a parameter to no slot of its parent"
    )
  }

  private def located(paths: String*): String =
    Report(
      paths.toList.map(path => Report.Source(path, Counter.definitions(parse("object O")))),
      Nil
    ).render

  def sameName = {
    val rendered = located("root/core/eo/package.scala", "root/avro/eo/avro/package.scala")
    (rendered must contain("core/eo/package.scala:1"))
      .and(rendered must contain("avro/eo/avro/package.scala:1"))
      .and(rendered must not(contain("  package.scala:1")))
  }

  def namedInputs =
    located("/home/someone/checkout/core/A.scala", "/home/someone/checkout/avro/B.scala") ===
      located("core/A.scala", "avro/B.scala")

  def singleSource =
    (located("/tmp/sources/p/Only.scala") must contain("  Only.scala:1"))
      .and(Locations.shared(List("dev/x/A.scala", "dev/x/B.scala"))("dev/x/B.scala") === "B.scala")

  private val scratch = Files.createTempDirectory("report-hygiene")

  def afterAll(): Unit =
    Files.walk(scratch).sorted(Comparator.reverseOrder()).forEach(path => Files.delete(path))

  private def written(path: Path, code: String): Path = {
    val _ = Files.createDirectories(path.getParent)
    Files.writeString(path, code, UTF_8)
  }

  // A sources jar and a directory read together: neither the jar's entry nor the directory's file
  // carries the absolute path of the machine that ran the report, and a directory's files read
  // relative to it — `q/D.scala`, though `q` is the only package it holds.
  def mixedInputs = {
    val jar = scratch.resolve("lib-sources.jar")
    val archive = new ZipOutputStream(new FileOutputStream(jar.toFile))
    try {
      archive.putNextEntry(new ZipEntry("p/J.scala"))
      archive.write("package p\nfinal case class J(on: Boolean)\n".getBytes(UTF_8))
      archive.closeEntry()
    } finally archive.close()
    val directory = scratch.resolve("dir")
    val _ = written(directory.resolve("q/D.scala"), "package q\nfinal case class D(on: Boolean)\n")
    val mixed = Report.of(List(jar, directory)).render
    val alone = Report.of(List(directory)).render
    (mixed must contain("  lib-sources.jar!/p/J.scala:2"))
      .and(mixed must contain("  q/D.scala:2"))
      .and(mixed must not(contain(scratch.toString)))
      .and(alone must contain("  q/D.scala:2"))
      .and(Report.of(List(jar)).render must contain("  p/J.scala:2"))
  }

  // An input that cannot be read is placed as its rows would be, and the line names no absolute
  // path: not the directory's, and not the one a missing file's exception carries.
  def failedInputs = {
    val directory = scratch.resolve("err")
    val _ = written(directory.resolve("Ok.scala"), "package e\nobject Ok\n")
    val _ = written(directory.resolve("Bad.scala"), "package e\nobject Bad { def : }\n")
    val rendered = Report.of(List(directory, scratch.resolve("absent/Gone.scala"))).render
    (rendered must contain("  could not read err/Bad.scala: "))
      .and(rendered must contain("  could not read absent/Gone.scala: NoSuchFileException"))
      .and(rendered must contain("  err/Ok.scala:2"))
      .and(rendered must not(contain(scratch.toString)))
  }

  private def row(code: String, name: String): String =
    Report(List(Report.Source("A.scala", Counter.definitions(parse(code)))), Nil).definitionRows
      .find(_.definition.name == name)
      .get
      .size

  // `Int` times `Option[Boolean]` is 3·2^32 values, which the capacity algebra keeps as a width of
  // 34 bits: at most 2^34, and the row says so instead of printing 2^34 as if it were the count.
  // `Option[Option[Byte]]` holds 258 values, rounded up twice to a width of 10: printed as `2^10`
  // it would claim four times the count. A floating-point width rounds the same way:
  // `Option[Option[Double]]` holds at most 2^64 + 2 values, not 2^66.
  def capacityRow =
    (row("case class Wide(n: Int, flag: Option[Boolean])", "Wide") === "≤2^34")
      .and(row("case class Twice(o: Option[Option[Byte]])", "Twice") === "≤2^10")
      .and(row("case class Lossy(d: Double)", "Lossy") === "≤2^64 (lossy)")
      .and(row("case class TwiceD(o: Option[Option[Double]])", "TwiceD") === "≤2^66 (lossy)")
      .and((EffectiveOmega + FiniteSize(33)).render === "ω + ≤2^33")

  def exactRow = row("case class Two(a: Boolean, b: Option[Boolean])", "Two") === "6"

}
