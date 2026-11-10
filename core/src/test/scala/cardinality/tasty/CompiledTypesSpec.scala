package cardinality.tasty

import cardinality.reporting.Report
import java.nio.file.{Files, Path}
import org.specs2.mutable.Specification

/** A member written without a result type reads the type the compiler wrote into the module's own
  * TASTy (inferredfixtures/Inferred.scala): the syntax-only reading refuses a tuple or a `map`, the
  * compiled one does not. A `transparent inline` result stays refused (Rule X).
  */
class CompiledTypesSpec extends Specification {

  private def jarOf(c: Class[?]): Path =
    Path.of(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  private val source: Path = {
    val relative = Path.of("core/src/test/scala/inferredfixtures/Inferred.scala")
    Iterator
      .iterate(Path.of("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .get
  }

  private val classes = CompiledTypes.Classes(
    List(jarOf(inferredfixtures.Flags.getClass)),
    List(jarOf(classOf[Option[?]]), jarOf(classOf[scala.deriving.Mirror])).distinct
  )

  private def row(report: Report, name: String): String =
    report.render.linesIterator.find(_.contains(s"inferredfixtures.$name ")).getOrElse("")

  private lazy val compiled = Report.of(List(source), Report.Environment(classes = Some(classes)))
  private lazy val syntax = Report.of(List(source))

  "a member without a result type" should {
    "be refused by the syntax-only reading" in {
      (row(syntax, "Lists.markers") must startWith("?")).and(
        row(syntax, "Pairs.pair") must startWith("?")
      )
    }

    "read the compiled type: Boolean 2, (Boolean, Boolean) 4, List[String] ω, 3 one value" in {
      (row(compiled, "Flags.flag") must startWith("2 "))
        .and(row(compiled, "Pairs.pair") must startWith("4 "))
        .and(row(compiled, "Lists.markers") must startWith("ω "))
        .and(row(compiled, "Literal.k") must startWith("1 "))
        .and(compiled.warnings must beEmpty)
        .and(compiled.render must contain("val markers: _root_.scala.collection.immutable.List["))
    }

    "be left as written when the source no longer matches its compiled class" in {
      val edited =
        Files.createTempDirectory("stale").resolve("core/src/test/scala/inferredfixtures")
      Files.createDirectories(edited)
      val copy = edited.resolve("Inferred.scala")
      Files.writeString(copy, "\n" + Files.readString(source))
      row(
        Report.of(List(copy), Report.Environment(classes = Some(classes))),
        "Lists.markers"
      ) must startWith("?")
    }

    "type locals, anonymous members and pattern names: (Boolean, Boolean) 4, Boolean 2" in {
      (row(syntax, "Locals.outer.pick") must startWith("?"))
        .and(row(syntax, "Patterns.left") must startWith("?"))
        .and(row(compiled, "Locals.outer.pick") must startWith("4 "))
        .and(row(compiled, "Anonymous.flag.<new#1>.f") must contain("f: _root_.scala.Boolean"))
        .and(row(compiled, "Patterns.left") must startWith("2 "))
        .and(row(compiled, "Patterns.right") must startWith("4 "))
        .and(row(compiled, "Lambdas.both") must startWith("ω "))
    }

    "type a lambda's parameters: parenthesized in place, a bare one wrapped" in {
      val parsed = Spell.source(Files.readString(source)).get
      val input = cardinality.analysis.MethodAnalysis.Input(source.toString, parsed)
      val text = CompiledTypes.ascribe(List(input), classes)._1.map(_.tree.pos.input.text).mkString
      (text must contain("(x: _root_.scala.Boolean) => !x")).and(
        text must contain("(a: _root_.scala.Boolean, b: _root_.scala.Boolean) => a && b")
      )
    }

    "read a name typed in its pattern from the source too" in {
      val dir = Files.createTempDirectory("typed-pattern")
      val file = dir.resolve("P.scala")
      Files.writeString(
        file,
        "package p\nobject P { val (a: Boolean, b: (Boolean, Boolean)) = (true, (true, false)) }\n"
      )
      val report = Report.of(List(file))
      def at(name: String) =
        report.render.linesIterator.find(_.contains(s"p.P.$name ")).getOrElse("")
      (at("a") must startWith("2 ")).and(at("b") must startWith("4 "))
    }

    "keep a transparent inline result refused" in {
      row(compiled, "Transparent.t") must startWith("?")
    }
  }

}
