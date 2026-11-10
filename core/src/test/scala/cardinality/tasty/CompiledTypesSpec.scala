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

    "keep a transparent inline result refused" in {
      row(compiled, "Transparent.t") must startWith("?")
    }
  }

}
