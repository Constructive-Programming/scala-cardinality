package cardinality.reporting

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.tasty.{StubFile, TastyStubs}
import java.nio.file.{Files, Path}
import org.specs2.mutable.Specification

/** A report reads its sources' dependency declarations from compiled TASTy (cats-kernel, on the
  * test classpath): a name the sources alone leave unsupplied gets a number, the stubs are never
  * rows, and without a classpath the report is the sources-only one.
  */
class ReportDependenciesSpec extends Specification {

  private def jarOf(c: Class[?]): Path =
    Path.of(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  private val classpath =
    List(classOf[Option[?]], classOf[scala.deriving.Mirror], classOf[cats.kernel.Comparison])
      .map(jarOf)
      .distinct

  private lazy val source: Path = {
    val root = Files.createTempDirectory("report-dependencies")
    Files.writeString(
      root.resolve("Main.scala"),
      """package t
        |import cats.kernel.Comparison
        |def pick(c: Comparison): Comparison = ???
        |case class Held(c: Comparison)
        |""".stripMargin
    )
  }

  private lazy val sourcesOnly = Report.of(List(source))
  private lazy val withDependencies = Report.of(List(source), classpath)

  private def count(report: Report, name: String): Count =
    report.methods.find(_.name == name).get.count

  "without dependencies the compiled name is not supplied" in {
    count(sourcesOnly, "t.pick") must beLike {
      case u: Count.Unresolved => u.reasons.mkString must contain("not supplied")
    }
  }

  "the classpath supplies the declaration: three cases, 3^3 implementations" in {
    (count(withDependencies, "t.pick") must_== Count.Finite(27))
      .and(count(withDependencies, "t.Held.<init>") must_== Count.Finite(3))
  }

  "stubs are environment, never rows, and parse" in {
    (withDependencies.sources.map(_.path) must_== List(source.toString))
      .and(withDependencies.warnings must beEmpty)
  }

  "a stub file written once reads back as the classpath it was rendered from" in {
    val file = Files.createTempDirectory("stub-file").resolve("stubs.bin")
    val rendered = TastyStubs.render(classpath, _ => true)
    StubFile.write(rendered, file)
    (StubFile.read(file) must_== rendered)
      .and(Report.of(List(source), stubs = Some(file)).render must_== withDependencies.render)
  }

  "a stub file of another format holds nothing, and says why" in {
    val file = Files.createTempFile("stubs", ".bin")
    val _ = Files.write(file, Array[Byte](0, 4, 'o', 'l', 'd', '!'))
    val read = StubFile.read(file)
    (read.stubs must beEmpty).and(read.errors.mkString must contain("not cardinality-stubs/1"))
  }

  "an empty classpath is the sources-only report" in {
    Report.of(List(source), Nil).render must_== sourcesOnly.render
  }

  "the Scala library alone renders no stub, so the report is unchanged" in {
    val library = classpath.filterNot(_ == jarOf(classOf[cats.kernel.Comparison]))
    Report.of(List(source), library).render must_== sourcesOnly.render
  }

  "an unreadable classpath is a warning, not a failure" in {
    val corrupt = Files.writeString(source.resolveSibling("corrupt.jar"), "not a jar")
    val report = Report.of(List(source), corrupt :: classpath)
    (report.errors must beEmpty).and(report.render must contain("dependency stub not read"))
  }

  "a selected query reads the same declarations" in {
    val found = Report.query(Report.Query(List(source), classpath = classpath))
    found.report.methods.find(_.name == "t.pick").map(_.count) must beSome(Count.Finite(27))
  }

}
