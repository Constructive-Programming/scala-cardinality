package cardinality.reporting

import cardinality.tasty.CompiledTypes
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.specs2.Specification

/** A library's sources jar given without its binary jar (`cardinalityReportOf` in either plugin).
  */
class SourcesJarSpec extends Specification {

  def is = s2"""
    A sources jar without its classes is one hint at the jar, not one error per source $alone
    A source file without classes keeps its own error                                   $file
    Beside compiled classes, a sources jar's source without classes keeps its own error $beside
  """

  private val uncompiled = Report.Environment(Some(CompiledTypes.Classes(Nil)))

  private def sourcesJar() = {
    val jar = Files.createTempFile(cardinality.Compiled.temporary("sources"), "lib", "-sources.jar")
    val out = new ZipOutputStream(Files.newOutputStream(jar))
    try
      List("p/A.scala" -> "package p\nclass A", "p/B.scala" -> "package p\nclass B").foreach {
        (name, text) =>
          out.putNextEntry(new ZipEntry(name))
          out.write(text.getBytes(UTF_8))
          out.closeEntry()
      }
    finally out.close()
    jar
  }

  def alone = {
    val jar = sourcesJar()
    Report.of(List(jar), uncompiled).errors === List(
      Report.Error(
        jar.toString,
        "no compiled classes for its sources: give the library's binary jar beside its sources jar"
      )
    )
  }

  def beside = {
    val output = Report.Environment(
      Some(CompiledTypes.Classes(List(cardinality.Compiled.temporary("classes"))))
    )
    Report.of(List(sourcesJar()), output).errors.map(_.message) ===
      List.fill(2)("no compiled classes for this source")
  }

  def file = {
    val source = cardinality.Compiled.temporary("sources").resolve("A.scala")
    Files.writeString(source, "class A")
    Report.of(List(source), uncompiled).errors.map(_.message) ===
      List("no compiled classes for this source")
  }

}
