package cardinality

import scala.jdk.CollectionConverters.*

import java.nio.file.{Files, Path}
import java.util.{List as JList, Map as JMap, Optional}
import org.specs2.mutable.Specification

/** The JDK-typed entry point the build-tool plugins call reflectively. */
class EntrySpec extends Specification {

  private val code = "package p\ncase class Flag(on: Boolean)\ndef pick[A](x: A, y: A): A = x\n"

  // The source on disk at the path its TASTy records, its compiled classes and their classpath,
  // and where each example's report goes.
  private lazy val (source, output, dir) = {
    val compiled = Compiled.classesOf("p/A.scala" -> code)
    val dir = Compiled.temporary("entry-spec")
    val file = dir.resolve("p").resolve("A.scala")
    Files.createDirectories(file.getParent)
    (
      JList.of(Files.writeString(file, code)),
      Optional.of(JMap.entry(compiled.output.asJava, compiled.classpath.asJava)),
      dir
    )
  }

  private def out(example: String): Path = dir.resolve(example).resolve("report.txt")

  "Entry.report" should {
    "write the report from the compiled classes, and refuse sources without classes" in {
      val (compiled, uncompiled) = (out("report"), out("uncompiled"))
      (Entry.report(source, JList.of(), output, compiled) must_== 0)
        .and(Files.readString(compiled) must contain("2  p.Flag.<init>"))
        .and(Entry.report(source, JList.of(), Optional.empty, uncompiled) must_== 1)
    }
  }

  "Entry.report with names" should {
    "write the selected report for a qualified method name" in {
      val selected = out("selected")
      (Entry.report(source, JList.of("p.pick"), output, selected) must_== 0)
        .and(Files.readString(selected) must contain("pick[A]"))
        .and(Files.readString(selected) must not(contain("Flag")))
    }
  }

}
