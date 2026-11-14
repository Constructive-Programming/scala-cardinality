package cardinality

import scala.jdk.CollectionConverters.*

import java.nio.file.{Files, Path}
import java.util.{List as JList, Optional}
import org.specs2.mutable.Specification

/** The JDK-typed entry point the build-tool plugins call reflectively. */
class EntrySpec extends Specification {

  private val code = "package p\ncase class Flag(on: Boolean)\ndef pick[A](x: A, y: A): A = x\n"

  // The source on disk, its compiled classes and their classpath, and where the report goes.
  private lazy val (source, output, classpath, out) = {
    val compiled = Compiled.classesOf("p/A.scala" -> code)
    val dir = Compiled.temporary("entry-spec")
    val file = Files.writeString(dir.resolve("A.scala"), code)
    (
      JList.of(file),
      Optional.of(compiled.output.asJava),
      compiled.classpath.asJava,
      dir.resolve("out").resolve("report.txt")
    )
  }

  private val uncompiled = Optional.empty[JList[Path]]

  "Entry.report" should {
    "write the report from the compiled classes, and refuse sources without classes" in {
      (Entry.report(source, output, classpath, out) must_== 0)
        .and(Files.readString(out) must contain("2  p.Flag.<init>"))
        .and(Entry.report(source, uncompiled, classpath, out) must_== 1)
    }
  }

  "Entry.selected" should {
    "write the selected report for a qualified method name" in {
      (Entry.selected(source, JList.of("p.pick"), output, classpath, out) must_== 0)
        .and(Files.readString(out) must contain("pick[A]"))
        .and(Files.readString(out) must not(contain("Flag")))
    }
  }

}
