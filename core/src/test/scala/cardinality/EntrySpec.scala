package cardinality

import java.nio.file.{Files, Path}
import java.util.List as JList
import org.specs2.mutable.Specification

/** The JDK-typed entry point the sbt 1 plugin calls reflectively. */
class EntrySpec extends Specification {

  private val none: JList[Path] = JList.of()

  "Entry" should {
    "write the report and count a source that does not parse" in {
      val dir = Files.createTempDirectory("entry-spec")
      val good =
        Files.writeString(dir.resolve("Good.scala"), "package p\ncase class Flag(on: Boolean)\n")
      val bad = Files.writeString(dir.resolve("Bad.scala"), "class {")
      val out = dir.resolve("out").resolve("report.txt")
      (Entry.report(JList.of(good), none, out) must_== 0)
        .and(Files.readString(out) must contain("p.Flag"))
        .and(Entry.report(JList.of(bad), none, out) must_== 1)
    }
    "write the selected report for a qualified method name" in {
      val dir = Files.createTempDirectory("entry-spec")
      val source =
        Files.writeString(dir.resolve("A.scala"), "package p\ndef pick[A](x: A, y: A): A = x\n")
      val out = dir.resolve("report.txt")
      (Entry.selected(JList.of(source), none, JList.of("p.pick"), none, out) must_== 0)
        .and(Files.readString(out) must contain("pick[A]"))
    }
  }

}
