package cardinality

import java.nio.file.{Files, Path}
import java.util.{List as JList, Map as JMap, Optional}
import org.specs2.mutable.Specification

/** The JDK-typed entry point the build-tool plugins call reflectively. */
class EntrySpec extends Specification {

  private val none: JList[Path] = JList.of()
  private val unlimited: JMap[String, java.lang.Long] = JMap.of()

  private def source(text: String): (Path, Path) = {
    val dir = Files.createTempDirectory("entry-spec")
    (Files.writeString(dir.resolve("A.scala"), text), dir.resolve("out").resolve("report.txt"))
  }

  "Entry.report" should {
    "write the report and count a source that does not parse" in {
      val (good, out) = source("package p\ncase class Flag(on: Boolean)\n")
      val (bad, _) = source("class {")
      (Entry.report(JList.of(good), Optional.empty, out) must_== 0)
        .and(Files.readString(out) must contain("p.Flag"))
        .and(Entry.report(JList.of(bad), Optional.empty, out) must_== 1)
    }
    "read dependency declarations from the stub file Entry.stubs renders" in {
      val (code, out) = source("def size(xs: java.util.List[Boolean]): Int = xs.size\n")
      val stubs = out.resolveSibling("stubs.bin")
      Entry.stubs(none, stubs)
      (Entry.report(JList.of(code), Optional.of(stubs), out) must_== 0)
        .and(Files.exists(stubs) must beTrue)
    }
  }

  "Entry.selected" should {
    val pick = "package p\ndef pick[A](x: A, y: A): A = x\n"
    "write the selected report for a qualified method name" in {
      val (code, out) = source(pick)
      (Entry.selected(JList.of(code), none, JList.of("p.pick"), Optional.empty, unlimited, out)
        must_== 0).and(Files.readString(out) must contain("pick[A]"))
    }
    "apply a limit by name: no request work exhausts the budget" in {
      val (code, out) = source(pick)
      val _ = Entry.selected(
        JList.of(code),
        none,
        JList.of("p.pick"),
        Optional.empty,
        JMap.of("maxRequestWork", 1L),
        out
      )
      Files.readString(out) must contain("budget exhausted")
    }
    "refuse a limit no limit type has" in {
      val (code, out) = source(pick)
      Entry.selected(
        JList.of(code),
        none,
        JList.of("p.pick"),
        Optional.empty,
        JMap.of("maxBogus", 1L),
        out
      ) must throwAn[IllegalArgumentException](message = "unknown limit\\(s\\) maxBogus")
    }
  }

}
