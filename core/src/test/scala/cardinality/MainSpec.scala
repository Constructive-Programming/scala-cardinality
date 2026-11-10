package cardinality

import java.nio.file.{Files, Paths}
import org.specs2.mutable.Specification

/** The argument protocol the sbt 1 plugin forks `cardinality.Main` with. */
class MainSpec extends Specification {

  "Main.parse" should {
    "read every option and keep the rest as sources" in {
      Main.parse(
        List(
          "--out",
          "r.txt",
          "--classpath",
          "a.jar:b.jar",
          "--select",
          "x.f",
          "--support",
          "s",
          "A.scala"
        )
      ) must beRight(
        Main.Args(
          Some(Paths.get("r.txt")),
          Seq(Paths.get("a.jar"), Paths.get("b.jar")),
          Set("x.f"),
          Seq(Paths.get("s")),
          Seq(Paths.get("A.scala"))
        )
      )
    }
    "refuse a missing --out and an unknown or incomplete option" in {
      Main.parse(List("A.scala")) must beLeft
      Main.parse(List("--out", "r", "--bogus")) must beLeft
      Main.parse(List("--out")) must beLeft
    }
  }

  "Main.run" should {
    "report over the sources, and flag a source that does not parse" in {
      val dir = Files.createTempDirectory("main-spec")
      val good =
        Files.writeString(dir.resolve("Good.scala"), "package p\ncase class Flag(on: Boolean)\n")
      val bad = Files.writeString(dir.resolve("Bad.scala"), "class {")
      val (rendered, failed) = Main.run(Main.Args(Some(dir.resolve("r")), sources = Seq(good)))
      (rendered must contain("p.Flag"))
        .and(failed must beFalse)
        .and(Main.run(Main.Args(Some(dir.resolve("r")), sources = Seq(bad)))._2 must beTrue)
    }
  }

}
