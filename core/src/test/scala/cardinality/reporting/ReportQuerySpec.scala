package cardinality.reporting

import scala.util.Using

import cardinality.analysis.inhabitation.*
import cardinality.request.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.specs2.Specification

class ReportQuerySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Bounded file report queries
      classify target and support roots without adding support rows       $roots
      reject incomplete support input without returning numeric rows      $incomplete
      keep identical archive entry paths distinct across two jars          $archives
      include content, selection and ingestion limits in the request key   $keys
      retain the existing report API and render query-only provenance      $legacy
      count non-Scala directory entries and bound compressed archives      $bounds
  """

  private def directory(): Path = Files.createTempDirectory("query-report")

  private def file(root: Path, name: String, text: String): Path =
    Files.writeString(root.resolve(name), text)

  private def jar(root: Path, name: String, text: String): Path = {
    val path = root.resolve(name)
    Using.resource(new ZipOutputStream(Files.newOutputStream(path))) { zip =>
      zip.putNextEntry(new ZipEntry("Same.scala"))
      zip.write(text.getBytes(UTF_8))
      zip.closeEntry()
    }
    path
  }

  def roots = {
    val root = directory()
    val main = file(root, "Main.scala", "package p; def get[A](a: Box[A]): A = a.value")
    val support = file(root, "Support.scala", "package p; case class Box[A](value: A)")
    val found = Report.query(Report.Query(Seq(main), Seq(support), Set("p.get")))
    (found.report.errors === Nil)
      .and(found.report.methods.map(_.name) === List("p.get"))
      .and(found.report.methods.head.count === Count.Finite(1))
      .and(found.report.sources.size === 1)
      .and(found.report.definitions === Nil)
  }

  def incomplete = {
    val root = directory()
    val main = file(root, "Main.scala", "def get[A](a: A): A = a")
    val broken = file(root, "Broken.scala", "this is not scala !")
    val parseError = Report.query(Report.Query(Seq(main), Seq(broken)))
    val missing = Report.query(Report.Query(Seq(main), Seq(root.resolve("missing.scala"))))
    val low = Report.query(
      Report.Query(Seq(main), captureLimits = SourceSnapshot.Limits(maxTotalBytes = 1))
    )
    List(parseError, missing, low).forall(r =>
      r.report.errors.nonEmpty && r.report.methods.isEmpty && r.requestKey.isEmpty
    ) must beTrue
  }

  def archives = {
    val root = directory()
    val first = jar(root, "First.jar", "package first; def get[A](a: A): A = a")
    val second = jar(root, "Second.jar", "package second; def get[A](a: A, b: A): A = a")
    val found = Report.query(Report.Query(Seq(first, second)))
    (found.report.errors === Nil)
      .and(found.report.sources.map(_.path).distinct.size === 2)
      .and(found.report.methods.map(_.count).toSet === Set(Count.Finite(1), Count.Finite(2)))
  }

  def keys = {
    val root = directory()
    val main = file(root, "Main.scala", "def get[A](a: A): A = a")
    val support = file(root, "Support.scala", "package other; type Flag = Boolean")
    val base = Report.Query(Seq(main), Seq(support))
    val before = Report.query(base)
    val reordered = Report.query(base.copy(support = Seq(support, support)))
    val limited = Report.query(base.copy(captureLimits = SourceSnapshot.Limits(maxFiles = 10)))
    val _ = file(root, "Support.scala", "package other; type Flag = Unit")
    val after = Report.query(base)
    (before.requestKey === reordered.requestKey)
      .and(before.requestKey must beSome)
      .and(before.requestKey must not(beEqualTo(limited.requestKey)))
      .and(before.requestKey must not(beEqualTo(after.requestKey)))
  }

  def legacy = {
    val root = directory()
    val main = file(root, "Main.scala", "def get[A](a: A): A = a")
    val old = Report.of(Seq(main))
    val query = Report.query(Report.Query(Seq(main)))
    (old.methods.map(_.count) === query.report.methods.map(_.count))
      .and(query.render must contain("no stored-value estimates"))
      .and(query.render must contain("persistent cache disabled"))
      .and(query.render must contain("work:"))
      .and(query.render must not(contain("Stored-value")))
  }

  def bounds = {
    val root = directory()
    val _ = file(root, "unrelated.txt", "not Scala")
    val archive = jar(root, "Small.jar", "def get[A](a: A): A = a")
    val directoryLimit =
      SourceSnapshot.capture(Seq(root), SourceSnapshot.Limits(maxDirectoryEntries = 1))
    val archiveLimit =
      SourceSnapshot.capture(Seq(archive), SourceSnapshot.Limits(maxArchiveBytes = 1))
    val rootLimit = SourceSnapshot.capture(Seq(root, archive), SourceSnapshot.Limits(maxRoots = 1))
    List(directoryLimit, archiveLimit, rootLimit).forall(_.isLeft) must beTrue
  }

}
