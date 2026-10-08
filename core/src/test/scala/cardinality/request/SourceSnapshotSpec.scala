package cardinality.request

import scala.jdk.CollectionConverters.*
import scala.util.Using

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.specs2.mutable.Specification

class SourceSnapshotSpec extends Specification {
  sequential

  private def temporary[A](run: Path => A): A = {
    val dir = Files.createTempDirectory("source-snapshot")
    try run(dir)
    finally
      Using.resource(Files.walk(dir)) { paths =>
        paths.iterator().asScala.toList.reverse.foreach(Files.delete)
      }
  }

  private def write(path: Path, text: String): Path =
    Files.write(path, text.getBytes(UTF_8))

  private def archive(path: Path, entries: List[(String, String)]): Path = {
    Using.resource(new ZipOutputStream(Files.newOutputStream(path))) { zip =>
      entries.foreach {
        case (name, text) =>
          zip.putNextEntry(new ZipEntry(name))
          zip.write(text.getBytes(UTF_8))
          zip.closeEntry()
      }
    }
    path
  }

  private def snapshot(paths: Path*): SourceSnapshot.Snapshot =
    SourceSnapshot.capture(paths).fold(errors => sys.error(errors.toString), identity)

  private def failure(paths: Seq[Path], limits: SourceSnapshot.Limits, message: String) =
    SourceSnapshot.capture(paths, limits).left.toOption.get.map(_.message).mkString must
      contain(message)

  "SourceSnapshot" should {
    "hash captured bytes and distinguish changed content at the same root" in temporary { dir =>
      val path = write(dir.resolve("A.scala"), "object A")
      val first = snapshot(path)
      val _ = write(path, "object B")
      val second = snapshot(path)
      (first.digest must not(beEqualTo(second.digest)))
        .and(first.sources.head.digest must not(beEqualTo(second.sources.head.digest)))
        .and(second.sources.head.text must beEqualTo("object B"))
    }

    "deduplicate sources and input roots without losing root classification" in temporary { dir =>
      val a = write(dir.resolve("A.scala"), "object A")
      val b = write(dir.resolve("B.scala"), "object B")
      val first = snapshot(dir, a, b)
      val second = snapshot(b, a, dir, a)
      (first must beEqualTo(second))
        .and(first.sources.map(_.id) must beEqualTo(List(a, b).map(SourceSnapshot.rootId).sorted))
        .and(first.roots(SourceSnapshot.rootId(a)) must beEqualTo(List(SourceSnapshot.rootId(a))))
        .and(first.roots(SourceSnapshot.rootId(dir)).size must beEqualTo(2))
        .and(snapshot(dir).digest must not(beEqualTo(first.digest)))
    }

    "include directory membership in the digest" in temporary { dir =>
      val _ = write(dir.resolve("A.scala"), "object A")
      val first = snapshot(dir)
      val _ = write(dir.resolve("B.scala"), "object B")
      snapshot(dir).digest must not(beEqualTo(first.digest))
    }

    "qualify matching jar entry paths by origin and support zip roots" in temporary { dir =>
      val a = archive(dir.resolve("a.jar"), List("pkg/A.scala" -> "object A"))
      val b = archive(dir.resolve("b.zip"), List("pkg/A.scala" -> "object A"))
      val result = snapshot(b, a)
      (result.sources.map(_.id).distinct.size must beEqualTo(2))
        .and(result.sources.map(_.digest).distinct.size must beEqualTo(1))
        .and(
          result.roots(SourceSnapshot.rootId(a)).head must startWith(
            "jar:" + SourceSnapshot.rootId(a)
          )
        )
    }

    "keep literal jar separators in origin and entry paths unambiguous" in temporary { dir =>
      val nested = Files.createDirectory(dir.resolve("a.jar!"))
      val a = archive(dir.resolve("a.jar"), List("b.jar!/A.scala" -> "object A"))
      val b = archive(nested.resolve("b.jar"), List("A.scala" -> "object B"))
      val result = snapshot(a, b)
      (result.sources.size must beEqualTo(2))
        .and(result.sources.map(_.id).distinct.size must beEqualTo(2))
        .and(result.sources.map(_.text).sorted must beEqualTo(List("object A", "object B")))
    }

    "reject duplicate Scala archive origins explicitly" in temporary { dir =>
      val zip = archive(
        dir.resolve("duplicate.zip"),
        List("A.scala" -> "object A", "B.scala" -> "object B")
      )
      // ZipOutputStream disallows duplicates. Rename both local and central-directory records,
      // keeping the name lengths, payloads and CRCs unchanged.
      val encoded = new String(Files.readAllBytes(zip), ISO_8859_1)
      val _ = Files.write(zip, encoded.replace("B.scala", "A.scala").getBytes(ISO_8859_1))
      failure(Seq(zip), SourceSnapshot.Limits(), "duplicate .scala archive entry name")
    }

    "bound decompressed archive bytes, including totals across entries" in temporary { dir =>
      val bomb = archive(dir.resolve("bomb.zip"), List("A.scala" -> ("a" * 128000)))
      val pair = archive(dir.resolve("pair.zip"), List("A.scala" -> "aaaa", "B.scala" -> "bbbb"))
      failure(Seq(bomb), SourceSnapshot.Limits(maxFileBytes = 1024), "maxFileBytes")
        .and(failure(Seq(pair), SourceSnapshot.Limits(maxTotalBytes = 7), "maxTotalBytes"))
        .and(failure(Seq(pair), SourceSnapshot.Limits(maxFiles = 1), "maxFiles"))
        .and(
          failure(
            Seq(bomb, pair),
            SourceSnapshot.Limits(maxArchiveEntries = 2),
            "maxArchiveEntries"
          )
        )
    }

    "hash exact UTF-8 bytes with SHA-256 and keep non-Scala directory files out" in temporary {
      dir =>
        val _ = write(dir.resolve("A.scala"), "abc")
        val _ = write(dir.resolve("ignored.txt"), "ignored")
        val result = snapshot(dir)
        (result.sources.size must beEqualTo(1)).and(
          result.sources.head.digest must beEqualTo(
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
          )
        )
    }

    "enforce individual, total, file count, and visited archive entry budgets" in temporary { dir =>
      val a = write(dir.resolve("A.scala"), "aaaa")
      val b = write(dir.resolve("B.scala"), "bbbb")
      val zip = archive(dir.resolve("a.zip"), List("ignored.txt" -> "", "A.scala" -> "aaaa"))
      failure(Seq(a), SourceSnapshot.Limits(maxFileBytes = 3), "maxFileBytes")
        .and(failure(Seq(zip), SourceSnapshot.Limits(maxFileBytes = 3), "maxFileBytes"))
        .and(failure(Seq(a, b), SourceSnapshot.Limits(maxTotalBytes = 7), "maxTotalBytes"))
        .and(failure(Seq(a, b), SourceSnapshot.Limits(maxFiles = 1), "maxFiles"))
        .and(failure(Seq(zip), SourceSnapshot.Limits(maxArchiveEntries = 1), "maxArchiveEntries"))
    }

    "reject missing roots and malformed UTF-8 rather than return partial sources" in temporary {
      dir =>
        val bad = Files.write(dir.resolve("Bad.scala"), Array[Byte](0xc3.toByte, 0x28))
        val good = write(dir.resolve("Good.scala"), "object Good")
        (SourceSnapshot.capture(Seq(good, bad)).isLeft must beTrue)
          .and(failure(Seq(dir.resolve("missing")), SourceSnapshot.Limits(), "does not exist"))
    }

    "detect byte drift even when size and last-modified time are restored" in temporary { dir =>
      val path = write(dir.resolve("A.scala"), "object A")
      val time = Files.getLastModifiedTime(path)
      val result = SourceSnapshot.captureWithVerification(
        Seq(path),
        SourceSnapshot.Limits(),
        () => {
          val _ = write(path, "object B")
          val _ = Files.setLastModifiedTime(path, time)
        }
      )
      result.left.toOption.get.head.message must contain("snapshot-changed")
    }

    "detect directory membership drift and archive generation drift" in temporary { dir =>
      val path = write(dir.resolve("A.scala"), "object A")
      val directory = SourceSnapshot.captureWithVerification(
        Seq(dir),
        SourceSnapshot.Limits(),
        () => { val _ = write(dir.resolve("B.scala"), "object B") }
      )
      val zip = archive(dir.resolve("a.jar"), List("A.scala" -> "object A"))
      val jar = SourceSnapshot.captureWithVerification(
        Seq(zip),
        SourceSnapshot.Limits(),
        () => { val _ = archive(zip, List("A.scala" -> "object B")) }
      )
      (directory.left.toOption.get.head.message must contain("snapshot-changed"))
        .and(jar.left.toOption.get.head.message must contain("snapshot-changed"))
        .and(snapshot(path).sources.size must beEqualTo(1))
    }

    "require positive bounds" in {
      (SourceSnapshot.Limits(maxFiles = 0) must throwA[IllegalArgumentException])
        .and(SourceSnapshot.Limits(maxFileBytes = 0) must throwA[IllegalArgumentException])
        .and(SourceSnapshot.Limits(maxTotalBytes = 0) must throwA[IllegalArgumentException])
        .and(SourceSnapshot.Limits(maxArchiveEntries = 0) must throwA[IllegalArgumentException])
    }
  }
}
