package cardinality

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.zip.ZipFile

/** Local, bounded source acquisition, independent of parsing or analysis. No cache is persisted.
  *
  * Verification detects changes across two observations, not arbitrary edits after verification or
  * edits reverted between observations. Directory traversal and compressed archives have separate
  * limits: ZipFile loads central-directory metadata before its visited-entry budget is enforced.
  */
object SourceSnapshot {

  final case class Limits(
      maxFiles: Int = 10000,
      maxFileBytes: Long = 8L * 1024 * 1024,
      maxTotalBytes: Long = 64L * 1024 * 1024,
      maxArchiveEntries: Int = 100000,
      maxDirectoryEntries: Int = 100000,
      maxArchiveBytes: Long = 64L * 1024 * 1024,
      maxRoots: Int = 10000
  ) {
    require(
      maxFiles > 0 && maxFileBytes > 0 && maxTotalBytes > 0 && maxArchiveEntries > 0 &&
        maxDirectoryEntries > 0 && maxArchiveBytes > 0 && maxRoots > 0
    )
  }

  final case class Source(id: String, text: String, digest: String)
  final case class Snapshot(sources: List[Source], roots: Map[String, List[String]], digest: String)
  final case class Error(path: String, message: String)

  def rootId(path: Path): String =
    (if (Files.exists(path)) path.toRealPath() else path.toAbsolutePath.normalize()).toUri.toString

  def capture(paths: Seq[Path], limits: Limits = Limits()): Either[List[Error], Snapshot] =
    captureWithVerification(paths, limits, () => ())

  /** Test seam: invoked once after acquisition and before the independent verification pass. */
  private[cardinality] def captureWithVerification(
      paths: Seq[Path],
      limits: Limits,
      beforeVerification: () => Unit
  ): Either[List[Error], Snapshot] = {
    var current = ""
    try {
      val roots = paths.toList
      if (roots.size > limits.maxRoots)
        throw new IllegalArgumentException("maxRoots exceeded")
      val first = acquire(roots, limits, path => current = path)
      beforeVerification()
      val second =
        try acquire(roots, limits, path => current = path)
        catch {
          case NonFatal(error) =>
            throw new IllegalStateException(s"snapshot-changed: ${error.getMessage}", error)
        }
      if (first != second)
        Left(List(Error(current, "snapshot-changed: sources, membership, or generation changed")))
      else Right(first.snapshot)
    } catch {
      case NonFatal(error) =>
        Left(List(Error(current, Option(error.getMessage).getOrElse(error.toString))))
    }
  }

  private case class Acquisition(snapshot: Snapshot, generations: Map[String, String])

  private def acquire(
      paths: List[Path],
      limits: Limits,
      visiting: String => Unit
  ): Acquisition = {
    val sources = mutable.Map.empty[String, Source]
    val memberships = mutable.Map.empty[String, List[String]]
    val generations = mutable.Map.empty[String, String]
    var totalBytes = 0L
    var archiveEntries = 0L
    var directoryEntries = 0L

    def generation(path: Path): Unit = {
      val id = rootId(path)
      visiting(id)
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
      val value =
        s"${attrs.size()}:${attrs.lastModifiedTime()}:${attrs.creationTime()}:${attrs.fileKey()}"
      generations.get(id).foreach { previous =>
        if (previous != value)
          throw new IllegalStateException("snapshot-changed: generation changed")
      }
      generations(id) = value
    }

    def read(id: String, open: () => InputStream): Unit =
      if (!sources.contains(id)) {
        visiting(id)
        if (sources.size >= limits.maxFiles)
          throw new IllegalArgumentException("maxFiles exceeded")
        val bytes = Using.resource(open()) { stream =>
          val output = new ByteArrayOutputStream()
          val buffer = new Array[Byte](8192)
          var count = stream.read(buffer)
          var size = 0L
          while (count != -1) {
            if (count.toLong > limits.maxFileBytes - size)
              throw new IllegalArgumentException("maxFileBytes exceeded")
            if (count.toLong > limits.maxTotalBytes - totalBytes)
              throw new IllegalArgumentException("maxTotalBytes exceeded")
            size += count
            totalBytes += count
            output.write(buffer, 0, count)
            count = stream.read(buffer)
          }
          output.toByteArray
        }
        val text = StandardCharsets.UTF_8
          .newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
          .decode(ByteBuffer.wrap(bytes))
          .toString
        sources(id) = Source(id, text, hash(bytes))
      }

    def file(path: Path): String = {
      val id = rootId(path)
      generation(path)
      read(id, () => Files.newInputStream(path))
      generation(path)
      id
    }

    val canonical = paths
      .map { path =>
        visiting(path.toAbsolutePath.normalize().toUri.toString)
        val resolved =
          if (Files.exists(path)) path.toRealPath() else path.toAbsolutePath.normalize()
        rootId(resolved) -> resolved
      }
      .sortBy(_._1)
    canonical.foreach {
      case (id, path) =>
        visiting(id)
        if (!memberships.contains(id)) {
          if (!Files.exists(path)) throw new IllegalArgumentException("path does not exist")
          generation(path)
          val members =
            if (Files.isDirectory(path)) {
              // Do not follow directory symlinks. File symlinks share their real source identity.
              val files = Using.resource(Files.walk(path)) { walk =>
                val selected = mutable.Map.empty[String, Path]
                val iterator = walk.iterator().asScala
                iterator.foreach { member =>
                  directoryEntries += 1
                  if (directoryEntries > limits.maxDirectoryEntries)
                    throw new IllegalArgumentException("maxDirectoryEntries exceeded")
                  if (Files.isRegularFile(member) && member.toString.endsWith(".scala")) {
                    selected(rootId(member)) = member
                  }
                }
                selected.toList.sortBy(_._1).map { case (_, member) => file(member) }
              }
              files
            } else if (path.toString.endsWith(".jar") || path.toString.endsWith(".zip")) {
              if (Files.size(path) > limits.maxArchiveBytes)
                throw new IllegalArgumentException("maxArchiveBytes exceeded")
              Using.resource(new ZipFile(path.toFile)) { zip =>
                val selected = mutable.Set.empty[String]
                val manifest = mutable.ListBuffer.empty[String]
                zip.entries().asScala.foreach { entry =>
                  archiveEntries += 1
                  if (archiveEntries > limits.maxArchiveEntries)
                    throw new IllegalArgumentException("maxArchiveEntries exceeded")
                  manifest += s"${entry.getName}:${entry.getSize}:${entry.getCrc}:${entry.getMethod}"
                  if (!entry.isDirectory && entry.getName.endsWith(".scala")) {
                    val entryPath = new URI(null, null, "/" + entry.getName, null).toASCIIString
                    // Keep literal "!/" in either path distinct from the jar URI separator.
                    val origin = id.replace("!", "%21")
                    val memberId = s"jar:$origin!${entryPath.replace("!", "%21")}"
                    if (!selected.add(memberId))
                      throw new IllegalArgumentException("duplicate .scala archive entry name")
                    read(memberId, () => zip.getInputStream(entry))
                  }
                }
                generations(s"archive-manifest:$id") = framedHash(manifest.toList.sorted)
                selected.toList.sorted
              }
            } else if (Files.isRegularFile(path) && path.toString.endsWith(".scala"))
              List(file(path))
            else throw new IllegalArgumentException("expected Scala file, directory, jar, or zip")
          generation(path)
          memberships(id) = members
        }
    }
    val ordered = sources.values.toList.sortBy(_.id)
    val roots = memberships.toMap
    val frames = List("sources", ordered.size.toString) ++
      ordered.flatMap(source => List(source.id, source.digest)) ++
      List("roots", roots.size.toString) ++ roots.toList.sortBy(_._1).flatMap {
        case (id, members) => List(id, members.size.toString) ++ members
      }
    Acquisition(Snapshot(ordered, roots, framedHash(frames)), generations.toMap)
  }

  private def hash(bytes: Array[Byte]): String =
    hex(MessageDigest.getInstance("SHA-256").digest(bytes))

  private[cardinality] def framedHash(values: List[String]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    values.foreach { value =>
      val bytes = value.getBytes(StandardCharsets.UTF_8)
      digest.update(ByteBuffer.allocate(8).putLong(bytes.length.toLong).array())
      digest.update(bytes)
    }
    hex(digest.digest())
  }

  private def hex(bytes: Array[Byte]): String =
    bytes.map(byte => f"${byte & 0xff}%02x").mkString

}
