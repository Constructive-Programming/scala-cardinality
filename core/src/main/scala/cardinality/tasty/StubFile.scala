package cardinality.tasty

import scala.util.control.NonFatal

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.nio.file.{Files, Path}

/** Rendered stubs in one file, so a build tool can cache them by the jars' contents (the sbt
  * plugin's `cardinalityDependencyStubs` is an sbt 2 cached task whose output is this file).
  *
  * The file starts with `Format`; a renderer change that alters any stub must bump it, and so
  * invalidate every file written before. Records are length-prefixed UTF strings: the count of
  * stubs, each stub's path and text, then the count of errors and each error.
  */
object StubFile {

  /** The rendering's version: bump it whenever a change to the renderer can change a stub. */
  val Format = "cardinality-stubs/4"

  /** Renders a classpath's declarations into `file`; a classpath not read is the file's error. */
  def render(classpath: List[Path], file: Path): Unit = {
    val rendered =
      if (classpath.isEmpty) TastyStubs.Rendered(Nil)
      else
        try TastyStubs.render(classpath, _ => true)
        catch {
          case NonFatal(error) =>
            TastyStubs.Rendered(Nil, List(s"dependency classpath not read: $error"))
        }
    write(rendered, file)
  }

  /** Replaces the file: one a build cache restored may be a read-only link into its store. */
  def write(rendered: TastyStubs.Rendered, file: Path): Unit = {
    val _ = Files.createDirectories(file.toAbsolutePath.getParent)
    val _ = Files.deleteIfExists(file)
    val out = new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(file)))
    try {
      out.writeUTF(Format)
      out.writeInt(rendered.stubs.size)
      rendered.stubs.foreach { (path, text) =>
        out.writeUTF(path)
        long(out, text)
      }
      out.writeInt(rendered.errors.size)
      rendered.errors.foreach(long(out, _))
    } finally out.close()
  }

  /** The stubs a file holds; a file of another format or a damaged one holds none, and says so. */
  def read(file: Path): TastyStubs.Rendered =
    try {
      val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(file)))
      try {
        val format = in.readUTF()
        if (format != Format)
          TastyStubs.Rendered(
            Nil,
            List(s"dependency stubs not read: $file is $format, not $Format")
          )
        else {
          val stubs = List.fill(in.readInt())(in.readUTF() -> long(in))
          TastyStubs.Rendered(stubs, List.fill(in.readInt())(long(in)))
        }
      } finally in.close()
    } catch {
      case NonFatal(error) => TastyStubs.Rendered(Nil, List(s"dependency stubs not read: $error"))
    }

  // A string of any length (writeUTF stops at 64 KiB).
  private def long(out: DataOutputStream, text: String): Unit = {
    val bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    out.writeInt(bytes.length)
    out.write(bytes)
  }

  private def long(in: DataInputStream): String =
    new String(in.readNBytes(in.readInt()), java.nio.charset.StandardCharsets.UTF_8)

}
