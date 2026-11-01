package cardinality.reporting

import java.nio.file.{Files, Path}

/** Where a report row, or a line about a source it could not read, says the source is. Nothing
  * absolute or machine-specific is printed, and how the inputs were named — absolute, or relative
  * to a checkout — does not move a row.
  */
private[reporting] object Locations {

  /** Each path below the deepest directory holding every one of `paths`. Two sources never share a
    * location (`package.scala` in two packages), and one source prints its file name.
    */
  def shared(paths: List[String]): String => String = {
    val depth = sharedDepth(paths.map(segments(_).dropRight(1)))
    path => segments(path).drop(depth).mkString("/")
  }

  /** Where `Report.of` places each input and the sources it held: a file on disk below the deepest
    * directory holding every disk input (a directory input, a loose file's directory), so a
    * directory's files read alike whatever else is read; a jar entry by its entry name, after the
    * jar's file name when the report reads anything else.
    */
  def placed(read: List[(Path, List[String])]): Map[String, String] = {
    val (jars, disk) = read.partition((input, _) => jar(input) && !Files.isDirectory(input))
    val depth = sharedDepth(disk.map((input, _) => absolute(directoryOf(input))))
    def below(name: String): String = {
      val path = absolute(Path.of(name))
      if (path.size > depth) path.drop(depth).mkString("/") else path.lastOption.getOrElse(name)
    }
    val onDisk = disk.flatMap((input, names) => (input.toString :: names).map(n => n -> below(n)))
    val inJars = jars.flatMap { (archive, names) =>
      val prefix = if (read.size == 1) "" else s"${archive.getFileName}!/"
      (archive.toString -> archive.getFileName.toString) :: names.map(name =>
        name -> s"$prefix$name"
      )
    }
    (onDisk ++ inJars).toMap
  }

  def jar(path: Path): Boolean = {
    val name = path.toString
    name.endsWith(".jar") || name.endsWith(".zip")
  }

  // The directory an input stands for: itself, or the directory holding a loose file.
  private def directoryOf(input: Path): Path =
    if (Files.isDirectory(input)) input else input.toAbsolutePath.normalize.getParent

  private def absolute(path: Path): List[String] = segments(path.toAbsolutePath.normalize.toString)

  // How many leading segments every path shares.
  private def sharedDepth(paths: List[List[String]]): Int =
    paths.reduceOption((a, b) => a.zip(b).takeWhile(_ == _).map(_._1)).fold(0)(_.size)

  // Jar entries are always named with `/`; a path read from disk may use either separator.
  private def segments(path: String): List[String] = path.split("[/\\\\]").toList

}
