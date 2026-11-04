package cardinality.tasty

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Writes the stubs a classpath file renders, for inspection:
  * `TastyDump <classpath-file> <output-directory> [jar-name-regex]`.
  */
object TastyDump {

  def main(args: Array[String]): Unit = {
    val classpath = Files
      .readAllLines(Path.of(args(0)), UTF_8)
      .toArray(Array.empty[String])
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Path.of(_))
    val selected = args.lift(2).getOrElse(".*")
    val stubs = TastyStubs.of(classpath, _.getFileName.toString.matches(selected))
    stubs.inputs.foreach { input =>
      val file = Path.of(args(1)).resolve(input.path)
      val _ = Files.createDirectories(file.getParent)
      val _ = Files.writeString(file, input.tree.pos.input.text, UTF_8)
    }
    stubs.errors.foreach(println)
    println(s"${stubs.inputs.size} stubs, ${stubs.errors.size} not parsed")
  }

}
