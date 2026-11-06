//> using scala 3.8.2

// Rank CPD duplicate groups by how often an idiom repeats, not by block size.
//
// Usage: scala-cli run cpd-idioms.scala -- cpd.csv [top]
// Native: scala-cli --power package --native cpd-idioms.scala -o cpd-idioms
// Input is `pmd cpd --format=csv` output. Groups led by `import`/`package` lines are dropped.
// Each row: occurrences, tokens, distinct files, first line of the snippet, first location.

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

final case class Group(occurrences: Int, tokens: Int, files: Int, text: String, at: String):
  def weight: Int = occurrences * tokens

@main def cpdIdioms(csv: String, top: Int = 40): Unit =
  val groups = Files.readAllLines(Path.of(csv)).asScala.toList.drop(1).flatMap { row =>
    val cells       = row.split(',')
    val occurrences = cells(2).toInt
    val locations   = List.tabulate(occurrences)(i => (cells(3 + 2 * i).toInt, cells(4 + 2 * i)))
    val (line, path) = locations.head
    val text         = Files.readAllLines(Path.of(path)).get(line - 1).trim
    Option.unless(text.startsWith("import") || text.startsWith("package"))(
      Group(occurrences, cells(1).toInt, locations.map(_._2).distinct.size, text.take(110), s"$path:$line")
    )
  }
  groups.sortBy(-_.weight).take(top).foreach { g =>
    println(f"${g.occurrences}%3dx ${g.tokens}%3dt ${g.files}%2df  ${g.text}%n           ${g.at}")
  }
