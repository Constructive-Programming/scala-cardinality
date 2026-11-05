package cardinality.capacity

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.Report
import java.nio.file.Files

/** How the name-resolution specs read a source set: each stored-value row as a reader sees it, and
  * one constructor's method-side count.
  */
private[capacity] object Rows {

  /** A row's number, `—` for an abstraction, or the class of its `?` with the reasons it names. */
  def verdict(d: Definition): String =
    if (d.unbound.isEmpty) d.size.fold("—")(_.render)
    else {
      val kind = Definition.gap(d) match {
        case Some(Definition.Gap.Unresolved) => "unresolved"
        case Some(Definition.Gap.Open)       => "open"
        case _                               => "instantiation"
      }
      d.unbound.map(_.render).mkString(s"$kind[", ", ", "]")
    }

  private def table(definitions: List[Definition]): Map[(String, Definition.Kind), String] =
    definitions.map(d => (d.name, d.kind) -> verdict(d)).toMap

  /** One source's rows, read alone. */
  def rows(code: String): Map[(String, Definition.Kind), String] =
    table(Counter.definitions(dialects.Scala3(code).parse[Source].get))

  /** A type's size, as the row of an alias to it reads it. */
  def sizeOf(tpe: String): Size =
    Counter.definitions(dialects.Scala3(s"type Measured_ = $tpe").parse[Source].get).head.size.get

  /** What a source's classes, enums, modules and top-level values hold together: their rows summed.
    */
  def held(code: String): Size =
    Counter
      .definitions(dialects.Scala3(code).parse[Source].get)
      .filter(d => Held(d.kind))
      .flatMap(_.size)
      .foldLeft(NothingSize: Size)(_ + _)

  private val Held = {
    import Definition.Kind.*
    Set(Class, Enum, Object, Value)
  }

  /** The rows of a source set laid out as files, read as the report reads them. */
  def rowsOf(files: (String, String)*): Map[(String, Definition.Kind), String] = {
    val root = Files.createTempDirectory("cardinality")
    files.foreach {
      case (name, content) =>
        val file = root.resolve(name)
        Files.createDirectories(file.getParent)
        Files.writeString(file, content)
    }
    table(Report.of(List(root)).definitions)
  }

  /** The method side's count for the signature `name` of a source set. */
  def constructor(name: String, files: (String, String)*): Count =
    MethodAnalysis
      .analyze(files.toList.map { (path, text) =>
        MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
      })
      .find(_.name == name)
      .map(_.count)
      .getOrElse(throw new AssertionError(s"no signature $name"))

}
