package cardinality

import scala.jdk.CollectionConverters.*
import scala.meta.{Source as ScalaSource, *}

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

/** A cardinality report over a set of Scala sources: what each source defines, how many values
  * every definition holds, and — for the ones the calculator could not bound — which type names
  * stopped it.
  *
  * The sources can be a project's, or the published `-sources.jar` of a library, which is what
  * makes a dependency's types measurable from outside.
  */
final case class Report(sources: List[Report.Source], errors: List[Report.Error]) {

  def definitions: List[Definition] = sources.flatMap(_.definitions)

  /** The report as it reads: what was measured, one line per definition ordered by how many values
    * it holds, and the sources that could not be read.
    */
  def render: String = {
    val rows = sources
      .flatMap(source => source.definitions.map(definition => (source, definition)))
      .sortBy(row => Report.order(row._2))
      .map(Report.row)
    val columns = List(
      rows.map(_._1.length).maxOption.getOrElse(0),
      rows.map(_._2.length).maxOption.getOrElse(0),
      rows.map(_._3.length).maxOption.getOrElse(0),
    )
    val table = rows.map {
      case (size, name, kind, location, unbounded) =>
        val row =
          s"${Report.padded(size, columns(0))}  ${Report.padded(name, columns(1))}  ${Report.padded(kind, columns(2))}  $location"
        if (unbounded.isEmpty) row else s"$row  unbounded by: $unbounded"
    }
    (Report.summary(this) ++ errors.map(Report.failed) ++ (if (table.isEmpty) Nil else "" :: table))
      .mkString("\n")
  }

}

object Report {

  /** One Scala source the report read: where it was read from — a file, or an entry of a sources
    * jar — and the definitions it introduces.
    */
  final case class Source(path: String, definitions: List[Definition])

  /** A source the report could not read or parse, and what went wrong. */
  final case class Error(path: String, message: String)

  /** Reads every Scala source under `paths` — a file, a directory walked recursively, or a jar such
    * as the `-sources.jar` a library publishes — and measures the definitions it finds.
    */
  def of(paths: Seq[Path]): Report = {
    val parsed = paths.toList.distinct.flatMap { path =>
      read(path) match {
        case Left(error)  => List((None, Some(error)))
        case Right(found) => found.map(parse)
      }
    }
    Report(
      parsed.collect { case (Some(source), _) => source },
      parsed.collect { case (_, Some(error)) => error }
    )
  }

  // Parses one source, and reports either what it defines or what stopped the parser.
  private def parse(source: (String, String)): (Option[Source], Option[Error]) = {
    val (path, text) = source
    dialects.Scala3(text).parse[ScalaSource].toEither match {
      case Right(tree) => (Some(Source(path, Counter.definitions(tree))), None)
      case Left(error) =>
        (None, Some(Error(path, s"${error.message} (line ${error.pos.startLine + 1})")))
    }
  }

  // The Scala sources one path holds: a directory is walked recursively, a jar is read as the
  // sources jar a library publishes, and any other path is a single source file. A path that
  // cannot be read at all is reported rather than thrown, so one bad root does not lose the rest.
  private def read(path: Path): Either[Error, List[(String, String)]] =
    try
      Right(
        if (Files.isDirectory(path)) walk(path).map(file => (file.toString, Files.readString(file)))
        else if (isJar(path)) entries(path)
        else List((path.toString, Files.readString(path))),
      )
    catch
      case e: IOException =>
        val message = Option(e.getMessage).filter(_.nonEmpty).getOrElse(path.toString)
        Left(Error(path.toString, s"${e.getClass.getSimpleName}: $message"))

  private def walk(directory: Path): List[Path] = {
    val found = Files.walk(directory)
    try found.iterator().asScala.filter(isScala).toList.sortBy(_.toString)
    finally found.close()
  }

  private def entries(jar: Path): List[(String, String)] = {
    val archive = new ZipFile(jar.toFile)
    try
      archive
        .entries()
        .asScala
        .filter(entry => !entry.isDirectory && isScala(entry.getName))
        .toList
        .sortBy(_.getName)
        .map(entry =>
          (entry.getName, new String(archive.getInputStream(entry).readAllBytes(), UTF_8))
        )
    finally archive.close()
  }

  private def isJar(path: Path): Boolean = {
    val name = path.toString
    name.endsWith(".jar") || name.endsWith(".zip")
  }

  private def isScala(path: Path): Boolean = isScala(path.toString)

  private def isScala(name: String): Boolean = name.endsWith(".scala")

  // What was measured: how many sources, how many definitions, how those fall out by size, and
  // how many of the unbounded ones are generic — a definition whose own type parameters are among
  // the names the calculator could not bound has a size that depends on what it is instantiated
  // with, which is the usual reason a library's types are unbounded.
  private def summary(report: Report): List[String] = {
    val definitions = report.definitions
    val counts = SizeClass.order.flatMap { sizeClass =>
      val count = definitions.count(definition => SizeClass(definition.size) == sizeClass)
      if (count == 0) Nil else List(s"$count ${sizeClass.label}")
    }
    val generic = definitions.count(genericUnbounded)
    List(
      s"scala-cardinality — ${plural(report.sources.size, "source")}, ${plural(definitions.size, "definition")}",
      "  sizes: exact up to 1024, 2^n above, ω countable, τ uncountable",
    ) ++
      (if (counts.isEmpty) Nil else List(s"  ${counts.mkString(" · ")}")) ++
      (if (generic == 0) Nil
       else List(s"  generic: $generic of the unbounded depend on their own type parameters"))
  }

  private def plural(count: Int, noun: String): String =
    s"$count $noun${if (count == 1) "" else "s"}"

  private def genericUnbounded(definition: Definition): Boolean =
    SizeClass(definition.size) == SizeClass.Unbounded && definition.params.exists(
      definition.unresolved.contains
    )

  private def failed(error: Error): String = s"  could not read ${error.path}: ${error.message}"

  private def row(entry: (Source, Definition)): (String, String, String, String, String) = {
    val (source, definition) = entry
    (
      definition.size.fold("—")(_.render),
      Definition.signature(definition),
      definition.kind.toString.toLowerCase,
      s"${fileName(source.path)}:${definition.line}",
      definition.unresolved.mkString(", "),
    )
  }

  private def padded(text: String, width: Int): String = text + " " * (width - text.length)

  // Jar entries are always named with `/`; a path read from disk may use either separator.
  private def fileName(path: String): String = path.split("[/\\\\]").last

  // The order the report reads in: the unbounded definitions first, then the finite sizes from the
  // largest down, the abstract ones — no cardinality of their own — last, and names breaking ties.
  private def order(definition: Definition): (Int, BigInt, String) = definition.size match {
    case Some(EffectiveTau)         => (0, BigInt(0), definition.name)
    case Some(EffectiveOmega)       => (0, BigInt(1), definition.name)
    case Some(l: LossyInfiniteSize) => (1, -l.bits, definition.name)
    case Some(f: FiniteSize)        => (2, -f.bits, definition.name)
    case Some(t: TinySize)          => (3, -BigInt(t.repr), definition.name)
    case None                       => (4, BigInt(0), definition.name)
  }

  // How the summary counts a definition: by the cardinality of its type, with the abstract ones
  // — no cardinality of their own — held apart.
  private enum SizeClass(val label: String) {
    case Unbounded extends SizeClass("unbounded")
    case Many extends SizeClass("with more than one value")
    case One extends SizeClass("with one value")
    case Empty extends SizeClass("with no values")
    case Abstract extends SizeClass("abstract")
  }

  private object SizeClass {

    /** The classes in the order the summary reads them: what needs attention first. */
    val order: List[SizeClass] = List(Unbounded, Many, One, Empty, Abstract)

    def apply(size: Option[Size]): SizeClass = size match {
      case None                                => Abstract
      case Some(EffectiveOmega | EffectiveTau) => Unbounded
      case Some(UnitSize)                      => One
      case Some(NothingSize)                   => Empty
      case Some(_)                             => Many
    }

  }

}
