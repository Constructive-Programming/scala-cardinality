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

  def methods: List[MethodAnalysis.Entry] = sources.flatMap(_.methods)

  /** The report as it reads: the generic method and constructor implementation cardinalities, then
    * the stored-value estimate over the definitions a source introduces.
    *
    * They answer different questions. A method's number is how many canonical pure, total,
    * parametric implementations its signature admits with everything in scope; a definition's is
    * how many values its constructor parameters can hold, the estimate this calculator carried
    * before method counts existed. Neither is a proof of infinity: what the analysis could not
    * bound is `?`, with the reason.
    */
  def render: String =
    (Report.methods(models) ++ Report.summary(this) ++ Report.estimate(this) ++
      errors.map(Report.failed)).mkString("\n")

  // The method and constructor rows, ordered by file so a reader can walk the sources.
  private def models: List[(Report.Source, MethodAnalysis.Entry)] =
    sources
      .flatMap(source => source.methods.map(method => (source, method)))
      .sortBy(entry => (entry._1.path, entry._2.line, entry._2.name))

}

object Report {

  /** One Scala source the report read: where it was read from — a file, or an entry of a sources
    * jar — and the definitions it introduces.
    */
  final case class Source(
      path: String,
      definitions: List[Definition],
      methods: List[MethodAnalysis.Entry] = Nil
  )

  /** A source the report could not read or parse, and what went wrong. */
  final case class Error(path: String, message: String)

  /** Reads every Scala source under `paths` — a file, a directory walked recursively, or a jar such
    * as the `-sources.jar` a library publishes — and measures the definitions it finds.
    */
  def of(paths: Seq[Path]): Report = {
    val parsed: List[Either[Error, MethodAnalysis.Input]] = paths.toList.distinct.flatMap { path =>
      read(path) match {
        case Left(error)  => List(Left(error))
        case Right(found) => found.map(parse)
      }
    }
    val inputs = parsed.collect { case Right(input) => input }
    val methods = MethodAnalysis.analyze(inputs).groupBy(_.path)
    Report(
      inputs.map(input =>
        Source(input.path, Counter.definitions(input.tree), methods.getOrElse(input.path, Nil))
      ),
      parsed.collect { case Left(error) => error }
    )
  }

  // Parses one source, and reports either what it defines or what stopped the parser.
  private def parse(source: (String, String)): Either[Error, MethodAnalysis.Input] = {
    val (path, text) = source
    dialects.Scala3(text).parse[ScalaSource].toEither match {
      case Right(tree) => Right(MethodAnalysis.Input(path, tree))
      case Left(error) =>
        Left(Error(path, s"${error.message} (line ${error.pos.startLine + 1})"))
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

  // The legacy data estimate remains separate from implementation counts. Its fallback values
  // are not proofs of infinity, and an opaque representation is not a singleton.
  private def summary(report: Report): List[String] = {
    val definitions = report.definitions
    val counts = SizeClass.order.flatMap { sizeClass =>
      val count = definitions.count(definition => SizeClass(definition) == sizeClass)
      if (count == 0) Nil else List(s"$count ${sizeClass.label}")
    }
    List(
      s"scala-cardinality — ${plural(report.sources.size, "source")}, ${plural(definitions.size, "definition")}",
      "  stored-value estimates: constructor inputs only; finite bit counts are rounded bounds",
      "  ?: unresolved, not a proof of infinity; opaque representations are not singletons",
    ) ++
      (if (counts.isEmpty) Nil else List(s"  ${counts.mkString(" · ")}"))
  }

  private def plural(count: Int, noun: String): String =
    s"$count $noun${if (count == 1) "" else "s"}"

  private def uncertain(definition: Definition): Boolean =
    definition.unresolved.nonEmpty || definition.kind == Definition.Kind.Opaque ||
      definition.size.exists(s => s == EffectiveOmega || s == EffectiveTau)

  // The generic method and constructor counts, with what stands between the report and a number
  // for the rest: the triage list a reader works down.
  private def methods(models: List[(Source, MethodAnalysis.Entry)]): List[String] = {
    import Inhabitation.Count
    if (models.isEmpty) Nil
    else {
      val entries = models.map(_._2)
      val finite = entries.count(_.count.isInstanceOf[Count.Finite])
      val countable = entries.count(_.count == Count.Countable)
      val unresolved = entries.size - finite - countable
      val blocking = entries
        .flatMap(entry =>
          entry.count match {
            case Count.Unresolved(reasons) => reasons.map(reason => entry -> reason)
            case _                         => Nil
          }
        )
        .groupBy { case (_, reason) => reason.split(':').head }
        .toList
        .map { case (kind, blocked) => (blocked.map(_._1).distinct.size, kind) }
        .sortBy { case (count, kind) => (-count, kind) }
        .map { case (count, kind) => s"  $count unresolved on: $kind" }
      val rows = models.flatMap {
        case (source, entry) =>
          List(
            s"${entry.count.render}  ${entry.name}  ${entry.signature}  ${fileName(source.path)}:${entry.line}  [${entry.kind}]"
          ) ++
            (if (entry.captures.isEmpty) Nil
             else List(s"    captures: ${entry.captures.mkString(", ")}")) ++
            (entry.count match {
              case Count.Unresolved(reasons) => reasons.map(reason => s"    unresolved: $reason")
              case _                         => Nil
            })
      }
      List(
        "Generic method / constructor implementation cardinalities",
        s"  ${plural(entries.size, "signature")}: $finite finite · $countable countably infinite · $unresolved unresolved",
      ) ++ blocking ++ List("") ++ rows
    }
  }

  // The stored-value estimate over the definitions a source introduces, ordered by cardinality.
  private def estimate(report: Report): List[String] = {
    val rows = report.sources
      .flatMap(source => source.definitions.map(definition => (source, definition)))
      .sortBy(entry => order(entry._2))
      .map(row)
    if (rows.isEmpty) Nil
    else {
      val widths = List(
        rows.map(_._1.length).maxOption.getOrElse(0),
        rows.map(_._2.length).maxOption.getOrElse(0),
        rows.map(_._3.length).maxOption.getOrElse(0),
      )
      val table = rows.map {
        case (size, name, kind, location, unresolved) =>
          val line =
            s"${padded(size, widths(0))}  ${padded(name, widths(1))}  ${padded(kind, widths(2))}  $location"
          if (unresolved.isEmpty) line else s"$line  unresolved: $unresolved"
      }
      List("Stored-value estimates (constructor inputs; `?` = unresolved)", "") ++ table
    }
  }

  private def failed(error: Error): String = s"  could not read ${error.path}: ${error.message}"

  private def row(entry: (Source, Definition)): (String, String, String, String, String) = {
    val (source, definition) = entry
    (
      if (uncertain(definition)) "?" else definition.size.fold("—")(_.render),
      Definition.signature(definition),
      definition.kind.toString.toLowerCase,
      s"${fileName(source.path)}:${definition.line}",
      definition.unresolved.mkString(", "),
    )
  }

  private def padded(text: String, width: Int): String = text + " " * (width - text.length)

  // Jar entries are always named with `/`; a path read from disk may use either separator.
  private def fileName(path: String): String = path.split("[/\\\\]").last

  // The order the report reads in: what the estimate could not bound first, then the finite sizes
  // from the largest down, the abstract ones — no cardinality of their own — last, names breaking
  // ties.
  private def order(definition: Definition): (Int, BigInt, String) = definition.size match {
    case _ if definition.unresolved.nonEmpty => (0, BigInt(0), definition.name)
    case Some(EffectiveTau)                  => (0, BigInt(1), definition.name)
    case Some(EffectiveOmega)                => (0, BigInt(2), definition.name)
    case Some(l: LossyInfiniteSize)          => (1, -l.bits, definition.name)
    case Some(f: FiniteSize)                 => (2, -f.bits, definition.name)
    case Some(t: TinySize)                   => (3, -BigInt(t.repr), definition.name)
    case None                                => (4, BigInt(0), definition.name)
  }

  // How the summary counts a definition: by the cardinality of its type, with what the estimate
  // could not bound held apart from the abstract types that have no cardinality of their own.
  private enum SizeClass(val label: String) {
    case Unresolved extends SizeClass("unresolved")
    case Many extends SizeClass("with more than one value")
    case One extends SizeClass("with one value")
    case Empty extends SizeClass("with no values")
    case Abstract extends SizeClass("abstract")
  }

  private object SizeClass {

    /** The classes in the order the summary reads them: what needs attention first. */
    val order: List[SizeClass] = List(Unresolved, Many, One, Empty, Abstract)

    def apply(definition: Definition): SizeClass =
      if (uncertain(definition)) Unresolved
      else
        definition.size match {
          case None              => Abstract
          case Some(UnitSize)    => One
          case Some(NothingSize) => Empty
          case Some(_)           => Many
        }

  }

}
