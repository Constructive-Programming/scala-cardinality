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
    // One library for the whole run: every source is read against the others, so a reference can
    // leave its file.
    val library = Library.of(inputs.map(_.tree))
    val methods = MethodAnalysis.analyze(inputs).groupBy(_.path)
    Report(
      inputs.map(input =>
        Source(
          input.path,
          Counter.definitions(input.tree, library),
          methods.getOrElse(input.path, Nil)
        )
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

  // What the report says, above the table: what was measured, what the sizes mean, and that a
  // question mark is a reason the calculator could not bound rather than a proof of infinity. The
  // stored-value estimate stays separate from the implementation counts the method section gives.
  private def summary(report: Report): List[String] = {
    val definitions = report.definitions
    val counts = SizeClass.order.flatMap { sizeClass =>
      val count = definitions.count(definition => SizeClass(definition) == sizeClass)
      if (count == 0) Nil else List(s"$count ${sizeClass.label}")
    }
    List(
      s"scala-cardinality — ${plural(report.sources.size, "source")}, ${plural(definitions.size, "definition")}",
      "  stored-value estimates: constructor inputs only; finite capacities are upper bounds",
      "  ?: no single number here — the reason after the row says what would give one",
    ) ++
      (if (counts.isEmpty) Nil else List(s"  ${counts.mkString(" · ")}"))
  }

  private def plural(count: Int, noun: String): String =
    s"$count $noun${if (count == 1) "" else "s"}"

  // A row is a question when something in its types is outside the calculator's vocabulary, when
  // its own parameters are what the size depends on, when an open abstraction unbounds it, or when
  // it is an opaque type whose representation the report cannot see. A countable or ε₀-tier size is
  // not a question by itself: since the solver learned to count recursive and lazy types, `ω` is
  // the answer for a definition like `case class St(head: Boolean, tail: => St)`.
  private def unresolved(definition: Definition): Boolean =
    definition.unbound.nonEmpty || definition.kind == Definition.Kind.Opaque

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
        case (size, name, kind, location, reason) =>
          val line =
            s"${padded(size, widths(0))}  ${padded(name, widths(1))}  ${padded(kind, widths(2))}  $location"
          if (reason.isEmpty) line else s"$line  $reason"
      }
      List("Stored-value estimates (constructor inputs; `?` = unresolved)", "") ++ table
    }
  }

  private def failed(error: Error): String = s"  could not read ${error.path}: ${error.message}"

  private def row(entry: (Source, Definition)): (String, String, String, String, String) = {
    val (source, definition) = entry
    (
      if (unresolved(definition)) "?" else definition.size.fold("—")(_.render),
      Definition.signature(definition),
      definition.kind.toString.toLowerCase,
      s"${fileName(source.path)}:${definition.line}",
      reason(definition),
    )
  }

  // How a row's reasons read. A row whose only reasons are the parameters it declares has no number
  // to show and says so; a row an open abstraction unbounds says that; any other row lists what it
  // could not read (a name, a match type, a refinement).
  private def reason(definition: Definition): String =
    if (definition.unbound.isEmpty) ""
    else if (Definition.instantiationDependent(definition))
      s"depends on its instantiation (${definition.unbound.map(_.render).mkString(", ")})"
    else if (Definition.open(definition))
      s"open to implementations (${definition.unbound.map(_.render).mkString(", ")})"
    else s"unresolved: ${definition.unbound.map(_.render).mkString(", ")}"

  private def padded(text: String, width: Int): String = text + " " * (width - text.length)

  // Jar entries are always named with `/`; a path read from disk may use either separator.
  private def fileName(path: String): String = path.split("[/\\\\]").last

  // The order the report reads in: what the calculator could not bound first, then the bounded
  // sizes from the largest down — ε₀, then ω, then finite capacities and exact counts — and the
  // definitions with no cardinality of their own — abstract types — last. Names break ties.
  private def order(definition: Definition): (Int, BigInt, BigInt, Int, BigInt, String) =
    if (unresolved(definition)) (0, BigInt(0), BigInt(0), 0, BigInt(0), definition.name)
    else
      definition.size match {
        case Some(size) =>
          (1, -size.epsilon, -size.omega, -size.finite.rank, -magnitude(size), definition.name)
        case None => (2, BigInt(0), BigInt(0), 0, BigInt(0), definition.name)
      }

  // The magnitude inside one finite rank: the count itself for an exact size, the bit width
  // otherwise, which is what `FinitePart.order` compares.
  private def magnitude(size: Size): BigInt = size.finite match {
    case FinitePart.Exact(repr) => repr
    case other                  => other.bits
  }

  // How the summary counts a definition: by the cardinality of its type, with what the estimate
  // could not bound and the abstract types that have no cardinality of their own held apart. A row
  // that is only waiting for an instantiation is not a gap in the calculator, and a row an open
  // abstraction unbounds is a fact about the sources, so neither joins the unresolved ones.
  private enum SizeClass(val label: String) {
    case Unresolved extends SizeClass("unresolved")
    case Instantiation extends SizeClass("instantiation-dependent")
    case Open extends SizeClass("unbounded by an open abstraction")
    case Many extends SizeClass("with more than one value")
    case One extends SizeClass("with one value")
    case Empty extends SizeClass("with no values")
    case Abstract extends SizeClass("abstract")
  }

  private object SizeClass {

    /** The classes in the order the summary reads them: what needs attention first. */
    val order: List[SizeClass] = List(Unresolved, Instantiation, Open, Many, One, Empty, Abstract)

    def apply(definition: Definition): SizeClass =
      if (Definition.instantiationDependent(definition)) Instantiation
      else if (Definition.open(definition)) Open
      else if (unresolved(definition)) Unresolved
      else
        definition.size match {
          case None              => Abstract
          case Some(UnitSize)    => One
          case Some(NothingSize) => Empty
          case Some(_)           => Many
        }

  }

}
