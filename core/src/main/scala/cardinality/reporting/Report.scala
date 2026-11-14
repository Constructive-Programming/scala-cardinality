package cardinality.reporting

import scala.jdk.CollectionConverters.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.capacity.*
import cardinality.compiled.TastyFrontEnd
import cardinality.tasty.CompiledTypes
import dev.constructive.eo.generics.{lens, prism}
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

/** A cardinality report over a set of Scala sources: what each source defines, how many values
  * every definition holds, and — for the ones the calculator could not bound — which type names
  * stopped it.
  *
  * The sources can be a project's, or the published `-sources.jar` of a library beside its binary
  * jar, which is what makes a dependency's types measurable from outside. `locations` holds where
  * `Report.of` placed each source it read below its input; a report assembled from sources alone
  * places them below the directory they share (`Report.locations`). `warnings` lists the compiled
  * classes that could not be read, which only refuses more.
  */
final case class Report(
    sources: List[Report.Source],
    errors: List[Report.Error],
    locations: Map[String, String] = Map.empty,
    warnings: List[String] = Nil
) {

  def definitions: List[Definition] = sources.flatMap(_.definitions)

  def methods: List[MethodAnalysis.Entry] = sources.flatMap(_.methods)

  /** The stored-value rows, source by source, each as the estimate table reads it. */
  def definitionRows: List[Report.DefinitionRow] =
    sources.flatMap(source => source.definitions.map(Report.DefinitionRow.of(source.path, _)))

  /** The report as it reads: the generic method and constructor implementation cardinalities, then
    * the stored-value estimate over the definitions a source introduces.
    *
    * They answer different questions. A method's number is how many canonical pure, total,
    * parametric implementations its signature admits with everything in scope; a definition's is
    * how many values its constructor parameters can hold, the estimate this calculator carried
    * before method counts existed. Neither is a proof of infinity: what the analysis could not
    * bound is `?`, with the reason.
    */
  def render: String = {
    val location = located
    (Report.methods(models, location) ++ Report.summary(this) ++
      Report.estimate(definitionRows, location) ++ errors.map(Report.failed(_, location)) ++
      Report.warned(warnings)).mkString("\n")
  }

  /** Where a row of each source says it is. */
  def located: String => String = {
    val shared = Locations.shared(sources.map(_.path).filterNot(locations.contains))
    path => locations.getOrElse(path, shared(path))
  }

  // The method and constructor rows, ordered by file so a reader can walk the sources.
  private def models: List[(Report.Source, MethodAnalysis.Entry)] =
    sources
      .flatMap(source => source.methods.map(method => (source, method)))
      .sortBy(entry => (entry._1.path, entry._2.line, entry._2.name))

}

object Report {

  /** A selected report: the method rows of `targets`' report whose qualified name is one of
    * `targetNames` (all of them when none is given), without the stored-value estimates.
    */
  final case class Query(
      targets: Seq[Path],
      targetNames: Set[String] = Set.empty,
      environment: Environment = Environment()
  )

  /** What a report reads beside its sources: `classes`, the sources' own compiled output and the
    * dependency classpath it is read against. A source without compiled classes is refused; the
    * output may be empty where only the classpath is given, for a binary jar among the paths.
    */
  final case class Environment(classes: Option[CompiledTypes.Classes] = None)

  final case class QueryResult(report: Report) {

    def render: String =
      (List("Selected implementation analysis (no stored-value estimates)") ++
        Report.methods(report.models, report.located) ++
        report.errors.map(Report.failed(_, report.located)) ++
        Report.warned(report.warnings)).mkString("\n")

  }

  /** The targets' report, its method rows filtered by qualified name. */
  def query(request: Query): QueryResult = {
    val report =
      try of(request.targets, request.environment)
      catch {
        // A query that failed before any row was measured: what was thrown, by its class alone.
        case scala.util.control.NonFatal(error) =>
          Report(Nil, List(Error("", s"query acquisition failed: ${error.getClass.getSimpleName}")))
      }
    def chosen(entry: MethodAnalysis.Entry) =
      request.targetNames.isEmpty || request.targetNames(entry.name)
    // A name is not found only where every source was read: a refused source may hold it.
    val missing = request.targetNames -- report.methods.map(_.name)
    val notFound =
      Option.when(missing.nonEmpty && report.errors.isEmpty)(
        Error("", s"selected targets not found: ${missing.toList.sorted.mkString(", ")}")
      )
    QueryResult(
      report.copy(
        sources =
          report.sources.map(source => Source(source.path, Nil, source.methods.filter(chosen))),
        errors = report.errors ++ notFound
      )
    )
  }

  /** One Scala source the report read: where it was read from — a file, or an entry of a sources
    * jar — and the definitions it introduces.
    */
  final case class Source(
      path: String,
      definitions: List[Definition],
      methods: List[MethodAnalysis.Entry] = Nil
  )

  /** A source the report could not read, and what went wrong. */
  final case class Error(path: String, message: String)

  /** One stored-value row as the report reads it: the size cell — `?` for a row with reasons, `—`
    * for a definition with no size of its own — and the summary class the row counts under.
    */
  final case class DefinitionRow(
      source: String,
      definition: Definition,
      size: String,
      sizeClass: String
  )

  object DefinitionRow {

    def of(source: String, definition: Definition): DefinitionRow =
      DefinitionRow(
        source,
        definition,
        if (unresolved(definition)) "?" else definition.size.fold("—")(_.render),
        SizeClass(definition).label
      )

  }

  /** Reads every Scala source under `paths` — a file, a directory walked recursively, or a jar such
    * as the `-sources.jar` a library publishes — and measures the definitions it finds, in
    * `environment`. A jar of compiled classes among `paths` (a library's binary jar) joins
    * `environment.classes`' output, read against its classpath; the sources still name and place
    * the rows.
    */
  def of(paths: Seq[Path], environment: Environment = Environment()): Report = {
    val listed = paths.toList.distinct
    val binaries = listed.flatMap(path => compiledEntries(path).map(path -> _)).toMap
    // A binary jar that holds no sources is not read, nor placed, as sources.
    val found = listed.filterNot(binaries.get(_).contains(false)).map(path => path -> read(path))
    val sources = found.flatMap(_._2.toOption).flatten
    val classes =
      if (binaries.isEmpty) environment.classes
      else {
        val module = environment.classes.getOrElse(CompiledTypes.Classes(Nil))
        Some(module.copy(output = module.output ++ listed.filter(binaries.contains)))
      }
    val rows = TastyFrontEnd.rows(FrontEnd.Module(sources, Environment(classes)))
    // The front end answers each source in order; a path that could not be read stands where it
    // was listed.
    val answers = rows.sources.iterator
    val answered = found.flatMap {
      case (_, Left(error))    => List(Left(error))
      case (_, Right(sources)) => sources.map(_ => answers.next())
    }
    // The rows of a binary jar are named and placed by its sources, which a sources jar holds.
    val unsourced = Option.when(binaries.nonEmpty && sources.isEmpty)(
      Error(
        listed.filter(binaries.contains).mkString(", "),
        "no sources beside the compiled classes: give the sources jar too"
      )
    )
    Report(
      answered.collect { case Right(source) => source },
      answered.collect { case Left(error) => error } ++ unsourced,
      Locations.placed(found.map((path, read) => path -> read.fold(_ => Nil, _.map(_._1)))),
      rows.warnings
    )
  }

  // For a jar of compiled classes (TASTy or class files), whether it holds sources too; nothing for
  // any other path. A jar that cannot be opened is left to `read`, which reports it.
  private def compiledEntries(path: Path): Option[Boolean] =
    Option.when(Locations.jar(path))(path).flatMap { jar =>
      try {
        val archive = new ZipFile(jar.toFile)
        try {
          val names = archive.entries().asScala.map(_.getName).toList
          Option.when(names.exists(n => n.endsWith(".tasty") || n.endsWith(".class")))(
            names.exists(isScala)
          )
        } finally archive.close()
      } catch { case _: IOException => None }
    }

  // The Scala sources one path holds: a directory is walked recursively, a jar is read as the
  // sources jar a library publishes, and any other path is a single source file. A path that
  // cannot be read at all is reported rather than thrown, so one bad root does not lose the rest.
  private def read(path: Path): Either[Error, List[(String, String)]] =
    try
      Right(
        if (Files.isDirectory(path)) walk(path).map(file => (file.toString, Files.readString(file)))
        else if (Locations.jar(path)) entries(path)
        else List((path.toString, Files.readString(path))),
      )
    catch
      // A file exception's message is the path as given, which may be absolute: the line places
      // the path itself, and the exception's name says what went wrong.
      case e: IOException =>
        val message = Option(e.getMessage).filter(m => m.nonEmpty && m != path.toString)
        Left(Error(path.toString, e.getClass.getSimpleName + message.fold("")(m => s": $m")))

  private def walk(directory: Path): List[Path] = {
    val found = Files.walk(directory)
    try found.iterator().asScala.filter(p => isScala(p.toString)).toList.sortBy(_.toString)
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

  private def isScala(name: String): Boolean = name.endsWith(".scala")

  // What the report says, above the table: what was measured, what the sizes mean, and that a
  // question mark is a reason the calculator could not bound rather than a proof of infinity. The
  // stored-value estimate stays separate from the implementation counts the method section gives.
  private def summary(report: Report): List[String] = {
    val definitions = report.definitions
    val counts = SizeClass.values.toList.flatMap { sizeClass =>
      val count = definitions.count(definition => SizeClass(definition) == sizeClass)
      if (count == 0) Nil else List(s"$count ${sizeClass.label}")
    }
    List(
      s"scala-cardinality — ${plural(report.sources.size, "source")}, ${plural(definitions.size, "definition")}",
      "  stored-value estimates: constructor inputs only; a finite capacity prints as the bound " +
        "≤2^b, marked (lossy) when it holds floating point",
      "  ?: no single number here — the reason after the row says what would give one",
    ) ++
      (if (counts.isEmpty) Nil else List(s"  ${counts.mkString(" · ")}"))
  }

  private def plural(count: Int, noun: String): String =
    s"$count $noun${if (count == 1) "" else "s"}"

  // A row is a question when something in its types is outside the calculator's vocabulary, when
  // its own parameters are what the size depends on, or when an open abstraction unbounds it. A
  // countable or ε₀-tier size is not a question by itself: since the solver learned to count
  // recursive and lazy types, `ω` is the answer for a definition like
  // `case class St(head: Boolean, tail: => St)`, and an opaque row reads its representation where
  // it is defined — the kind says the representation is hidden to everyone else.
  private def unresolved(definition: Definition): Boolean = definition.unbound.nonEmpty

  private val entryReasons =
    lens[MethodAnalysis.Entry](_.count)
      .andThen(prism[Inhabitation.Count, Inhabitation.Count.Unresolved])

  // The generic method and constructor counts, with what stands between the report and a number
  // for the rest: the triage list a reader works down.
  private def methods(
      models: List[(Source, MethodAnalysis.Entry)],
      location: String => String
  ): List[String] =
    if (models.isEmpty) Nil
    else {
      val entries = models.map(_._2)
      val finite = entries.count(_.count.isFinite)
      val countable = entries.count(_.count.isCountable)
      val beyond = entries.count(_.count.isBeyondCountable)
      val unresolved = entries.size - finite - countable - beyond
      val blocking = entries
        .flatMap(entry => entryReasons.getOption(entry).toList.flatMap(_.reasons).map(entry -> _))
        .groupBy { case (_, reason) => Blockers.category(reason) }
        .toList
        .map { case (kind, blocked) => (blocked.map(_._1).distinct.size, kind) }
        .sortBy { case (count, kind) => (-count, kind) }
        .map { case (count, kind) => s"  $count unresolved on: $kind" }
      val rows = models.flatMap {
        case (source, entry) =>
          List(
            s"${entry.count.render}  ${entry.name}  ${entry.signature}  ${location(source.path)}:${entry.line}  [${entry.kind}]"
          ) ++
            (if (entry.captures.isEmpty) Nil
             else List(s"    captures: ${entry.captures.mkString(", ")}")) ++
            entryReasons
              .getOption(entry)
              .toList
              .flatMap(_.reasons)
              .map(reason => s"    unresolved: $reason")
      }
      List(
        "Generic method / constructor implementation cardinalities",
        s"  ${plural(entries.size, "signature")}: " +
          List(
            s"$finite finite",
            s"$countable countably infinite",
            if (beyond == 0) "" else s"$beyond beyond countable",
            s"$unresolved unresolved",
          ).filter(_.nonEmpty).mkString(" · "),
      ) ++ blocking ++ List("") ++ rows
    }

  // The stored-value estimate over the definitions a source introduces, ordered by cardinality.
  private def estimate(
      definitions: List[DefinitionRow],
      location: String => String
  ): List[String] = {
    val rows = definitions.sortBy(found => order(found.definition)).map(row(_, location))
    if (rows.isEmpty) Nil
    else {
      val widths = List(
        rows.map(_._1.length).maxOption.getOrElse(0),
        rows.map(_._2.length).maxOption.getOrElse(0),
        rows.map(_._3.length).maxOption.getOrElse(0),
      )
      val table = rows.map {
        case (size, name, kind, where, reason) =>
          val line =
            s"${padded(size, widths(0))}  ${padded(name, widths(1))}  ${padded(kind, widths(2))}  $where"
          if (reason.isEmpty) line else s"$line  $reason"
      }
      List("Stored-value estimates (constructor inputs; `?` = unresolved)", "") ++ table
    }
  }

  // A source or input that could not be read, placed as a row would be.
  private def failed(error: Error, location: String => String): String =
    s"  could not read ${location(error.path)}: ${error.message}"

  // What the report read less of than it was given, after the errors.
  private def warned(warnings: List[String]): List[String] = warnings.map(warning => s"  $warning")

  private def row(
      found: DefinitionRow,
      location: String => String
  ): (String, String, String, String, String) =
    (
      found.size,
      Definition.signature(found.definition),
      found.definition.kind.toString.toLowerCase,
      s"${location(found.source)}:${found.definition.line}",
      Definition.reasons(found.definition),
    )

  private def padded(text: String, width: Int): String = text + " " * (width - text.length)

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
  private def magnitude(size: Size): BigInt =
    cardinality.capacity.SizeOptics.exactFiniteValue.getOption(size).getOrElse(size.finite.bits)

  // How the summary counts a definition: by the cardinality of its type, with what the estimate
  // could not bound and the abstract types that have no cardinality of their own held apart. A row
  // that is only waiting for an instantiation is not a gap in the calculator, and a row an open
  // abstraction unbounds is a fact about the sources, so neither joins the unresolved ones.
  private[cardinality] enum SizeClass(val label: String) {
    case Unresolved extends SizeClass("unresolved")
    case Instantiation extends SizeClass("instantiation-dependent")
    case Open extends SizeClass("unbounded by an open abstraction")
    case Constructor extends SizeClass("type constructors, with no value space")
    case Many extends SizeClass("with more than one value")
    case One extends SizeClass("with one value")
    case Empty extends SizeClass("with no values")
    case Abstract extends SizeClass("abstract")
  }

  private[cardinality] object SizeClass {

    def apply(definition: Definition): SizeClass =
      Definition.gap(definition) match {
        case Some(Definition.Gap.Unresolved)    => Unresolved
        case Some(Definition.Gap.Open)          => Open
        case Some(Definition.Gap.Instantiation) => Instantiation
        case None                               => valued(definition)
      }

    private def valued(definition: Definition): SizeClass =
      if (definition.kind == Definition.Kind.Constructor) Constructor
      else
        definition.size match {
          case None              => Abstract
          case Some(UnitSize)    => One
          case Some(NothingSize) => Empty
          case Some(_)           => Many
        }

  }

}
