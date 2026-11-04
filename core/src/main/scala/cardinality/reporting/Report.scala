package cardinality.reporting

import scala.jdk.CollectionConverters.*
import scala.meta.{Source as ScalaSource, *}

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.{Measuring, MethodAnalysis}
import cardinality.capacity.*
import cardinality.request.{AnalysisQuery, SourceSnapshot}
import cardinality.tasty.TastyStubs
import dev.constructive.eo.generics.{lens, prism}
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

/** A cardinality report over a set of Scala sources: what each source defines, how many values
  * every definition holds, and — for the ones the calculator could not bound — which type names
  * stopped it.
  *
  * The sources can be a project's, or the published `-sources.jar` of a library, which is what
  * makes a dependency's types measurable from outside. `locations` holds where `Report.of` placed
  * each source it read below its input; a report assembled from sources alone places them below the
  * directory they share (`Report.locations`). `warnings` lists the dependency declarations that
  * could not be read: their names stay unsupplied, which only refuses more.
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
    val shared = Report.locations(sources.map(_.path).filterNot(locations.contains))
    path => locations.getOrElse(path, shared(path))
  }

  // The method and constructor rows, ordered by file so a reader can walk the sources.
  private def models: List[(Report.Source, MethodAnalysis.Entry)] =
    sources
      .flatMap(source => source.methods.map(method => (source, method)))
      .sortBy(entry => (entry._1.path, entry._2.line, entry._2.name))

}

object Report {

  /** Opt-in implementation-only report. Supporting sources affect scope but never become rows.
    * Snapshot/read/parse failures reject the entire request rather than certify a partial scope.
    */
  final case class Query(
      targets: Seq[Path],
      support: Seq[Path] = Nil,
      targetNames: Set[String] = Set.empty,
      captureLimits: SourceSnapshot.Limits = SourceSnapshot.Limits(),
      limits: MethodAnalysis.Limits = MethodAnalysis.Limits(),
      budget: AnalysisQuery.Budget = AnalysisQuery.Budget(),
      classpath: Seq[Path] = Nil
  )

  final case class QueryResult(
      report: Report,
      snapshot: Option[String],
      analysis: Option[AnalysisQuery.Result],
      requestKey: Option[String] = None,
      configuration: Option[String] = None
  ) {

    def render: String =
      (List("Selected implementation analysis (no stored-value estimates)") ++
        configuration.map(value => s"limits: $value") ++
        snapshot.map(value => s"snapshot: $value") ++
        requestKey.map(value => s"request: $value (persistent cache disabled)") ++
        analysis.toList.flatMap(
          _.usage.map(u => s"work: ${u.path} ${u.name}: ${u.consumed}/${u.quota}")
        ) ++
        Report.methods(report.models, report.located) ++
        report.errors.map(Report.failed(_, report.located)) ++
        Report.warned(report.warnings)).mkString("\n")

  }

  def query(request: Query): QueryResult = {
    val result = try readQuery(request)
    catch {
      case scala.util.control.NonFatal(error) =>
        QueryResult(
          Report(
            Nil,
            List(Error("", s"query acquisition failed: ${error.getClass.getSimpleName}"))
          ),
          None,
          None
        )
    }
    result.copy(configuration =
      Some(
        s"${request.captureLimits}; ${request.limits}; ${request.budget}"
      )
    )
  }

  private def readQuery(request: Query): QueryResult = {
    val targetRoots = request.targets.map(SourceSnapshot.rootId).toSet
    SourceSnapshot.capture(request.targets ++ request.support, request.captureLimits) match {
      case Left(errors) =>
        QueryResult(Report(Nil, errors.map(e => Error(e.path, e.message))), None, None)
      case Right(snapshot) =>
        val parsed = snapshot.sources.map { source =>
          try parse(source.id -> source.text)
          catch {
            case _: StackOverflowError =>
              Left(Error(source.id, "parser recursion limit exceeded"))
          }
        }
        val errors = parsed.collect { case Left(error) => error }
        if (!targetRoots.subsetOf(snapshot.roots.keySet))
          QueryResult(
            Report(Nil, List(Error("", "snapshot-changed: target roots changed"))),
            None,
            None
          )
        else if (errors.nonEmpty)
          QueryResult(Report(Nil, errors), Some(snapshot.digest), None)
        else {
          val selected = targetRoots.flatMap(snapshot.roots)
          val stubs = dependencies(request.classpath)
          val found = AnalysisQuery.run(
            AnalysisQuery.Request(
              parsed.collect { case Right(input) => input } ++ stubs.inputs,
              selected,
              request.targetNames,
              request.limits,
              request.budget
            )
          )
          val grouped = found.entries.groupBy(_.path)
          val sources = snapshot.sources
            .filter(source => selected(source.id))
            .map(source => Source(source.id, Nil, grouped.getOrElse(source.id, Nil)))
          QueryResult(
            Report(
              sources,
              found.errors.map(message => Error("", message)),
              warnings = stubs.errors
            ),
            Some(snapshot.digest),
            Some(found),
            Option.when(found.key.nonEmpty)(
              SourceSnapshot.framedHash(
                List(
                  "report-query-v1",
                  snapshot.digest,
                  found.key,
                  request.captureLimits.toString
                )
              )
            )
          )
        }
    }
  }

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

  /** Where a row says it is: the source's path below the deepest directory holding every source of
    * the report (`Locations.shared`).
    */
  def locations(paths: List[String]): String => String = Locations.shared(paths)

  /** Reads every Scala source under `paths` — a file, a directory walked recursively, or a jar such
    * as the `-sources.jar` a library publishes — and measures the definitions it finds.
    *
    * `classpath` lists compiled dependencies (jars or class directories): their Scala 3 classes
    * join the environment as signature stubs (`TastyStubs`), never measured and never reported.
    */
  def of(paths: Seq[Path], classpath: Seq[Path] = Nil): Report = {
    val found = paths.toList.distinct.map(path => path -> read(path))
    val parsed: List[Either[Error, MethodAnalysis.Input]] = found.flatMap {
      case (_, Left(error))    => List(Left(error))
      case (_, Right(sources)) => sources.map(parse)
    }
    val inputs = parsed.collect { case Right(input) => input }
    val stubs = dependencies(classpath)
    // One library for the whole run: every source is read against the others, so a reference can
    // leave its file.
    val library = Library.of((inputs ++ stubs.inputs).map(_.tree))
    val methods = Measuring.rows(inputs, stubs.inputs, library = Some(library)).groupBy(_.path)
    Report(
      inputs.map(input =>
        Source(
          input.path,
          Counter.definitions(input.tree, library),
          methods.getOrElse(input.path, Nil)
        )
      ),
      parsed.collect { case Left(error) => error },
      Locations.placed(found.map((path, read) => path -> read.fold(_ => Nil, _.map(_._1)))),
      stubs.errors
    )
  }

  // The dependency stubs `classpath` renders. Every jar is rendered; nothing is rendered into the
  // stdlib model's namespaces, so the Scala library alone yields none. A classpath that cannot be
  // read is a warning: its names stay unsupplied, which only refuses more.
  private def dependencies(classpath: Seq[Path]): TastyStubs.Result =
    if (classpath.isEmpty) TastyStubs.Result(Nil, Nil)
    else
      try TastyStubs.of(classpath.toList.distinct, _ => true)
      catch {
        case scala.util.control.NonFatal(error) =>
          TastyStubs.Result(Nil, List(s"dependency classpath not read: $error"))
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

  // A dependency declaration left unsupplied, after the errors.
  private def warned(warnings: List[String]): List[String] =
    warnings.map(warning => s"  dependency stub not read: $warning")

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
