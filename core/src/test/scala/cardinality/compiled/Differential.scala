package cardinality.compiled

import scala.jdk.CollectionConverters.*

import cardinality.analysis.MethodAnalysis.Entry
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.Definition
import cardinality.reporting.{FrontEnd, Report, ScalametaFrontEnd}
import cardinality.tasty.CompiledTypes
import java.nio.file.{Files, Path}

/** The differential gate of the front-end rewrite (docs/plans/compiled-own-types.md, phase 4b): two
  * front ends read the same input, and every row of either is classified.
  *
  * A row of the new front end is its own or it is `Uncovered`: the new front end never asks the old
  * one, so an identical row is one the new front end measured itself. A `Worse` row and a row only
  * one side has fail the gate (`failures`, and the runner's exit status). A `Better` row and a row
  * paired by a looser key than its name and line are listed to be explained (`explained`): a better
  * row needs its derivation, since the old front end is no oracle where it says `?`. Parity is
  * every old numeric row `Identical`; until then the summary says how many are.
  */
object Differential {

  enum Verdict {

    /** The same count, or `?` on both sides. */
    case Identical

    /** `?` before, a number now. */
    case Better

    /** A number before, and `?` or another number now. */
    case Worse

    /** The new front end does not read the row yet. */
    case Uncovered

    /** A row only the old front end has. */
    case OnlyOld

    /** A row only the new front end has. */
    case OnlyNew
  }

  /** A row of either table as the comparison reads it: what places it, its count (the count itself,
    * compared structurally, and how it prints) and its reasons.
    */
  final case class Row(
      table: String,
      source: String,
      name: String,
      line: Int,
      kind: String,
      signature: String,
      value: Any,
      shown: String,
      reasons: List[String]
  ) {
    def numeric: Boolean = reasons.isEmpty

    /** Whether the new front end declined the row instead of measuring it. */
    def uncovered: Boolean =
      reasons.nonEmpty && reasons.forall(_.startsWith(TastyFrontEnd.Uncovered))

    def render: String = s"$shown  $name  $signature  $source:$line  [$kind]" +
      reasons.map(reason => s"\n        $reason").mkString

  }

  object Row {

    def of(entry: Entry): Row = {
      val reasons = entry.count match {
        case Count.Unresolved(reasons) => reasons
        case _                         => Nil
      }
      val value = if (reasons.isEmpty) entry.count else "?"
      Row(
        "method",
        entry.path,
        entry.name,
        entry.line,
        entry.kind,
        entry.signature,
        value,
        entry.count.render,
        reasons
      )
    }

    def of(source: String, definition: Definition): Row = {
      val reasons = definition.unbound.map(_.render)
      val row = Report.DefinitionRow.of(source, definition)
      Row(
        "definition",
        source,
        definition.name,
        definition.line,
        definition.kind.toString.toLowerCase,
        Definition.signature(definition),
        if (reasons.isEmpty) definition.size else "?",
        row.size,
        reasons
      )
    }

    def all(rows: FrontEnd.Rows): List[Row] = rows.sources.flatMap {
      case Right(source) =>
        source.methods.map(of) ++ source.definitions.map(of(source.path, _))
      case Left(_) => Nil
    }

  }

  /** One row compared: its verdict, each side's row, and what paired them. */
  final case class Compared(verdict: Verdict, old: Option[Row], next: Option[Row], by: String) {
    def row: Row = old.orElse(next).get

    def render: String =
      s"${verdict.toString.toUpperCase} (paired by $by)" +
        old.map(row => s"\n    scalameta: ${row.render}").mkString +
        next
          .map(row => s"\n    ${if (row.uncovered) "tasty, uncovered" else "tasty"}: ${row.render}")
          .mkString

  }

  /** Every row of the two answers, paired and classified. */
  def compare(old: FrontEnd.Rows, next: FrontEnd.Rows): List[Compared] =
    paired(Row.all(old), Row.all(next)).map {
      case (Some(before), Some(after), by) =>
        Compared(verdict(before, after), Some(before), Some(after), by)
      case (before, None, by) => Compared(Verdict.OnlyOld, before, None, by)
      case (None, after, by)  => Compared(Verdict.OnlyNew, None, after, by)
    }

  private def verdict(old: Row, next: Row): Verdict =
    if (next.uncovered) Verdict.Uncovered
    else if (old.value == next.value) Verdict.Identical
    else if (!old.numeric) Verdict.Better
    else Verdict.Worse

  // The keys rows are paired by, strictest first: the row's whole place; its position, where the
  // scopes of a body are named differently (the name without its `<kind#n>` segments, on the same
  // line); its name and line, where the kind differs. A key pairs the rows that carry it, in
  // source order, only when both sides have as many: no row is paired by guessing.
  val Strict = "name, kind and line"

  private val keys: List[(String, Row => Any)] = List(
    Strict -> (row => (row.table, row.source, row.name, row.kind, row.line)),
    "position" -> (row => (row.table, row.source, row.kind, row.line, unscoped(row.name))),
    "name and line, of another kind" -> (row => (row.table, row.source, row.name, row.line))
  )

  private def unscoped(name: String): String = name.replaceAll("""<[a-z]+#\d+>\.""", "")

  private def paired(old: List[Row], next: List[Row]): List[(Option[Row], Option[Row], String)] = {
    val (pairs, restOld, restNext) =
      keys.foldLeft((List.empty[(Option[Row], Option[Row], String)], old, next)) {
        case ((pairs, old, next), (by, key)) =>
          val (olds, nexts) = (old.groupBy(key), next.groupBy(key))
          val matched = olds.keySet.filter(k => nexts.get(k).exists(_.size == olds(k).size))
          val found =
            old.filter(row => matched(key(row))).groupBy(key).toList.flatMap { (k, rows) =>
              rows.zip(nexts(k)).map((before, after) => (Option(before), Option(after), by))
            }
          (
            pairs ++ found,
            old.filterNot(r => matched(key(r))),
            next.filterNot(r => matched(key(r)))
          )
      }
    pairs ++ restOld.map(row => (Some(row), None, "nothing")) ++
      restNext.map(row => (None, Some(row), "nothing"))
  }

  /** The totals of a comparison: per table the verdicts, how many old numeric rows kept their
    * number, how the rows were paired and how many the new front end measured itself.
    */
  def summary(compared: List[Compared]): String =
    compared
      .groupBy(_.row.table)
      .toList
      .sortBy(_._1)
      .flatMap { (table, rows) =>
        val by = rows.groupBy(_.verdict).view.mapValues(_.size).toMap.withDefaultValue(0)
        val olds = rows.count(_.old.nonEmpty)
        val nexts = rows.flatMap(_.next)
        val numeric = rows.filter(_.old.exists(_.numeric))
        val own = nexts.count(!_.uncovered)
        val pairing =
          rows.groupBy(_.by).toList.sortBy(_._1).map((by, rows) => s"${rows.size} by $by")
        val signatures = rows.count(c => c.old.zip(c.next).exists(_.signature != _.signature))
        List(
          s"$table rows: $olds old, ${nexts.size} new",
          "  " + Verdict.values.map(v => s"${by(v)} ${v.toString.toLowerCase}").mkString(" · "),
          s"  old numeric rows: ${numeric.size}, of which " +
            Verdict.values
              .map(v => s"${numeric.count(_.verdict == v)} ${v.toString.toLowerCase}")
              .mkString(" · "),
          s"  measured by the new front end: $own of ${nexts.size} rows" +
            (if (olds == 0) "" else f" (${100.0 * own / olds}%.1f%% of the old rows)"),
          s"  paired: ${pairing.mkString(" · ")}; $signatures paired rows spell another signature"
        )
      }
      .mkString("\n")

  /** The rows that fail the gate: a worse one, and one only one side has. */
  def failures(compared: List[Compared]): List[Compared] =
    compared.filter(c => Set(Verdict.Worse, Verdict.OnlyOld, Verdict.OnlyNew)(c.verdict))

  /** The rows to explain: a better one (its derivation), and one whose two sides differ in name,
    * kind or scope (what paired them).
    */
  def explained(compared: List[Compared]): List[Compared] =
    compared.filter(c =>
      c.verdict == Verdict.Better || (c.old.nonEmpty && c.next.nonEmpty && c.by != Strict)
    )

  /** What the uncovered rows need, most frequent first. */
  def needs(compared: List[Compared]): List[(String, Int)] =
    compared
      .flatMap(_.next.filter(_.uncovered).toList.flatMap(_.reasons))
      .map(_.stripPrefix(TastyFrontEnd.Uncovered + ": ").replaceAll(": .*", ""))
      .groupBy(identity)
      .toList
      .map((reason, found) => reason -> found.size)
      .sortBy((reason, count) => (-count, reason))

  // ---- inputs -------------------------------------------------------------------------------

  /** One module both front ends read; `label` is put before its rows' sources, to tell one
    * fixture's from another's.
    */
  final case class Input(label: String, module: FrontEnd.Module)

  /** One input compared: its rows, and what a front end could not read (a source, a class). */
  final case class Outcome(compared: List[Compared], failed: List[String])

  /** A module as the build plugins hand it over: `<root>/core/src/main/scala` sources, `classes`
    * and `classpath.txt` (the frozen self-measurement corpus).
    */
  def corpus(root: Path): Input = {
    val classpath = Files.readAllLines(root.resolve("classpath.txt")).asScala.toList.map(Path.of(_))
    val classes = CompiledTypes.Classes(List(root.resolve("classes")), classpath)
    Input(
      "",
      FrontEnd.Module(
        scalaFiles(root.resolve("core/src/main/scala"), identity),
        Report.Environment(classpath, None, Some(classes))
      )
    )
  }

  /** The compiled test fixtures under `cache` (`cardinality.Compiled`'s): each `<key>/src` with its
    * `classes`, read against `libraries` as the tests read them, under the paths the tests gave. A
    * directory `<key>.<thread>` is a compile's staging the cache left behind, not a fixture.
    */
  def fixtures(cache: Path, libraries: List[Path]): List[Input] =
    list(cache).filter(fixture).map { dir =>
      val src = dir.resolve("src")
      val classes = CompiledTypes.Classes(List(dir.resolve("classes")), libraries)
      val read = Report.Environment(classes = Some(classes))
      Input(s"${dir.getFileName}/", FrontEnd.Module(scalaFiles(src, src.relativize(_)), read))
    }

  private def fixture(dir: Path): Boolean =
    !dir.getFileName.toString.contains('.') && Files.exists(dir.resolve("ok"))

  private def list(directory: Path): List[Path] = {
    val found = Files.list(directory)
    try found.iterator().asScala.toList.sortBy(_.toString)
    finally found.close()
  }

  private def scalaFiles(root: Path, named: Path => Path): List[(String, String)] = {
    val found = Files.walk(root)
    try
      found
        .iterator()
        .asScala
        .filter(_.toString.endsWith(".scala"))
        .toList
        .sortBy(_.toString)
        .map(file => named(file).toString -> Files.readString(file))
    finally found.close()
  }

  /** One front end's answer; one that throws answers no source. */
  def answer(frontEnd: FrontEnd, module: FrontEnd.Module): FrontEnd.Rows =
    try frontEnd.rows(module)
    catch {
      case scala.util.control.NonFatal(error) =>
        FrontEnd.Rows(
          module.sources.map((path, _) => Left(Report.Error(path, error.toString))),
          Nil
        )
    }

  /** Both front ends over one input, `old` being the scalameta front end's answer. */
  def run(input: Input, old: FrontEnd.Rows): Outcome = {
    val next = answer(TastyFrontEnd, input.module)
    def failed(side: String, rows: FrontEnd.Rows) = rows.sources.collect {
      case Left(error) => s"$side could not read ${input.label}${error.path}: ${error.message}"
    }
    def labelled(row: Row) = row.copy(source = input.label + row.source)
    Outcome(
      compare(old, next).map(c => c.copy(old = c.old.map(labelled), next = c.next.map(labelled))),
      failed("scalameta", old) ++ failed("tasty", next) ++
        next.warnings.map(warning => s"tasty warns in ${input.label}: $warning")
    )
  }

  // The scalameta front end's answer, kept in `file` when one is named: the old front end takes
  // about 100 s on the corpus and does not change while the new one is written. The file is named
  // on the command line and said in the output; delete it when the old front end or the input
  // changes.
  private def kept(file: Option[Path], input: Input): FrontEnd.Rows = file match {
    case Some(path) if Files.exists(path) =>
      println(s"scalameta rows read from $path")
      val in = new java.io.ObjectInputStream(Files.newInputStream(path))
      try in.readObject().asInstanceOf[FrontEnd.Rows]
      finally in.close()
    case _ =>
      val rows = answer(ScalametaFrontEnd, input.module)
      // An answer that read every source is worth keeping; a failure is computed again.
      file.filter(_ => rows.sources.forall(_.isRight)).foreach { path =>
        val out = new java.io.ObjectOutputStream(Files.newOutputStream(path))
        try out.writeObject(rows)
        finally out.close()
      }
      rows
  }

  /** `corpus <root>` or `fixtures <cache>`, then `--rows` to list every row with the front end that
    * produced it and `--old <file>` to keep the scalameta rows of a corpus between runs. Prints the
    * totals, what the uncovered rows need, what was not read, the rows that fail the gate and the
    * rows to explain; exits with 1 when a row fails the gate or a front end could not read
    * something.
    */
  def main(args: Array[String]): Unit = {
    val libraries = sys
      .props("java.class.path")
      .split(java.io.File.pathSeparator)
      .toList
      .map(Path.of(_))
      .filter { p =>
        val name = p.getFileName.toString
        name.startsWith("scala-library") || name.startsWith("scala3-library")
      }
    val inputs = args.toList match {
      case "corpus" :: root :: _    => List(corpus(Path.of(root)))
      case "fixtures" :: cache :: _ => fixtures(Path.of(cache), libraries)
      case _                        =>
        sys.error("usage: Differential (corpus <root> | fixtures <cache>) [--rows] [--old <file>]")
    }
    val keep = args.sliding(2).collectFirst { case Array("--old", file) => Path.of(file) }
    require(inputs.nonEmpty, "no input: no fixture compiled under the cache")
    require(keep.isEmpty || inputs.size == 1, "--old keeps one input's rows: a corpus")
    val started = System.nanoTime
    val outcomes =
      inputs.asJava
        .parallelStream()
        .map(input => run(input, kept(keep, input)))
        .toList
        .asScala
        .toList
    val compared = outcomes.flatMap(_.compared)
    println(summary(compared))
    println(f"  ${inputs.size} inputs in ${(System.nanoTime - started) / 1e9}%.1f s")
    println("uncovered rows need:")
    needs(compared).foreach((reason, count) => println(f"  $count%6d  $reason"))
    val unread = outcomes.flatMap(_.failed)
    unread.foreach(println)
    val failed = failures(compared)
    val listed = if (args.contains("--rows")) compared else (failed ++ explained(compared)).distinct
    listed.sortBy(c => (c.row.source, c.row.line, c.row.name)).foreach(c => println(c.render))
    if (failed.nonEmpty || unread.nonEmpty) sys.exit(1)
  }

}
