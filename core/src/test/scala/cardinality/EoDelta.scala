package cardinality

import scala.jdk.CollectionConverters.*

import cardinality.reporting.Report.SizeClass
import cardinality.types.Spelling
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** An opt-in review tool beside `EoCheckout`: the per-row delta between two of its runs, the
  * reviewable artifact a change to the analyzer is judged by.
  *
  * Method rows are joined on a key that mirrors `EoBaseline.identity`'s columns as the TSV writes
  * them — source, qualified name with anonymous owners' line numbers dropped, kind, enclosing
  * signature and signature, the last two spelled on one line — and definition rows on source,
  * qualified name, kind and parameters. The report prints added and removed rows, every changed row
  * old against new, a tally by transition (`? -> n`, `n -> ?`, `n -> n`, reason only) and, for
  * definitions, the moves between the summary classes. Reasons are compared as the report prints
  * them.
  *
  * Runs written before `eo-checkout-definitions.tsv` existed still compare: their definitions are
  * read from the report's table, which names a source by its file name only, so when either run is
  * one of those both are joined on the qualified name with parameters, the kind and the file name
  * (an instance member's `O#T` read as the `O.T` those runs printed), and their classes read from
  * the size and reasons. Their method rows whose reasons spanned lines are joined back.
  *
  * Usage: `EoDelta <old-run-directory> <new-run-directory> [<output-file>]`.
  */
object EoDelta {

  def main(args: Array[String]): Unit = {
    require(
      args.length == 2 || args.length == 3,
      "usage: EoDelta <old-run-directory> <new-run-directory> [<output-file>]"
    )
    val text = report(Path.of(args(0)), Path.of(args(1)))
    print(text)
    args.lift(2).foreach(file => Files.writeString(Path.of(file), text, UTF_8))
  }

  /** What a row says: its count or size, its reasons, and for a definition its summary class. */
  final case class Reading(value: String, reasons: String, sizeClass: String = "")

  /** A definition row under the identity its run can give: the full one when the run has the
    * definitions TSV, and always the coarser one an older run's report table gives.
    */
  final case class Found(full: Option[List[String]], coarse: List[String], reading: Reading)

  def report(old: Path, next: Path): String = {
    val (before, after) = joined(definitions(old), definitions(next))
    (delta("methods", methods(old), methods(next)) ++
      delta("stored values", before, after) ++
      transitions(before, after)).mkString("", "\n", "\n")
  }

  def methods(run: Path): Map[List[String], Reading] = {
    val lines = Files.readAllLines(run.resolve("eo-checkout-methods.tsv"), UTF_8).asScala.toList
    unique(
      rejoined(lines.drop(1)).map(_.split("\t", -1).toList).map { fields =>
        val key = List(
          fields(1),
          EoBaseline.anonymousOwners(fields(2)),
          fields(3),
          canonical(fields(4)),
          canonical(fields(5))
        )
        key -> Reading(fields(6), Tsv.field(fields.lift(7).getOrElse("")))
      },
      s"method rows of $run"
    )
  }

  /** A signature as `Spelling` writes one, from the whitespace-collapsed text an older run wrote:
    * no space just inside a parenthesis or bracket, no trailing comma before one that closes, and
    * no space between two clauses (`(x: A) (y: A)` is `(x: A)(y: A)`).
    */
  def canonical(signature: String): String =
    Spelling
      .line(signature)
      .replaceAll("([(\\[]) ", "$1")
      .replaceAll(",? ?([)\\]])", "$1")
      .replaceAll("([)\\]]) ([(\\[])", "$1$2")

  def definitions(run: Path): List[Found] = {
    val table = run.resolve("eo-checkout-definitions.tsv")
    if (Files.exists(table)) Tsv.rows(Files.readString(table, UTF_8)).map(tabled)
    else reported(Files.readString(run.resolve("eo-checkout.txt"), UTF_8))
  }

  // Rows of an older TSV whose reasons held line breaks: a line that does not open a row with all
  // its columns continues the row before it.
  private def rejoined(lines: List[String]): List[String] =
    lines
      .foldLeft(List.empty[String]) {
        case (row :: done, line) if !opensRow(line) => s"$row ${line.trim}" :: done
        case (done, line)                           => line :: done
      }
      .reverse

  private def opensRow(line: String): Boolean =
    line.count(_ == '\t') >= 7 && !line.headOption.exists(_.isWhitespace)

  private def tabled(row: Map[String, String]): Found = {
    val params = Tsv.items(row("params"))
    val name = row("qualified_name")
    val kind = row("kind")
    Found(
      Some(List(row("source"), name, kind, Tsv.list(params))),
      coarse(signature(name, params), kind, row("source")),
      Reading(row("size"), row("reasons"), row("class"))
    )
  }

  private val Line =
    """^(\S+(?: \+ \S+)*(?: \(lossy\))?)\s{2,}(\S.*?)\s{2,}(\w+)\s{2,}(\S+):(\d+)(?:\s{2}(.*))?$""".r

  // The table of a report written before the definitions TSV existed.
  private def reported(text: String): List[Found] =
    text.linesIterator
      .dropWhile(!_.startsWith("Stored-value estimates (constructor inputs"))
      .drop(2)
      .toList
      .collect {
        case Line(size, name, kind, file, _, reasons) =>
          val said = Option(reasons).getOrElse("")
          Found(None, coarse(name, kind, file), Reading(size, said, sizeClass(size, kind, said)))
      }

  private def signature(name: String, params: List[String]): String =
    if (params.isEmpty) name else params.mkString(s"$name[", ", ", "]")

  private def coarse(signature: String, kind: String, source: String): List[String] =
    List(signature.replace('#', '.'), kind, source.split("[/\\\\]").last)

  // An older run's summary class, read as the report decides it from what an older report printed
  // — the first class of reasons, else the kind and the size — under the report's own labels.
  private def sizeClass(size: String, kind: String, reasons: String): String =
    (if (size == "?")
       if (reasons.startsWith("open to implementations")) SizeClass.Open
       else if (reasons.startsWith("depends on its instantiation")) SizeClass.Instantiation
       else SizeClass.Unresolved
     else if (kind == "constructor") SizeClass.Constructor
     else
       size match {
         case "—" => SizeClass.Abstract
         case "0" => SizeClass.Empty
         case "1" => SizeClass.One
         case _   => SizeClass.Many
       }).label

  // The finest identity both runs carry.
  private def joined(
      before: List[Found],
      after: List[Found]
  ): (Map[List[String], Reading], Map[List[String], Reading]) = {
    val full = (before ++ after).forall(_.full.nonEmpty)
    def keyed(rows: List[Found], label: String) =
      unique(rows.map(row => (if (full) row.full.get else row.coarse) -> row.reading), label)
    (keyed(before, "old definition rows"), keyed(after, "new definition rows"))
  }

  private def unique(
      rows: List[(List[String], Reading)],
      label: String
  ): Map[List[String], Reading] = {
    val repeated = rows.groupBy(_._1).collect { case (key, found) if found.size > 1 => key }
    require(repeated.isEmpty, s"$label repeat an identity: ${repeated.map(show).mkString("; ")}")
    rows.toMap
  }

  private def show(key: List[String]): String = key.mkString(" | ")

  private def delta(
      label: String,
      before: Map[List[String], Reading],
      after: Map[List[String], Reading]
  ): List[String] = {
    def sorted(keys: Set[List[String]]) = keys.toList.sortBy(_.mkString("\t"))
    val added = sorted(after.keySet -- before.keySet)
    val removed = sorted(before.keySet -- after.keySet)
    val changed = sorted(before.keySet & after.keySet).filter { key =>
      val (was, now) = (before(key), after(key))
      (was.value, was.reasons) != (now.value, now.reasons)
    }
    val tally = changed
      .groupBy(key => transition(before(key), after(key)))
      .toList
      .sortBy(_._1)
      .map((kind, keys) => s"  ${keys.size} changed: $kind")
    List(
      s"== $label: ${before.size} -> ${after.size} rows; " +
        s"+${added.size} -${removed.size} ~${changed.size}"
    ) ++
      changed.flatMap { key =>
        List(
          s"  ~ ${show(key)}",
          s"      ${said(before(key))}",
          s"   -> ${said(after(key))}"
        )
      } ++
      added.map(key => s"  + ${show(key)}  ${said(after(key))}") ++
      removed.map(key => s"  - ${show(key)}  ${said(before(key))}") ++
      tally
  }

  private def said(reading: Reading): String = s"${reading.value} [${reading.reasons}]"

  private def transition(was: Reading, now: Reading): String =
    if (was.value == now.value) "reason only"
    else s"${shape(was.value)} -> ${shape(now.value)}"

  private def shape(value: String): String = if (value == "?") "?" else "n"

  // The definitions that moved between summary classes, by move.
  private def transitions(
      before: Map[List[String], Reading],
      after: Map[List[String], Reading]
  ): List[String] = {
    val moves = (before.keySet & after.keySet).toList
      .map(key => (before(key).sizeClass, after(key).sizeClass))
      .filter(_ != _)
      .groupBy(identity)
      .toList
      .map((move, found) => (move, found.size))
      .sortBy((move, size) => (-size, move._1, move._2))
    s"== stored-value classes: ${moves.map(_._2).sum} rows moved" ::
      moves.map { case ((from, to), size) => s"  $size $from -> $to" }
  }

}
