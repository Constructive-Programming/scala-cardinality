package cardinality.compiled

import scala.jdk.CollectionConverters.*

import cardinality.analysis.MethodAnalysis.Entry
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.Definition
import cardinality.reporting.{Blockers, FrontEnd, Report, ScalametaFrontEnd}
import cardinality.tasty.CompiledTypes
import java.nio.file.{Files, Path}

/** The differential gate of the front-end rewrite (docs/plans/compiled-own-types.md, phase 4b): two
  * front ends read the same input, and every row of either is classified.
  *
  * A row of the new front end is its own or it is `Uncovered`: the new front end never asks the old
  * one, so an identical row is one the new front end measured itself. A row only one side has fails
  * the gate, and so does a `Worse` row that is not a listed correction: a number may change only
  * where the old one is derivably wrong, and each such row is listed with both values and its
  * derivation (`--corrected`). An upper bound that became an exact number within it is `Tightened`,
  * and passes. The runner also fails on anything a front end could not read, on an input it reads
  * no source of, on a row it measured before and leaves uncovered or counts otherwise now
  * (`--covered`), and on a listed correction that passes no row.
  *
  * The old front end is no oracle where it says `?`, so a `Better` row is read by why it said so.
  * Where every reason is a limit of reading syntax (`resolution`), a number is expected: the run
  * prints one row per reason to derive as its sample. Where a reason is a limit of the counting
  * model, the new front end counts what the model refuses: the row fails the gate until it is
  * derived by hand and listed with its count and the rule that makes it countable (`--derived`). A
  * row paired by a looser key than its name, kind and line is listed to be explained. Parity is
  * every old numeric row `Identical`, `Tightened` or a listed correction; until then the summary
  * says how many are.
  */
object Differential {

  enum Verdict {

    /** The same count, or `?` on both sides. */
    case Identical

    /** `?` before, a number now. */
    case Better

    /** An upper bound before (`≤2^32`), an exact number within it now (`2^32`; `257` under `≤2^9`):
      * the measurement is tighter and nothing the bound promised is broken.
      */
    case Tightened

    /** A number before, and `?` or another number now: it fails unless it is a listed correction.
      */
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
      reasons: List[String],
      causes: List[String],
      captures: List[String] = Nil
  ) {
    def numeric: Boolean = reasons.isEmpty

    /** What places the row, whichever front end made it and whatever it counts. */
    def key: String = s"$table $source:$line $name [$kind]"

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
        reasons,
        reasons.map(Blockers.category),
        entry.captures
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
        reasons,
        // A refusal reads like a method row's reason; any other is its tag.
        definition.unbound.map {
          case Definition.Unbound.Refusal(what) => Blockers.category(what)
          case reason                           => s"definition ${reason.tag}"
        }
      )
    }

    def all(rows: FrontEnd.Rows): List[Row] = rows.sources.flatMap {
      case Right(source) =>
        source.methods.map(of) ++ source.definitions.map(of(source.path, _))
      case Left(_) => Nil
    }

  }

  // The refusals that are limits of reading syntax, by category (`Blockers.category`; a definition
  // row's by its reason's tag). Any other is a limit of the counting model.
  private val resolution = Set(
    "declaration not supplied",
    "unresolved type",
    "inherited declaration identity not resolved",
    "inherited override identity not resolved",
    "inherited type member not resolved",
    "inherited term member not resolved",
    "inherited parent declaration not resolved",
    "inherited parent declaration not supplied",
    "member may be overridden through a parent outside the sources",
    "sealed subtype reaches its parent only through a qualified name",
    "sealed family is extended through an alias",
    "path through a value not followed",
    "inferred result type not resolved",
    "construction not resolved",
    "wildcard export not supplied",
    "export not followed",
    "definition unknown",
    "definition imported"
  )

  // Whether one reason of the old front end is a limit of reading syntax. A builtin name something
  // may shadow is one, however the reason words it, and a union's reason is its alternative's. A
  // `transparent inline` result stays refused whatever the compiler knows (rule X), and a
  // refinement is a form the estimate does not read, not a name it could not resolve.
  private def syntactic(cause: String, reason: String): Boolean = {
    val category = cause.stripPrefix("union _: ")
    (resolution(category) || category.startsWith("builtin name") ||
      (category == Unsummed && unattached.exists(reason.contains))) &&
    !reason.contains("transparent inline") && !reason.contains("a refinement")
  }

  // The old estimate's refusal to sum a sealed family, and the causes of it that are limits of
  // reading syntax: a subtype it could not attach (written through an alias or a renamed import,
  // or under a parent it was not given, which may rebind the family's name), a bottom pin whose
  // name a source may rebind.
  private val Unsummed = "sealed family not summed"

  private val unattached = List(
    "a subtype extends it through an alias",
    "a subtype extends it through a renamed import",
    "a subtype whose clause may name another",
    ", which a source binds"
  )

  /** One row compared: its verdict, each side's row, and what paired them. */
  final case class Compared(verdict: Verdict, old: Option[Row], next: Option[Row], by: String) {
    def row: Row = old.orElse(next).get

    /** A better row that counts what the model refused: one of the old reasons is no limit of
      * reading syntax.
      */
    def beyondModel: Boolean =
      verdict == Verdict.Better &&
        !old.exists(row => row.causes.lazyZip(row.reasons).forall(syntactic))

    private def heading: String =
      if (verdict != Verdict.Better) verdict.toString.toUpperCase
      else s"BETTER, past a ${if (beyondModel) "model" else "resolution"} limit"

    def render: String =
      s"$heading (paired by $by)" +
        captured(this).collect { case Left(names) => s"\n    CAPTURES DIFFER: $names" }.mkString +
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
    else if (old.shown.startsWith(Bound) && within(next.shown, old.shown.stripPrefix(Bound)))
      Verdict.Tightened
    else Verdict.Worse

  // How a count that is an upper bound prints.
  private val Bound = "≤"

  // Whether an exact count is within a bound, each as it prints: the same text, or two numbers
  // (a literal or a power of two) the first no greater than the second. A value above the bound
  // is never tightened.
  private def within(exact: String, bound: String): Boolean = {
    val Power = """2\^(\d+)""".r
    // A number as its exponent of two, or itself: enough to order a literal against a power.
    def number(shown: String): Option[Either[BigInt, BigInt]] = shown match {
      case Power(exponent)                  => Some(Left(BigInt(exponent)))
      case digits if digits.matches("\\d+") => Some(Right(BigInt(digits)))
      case _                                => None
    }
    exact == bound || number(exact).zip(number(bound)).exists {
      case (Left(a), Left(b))   => a <= b
      case (Right(a), Right(b)) => a <= b
      case (Right(a), Left(b))  => (a - 1).bitLength <= b
      case (Left(a), Right(b))  => a < b.bitLength
    }
  }

  /** How the captures of a row both front ends count differ, if they do (a refused row lists none,
    * on either side): `Right` one of the classes the coordinator listed, `Left` the two listings
    * where neither holds the other, which fails the gate. The classes are told by the names alone:
    * that the old front end's extra name is an alias of a capture, or that it did not bind a name
    * the new one lists, is not checked row by row. The report prints captures.
    */
  def captured(c: Compared): Option[Either[String, String]] =
    c.old.zip(c.next).collect {
      case (old, next) if old.numeric && next.numeric && old.captures != next.captures =>
        // A later overload is listed as `name#2` by the old front end, by its name here.
        val before = old.captures.map(_.replaceAll("#\\d+$", "")).toSet
        val after = next.captures.toSet
        if (before == after) Right("a second overload by its name")
        else if (after.subsetOf(before)) Right("the old lists more: an alias of a capture")
        else if (before.subsetOf(after)) Right("the new lists more: names the old did not bind")
        else Left(s"${old.captures.mkString(", ")} -> ${next.captures.mkString(", ")}")
    }

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
        val beyond = rows.count(_.beyondModel)
        List(
          s"$table rows: $olds old, ${nexts.size} new",
          "  " + Verdict.values.map(v => s"${by(v)} ${v.toString.toLowerCase}").mkString(" · "),
          s"  better: ${by(Verdict.Better) - beyond} past a resolution limit, " +
            s"$beyond past a model limit",
          s"  old numeric rows: ${numeric.size}, of which " +
            Verdict.values
              .map(v => s"${numeric.count(_.verdict == v)} ${v.toString.toLowerCase}")
              .mkString(" · "),
          s"  measured by the new front end: $own of ${nexts.size} rows" +
            (if (olds == 0) "" else f" (${100.0 * own / olds}%.1f%% of the old rows)"),
          s"  paired: ${pairing.mkString(" · ")}; $signatures paired rows spell another signature"
        ) ++ rows
          .flatMap(captured)
          .groupBy(_.fold(_ => "unlisted", identity))
          .toList
          .sortBy(_._1)
          .map((kind, differing) => s"  captures differ: ${differing.size} $kind")
      }
      .mkString("\n")

  /** The rows that fail the gate: one only one side has; a better one past a model limit that
    * `derived` does not list with the count it has now (row key to count); and a worse one that
    * `corrected` does not list with both its values (row key to `old -> new`). A number may change
    * only where the old one is derivably wrong; the file gives each such row's derivation.
    */
  def failures(
      compared: List[Compared],
      derived: Map[String, String] = Map.empty,
      corrected: Map[String, String] = Map.empty
  ): List[Compared] = {
    val accepted = corrections(compared, corrected).toSet
    compared.filter { c =>
      Set(Verdict.OnlyOld, Verdict.OnlyNew)(c.verdict) ||
      (c.verdict == Verdict.Worse && !accepted(c)) || captured(c).exists(_.isLeft) ||
      (c.beyondModel && !c.next.exists(row => derived.get(c.row.key).contains(row.shown)))
    }
  }

  /** The worse rows `corrected` lists with the values they have: the old one and the new one, then
    * `:` and the derivation (`corrected` holds each line whole). A value that is no number is
    * listed with its reasons (`? A; B`).
    */
  def corrections(compared: List[Compared], corrected: Map[String, String]): List[Compared] = {
    def value(row: Row) =
      if (row.numeric) row.shown else s"${row.shown} ${row.reasons.mkString("; ")}"
    compared.filter(c =>
      c.verdict == Verdict.Worse && c.old.zip(c.next).exists { (old, next) =>
        val values = s"${value(old)} -> ${value(next)}"
        corrected.get(c.row.key).exists(line => line == values || line.startsWith(s"$values: "))
      }
    )
  }

  /** The corrections listed for a source of this comparison that pass no row: the row is no longer
    * worse, has other values, or is gone. A list of numbers changed on purpose holds no dead line.
    * `holdsAll` says which keys this run holds every source of: a fixture's key starts with the
    * hash of its text, so one whose fixture changed names a source that no run holds any more.
    */
  def unused(
      compared: List[Compared],
      corrected: Map[String, String],
      holdsAll: String => Boolean = _ => false
  ): List[String] = {
    val used = corrections(compared, corrected).map(_.row.key).toSet
    val sources = compared.map(c => s"${c.row.table} ${c.row.source}:").toSet
    corrected.keys
      .filter(key => !used(key) && (holdsAll(key) || sources.exists(key.startsWith)))
      .toList
      .sorted
  }

  // The key of a row of a compiled fixture: its source is under the hash of the fixture's text.
  private def ofFixture(key: String): Boolean = key.matches("\\w+ [0-9a-f]{64}/.*")

  /** The rows to explain: one whose two sides differ in name, kind or scope (what paired them). */
  def explained(compared: List[Compared]): List[Compared] =
    compared.filter(c => c.old.nonEmpty && c.next.nonEmpty && c.by != Strict)

  /** The better rows past a resolution limit by the old reasons' categories, most frequent first,
    * each with one row to derive as its sample.
    */
  def samples(compared: List[Compared]): List[(String, Int, Compared)] =
    compared
      .filter(c => c.verdict == Verdict.Better && !c.beyondModel)
      .groupBy(_.old.toList.flatMap(_.causes).distinct.sorted.mkString("; "))
      .toList
      .map((causes, rows) => (causes, rows.size, rows.minBy(_.row.key)))
      .sortBy((causes, count, _) => (-count, causes))

  /** The rows the new front end answers otherwise than at the last passing run (`previous`, row key
    * to count): one it measured and leaves uncovered now, or one it counts differently. The old
    * front end does not catch the second where it says `?` itself.
    */
  def regressed(compared: List[Compared], previous: Map[String, String]): List[Compared] =
    compared.filter(c =>
      c.next.exists(row =>
        previous.get(row.key).exists(before => row.uncovered || before != row.shown)
      )
    )

  /** The rows the new front end measured itself, each key with its count. Two rows of one key (two
    * overloads on a line) are kept only where both are measured alike: the key cannot tell them
    * apart.
    */
  def measured(compared: List[Compared]): List[(String, String)] =
    compared
      .flatMap(_.next)
      .groupBy(_.key)
      .toList
      .collect {
        case (key, rows) if rows.forall(!_.uncovered) && rows.map(_.shown).distinct.sizeIs == 1 =>
          key -> rows.head.shown
      }

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

  /** One input: the label put before its rows' sources (to tell one fixture's from another's), and
    * the module each front end reads. They differ for a fixture with withheld support only: the old
    * front end spells its compiled types against the support's classes, and the new one is given
    * nothing of what is withheld.
    */
  final case class Input(label: String, old: FrontEnd.Module, next: FrontEnd.Module)

  /** One input compared: its rows, what a front end could not read (a source, a class, the module's
    * compiled types or its classpath), and what the old front end said of one dependency
    * declaration it left unsupplied, which only refuses more on its side.
    */
  final case class Outcome(compared: List[Compared], failed: List[String], noted: List[String])

  /** A module as the build plugins hand it over (a frozen corpus): every Scala source under `root`,
    * each at a path that ends with the one its classes record; `classes`; and `classpath.txt`, one
    * dependency jar per line.
    */
  def corpus(root: Path): Input = {
    val classpath = Files.readAllLines(root.resolve("classpath.txt")).asScala.toList.map(Path.of(_))
    val classes = CompiledTypes.Classes(List(root.resolve("classes")), classpath)
    val module = FrontEnd.Module(
      files(root, _.toString.endsWith(".scala")),
      Report.Environment(classpath, None, Some(classes))
    )
    Input(s"${root.getFileName}/", module, module)
  }

  /** The compiled test fixtures under `cache` (`cardinality.Compiled`'s): each `<key>/src` with its
    * `classes`, read against the class path its `classpath.txt` records (`libraries` where an older
    * entry records none), under the paths the tests gave. A directory `<key>.<thread>` is a
    * compile's staging the cache left behind, not a fixture. Sources under `src/withheld` and their
    * classes in `support` are what the fixture compiled against and the analysis is not given.
    * Sources under `src/dependency/<unit>` are a dependency: both front ends are given their
    * classes (`dependency/<unit>`), neither their sources.
    */
  def fixtures(cache: Path, libraries: List[Path]): List[Input] =
    list(cache).filter(fixture).map { dir =>
      val src = dir.resolve("src")
      // A test names a fixture's source as it likes: every file but the withheld support and the
      // dependency units, which are compiled apart and given as classes.
      val apart = Set(cardinality.Compiled.Withheld, cardinality.Compiled.Dependency)
        .map(_.stripSuffix("/"))
      val sources = files(src, path => !apart(path.getName(0).toString))
      def directories(name: String) = List(dir.resolve(name)).filter(Files.isDirectory(_))
      val units = directories("dependency").flatMap(list)
      // What the fixture was compiled against beside its support and units, where it is recorded.
      val classpathFile = dir.resolve("classpath.txt")
      val beside =
        if (Files.exists(classpathFile))
          Files.readAllLines(classpathFile).asScala.toList.filter(_.nonEmpty).map(Path.of(_))
        else libraries
      def read(classpath: List[Path]) = FrontEnd.Module(
        sources,
        Report.Environment(classes =
          Some(CompiledTypes.Classes(List(dir.resolve("classes")), classpath))
        )
      )
      Input(
        s"${dir.getFileName}/",
        read(directories("support") ++ units ++ beside),
        read(units ++ beside)
      )
    }

  private def fixture(dir: Path): Boolean =
    !dir.getFileName.toString.contains('.') && Files.exists(dir.resolve("ok"))

  private def list(directory: Path): List[Path] = {
    val found = Files.list(directory)
    try found.iterator().asScala.toList.sortBy(_.toString)
    finally found.close()
  }

  // Every file under `root` that `source` accepts, at its path below it, with its text.
  private def files(root: Path, source: Path => Boolean): List[(String, String)] = {
    val found = Files.walk(root)
    try
      found
        .iterator()
        .asScala
        .filter(file => Files.isRegularFile(file) && source(root.relativize(file)))
        .toList
        .sortBy(_.toString)
        .map(file => root.relativize(file).toString -> Files.readString(file))
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
    val next = answer(TastyFrontEnd, input.next)
    def failed(side: String, rows: FrontEnd.Rows) = rows.sources.collect {
      case Left(error) => s"$side could not read ${input.label}${error.path}: ${error.message}"
    }
    def labelled(row: Row) = row.copy(source = input.label + row.source)
    val sourceless =
      List(s"no source read in ${input.label}").filter(_ => input.next.sources.isEmpty)
    Outcome(
      compare(old, next).map(c => c.copy(old = c.old.map(labelled), next = c.next.map(labelled))),
      sourceless ++ failed("scalameta", old) ++ failed("tasty", next) ++
        old.warnings.filter(whole).map(warning => s"scalameta warns in ${input.label}: $warning") ++
        next.warnings.map(warning => s"tasty warns in ${input.label}: $warning"),
      old.warnings.filterNot(whole).map(warning => s"scalameta notes in ${input.label}: $warning")
    )
  }

  // A warning of the old front end that it lost the module's compiled types or its whole
  // classpath: every row it then refuses would read as a limit of resolution.
  private def whole(warning: String): Boolean =
    warning.startsWith("compiled types not read") ||
      warning.startsWith("dependency classpath not read")

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
      val rows = answer(ScalametaFrontEnd, input.old)
      // An answer that read every source and its whole environment is worth keeping; a failure is
      // computed again.
      file.filter(_ => rows.sources.forall(_.isRight) && !rows.warnings.exists(whole)).foreach {
        path =>
          val out = new java.io.ObjectOutputStream(Files.newOutputStream(path))
          try out.writeObject(rows)
          finally out.close()
      }
      rows
  }

  /** What a file of rows lists, one per line: the row's key, which ends with its kind in brackets,
    * ` | `, and its count up to a `:` (a derivation may follow it).
    */
  def listed(lines: Iterable[String], whole: Boolean = false): Map[String, String] =
    lines
      .map(_.split(Bar, 2))
      .collect {
        case Array(key, rest) => key -> (if (whole) rest else rest.takeWhile(_ != ':'))
      }
      .toMap

  // What parts a listed line: the bar after the kind that ends a row's key.
  private val Bar = "(?<=\\]) \\| "

  /** The lines of a hand-written list (`--derived`, `--corrected`) that `listed` would misread: one
    * that is no `key | value: derivation`, and a key listed twice (the last would win silently).
    */
  def malformed(lines: Iterable[String]): List[String] = {
    val written = lines.filter(_.trim.nonEmpty).toList
    val keys = written.map(_.split(Bar, 2).head)
    written.zip(keys).collect {
      case (line, key) if !line.matches(".*\\] \\| [^:]+: .+") || keys.count(_ == key) > 1 => line
    }
  }

  /** A row of the coverage memory as `listed` reads it back. */
  def line(key: String, count: String): String = s"$key | $count"

  private def recorded(file: Path, whole: Boolean = false): Map[String, String] =
    if (Files.exists(file)) listed(Files.readAllLines(file).asScala, whole) else Map.empty

  private val flags = Set("--rows", "--old", "--covered", "--derived", "--corrected", "--out")

  /** `corpus <root>` or `fixtures <cache>`, then:
    *   - `--rows` lists every row with the front end that produced it;
    *   - `--old <file>` keeps the scalameta rows of a corpus between runs;
    *   - `--covered <file>` reads the rows the new front end measured at the last passing run, each
    *     with its count; fails when one is uncovered or counted otherwise now; and, when the run
    *     passes, writes the rows measured now beside the lines of rows this run did not see (to
    *     accept a changed count, delete its line);
    *   - `--derived <file>` names the better rows past a model limit that were derived by hand, one
    *     per line: the row's key as the run prints it, ` | `, its count, `:`, and the rule that
    *     makes it countable;
    *   - `--corrected <file>` names the rows whose number changes because the old one is wrong, one
    *     per line: the row's key, ` | `, `old -> new` as the run prints the two values, `:`, and
    *     the derivation;
    *   - `--out <file>` writes what the run prints to a file instead.
    *
    * Prints the totals, what the uncovered rows need, what was not read, a sample of the better
    * rows per old reason, the rows measured before that are uncovered or counted otherwise now, the
    * listed corrections and those that pass no row, the tightened rows, the rows that fail the gate
    * and the rows to explain; exits with 1 when a row fails the gate, a row measured before is
    * uncovered or counted otherwise, a correction passes no row, a front end could not read
    * something, or an input has no source.
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
    val usage = "usage: Differential (corpus <root> | fixtures <cache>) [--rows] [--old <file>] " +
      "[--covered <file>] [--derived <file>] [--corrected <file>] [--out <file>]"
    val inputs = args.toList match {
      case "corpus" :: root :: _    => List(corpus(Path.of(root)))
      case "fixtures" :: cache :: _ => fixtures(Path.of(cache), libraries)
      case _                        => sys.error(usage)
    }
    require(args.filter(_.startsWith("--")).forall(flags), usage)
    // A flag that takes a file is followed by one.
    require(
      args
        .zipAll(args.drop(1), "", "--")
        .forall((flag, value) => flag == "--rows" || !flags(flag) || !value.startsWith("--")),
      usage
    )
    require(inputs.nonEmpty, "no input: no fixture compiled under the cache")
    def option(name: String) = args.sliding(2).collectFirst { case Array(`name`, v) => Path.of(v) }
    require(option("--old").isEmpty || inputs.size == 1, "--old keeps one input's rows: a corpus")
    val out = option("--out").fold(System.out)(file => new java.io.PrintStream(file.toFile))
    if (Console.withOut(out)(report(inputs, args.contains("--rows"), option))) sys.exit(1)
  }

  // Prints one run; whether it fails.
  private def report(
      inputs: List[Input],
      rows: Boolean,
      option: String => Option[Path]
  ): Boolean = {
    val started = System.nanoTime
    val outcomes =
      inputs.asJava
        .parallelStream()
        .map(input => run(input, kept(option("--old"), input)))
        .toList
        .asScala
        .toList
    val compared = outcomes.flatMap(_.compared)
    println(summary(compared))
    println(f"  ${inputs.size} inputs in ${(System.nanoTime - started) / 1e9}%.1f s")
    println("uncovered rows need:")
    needs(compared).foreach((reason, count) => println(f"  $count%6d  $reason"))
    val unread = outcomes.flatMap(_.failed)
    (unread ++ outcomes.flatMap(_.noted)).foreach(println)
    println("better rows past a resolution limit, by the old reasons, each with one to derive:")
    samples(compared).foreach { (causes, count, sample) =>
      println(f"  $count%6d  $causes\n${sample.render}")
    }
    val previous = option("--covered").fold(Map.empty)(recorded(_))
    val lost = regressed(compared, previous)
    lost.foreach { c =>
      println("MEASURED BEFORE, UNCOVERED OR COUNTED OTHERWISE NOW")
      println(s"    ${c.next.get.render}\n    key: ${c.next.get.key}")
    }
    val corrected = option("--corrected").fold(Map.empty)(recorded(_, whole = true))
    val failed = failures(compared, option("--derived").fold(Map.empty)(recorded(_)), corrected)
    // Every number this run changes on purpose is printed: the owner sees each one.
    corrections(compared, corrected).foreach { c =>
      println(s"CORRECTED, listed with both values\n${c.render}\n    key: ${c.row.key}")
    }
    // A run over the fixtures holds every fixture: a fixture's key no row has is of a changed one.
    val overFixtures = compared.exists(c => ofFixture(c.row.key))
    val dead = unused(compared, corrected, key => overFixtures && ofFixture(key))
    dead.foreach(key => println(s"CORRECTION THAT PASSES NO ROW\n    key: $key"))
    val misread = List("--derived", "--corrected")
      .flatMap(option)
      .filter(Files.exists(_))
      .flatMap(list => malformed(Files.readAllLines(list).asScala))
    misread.foreach(line => println(s"MALFORMED LINE OF A LIST\n    $line"))
    val tightened = compared.filter(_.verdict == Verdict.Tightened)
    val shown = if (rows) compared else (failed ++ explained(compared) ++ tightened).distinct
    shown.sortBy(c => (c.row.source, c.row.line, c.row.name)).foreach { c =>
      println(s"${c.render}\n    key: ${c.row.key}")
    }
    val failing =
      failed.nonEmpty || unread.nonEmpty || lost.nonEmpty || dead.nonEmpty || misread.nonEmpty
    // A passing run rewrites the memory of the rows it saw and keeps what it holds of rows it did
    // not see (a fixture another checkout has, a run over part of the cache).
    if (!failing) option("--covered").foreach { file =>
      val seen = compared.flatMap(_.next).map(_.key).toSet
      val unseen = previous.view.filterKeys(!seen(_)).toList
      Files.write(file, (unseen ++ measured(compared)).sortBy(_._1).map(line).asJava)
    }
    failing
  }

}
