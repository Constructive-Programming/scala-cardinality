package cardinality

import scala.meta.{Defn, Init, *}
import scala.sys.process.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.capacity.*
import cardinality.reporting.*
import cardinality.tasty.TastyStubs
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** An opt-in real-project workflow over a full cats-eo checkout, not a network-dependent unit test.
  *
  * The pinned `EoBaseline` runner measures the published core sources jar; this one reads every
  * Git-tracked production source of the project — every module, `core` included — as one combined
  * scope, and additionally re-runs `core` alone for comparison with the jar baseline. A green run
  * means the analyzer *processed* the whole project: every file parsed, the production `Report.of`
  * path agrees with the assembled report, input order cannot change a method or a stored-value
  * result, and every method and definition row has a unique identity. The counts remain analyzer
  * output, not independently validated answers.
  *
  * Supply the checkout, an output directory and the analyzer revision. The command in
  * docs/baselines/eo-checkout-2026-10-27-full.md fetches the input separately. `EoDelta` compares
  * two runs row by row.
  */
object EoCheckout {

  // The survey scope of the earlier checkout run: production module sources only. Tests, demos
  // and benchmarks are not part of what a report on the project must process.
  private val excludedRoots = List("benchmarks/", "tests/", "demos/")

  def main(args: Array[String]): Unit = {
    require(
      args.length == 3 || args.length == 4,
      "usage: EoCheckout <checkout-directory> <output-directory> <revision> [classpath-file]"
    )
    val checkout = Path.of(args(0)).toAbsolutePath.normalize()
    val output = Path.of(args(1))
    val revision = args(2)
    require(Files.isDirectory(checkout), s"not a directory: $checkout")
    val commit =
      Process("git", List("-C", checkout.toString, "rev-parse", "HEAD")).!!.trim

    val selected = trackedSources(checkout)
    val inputs = selected.map { relative =>
      val text = Files.readString(checkout.resolve(relative), UTF_8)
      MethodAnalysis.Input(relative, dialects.Scala3(text).parse[Source].get)
    }
    // Compiled dependency declarations: the classpath file lists the project's dependency jars,
    // one per line; every Scala 3 class they declare outside the stdlib model's namespaces joins
    // the environment as a signature stub, never as a report row.
    val classpath = args.lift(3).filter(_.nonEmpty).map(Path.of(_)).toList.flatMap { file =>
      Files
        .readAllLines(file, UTF_8)
        .toArray(Array.empty[String])
        .toList
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(Path.of(_))
    }
    val dependencies =
      if (classpath.isEmpty) Nil
      else {
        val stubs = TastyStubs.of(classpath, _ => true)
        // An unparseable stub is left out: its names stay unsupplied, which only refuses more.
        stubs.errors.foreach(e => println(s"stub not parsed: $e"))
        stubs.inputs
      }
    val trees = inputs.map(input => input.path -> input.tree).toMap
    def dynamicInit(init: Init): Boolean =
      init.tpe.syntax == "Dynamic" || init.tpe.syntax == "scala.Dynamic"
    // The structural-member notice: a `scala.Dynamic` parent declares no member a count reads,
    // and the structural calls its subclasses enable (selectDynamic, …) are outside the counting
    // model - a body that calls them is read without them.
    val dynamicDeclarations = inputs.count(input =>
      input.tree.collect {
        case t: (Defn.Class | Defn.Trait | Defn.Object | Defn.Enum)
            if t.templ.inits.exists(dynamicInit) =>
          1
      }.nonEmpty
    )
    if (dynamicDeclarations > 0)
      println(
        s"notice: $dynamicDeclarations declarations extend scala.Dynamic; their structural " +
          "members (selectDynamic, applyDynamic, ...) are outside the counting model"
      )

    def key(entry: MethodAnalysis.Entry) = EoBaseline.identity(entry, trees(entry.path))

    val forward = measured(inputs, dependencies).sortBy(key)
    val reverse = measured(inputs.reverse, dependencies.reverse).sortBy(key)
    require(reverse == forward, "method results depend on source input order")
    require(forward.map(key).distinct.size == forward.size, "duplicate method row identity")
    val (definitions, rows) = checkedDefinitions(inputs, dependencies)

    val report = assemble(inputs, definitions, forward)
    // The production path reads sources only; with compiled dependencies the reports differ by
    // exactly what the dependencies resolve.
    if (dependencies.isEmpty) {
      val production = Report.of(selected.map(checkout.resolve))
      require(production.errors.isEmpty, production.errors.mkString("\n"))
      require(
        production.render == report.render,
        "the production Report.of path and the assembled checkout report differ"
      )
    }

    val coreInputs = inputs.filter(_.path.startsWith("core/"))
    val coreReport = assemble(
      coreInputs,
      definitionsOf(coreInputs, dependencies),
      measured(coreInputs, dependencies)
    )

    val _ = Files.createDirectories(output)
    write(output.resolve("eo-checkout.txt"), header(commit, revision) + report.render + "\n")
    write(output.resolve("eo-checkout-core.txt"), coreReport.render + "\n")
    writeTables(output, forward.map(entry => row(entry, key(entry)._4)), rows)
    write(output.resolve("eo-checkout-blockers.tsv"), blockers(forward, coreReport))

    if (dependencies.nonEmpty)
      println(s"${dependencies.size} dependency stubs from ${classpath.size} classpath entries.")
    println(
      s"${selected.size} sources, ${report.definitions.size} definitions, " +
        s"${forward.size} signatures at $commit."
    )
    println(moduleTable(report, forward))
    println(
      (if (dependencies.isEmpty) "The production Report.of path rendered the same report; "
       else "") + "input order cannot move a result; identities unique."
    )
    println(s"Artifacts written to $output.")
  }

  private def header(commit: String, revision: String): String =
    s"""# scala-cardinality full-checkout run — cats-eo project at $commit
       |#
       |# Provisional analyzer output, not independently validated answers.
       |# analyzer: $revision
       |# versions: sbt 2.1.0-RC1, Scala ${scala.util.Properties.versionNumberString}, scalameta 4.17.4
       |# input: Git-tracked */src/main/*.scala of the checkout, excluding benchmarks/, tests/, demos/
       |# limits: MethodAnalysis.Limits(maxTypeDepth = 64, maxStates = 256); supplied sources only
       |# command: sbt 'core/Test/runMain cardinality.EoCheckout <checkout> <output> $revision'
       |# checks: every tracked source parsed; forward/reversed input results equal for methods
       |#         and stored values; unique method and definition row identities; the production
       |#         Report.of path renders the same report
       |#
       |""".stripMargin

  /** Every source's definitions and the definition rows in table order, once input order is shown
    * not to move a stored value and no two rows share an identity.
    */
  private[cardinality] def checkedDefinitions(
      inputs: List[MethodAnalysis.Input],
      dependencies: List[MethodAnalysis.Input] = Nil
  ): (Map[String, List[Definition]], List[Report.DefinitionRow]) = {
    val definitions = definitionsOf(inputs, dependencies)
    require(
      definitionsOf(inputs.reverse, dependencies.reverse) == definitions,
      "stored-value results depend on source input order"
    )
    val rows = Report(
      inputs.map(input => Report.Source(input.path, definitions(input.path))),
      Nil
    ).definitionRows.sortBy(definitionOrder)
    require(rows.map(rowIdentity).distinct.size == rows.size, "duplicate definition row identity")
    (definitions, rows)
  }

  private def writeTables(
      output: Path,
      methods: List[List[String]],
      definitions: List[Report.DefinitionRow]
  ): Unit = {
    write(output.resolve("eo-checkout-methods.tsv"), Tsv.table(MethodColumns, methods))
    write(
      output.resolve("eo-checkout-definitions.tsv"),
      Tsv.table(DefinitionColumns, definitions.map(definitionRow))
    )
  }

  val MethodColumns = List(
    "module",
    "source",
    "qualified_name",
    "kind",
    "enclosing_signature",
    "signature",
    "analyzer_count",
    "unresolved_reasons"
  )

  /** A definition row's identity is its source, qualified name, kind and parameters: the kind tells
    * a class from its companion object, and the name a member of each instance from a member of the
    * object (`O#T`, `O.T`); the line is left out, so moving a declaration does not make it a new
    * row. `reasons` is the report's own text; `reason_kinds` lists each reason tagged by its kind
    * (`imported:Schema (…)`, `param:A`, `member:M`).
    */
  val DefinitionColumns = List(
    "module",
    "source",
    "qualified_name",
    "kind",
    "params",
    "line",
    "size",
    "class",
    "reasons",
    "reason_kinds"
  )

  // The report as Report.of assembles it, but with checkout-relative paths so artifacts stay
  // repository-relative rather than carrying one developer's cache directory.
  private def assemble(
      inputs: List[MethodAnalysis.Input],
      definitions: Map[String, List[Definition]],
      entries: List[MethodAnalysis.Entry]
  ): Report = {
    val methods = entries.groupBy(_.path)
    Report(
      inputs.map(input =>
        Report.Source(input.path, definitions(input.path), methods.getOrElse(input.path, Nil))
      ),
      Nil
    )
  }

  // Every source's definitions, read against one library of the sources (and the dependency
  // stubs) in the order given.
  private[cardinality] def definitionsOf(
      inputs: List[MethodAnalysis.Input],
      dependencies: List[MethodAnalysis.Input] = Nil
  ): Map[String, List[Definition]] = {
    val library = Library.of((inputs ++ dependencies).map(_.tree))
    inputs.map(input => input.path -> Counter.definitions(input.tree, library)).toMap
  }

  // Logical work one target may spend with dependency stubs in the environment: ten times the most
  // any eo v1.0.0 target spends with the sources alone (29,366 steps). A dependency's records can
  // nest without end (a type class whose members' types open further type classes); a target that
  // exhausts the quota refuses with the budget named, like any `AnalysisQuery` target.
  private val TargetWork = 300000L

  // The targets' method rows, with the dependency stubs in the environment but never measured.
  private def measured(
      inputs: List[MethodAnalysis.Input],
      dependencies: List[MethodAnalysis.Input]
  ): List[MethodAnalysis.Entry] =
    if (dependencies.isEmpty) MethodAnalysis.analyze(inputs)
    else {
      val limits = MethodAnalysis.Limits()
      val work = new AnalysisWork(Long.MaxValue, limits.maxTypeDepth)
      val index = new MethodAnalysis.Index(inputs ++ dependencies, limits, Some(work))
      index.selected(inputs.map(_.path).toSet, Set.empty).map { target =>
        work.target(TargetWork)
        index.measure(target)
      }
    }

  private[cardinality] def row(entry: MethodAnalysis.Entry, enclosing: String): List[String] = {
    val reasons = entry.count match
      case unresolved: Inhabitation.Count.Unresolved => unresolved.reasons
      case _                                         => Nil
    List(
      module(entry.path),
      entry.path,
      entry.name,
      entry.kind,
      enclosing,
      entry.signature,
      entry.count.render,
      Tsv.list(reasons)
    )
  }

  private def kind(definition: Definition): String = definition.kind.toString.toLowerCase

  private def rowIdentity(row: Report.DefinitionRow): (String, String, String, List[String]) =
    (row.source, row.definition.name, kind(row.definition), row.definition.params)

  private def definitionOrder(row: Report.DefinitionRow): (String, Int, String, String) =
    (row.source, row.definition.line, row.definition.name, kind(row.definition))

  private[cardinality] def definitionRow(row: Report.DefinitionRow): List[String] =
    List(
      module(row.source),
      row.source,
      row.definition.name,
      kind(row.definition),
      Tsv.list(row.definition.params),
      row.definition.line.toString,
      row.size,
      row.sizeClass,
      Definition.reasons(row.definition),
      Tsv.list(row.definition.unbound.map(reason => s"${reason.tag}:${reason.subject}"))
    )

  // One line per blocker category: how many distinct signatures it stopped, combined and core.
  private def blockers(
      forward: List[MethodAnalysis.Entry],
      coreReport: Report
  ): String = {
    def categories(entries: List[MethodAnalysis.Entry]): Map[String, Set[(String, Int, String)]] =
      entries
        .flatMap { entry =>
          val reasons = entry.count match
            case unresolved: Inhabitation.Count.Unresolved => unresolved.reasons
            case _                                         => Nil
          reasons.map(reason => (Blockers.category(reason), (entry.path, entry.line, entry.name)))
        }
        .groupBy(_._1)
        .map { (category, hits) => category -> hits.map(_._2).toSet }
    val combined = categories(forward)
    val core = categories(coreReport.methods)
    val header = "category\tcombined_signatures\tcore_signatures"
    (combined.keySet ++ core.keySet).toList.sorted
      .map { category =>
        s"$category\t${combined.getOrElse(category, Set.empty).size}\t" +
          s"${core.getOrElse(category, Set.empty).size}"
      }
      .prepended(header)
      .mkString("\n") + "\n"
  }

  private def moduleTable(
      report: Report,
      forward: List[MethodAnalysis.Entry]
  ): String =
    val rows = report.sources
      .map(_.path)
      .map(module)
      .distinct
      .sorted
      .map { name =>
        val entries = forward.filter(_.path.startsWith(name + "/"))
        val definitions = report.sources
          .filter(_.path.startsWith(name + "/"))
          .map(_.definitions.size)
          .sum
        f"  $name%-14s ${entries.size}%6d signatures  ${entries.count(_.count.isFinite)}%5d finite  " +
          f"${entries.count(_.count.isCountable)}%4d countable  " +
          f"${entries.count(_.count.isBeyondCountable)}%5d beyond-e0  " +
          f"${entries.count(entry =>
              !entry.count.isFinite && !entry.count.isCountable && !entry.count.isBeyondCountable
            )}%5d unresolved  " +
          f"$definitions%5d definitions"
      }
    ("Per module:" :: rows).mkString("\n")

  private def module(path: String): String = path.takeWhile(_ != '/')

  private def trackedSources(checkout: Path): List[String] = {
    val listing = Process("git", List("-C", checkout.toString, "ls-files", "-z", "*.scala")).!!
    listing
      .split('\u0000')
      .filter(_.nonEmpty)
      .filter(_.contains("/src/main/"))
      .filterNot(path => excludedRoots.exists(path.startsWith))
      .toList
      .sorted
  }

  private def write(path: Path, text: String): Unit = {
    val _ = Files.writeString(path, text, UTF_8)
  }

}
