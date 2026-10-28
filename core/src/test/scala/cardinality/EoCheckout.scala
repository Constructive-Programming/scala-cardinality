package cardinality

import scala.meta.*
import scala.sys.process.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.capacity.*
import cardinality.reporting.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** An opt-in real-project workflow over a full cats-eo checkout, not a network-dependent unit test.
  *
  * The pinned `EoBaseline` runner measures the published core sources jar; this one reads every
  * Git-tracked production source of the project — every module, `core` included — as one combined
  * scope, and additionally re-runs `core` alone for comparison with the jar baseline. A green run
  * means the analyzer *processed* the whole project: every file parsed, the production `Report.of`
  * path agrees with the assembled report, input order cannot change a result, and every row has a
  * unique identity. The counts remain analyzer output, not independently validated answers.
  *
  * Supply the checkout, an output directory and the analyzer revision. The command in
  * docs/baselines/eo-checkout-2026-10-27-full.md fetches the input separately.
  */
object EoCheckout {

  // The survey scope of the earlier checkout run: production module sources only. Tests, demos
  // and benchmarks are not part of what a report on the project must process.
  private val excludedRoots = List("benchmarks/", "tests/", "demos/")

  def main(args: Array[String]): Unit = {
    require(
      args.length == 3,
      "usage: EoCheckout <checkout-directory> <output-directory> <revision>"
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
    val trees = inputs.map(input => input.path -> input.tree).toMap
    def key(entry: MethodAnalysis.Entry) = EoBaseline.identity(entry, trees(entry.path))

    val library = Library.of(inputs.map(_.tree))
    val forward = MethodAnalysis.analyze(inputs).sortBy(key)
    val reverse = MethodAnalysis.analyze(inputs.reverse).sortBy(key)
    require(reverse == forward, "method results depend on source input order")
    require(forward.map(key).distinct.size == forward.size, "duplicate method row identity")

    val report = assemble(inputs, library, forward)
    val production = Report.of(selected.map(checkout.resolve))
    require(production.errors.isEmpty, production.errors.mkString("\n"))
    require(
      production.render == report.render,
      "the production Report.of path and the assembled checkout report differ"
    )

    val coreInputs = inputs.filter(_.path.startsWith("core/"))
    val coreReport = assemble(
      coreInputs,
      Library.of(coreInputs.map(_.tree)),
      MethodAnalysis.analyze(coreInputs)
    )

    val header =
      s"""# scala-cardinality full-checkout run — cats-eo project at $commit
         |#
         |# Provisional analyzer output, not independently validated answers.
         |# analyzer: $revision
         |# versions: sbt 2.1.0-RC1, Scala ${scala.util.Properties.versionNumberString}, scalameta 4.17.4
         |# input: Git-tracked */src/main/*.scala of the checkout, excluding benchmarks/, tests/, demos/
         |# limits: MethodAnalysis.Limits(maxTypeDepth = 64, maxStates = 256); supplied sources only
         |# command: sbt 'core/Test/runMain cardinality.EoCheckout <checkout> <output> $revision'
         |# checks: every tracked source parsed; forward/reversed input results equal; unique row
         |#         identities; the production Report.of path renders the same report
         |#
         |""".stripMargin

    val _ = Files.createDirectories(output)
    write(output.resolve("eo-checkout.txt"), header + report.render + "\n")
    write(output.resolve("eo-checkout-core.txt"), coreReport.render + "\n")
    write(
      output.resolve("eo-checkout-methods.tsv"),
      "module\tsource\tqualified_name\tkind\tenclosing_signature\tsignature\tanalyzer_count\tunresolved_reasons\n" +
        forward.map(entry => row(entry, key(entry)._4)).mkString("\n") + "\n"
    )
    write(output.resolve("eo-checkout-blockers.tsv"), blockers(forward, coreReport))

    println(
      s"${selected.size} sources, ${report.definitions.size} definitions, " +
        s"${forward.size} signatures at $commit."
    )
    println(moduleTable(report, forward))
    println(
      "The production Report.of path rendered the same report; input order cannot move a " +
        "result; identities unique."
    )
    println(s"Artifacts written to $output.")
  }

  // The report as Report.of assembles it, but with checkout-relative paths so artifacts stay
  // repository-relative rather than carrying one developer's cache directory.
  private def assemble(
      inputs: List[MethodAnalysis.Input],
      library: Library,
      entries: List[MethodAnalysis.Entry]
  ): Report = {
    val methods = entries.groupBy(_.path)
    Report(
      inputs.map(input =>
        Report.Source(
          input.path,
          Counter.definitions(input.tree, library),
          methods.getOrElse(input.path, Nil)
        )
      ),
      Nil
    )
  }

  private def row(entry: MethodAnalysis.Entry, enclosing: String): String = {
    val reasons = entry.count match
      case unresolved: Inhabitation.Count.Unresolved => unresolved.reasons.mkString("; ")
      case _                                         => ""
    List(
      module(entry.path),
      entry.path,
      entry.name,
      entry.kind,
      enclosing,
      entry.signature,
      entry.count.render,
      reasons
    ).mkString("\t")
  }

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
          reasons.map(reason => (reason.split(':').head, (entry.path, entry.line, entry.name)))
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
          f"${entries.count(entry => !entry.count.isFinite && !entry.count.isCountable)}%5d unresolved  " +
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
