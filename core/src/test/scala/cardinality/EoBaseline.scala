package cardinality

import scala.jdk.CollectionConverters.*
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.capacity.*
import cardinality.reporting.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.zip.ZipFile

/** An opt-in real-library workflow, not a network-dependent unit test.
  *
  * Supply the pinned sources jar, an output directory and the analyzer revision. The command in
  * docs/baselines/eo-core-0.16.0-review.md downloads the input separately.
  */
object EoBaseline {

  private given Dialect = dialects.Scala3

  private val checksum =
    "61907fc2f4e1c0892fa8723c7a014af942b825b64a916a9b53dd64b3f12dbfe3"

  // Independent derivations live in the review ledger, not in the analyzer's own output.
  private val reviewed = Map(
    ("dev.constructive.eo.CanGet.get", "declaration", "get(s: S): A") ->
      Inhabitation.Count.Finite(0),
    ("dev.constructive.eo.CanGetOption.getOption", "declaration", "getOption(s: S): Option[A]") ->
      Inhabitation.Count.Finite(1),
    ("dev.constructive.eo.CanPlace.place", "declaration", "place(b: B): T => T") ->
      Inhabitation.Count.Finite(1),
    ("dev.constructive.eo.CanPlace.transfer", "method", "transfer[C](f: C => B): T => C => T") ->
      Inhabitation.Count.Countable,
    ("dev.constructive.eo.CanModifyP.replace", "method", "replace(b: B): S => T") ->
      Inhabitation.Count.Finite(1)
  )

  def main(args: Array[String]): Unit = {
    require(args.length == 3, "usage: EoBaseline <sources.jar> <output-directory> <revision>")
    val jar = Path.of(args(0))
    val output = Path.of(args(1))
    val revision = args(2)
    verifyChecksum(Files.readAllBytes(jar))

    val inputs = readInputs(jar)
    val report = Report.of(Seq(jar))
    require(report.errors.isEmpty, report.errors.mkString("\n"))
    require(inputs.size == 53, s"expected 53 Scala sources, found ${inputs.size}")
    val trees = inputs.map(input => input.path -> input.tree).toMap
    def key(entry: MethodAnalysis.Entry) = identity(entry, trees(entry.path))
    val forward = MethodAnalysis.analyze(inputs).sortBy(key)
    val reverse = MethodAnalysis.analyze(inputs.reverse).sortBy(key)
    require(forward == reverse, "method results depend on source input order")
    require(report.methods.sortBy(key) == forward, "report and direct analysis differ")
    require(forward.map(key).distinct.size == forward.size, "duplicate method row identity")
    reviewed.foreach { (id, expected) =>
      val found = forward.filter(entry => (entry.name, entry.kind, entry.signature) == id)
      require(found.size == 1, s"reviewed row missing or ambiguous: $id")
      require(found.head.count == expected, s"reviewed count changed for $id: ${found.head.count}")
    }

    val selected = Report.query(
      Report.Query(
        Seq(jar),
        targetNames = reviewed.keys.map(_._1).toSet
      )
    )
    require(selected.report.errors.isEmpty, selected.report.errors.mkString("\n"))
    require(
      selected.report.methods.size == reviewed.size,
      "selected query measured unrelated targets"
    )
    reviewed.foreach { (id, expected) =>
      val found =
        selected.report.methods.filter(entry => (entry.name, entry.kind, entry.signature) == id)
      require(found.size == 1 && found.head.count == expected, s"selected query differs for $id")
    }

    val header =
      s"""# scala-cardinality baseline — dev.constructive:cats-eo_3:0.16.0, sources jar
         |#
         |# Provisional analyzer output, not independently validated answers.
         |# Review statuses and derivations: eo-core-0.16.0-review.md.
         |# analyzer: $revision (production code; opt-in baseline runner added on top)
         |# versions: sbt 2.0.9, Scala 3.8.4, scalameta 4.17.4
         |# input: https://repo1.maven.org/maven2/dev/constructive/cats-eo_3/0.16.0/cats-eo_3-0.16.0-sources.jar
         |# sha256: $checksum
         |# limits: MethodAnalysis.Limits(maxTypeDepth = 64, maxStates = 256); supplied sources only
         |# command: sbt 'core/Test/runMain cardinality.EoBaseline <sources.jar> <output-directory> $revision'
         |# checks: 53 parsed sources; unique method identities; forward/reversed input results equal;
         |#         five independently derived model counts agree with the full-source report
         |#
         |""".stripMargin
    val rows = forward.map { entry =>
      val (path, name, kind, enclosing, signature) = key(entry)
      val status =
        if (reviewed.contains((entry.name, entry.kind, entry.signature))) "model-reviewed"
        else "unreviewed"
      List(path, name, kind, enclosing, signature, entry.count.render, status).mkString("\t")
    }
    val aggregates = inputs.map { input =>
      s"${input.path}\t${Counter.sourceSignature(input.tree).render}"
    }
    val _ = Files.createDirectories(output)
    write(output.resolve("eo-core-0.16.0.txt"), header + report.render + "\n")
    write(
      output.resolve("eo-core-0.16.0-methods.tsv"),
      "source\tqualified_name\tkind\tenclosing_signature\tsignature\tanalyzer_count\treview_status\n" +
        rows.mkString("\n") + "\n"
    )
    write(
      output.resolve("eo-core-0.16.0-signatures.tsv"),
      "# Legacy declared-signature estimates, NOT parametric implementation counts.\n" +
        "source\tdeclared_signature_estimate\n" + aggregates.mkString("\n") + "\n"
    )
    println(
      s"${report.sources.size} sources, ${report.definitions.size} definitions, " +
        s"${forward.size} signatures. Five reviewed model counts and input-order checks passed."
    )
    println(s"Baseline artifacts written to $output.")
  }

  private[cardinality] def verifyChecksum(bytes: Array[Byte]): Unit = {
    val actual = MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
    require(actual == checksum, s"EO sources checksum mismatch: $actual")
  }

  private[cardinality] def identity(
      entry: MethodAnalysis.Entry,
      tree: Source
  ): (String, String, String, String, String) = {
    // Anonymous owners in the human-readable report contain line numbers. Replace those with
    // their signature context so moving a declaration does not change its review identity.
    val enclosing = tree
      .collect {
        case group: Defn.ExtensionGroup if contains(group, entry.line) =>
          "extension " + group.paramClauseGroup.toList.map(_.syntax).mkString(" ")
        case givenDef: Defn.Given if contains(givenDef, entry.line) =>
          "given " + givenDef.paramClauseGroups.map(_.syntax).mkString(" ") + ": " +
            givenDef.templ.inits.map(_.syntax).mkString(" with ")
      }
      .mkString(" / ")
      .replaceAll("\\s+", " ")
      .trim
    (
      entry.path,
      entry.name.replaceAll("<(extension|given)@\\d+>", "<$1>"),
      entry.kind,
      enclosing,
      entry.signature
    )
  }

  private def contains(tree: Tree, line: Int): Boolean =
    tree.pos.startLine + 1 <= line && line <= tree.pos.endLine + 1

  private def write(path: Path, text: String): Unit = {
    val _ = Files.writeString(path, text, UTF_8)
  }

  private def readInputs(jar: Path): List[MethodAnalysis.Input] = {
    val archive = new ZipFile(jar.toFile)
    try
      archive
        .entries()
        .asScala
        .filter(entry => !entry.isDirectory && entry.getName.endsWith(".scala"))
        .toList
        .sortBy(_.getName)
        .map { entry =>
          val stream = archive.getInputStream(entry)
          val text = try new String(stream.readAllBytes(), UTF_8)
          finally stream.close()
          val tree = dialects.Scala3(text).parse[Source].get
          MethodAnalysis.Input(entry.getName, tree)
        }
    finally archive.close()
  }

}
