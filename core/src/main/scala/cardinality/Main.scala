package cardinality

import scala.annotation.tailrec

import cardinality.reporting.Report
import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

/** Command-line face of the calculator, for build tools that cannot load Scala 3 classes in their
  * own JVM: the sbt 1 plugin runs on Scala 2.12, so it forks this instead.
  *
  * {{{
  * cardinality.Main --out <file> [--classpath <a:b:…>] [--select <name>]… [--support <path>]… <source>…
  * }}}
  *
  * Without `--select` it prints and writes the report over the sources; with it, the selected
  * implementation report for those qualified method names. Exits 1 when the report has errors.
  */
object Main {

  final case class Args(
      out: Option[Path] = None,
      classpath: Seq[Path] = Nil,
      select: Set[String] = Set.empty,
      support: Seq[Path] = Nil,
      sources: Seq[Path] = Nil
  )

  @tailrec
  def parse(args: List[String], acc: Args = Args()): Either[String, Args] = args match {
    case "--out" :: file :: rest     => parse(rest, acc.copy(out = Some(Paths.get(file))))
    case "--classpath" :: cp :: rest =>
      val entries = cp.split(File.pathSeparator).toSeq.filter(_.nonEmpty).map(Paths.get(_))
      parse(rest, acc.copy(classpath = acc.classpath ++ entries))
    case "--select" :: name :: rest  => parse(rest, acc.copy(select = acc.select + name))
    case "--support" :: path :: rest =>
      parse(rest, acc.copy(support = acc.support :+ Paths.get(path)))
    case flag :: _ if flag.startsWith("--") => Left(s"unknown or incomplete option: $flag")
    case source :: rest => parse(rest, acc.copy(sources = acc.sources :+ Paths.get(source)))
    case Nil            => acc.out.toRight("--out <file> is required").map(_ => acc)
  }

  /** The rendered report and whether it had errors. */
  def run(args: Args): (String, Boolean) =
    if (args.select.isEmpty) {
      val found = Report.of(args.sources, args.classpath)
      (found.render, found.errors.nonEmpty)
    } else {
      val found = Report.query(
        Report.Query(args.sources, args.support, args.select, classpath = args.classpath)
      )
      (found.render, found.report.errors.nonEmpty)
    }

  def main(argv: Array[String]): Unit = parse(argv.toList) match {
    case Left(problem) =>
      System.err.println(problem)
      sys.exit(2)
    case Right(args) =>
      val (rendered, failed) = run(args)
      args.out.foreach { out =>
        Option(out.getParent).foreach(Files.createDirectories(_))
        Files.write(out, rendered.getBytes(UTF_8))
      }
      println(rendered)
      if (failed) sys.exit(1)
  }

}
