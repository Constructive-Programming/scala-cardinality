package cardinality

import cardinality.reporting.Report
import java.nio.file.Paths

/** Spike entry point of the native binary: `[--stubs <file>] <source path>...` prints the report.
  */
object NativeMain {

  def main(args: Array[String]): Unit = {
    val (stubs, sources) = args.toList match {
      case "--stubs" :: file :: rest => (Some(Paths.get(file)), rest)
      case rest                      => (None, rest)
    }
    val found = Report.of(sources.map(Paths.get(_)), stubs = stubs)
    println(found.render)
    if (found.errors.nonEmpty) sys.exit(1)
  }

}
