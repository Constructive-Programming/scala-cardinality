package cardinality.bench

import java.lang.management.{ManagementFactory, MemoryType}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Locale

import scala.jdk.CollectionConverters.*

import cardinality.reporting.Report
import cardinality.tasty.CompiledTypes

/** One `Report.of` over a frozen corpus (scripts/measure-corpus.sh runs this in a fresh JVM). A
  * corpus directory holds the sources, each at a path that ends with the one its TASTy records;
  * `classes`, the compiled output of exactly those sources (a directory or a jar); and
  * `classpath.txt`, one dependency jar per line.
  *
  * Only the `Report.of` call is timed, reading the compiled classes against the dependency
  * classpath. The rendered report goes to the output file; what the run cost and what the report
  * holds go to standard output as `PERF` and `ROWS` lines.
  */
object CorpusRun {

  def main(args: Array[String]): Unit = {
    val (root, out) = args match {
      case Array(corpus, out) => (Path.of(corpus), Path.of(out))
      case _                  => sys.error("usage: CorpusRun <corpus directory> <report file>")
    }
    val classpath = Files
      .readAllLines(root.resolve("classpath.txt"), UTF_8)
      .asScala
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Path.of(_))
    val classes = CompiledTypes.Classes(List(root.resolve("classes")), classpath)
    val os = ManagementFactory.getOperatingSystemMXBean
      .asInstanceOf[com.sun.management.OperatingSystemMXBean]
    val heaps = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP)
    val collectors = ManagementFactory.getGarbageCollectorMXBeans.asScala
    heaps.foreach(_.resetPeakUsage())
    val gcBefore = collectors.map(_.getCollectionTime).sum
    val cpuBefore = os.getProcessCpuTime
    val start = System.nanoTime
    val report =
      Report.of(List(root), Report.Environment(Some(classes)))
    val wall = (System.nanoTime - start) / 1e9
    val cpu = (os.getProcessCpuTime - cpuBefore) / 1e9
    val gc = (collectors.map(_.getCollectionTime).sum - gcBefore) / 1e3
    val gib = 1024.0 * 1024 * 1024
    // The heap pools' peaks summed: an upper bound, since the pools peak at different times.
    val heap = heaps.map(_.getPeakUsage.getUsed).sum / gib
    // The peak resident set in GiB, from the kernel's `VmHWM` (in kB); 0 where there is no /proc.
    val status = Path.of("/proc/self/status")
    val rss =
      if (!Files.isReadable(status)) 0.0
      else
        Files
          .readAllLines(status)
          .asScala
          .collectFirst {
            case line if line.startsWith("VmHWM") => line.split("\\s+")(1).toLong * 1024 / gib
          }
          .getOrElse(0.0)
    Files.writeString(out, report.render, UTF_8)
    println(
      "PERF wall=%.1f s cpu=%.1f s gc=%.1f s peakHeap=%.2f GiB peakRss=%.2f GiB"
        .formatLocal(Locale.ROOT, wall, cpu, gc, heap, rss)
    )
    println(s"ROWS ${rows(report)}")
  }

  // Rows by kind. A method row is finite, countably infinite, beyond countable or `?`; a stored
  // value row is one of those or `—` (an abstract type, with no size of its own).
  private def rows(report: Report): String = {
    val methods = report.methods.map(_.count)
    val finite = methods.count(_.isFinite)
    val countable = methods.count(_.isCountable)
    val beyond = methods.count(_.isBeyondCountable)
    val definitions = report.definitionRows
      .groupMapReduce { row =>
        (row.size, row.definition.size) match {
          case ("?", _)                            => "?"
          case (_, None)                           => "—"
          case (_, Some(size)) if size.epsilon > 0 => "beyond"
          case (_, Some(size)) if size.omega > 0   => "countable"
          case _                                   => "finite"
        }
      }(_ => 1)(_ + _)
      .withDefaultValue(0)
    s"sources=${report.sources.size} " +
      s"signatures=${methods.size} finite=$finite countable=$countable beyond=$beyond " +
      s"unresolved=${methods.size - finite - countable - beyond} | " +
      s"definitions=${report.definitionRows.size} finite=${definitions("finite")} " +
      s"countable=${definitions("countable")} beyond=${definitions("beyond")} " +
      s"unresolved=${definitions("?")} abstract=${definitions("—")}"
  }

}
