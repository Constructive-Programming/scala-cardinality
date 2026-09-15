package cardinality

import scala.meta.*

import java.nio.file.Files
import sbt.*
import sbt.Keys.*

// The sbt face of scala-cardinality. Triggered automatically so a build only
// has to add the plugin to `project/plugins.sbt`; everything it computes is
// delegated to the `core` module, where the coverage and mutation gates bite.
object CardinalityPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val cardinalityReport =
      taskKey[Unit]("Report the cardinality of every Scala source file in the project.")

  }

  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    // `Def.uncached`: a report over transient inputs (`sources` is excluded
    // from sbt 2's cache key) must re-run every time, never be served stale
    // from the task cache.
    cardinalityReport := Def.uncached {
      val log = streams.value.log
      // Walking skeleton (M0): one line per file with the raw Size, and a
      // total. M1 grows this into a per-definition tree, M2 renders sizes
      // readably, M3 reports every parse error before failing the task — until
      // then `.get` fails it on the first one.
      val total = (Compile / sources).value
        .filter(_.getName.endsWith(".scala"))
        .sortBy(_.getPath)
        .foldLeft(NothingSize: Size) { (acc, file) =>
          val size =
            Counter.source(dialects.Scala3(Files.readString(file.toPath)).parse[Source].get)
          log.info(s"$file: $size")
          acc + size
        }
      log.info(s"total: $total")
    }
  )

}
