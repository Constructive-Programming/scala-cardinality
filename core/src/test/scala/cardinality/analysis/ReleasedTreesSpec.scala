package cardinality.analysis

import scala.meta.*

import java.lang.ref.WeakReference
import org.specs2.mutable.Specification

/** A report keeps nothing once it is done: a build tool runs one per module in one JVM, so trees a
  * finished report left reachable would add up until the heap is gone (issue #36).
  */
class ReleasedTreesSpec extends Specification {

  // Analyzes a source that reads a capability record's members; only a weak reference escapes.
  private def analyzed(): WeakReference[Source] = {
    val source = dialects
      .Scala3(
        "trait Rec { def a: Boolean; def b(x: Boolean): Boolean }\ndef k(r: Rec): Boolean = ???"
      )
      .parse[Source]
      .get
    val rows = MethodAnalysis.analyze(List(MethodAnalysis.Input("R.scala", source)))
    rows.foreach(_.count.toString)
    new WeakReference(source)
  }

  "a finished analysis leaves its trees collectable" in {
    val source = analyzed()
    val collected = Iterator.range(0, 50).exists { _ =>
      System.gc()
      Thread.sleep(20)
      source.get == null
    }
    collected must beTrue
  }

}
