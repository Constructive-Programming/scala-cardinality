package cardinality.analysis

/** Logical work accounting for an opt-in request. Each target receives fuel before measurement;
  * unused fuel is not transferred to another target.
  */
final private[cardinality] class AnalysisWork(indexLimit: Long, depthLimit: Int) {
  private var indexing = true
  private var remaining = indexLimit
  private var depth = 0
  private var used = 0L
  private var counts = Map.empty[String, Long]

  def counters: Map[String, Long] = counts
  def consumed: Long = used

  def target(quota: Long): Unit = {
    indexing = false
    remaining = quota
    depth = 0
    used = 0L
  }

  def step(operation: String): Unit = {
    if (remaining == 0)
      throw AnalysisWork.Exhausted(if (indexing) "index" else "target", operation)
    remaining -= 1
    used += 1
    counts = counts.updated(operation, counts.getOrElse(operation, 0L) + 1)
  }

  def expanding[A](operation: String)(read: => A): A = {
    step(operation)
    if (depth >= depthLimit) throw AnalysisWork.Exhausted("resolution depth", operation)
    depth += 1
    try read
    finally depth -= 1
  }

}

private[cardinality] object AnalysisWork {

  final case class Exhausted(budget: String, frontier: String)
      extends RuntimeException(s"$budget budget exhausted at $frontier")

}
