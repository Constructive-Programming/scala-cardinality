package cardinality.analysis

/** Logical work accounting for an opt-in request. Each target receives fuel before measurement;
  * unused fuel is not transferred to another target.
  */
final private[cardinality] class AnalysisWork(indexLimit: Long, depthLimit: Int) {
  import AnalysisWork.*

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
      throw Exhausted.Fuel(if (indexing) Budget.Index else Budget.Target, operation)
    remaining -= 1
    used += 1
    counts = counts.updated(operation, counts.getOrElse(operation, 0L) + 1)
  }

  def expanding[A](operation: String)(read: => A): A = {
    step(operation)
    if (depth >= depthLimit)
      throw Exhausted.ResolutionDepth(operation)
    depth += 1
    try read
    finally depth -= 1
  }

}

private[cardinality] object AnalysisWork {

  /** The step-fuel budgets: indexing drains one tank, each measured target another. */
  enum Budget {
    case Index
    case Target

    def render: String = this match {
      case Budget.Index  => "index"
      case Budget.Target => "target"
    }
  }

  /** A work stop. A sum, not a budget × frontier product: a syntax-depth stop reports the source
    * path while every other stop reports the operation name, so the product admitted combinations
    * no stop can produce. Message text is unchanged — tests and reports read the same strings.
    */
  enum Exhausted(message: String) extends RuntimeException(message) {
    case Fuel(budget: AnalysisWork.Budget, operation: String)
        extends Exhausted(s"${budget.render} budget exhausted at $operation")
    case ResolutionDepth(operation: String)
        extends Exhausted(s"resolution depth budget exhausted at $operation")
    case SyntaxDepth(path: String)
        extends Exhausted(s"syntax depth budget exhausted at $path")
  }

}
