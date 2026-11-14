package cardinality.analysis

/** Step fuel for one count: each solver step costs one, and a count with none left stops. */
final private[cardinality] class AnalysisWork(quota: Long) {

  private var remaining = quota

  def step(operation: String): Unit = {
    if (remaining == 0) throw AnalysisWork.Exhausted(operation)
    remaining -= 1
  }

}

private[cardinality] object AnalysisWork {

  /** The stop when the fuel runs out, at the step that found none. */
  final case class Exhausted(operation: String)
      extends RuntimeException(s"target budget exhausted at $operation")

}
