package cardinality.analysis

import cardinality.analysis.inhabitation.Inhabitation.Count

/** The method side's rows: pure, total, parametric implementations, not stored-value cardinality.
  * Unsupported code or an incomplete environment is a reason, never evidence of infinity. No source
  * code is executed.
  */
object MethodAnalysis {

  final case class Entry(
      path: String,
      name: String,
      signature: String,
      line: Int,
      kind: String,
      count: Count,
      captures: List[String]
  )

}
