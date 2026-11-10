package cardinality.analysis

import cardinality.analysis.capability.TargetViews

import MethodAnalysis.Entry
import TargetViews.ViewMode

/** A count refused only by possible views P (capabilities C6) read between two bounds (register
  * D31): P bound as free capabilities admits every recursion-free implementation (R ≤ upper), P
  * left out keeps only ones that never use P (lower ≤ R). So an upper 0 is the count; other bounds
  * are left undecided by choice, and a refused binding refuses.
  */
private[cardinality] object ViewSandwich {

  def entry(measurement: ViewMode => Measurement): Entry = {
    val reading = measurement(ViewMode.Refuse)
    val first = reading.entry()
    if (!reading.refusedByViews(first.count)) first
    else {
      val included = measurement(ViewMode.Include).entry().count
      if (included.isZero) first.copy(count = included) else first
    }
  }

}
