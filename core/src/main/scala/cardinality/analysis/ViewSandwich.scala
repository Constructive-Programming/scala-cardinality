package cardinality.analysis

import cardinality.analysis.capability.TargetViews
import cardinality.analysis.inhabitation.Inhabitation.Count

import MethodAnalysis.Entry
import TargetViews.ViewMode

/** A count refused only by possible views P (capabilities C6) read between two bounds (register
  * D31): P bound as free capabilities admits every recursion-free implementation (R ≤ upper), P
  * left out keeps only ones that never use P (lower ≤ R). So an upper 0 is the count, and so is 1
  * when the lower is 1; agreeing bounds past 1 are left undecided by choice, and a refused binding
  * refuses.
  */
private[cardinality] object ViewSandwich {

  def entry(measurement: ViewMode => Measurement): Entry = {
    val reading = measurement(ViewMode.Refuse)
    val first = reading.entry()
    if (!reading.refusedByViews(first.count)) first
    else {
      lazy val lower = measurement(ViewMode.Exclude).entry().count
      measurement(ViewMode.Include).entry().count match {
        case zero @ Count.Finite(n) if n == 0                => first.copy(count = zero)
        case one @ Count.Finite(n) if n == 1 && lower == one => first.copy(count = one)
        case _                                               => first
      }
    }
  }

}
