package cardinality.capacity

import scala.meta.*

/** What a reading met whose size is not whether a value exists (`ValueRows`): a function from an
  * empty type (the algebra's 0, though the absurd function is a value), an intersection or
  * refinement (an upper bound, maybe over an empty type: `Int & String`), or a value that is not
  * known to exist — a template whose construction is not read (`Builds`), a singleton type.
  */
private[cardinality] enum Overcount {
  case EmptyDomain
  case Narrowing
  case Unread(what: String)
}

// What a top-level value holds (docs/research/pure-code.md §2, register D30): one value when its
// declared type has one — `min(1, |T|)` read as inhabitation, not as the algebra's capacity. With
// no reason, a nonzero capacity is inhabitation; a zero (an empty type, or a function from one,
// which is a value) and any overcount leave the row undecided. The initializer is never read.
private[capacity] object ValueRows {

  /** What a value holds: 1, or — with the reasons noted — at most 1. With no declared type, 1. */
  def value(scope: Scope)(declared: Option[Type]): Size = {
    declared.foreach { tpe =>
      val probe = scope.measuredValue
      val size = Counter.typeIn(probe)(tpe)
      val reasons =
        if (probe.unbound.nonEmpty) probe.unbound
        else if (size.isZero || probe.overcounts.nonEmpty)
          List(Definition.Unbound.Refusal(s"value not decided: ${tpe.syntax}"))
        else Nil
      reasons.foreach(scope.note)
    }
    UnitSize
  }

}
