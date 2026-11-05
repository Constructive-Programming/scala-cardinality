package cardinality.types

import scala.meta.*

/** The refusal an existential wildcard reads as. */
private[cardinality] object TypePlaceholders {

  def existential(wildcard: Type.Wildcard): String =
    if (wildcard.bounds.lo.nonEmpty || wildcard.bounds.hi.nonEmpty)
      s"bounded existential wildcard requires unknown witness and bounds analysis: ${Spelling.of(wildcard)}"
    else s"existential wildcard requires unknown witness analysis: ${Spelling.of(wildcard)}"

}
