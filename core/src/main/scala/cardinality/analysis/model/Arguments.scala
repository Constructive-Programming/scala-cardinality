package cardinality.analysis.model

import scala.meta.Type

import cardinality.analysis.inhabitation.Inhabitation.Shape

/** An application's type arguments: as read, and as written with the scope each was written in,
  * which a projection rereads (members MT1'); none are written where no application was.
  */
final private[cardinality] case class Arguments(
    shapes: List[Shape],
    written: List[(Type, ResolutionContext)]
)

private[cardinality] object Arguments {
  val none: Arguments = Arguments(Nil, Nil)
}
