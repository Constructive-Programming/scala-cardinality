package cardinality.analysis.model

import scala.meta.Type

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.types.Kinds

/** An application's type arguments: as read, and as written with the scope each was written in,
  * which a projection rereads (members MT1'); none are written where no application was.
  */
final private[cardinality] case class Arguments(
    shapes: List[Shape],
    written: List[(Type, ResolutionContext)]
) {

  /** The arguments as written of the proper parameters among `parameters`, by name: what a
    * projection over one of them rereads.
    */
  def proper(parameters: List[Type.Param]): List[(String, (Type, ResolutionContext))] =
    parameters.zip(written).collect {
      case (p, argument) if Kinds.proper(p) => p.name.value -> argument
    }

}

private[cardinality] object Arguments {
  val none: Arguments = Arguments(Nil, Nil)
}
