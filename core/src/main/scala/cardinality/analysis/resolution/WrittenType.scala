package cardinality.analysis.resolution

import scala.meta.Type

import cardinality.analysis.Resolver
import cardinality.analysis.model.ResolutionContext

/** A type as written at a place: the tree, the scope it is read in, and the resolver that reads it
  * there. A name in it means what that scope binds it to, so the three travel together.
  */
final private[cardinality] case class WrittenType(
    tpe: Type,
    context: ResolutionContext,
    resolver: Resolver
) {

  /** Another type written at the same place: a part of this one, read in its scope. */
  def at(other: Type): WrittenType = copy(tpe = other)

}
