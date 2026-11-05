package cardinality.analysis.resolution

import scala.annotation.unused
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, Resolved}
import cardinality.analysis.Resolver
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.ResolutionContext
import cardinality.types.{Spelling, TypeSubstitution}

/** Singleton types and intersections: no corpus reads one, so both are refused. */
private[cardinality] object SingletonIntersections {

  /** A singleton type `p.type` or an intersection `A & B`. */
  def unapply(tpe: Type): Option[Type] = tpe match {
    case _: Type.Singleton | TypeSubstitution.Meet(_, _) => Some(tpe)
    case _                                               => None
  }

  def resolve(
      syntax: Type,
      @unused context: ResolutionContext,
      @unused resolver: Resolver
  ): Resolved = Left(s"${Spelling.of(syntax)}: singleton and intersection types are not resolved")

  /** A singleton identity is not rechecked at the use site, so it is refused. */
  def validate(shape: Shape, from: Frame, resolver: Resolver): Resolved =
    resolver.expanding("shape validation")(validateShape(shape, from, resolver))

  private def validateShape(shape: Shape, from: Frame, resolver: Resolver): Resolved =
    shape match {
      case _: Shape.Singleton => Left("singleton identity not validated at the use site")
      case other => Shape.tryRebuild(other, Shape.children(other).map(validate(_, from, resolver)))
    }

}
