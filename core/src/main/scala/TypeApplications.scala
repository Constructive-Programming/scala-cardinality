package cardinality

import scala.meta.*

/** Shared syntax rules for constructor applications and their binders. */
private[cardinality] object TypeApplications {
  private given Dialect = dialects.Scala3

  def name(tpe: Type): String = tpe match {
    case n: Type.Name => n.value
    case _            => tpe.syntax.stripPrefix("_root_.")
  }

  def lambda(tpe: Type): Option[Either[String, Type.Lambda]] = tpe match {
    case lambda: Type.Lambda => Some(Right(lambda))
    case _                   => TypePlaceholders.lambda(tpe)
  }

  def constrained(parameter: Type.Param): Boolean =
    parameter.bounds.lo.nonEmpty || parameter.bounds.hi.nonEmpty ||
      parameter.bounds.context.nonEmpty || parameter.bounds.view.nonEmpty ||
      parameter.tparamClause.values.nonEmpty

}
