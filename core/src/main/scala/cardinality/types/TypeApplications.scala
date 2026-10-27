package cardinality.types

import scala.meta.*

import dev.constructive.eo.optics.{Lens, Prism}

/** Shared syntax rules for constructor applications and their binders. */
private[cardinality] object TypeApplications {
  private given Dialect = dialects.Scala3

  val applied = Prism.optional[Type, Type.Apply](
    {
      case a: Type.Apply => Some(a)
      case _             => None
    },
    identity
  )

  val applyArguments = Lens[Type.Apply, List[Type]](
    _.argClause.values,
    (a, args) => Type.Apply.After_4_6_0(a.tpe, Type.ArgClause(args))
  )

  val arguments = applied.andThen(applyArguments)

  val typeDefinition = Prism.optional[Tree, Defn.Type](
    {
      case d: Defn.Type => Some(d)
      case _            => None
    },
    identity
  )

  val patVarName = Prism.optional[Pat, String](
    {
      case Pat.Var(name) => Some(name.value)
      case _             => None
    },
    name => Pat.Var(Term.Name(name))
  )

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

  /** The checks an applied type constructor must pass before its arguments are substituted: no
    * constrained parameters, and exactly one argument per declared parameter.
    */
  def checkApplication(
      name: String,
      parameters: List[Type.Param],
      argCount: Int
  ): Either[String, Unit] =
    if (parameters.exists(constrained)) Left(s"constrained type constructor: $name")
    else if (parameters.size != argCount) Left(s"type argument arity: $name")
    else Right(())

}
