package cardinality.types

import scala.meta.*
import scala.reflect.ClassTag

import dev.constructive.eo.optics.{Lens, Prism}

/** Shared syntax rules for constructor applications and their binders. */
private[cardinality] object TypeApplications {
  private given Dialect = dialects.Scala3

  def subPrism[S, A <: S: ClassTag] =
    Prism.optional[S, A](
      {
        case a: A => Some(a)
        case _    => None
      },
      identity
    )

  val applied = subPrism[Type, Type.Apply]

  val applyArguments = Lens[Type.Apply, List[Type]](
    _.argClause.values,
    (a, args) => Type.Apply.After_4_6_0(a.tpe, Type.ArgClause(args))
  )

  val arguments = applied.andThen(applyArguments)

  val typeDefinition = subPrism[Tree, Defn.Type]

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

  /** A kind with nothing else said about it: `F[_]`, `F[_, _]` — no bounds, no view, no context
    * bound, no modifiers. Its application can read as an opaque slot; a binder with anything more
    * said about it restricts which type the caller may supply, and stays a refusal.
    */
  def unboundedKind(parameter: Type.Param): Boolean =
    parameter.tparamClause.values.nonEmpty && parameter.mods.isEmpty &&
      parameter.bounds.lo.isEmpty && parameter.bounds.hi.isEmpty &&
      parameter.bounds.context.isEmpty && parameter.bounds.view.isEmpty

  /** The checks an applied type constructor must pass before its arguments are substituted: no
    * constrained parameters, and exactly one argument per declared parameter.
    */
  def checkApplication(
      name: String,
      parameters: List[Type.Param],
      argCount: Int,
      kind: String = "type constructor",
      arityKind: String = "type"
  ): Either[String, Unit] =
    if (parameters.exists(constrained)) Left(s"constrained $kind: $name")
    else if (parameters.size != argCount) Left(s"$arityKind argument arity: $name")
    else Right(())

}
