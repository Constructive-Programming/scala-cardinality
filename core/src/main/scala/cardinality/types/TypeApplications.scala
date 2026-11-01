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

  /** The template a class, trait, object, enum or given instance declares. */
  def template(tree: Tree): Option[Template] = tree match {
    case d: (Defn.Class | Defn.Trait | Defn.Object | Defn.Enum | Defn.Given) => Some(d.templ)
    case _                                                                   => None
  }

  def name(tpe: Type): String = spelled(tpe).stripPrefix("_root_.")

  /** A constructor's name as written, a `_root_` anchor kept for lookup to read. */
  def spelled(tpe: Type): String = tpe match {
    case n: Type.Name => n.value
    case _            => tpe.syntax
  }

  def lambda(tpe: Type): Option[Either[String, Type.Lambda]] = tpe match {
    case lambda: Type.Lambda => Some(Right(lambda))
    case _                   => TypePlaceholders.lambda(tpe)
  }

  /** A binder a polymorphic function type cannot take: any bound, or a kind of its own. */
  def constrained(parameter: Type.Param): Boolean =
    !Kinds.plain(parameter) || parameter.tparamClause.values.nonEmpty

  /** The checks an applied type constructor must pass before its arguments are substituted: every
    * parameter plain (`Kinds.plain`: a rigid higher-kinded one included), and exactly one argument
    * per declared parameter.
    */
  def checkApplication(
      name: String,
      parameters: List[Type.Param],
      argCount: Int,
      kind: String = "type constructor",
      arityKind: String = "type"
  ): Either[String, Unit] =
    if (!parameters.forall(Kinds.plain)) Left(s"constrained $kind: $name")
    else if (parameters.size != argCount) Left(s"$arityKind argument arity: $name")
    else Right(())

}
