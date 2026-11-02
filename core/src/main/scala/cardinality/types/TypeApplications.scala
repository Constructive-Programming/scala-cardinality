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

  /** A type's head and the arguments it is applied to, none for an unapplied one. */
  def headAndArguments(tpe: Type): (Type, List[Type]) =
    applied.getOption(tpe).fold(tpe -> List.empty[Type])(app => app.tpe -> applyArguments.get(app))

  val typeDefinition = subPrism[Tree, Defn.Type]

  val patVarName = Prism.optional[Pat, String](
    {
      case Pat.Var(name) => Some(name.value)
      case _             => None
    },
    name => Pat.Var(Term.Name(name))
  )

  /** Whether one of a definition's patterns binds the name as a variable. */
  def binds(name: String, patterns: List[Pat]): Boolean =
    patterns.exists(patVarName.getOption(_).contains(name))

  /** Every variable a definition's patterns bind. `val (f, z) = …`, `val seed @ _ = …` and `val
    * Some(x) = …` define their variables as members as `val f = …` does, though none of them has a
    * declared type of its own.
    */
  def boundNames(patterns: List[Pat]): List[String] =
    patterns.flatMap(_.collect {
      case Pat.Var(name)      => name.value
      case Pat.Repeated(name) => name.value
    })

  /** A value class: its first parent is `AnyVal`. */
  def valueClass(c: Defn.Class): Boolean =
    c.templ.inits.headOption.exists(
      _.tpe.syntax.stripPrefix("_root_.").stripPrefix("scala.") == "AnyVal"
    )

  /** The template a class, trait, object, enum, given instance or anonymous class declares. */
  def template(tree: Tree): Option[Template] = tree match {
    case d: (Defn.Class | Defn.Trait | Defn.Object | Defn.Enum | Defn.Given) => Some(d.templ)
    case n: Term.NewAnonymous                                                => Some(n.templ)
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
