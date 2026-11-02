package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Resolved
import cardinality.analysis.inhabitation.{Inhabitation, PolymorphicMembers}
import cardinality.types.TypeApplications

/** What a member's syntax spells — its clauses, the names it defines, the callable it is — read one
  * way wherever members are bound, compared or classified.
  */
private[cardinality] object MemberSyntax {

  /** A member's callable type, its identity and its own type parameters, each read as the member's
    * own atom (`binders`): `close` reads them once the type is (capabilities C8).
    */
  final case class Callable(tpe: Type, member: String, typeParameters: List[Type.Param]) {

    lazy val binders: Map[String, Resolved] =
      typeParameters.map(p => p.name.value -> PolymorphicMembers.binder(member, p)).toMap

    def close(shape: Inhabitation.Shape): Resolved =
      PolymorphicMembers.close(member, typeParameters, shape)

  }

  def groups(stat: Tree): List[Member.ParamClauseGroup] = stat match {
    case d: Decl.Def        => d.paramClauseGroups
    case d: Defn.Def        => d.paramClauseGroups
    case g: Defn.GivenAlias => g.paramClauseGroups
    case g: Decl.Given      => g.paramClauseGroups
    case g: Defn.Given      => g.paramClauseGroups
    case _                  => Nil
  }

  def parameters(groups: List[Member.ParamClauseGroup]): List[Term.Param] =
    groups.flatMap(_.paramClauses).flatMap(_.values)

  def parameterNames(groups: List[Member.ParamClauseGroup]): Set[String] =
    parameters(groups).map(_.name.value).toSet

  /** Type parameter count, then each term clause's size: what tells an override from an overload
    * without reading a type.
    */
  def shape(groups: List[Member.ParamClauseGroup]): List[Int] =
    groups.flatMap(group => group.tparamClause.values.size :: group.paramClauses.map(_.values.size))

  /** The term members a statement defines or declares. */
  def names(stat: Tree): List[String] = stat match {
    case d: (Defn.Def | Decl.Def) => List(d.name.value)
    case v: Defn.Val              => v.pats.flatMap(TypeApplications.patVarName.getOption)
    case v: Decl.Val              => v.pats.flatMap(TypeApplications.patVarName.getOption)
    case v: Defn.Var              => v.pats.flatMap(TypeApplications.patVarName.getOption)
    case v: Decl.Var              => v.pats.flatMap(TypeApplications.patVarName.getOption)
    case o: Defn.Object           => List(o.name.value)
    case g: Defn.GivenAlias       => List(g.name.value)
    case g: Defn.Given            => List(g.name.value)
    case g: Decl.Given            => List(g.name.value)
    case _                        => Nil
  }

  /** The names a pattern binds, a sequence wildcard's (`xs*`) included. */
  def patternNames(pat: Pat): List[String] = TypeApplications.boundNames(List(pat))

  /** The term members a statement defines, a pattern definition's binders included. */
  def defined(stat: Tree): List[String] = stat match {
    case v: Defn.Val => TypeApplications.boundNames(v.pats)
    case v: Defn.Var => TypeApplications.boundNames(v.pats)
    case other       => names(other)
  }

  /** The name a class, trait, object or enum is declared under. */
  def templateName(tree: Tree): Option[String] = tree match {
    case c: Defn.Class  => Some(c.name.value)
    case t: Defn.Trait  => Some(t.name.value)
    case o: Defn.Object => Some(o.name.value)
    case e: Defn.Enum   => Some(e.name.value)
    case _              => None
  }

  /** A member's declared result type, where it writes one. */
  def result(stat: Tree): Option[Type] = stat match {
    case d: Decl.Def        => Some(d.decltpe)
    case d: Defn.Def        => d.decltpe
    case g: Defn.GivenAlias => Some(g.decltpe)
    case g: Decl.Given      => Some(g.decltpe)
    case _                  => None
  }

  /** The type a curried result finally builds. */
  def lastResult(tpe: Type): Type = tpe match {
    case f: Type.Function => lastResult(f.res)
    case other            => other
  }

  /** A clause the compiler fills from implicit scope: `using` or `implicit`. */
  def contextual(clause: Term.ParamClause): Boolean =
    clause.mod.nonEmpty || clause.values.exists(_.mods.exists(_.is[Mod.Implicit]))

  /** A `val` or `var` class parameter: one its class keeps as a field. */
  def keptParameter(parameter: Term.Param): Boolean =
    parameter.mods.exists(m => m.is[Mod.ValParam] || m.is[Mod.VarParam])

  /** A repeated (`A*`) parameter. */
  def repeated(parameter: Term.Param): Boolean = parameter.decltpe.exists(_.is[Type.Repeated])

  /** A member restating one of `Any`'s overridable members — `toString`, `hashCode`, `equals(Any)`:
    * runtime printing, hashing and equality, outside the model (capabilities C0). `isAny` tells
    * whether a parameter type is `Any` where the member is written.
    */
  def universal(stat: Tree)(isAny: Type => Boolean): Boolean = stat match {
    case d: (Decl.Def | Defn.Def) if d.paramClauseGroups.forall(_.tparamClause.values.isEmpty) =>
      d.paramClauseGroups.flatMap(_.paramClauses).map(_.values) match {
        case Nil | List(Nil)   => Set("toString", "hashCode")(d.name.value)
        case List(List(value)) => d.name.value == "equals" && value.decltpe.exists(isAny)
        case _                 => false
      }
    case _ => false
  }

  /** What observing a member gives: a callable (its parameters to its result) or a value of its
    * declared type.
    */
  enum Observation {
    case Applied(callable: Callable, result: Type)
    case Value(tpe: Type)
  }

  /** How a member is observed, or why its type is unknown: a method or a given with parameters is a
    * callable, a parameterless given or a value its declared type. A `var` is observed as the value
    * it holds (the model has no mutation): its getter, its setter an effect. `name` is how a reason
    * spells the member; `member` is its identity, which names its binders (`Owner.name`).
    */
  def observation(stat: Tree, name: String, member: String): Either[String, Observation] = {
    val (applied, value) = stat match {
      case d: Decl.Def                                        => (Some(d.decltpe), None)
      case d: Defn.Def                                        => (d.decltpe, None)
      case g: Defn.GivenAlias if g.paramClauseGroups.nonEmpty => (Some(g.decltpe), None)
      case g: Defn.GivenAlias                                 => (None, Some(g.decltpe))
      case g: Decl.Given                                      => (None, Some(g.decltpe))
      case v: Decl.Val                                        => (None, Some(v.decltpe))
      case v: Defn.Val                                        => (None, v.decltpe)
      case v: Decl.Var                                        => (None, Some(v.decltpe))
      case v: Defn.Var                                        => (None, v.decltpe)
      case p: Term.Param                                      => (None, p.decltpe)
      case _                                                  => (None, None)
    }
    applied
      .map(res => callable(name, member, groups(stat), res).map(Observation.Applied(_, res)))
      .orElse(value.map(tpe => Right(Observation.Value(tpe))))
      .getOrElse(Left(s"overridable member's result type is inferred: $name"))
  }

  /** Whether every application of a member with this result builds Unit: such a member has its one
    * construction whatever the environment holds, so it is irrelevant to every count and its
    * parameter types are never read (capabilities C7, the exact case).
    */
  def buildsUnit(result: Type, read: Type => Resolved): Boolean =
    read(lastResult(result)) == Right(Inhabitation.Shape.Product(Nil))

  /** A member implicit search may select: a given, or an `implicit` definition, object or class. */
  def implicitMember(stat: Tree): Boolean = stat match {
    case _: (Defn.GivenAlias | Defn.Given | Decl.Given) => true
    case d: Stat.WithMods                               => d.mods.exists(_.is[Mod.Implicit])
    case _                                              => false
  }

  /** An abstract `inline` (or `transparent inline`) member: a deferred inline method or value. */
  def deferredInline(stat: Tree): Boolean =
    (stat.is[Decl.Def] || stat.is[Decl.Val]) && inlined(stat)

  /** Whether a definition is `inline`: its body is expanded at each call site. */
  def inlined(tree: Tree): Boolean = modifiers(tree).exists(_.is[Mod.Inline])

  private def modifiers(tree: Tree): List[Mod] = tree match {
    case d: Stat.WithMods => d.mods
    case _                => Nil
  }

  /** A modifier that hides a declaration from some scope. */
  def restricted(mod: Mod): Boolean = mod.is[Mod.Private] || mod.is[Mod.Protected]

  // A member's callable: its uncurried parameter types to its result, or why it has none.
  private def callable(
      name: String,
      member: String,
      groups: List[Member.ParamClauseGroup],
      result: Type
  ): Either[String, Callable] = {
    val params = parameters(groups)
    if (!params.forall(_.decltpe.isDefined)) Left(s"missing callable parameter type: $name")
    else
      Right(
        Callable(
          Type.Function(Type.FuncParamClause(params.flatMap(_.decltpe)), result),
          member,
          groups.flatMap(_.tparamClause.values)
        )
      )
  }

}
