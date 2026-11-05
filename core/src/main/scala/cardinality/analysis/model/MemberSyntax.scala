package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Resolved
import cardinality.analysis.inhabitation.{Inhabitation, PolymorphicMembers}
import cardinality.types.{Kind, TypeApplications}

import Inhabitation.Shape

/** What a member's syntax spells — its clauses, the names it defines, the callable it is — read one
  * way wherever members are bound, compared or classified.
  */
private[cardinality] object MemberSyntax {

  /** A member's callable type, its identity and its own type parameters, each read as the member's
    * own atom (`binders`): `close` reads them once the type is (capabilities C8).
    */
  final case class Callable(
      tpe: Type,
      member: String,
      typeParameters: List[Type.Param],
      rigid: (String, Kind) => Shape
  ) {

    lazy val binders: Map[String, Resolved] =
      typeParameters.map(p => p.name.value -> PolymorphicMembers.binder(member, p, rigid)).toMap

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
    case g: Defn.GivenAlias       => List(givenName(g))
    case g: Defn.Given            => List(givenName(g))
    case g: Decl.Given            => List(givenName(g))
    case _                        => Nil
  }

  /** The name Scala synthesizes for an anonymous given (`GivenNames`); none for any other tree. */
  def anonymousGiven(tree: Tree): Option[String] = tree match {
    case g: GivenNames.Given => GivenNames.of(g)
    case _                   => None
  }

  // A given's name, synthesized when anonymous (`GivenNames.Unknown` when that name is not known).
  private def givenName(g: GivenNames.Given): String =
    if (g.name.value.nonEmpty) g.name.value else GivenNames.of(g).getOrElse(GivenNames.Unknown)

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

  /** A definition's right-hand side: a method's or a given alias's body, a value's initializer. */
  def rightHandSide(tree: Tree): Option[Term] = tree match {
    case d: Defn.Def        => Some(d.body)
    case g: Defn.GivenAlias => Some(g.body)
    case v: Defn.Val        => Some(v.rhs)
    case v: Defn.Var        => Some(v.body)
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

  /** The constructor parameters of a class or trait that are members: `val`/`var` ones, and a case
    * class's first clause.
    */
  def fieldParams(declaration: Tree): List[Term.Param] = declaration match {
    case c: Defn.Class =>
      val clauses = c.ctor.paramClauses.toList.map(_.values)
      val caseFields = if (c.mods.exists(_.is[Mod.Case])) clauses.take(1).flatten else Nil
      clauses.flatten.filter(p => keptParameter(p) || caseFields.exists(_ eq p))
    case t: Defn.Trait => t.ctor.paramClauses.toList.flatMap(_.values).filter(keptParameter)
    case _             => Nil
  }

  /** A repeated (`A*`) parameter. */
  def repeated(parameter: Term.Param): Boolean = parameter.decltpe.exists(_.is[Type.Repeated])

  /** A member restating one of `Any`'s overridable members — `toString`, `hashCode`, `equals(Any)`:
    * runtime printing, hashing and equality, outside the model (capabilities C0) — or one of
    * `Product`'s and `Equals`' (`productArity`, `productElement(n)`, `canEqual(Any)`, …): reflection
    * over a value's fields and an equality test, outside it too (v1.md §2). `isAny` tells whether a
    * parameter type is `Any` where the member is written.
    */
  def universal(stat: Tree)(isAny: Type => Boolean): Boolean = stat match {
    case d: (Decl.Def | Defn.Def) if d.paramClauseGroups.forall(_.tparamClause.values.isEmpty) =>
      val name = d.name.value
      d.paramClauseGroups.flatMap(_.paramClauses).map(_.values) match {
        case Nil | List(Nil) =>
          Set("toString", "hashCode")(name) || UniversalMembers.productValues(name)
        case List(List(value)) =>
          (Set("equals", "canEqual")(name) && value.decltpe.exists(isAny)) ||
          UniversalMembers.productReaders(name)
        case _ => false
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
  def observation(
      stat: Tree,
      name: String,
      member: String,
      rigid: (String, Kind) => Shape
  ): Either[String, Observation] = {
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
      .map(res => callable(name, member, groups(stat), res, rigid).map(Observation.Applied(_, res)))
      .orElse(value.map(tpe => Right(Observation.Value(tpe))))
      .getOrElse(Left(s"overridable member's result type is inferred: $name"))
  }

  /** Whether every application of a member with this result builds Unit: such a member has its one
    * construction whatever the environment holds, so it is irrelevant to every count and its
    * parameter types are never read (capabilities C7, the exact case).
    */
  def buildsUnit(result: Type, read: Type => Resolved): Boolean =
    read(lastResult(result)) == Right(Inhabitation.Shape.Product(Nil))

  /** What an applied member observes, its types read by `read`: its callable, or nothing when it
    * only ever builds Unit (its parameter types then need no reading).
    */
  def applied(
      callable: Callable,
      result: Type,
      read: Type => Resolved
  ): Either[String, Option[Inhabitation.Shape]] =
    read(callable.tpe).flatMap(callable.close) match {
      case Left(_) if buildsUnit(result, read) => Right(None)
      case other                               => other.map(Some(_))
    }

  /** The modifiers of a method or a given alias, the definitions that may be `inline`. */
  def methodModifiers(tree: Tree): List[Mod] = tree match {
    case d: Defn.Def        => d.mods
    case d: Decl.Def        => d.mods
    case g: Defn.GivenAlias => g.mods
    case _                  => Nil
  }

  /** Whether a method or a given alias is `inline`. */
  def inline(tree: Tree): Boolean = methodModifiers(tree).exists(_.is[Mod.Inline])

  /** Whether modifiers include `implicit`. */
  def implicitModifier(mods: List[Mod]): Boolean = mods.exists(_.is[Mod.Implicit])

  /** Whether a top-level statement of a file is compiled into the file's package object
    * (`f$package`), where every top-level opaque type of the file is transparent: any statement but
    * a class, trait, enum or object definition or a nested package — save an implicit class or
    * object (Scala 3 wraps "implicit classes and objects") and an object that accompanies a type
    * alias (an opaque or plain one, such as `object MF` beside `type MF[F[_]] = [X, A] =>> MFK[F,
    * X, A]`, compiled to `f$package$MF`). Scala 3.9.0 lets `implicit class R(f: Flag)`, `implicit
    * object IO` and a `given … with` body read `Flag`'s representation (`StoredScopesSpec`'s
    * `opaqueTopLevel` and `opaqueLending`).
    */
  def wrappedInPackageObject(stat: Tree): Boolean = stat match {
    case module: Defn.Object =>
      implicitModifier(module.mods) || module.parent.exists(_.children.exists {
        case alias: Defn.Type => alias.name.value == module.name.value
        case _                => false
      })
    case c: Defn.Class                                  => implicitModifier(c.mods)
    case _: (Defn.Trait | Defn.Enum | Pkg | Pkg.Object) => false
    case _                                              => true
  }

  /** A member implicit search may select: a given, or an `implicit` definition, object or class. */
  def implicitMember(stat: Tree): Boolean = stat match {
    case _: (Defn.GivenAlias | Defn.Given | Decl.Given) => true
    case d: Stat.WithMods                               => implicitModifier(d.mods)
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
      result: Type,
      rigid: (String, Kind) => Shape
  ): Either[String, Callable] = {
    val params = parameters(groups)
    if (!params.forall(_.decltpe.isDefined)) Left(s"missing callable parameter type: $name")
    else
      Right(
        Callable(
          Type.Function(Type.FuncParamClause(params.flatMap(_.decltpe)), result),
          member,
          groups.flatMap(_.tparamClause.values),
          rigid
        )
      )
  }

}
