package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.model.{MemberPath, ResolutionContext, ScopeTerms}
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.{Traversals, TypeApplications}

import InferredResults.{Inferred, Signature, Unresolved}
import MethodAnalysis.{Frame, Resolved, Target}

/** The result Scala gives a member written without one where it overrides another.
  *
  * Scala 3's namer types such a member by every base class's non-private member of its name whose
  * parameter types match its own, seen from the subclass, intersected: a 3.9.0 fixture types
  * `Some(1)` overriding `def f: Option[Int]` as `Option[Int]`, and `def h = true` beside a parent's
  * `private def h: Any` as `Boolean`. So only a member exactly one supplied declaration matches is
  * read, through that declaration's result in its parent's scope with the subclass as `this`; a
  * base class outside the sources — `Any` and `Object` for every template, `Product` and `Equals`
  * for a case class, `scala.reflect.Enum` for an enum, an unsupplied or modelled parent — that may
  * declare the name refuses the reading, as does a parameter type whose identity is not known. An
  * abstract `var` declares a getter and a setter `f_=(x: T): Unit` a method may implement, and a
  * given declares a method of its name (3.9.0 fixtures: `def f = true` implementing `var f: Any` or
  * `given f: Any` is an `Any`).
  */
final private[cardinality] class OverriddenResults(
    target: Target,
    definition: Defn.Def,
    variables: Map[String, Resolved],
    resolver: Resolver
) {
  private val name = definition.name.value

  private val ownContext = ResolutionContext(target.frame, variables, Set.empty)

  private val ownBounded = bounded(definition.paramClauseGroups)

  /** The overridden declaration's result where the target may override one; None for a member
    * nothing it inherits declares under its name and parameter types, which its body types.
    */
  def result(steps: List[InheritedScopes.Step]): Option[Either[String, Inferred]] =
    target.frame.parent.flatMap { owner =>
      owner.tree match {
        // An extension method of a template may override one its parents declare: not matched here.
        case Some(_: Defn.ExtensionGroup) =>
          Option.when(owner.parent.exists(_.parents.nonEmpty))(
            Left(s"$Unresolved: an extension method that may override an inherited one")
          )
        case _ if owner.isTemplate || resolver.isPackage(owner) =>
          member(owner, InheritedScopes.of(steps, owner))
        case _ => None
      }
    }

  private def member(
      template: Frame,
      levels: List[InheritedScopes.Step]
  ): Option[Either[String, Inferred]] =
    InheritedTerms.foreign(template :: levels.map(_.declared), name, resolver) match {
      case Some(parent) =>
        Some(Left(s"$Unresolved: a member that may override a declaration of ${parent.parent}"))
      case None =>
        Traversals.sequence(levels.flatMap(candidates(template, _))).map(_.flatten) match {
          case Left(reason)                                    => Some(Left(reason))
          case Right(Nil)                                      => overriding
          case Right(List(Candidate(step, groups, Some(tpe)))) =>
            Some(inherited(template, step, groups, tpe))
          case Right(List(_)) =>
            Some(Left(s"$Unresolved: overriding member $name, whose overridden result is inferred"))
          case Right(_) =>
            Some(Left(s"$Unresolved: overriding member $name of several declarations"))
        }
    }

  // An `override` with no declaration found overrides one this reading does not see.
  private def overriding: Option[Either[String, Inferred]] =
    Option.when(definition.mods.exists(_.is[Mod.Override]))(
      Left(s"$Unresolved: an override of a declaration not found")
    )

  /** A declaration of a parent level the target may override, with its clauses and result. */
  private case class Candidate(
      step: InheritedScopes.Step,
      groups: List[Member.ParamClauseGroup],
      result: Option[Type]
  )

  // One level's methods of the target's name and clause shape that subclasses inherit, each kept
  // when its parameter types are the target's, or refused when that is not known.
  private def candidates(
      template: Frame,
      step: InheritedScopes.Step
  ): List[Either[String, Option[Candidate]]] =
    step.declared.stats
      .flatMap(stat =>
        overridable(stat)
          .filter(same(stat, _))
          .map(method => Candidate(step, method.groups, method.result))
      )
      .map(candidate => matching(template, candidate).map(Option.when(_)(candidate)))

  // The methods a declaration gives a subclass to implement: its own signature, an abstract `var`'s
  // getter and setter, a given's.
  private def overridable(stat: Stat): List[Signature] = stat match {
    case v: Decl.Var =>
      v.pats.flatMap(TypeApplications.patVarName.getOption).flatMap { getter =>
        List(
          Signature(getter, Nil, Some(v.decltpe)),
          Signature(s"${getter}_=", List(setter(v.decltpe)), Some(InferredResults.rooted(Unit)))
        )
      }
    case g: Decl.Given      => givenMethod(g.name, g.paramClauseGroups, g.decltpe)
    case g: Defn.GivenAlias => givenMethod(g.name, g.paramClauseGroups, g.decltpe)
    case other              => InferredResults.signature(other).toList
  }

  private val Unit = List("scala", "Unit")

  private def setter(tpe: Type): Member.ParamClauseGroup =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      List(Term.ParamClause(List(Term.Param(Nil, Term.Name("x$1"), Some(tpe), None)), None))
    )

  private def givenMethod(
      name: Name,
      groups: List[Member.ParamClauseGroup],
      tpe: Type
  ): List[Signature] =
    Option.when(name.value.nonEmpty)(Signature(name.value, groups, Some(tpe))).toList

  private def same(stat: Stat, method: Signature): Boolean =
    method.name == name && !ScopeTerms.privateMember(stat) && sameShape(method)

  // A context bound adds an evidence parameter whose clause Scala places itself: a member with one
  // matches by name alone, and its parameters are not compared.
  private def sameShape(method: Signature): Boolean =
    ownBounded || bounded(method.groups) ||
      FinalImplementations.shape(method.groups) ==
      FinalImplementations.shape(definition.paramClauseGroups)

  private def bounded(groups: List[Member.ParamClauseGroup]): Boolean =
    groups.exists(_.tparamClause.values.exists(p => (p.bounds.context ++ p.bounds.view).nonEmpty))

  // Whether the candidate's parameter types are the target's: false when a known pair differs (an
  // overload), refused when an identity is not known.
  private def matching(template: Frame, candidate: Candidate): Either[String, Boolean] = {
    val theirs = context(template, candidate)
    val pairs = parameters(definition.paramClauseGroups)
      .zip(parameters(candidate.groups))
      .map((own, other) =>
        (
          own.flatMap(TypeIdentities.parameter(_, ownContext, resolver)),
          other.flatMap(TypeIdentities.parameter(_, theirs, resolver))
        )
      )
    val differs = pairs.exists { case (Some(own), Some(other)) => own != other; case _ => false }
    val comparable = !ownBounded && !bounded(candidate.groups)
    val known = pairs.forall((own, other) => own.nonEmpty && other.nonEmpty)
    if (differs) Right(false)
    else if (comparable && known) Right(true)
    else
      Left(
        s"$Unresolved: overriding member $name, whose parameter types are not matched against " +
          candidate.step.declared.path.lastOption.getOrElse("")
      )
  }

  private def parameters(groups: List[Member.ParamClauseGroup]): List[Option[Type]] =
    groups.flatMap(_.paramClauses).flatMap(_.values).map(_.decltpe)

  // A parent's declaration read where it is declared: its binders as the `extends` chain
  // instantiates them, the method's own binders as the target's position by position, and the
  // subclass as `this`, so an abstract type member it names is the subclass's.
  private def context(template: Frame, candidate: Candidate): ResolutionContext = {
    val renamed = InferredResults
      .binders(candidate.groups)
      .zip(InferredResults.binders(definition.paramClauseGroups))
      .map((own, other) => own -> variables.getOrElse(other, Left(other)))
    ResolutionContext(
      candidate.step.declared,
      candidate.step.env ++ renamed,
      Set.empty,
      Some(target.frame)
    ).as(MemberPath.self(template, resolver.typeParameters(template)))
  }

  private def inherited(
      template: Frame,
      step: InheritedScopes.Step,
      groups: List[Member.ParamClauseGroup],
      declared: Type
  ): Either[String, Inferred] =
    if (InferredResults.dependent(declared, groups))
      Left(s"$Unresolved: dependent result of an overridden $name")
    else Right(Inferred(declared, context(template, Candidate(step, groups, Some(declared)))))

}
