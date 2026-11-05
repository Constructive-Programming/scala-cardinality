package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.model.{MemberSyntax, ResolutionContext}
import cardinality.analysis.resolution.{Found, InheritedScopes, OverrideIdentity}
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.Traversals.sequence
import cardinality.types.TypeApplications

import MethodAnalysis.Frame
import OverrideIdentity.MethodMember
import TargetViews.Applied

/** What a body on one receiver calls: the declarations a call's head may denote, whether they take
  * each argument list by value, and whether a qualifier's declared template has a member (C6).
  *
  * An argument is evaluated before the call only when every declaration the head may denote takes
  * it by value. A by-name or `inline` parameter does not, nor does one whose type is a context
  * function, an alias of one, or a binder the call may instantiate with one — the compiler wraps
  * such an argument in a lambda: explicit type arguments for the method's own binders, the extends
  * chain for another level's class binders, the declared type's arguments for a qualifier's
  * template's. A binder of the body's own level is abstract there, and a method binder the call
  * infers is its argument's type, so the compiler wraps nothing for them.
  */
final private[capability] class ReceiverCalls(levels: List[Frame], resolver: Resolver) {
  import ReceiverCalls.*

  /** For each argument list of a call whose head is `head`, read in a body at `at` that sees the
    * levels `visible` selects, whether it fills a by-value clause of every declaration the head may
    * denote; `None` when the head is not looked up.
    */
  def byValue(
      head: Term,
      call: Call,
      at: Frame,
      visible: String => Boolean
  ): Option[List[Boolean]] =
    callees(head, at, visible).filter(_.nonEmpty).map { found =>
      found
        .map(callee => eagerClauses(callee.method, call.contextual, byValue(callee, call.typed)))
        .transpose
        .map(_.forall(eager => eager))
    }

  /** Whether the receiver member `member` (parameterless, with a declared type) has a template
    * declaring or inheriting `name` at the given arity: then selecting it evaluates the member.
    */
  def declares(
      member: String,
      name: String,
      applied: Applied,
      visible: String => Boolean
  ): Boolean =
    declaredType(member, visible).flatMap((tpe, level) => templateOf(tpe, level)).exists {
      template =>
        sequence(
          InheritedScopes
            .walk(List(template -> resolver.typeParameters(template)), resolver, template)
        ).toOption.exists { steps =>
          (template :: steps.map(_.declared)).exists(level =>
            ReceiverMembers.fields(level)(name) || level.stats.exists(stat =>
              MemberSyntax.names(stat).contains(name) &&
                ReceiverMembers.Signature.of(stat).accepts(applied)
            )
          )
        }
    }

  // The declarations a call's head may denote: a method of a level the body sees, or a method of a
  // declared receiver member's own template when that template inherits nothing that could add an
  // overload. `None` when this reading does not look the head up.
  private def callees(head: Term, at: Frame, visible: String => Boolean): Option[List[Callee]] =
    head match {
      case Term.Name(name) =>
        Some(levels.filter(l => visible(l.id)).flatMap { level =>
          level.stats.flatMap(methodNamed(name)).map(Callee(_, classBinders(level, at)))
        })
      case Term.Select(Term.Name(qualifier), Term.Name(method)) =>
        declaredType(qualifier, visible).flatMap { (tpe, level) =>
          templateOf(tpe, level).collect {
            case template if template.parents.isEmpty =>
              val arguments = TypeApplications.arguments.getOption(tpe).getOrElse(Nil)
              val bound = template.typeParams.map(_.name.value).zip(arguments).toMap
              template.stats.flatMap(methodNamed(method)).map { declaration =>
                Callee(declaration, name => bound.get(name).map(Binder.Argument(_, level, at)))
              }
          }
        }
      case _ => None
    }

  // A level's class binders as a body at `at` sees them: abstract at its own level, instantiated
  // by the extends chain at any other.
  private def classBinders(level: Frame, at: Frame): String => Option[Binder] =
    name =>
      Option.when(level.typeParams.exists(_.name.value == name))(
        if (level.id == at.id) Binder.Abstract else Binder.Unknown
      )

  // Whether a parameter takes its argument by value at this call.
  private def byValue(callee: Callee, typed: Boolean)(parameter: Term.Param): Boolean = {
    val own = callee.method.paramClauseGroups.flatMap(_.tparamClause.values).map(_.name.value)
    val binder: String => Option[Binder] = name =>
      if (own.contains(name)) Some(if (typed) Binder.Unknown else Binder.Abstract)
      else callee.binder(name)
    !parameter.mods.exists(_.is[Mod.Inline]) && !parameter.decltpe.exists(deferred(_, binder, 0))
  }

  // Whether an argument of this type may be wrapped instead of evaluated.
  private def deferred(tpe: Type, binder: String => Option[Binder], depth: Int): Boolean =
    resolver.insertions.deferring(tpe) || (tpe match {
      case Type.Repeated(element) => deferred(element, binder, depth)
      case Type.Name(name)        =>
        binder(name).exists {
          case Binder.Abstract                 => false
          case Binder.Unknown                  => true
          case Binder.Argument(arg, level, at) =>
            depth >= resolver.limits.maxTypeDepth ||
            deferred(arg, classBinders(level, at), depth + 1)
        }
      case _ => false
    })

  private def methodNamed(name: String)(stat: Stat): Option[MethodMember] = stat match {
    case d: Decl.Def if d.name.value == name => Some(d)
    case d: Defn.Def if d.name.value == name => Some(d)
    case _                                   => None
  }

  // A receiver member's declared type, where a parameterless member of a level the body sees
  // declares one, with its level.
  private def declaredType(name: String, visible: String => Boolean): Option[(Type, Frame)] =
    levels.iterator
      .filter(level => visible(level.id))
      .flatMap(level => level.stats.collectFirst(Function.unlift(declared(name))).map(_ -> level))
      .nextOption()

  private def declared(name: String)(stat: Stat): Option[Type] = stat match {
    case d: Decl.Def if d.name.value == name && d.paramClauseGroups.isEmpty => Some(d.decltpe)
    case d: Defn.Def if d.name.value == name && d.paramClauseGroups.isEmpty => d.decltpe
    case d: Decl.Val if TypeApplications.binds(name, d.pats)                => Some(d.decltpe)
    case d: Defn.Val if TypeApplications.binds(name, d.pats)                => d.decltpe
    case _                                                                  => None
  }

  private def templateOf(tpe: Type, level: Frame): Option[Frame] = {
    val head = tpe match {
      case Type.Apply.After_4_6_0(callee, _) => callee
      case other                             => other
    }
    val context =
      ResolutionContext(level, resolver.typeParameters(level), Set.empty).referencing(head)
    resolver.resolveName(TypeApplications.spelled(head), context) match {
      case Found.Entry(entry) => entry.self.filter(_.isTemplate)
      case _                  => None
    }
  }

}

private[capability] object ReceiverCalls {

  /** One call's argument lists, each flagged when it is an explicit `using` list, and whether it
    * passes explicit type arguments.
    */
  final case class Call(contextual: List[Boolean], typed: Boolean)

  // How a call sees a binder a parameter type names: abstract where the body is written, or
  // instantiated — by an extends chain this reading does not follow, or by a declared type's
  // argument written at `level` and read from a body at `at`.
  private enum Binder {
    case Abstract, Unknown
    case Argument(tpe: Type, level: Frame, at: Frame)
  }

  // One declaration a call's head may denote, with the binders its parameter types may name.
  final private case class Callee(method: MethodMember, binder: String => Option[Binder])

  /** For each argument list of a call (each flagged when it is an explicit `using` list), whether
    * it fills a clause whose parameters all take their arguments by value: a `using` list fills the
    * next `using` clause, any other the next clause the compiler does not fill. A Scala 2
    * `implicit` clause may be filled either way, so it leaves every list unknown.
    */
  def eagerClauses(
      method: MethodMember,
      contextual: List[Boolean],
      byValue: Term.Param => Boolean
  ): List[Boolean] = {
    val clauses = method.paramClauseGroups.flatMap(_.paramClauses)
    def fill: List[Boolean] =
      contextual
        .foldLeft((clauses, List.empty[Boolean])) {
          case ((remaining, acc), explicit) =>
            remaining.dropWhile(c => MemberSyntax.contextual(c) != explicit) match {
              case clause :: after => (after, clause.values.forall(byValue) :: acc)
              case Nil             => (Nil, false :: acc)
            }
        }
        ._2
        .reverse
    if (clauses.exists(c => MemberSyntax.contextual(c) && !c.mod.exists(_.is[Mod.Using])))
      List.fill(contextual.size)(false)
    else fill
  }

}
