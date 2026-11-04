package cardinality.analysis

import scala.collection.mutable
import scala.meta.{Defn, Member, Mod, Term, Tree, Type}

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{ContextualEvidence, MemberSyntax, ResolutionContext}
import cardinality.analysis.resolution.*
import cardinality.types.Spelling

import ContextFunctions.Binder
import ContextualEvidence.Nominal
import MethodAnalysis.{Frame, Resolved, Target}

/** The context-function binders around a target's body (`ContextFunctions`), by the definition
  * whose result puts them there: the target's own, read from its result as declared or inferred,
  * and each enclosing one's, which every definition nested in its body reaches through implicit
  * search alone — a wrapped body's binders are synthetic. An enclosing method without a declared
  * result has one only where it overrides a declaration: Scala types it by that declaration's
  * result as seen from the subclass (fixture cf3 `C.g`), while a result inferred from the body sets
  * no expected type and wraps nothing.
  */
final private[cardinality] class ContextBinders(
    target: Target,
    inferred: => Either[String, InferredResults.Inferred],
    steps: => List[InheritedScopes.Step],
    reading: Frame => ResolutionContext,
    resolver: Resolver
) {
  import ContextBinders.Read

  private val reads = mutable.Map.empty[String, Either[String, List[Read]]]

  /** The binders a definition of the target's chain puts around the code nested in it, each read
    * where its result is, or why they are not known.
    */
  def of(owner: Frame): Either[String, List[Read]] =
    reads.getOrElseUpdate(
      owner.id,
      binders(owner).map(_.zipWithIndex.map((binder, index) => read(owner, binder, index)))
    )

  private def binders(owner: Frame): Either[String, List[Binder]] =
    if (owner.id != target.frame.id)
      enclosing(owner).left.map(reason => s"$reason, in the result of enclosing ${owner.path.last}")
    else if (target.constructor) Right(Nil)
    // An unsupplied name in the target's result is reported by the goal's own reading, so this
    // chain need not be strict.
    else
      inferred.fold(
        _ => Right(Nil),
        result => ContextFunctions.binders(result.tpe, result.context, resolver)
      )

  // A member value or given alias wraps its right-hand side as a method does its body (an
  // anonymous class or a local definition there is nested in it); one written without a type takes
  // an overridden declaration's result as its expected type, binders and all. A right-hand side that
  // already is a context-function literal is not wrapped (Scala 3 reference, context functions: an
  // expression is converted only when it is no such literal): the literal binds its clause through
  // its own parameters, which its `<lambda#k>` frame reads, and a literal directly in its body the
  // next. Only the clauses past those wrap the innermost body.
  private def enclosing(owner: Frame): Either[String, List[Binder]] =
    (owner.tree match {
      case Some(method: Defn.Def) =>
        method.decltpe.fold(overridden(owner, method.mods, method.paramClauseGroups))(
          enclosed(_, reading(owner))
        )
      case Some(alias: Defn.GivenAlias) => enclosed(alias.decltpe, reading(owner))
      case Some(value: Defn.Val)        => typed(value.mods, value.decltpe, owner)
      case Some(variable: Defn.Var)     => typed(variable.mods, variable.decltpe, owner)
      case _                            => Right(Nil)
    }).map { binders =>
      val literals =
        owner.tree.flatMap(MemberSyntax.rightHandSide).fold(0)(ContextBinders.literals)
      binders.filter(_.clause >= literals)
    }

  private def typed(
      mods: List[Mod],
      declared: Option[Type],
      owner: Frame
  ): Either[String, List[Binder]] =
    declared.fold(overridden(owner, mods, Nil))(enclosed(_, reading(owner)))

  private def overridden(
      owner: Frame,
      mods: List[Mod],
      groups: List[Member.ParamClauseGroup]
  ): Either[String, List[Binder]] =
    owner.name.fold(Right(Nil)) { name =>
      new OverriddenResults(
        owner,
        OverriddenResults.Overriding(name, mods, groups),
        resolver.typeParameters(owner),
        resolver
      )
        .result(steps)
        .fold(Right(Nil))(_.flatMap(result => enclosed(result.tpe, result.context)))
    }

  // Nothing else reads an enclosing method's result: a name it cannot see behind refuses.
  private def enclosed(tpe: Type, context: ResolutionContext) =
    ContextFunctions.required(tpe, context, resolver)

  // Binders are nameless: each is bound under its owner and position.
  private def read(owner: Frame, binder: Binder, index: Int): Read = {
    val shape = resolver.resolve(binder.tpe, binder.context)
    Read(
      binder.clause,
      s"context function binder ${Spelling.of(binder.tpe)}",
      shape,
      shape.toOption.toList
        .flatMap(ContextFunctions.results(binder.tpe, _, binder.context, resolver)),
      Nominal(
        TypeIdentities.of(binder.tpe, binder.context, resolver),
        TypeIdentities.finalClass(binder.tpe, binder.context, resolver)
      ),
      s"<context#$index@${owner.path.mkString(".")}>"
    )
  }

}

private[cardinality] object ContextBinders {

  /** How many context-function literals a term starts with, each the body of the one before, a
    * block holding only one counting as the literal (Scala checks the closure inside such a block).
    */
  def literals(term: Term): Int = term match {
    case f: Term.ContextFunction      => 1 + literals(f.body)
    case Term.Block(List(only: Term)) => literals(only)
    case _                            => 0
  }

  /** A binder as the scope reads it: its clause, label, type read, the results a search may apply
    * it for (`ContextFunctions.results`), identity, and the key it is bound under.
    */
  final case class Read(
      clause: Int,
      label: String,
      shape: Resolved,
      results: List[Shape],
      nominal: Nominal,
      key: String
  )

}
