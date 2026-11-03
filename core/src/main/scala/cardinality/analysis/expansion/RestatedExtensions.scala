package cardinality.analysis.expansion

import scala.meta.*

import cardinality.analysis.model.*
import cardinality.analysis.resolution.{Found, TypeIdentities}
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.{TypeApplications, Variance}

import MethodAnalysis.Frame

/** Applied selections no extension can answer (register D27). Scala tries an extension of the
  * selected name only where no member of the qualifier's type applies to the arguments. This
  * reading does not type arguments, so it keeps that fallback open, unless every extension of the
  * name the body may meet would reject what the member rejects: its receiver class is supplied and
  * no base class of the qualifier's (it never applies), or it restates the member — its receiver an
  * invariant base class of the qualifier's, every parameter of the group spelled once, and its one
  * value clause, under the qualifier's base type, the member's own clause: the same parameter names
  * and types, compared by the declarations they resolve to. The member is one the selection finds —
  * other alternatives only add ways for a member to apply — of the qualifier's final class, its own
  * and public, with one plain value clause the call fills.
  */
final private[expansion] class RestatedExtensions(terms: BodyTerms, resolver: Resolver) {
  import RestatedExtensions.Call

  def answered(call: Call, found: Extensions): Boolean = found match {
    case Extensions.Known(extensions) if call.clauses == List(false) =>
      (for {
        (template, method) <- members(call)
        levels <- terms.levels(template).toOption.toList
      } yield extensions.forall(rejects(_, template, levels, method))).contains(true)
    case _ => false
  }

  // An extension rejects what the member rejects: its receiver class is supplied and none of the
  // qualifier's base classes (the template and its `levels`), or it restates the member.
  private def rejects(e: Extension, template: Frame, levels: List[Frame], member: Defn.Def) =
    receiverClass(e).exists(r =>
      !(template :: levels).exists(_.id == r.id) || restates(e, r, template, member)
    )

  private def restates(e: Extension, base: Frame, template: Frame, member: Defn.Def): Boolean =
    receiverRestates(e, base) && clauseRestates(e, base, template, member)

  private def members(call: Call): List[(Frame, Defn.Def)] =
    (call.meaning, call.receiver()) match {
      case (Meaning.Methods(definitions), Owner(template)) if resolver.fixedTemplate(template) =>
        definitions.collect {
          case Definition(d: Defn.Def, _)
              if template.stats.exists(_ eq d) && !d.mods.exists(MemberSyntax.restricted) &&
                plain(d).exists(_.size == call.arguments) =>
            template -> d
        }
      case _ => Nil
    }

  // The class a value of the receiver is: a final one, read as its own or as an argument's.
  private object Owner {

    def unapply(receiver: Receiver): Option[Frame] = receiver match {
      case Receiver.Template(frame)                   => Some(frame)
      case Receiver.Open(Receiver.Template(frame), _) => Some(frame)
      case _                                          => None
    }

  }

  // A method's one value clause, with no binder, default, repeated or contextual parameter.
  private def plain(method: Defn.Def | Decl.Def): Option[List[Term.Param]] =
    MemberSyntax.groups(method) match {
      case List(group) if group.tparamClause.values.isEmpty =>
        group.paramClauses match {
          case List(clause)
              if !MemberSyntax.contextual(clause) &&
                clause.values.forall(p => p.default.isEmpty && !MemberSyntax.repeated(p)) =>
            Some(clause.values)
          case _ => None
        }
      case _ => None
    }

  private def receiverType(e: Extension): Option[Type] =
    e.group.paramClauseGroup
      .flatMap(_.paramClauses.headOption)
      .flatMap(_.values.headOption)
      .flatMap(_.decltpe)

  // The class an extension's receiver type names; none where its head is a type parameter — the
  // group's own or one of a scope around it — which may be instantiated to any class.
  private def receiverClass(e: Extension): Option[Frame] =
    receiverType(e)
      .map(TypeApplications.headAndArguments(_)._1)
      .filterNot {
        case Type.Name(name) =>
          binders(e)
            .contains(name) || e.scope.chain.exists(_.typeParams.exists(_.name.value == name))
        case _ => false
      }
      .flatMap(classOf(_, e.scope))

  private def binders(e: Extension): List[String] =
    e.group.paramClauseGroup.toList.flatMap(_.tparamClause.values).map(_.name.value)

  private def classOf(head: Type, scope: Frame): Option[Frame] =
    terms.typeDeclaration(head, scope)._1 match {
      case Found.Entry(entry) => entry.self
      case _                  => None
    }

  // The receiver is an invariant base class whose arguments are the group's binders, each spelled
  // once.
  private def receiverRestates(e: Extension, base: Frame): Boolean = {
    val slots = receiverSlots(e)
    invariant(base) && slots.distinct == slots && slots.toSet == binders(e).toSet
  }

  private def receiverSlots(e: Extension): List[String] =
    receiverType(e).toList.flatMap(TypeApplications.headAndArguments(_)._2).collect {
      case Type.Name(name) => name
    }

  // Under the base type the class gives `base`, the extension's one value clause is the member's:
  // the same parameter names, with types resolving to the same declarations.
  private def clauseRestates(
      e: Extension,
      base: Frame,
      template: Frame,
      member: Defn.Def
  ): Boolean = {
    val inClass = ResolutionContext(template, resolver.typeParameters(template), Set.empty)
    val slots = receiverSlots(e)
    (for {
      arguments <- baseArguments(base, template)
      if arguments.size == slots.size
      theirs <- plain(e.method)
      ours <- plain(member)
      if theirs.size == ours.size
    } yield {
      val bound = slots.zip(arguments.map(resolver.resolve(_, inClass))).toMap
      val inExtension = ResolutionContext(e.scope, bound, Set.empty)
      theirs.zip(ours).forall((t, o) => sameParameter(t, inExtension, o, inClass))
    }).contains(true)
  }

  private def sameParameter(
      theirs: Term.Param,
      inExtension: ResolutionContext,
      ours: Term.Param,
      inClass: ResolutionContext
  ): Boolean =
    theirs.name.value == ours.name.value && theirs.decltpe.zip(ours.decltpe).exists { (t, o) =>
      val identity = TypeIdentities.parameter(o, inClass, resolver)
      identity.nonEmpty && TypeIdentities.parameter(t, inExtension, resolver) == identity
    }

  // The arguments the class's base type gives a base class: its own binders for itself, its
  // `extends` clause's for a direct parent; another level is not followed.
  private def baseArguments(base: Frame, template: Frame): Option[List[Type]] =
    if (base.id == template.id) Some(template.typeParams.map(p => Type.Name(p.name.value)))
    else
      template.parents
        .find(init =>
          resolver
            .parent(init, template)
            .toOption
            .flatten
            .flatMap(_._1.self)
            .exists(_.id == base.id)
        )
        .map(init => TypeApplications.headAndArguments(init.tpe)._2)

  private def invariant(base: Frame): Boolean =
    base.typeParams.forall(Variance.of(_) == Variance.Invariant)

}

private[expansion] object RestatedExtensions {

  /** The name a call applies when its member may reject the arguments, and the call itself where
    * the name's extensions may only restate the member.
    */
  final case class Applied(name: String, call: Option[Call])

  /** An applied selection `q.m(args)`: what `q` is (read on demand), what `m` selects there, the
    * arguments the call passes and whether each of its clauses is a `using` one.
    */
  final case class Call(
      receiver: () => Receiver,
      meaning: Meaning,
      arguments: Int,
      clauses: List[Boolean]
  )

}
