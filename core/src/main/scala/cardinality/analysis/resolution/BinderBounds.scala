package cardinality.analysis.resolution

import scala.meta.{Defn, Type, XtensionSyntax}

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.Resolver
import cardinality.analysis.inhabitation.ExistentialInputs
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.ResolutionContext
import cardinality.capacity.Counter
import cardinality.types.{Kinds, Spelling}

/** A subtype bound on a proper binder is an ambient proof (hkt R5), which this reading does not
  * construct: a bound other than `Any` above refuses the binder, unless it is `AnyRef`, `AnyVal` or
  * `Matchable`, which say nothing of the binder's values. A bound to an open class needs that
  * class's members as capabilities; any other bound keeps the reason its reading gives. A bound is
  * read only for a binder the environment mentions.
  */
final private[cardinality] class BinderBounds(resolver: Resolver, observer: Frame) {

  /** Why a clause of the observer's scopes is no Scala program, if one is not: the signature is
    * then no program either, whatever its result and whether or not the clause's binders are read.
    */
  lazy val invalid: List[String] = observer.chain.flatMap(frame => Kinds.invalid(frame.typeParams))

  /** The proofs of every bounded binder the shapes mention, and why a bound could not be read. */
  def of(shapes: List[Shape]): (List[Shape], List[String]) =
    (
      Nil,
      shapes
        .flatMap(ExistentialInputs.freeAtoms)
        .distinct
        .flatMap(resolver.boundedBinders.get)
        .flatMap((scope, parameter) => read(scope, parameter))
    )

  private def read(scope: Frame, parameter: Type.Param): Option[String] = {
    val context =
      ResolutionContext(scope, resolver.typeParameters(scope), Set.empty, Some(observer))
    parameter.bounds.hi
      .filterNot(TopTypes.isAny(_, context, resolver))
      .flatMap { bound =>
        classBound(bound, context) match {
          case Some(_) => None
          case None    => Some(read(bound, context, parameter))
        }
      }
      .orElse(parameter.bounds.lo.map(_ => s"lower bound not read: ${Spelling.of(parameter)}"))
  }

  private def read(bound: Type, context: ResolutionContext, parameter: Type.Param): String =
    resolver.resolve(bound, context) match {
      case Right(_) =>
        s"bound to a structural type requires coercion analysis: ${Spelling.of(parameter)}"
      case Left(_) if membersOnly(bound, context) =>
        s"upper bound requires capability analysis: ${Spelling.of(parameter)}"
      case Left(reason) => reason
    }

  // A bound declared without a sealed structure — an open class — is known by its members alone:
  // its values are read through them, as capabilities.
  private def membersOnly(bound: Type, context: ResolutionContext): Boolean =
    Found.named(resolver, bound, bound.syntax, context).exists {
      case Found.Entry(entry) =>
        entry.tree match {
          case c: Defn.Class => !Counter.isSealed(c)
          case _             => false
        }
      case _ => false
    }

  // `AnyRef` (or `Object`), `AnyVal` or `Matchable` above, when the name is the builtin's where it
  // is read: past every channel that may rebind a top name (`TopTypes.denoted`).
  private def classBound(bound: Type, context: ResolutionContext): Option[String] =
    TopTypes.denoted(bound, context, resolver).filterNot(_ == TopTypes.Kind.Universal).map(_.label)

}
