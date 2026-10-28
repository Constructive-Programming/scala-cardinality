package cardinality.analysis.model

import scala.meta.Type

import cardinality.analysis.*
import cardinality.types.TypeApplications
import dev.constructive.eo.generics.lens

import MethodAnalysis.{Frame, Resolved, TypeEntry}

/** Lexical lookup and binder scope travel together; the observing use site survives rebinding. */
final private[cardinality] case class ResolutionContext(
    frame: Frame,
    variables: Map[String, Resolved],
    visiting: Set[String],
    observer: Option[Frame] = None,
    // The position the name is read from, as (source, offset): the reference decides which
    // import clauses of the enclosing blocks have already bound their names. `resolve` sets it
    // from the type it is given, so every read carries the position of its own reference.
    at: Option[(String, Int)] = None
) {

  def useSite: Frame = observer.getOrElse(frame)

  def read(tpe: Type, resolver: Resolver): Resolved =
    resolver.resolve(tpe, this)

  def bind(entries: Iterable[(String, Resolved)]): ResolutionContext =
    ResolutionOptics.variablesLens.modify(_ ++ entries)(this)

  def enter(key: String): ResolutionContext =
    ResolutionOptics.visitingLens.modify(_ + key)(this)

  /** Alias declarations supply lexical names, not their caller's representation permissions. */
  def inScope(owner: Frame, bindings: Map[String, Resolved]): ResolutionContext =
    copy(frame = owner, variables = bindings, observer = Some(useSite))

}

private[cardinality] object ResolutionOptics {

  val variablesLens = lens[ResolutionContext](_.variables)
  val visitingLens = lens[ResolutionContext](_.visiting)
  val entryTreeLens = lens[TypeEntry](_.tree)
  val entryTypeDefinition = entryTreeLens.andThen(TypeApplications.typeDefinition)
  val typeArguments = TypeApplications.arguments
  val patVarName = TypeApplications.patVarName

}
