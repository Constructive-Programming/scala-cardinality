package cardinality.analysis.model

import scala.meta.Type

import cardinality.analysis.*
import dev.constructive.eo.generics.lens

import MethodAnalysis.{Frame, Resolved}

/** Lexical lookup and binder scope travel together; the observing use site survives rebinding. */
final private[cardinality] case class ResolutionContext(
    frame: Frame,
    variables: Map[String, Resolved],
    visiting: Set[String],
    observer: Option[Frame] = None
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

}
