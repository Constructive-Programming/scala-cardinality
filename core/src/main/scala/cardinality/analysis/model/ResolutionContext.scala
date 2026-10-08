package cardinality.analysis.model

import scala.meta.Type

import cardinality.analysis.*

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

  def enter(key: String): ResolutionContext = copy(visiting = visiting + key)

  /** Alias declarations supply lexical names, not their caller's representation permissions. */
  def inScope(owner: Frame, bindings: Map[String, Resolved]): ResolutionContext =
    copy(frame = owner, variables = bindings, observer = Some(useSite))

}
