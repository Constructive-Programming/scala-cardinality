package cardinality

import scala.meta.Type

import MethodAnalysis.{Frame, Resolved}

/** The lexical scope, binder environment and recursion guard must travel together. */
final private[cardinality] case class ResolutionContext(
    frame: Frame,
    variables: Map[String, Resolved],
    visiting: Set[String]
) {

  def read(tpe: Type, resolver: Resolver): Resolved =
    resolver.resolve(tpe, frame, variables, visiting)

  def enter(key: String): ResolutionContext = copy(visiting = visiting + key)
}
