package cardinality.analysis.expansion

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.Resolver

/** The objects that make up the implicit scope a template anchors (register D43): its companion, an
  * object of the same path, and the objects it is a member of. A qualifier's anchors
  * (`Conversions.anchored`) and the closure of what an inline body can reach (`Reachable`) both
  * read a template's contribution here.
  */
final private[expansion] class ImplicitScope(resolver: Resolver) {

  /** The supplied objects of each path. */
  lazy val objects: Map[List[String], List[Frame]] =
    resolver.memberScopes.filter(_.isObject).groupBy(_.path)

  /** A template's companion and the objects it is a member of, innermost first. */
  def of(template: Frame): List[Frame] =
    objects.getOrElse(template.path, Nil) ++ template.chain.drop(1).filter(_.isObject)

}
