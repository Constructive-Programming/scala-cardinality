package cardinality.analysis.resolution

import scala.meta.Type

import cardinality.analysis.MethodAnalysis.TypeEntry
import cardinality.analysis.Resolver
import cardinality.analysis.model.ResolutionContext

/** What a simple-name resolution reached: one declaration, a refusal to read past the name with the
  * reason, a declaration outside the source set, or nothing — which is where the model's own
  * vocabulary may still answer.
  */
sealed private[cardinality] trait Found

private[cardinality] object Found {
  final case class Entry(entry: TypeEntry) extends Found

  /** A lookup that reached neither a supplied declaration nor the builtins, with its reason. */
  sealed trait Failed extends Found {
    def reason: String
  }

  /** A refusal; `hidden` is the supplied declaration an unsupplied wildcard may hide. */
  final case class Blocked(reason: String, hidden: Option[TypeEntry] = None) extends Failed

  object Blocked {

    def hiding(reason: String, found: Option[Found]): Blocked =
      Blocked(reason, found.collect { case Entry(entry) => entry })

  }

  // An import binds the name to a path outside the source set: a declaration that exists, but was
  // not supplied — or one the stdlib model reads (`import java.util.List`).
  final case class Foreign(path: String, binding: String) extends Failed {

    /** Why a name bound outside the sources is not read: the information is absent. */
    def reason: String = s"declaration not supplied: $path ($binding)"
  }

  case object Absent extends Found

  /** What a type written as a name (`T`, `p.T`) denotes where it is read; None for any other type.
    */
  def named(
      resolver: Resolver,
      tpe: Type,
      name: String,
      context: ResolutionContext
  ): Option[Found] =
    tpe match {
      case _: Type.Name | _: Type.Select => Some(resolver.declaration(tpe, name, context))
      case _                             => None
    }

}
