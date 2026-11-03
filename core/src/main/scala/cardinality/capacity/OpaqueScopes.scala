package cardinality.capacity

import scala.meta.*

import cardinality.analysis.model.MemberSyntax
import cardinality.types.Trees

// Where an opaque alias is transparent (Scala 3 reference, opaque type aliases): inside the
// template that declares it, or, for a top-level one, inside its file's synthetic package object —
// the statements of its package block `MemberSyntax.wrappedInPackageObject` says Scala wraps, as the
// method side's `OpaqueTypes` reads them (`StoredScopesSpec`'s `opaqueTopLevel`). Elsewhere, and for
// a type rebuilt with no position, a reference is a reason.
private[capacity] object OpaqueScopes {

  /** Whether `alias`'s representation is visible where `reference` is written. */
  def transparent(alias: Defn.Type, reference: Tree): Boolean =
    alias.parent match {
      case Some(body: Template.Body)        => Trees.ancestors(reference).exists(_ eq body)
      case Some(block: (Pkg.Body | Source)) =>
        Trees
          .topLevelStatement(reference)
          .exists(stat =>
            stat.parent.exists(_ eq block) && MemberSyntax.wrappedInPackageObject(stat)
          )
      case _ => false
    }

  /** Why a reference outside the alias's scope has no number. */
  def outside(alias: Defn.Type): Definition.Unbound =
    Definition.Unbound.Refusal(s"opaque type read outside its scope: ${Qualified.of(alias)}")

}
