package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*

import MethodAnalysis.{Frame, TypeEntry}

/** Representation access follows the use site, never the scope of an exported transparent alias. */
private[cardinality] object OpaqueTypes {

  def hidden(entry: TypeEntry, from: Frame): Boolean = entry.tree match {
    case alias: Defn.Type if alias.mods.exists(_.is[Mod.Opaque]) => !visible(entry, from)
    case _                                                       => false
  }

  private def visible(entry: TypeEntry, from: Frame): Boolean =
    if (entry.owner.tree.nonEmpty) from.chain.exists(_.id == entry.owner.id)
    else topLevelVisible(entry, from)

  private def topLevelVisible(entry: TypeEntry, from: Frame): Boolean =
    if (entry.owner.chain.last.id != from.chain.last.id) false
    else if (!samePackage(entry.owner, from)) false
    else if (companionScope(entry, from)) true
    else !from.chain.exists(scope => scope.tree.exists(template))

  private def samePackage(owner: Frame, from: Frame): Boolean =
    from.chain.find(_.tree.isEmpty).exists(_.path == owner.path)

  private def companionScope(entry: TypeEntry, from: Frame): Boolean =
    from.chain.exists(scope =>
      scope.tree match {
        case Some(_: Defn.Object) => scope.path == entry.owner.path :+ entry.name
        case _                    => false
      }
    )

  private def template(tree: Tree): Boolean = tree match {
    case _: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
    case _: Defn.Given | _: Pkg.Object                                 => true
    case _                                                             => false
  }

}
