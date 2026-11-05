package cardinality.analysis.resolution

import scala.meta.{Defn, Mod, XtensionClassifiable}

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{MemberSyntax, ResolutionContext}
import cardinality.types.Trees

/** Representation access follows the use site, never the scope of an exported transparent alias.
  * Outside its scope an opaque type counts one value (v1.md §2, opaque types): its representation
  * is hidden, and the model reads it as `Unit` there.
  */
private[cardinality] object OpaqueTypes {

  /** The one value an opaque type has where its representation is hidden. */
  val single: Shape = Shape.Product(Nil)

  /** Whether the source declaration `entry` can be read from `context`: not one this reading is
    * already inside (a cycle is not read).
    */
  def readable(context: ResolutionContext, entry: TypeEntry): Either[String, Unit] =
    if (context.visiting(entry.key)) Left(s"recursive type not read: ${entry.key}")
    else Right(())

  def hidden(entry: TypeEntry, from: Frame): Boolean = entry.tree match {
    case alias: Defn.Type if alias.mods.exists(_.is[Mod.Opaque]) => !visible(entry, from)
    case _                                                       => false
  }

  private def visible(entry: TypeEntry, from: Frame): Boolean =
    if (entry.owner.tree.nonEmpty) from.chain.exists(_.id == entry.owner.id)
    else topLevelVisible(entry, from)

  private def topLevelVisible(entry: TypeEntry, from: Frame): Boolean =
    entry.owner.chain.last.id == from.chain.last.id && samePackage(entry.owner, from) &&
      from.chain
        .flatMap(_.tree)
        .headOption
        .forall(Trees.topLevelStatement(_).exists(MemberSyntax.wrappedInPackageObject))

  private def samePackage(owner: Frame, from: Frame): Boolean =
    from.chain.find(_.tree.isEmpty).exists(_.path == owner.path)

}
