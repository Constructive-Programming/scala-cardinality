package cardinality.analysis.resolution

import scala.meta.{Defn, Mod, Pkg, Tree, XtensionClassifiable}

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.model.ResolutionContext

/** Representation access follows the use site, never the scope of an exported transparent alias. */
private[cardinality] object OpaqueTypes {

  /** Whether the source declaration `entry` can be read from `context`: not one this reading is
    * already inside (a cycle needs a structural proof), and not an opaque type hidden from the use
    * site.
    */
  def readable(context: ResolutionContext, entry: TypeEntry): Either[String, Unit] =
    if (context.visiting(entry.key))
      Left(s"recursive type requires a structural proof: ${entry.key}")
    else if (hidden(entry, context.useSite))
      Left(s"opaque representation is not visible at use site: ${entry.key}")
    else Right(())

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

  // Scala 3 compiles the companion object of a top-level type — opaque or a plain alias, such as
  // `object MF` beside `type MF[F[_]] = [X, A] =>> MFK[F, X, A]` — into the file's package object
  // (`f$package$MF`), where every top-level opaque type of the file is transparent.
  private def companionScope(entry: TypeEntry, from: Frame): Boolean =
    from.chain.exists(scope =>
      scope.tree match {
        case Some(module: Defn.Object) =>
          scope.path == entry.owner.path :+ module.name.value && accompaniesType(module)
        case _ => false
      }
    )

  private def accompaniesType(module: Defn.Object): Boolean =
    module.parent.exists(_.children.exists {
      case alias: Defn.Type => alias.name.value == module.name.value
      case _                => false
    })

  private def template(tree: Tree): Boolean = tree match {
    case _: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
    case _: Defn.Given | _: Pkg.Object                                 => true
    case _                                                             => false
  }

}
