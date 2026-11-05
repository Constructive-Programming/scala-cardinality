package cardinality.capacity

import scala.meta.*

import cardinality.analysis.model.MemberSyntax
import cardinality.capacity.ParentNames.Denotes
import cardinality.types.{Overrides, SealedSlots, Trees, TypeApplications}

// A case class subtype's parents as its file reads them: a class or trait of the file the plain
// name denotes where the clause is written (`ParentNames`). Anything else is not read.
final private[capacity] class FileParents(leaf: Defn.Class) extends Overrides.Parents[Defn] {

  def body(level: Defn): List[Stat] = template(level).fold(List.empty[Stat])(_.body.stats)

  def fields(level: Defn): List[Term.Param] = MemberSyntax.fieldParams(level)

  def key(level: Defn): String = s"${level.pos.start}:${level.pos.end}"

  def parents(level: Defn): List[(String, Overrides.Parent[Defn])] =
    template(level).fold(List.empty[Template])(List(_)).flatMap(_.inits).map { init =>
      val name = TypeApplications.parentName(init)
      name -> SealedSlots.plainCallee(init.tpe).fold(Overrides.Parent.Unread)(parent(level, _))
    }

  private def parent(level: Defn, name: String): Overrides.Parent[Defn] =
    Trees
      .root(leaf)
      .collect {
        case d: (Defn.Class | Defn.Trait)
            if d.name.value == name && ParentNames.denotes(level, d, name) == Denotes.Parent =>
          d
      }
      .headOption match {
      case Some(declared) => Overrides.Parent.Source(declared)
      case None           => Overrides.Parent.Unread
    }

  private def template(level: Defn): Option[Template] = level match {
    case d: (Defn.Class | Defn.Trait) => Some(d.templ)
    case _                            => None
  }

}
