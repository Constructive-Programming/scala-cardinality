package cardinality.analysis

import scala.meta.*

import cardinality.analysis.resolution.InheritedTypeMembers

import MethodAnalysis.Frame

/** Parents outside the source set, as members of an open template's environment.
  *
  * A parent outside the sources may declare abstract members an open template (a trait, an abstract
  * or non-final class) leaves to whoever instantiates it, and concrete ones a subclass may
  * override: capabilities this environment cannot list. A receiver no subclass replaces (an object,
  * a given, a final class, a sealed template whose subtypes are all such) has implemented them, and
  * concrete code cannot change a count whose inputs and result are read. The root and marker types
  * declare no member a count reads; the stdlib model knows the members of the parents it summarizes
  * — where the root imports bind the parent's name.
  */
private[cardinality] object ForeignParents {

  /** Why the parents of an open template and of its supplied ancestors leave its environment
    * incomplete: the template first, its ancestors after.
    */
  def reasons(templates: List[Frame], resolver: Resolver): List[String] =
    if (!templates.headOption.exists(open(_, resolver))) Nil
    else {
      val defined = templates.flatMap(InheritedClosedSlots.concrete).toSet
      templates
        .flatMap(frame => frame.parents.flatMap(resolver.outsideParent(_, frame)))
        .flatMap(parentReasons(_, defined))
        .distinct
    }

  // A parent the root imports may not bind is absent; one the model summarizes leaves the members
  // it lists; any other entry, or a summary without term members, says why they are unread.
  private def parentReasons(
      parent: InheritedTypeMembers.Outside,
      defined: Set[String]
  ): List[String] =
    (parent.shadow, StdlibModel.parent(parent.path)) match {
      case (Some(route), _) => List(absent(parent.path, s"may be bound $route"))
      case (None, Some(summary)) if summary.listed =>
        unread(StdlibModel.canonical(parent.path).getOrElse(parent.path), summary, defined)
      case (None, _) =>
        List(
          StdlibModel
            .entry(parent.path)
            .fold(absent(parent.path, parent.binding))(StdlibModel.parentReason)
        )
    }

  private def absent(path: String, binding: String): String =
    s"inherited parent declaration not supplied: $path ($binding)"

  // A summarized parent's members the sources leave to a subclass: abstract ones they do not
  // implement, concrete ones they do not override (whose stdlib bodies the model does not read).
  private def unread(
      parent: String,
      summary: StdlibModel.Summary,
      defined: Set[String]
  ): List[String] =
    (summary.abstracts -- defined).toList.sorted.map(member =>
      s"inherited parent declaration not implemented: $parent.$member"
    ) ++ (summary.overridable -- defined).toList.sorted.map(member =>
      s"inherited stdlib member a subclass may override: $parent.$member"
    )

  // A trait or class whose instances may be of a subclass outside the sources.
  private def open(frame: Frame, resolver: Resolver): Boolean =
    frame.tree.exists(t => t.is[Defn.Trait] || t.is[Defn.Class]) && !resolver.fixedTemplate(frame)

}
