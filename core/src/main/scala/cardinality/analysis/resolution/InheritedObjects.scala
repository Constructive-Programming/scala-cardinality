package cardinality.analysis.resolution

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}

/** The member objects a template inherits, which a path's first segment may name (`at`, inside its
  * body): the inherited object's path, or why a parent leaves the name unread.
  */
private[cardinality] trait InheritedObjects {
  def at(head: InheritedObjects.Head): InheritedObjects.Path

  /** The type member a supplied object inherits under `name` that `from` may select, or why it is
    * not read.
    */
  def inheritedBy(module: Frame, name: String, from: Frame): InheritedObjects.Selected = None

  /** The inherited member object a clause over a supplied (package) object imports under a term
    * name, or why it is not read: a value, or a parent outside the sources.
    */
  def inheritedTerm(
      template: Frame,
      name: String,
      givens: Boolean,
      from: Frame
  ): InheritedObjects.Path = None

}

private[cardinality] object InheritedObjects {

  final case class Head(name: String, frame: Frame, file: String, position: Int)

  type Path = Option[Either[String, List[String]]]

  type Selected = Option[Either[String, TypeEntry]]

  val none: InheritedObjects = _ => None
}
