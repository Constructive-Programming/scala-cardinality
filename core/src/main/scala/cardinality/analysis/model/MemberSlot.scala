package cardinality.analysis.model

import cardinality.analysis.MethodAnalysis.{Frame, Resolved}

/** Where a declaration is read: its frame, its types' environment and the template whose member it
  * is (`Outer.this.seed` and `Inner.this.seed` are two slots). `concrete` marks a receiver's
  * effective open member (capabilities C5), never a restatement of a declaration.
  */
final private[cardinality] case class MemberSlot(
    scope: Frame,
    env: Map[String, Resolved],
    template: Frame,
    concrete: Boolean = false
) {

  def id(name: String): String =
    if (template.id == scope.id) s"${scope.id}:$name" else s"${template.id}<${scope.id}:$name"

  def receiver: Option[Frame] = Option.when(template.id != scope.id)(template)

}
