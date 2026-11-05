package cardinality.analysis.capability

import scala.meta.{Template, Term, Tree, XtensionCollectionLikeUI}

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.model.MemberSyntax
import cardinality.types.TypeApplications

import MethodAnalysis.Frame

/** Whether a template that only code of its own scope may extend — its file, or its enclosing block
  * when it is local — has a member another template of that scope may override: one that defines
  * the name in its body (a pattern binder included) or as a constructor parameter.
  */
final private[capability] class FileOverrides(root: Frame, linearization: List[Frame]) {
  private val own = linearization.flatMap(_.tree.flatMap(TypeApplications.template))

  private val local = root.chain.find(_.local)

  private val scope: List[Tree] = local.flatMap(_.tree).fold(root.chain.last.stats)(List(_))

  private val others: List[Template] =
    scope.flatMap(_.collect { case t: Template if !own.contains(t) => t })

  /** Whether some template of the scope may override the member named `name`. */
  def overridden(name: String): Boolean = others.exists(defines(_, name))

  private def defines(template: Template, name: String): Boolean =
    template.body.stats.exists(stat => MemberSyntax.defined(stat).contains(name)) ||
      template.parent.exists(_.collect { case p: Term.Param if p.name.value == name => p }.nonEmpty)

}
