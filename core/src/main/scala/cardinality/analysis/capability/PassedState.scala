package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.Resolver
import cardinality.analysis.model.MemberSyntax
import cardinality.analysis.resolution.Linearization

/** The constructor state a template passes its parents, where its members may not reach it.
  *
  * An inherited member reading a parent's hidden constructor parameter is concrete code over the
  * argument the template passed, which adds no choice only where the template's own code can write
  * that argument again (D-env-concrete): not when a name the argument reads means something else in
  * the body, nor when another level passes it (docs/research/definition-inventory.md).
  */
private[cardinality] object PassedState {

  /** Why the members of `template` may read a value its environment lacks, if they may. */
  def reasons(template: Frame, levels: List[Frame], resolver: Resolver): List[String] = {
    val linearization = new Linearization(template :: levels, resolver)
    val direct = linearization.parents.getOrElse(template.id, Nil).toSet
    val argumentsTransparent = transparent(template, template :: levels)
    levels.filterNot(_.id == template.id).flatMap { level =>
      val rewritable = direct(level.id) && argumentsTransparent
      hidden(level)
        .filter(name => !rewritable && read(level, name))
        .map(name => s"open class with hidden constructor state: ${level.path.mkString(".")}.$name")
    }
  }

  // The constructor parameters of a level that no public field exposes.
  private def hidden(level: Frame): List[String] = {
    val fields = ReceiverMembers.fieldParams(level)
    level.params
      .filter(p => !fields.exists(_ eq p) || p.mods.exists(_.is[Mod.Private]))
      .map(_.name.value)
      .filter(_.nonEmpty)
  }

  private def read(level: Frame, name: String): Boolean =
    level.stats.exists(_.collect { case Term.Name(`name`) => () }.nonEmpty)

  // Whether every name the template's parent arguments mention means the same value in its body: a
  // constructor parameter of the template does (its members read it as a capture), and any other
  // name unless it is a member of the template or of a level, a field of a level, or under an
  // import of the body.
  private def transparent(template: Frame, levels: List[Frame]): Boolean = {
    val mentioned = template.parents
      .flatMap(_.argClauses.flatMap(_.values))
      .flatMap(_.collect { case Term.Name(name) => name })
      .toSet -- template.params.map(_.name.value)
    val members = levels.flatMap(_.stats.flatMap(MemberSyntax.names)) ++
      levels.filterNot(_.id == template.id).flatMap(ReceiverMembers.fields)
    !template.stats.exists(_.is[Import]) && mentioned.intersect(members.toSet).isEmpty
  }

}
