package cardinality.analysis.capability

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.model.MemberSyntax
import cardinality.analysis.resolution.InheritedScopes
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.{TypeApplications, TypeName}

import MethodAnalysis.{sequence, Frame}

/** Whether a template that only code of its own scope may extend — its file, or its enclosing block
  * when it is local — has a member another template of that scope may override.
  *
  * Every subclass is written in the scope, but what it inherits need not be: `class D extends C
  * with T2` overrides C's `m` with T2's when T2, written anywhere, defines `m` — so a subclass's
  * whole supplied linearization counts, not only its body. A template of the scope that defines the
  * name in its body (a pattern binder included) or as a constructor parameter may override it; one
  * that may extend the receiver and mixes in a parent from outside the scope whose supplied
  * linearization defines the name, or that this reading cannot follow, may too.
  */
final private[capability] class FileOverrides(
    root: Frame,
    linearization: List[Frame],
    resolver: Resolver
) {
  private val own = linearization.flatMap(_.tree.flatMap(TypeApplications.template))

  private val local = root.chain.find(_.local)

  private val scope: List[Tree] = local.flatMap(_.tree).fold(root.chain.last.stats)(List(_))

  private val others: List[Template] =
    scope.flatMap(_.collect { case t: Template if !own.contains(t) => t })

  // The names templates of the scope or of the receiver's linearization are written under: a
  // parent so named is checked through its own body, not as an outside mixin.
  private val written = (own ++ others).flatMap(nameOf).toSet

  // The templates of the scope an instance of the receiver may be: those naming it, one of them or
  // an alias of either among their parents.
  private val subclasses: List[Template] = {
    val aliases = scope.flatMap(_.collect { case d: Defn.Type => d })
    @tailrec
    def grow(found: Set[String]): Set[String] = {
      val next = found ++ others.filter(parentNames(_).exists(found)).flatMap(nameOf) ++
        aliases.filter(alias => TypeName.words(alias.body).exists(found)).map(_.name.value)
      if (next == found) found else grow(next)
    }
    val names = grow(root.path.lastOption.toSet)
    others.filter(parentNames(_).exists(names))
  }

  /** Whether some template of the scope may override the member named `name`. */
  def overridden(name: String): Boolean =
    others.exists(defines(_, name)) || subclasses.exists(mixesIn(_, name))

  private def defines(template: Template, name: String): Boolean =
    template.body.stats.exists(stat => MemberSyntax.defined(stat).contains(name)) ||
      template.parent.exists(_.collect { case p: Term.Param if p.name.value == name => p }.nonEmpty)

  // A parent from outside the scope: unknown, or a supplied template whose linearization defines
  // the name.
  private def mixesIn(template: Template, name: String): Boolean = {
    val from = enclosing(template)
    template.inits.filterNot(init => written(simpleName(init))).exists { init =>
      resolver.parent(init, from) match {
        case Right(Some((entry, _))) => entry.self.forall(reaches(_, name))
        case _                       => true
      }
    }
  }

  private def reaches(parent: Frame, name: String): Boolean =
    sequence(
      InheritedScopes.walk(List(parent -> resolver.typeParameters(parent)), resolver, parent)
    ).fold(
      _ => true,
      steps =>
        (parent :: steps.map(_.declared)).exists(level =>
          level.stats.exists(MemberSyntax.defined(_).contains(name)) ||
            level.params.exists(_.name.value == name) ||
            level.parents.exists(resolver.unsuppliedParent(_, level))
        )
    )

  // The frame whose scope a template's parents are named in: the frame holding its definition.
  private def enclosing(template: Template): Frame =
    root.chain
      .find(_.stats.exists(_.collect { case t: Template => t }.contains(template)))
      .orElse(local)
      .getOrElse(root.chain.last)

  private def parentNames(template: Template): List[String] = template.inits.map(simpleName)

  private def simpleName(init: Init): String = InheritedScopes.typeName(init.tpe).split('.').last

  private def nameOf(template: Template): Option[String] =
    template.parent.flatMap(MemberSyntax.templateName)

}
