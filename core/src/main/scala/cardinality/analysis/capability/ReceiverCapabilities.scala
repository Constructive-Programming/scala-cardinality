package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.model.{MemberSlot, MemberSyntax}
import cardinality.analysis.resolution.{InheritedScopes, Linearization, OverrideIdentity}
import cardinality.analysis.{MethodAnalysis, Resolver}

import MethodAnalysis.{Frame, Target}
import ReceiverMembers.Measured

/** What the templates `this` may denote add to one signature's environment (capabilities C5): the
  * open members of each open owner — a trait or class some instance may extend — its parents'
  * overridable fields among them, and the fields of the parameterized parents no class of an owner
  * passes arguments to. A constructor reads only its parameters: no member of its class exists
  * before it runs.
  */
final private[cardinality] class ReceiverCapabilities(
    target: Target,
    resolver: Resolver,
    identity: OverrideIdentity,
    steps: List[InheritedScopes.Step]
) {
  import ReceiverCapabilities.*

  private val tables: List[(Frame, ReceiverMembers)] =
    if (target.constructor) Nil
    else
      // Outermost first, as the measurement binds owners: an inner member shadows an outer one.
      target.frame.chain.reverse
        .filter(owner =>
          owner.tree.exists(t => t.is[Defn.Class] || t.is[Defn.Trait]) &&
            !resolver.fixedTemplate(owner)
        )
        .map(root =>
          root -> new ReceiverMembers(root, steps.filter(_.root.id == root.id), identity, resolver)
        )

  /** Whether an open owner's inherited declaration is implemented by a concrete member or field of
    * its linearization: then that member is classified instead.
    */
  def implemented(stat: Stat, step: InheritedScopes.Step): Either[String, Boolean] =
    tables.find(_._1.id == step.root.id).fold(Right(false)) { (_, table) =>
      table.implemented(stat, step.declared, target.tree)
    }

  /** Each open owner's effective open members and free fields, with the slot each is read in, or
    * why one leaves the environment unread.
    */
  def members: List[Either[String, Member]] =
    tables.flatMap { (root, table) =>
      table.open(measured(root)).map(_.map(c => Member(slot(root, c.level), c.tree, c.name)))
    }

  /** Whether every template the declaration runs in is built (register D26): its own, and each one
    * that template's instances carry — an inner template its outer instance, a local one the
    * receiver of the code declaring it, a member object the instance it belongs to — and, for a
    * top-level definition, the object holding its package's top-level definitions. A template a
    * static object (one nested only in objects and packages) encloses is built without the object.
    * Where one is never built, no call of the declaration returns, so every implementation is the
    * same one. A constructor builds its instance: its row reads its parameters only.
    */
  lazy val construction: Construction.Verdict =
    if (target.constructor) Construction.Verdict.Built
    else {
      val built = new Construction(resolver, steps)
      Construction.Verdict.all(needed(target.frame).map { owner =>
        if (resolver.isPackage(owner) && !owner.isTemplate) built.topLevel(owner, Some(target.tree))
        else {
          val levels = new Linearization(owner :: InheritedScopes.levels(steps, owner), resolver)
          built.of(levels.order, Some(target.tree))
        }
      })
    }

  private def needed(frame: Frame): List[Frame] = frame.parent.toList.flatMap { owner =>
    if (owner.isTemplate) if (frame.isTemplate && static(owner)) Nil else owner :: needed(owner)
    else if (resolver.isPackage(owner)) if (frame.isTemplate) Nil else List(owner)
    else needed(owner)
  }

  private def static(frame: Frame): Boolean =
    frame.chain.forall(f => resolver.isPackage(f) || f.tree.exists(_.is[Defn.Object]))

  private def slot(root: Frame, level: Frame): MemberSlot =
    InheritedScopes
      .at(steps, root, level)
      .fold(MemberSlot(root, resolver.typeParameters(root), root))(step =>
        MemberSlot(step.declared, step.env, step.root)
      )

  // The measured declaration as a receiver sees it: its own member, or a member of a template the
  // receiver encloses, named through its path when every template between is an object.
  private def measured(root: Frame): Option[Measured] =
    for {
      template <- target.frame.parent
      name <- MemberSyntax.names(target.tree).headOption
      between = template.chain.takeWhile(_.id != root.id)
      if between.size < template.chain.size
    } yield Measured(
      target.tree,
      name,
      template,
      Option.when(between.forall(_.tree.exists(_.is[Defn.Object])))(
        between.reverse.flatMap(_.path.lastOption)
      )
    )

}

private[cardinality] object ReceiverCapabilities {

  /** One open member, or one field of inherited state, of a receiver. */
  final case class Member(slot: MemberSlot, tree: Stat | Term.Param, name: String)

}
