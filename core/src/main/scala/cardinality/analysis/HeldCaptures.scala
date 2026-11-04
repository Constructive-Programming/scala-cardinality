package cardinality.analysis

import scala.meta.*

import cardinality.analysis.capability.ReceiverMembers
import cardinality.analysis.inhabitation.Inhabitation.Binding
import cardinality.analysis.model.{DeclarationIdentity, MemberSyntax, ResolutionContext, ScopeTerms}
import cardinality.analysis.resolution.{
  FinalImplementations,
  InheritedScopes,
  Linearization,
  TopTypes
}
import cardinality.types.TypeApplications

import MethodAnalysis.{Frame, Target}

/** Local captures the measured definition cannot name that a member of a template around it still
  * holds. Concrete code adds no choice only when it computes from values the environment has
  * (D-env-concrete); a capture hidden at the target — by a member, an inherited member or the
  * target's own parameter of its name — is not among them, so a member that holds one keeps it an
  * input. A member that is the capture itself (`def seed: A = a1`, or a parent's field the template
  * can read passed exactly the capture) adds it back, when the type it is read at is the capture's
  * own: reading the member is reading the capture. Read at a type with one value (`seed: Any`, a
  * field typed by a binder the template gives `Any`) it shows nothing (D-top), and adds nothing.
  * Any other reading of it — a field passed `!n`, a parameter no field exposes, a member computing
  * from it, a field at a wider type, a field a nearer level overrides (whose parent still read the
  * argument while it was built) — is refused, never dropped.
  *
  * The members read are those no instance replaces: every member of a fixed template, and the
  * final, private or inline ones of an open one; an open member is a capability of its own.
  */
final private[cardinality] class HeldCaptures(
    target: Target,
    steps: List[InheritedScopes.Step],
    visibleAt: String => Map[String, Binding],
    hidden: (String, Binding) => Boolean,
    resolver: Resolver
) {
  import HeldCaptures.{HeldAt, Holder, Holding}

  /** For the templates among `owners`, and the inherited declarations a final template implements:
    * each hidden capture a member holds as itself, or why one is read otherwise.
    */
  def of(owners: List[Frame], implementations: List[(Frame, Tree)]): List[Either[String, Binding]] =
    if (target.constructor) Nil
    else {
      val implemented = implementations.distinct.filterNot((_, tree) => own(tree))
      implemented.flatMap((template, tree) => member(template, tree, Implemented)) ++
        owners.filter(_.isTemplate).flatMap { template =>
          arguments(template) ++ template.stats
            .filter(stat => derived(template, stat) && !implemented.exists(_._2 eq stat))
            .flatMap(member(template, _, Held))
        } ++ owners.filter(local).flatMap(locals)
    }

  // A block around the target: a method's or a block's, not a template's or a package's, and not
  // the target's own body, which is the choice being counted.
  private def local(owner: Frame): Boolean =
    !owner.isTemplate && !resolver.isPackage(owner) && owner.id != target.frame.id

  // A local definition around the target — a method, a lazy value, a given or a local template,
  // all concrete code D-env-concrete leaves out — holds what it reads as a member does; a template
  // is never exactly a capture. A strict value is a capture of its own, and a definition the
  // target sits in is an owner, read in turn.
  private def locals(owner: Frame): List[Either[String, Binding]] = {
    val context = ResolutionContext(owner, resolver.typeParameters(owner), Set.empty)
    resolver.scopes(owner).flatMap(_.stats).filter(holding).flatMap { stat =>
      val (reads, alias) = stat match {
        case t: (Defn.Object | Defn.Class | Defn.Trait | Defn.Enum) => (templateReads(t), None)
        case other => FinalImplementations.reads(other)
      }
      val holder = alias.map(Holder(_, heldAt(stat, context)))
      held(reads, visibleAt(owner.id), holder, Held)
    }
  }

  // The names a local template reads besides those its body and constructor define, which mean
  // its own members throughout it.
  private def templateReads(t: Defn.Object | Defn.Class | Defn.Trait | Defn.Enum): Set[String] = {
    val own = TypeApplications.template(t).toList.flatMap(_.body.stats)
    val params = t match {
      case c: Defn.Class => c.ctor.paramClauses.toList.flatMap(_.values)
      case d: Defn.Trait => d.ctor.paramClauses.toList.flatMap(_.values)
      case e: Defn.Enum  => e.ctor.paramClauses.toList.flatMap(_.values)
      case _             => Nil
    }
    FinalImplementations.readNames(t).toSet -- ScopeTerms.defined(own) -- params.map(_.name.value)
  }

  private def holding(stat: Stat): Boolean = !own(stat) && !encloses(stat) && (stat match {
    case v: Defn.Val => v.mods.exists(_.is[Mod.Lazy])
    case _: (Defn.Def | Defn.GivenAlias | Defn.Given | Defn.Object | Defn.Class | Defn.Trait |
          Defn.Enum) =>
      true
    case _ => false
  })

  private def encloses(stat: Stat): Boolean =
    stat.pos.start <= target.tree.pos.start && target.tree.pos.end <= stat.pos.end

  private val Implemented = "final implementation reads a hidden capture"

  private val Held = "local capture held by a member that hides it"

  private def own(tree: Tree): Boolean = DeclarationIdentity.same(tree, target.tree)

  private def derived(template: Frame, stat: Stat): Boolean =
    !own(stat) && (resolver.fixedTemplate(template) || ReceiverMembers.Concrete.unoverridable(stat))

  // A member's body reads the captures visible where its template is declared; a member that is
  // one of them holds it at its declared type, or the capture's own when it declares none.
  private def member(template: Frame, tree: Tree, reason: String): List[Either[String, Binding]] = {
    val members =
      FinalImplementations.memberNames(template :: InheritedScopes.levels(steps, template))
    val (reads, alias) = FinalImplementations.reads(tree)
    val context = ResolutionContext(template, resolver.typeParameters(template), Set.empty)
    val holder = alias.map(Holder(_, heldAt(tree, context)))
    held(reads.filterNot(members), visibleAt(template.id), holder, reason)
  }

  // A member holds what it reads at its declared type, or at the capture's own when it has none.
  private def heldAt(tree: Tree, context: ResolutionContext): HeldAt =
    (tree match {
      case v: Defn.Val   => v.decltpe
      case v: Defn.Var   => v.decltpe
      case p: Term.Param => p.decltpe
      case other         => MemberSyntax.result(other)
    }).fold(HeldAt.Inferred)(HeldAt.Declared(_, context))

  // A parent's arguments are read where the template is written, outside its body. An argument that
  // is exactly a capture, passed to a field of a supplied parent the template can read, is held at
  // the field's type as the extends chain gives it.
  private def arguments(template: Frame): List[Either[String, Binding]] = {
    val outside = template.parent.fold(Map.empty[String, Binding])(p => visibleAt(p.id))
    val nearer = new Linearization(template :: InheritedScopes.levels(steps, template), resolver)
    for {
      init <- template.parents
      fields = fieldClauses(template, init, nearer)
      (clause, i) <- init.argClauses.toList.zipWithIndex
      (argument, j) <- clause.values.zipWithIndex
      holder = argument match {
        case Term.Name(name) => fields.lift(i).flatMap(_.lift(j)).flatten.map(Holder(name, _))
        case _               => None
      }
      reading <- held(FinalImplementations.readNames(argument).toSet, outside, holder, Held)
    } yield reading
  }

  // For each constructor clause of the parent an initializer names, the type each parameter holds
  // an argument at, when it is a field the template reads through no override; no clause for a
  // parent outside the sources.
  private def fieldClauses(
      template: Frame,
      init: Init,
      levels: Linearization
  ): List[List[Option[HeldAt]]] =
    for {
      step <- steps
        .find(s => s.root.id == template.id && s.from.id == template.id && (s.parent eq init))
        .toList
      clause <- ReceiverMembers.constructorClauses(step.declared)
    } yield clause.map { p =>
      Option.when(readable(p, step.declared) && !overridden(p.name.value, step.declared, levels))(
        heldAt(p, ResolutionContext(step.declared, step.env, Set.empty))
      )
    }

  // A field the template can read: a member of its level, not private to it.
  private def readable(p: Term.Param, level: Frame): Boolean =
    ReceiverMembers.fieldParams(level).exists(_ eq p) && !p.mods.exists(_.is[Mod.Private])

  // A level nearer the template than the field's own defines the name: the template reads that
  // definition, and the parent's constructor still read the argument it was given.
  private def overridden(name: String, level: Frame, levels: Linearization): Boolean = {
    val nearer = levels.preceding(level.id)
    levels.order.exists(l =>
      nearer(l.id) &&
        (ReceiverMembers.fields(l)(name) || l.stats.exists(MemberSyntax.names(_).contains(name)))
    )
  }

  private def held(
      reads: Set[String],
      visible: Map[String, Binding],
      holder: Option[Holder],
      reason: String
  ): List[Either[String, Binding]] =
    reads.toList.sorted.flatMap { name =>
      visible.get(name).filter(hidden(name, _)).flatMap { binding =>
        holder.filter(_.name == name).map(_.reading(binding, resolver)) match {
          case Some(Holding.Restore(again)) => Some(Right(again))
          case Some(Holding.Drop)           => None
          case _                            => Some(Left(s"$reason: $name"))
        }
      }
    }

}

private[cardinality] object HeldCaptures {

  /** The type a holder is read at: its declared one (in its context), or the capture's own. */
  private enum HeldAt {
    case Declared(tpe: Type, context: ResolutionContext)
    case Inferred
  }

  /** What reading a holder gives: the capture again, nothing (a type with one value), or what this
    * reading refuses.
    */
  private enum Holding {
    case Restore(binding: Binding)
    case Drop
    case Refuse
  }

  /** A member or field that is exactly the capture `name`, read at `at`. */
  final private case class Holder(name: String, at: HeldAt) {

    def reading(binding: Binding, resolver: Resolver): Holding = at match {
      case HeldAt.Inferred               => Holding.Restore(binding)
      case HeldAt.Declared(tpe, context) =>
        resolver.resolve(tpe, context) match {
          case Right(shape) if shape == binding.shape       => Holding.Restore(binding)
          case Right(shape) if TopTypes.terminalGoal(shape) => Holding.Drop
          case _                                            => Holding.Refuse
        }
    }

  }

}
