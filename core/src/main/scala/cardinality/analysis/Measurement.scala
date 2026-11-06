package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.capability.{
  DeferredInline,
  PassedState,
  ReceiverCapabilities,
  ReceiverMembers,
  TargetViews
}
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.model.*
import cardinality.analysis.resolution.*
import cardinality.types.{Traversals, TypeApplications}

import MemberSyntax.Observation
import Inhabitation.{Binding, Count, Shape}
import MethodAnalysis.{Entry, Frame, Resolved, Target}

/** One signature's environment: the bindings visible from its frame, the diagnostics that make that
  * environment incomplete, and the count the two admit.
  */
final private[cardinality] class Measurement(
    target: Target,
    resolver: Resolver,
    mode: TargetViews.ViewMode = TargetViews.ViewMode.Refuse
) {

  private val errors = mutable.ListBuffer.empty[String]
  // The reasons that are possible views of the declaration (capabilities C6), for `ViewSandwich`.
  private val views = mutable.Set.empty[String]
  // Whether an import clause may rebind a builtin name.
  private var rebound = false
  private val values = mutable.LinkedHashMap.empty[String, Binding]
  private val qualifiedValues = mutable.LinkedHashMap.empty[String, Binding]
  private val captures = mutable.ListBuffer.empty[String]
  private val variables = resolver.typeParameters(target.frame)
  private val owners = target.frame.chain.reverse
  private val overrideIdentity = new OverrideIdentity(resolver)
  private val declaredMembers = new DeclaredMembers
  // The values each owner's scan left visible, and the implementations a final template gives its
  // inherited declarations: a local one may read a capture the target hides.
  private val visibleAt = mutable.Map.empty[String, Map[String, Binding]]
  private val implementations = mutable.ListBuffer.empty[(Frame, Tree)]
  // The bindings of open concrete members (capabilities C5): each is its receiver's effective
  // member, never a restatement of a declaration.
  private val concreteMembers = mutable.Set.empty[String]
  // The slot, member name and short key each qualified binding was declared under.
  private val declaredIn = mutable.Map.empty[String, Declared]

  private val positions =
    new ConstructorPositions(target, resolver, variables, (tpe, owner) => resolveIn(tpe, owner))

  private val receivers = new SealedReceivers(target, resolver)

  private val bounded = new BinderBounds(resolver, target.frame)

  private lazy val inferred = InferredResults.declared(target, steps, variables, resolver)

  private val contexts =
    new ContextBinders(target, inferred, steps, readingContext(_, Map.empty), resolver)

  /** Whether possible views (capabilities C6) are all that refused a count of this measurement. */
  def refusedByViews(count: Count): Boolean = count match {
    case Count.Unresolved(reasons) => reasons.nonEmpty && reasons.forall(views)
    case _                         => false
  }

  def entry(): Entry = {
    owners.foreach { owner =>
      scan(owner)
      visibleAt.update(owner.id, values.toMap)
    }
    owners.foreach(selfType)
    scanParents()
    val candidates = new ContextualScope(owners, steps, scopeReader).candidates
    errors ++= ContextualEvidence.unreachable(candidates, resolver.boundedBinders.contains)
    // The target's own binders matter only where they may hide anonymous evidence.
    if (candidates.nonEmpty) contexts.of(target.frame).left.foreach(errors += _)
    // Parameters of the target hide outer names, but their type binders keep their identities.
    bindParams(target.frame)
    errors ++= positions.refusals
    errors ++= resolver.inlineBodies.refusal(target)
    heldCaptures()
    errors ++= capabilities.construction.built.left.toOption
    val result = if (target.constructor) constructorShape() else declaredShape(inferred)
    result.left.foreach(errors += _)
    val shapes = environment.map(_.shape) ++ result.toOption
    val (proofs, refusals) = bounded.of(shapes)
    errors ++= refusals
    errors ++= StuckProjections.refusal(shapes, proofs)
    val bounds = proofs.zipWithIndex.map((proof, i) => Binding(s"<bound#$i>", proof))
    Entry.of(
      target,
      resolver.kindOf(target),
      count(result, inferred, bounds),
      captures.toList.distinct.sorted
    )
  }

  // Every owner's supplied ancestors, read once: a scope's inherited members hide the local
  // captures of its name before its own members are bound, and its declarations become slots.
  private lazy val walked: List[Either[String, InheritedScopes.Step]] =
    InheritedScopes.walk(
      owners.map(owner => owner -> resolver.typeParameters(owner)),
      resolver,
      target.frame
    )

  private lazy val capabilities =
    new ReceiverCapabilities(
      target,
      resolver,
      overrideIdentity,
      walked.flatMap(_.toOption),
      mode
    )

  // A declaration inherited from a parent the source set defines is a slot as well: the caller
  // fills it through the subclass. Declarations travel, and so do the open concrete members the
  // receiver tables classify — a parent's overridable fields and an unpassed parent's fields among
  // them (`ReceiverCapabilities`, capabilities C5); derived (final, private, inline, given) members
  // do not — they are concrete code over the environment the receiver already shares. An external
  // parent of an open owner is reported, not guessed at (`ForeignParents`). Each level is read
  // under the arguments the whole `extends` chain gives it, so a grandparent's `seed: X` is this
  // scope's type, not an intermediate level's binder. Each owner is its own receiver: a parent two
  // of them inherit gives each its own members, and each owner then judges the closed declarations
  // bound as its members (`InheritedClosedSlots`).
  private lazy val steps: List[InheritedScopes.Step] = walked.flatMap(_.toOption)

  private def scanParents(): Unit = {
    walked.foreach(_.left.foreach(errors += _))
    val ancestors = steps.groupMap(_.root.id)(_.declared).withDefaultValue(Nil)
    steps.foreach(step =>
      inherited(step, step.root :: ancestors(step.root.id), steps, capabilities)
    )
    capabilities.members.foreach {
      case Left(refusal) =>
        errors += refusal.reason
        if (refusal.view) views += refusal.reason
      case Right(member) =>
        val slot = member.slot.copy(concrete = true)
        member.tree match {
          case p: Term.Param => add(member.name, p.decltpe, slot, slot.scope)
          case stat: Stat    => addMember(slot, stat, member.name)
        }
    }
    owners.foreach { root =>
      judge(root)
      // A constructor chooses only its parameters: no owner's parents add a choice to it.
      if (!target.constructor) {
        errors ++= ForeignParents.reasons(root :: ancestors(root.id), resolver)
        if (root.isTemplate) errors ++= PassedState.reasons(root, ancestors(root.id), resolver)
      }
    }
  }

  // One template's members in the order they were bound, its own declarations first. A closed one
  // the template implements or restates is not a choice: dropped, or refused.
  private def judge(root: Frame): Unit = {
    val slots = new InheritedClosedSlots(root)
    qualifiedValues.toList.foreach { (id, binding) =>
      declaredIn
        .get(id)
        .filter(d => d.slot.template.id == root.id && !concreteMembers(id))
        .foreach { declared =>
          slots.verdict(declared.slot.scope, declared.member, declared.key, binding) match {
            case InheritedClosedSlots.Keep           => ()
            case InheritedClosedSlots.Refuse(reason) => errors += reason
            case InheritedClosedSlots.Drop           =>
              val dropped = qualifiedValues.remove(id)
              if (dropped.exists(values.get(declared.key).contains)) values -= declared.key
          }
        }
    }
  }

  // What an inherited frame contributes: its declarations, read in the scope that named it, except
  // those a final root implements, in its own body or at another level of its linearization, and
  // those an open root's concrete members or fields implement (that member is classified instead,
  // capabilities C5).
  //
  // Members gate G1: a final root's inherited declaration that another level of its linearization
  // implements is that level's concrete code — no subclass replaces it — never a free capability:
  // concrete code computes from the environment it is given, so it is dropped as the root's own
  // implementations are. One a local template implements may read a capture the target hides (no
  // prefix names it), so there it is refused.
  private def inherited(
      step: InheritedScopes.Step,
      linearization: List[Frame],
      steps: List[InheritedScopes.Step],
      capabilities: ReceiverCapabilities
  ): Unit = {
    val implementedOpen =
      step.declared.stats.map(stat => stat -> capabilities.implemented(stat, step))
    errors ++= implementedOpen.collect { case (_, Left(reason)) => reason }
    implementedOpen.collect { case (stat, Right(false)) => stat }.foreach { stat =>
      val implemented = FinalImplementations.implementation(step.root, stat)
      implemented.foreach((_, tree) => implementations += step.root -> tree)
      val slot = MemberSlot(step.declared, step.env, step.root)
      val implementer =
        if (implemented.nonEmpty) None
        else FinalImplementations.implementer(step.root, linearization, stat)
      (stat, implementer) match {
        case (_, Some(level))     => errors ++= FinalImplementations.localImplementer(level, stat)
        case (value: Decl.Val, _) =>
          withOwn(step, value)(own =>
            addDeclaredValue(slot, value, implemented.map(_._1).toSet ++ own)
          )
        case _ if implemented.nonEmpty => ()
        case (method: Decl.Def, _)     => inheritedCallable(step, slot, method, steps)
        case (other, _) => withOwn(step, other)(own => if (own.isEmpty) declaration(slot, other))
      }
    }
  }

  // The names of an inherited declaration the measured definition may be itself — binding one would
  // let it delegate to itself — with the reasons naming those whose identity is not known.
  private def withOwn(step: InheritedScopes.Step, stat: Stat)(use: Set[String] => Unit): Unit = {
    val (own, unresolved) = OverrideIdentity.own(target, step, stat)
    errors ++= unresolved
    use(own)
  }

  private def inheritedCallable(
      step: InheritedScopes.Step,
      slot: MemberSlot,
      method: Decl.Def,
      steps: List[InheritedScopes.Step]
  ): Unit =
    overrideIdentity.matches(target, method, step, steps) match {
      case Some(true)  => ()
      case Some(false) => addMember(slot, method, method.name.value)
      case None => errors += s"inherited override identity not resolved: ${method.name.value}"
    }

  // A capture the target cannot name that a member around it holds stays an input, or refuses the
  // count (`HeldCaptures`).
  private def heldCaptures(): Unit =
    new HeldCaptures(target, steps, visibleAt.getOrElse(_, Map.empty), hidden, resolver)
      .of(owners, implementations.toList)
      .foreach {
        case Left(reason)   => errors += reason
        case Right(binding) => qualifiedValues.update(binding.id, binding)
      }

  private def hidden(name: String, binding: Binding): Boolean =
    !qualifiedValues.contains(binding.id) && !values.get(name).contains(binding)

  private def resolveIn(
      tpe: Type,
      owner: Frame,
      env: Map[String, Resolved] = Map.empty,
      receiver: Option[Frame] = None
  ): Resolved = resolver.resolve(tpe, readingContext(owner, env, receiver))

  // A parent's declaration is a member of the template that inherits it: its type members are
  // that receiver's (members M2), so `to: S => F[X, A]` reads the receiver's `X`.
  private def readingContext(
      owner: Frame,
      env: Map[String, Resolved],
      receiver: Option[Frame] = None
  ): ResolutionContext = {
    val bindings = if (env.isEmpty) resolver.typeParameters(owner) else env
    val context = ResolutionContext(owner, bindings, Set.empty, Some(target.frame))
    receiver.fold(context)(r => context.as(MemberPath.self(r, resolver.typeParameters(r))))
  }

  // The names in `own` are the measured definition's: of a declaration of several names (`val a,
  // b: A`) the others are slots of its environment.
  private def declaration(slot: MemberSlot, stat: Stat, own: Set[String] = Set.empty): Unit =
    stat match {
      case v: Decl.Val       => addDeclaredValue(slot, v, own)
      case v: Decl.Var       => addDeclaredValue(slot, Decl.Val(v.mods, v.pats, v.decltpe), own)
      case d: Decl.Def       => addMember(slot, d, d.name.value)
      case d: Decl.GivenLike => addDeclaredGiven(slot, d)
      case _                 => ()
    }

  // One owner's bindings: its own parameters, then every scope that shares its path.
  private def scan(owner: Frame): Unit = {
    errors ++= owner.refusals
    val outer = values.keySet.toSet -- owner.params.map(_.name.value)
    receiverSpace(owner)
    if (owner.id != target.frame.id) bindContext(owner)
    frames(owner).foreach(scope => hide(scope.stats))
    hideInherited(owner, outer)
    hideImported(owner, outer)
    frames(owner).foreach(scanFrame)
  }

  // A name the scope defines hides an outer binding of it everywhere inside the scope. One a
  // qualified path reaches (`Outer.this.seed`) stays in the environment; a local owner's has no such
  // path and leaves it — or, behind an export clause this reading does not enumerate, is unknown.
  private def hide(stats: List[Stat]): Unit = {
    ScopeTerms.defined(stats).foreach(values -= _)
    ScopeTerms.exported(stats) match {
      case Some(names) =>
        local(names).foreach(name => errors += s"local capture hidden by an export: $name")
      case None =>
        local(values.keySet.toSet).headOption.foreach(name =>
          errors += s"local capture may be hidden by an export: $name"
        )
    }
  }

  // The names among `names` whose binding is a local capture, which no qualified path reaches.
  private def local(names: Set[String]): List[String] =
    names.toList.sorted.filter(name =>
      values.get(name).exists(binding => !qualifiedValues.contains(binding.id))
    )

  // A member a template inherits under a local capture's name makes the simple name ambiguous
  // there (E049): the capture is out of reach. One a parent this reading does not enumerate may
  // declare is unknown.
  private def hideInherited(owner: Frame, outer: Set[String]): Unit =
    if (owner.isTemplate) {
      val levels = InheritedScopes.levels(steps, owner)
      local(outer).foreach { name =>
        InheritedTerms.inherits(owner, levels, name, resolver).foreach { parent =>
          if (parent.certain) values -= name
          else errors += s"local capture may be hidden by an inherited member: $name"
        }
      }
    }

  // An import of an inner scope binding a local capture's name makes the simple name ambiguous
  // there as well; one whose selectors this reading does not enumerate may bind any name.
  private def hideImported(owner: Frame, outer: Set[String]): Unit = {
    val clauses =
      owner.stats.filter(stat => stat.is[Import] && stat.pos.start < target.tree.pos.start)
    val bound = ScopeTerms.imported(clauses).fold(local(outer))(names => local(outer & names))
    bound.headOption.foreach(name => errors += s"local capture may be hidden by an import: $name")
  }

  // One owner's parameters as bindings: the positions the estimate did not read, since a space it
  // read *is* the count and binding the position would only add a diagnostic for it. An anonymous
  // contextual parameter binds under its owner and position, since every one of them has the
  // empty name. A `var` parameter is a parameter: the model excludes mutation, so the field holds
  // the value the caller supplied.
  private def bindParams(owner: Frame): Unit =
    owner.params.zipWithIndex.foreach { (p, index) =>
      if (positions.leftToShape(p, owner))
        add(
          if (ContextualEvidence.anonymous(p)) ContextualEvidence.key(owner.path, index)
          else p.name.value,
          p.decltpe,
          MemberSlot(owner, resolver.typeParameters(owner), owner),
          ParameterScope.of(owner),
          builtBefore(p, owner)
        )
    }

  // Whether a parameter holds a value built before the measured declaration's receiver, so never
  // that receiver (capabilities C6): one of the receiver template's constructor or of a frame
  // around it, evaluated once before the receiver is built — not by name or inline (re-evaluated
  // where it is read), and no field a subclass may override with the receiver. Scala rejects a
  // self reference passed to a superclass constructor by value, so even the receiver template's
  // own constructor parameter is not it. The member's own parameters may be.
  private def builtBefore(p: Term.Param, owner: Frame): Boolean =
    receiverChain(owner.id) &&
      !p.decltpe.exists(_.is[Type.ByName]) &&
      !p.mods.exists(_.is[Mod.Inline]) &&
      !(owner.isTemplate && ReceiverMembers.fieldParams(owner).exists(_ eq p))

  // A type spelled as a class applied or not: no other part of it starts where it does, but an
  // infix, union, function or annotated type starts with an operand.
  private def named(tpe: Type): Boolean =
    tpe.pos.start >= 0 && (tpe match {
      case _: (Type.Name | Type.Select | Type.Apply) => true
      case _                                         => false
    })

  // The measured member's receiver template and the frames around it.
  private lazy val receiverChain: Set[String] =
    target.frame.parent.filter(_.isTemplate).fold(Set.empty[String])(_.chain.map(_.id).toSet)

  // An enclosing receiver's fields, as its public constructors build them (`ConstrainedReceivers`):
  // closed fields holding fewer values together than their types are one value of that space.
  private def receiverSpace(owner: Frame): Unit =
    if (owner.id == target.frame.id) bindParams(owner)
    else
      ConstrainedReceivers.space(owner, target.frame, resolver, resolveIn(_, owner)) match {
        case Left(reason)       => errors += reason
        case Right(None)        => bindParams(owner)
        case Right(Some(space)) =>
          val name = s"<receiver ${owner.path.mkString(".")}>"
          bindSlot(
            MemberSlot(owner, resolver.typeParameters(owner), owner),
            name,
            Shape.Ground(name, space)
          )
      }

  // The binders an enclosing definition's context-function result puts around its body are values
  // of everything nested in it, read where its result is; implicit search alone reaches them, so
  // the contextual scope refuses one a deeper candidate may shadow.
  private def bindContext(owner: Frame): Unit =
    contexts.of(owner) match {
      case Left(reason)   => errors += reason
      case Right(binders) =>
        binders.foreach { binder =>
          binder.shape match {
            case Left(reason) => errors += reason
            case Right(shape) =>
              bindSlot(MemberSlot(owner, resolver.typeParameters(owner), owner), binder.key, shape)
          }
        }
    }

  // A self type adds another type's members to `this` without inheriting them. This reading does
  // not enumerate them, so the environment is incomplete rather than smaller. A constructor's own
  // template is exempt: the instance whose `this` it would read does not exist yet.
  private def selfType(owner: Frame): Unit =
    if (!(target.constructor && owner.id == target.frame.id))
      TypeMembers.selfType(owner).foreach(errors += _)

  // Package-level declarations are indexed across all files. Unrelated class parameters are never
  // pooled just because both happen to be named A.
  private def frames(owner: Frame): List[Frame] = resolver.scopes(owner)

  private def scanFrame(scope: Frame): Unit = {
    resolver.inspect("scope")
    val visible = scope.stats.filter(visibleIn(scope, _))
    new CaptureAliases(visible, values.get, resolveIn(_, scope), resolver).bindings.foreach(
      (name, binding) => bind(name, binding, scope)
    )
    visible.foreach(flagStat(scope, _))
  }

  private def visibleIn(scope: Frame, stat: Stat): Boolean =
    (!scope.local || stat.pos.start < target.tree.pos.start) &&
      resolver.accessible(stat, scope, target.frame)

  private def scopeReader = ContextualScope.Reader(
    frames,
    visibleIn,
    resolveIn(_, _, _).toOption,
    (tpe, owner, env, shape) =>
      ContextFunctions.results(tpe, shape, readingContext(owner, env), resolver),
    identify,
    positions.leftToShape,
    resolver.unsuppliedParent,
    contexts.of(_).toOption.toList.flatten
  )

  private def identify(tpe: Type, owner: Frame, env: Map[String, Resolved]) =
    TypeIdentities.nominal(tpe, readingContext(owner, env), resolver)

  private def bind(name: String, binding: Binding, owner: Frame): Unit = {
    values.update(name, binding)
    if (owner.id != target.frame.id) {
      captures += name
      // Shadowing the short name does not remove this.x / Outer.this.x / package.x.
      if (!owner.local) qualifiedValues.update(binding.id, binding)
    }
  }

  /** A declaration bound in a slot: the member's name, and the key `values` holds it under (an
    * overload's or an anonymous parameter's own).
    */
  private case class Declared(slot: MemberSlot, member: String, key: String)

  // A member bound under its own name.
  private def bindSlot(slot: MemberSlot, member: String, shape: Shape): Unit =
    bindSlot(slot, member, member, shape)

  private def bindSlot(slot: MemberSlot, member: String, key: String, shape: Shape): Unit = {
    val binding = Binding(slot.id(key), shape, Some(member))
    bind(key, binding, slot.scope)
    if (slot.concrete) concreteMembers += binding.id
    if (qualifiedValues.contains(binding.id))
      declaredIn.update(binding.id, Declared(slot, member, key))
  }

  // A declaration is read where its slot declares it; a parameter, where its owner types it. A
  // stable value's type-only refinement pins members of its path, which its path's reading follows:
  // the value itself is a value of the refined type.
  private def add(
      name: String,
      tpe: Option[Type],
      slot: MemberSlot,
      reading: Frame,
      preceding: Boolean = false
  ): Unit =
    readValue(name, tpe, slot, reading, preceding) match {
      case Left(reason) => errors += reason
      case Right(shape) => bindSlot(slot, name, shape)
    }

  private def readValue(
      name: String,
      tpe: Option[Type],
      slot: MemberSlot,
      reading: Frame,
      preceding: Boolean = false
  ): Resolved =
    tpe
      .toRight(s"missing type of accessible value: $name")
      .flatMap(resolver.stableInput(_, readingContext(reading, slot.env, slot.receiver)))
      .flatMap { tpe =>
        val context = readingContext(reading, slot.env, slot.receiver)
        resolver.resolve(
          tpe,
          if (preceding && named(tpe))
            context.copy(preceding = Some(reading.input -> tpe.pos.start))
          else context
        )
      }

  // A scope's declarations by kind: a declaration binds a slot, a concrete value or variable is
  // read through `Aliases`, and an import only matters where it may rebind a builtin name. The
  // model excludes mutation, so a `var` holds the value it was initialized with, as a `val` does,
  // and an abstract `var` is an abstract getter (its setter returns `Unit`): a declared slot.
  private def flagStat(scope: Frame, stat: Stat): Unit = stat match {
    case d: (Decl.Val | Decl.Var | Decl.Def | Decl.GivenLike) =>
      val slot = MemberSlot(scope, resolver.typeParameters(scope), scope)
      if (!isTarget(d)) declaration(slot, d)
      else if (d.is[Decl.Val] || d.is[Decl.Var])
        declaration(slot, d, target.frame.path.lastOption.toSet)
    case i: Import => flagShadowing(i)
    case _         => ()
  }

  // The signature being measured is not one of its own slots: binding it would let a declaration
  // delegate to itself and claim an implementation it does not have.
  private def isTarget(stat: Stat): Boolean = DeclarationIdentity.same(stat, target.tree)

  private def addDeclaredValue(
      slot: MemberSlot,
      declaration: Decl.Val,
      implemented: Set[String]
  ): Unit =
    declaration.pats.foreach { pat =>
      TypeApplications.patVarName.getOption(pat) match {
        case Some(name) if implemented(name) => ()
        case Some(name)
            if MemberSyntax.deferredInline(declaration) &&
              !DeferredInline.supplied(Some(target.tree)) =>
          val shape = readValue(name, Some(declaration.decltpe), slot, slot.scope)
          errors ++= DeferredInline.read(shape, slot.member(name), resolver)
        case Some(name) => add(name, Some(declaration.decltpe), slot, slot.scope)
        case None       => errors += "destructured capture not resolved"
      }
    }

  // A declaration (or an open member) is not an implementation: someone else supplies it, so its
  // result is a value this scope has, and a callable one is a callable it can apply. Its *type* is
  // what the count works with: `def modify(f: A => B): S => T` applies like any other callable, and
  // `def get[M](): M` is read through its own binder (`PolymorphicMembers`, capabilities C8). A
  // member that only ever builds Unit is irrelevant (capabilities C7, the exact case).
  private def addMember(slot: MemberSlot, member: Stat, name: String): Unit =
    MemberSyntax.observation(
      member,
      name,
      slot.member(name),
      (id, kind) => resolver.operators.rigid(id, kind)
    ) match {
      case Left(reason)                                 => errors += reason
      case Right(Observation.Value(tpe))                => add(name, Some(tpe), slot, slot.scope)
      case Right(Observation.Applied(callable, result)) =>
        val read = resolveIn(_, slot.scope, slot.env ++ callable.binders, slot.receiver)
        MemberSyntax.applied(callable, result, read) match {
          case Right(None)  => ()
          case Left(reason) => errors += reason
          case Right(Some(shape))
              if MemberSyntax.deferredInline(member) &&
                !DeferredInline.supplied(Some(target.tree)) =>
            errors ++= DeferredInline.refusal(shape, slot.member(name), resolver)
          case Right(Some(shape)) => bindMember(slot, name, member, shape)
        }
    }

  // One key per member: an overload is a second key, a restatement none. Its parameter types'
  // identities tell the two apart.
  private def bindMember(slot: MemberSlot, name: String, declaration: Stat, shape: Shape): Unit = {
    val context = readingContext(slot.scope, slot.env)
    val identities = MemberSyntax
      .parameters(MemberSyntax.groups(declaration))
      .flatMap(_.decltpe)
      .map(TypeIdentities.of(_, context, resolver))
    val member = DeclaredMembers.Declared(slot.template.id, slot.scope.id, shape, identities)
    declaredMembers.key(name, declaration, member) match {
      case Left(reason)     => errors += reason
      case Right(None)      => ()
      case Right(Some(key)) => bindSlot(slot, name, key, shape)
    }
  }

  private def addDeclaredGiven(slot: MemberSlot, declaration: Decl.GivenLike): Unit =
    declaration.name match {
      case name: Term.Name => add(name.value, Some(declaration.decltpe), slot, slot.scope)
      case _               => errors += "anonymous given not resolved"
    }

  private def flagShadowing(clause: Import): Unit =
    ImportShadowing.reason(clause).foreach { reason =>
      rebound = true
      errors += reason
    }

  // The positions left to the shape model: a type parameter's slot, whose count is the values the
  // caller supplies — the reading behind `Pair[A](x: A, y: A)` having four implementations — and
  // any position the estimate reads with a reason, where the model's diagnostic says more than a
  // number resting on an unknown name.
  private def constructorShape(): Resolved =
    Traversals
      .sequence(
        target.frame.params
          .filter(positions.leftToShape(_, target.frame))
          .map(p =>
            p.decltpe
              .toRight(s"missing parameter type: ${p.name.value}")
              .flatMap(resolver.resolve(_, target.frame, variables))
          )
      )
      .map(Shape.Product(_))

  // The result type, declared or inferred, read in its context.
  private def declaredShape(inferred: Either[String, InferredResults.Inferred]): Resolved =
    inferred.flatMap { result =>
      resolver.resolve(result.tpe, result.context).flatMap(shape => observed(result).toLeft(shape))
    }

  // A transparent result is what the call site observes only when nothing can narrow it (rule X).
  private def observed(result: InferredResults.Inferred): Option[String] =
    Option
      .when(TransparentResults.transparent(target))(result.tpe)
      .flatMap(TransparentResults.narrowed)

  // A goal with one value has one implementation whatever the environment holds, so nothing left
  // unresolved around it can change the count — unless a channel around the result reaches
  // declarations the sources do not supply, which may give a name the result spells another
  // meaning. A builtin spelling (`Unit`, `scala.Any`) is never rebound that way (the never-shadowed
  // assumption): only a declaration the builtin reading reaches shadows it, and it refused above.
  // A type parameter clause that is no Scala program makes the signature none either: it has no
  // implementation to count.
  private def count(
      result: Resolved,
      inferred: Either[String, InferredResults.Inferred],
      bounds: List[Binding]
  ): Count = result match {
    // No receiver the declaration runs in is ever built (register D26): it is never called.
    case _ if capabilities.neverCalled => Count.Finite(1)
    case Right(shape)
        if !target.constructor && !rebound && bounded.invalid.isEmpty &&
          TopTypes.terminalGoal(TopTypes.polarized(shape, true)) =>
      inferred.toOption
        .filterNot(r => BuiltinTypes.canonical(r.tpe.syntax).nonEmpty)
        .flatMap(r => resolver.unreadChannel(r.context.referencing(r.tpe))) match {
        case None         => Count.Finite(1)
        case Some(reason) => measured(result, bounds, List(reason))
      }
    case _ => measured(result, bounds, Nil)
  }

  // The ambient proofs of the bounded binders the environment mentions join it for the solver.
  private def measured(result: Resolved, bounds: List[Binding], doubts: List[String]): Count =
    positions.declining(
      if (errors.nonEmpty) Count.Unresolved((errors.toList ++ doubts).distinct.sorted)
      else
        result.fold(
          unresolved,
          shape => {
            val (exact, estimated) = positions.factors
            receivers.certify(shape, Count.product(exact, estimated, inhabitation(shape, bounds)))
          }
        )
    )

  private def unresolved(reason: String): Count = Count.Unresolved(List(reason))

  // The values the body can read, without the ambient proofs of their binders' bounds.
  private def environment: List[Binding] = (values.values ++ qualifiedValues.values).toList

  private def inhabitation(shape: Shape, bounds: List[Binding]): Count =
    Inhabitation.count(
      environment ++ bounds,
      shape,
      resolver.limits.maxStates,
      resolver.inspect,
      resolver.solverDepth
    )

}
