package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.*
import cardinality.analysis.resolution.*
import cardinality.capacity.Reading
import cardinality.types.{TypeApplications, TypeName}

import ContextualEvidence.Nominal
import Inhabitation.{Binding, Count, Shape}
import MethodAnalysis.{sequence, Entry, Frame, Limits, Resolved, Target, TypeEntry}

/** The narrow view of the index the measurement needs: resolve a name to the types that declare it,
  * resolve a type to its shape, the frames a name may be read in, and the package and module lists
  * an outside member could come from. The measurement holds this rather than the whole index, so
  * the two files stay apart without a second copy of the resolution rules.
  */
private[cardinality] trait Resolver {

  /** The estimate's reading of the source a signature was indexed from, so a constructor position
    * can answer with the same space the definitions' rows use.
    */
  def reading(path: String): Reading

  def lookup(name: String, frame: Frame): List[TypeEntry]

  /** The declaration a simple or dotted name means at the context's reference, imports included. */
  def resolveName(name: String, context: ResolutionContext): Found

  /** Whether a simple name, read at `tpe`'s position, means a member its template inherits. */
  def inheritsName(tpe: Type, name: String, context: ResolutionContext): Boolean

  /** The supplied declaration an `extends` clause names, with its declaring scope's environment;
    * None for a parent outside the source set.
    */
  def parent(init: Init, from: Frame): Either[String, Option[(TypeEntry, Map[String, Resolved])]]

  /** Whether a parent clause names a type whose members this reading does not have. */
  def unsuppliedParent(init: Init, from: Frame): Boolean

  /** A parent clause outside the source set: its path, how its name was bound, and whether the
    * stdlib model's declaration of that path is the one it names.
    */
  def outsideParent(init: Init, from: Frame): Option[InheritedTypeMembers.Outside]

  /** Whether every instance of a template is of a supplied declaration no subclass extends. */
  def fixedTemplate(frame: Frame): Boolean

  /** The declaration a type name means at `tpe`'s position, an inherited member first. */
  def declaration(tpe: Type, name: String, context: ResolutionContext): Found

  /** A channel around the reference through which declarations the sources do not supply may bind a
    * builtin name, if one is open there.
    */
  def unreadChannel(context: ResolutionContext): Option[String]

  /** The channel through which another declaration may bind a name the stdlib model reads, where
    * the name is read, if one is open there.
    */
  def rootShadow(name: String, context: ResolutionContext): Option[String]

  /** Why the call site of a `transparent inline` method may observe more than its declared result
    * `tpe`, if it may.
    */
  def narrowedResult(tpe: Type, context: ResolutionContext): Option[String]

  def resolve(
      tpe: Type,
      frame: Frame,
      variables: Map[String, Resolved],
      visiting: Set[String] = Set.empty
  ): Resolved

  def resolve(tpe: Type, context: ResolutionContext): Resolved

  def typeParameters(frame: Frame): Map[String, Resolved]

  /** The operators higher-kinded names are bound to. */
  def operators: TypeOperators

  /** Every proper binder that carries a subtype bound, by its atom, with its declaring frame. */
  def boundedBinders: Map[String, (Frame, Type.Param)]

  def accessible(tree: Tree, owner: Frame, from: Frame): Boolean

  def packages: List[Frame]

  def modules: List[Frame]

  def mentionedConcrete(target: Target): Set[String]

  def kindOf(target: Target): String

  def limits: Limits

  def inspect(operation: String): Unit
  def expanding[A](operation: String)(read: => A): A
  def solverDepth: Int
  def peers(path: List[String]): List[Frame]
  def isPackage(frame: Frame): Boolean
}

/** One signature's environment: the bindings visible from its frame, the diagnostics that make that
  * environment incomplete, and the count the two admit.
  */
final private[cardinality] class Measurement(target: Target, resolver: Resolver) {

  private val errors = mutable.ListBuffer.empty[String]
  // Whether an import or export clause may rebind a builtin name.
  private var rebound = false
  private val values = mutable.LinkedHashMap.empty[String, Binding]
  private val qualifiedValues = mutable.LinkedHashMap.empty[String, Binding]
  private val captures = mutable.ListBuffer.empty[String]
  private val variables = resolver.typeParameters(target.frame)
  private val owners = target.frame.chain.reverse
  private val overrideIdentity = new OverrideIdentity(target, resolver)
  private val declaredMembers = new DeclaredMembers
  // The values each owner's scan left visible, and the implementations a final template gives its
  // inherited declarations: a local one may read a capture the target hides.
  private val visibleAt = mutable.Map.empty[String, Map[String, Binding]]
  private val implementations = mutable.ListBuffer.empty[(Frame, Tree)]
  // The slot, member name and short key each qualified binding was declared under.
  private val declaredIn = mutable.Map.empty[String, Declared]
  // Members of a module outside the lexical chain can never hold or produce this method's
  // type parameters — a module's scope is fixed, and no method's binders reach it. They can
  // only matter where a signature mentions a concrete, modelled type (a Boolean, an Option),
  // so those modules, imports and sibling bodies are the ones to report rather than every
  // companion object and every import in the file.
  private val mentions = resolver.mentionedConcrete(target)

  private val positions =
    new ConstructorPositions(target, resolver, variables, (tpe, owner) => resolveIn(tpe, owner))

  private val receivers = new SealedReceivers(target, resolver)

  private val bounded = new BinderBounds(resolver, target.frame)

  def entry(): Entry = {
    owners.foreach { owner =>
      scan(owner)
      visibleAt.update(owner.id, values.toMap)
    }
    owners.foreach(selfType)
    val steps = scanParents()
    flagReachableModules()
    errors ++= ContextualEvidence.unreachable(
      new ContextualScope(owners, steps, scopeReader).candidates,
      resolver.boundedBinders.contains
    )
    // Parameters of the target hide outer names, but their type binders keep their identities.
    bindParams(target.frame)
    errors ++= positions.refusals
    errors ++= TopTypes.inlineDispatch(
      target,
      ResolutionContext(target.frame, variables, Set.empty),
      resolver
    )
    hiddenReads(steps)
    val result = resultShape()
    result.left.foreach(errors += _)
    val (proofs, refusals) = bounded.of(environment.map(_.shape) ++ result.toOption)
    errors ++= refusals
    Entry.of(
      target,
      resolver.kindOf(target),
      count(result, proofs.zipWithIndex.map((proof, i) => Binding(s"<bound#$i>", proof))),
      captures.toList.distinct.sorted
    )
  }

  // A declaration inherited from a parent the source set defines is a slot as well: the caller
  // fills it through the subclass. Only declarations travel — a parent's concrete members hold
  // values built from the environment it already shares — and an external parent of an open owner
  // is reported, not guessed at (`ForeignParents`). Each level is read under the arguments the
  // whole `extends` chain gives it, so a grandparent's `seed: X` is this scope's type, not an
  // intermediate level's binder. Each owner is its own receiver: a parent two of them inherit gives
  // each its own members, and each owner then judges the closed declarations bound as its members
  // (`InheritedClosedSlots`).
  private def scanParents(): List[InheritedScopes.Step] = {
    val steps = InheritedScopes
      .walk(owners.map(owner => owner -> resolver.typeParameters(owner)), resolver, target.frame)
      .flatMap {
        case Left(reason) =>
          errors += reason
          None
        case Right(step) =>
          inherited(step)
          Some(step)
      }
    owners.foreach { root =>
      val ancestors = steps.filter(_.root.id == root.id).map(_.declared)
      judge(root, ancestors)
      // A constructor chooses only its parameters: no owner's parents add a choice to it.
      if (!target.constructor) errors ++= ForeignParents.reasons(root :: ancestors, resolver)
    }
    steps
  }

  // One template's members in the order they were bound, its own declarations first. A closed one
  // the template implements or restates is not a choice: dropped, or refused.
  private def judge(root: Frame, ancestors: List[Frame]): Unit = {
    val slots = new InheritedClosedSlots(root, ancestors)
    qualifiedValues.toList.foreach { (id, binding) =>
      declaredIn.get(id).filter(_.slot.template.id == root.id).foreach { declared =>
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
  // those the root, a final template, implements in its own body.
  private def inherited(step: InheritedScopes.Step): Unit =
    step.declared.stats.foreach { stat =>
      val implemented = FinalImplementations.implementation(step.root, stat)
      implemented.foreach((_, tree) => implementations += step.root -> tree)
      val slot = Slot(step.declared, step.env, step.root)
      stat match {
        case value: Decl.Val           => addDeclaredValue(slot, value, implemented.map(_._1).toSet)
        case _ if implemented.nonEmpty => ()
        case method: Decl.Def          => inheritedCallable(step, slot, method)
        case other                     => declaration(slot, other)
      }
    }

  private def inheritedCallable(step: InheritedScopes.Step, slot: Slot, method: Decl.Def): Unit =
    overrideIdentity.matches(method, step.parent, step.from, step.declared) match {
      case Some(true)  => ()
      case Some(false) => addDeclaredCallable(slot, method)
      case None => errors += s"inherited override identity not resolved: ${method.name.value}"
    }

  // A final template's implementation reads the captures visible where the template is declared.
  // One of an enclosing local owner (no `Outer.this` reaches it) that the target hides behind a
  // nearer binding stays reachable through that member: a member that is the capture itself (`def
  // seed: A = a1`) adds it back to the environment; any other reading of it is refused.
  private def hiddenReads(steps: List[InheritedScopes.Step]): Unit =
    implementations.distinct
      .filterNot((_, tree) => DeclarationIdentity.same(tree, target.tree))
      .foreach { (template, tree) =>
        val members = FinalImplementations.memberNames(
          template :: steps.filter(_.root.id == template.id).map(_.declared)
        )
        val (reads, alias) = FinalImplementations.reads(tree)
        val visible = visibleAt.getOrElse(template.id, Map.empty)
        reads.filterNot(members).foreach { name =>
          visible.get(name).filter(hidden(name, _)).foreach { binding =>
            if (alias.contains(name)) qualifiedValues.update(binding.id, binding)
            else errors += s"final implementation reads a hidden capture: $name"
          }
        }
      }

  private def hidden(name: String, binding: Binding): Boolean =
    !qualifiedValues.contains(binding.id) && !values.get(name).contains(binding)

  private def resolveIn(
      tpe: Type,
      owner: Frame,
      env: Map[String, Resolved] = Map.empty
  ): Resolved = resolver.resolve(tpe, readingContext(owner, env))

  private def readingContext(owner: Frame, env: Map[String, Resolved]): ResolutionContext = {
    val bindings = if (env.isEmpty) resolver.typeParameters(owner) else env
    ResolutionContext(owner, bindings, Set.empty, Some(target.frame))
  }

  private def declaration(slot: Slot, stat: Stat): Unit =
    stat match {
      case v: Decl.Val       => addDeclaredValue(slot, v)
      case d: Decl.Def       => addDeclaredCallable(slot, d)
      case d: Decl.GivenLike => addDeclaredGiven(slot, d)
      case _                 => ()
    }

  // One owner's bindings: its own parameters, then every scope that shares its path.
  private def scan(owner: Frame): Unit = {
    if (owner.id != target.frame.id) errors ++= ConstrainedReceivers.reason(owner)
    bindParams(owner)
    frames(owner).foreach(scanFrame)
  }

  // One owner's parameters as bindings: the positions the estimate did not read, since a space it
  // read *is* the count and binding the position would only add a diagnostic for it. An anonymous
  // contextual parameter binds under its owner and position, since every one of them has the
  // empty name. A `var` parameter of an enclosing template is a field the instance can reassign: a
  // member reads its current value, not an input fixed at the call — the same capture a `var`
  // member is.
  private def bindParams(owner: Frame): Unit =
    owner.params.zipWithIndex.foreach { (p, index) =>
      if (owner.id != target.frame.id && p.mods.exists(_.is[Mod.VarParam]))
        errors += "mutable capture"
      else if (positions.leftToShape(p, owner))
        add(
          if (ContextualEvidence.anonymous(p)) ContextualEvidence.key(owner.path, index)
          else p.name.value,
          p.decltpe,
          Slot(owner, resolver.typeParameters(owner), owner),
          ParameterScope.of(owner)
        )
    }

  // A self type adds another type's members to `this` without inheriting them. This reading does
  // not enumerate them, so the environment is incomplete rather than smaller. A constructor's own
  // template is exempt: the instance whose `this` it would read does not exist yet.
  private def selfType(owner: Frame): Unit =
    if (!(target.constructor && owner.id == target.frame.id))
      owner.tree
        .flatMap(TypeApplications.template)
        .flatMap(_.body.selfOpt.flatMap(_.decltpe))
        .foreach(tpe => errors += s"self type not resolved: ${tpe.syntax}")

  // Package-level declarations are indexed across all files. Unrelated class parameters are never
  // pooled just because both happen to be named A.
  private def frames(owner: Frame): List[Frame] = {
    val peers = resolver.peers(owner.path)
    if (peers.exists(_.id == owner.id)) peers else List(owner)
  }

  private def scanFrame(scope: Frame): Unit = {
    resolver.inspect("scope")
    val visible = scope.stats.filter(visibleIn(scope, _))
    new Aliases(visible, scope).report()
    visible.foreach(flagStat(scope, _))
  }

  private def visibleIn(scope: Frame, stat: Stat): Boolean =
    (!scope.local || stat.pos.start < target.tree.pos.start) &&
      resolver.accessible(stat, scope, target.frame)

  private def scopeReader = ContextualScope.Reader(
    frames,
    visibleIn,
    resolveIn(_, _, _).toOption,
    identify,
    positions.leftToShape,
    resolver.unsuppliedParent
  )

  private def identify(tpe: Type, owner: Frame, env: Map[String, Resolved]): Nominal = {
    val context = readingContext(owner, env)
    Nominal(
      TypeIdentities.of(tpe, context, resolver),
      TypeIdentities.finalClass(tpe, context, resolver)
    )
  }

  private def bind(name: String, binding: Binding, owner: Frame): Unit = {
    values.update(name, binding)
    if (owner.id != target.frame.id) {
      captures += name
      // Shadowing the short name does not remove this.x / Outer.this.x / package.x.
      if (!owner.local) qualifiedValues.update(binding.id, binding)
    }
  }

  /** Where a declaration is read: the frame that declares it, the environment its types are read
    * under, and the template whose member it is — a parent's member reached from two receivers is
    * two slots, `Outer.this.seed` and `Inner.this.seed`.
    */
  private case class Slot(scope: Frame, env: Map[String, Resolved], template: Frame) {

    def id(name: String): String =
      if (template.id == scope.id) s"${scope.id}:$name" else s"${template.id}<${scope.id}:$name"

  }

  /** A declaration bound in a slot: the member's name, and the key `values` holds it under (an
    * overload's or an anonymous parameter's own).
    */
  private case class Declared(slot: Slot, member: String, key: String)

  private def bindSlot(slot: Slot, member: String, key: String, shape: Shape): Unit = {
    val binding = Binding(slot.id(key), shape)
    bind(key, binding, slot.scope)
    if (qualifiedValues.contains(binding.id))
      declaredIn.update(binding.id, Declared(slot, member, key))
  }

  // A declaration is read where its slot declares it; a parameter, where its owner types it.
  private def add(name: String, tpe: Option[Type], slot: Slot, reading: Frame): Unit =
    tpe
      .toRight(s"missing type of accessible value: $name")
      .flatMap(resolveIn(_, reading, slot.env)) match {
      case Left(reason) => errors += reason
      case Right(shape) => bindSlot(slot, name, name, shape)
    }

  // A scope's declarations by kind: a value binds, a variable or an unreadable given is a
  // diagnostic, and a body only matters where a concrete type is at stake.
  private def flagStat(scope: Frame, stat: Stat): Unit = stat match {
    case v: Defn.Val                               => flagVal(v)
    case d: (Decl.Val | Decl.Def | Decl.GivenLike) =>
      if (!isTarget(d)) declaration(Slot(scope, resolver.typeParameters(scope), scope), d)
    case _: Defn.Var | _: Decl.Var => errors += "mutable capture"
    case other                     => flagEnvironment(other)
  }

  // The signature being measured is not one of its own slots: binding it would let a declaration
  // delegate to itself and claim an implementation it does not have.
  private def isTarget(stat: Stat): Boolean = DeclarationIdentity.same(stat, target.tree)

  private def flagVal(value: Defn.Val): Unit =
    // A concrete value's body may compute, but what it can compute is already reachable from the
    // environment: its own inhabitants are named by the values in scope, so it neither adds a
    // choice nor hides one. Only a binding the scope cannot read at all does. (That is the
    // parametricity argument: a total parametric body cannot invent a value it is not given, so
    // leaving it out of the environment loses nothing.)
    // A destructured value names the components of one binding already in scope — `val (a, b) =
    // pair` binds nothing new, and `val (a, b) = capability()` names fields the capability's own
    // result shape already carries — so it is skipped for the same reason as an unreadable body.
    if (!value.pats.forall(TypeApplications.patVarName.getOption(_).nonEmpty)) ()

  private def addDeclaredValue(
      slot: Slot,
      declaration: Decl.Val,
      implemented: Set[String] = Set.empty
  ): Unit =
    declaration.pats.foreach { pat =>
      TypeApplications.patVarName.getOption(pat) match {
        case Some(name) if implemented(name) => ()
        case Some(name) => add(name, Some(declaration.decltpe), slot, slot.scope)
        case None       => errors += "destructured capture not resolved"
      }
    }

  // A declaration is not an implementation: someone else supplies it, so its result is a value this
  // scope has, and a callable declaration is a callable it can apply. Its *type* is what the count
  // works with: `def modify(f: A => B): S => T` applies like any other callable, while `def get[M]():
  // M` reads as unresolved, since its own binder M is not a shape this scope has.
  private def addDeclaredCallable(slot: Slot, declaration: Decl.Def): Unit = {
    val groups = declaration.paramClauseGroups
    val params = groups.flatMap(_.paramClauses).flatMap(_.values)
    if (!params.forall(_.decltpe.isDefined))
      errors += s"missing callable parameter type: ${declaration.name.value}"
    else {
      val tpes = params.flatMap(_.decltpe)
      val callable = Type.Function(Type.FuncParamClause(tpes), declaration.decltpe)
      val binders = groups
        .flatMap(_.tparamClause.values)
        .map(p => p.name.value -> Left(s"polymorphic capability binder: ${p.name.value}"))
        .toMap
      resolveIn(callable, slot.scope, slot.env ++ binders) match {
        case Left(reason) => errors += reason
        case Right(shape) => bindMember(slot, declaration, tpes, shape)
      }
    }
  }

  // One key per member: an overload is a second key, a restatement none.
  private def bindMember(
      slot: Slot,
      declaration: Decl.Def,
      tpes: List[Type],
      shape: Shape
  ): Unit = {
    val context = readingContext(slot.scope, slot.env)
    val identities = tpes.map(TypeIdentities.of(_, context, resolver))
    val member = DeclaredMembers.Declared(slot.template.id, slot.scope.id, shape, identities)
    declaredMembers.key(declaration.name.value, declaration, member) match {
      case Left(reason)     => errors += reason
      case Right(None)      => ()
      case Right(Some(key)) => bindSlot(slot, declaration.name.value, key, shape)
    }
  }

  private def addDeclaredGiven(slot: Slot, declaration: Decl.GivenLike): Unit =
    declaration.name match {
      case name: Term.Name => add(name.value, Some(declaration.decltpe), slot, slot.scope)
      case _               => errors += "anonymous given not resolved"
    }

  // A method that is only declared, in this scope or an inherited one, cannot invent a value: a
  // total parametric body builds its result from arguments and captures, and both are already in
  // the environment. An import of a type or a typeclass is the same shape of thing. Only what can
  // carry a *concrete* type this signature mentions is worth a diagnostic, since that is where an
  // outside producer could change the count.
  private def flagEnvironment(stat: Stat): Unit = stat match {
    case _: Defn.Def => ()
    case i: Import   => flagShadowing(i)
    case _: Export   => flagShadowing(stat)
    case _           => ()
  }

  // An import or an export can put a *different* type behind a name this resolution reads as a
  // builtin (`Option`, `Boolean`), so a builtin is only trustworthy while nothing shadows it.
  // That is the one way a name outside the lexical chain changes an answer here: a value
  // outside the chain cannot hold this method's binders, so it cannot add one of their values.
  // Named and renamed imports are now bound by lookup itself — they resolve their target or
  // refuse the builtin — so they are no longer this shape of unknown. What lookup does not
  // bind still is: a clause whose prefix is no pure path spells everything, and a `given`
  // selector names a type behind a value no lookup models. Exports keep the blanket read.
  private def flagShadowing(tree: Tree): Unit = {
    val unbound = tree match {
      case i: Import => unboundImportNames(i)
      case _         => names(tree)
    }
    if (shadowable.exists(unbound.contains)) {
      rebound = true
      errors += s"imported environment not resolved: ${tree.syntax.replaceAll("\\s+", " ")}"
    }
  }

  private def unboundImportNames(clause: Import): Set[String] =
    clause.importers.flatMap { importer =>
      val selectors = importer.importees.flatMap {
        case _: Importee.Name   => Nil
        case _: Importee.Rename => Nil
        case e                  => names(e)
      }
      if (SourceIndex.importPath(importer.ref).isEmpty) names(importer.ref) ++ selectors
      else selectors
    }.toSet

  // Every identifier a tree spells, which is what an import or an export can put a new meaning
  // behind.
  private def names(tree: Tree): Set[String] =
    TypeName.words(tree).toSet

  // Every name the root imports bind to a builtin or a model entry: the shape vocabulary, the
  // nominal keys of a rigid constructor's arguments and the top types' spellings.
  private def shadowable: Set[String] = BuiltinTypes.rootNames

  private def flagReachableModules(): Unit = {
    val reachable = resolver.modules.filter { module =>
      resolver.inspect("module candidate")
      !inChain(module) && moduleVisible(module)
    }
    if (reachable.nonEmpty) errors += qualifiedMemberMessage(reachable)
  }

  private def inChain(module: Frame): Boolean = owners.exists(_.id == module.id)

  private def moduleVisible(module: Frame): Boolean =
    !withinChain(module) && mentions.nonEmpty && module.stats.exists(
      resolver.accessible(_, module, target.frame)
    )

  // A module nested inside the class, given or method this target lives in shares its binders: its
  // members can hold the target's type parameters, so it is part of the scope rather than an
  // outside producer. Package and file frames are not binders — sharing those means only that two
  // declarations sit in the same package.
  private def withinChain(module: Frame): Boolean = {
    val binders = owners.filterNot(resolver.isPackage)
    module.chain.exists(frame => binders.exists(_.id == frame.id))
  }

  private def qualifiedMemberMessage(reachable: List[Frame]): String = {
    val rest = if (reachable.size == 1) "" else s" and ${reachable.size - 1} more"
    s"qualified member environment not resolved: ${reachable.head.path.mkString(".")}$rest"
  }

  private def resultShape(): Resolved =
    if (target.constructor) constructorShape() else declaredShape()

  // The positions left to the shape model: a type parameter's slot, whose count is the values the
  // caller supplies — the reading behind `Pair[A](x: A, y: A)` having four implementations — and
  // any position the estimate reads with a reason, where the model's diagnostic says more than a
  // number resting on an unknown name.
  private def constructorShape(): Resolved =
    sequence(
      target.frame.params
        .filter(positions.leftToShape(_, target.frame))
        .map(p =>
          p.decltpe
            .toRight(s"missing parameter type: ${p.name.value}")
            .flatMap(resolver.resolve(_, target.frame, variables))
        )
    ).map(Shape.Product(_))

  private def declaredShape(): Resolved =
    target.result
      .toRight("inferred result type not resolved")
      .flatMap(tpe =>
        resolver
          .resolve(tpe, target.frame, variables)
          .flatMap(shape => observed(tpe).toLeft(shape))
      )

  // A transparent result is what the call site observes only when no subclass can narrow it.
  private def observed(tpe: Type): Option[String] =
    Option
      .when(TransparentResults.transparent(target))(
        resolver.narrowedResult(tpe, resultContext(tpe))
      )
      .flatten

  private def resultContext(tpe: Type): ResolutionContext =
    ResolutionContext(target.frame, variables, Set.empty).referencing(tpe)

  // A goal with one value has one implementation whatever the environment holds, so nothing left
  // unresolved around it can change the count — unless an import or export clause may rebind a
  // builtin name, or a channel around the result reaches declarations the sources do not supply,
  // which may give any name the result spells another meaning. A type parameter clause that is no
  // Scala program makes the signature none either: it has no implementation to count.
  private def count(result: Resolved, bounds: List[Binding]): Count = result match {
    case Right(shape)
        if !target.constructor && !rebound && bounded.invalid.isEmpty &&
          TopTypes.terminalGoal(shape) =>
      target.result.flatMap(tpe => resolver.unreadChannel(resultContext(tpe))) match {
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

  /** One scope's value declarations, each resolved to the binding it names.
    *
    * A forward alias chain folds onto the value at its end; a chain that loops, names something
    * invisible, or has a body the model cannot read fails closed, as a diagnostic.
    */
  private class Aliases(stats: List[Stat], owner: Frame) {

    private val declared = stats
      .collect { case v: Defn.Val => v }
      .flatMap(v => v.pats.flatMap(TypeApplications.patVarName.getOption(_).map(_ -> v)))
      .toMap

    private val done = mutable.Map.empty[String, Either[String, Binding]]

    def report(): Unit = declared.keys.toList.sorted.foreach(reportOne)

    private def reportOne(name: String): Unit =
      value(name, Set.empty) match {
        case Right(binding) => bind(name, binding, owner)
        // A body this pass cannot follow is a *specific* inhabitant of the value's type, and a
        // total parametric body can only compute what the environment already holds: its value is
        // one of the bindings in scope, so skipping it loses nothing and blocking the count would
        // lose everything. Only an alias is worth following, because it *names* one of those
        // bindings and so raises the count's precision.
        case Left(_) => ()
      }

    private def value(name: String, active: Set[String]): Either[String, Binding] =
      resolver.expanding("capture alias") {
        if (active(name)) Left(s"recursive capture alias: $name")
        else done.getOrElseUpdate(name, source(name, active))
      }

    private def source(name: String, active: Set[String]): Either[String, Binding] = {
      val declaration = declared(name)
      val named = declaration.rhs match {
        case Term.Name(n) if declared.contains(n) => value(n, active + name)
        case Term.Name(n)                         =>
          values.get(n).toRight(s"capture alias not resolved: $name -> $n")
        case _ => Left(s"$name is not an alias")
      }
      named.flatMap(binding => checked(name, binding, declaration))
    }

    // A declared type must agree with the binding the alias names; a mismatch is a conversion the
    // model cannot follow.
    private def checked(
        name: String,
        binding: Binding,
        declaration: Defn.Val
    ): Either[String, Binding] =
      declaration.decltpe.fold(Right(binding): Either[String, Binding]) { tpe =>
        resolveIn(tpe, owner).flatMap { shape =>
          if (binding.shape == shape) Right(binding)
          else Left(s"capture alias type conversion not resolved: $name")
        }
      }

  }

}
