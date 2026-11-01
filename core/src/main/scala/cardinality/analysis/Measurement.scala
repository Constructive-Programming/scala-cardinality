package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.*
import cardinality.analysis.resolution.*
import cardinality.types.{Spelling, TypeApplications, TypeName}

import ContextualEvidence.Nominal
import Inhabitation.{Binding, Count, Shape}
import MethodAnalysis.{sequence, Entry, Frame, Resolved, Target}

/** One signature's environment: the bindings visible from its frame, the diagnostics that make that
  * environment incomplete, and the count the two admit.
  */
final private[cardinality] class Measurement(target: Target, resolver: Resolver) {

  private val errors = mutable.ListBuffer.empty[String]
  // Whether an import clause may rebind a builtin name.
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
    val inferred = InferredResults.declared(target, steps, variables, resolver)
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

  // A declaration inherited from a parent the source set defines is a slot as well: the caller
  // fills it through the subclass. Only declarations travel — a parent's concrete members hold
  // values built from the environment it already shares — and an external parent of an open owner
  // is reported, not guessed at (`ForeignParents`). Each level is read under the arguments the
  // whole `extends` chain gives it, so a grandparent's `seed: X` is this scope's type, not an
  // intermediate level's binder. Each owner is its own receiver: a parent two of them inherit gives
  // each its own members, and each owner then judges the closed declarations bound as its members
  // (`InheritedClosedSlots`).
  private def scanParents(): List[InheritedScopes.Step] = {
    val steps = walked
      .flatMap {
        case Left(reason) =>
          errors += reason
          None
        case Right(step) => Some(step)
      }
    val ancestors = steps.groupMap(_.root.id)(_.declared).withDefaultValue(Nil)
    steps.foreach(step => inherited(step, step.root :: ancestors(step.root.id)))
    owners.foreach { root =>
      judge(root, ancestors(root.id))
      // A constructor chooses only its parameters: no owner's parents add a choice to it.
      if (!target.constructor)
        errors ++= ForeignParents.reasons(root :: ancestors(root.id), resolver)
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
  // those a final root implements, in its own body or at another level of its linearization.
  //
  // Members gate G1: a final root's inherited declaration that another level of its linearization
  // implements is that level's concrete code — no subclass replaces it — never a free capability:
  // concrete code computes from the environment it is given, so it is dropped as the root's own
  // implementations are. One a local template implements may read a capture the target hides (no
  // prefix names it), so there it is refused. An open root's implemented members stay the open
  // capabilities a subclass may override.
  private def inherited(step: InheritedScopes.Step, linearization: List[Frame]): Unit =
    step.declared.stats.foreach { stat =>
      val implemented = FinalImplementations.implementation(step.root, stat)
      implemented.foreach((_, tree) => implementations += step.root -> tree)
      val slot = Slot(step.declared, step.env, step.root)
      val implementer =
        if (implemented.nonEmpty) None
        else FinalImplementations.implementer(step.root, linearization, stat)
      (stat, implementer) match {
        case (_, Some(level))     => errors ++= FinalImplementations.localImplementer(level, stat)
        case (value: Decl.Val, _) => addDeclaredValue(slot, value, implemented.map(_._1).toSet)
        case _ if implemented.nonEmpty => ()
        case (method: Decl.Def, _)     => inheritedCallable(step, slot, method)
        case (other, _)                => declaration(slot, other)
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
          template :: InheritedScopes.levels(steps, template)
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

  private def declaration(slot: Slot, stat: Stat): Unit =
    stat match {
      case v: Decl.Val       => addDeclaredValue(slot, v)
      case v: Decl.Var       => addDeclaredValue(slot, Decl.Val(v.mods, v.pats, v.decltpe))
      case d: Decl.Def       => addDeclaredCallable(slot, d)
      case d: Decl.GivenLike => addDeclaredGiven(slot, d)
      case _                 => ()
    }

  // One owner's bindings: its own parameters, then every scope that shares its path.
  private def scan(owner: Frame): Unit = {
    if (owner.id != target.frame.id) errors ++= ConstrainedReceivers.reason(owner)
    val outer = values.keySet.toSet -- owner.params.map(_.name.value)
    bindParams(owner)
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
      val levels = InheritedScopes.levels(walked.flatMap(_.toOption), owner)
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
          Slot(owner, resolver.typeParameters(owner), owner),
          ParameterScope.of(owner)
        )
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

    /** The template whose member an inherited declaration is. */
    def receiver: Option[Frame] = Option.when(template.id != scope.id)(template)

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

  // A declaration is read where its slot declares it; a parameter, where its owner types it. A
  // stable value's type-only refinement pins members of its path, which its path's reading follows:
  // the value itself is a value of the refined type.
  private def add(name: String, tpe: Option[Type], slot: Slot, reading: Frame): Unit =
    tpe
      .toRight(s"missing type of accessible value: $name")
      .flatMap(resolver.stableInput(_, readingContext(reading, slot.env, slot.receiver)))
      .flatMap(resolveIn(_, reading, slot.env, slot.receiver)) match {
      case Left(reason) => errors += reason
      case Right(shape) => bindSlot(slot, name, name, shape)
    }

  // A scope's declarations by kind: a declaration binds a slot, a concrete value or variable is
  // read through `Aliases`, and an import only matters where it may rebind a builtin name. The
  // model excludes mutation, so a `var` holds the value it was initialized with, as a `val` does,
  // and an abstract `var` is an abstract getter (its setter returns `Unit`): a declared slot.
  private def flagStat(scope: Frame, stat: Stat): Unit = stat match {
    case d: (Decl.Val | Decl.Var | Decl.Def | Decl.GivenLike) =>
      if (!isTarget(d)) declaration(Slot(scope, resolver.typeParameters(scope), scope), d)
    case i: Import => flagShadowing(i)
    case _         => ()
  }

  // The signature being measured is not one of its own slots: binding it would let a declaration
  // delegate to itself and claim an implementation it does not have.
  private def isTarget(stat: Stat): Boolean = DeclarationIdentity.same(stat, target.tree)

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
      resolveIn(callable, slot.scope, slot.env ++ binders, slot.receiver) match {
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

  // An import can put a *different* type behind a name this resolution reads as a builtin
  // (`Option`, `Boolean`), so a builtin is only trustworthy while nothing shadows it. Lookup binds
  // what a clause with a pure path brings — a named or renamed selector resolves its target or
  // refuses, a wildcard opens its namespace less the names it leaves out, and a `given` selector
  // brings contextual values and no name at all — and export clauses define members it binds the
  // same way. What lookup cannot bind is a clause whose prefix is no path (`import this.x.N`):
  // every name its selectors and prefix spell.
  private def flagShadowing(clause: Import): Unit = {
    val unbound = clause.importers
      .filter(importer => SourceIndex.importPath(importer.ref).isEmpty)
      .flatMap(importer => names(importer.ref) ++ importer.importees.flatMap(SourceIndex.boundName))
      .toSet
    if (shadowable.exists(unbound.contains)) {
      rebound = true
      errors += s"imported environment not resolved: ${Spelling.of(clause)}"
    }
  }

  // Every identifier a tree spells, which is what an import can put a new meaning behind.
  private def names(tree: Tree): Set[String] =
    TypeName.words(tree).toSet

  // Every name the root imports bind to a builtin or a model entry: the shape vocabulary, the
  // nominal keys of a rigid constructor's arguments and the top types' spellings.
  private def shadowable: Set[String] = BuiltinTypes.rootNames

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

  // The result type, declared or inferred, read in its context.
  private def declaredShape(inferred: Either[String, InferredResults.Inferred]): Resolved =
    inferred.flatMap { result =>
      resolver.resolve(result.tpe, result.context).flatMap(shape => observed(result).toLeft(shape))
    }

  // A transparent result is what the call site observes only when no subclass can narrow it.
  private def observed(result: InferredResults.Inferred): Option[String] =
    Option
      .when(TransparentResults.transparent(target))(
        resolver.narrowedResult(result.tpe, result.context.referencing(result.tpe))
      )
      .flatten

  // A goal with one value has one implementation whatever the environment holds, so nothing left
  // unresolved around it can change the count — unless an import clause may rebind a builtin
  // name, or a channel around the result reaches declarations the sources do not supply, which
  // may give any name the result spells another meaning. A type parameter clause that is no Scala
  // program makes the signature none either: it has no implementation to count.
  private def count(
      result: Resolved,
      inferred: Either[String, InferredResults.Inferred],
      bounds: List[Binding]
  ): Count = result match {
    case Right(shape)
        if !target.constructor && !rebound && bounded.invalid.isEmpty &&
          TopTypes.terminalGoal(shape) =>
      inferred.toOption.flatMap(r => resolver.unreadChannel(r.context.referencing(r.tpe))) match {
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

  /** A concrete value's initializer and declared type. */
  private case class Initialized(body: Term, decltpe: Option[Type])

  /** One scope's value declarations, each resolved to the binding it names.
    *
    * A forward alias chain folds onto the value at its end; a chain that loops, names something
    * invisible, or has a body the model cannot read fails closed, as a diagnostic.
    */
  private class Aliases(stats: List[Stat], owner: Frame) {

    // A `var` is read as the value it was initialized with: the model excludes the mutation that
    // could make it anything else.
    private val declared = stats
      .collect {
        case v: Defn.Val => (v.pats, Initialized(v.rhs, v.decltpe))
        case v: Defn.Var => (v.pats, Initialized(v.body, v.decltpe))
      }
      .flatMap((pats, v) => pats.flatMap(TypeApplications.patVarName.getOption(_).map(_ -> v)))
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
      val named = declaration.body match {
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
        declaration: Initialized
    ): Either[String, Binding] =
      declaration.decltpe.fold(Right(binding): Either[String, Binding]) { tpe =>
        resolveIn(tpe, owner).flatMap { shape =>
          if (binding.shape == shape) Right(binding)
          else Left(s"capture alias type conversion not resolved: $name")
        }
      }

  }

}
