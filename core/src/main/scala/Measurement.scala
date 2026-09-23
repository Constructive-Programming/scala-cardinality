package cardinality

import scala.collection.mutable
import scala.meta.*

import Inhabitation.{Binding, Count, Shape}
import MethodAnalysis.{sequence, Entry, Frame, Limits, Resolved, Target, TypeEntry}

/** The narrow view of the index the measurement needs: resolve a name to the types that declare it,
  * resolve a type to its shape, the frames a name may be read in, and the package and module lists
  * an outside member could come from. The measurement holds this rather than the whole index, so
  * the two files stay apart without a second copy of the resolution rules.
  */
private[cardinality] trait Resolver {
  def lookup(name: String, frame: Frame): List[TypeEntry]

  def resolve(
      tpe: Type,
      frame: Frame,
      variables: Map[String, Resolved],
      visiting: Set[String] = Set.empty
  ): Resolved

  def typeParameters(frame: Frame): Map[String, Resolved]

  def accessible(tree: Tree, owner: Frame, from: Frame): Boolean

  def packages: List[Frame]

  def modules: List[Frame]

  def mentionedConcrete(target: Target): Set[String]

  def kindOf(target: Target): String

  def limits: Limits
}

/** One signature's environment: the bindings visible from its frame, the diagnostics that make that
  * environment incomplete, and the count the two admit.
  */
final private[cardinality] class Measurement(target: Target, resolver: Resolver) {

  private val errors = mutable.ListBuffer.empty[String]
  private val values = mutable.LinkedHashMap.empty[String, Binding]
  private val qualifiedValues = mutable.LinkedHashMap.empty[String, Binding]
  private val captures = mutable.ListBuffer.empty[String]
  private val variables = resolver.typeParameters(target.frame)
  private val owners = target.frame.chain.reverse
  // Members of a module outside the lexical chain can never hold or produce this method's
  // type parameters — a module's scope is fixed, and no method's binders reach it. They can
  // only matter where a signature mentions a concrete, modelled type (a Boolean, an Option),
  // so those modules, imports and sibling bodies are the ones to report rather than every
  // companion object and every import in the file.
  private val mentions = resolver.mentionedConcrete(target)

  def entry(): Entry = {
    variables.values.collect { case Left(reason) => reason }.foreach(errors += _)
    owners.foreach(scan)
    scanParents()
    flagReachableModules()
    // Parameters of the target hide outer names, but their type binders keep their identities.
    target.frame.params.foreach(p => add(p.name.value, p.decltpe, target.frame))
    val result = resultShape()
    result.left.foreach(errors += _)
    Entry(
      target.input,
      target.name,
      target.signature.replaceAll("\\s+", " "),
      target.tree.pos.startLine + 1,
      resolver.kindOf(target),
      count(result),
      captures.toList.distinct.sorted
    )
  }

  // A declaration inherited from a parent the source set defines is a slot as well: the caller
  // fills it through the subclass. Only declarations travel — a parent's concrete members hold
  // values built from the environment it already shares — and an external parent is left alone
  // rather than guessed at.
  private def scanParents(): Unit = {
    val seen = mutable.Set.empty[String]
    var pending = owners.flatMap(owner => owner.parents.map(owner -> _))
    while (pending.nonEmpty) {
      pending = pending.flatMap { case (from, parent) => scanParent(seen, from, parent) }
    }
  }

  // The parents one inherited declaration leaves to walk: nothing for a parent the source set does
  // not define, and once per frame so a diamond does not read the same declaration twice.
  private def scanParent(
      seen: mutable.Set[String],
      from: Frame,
      parent: Init
  ): List[(Frame, Init)] =
    resolver
      .lookup(typeName(parent.tpe), target.frame)
      .headOption
      .toList
      .flatMap(entry =>
        entry.self.toList.flatMap(declared => scanDeclared(seen, from, parent, declared))
      )

  private def scanDeclared(
      seen: mutable.Set[String],
      from: Frame,
      parent: Init,
      declared: Frame
  ): List[(Frame, Init)] =
    if (seen.add(declared.id)) inherited(declared, parent, from) else Nil

  // What an inherited frame contributes: its declarations, read in the scope that named it, then
  // the parents it introduces itself.
  private def inherited(declared: Frame, parent: Init, from: Frame): List[(Frame, Init)] = {
    val env = resolver.typeParameters(declared) ++ inheritedEnv(declared, parent, from)
    declared.stats.foreach(declaration(declared, env, _))
    declared.parents.map(declared -> _)
  }

  // The parent's type arguments, resolved where the parent is named: `extends Base[A]` reads an
  // inherited `Base[A].seed` as this scope's A, not as a second binder that happens to share the
  // name.
  private def inheritedEnv(declared: Frame, parent: Init, from: Frame): Map[String, Resolved] =
    arguments(parent) match {
      case None       => Map.empty
      case Some(args) =>
        declared.typeParams
          .map(_.name.value)
          .zip(args)
          .flatMap {
            case (name, arg) =>
              resolver
                .resolve(arg, from, resolver.typeParameters(from))
                .toOption
                .map(name -> Right(_))
          }
          .toMap
    }

  private def arguments(parent: Init): Option[List[Type]] = parent.tpe match {
    case Type.Apply.After_4_6_0(_, Type.ArgClause(args)) => Some(args)
    case _                                               => None
  }

  // The name a parent is resolved by: `Base` in `Base[A]`, the last segment of `Outer.Base`.
  // `Init.name` is anonymous for a plain type parent, so the type is the source of the name.
  private def typeName(tpe: Type): String = tpe match {
    case Type.Name(name)                   => name
    case Type.Select(_, name)              => name.value
    case Type.Apply.After_4_6_0(callee, _) => typeName(callee)
    case other                             => other.syntax
  }

  private def declaration(scope: Frame, env: Map[String, Resolved], stat: Stat): Unit =
    stat match {
      case v: Decl.Val       => addDeclaredValue(scope, env, v)
      case d: Decl.Def       => addDeclaredCallable(scope, env, d)
      case d: Decl.GivenLike => addDeclaredGiven(scope, env, d)
      case _                 => ()
    }

  // One owner's bindings: its own parameters, then every scope that shares its path.
  private def scan(owner: Frame): Unit = {
    owner.params.foreach(p => add(p.name.value, p.decltpe, owner))
    frames(owner).foreach(scanFrame)
  }

  // Package-level declarations are indexed across all files. Unrelated class parameters are never
  // pooled just because both happen to be named A.
  private def frames(owner: Frame): List[Frame] = {
    val peers = resolver.packages.filter(_.path == owner.path).toList
    if (peers.exists(_.id == owner.id)) peers else List(owner)
  }

  private def scanFrame(scope: Frame): Unit = {
    val visible = scope.stats.filter(visibleIn(scope, _))
    new Aliases(visible, scope).report()
    visible.foreach(flagStat(scope, _))
  }

  private def visibleIn(scope: Frame, stat: Stat): Boolean =
    (!scope.local || stat.pos.start < target.tree.pos.start) &&
      resolver.accessible(stat, scope, target.frame)

  private def bind(name: String, binding: Binding, owner: Frame): Unit = {
    values.update(name, binding)
    if (owner.id != target.frame.id) {
      captures += name
      // Shadowing the short name does not remove this.x / Outer.this.x / package.x.
      if (!owner.local) qualifiedValues.update(s"${owner.id}:$name", binding)
    }
  }

  private def add(name: String, tpe: Option[Type], owner: Frame): Unit =
    add(name, tpe, owner, resolver.typeParameters(owner))

  private def add(
      name: String,
      tpe: Option[Type],
      owner: Frame,
      env: Map[String, Resolved]
  ): Unit =
    tpe
      .toRight(s"missing type of accessible value: $name")
      .flatMap(resolver.resolve(_, owner, env)) match {
      case Left(reason) => errors += reason
      case Right(shape) => bind(name, Binding(s"${owner.id}:$name", shape), owner)
    }

  // A scope's declarations by kind: a value binds, a variable or an unreadable given is a
  // diagnostic, and a body only matters where a concrete type is at stake.
  private def flagStat(scope: Frame, stat: Stat): Unit = stat match {
    case v: Defn.Val               => flagVal(v)
    case v: Decl.Val               => addDeclaredValue(scope, resolver.typeParameters(scope), v)
    case d: Decl.Def               => addDeclaredCallable(scope, resolver.typeParameters(scope), d)
    case d: Decl.GivenLike         => addDeclaredGiven(scope, resolver.typeParameters(scope), d)
    case _: Defn.Var | _: Decl.Var => errors += "mutable capture"
    case other                     => flagEnvironment(other)
  }

  private def flagVal(value: Defn.Val): Unit =
    // A concrete value's body may compute, but what it can compute is already reachable from the
    // environment: its own inhabitants are named by the values in scope, so it neither adds a
    // choice nor hides one. Only a binding the scope cannot read at all does. (That is the
    // parametricity argument: a total parametric body cannot invent a value it is not given, so
    // leaving it out of the environment loses nothing.)
    // A destructured value names the components of one binding already in scope — `val (a, b) =
    // pair` binds nothing new, and `val (a, b) = capability()` names fields the capability's own
    // result shape already carries — so it is skipped for the same reason as an unreadable body.
    if (!value.pats.forall(_.is[Pat.Var])) ()

  private def addDeclaredValue(
      scope: Frame,
      env: Map[String, Resolved],
      declaration: Decl.Val
  ): Unit =
    declaration.pats.foreach {
      case Pat.Var(name) => add(name.value, Some(declaration.decltpe), scope, env)
      case _             => errors += "destructured capture not resolved"
    }

  // A declaration is not an implementation: someone else supplies it, so its result is a value this
  // scope has, and a callable declaration is a callable it can apply.
  private def addDeclaredCallable(
      scope: Frame,
      env: Map[String, Resolved],
      declaration: Decl.Def
  ): Unit = {
    val groups = declaration.paramClauseGroups
    val params = groups.flatMap(_.paramClauses).flatMap(_.values)
    val tpes = params.map(_.decltpe)
    // A declared callable is a capability whose *type* is what the fragment can count with:
    // `def modify(f: A => B): S => T` is expressible and applies like any other callable, while
    // `def get[M](): M` reads as an unresolved type because its own parameter M is not a shape this
    // scope has. Resolution decides, not the mere presence of parameters.
    if (!params.forall(_.decltpe.isDefined))
      errors += s"missing callable parameter type: ${declaration.name.value}"
    else {
      val callable = Type.Function(Type.FuncParamClause(tpes.flatten), declaration.decltpe)
      add(declaration.name.value, Some(callable), scope, env)
    }
  }

  private def addDeclaredGiven(
      scope: Frame,
      env: Map[String, Resolved],
      declaration: Decl.GivenLike
  ): Unit =
    declaration.name match {
      case name: Term.Name => add(name.value, Some(declaration.decltpe), scope, env)
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
  // builtin (`Option`, `Boolean`), so a builtin is only trustworthy while nothing shadows it. That
  // is the one way a name outside the lexical chain changes an answer here: a value outside the
  // chain cannot hold this method's binders, so it cannot add one of their values.
  private def flagShadowing(tree: Tree): Unit = {
    val shadowed = shadowable.filter(names(tree).contains)
    if (shadowed.nonEmpty)
      errors += s"imported environment not resolved: ${tree.syntax.replaceAll("\\s+", " ")}"
  }

  // Every identifier a tree spells, which is what an import or an export can put a new meaning
  // behind.
  private def names(tree: Tree): Set[String] =
    tree.syntax.split("[^\\p{L}\\p{N}_$]+").toSet

  private def shadowable: Set[String] =
    Set("Unit", "Nothing", "Boolean", "Option", "Either", "EmptyTuple")

  private def flagReachableModules(): Unit = {
    val reachable = resolver.modules.filterNot(inChain).filter(moduleVisible).toList
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
    val binders = owners.filterNot(owner => resolver.packages.exists(_.id == owner.id))
    module.chain.exists(frame => binders.exists(_.id == frame.id))
  }

  private def qualifiedMemberMessage(reachable: List[Frame]): String = {
    val rest = if (reachable.size == 1) "" else s" and ${reachable.size - 1} more"
    s"qualified member environment not resolved: ${reachable.head.path.mkString(".")}$rest"
  }

  private def resultShape(): Resolved =
    if (target.constructor) constructorShape() else declaredShape()

  private def constructorShape(): Resolved =
    sequence(
      target.frame.params.map(p =>
        p.decltpe
          .toRight(s"missing parameter type: ${p.name.value}")
          .flatMap(resolver.resolve(_, target.frame, variables))
      )
    ).map(Shape.Product(_))

  private def declaredShape(): Resolved =
    target.result
      .toRight("inferred result type not resolved")
      .flatMap(resolver.resolve(_, target.frame, variables))

  private def count(result: Resolved): Count =
    if (errors.nonEmpty) Count.Unresolved(errors.toList.distinct.sorted)
    else result.fold(unresolved, inhabitation)

  private def unresolved(reason: String): Count = Count.Unresolved(List(reason))

  private def inhabitation(shape: Shape): Count =
    Inhabitation.count(
      (values.values ++ qualifiedValues.values).toList,
      shape,
      resolver.limits.maxStates
    )

  /** One scope's value declarations, each resolved to the binding it names.
    *
    * A forward alias chain folds onto the value at its end; a chain that loops, names something
    * invisible, or has a body the model cannot read fails closed, as a diagnostic.
    */
  private class Aliases(stats: List[Stat], owner: Frame) {

    private val declared = stats
      .collect { case v: Defn.Val => v }
      .flatMap(v => v.pats.collect { case Pat.Var(n) => n.value -> v })
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
      if (active(name)) Left(s"recursive capture alias: $name")
      else done.getOrElseUpdate(name, source(name, active))

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
        resolver.resolve(tpe, owner, resolver.typeParameters(owner)).flatMap { shape =>
          if (binding.shape == shape) Right(binding)
          else Left(s"capture alias type conversion not resolved: $name")
        }
      }

  }

}
