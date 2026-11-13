package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.types.*

// The source front end's side of the fixed point (`Fixpoint`): the equations `Counter.equations`
// builds, their dependency graph and positivity, read off the syntax. `Scope` and `Walk` drive it
// through `Solver.solve`.

/** What the solver reads off a body's syntax besides its equations: the aliases' bodies, the
  * constructor arms, the sealed parents with their children, and the types a class holds through a
  * member of a value it stores (`reads`), every name already the key the system solves it under — a
  * member of an object joins its enclosing body's system as `O.T`.
  */
final private[cardinality] case class Shapes(
    aliases: Map[TypeName, Option[Type]],
    arms: Map[TypeName, List[List[Type]]],
    parents: Map[TypeName, List[TypeName]],
    reads: Map[TypeName, List[Type]] = Map.empty,
) {

  def ++(other: Shapes): Shapes =
    Shapes(
      aliases ++ other.aliases,
      arms ++ other.arms,
      parents ++ other.parents,
      reads ++ other.reads
    )

}

private[cardinality] object Shapes {

  /** A body's shapes, each type read through `rewrite` with the definition it belongs to — the
    * identity for a body solved under its own names.
    */
  def of(stats: List[Stat], rewrite: (Defn, Type) => Type = (_, t) => t): Shapes = {
    val classes = stats.collect { case d: Defn.Class if Counter.isConcreteClass(d) => d }
    Shapes(
      // An opaque alias's equation reads its representation, so what that names is a dependency.
      stats.collect { case d: Defn.Type => Walk.named(d) -> Some(rewrite(d, d.body)) }.toMap,
      classes.map { d =>
        Walk.named(d) -> List(stored(d.tparamClause.values, d.ctor).map(rewrite(d, _)))
      }.toMap ++ stats.collect {
        case d: Defn.Enum =>
          Walk.named(d) -> d.templ.body.stats.collect {
            case c: Defn.EnumCase => stored(c.tparamClause.values, c.ctor).map(rewrite(d, _))
            case _: Defn.RepeatedEnumCase => Nil
          }
      }.toMap,
      // A system adds its sealed families' subtypes (`SealedFamilies.edges`).
      Map.empty,
      classes.map(d => Walk.named(d) -> memberReads(stats, d).map(rewrite(d, _))).toMap,
    )
  }

  // A field typed by a member of another field's value (`x: q.X` beside `q: C`) holds what the
  // body's class `C` makes that member: a dependency no name of the field shows. The solver reads
  // such a member over an earlier solve of the body, so a cycle through it is never iterated.
  private def memberReads(stats: List[Stat], d: Defn.Class): List[Type] = {
    val declared = d.ctor.paramClauses
      .flatMap(_.values)
      .flatMap { p =>
        p.decltpe.flatMap(Counter.nameOf).map(p.name.value -> _.value)
      }
      .toMap
    def member(owner: String, name: String): Option[Type] =
      stats.collectFirst {
        case c: (Defn.Class | Defn.Trait) if c.name.value == owner =>
          c.templ.body.stats.collectFirst { case t: Defn.Type if t.name.value == name => t.body }
      }.flatten
    Trees
      .fields(d.ctor)
      .flatMap(_.collect {
        case Type.Select(Term.Name(q), Type.Name(m)) => declared.get(q).flatMap(member(_, m))
      }.flatten)
  }

  // The types a constructor stores, its context bounds' evidence included.
  private def stored(tparams: List[Type.Param], ctor: Ctor.Primary): List[Type] =
    Trees.fields(ctor) ++ Counter.evidence(tparams)

}

private[cardinality] object Solver {
  import Fixpoint.{Arm, Continuation}

  // The body's names keep their values in its scope.
  private given Store[TypeName, Scope] with {

    extension (scope: Scope) {
      def value(name: TypeName): Size = scope.getOrElse(name, NothingSize)
      def withValues(values: Iterable[(TypeName, Size)]): Scope = scope ++ values

      def withBlocked(blocked: Iterable[(TypeName, Blocked)]): Scope =
        Scope.blockedLens.modify(_ ++ blocked)(scope)

    }

  }

  // Solves the body's system of equations by Kleene iteration from the empty type until stable
  // (`Fixpoint`). Recognized productive cycles widen to their tier (`Size.widen`) and are omitted
  // from later rounds so consumers settle without demoting ε₀ to ω. `views` gives each name the
  // scope its own syntax reads in: an object member's arms are read with the object's names in
  // front, exactly as its equation reads them. The names left on a lazy cycle the ν completion
  // could not finish are recorded as blocked: their μ value counts the finite values only, so a
  // read of them is a reason, not a number.
  private[cardinality] def solve(
      entries: List[(TypeName, Named)],
      shapes: Shapes,
      base: Scope,
      views: TypeName => Scope => Scope = _ => identity,
  ): Scope = {
    val eqs = entries.map { case (name, named) => name -> named.equation }
    val deps = dependencies(shapes, eqs.map(_._1).toSet)
    val reaches = Fixpoint.reaches(deps)
    // A cycle through a non-positive position has no count: its definitions hold the unknown's ω
    // and are never iterated, so no consumer reads a number off the equation.
    val positivity = Positivity.of(
      shapes,
      entries.map((n, named) => n -> named.params).toMap,
      reaches,
      (owner, callee, arity) => slotsOf(views(owner)(base), callee, arity)
    )
    // A cycle through a member read off a value is solved against an earlier solve's values, never
    // iterated from 0: no count either, unless positivity already refused it.
    val blocks = positivity.blocked.map((n, c) => n -> blocking(c)) ++
      (unseen(shapes, reaches) -- positivity.blocked.keySet).map(_ -> Blocked.Unseen)
    val negative = blocks.keySet
    val marked = Scope.blockedLens.modify(_ ++ blocks)(
      Scope.slotsLens.modify(_ ++ positivity.slots)(base ++ negative.map(_ -> EffectiveOmega))
    )
    Fixpoint.solve(eqs, negative, marked, lazily(shapes, views), reaches)
  }

  // The definitions on a cycle through a type a class holds by a member of a value (`reads`).
  private def unseen(shapes: Shapes, reaches: Map[TypeName, Set[TypeName]]): Set[TypeName] =
    shapes.reads.toList.flatMap { (from, held) =>
      def back(to: TypeName) = to == from || reaches.getOrElse(to, Set.empty)(from)
      if (!held.flatMap(namesOf).exists(back)) Nil
      else from :: reaches.getOrElse(from, Set.empty).filter(back).toList
    }.toSet

  private def blocking(cause: Positivity.Cause): Blocked = cause match {
    case Positivity.Cause.Domain              => Blocked.Negative
    case Positivity.Cause.Unread(constructor) => Blocked.Unread(Namespaces.written(constructor))
  }

  // Which slots of a constructor the system does not define hold their arguments positively, as
  // the definition naming it reads the name: a definition solved around the read says (its system
  // found them), a standard constructor no source binds by the standard table (a partial
  // function's keys are its domain), and nothing else is read.
  private def slotsOf(scope: Scope, callee: String, arity: Int): Option[Positivity.Slots] = {
    val name = TypeName.of(callee)
    scope.binding(name) match {
      case Binding.Free =>
        StandardConstructors
          .slots(callee, arity)
          .map(_.map(data => Option.unless(data)(Positivity.Cause.Domain)))
      case Binding.Here               => scope.slots(name)
      case Binding.Elsewhere(in, key) => in.slots(key)
      case Binding.Clause(_)          => None
    }
  }

  // The dependency graph of the body equations: every name a definition mentions anywhere in
  // its syntax, plus the children of a sealed parent (whose value is their sum). Only a name
  // that can reach itself may be widened, so an acyclic chain of forward references settles
  // exactly, however many rounds it takes.
  private def dependencies(
      shapes: Shapes,
      defined: Set[TypeName]
  ): Map[TypeName, Set[TypeName]] = {
    val aliases = shapes.aliases.view.mapValues(_.fold(Set.empty[TypeName])(namesOf))
    val arms = shapes.arms.view.mapValues(_.flatten.flatMap(namesOf).toSet)
    val direct = aliases.toMap ++ arms
    defined.map { name =>
      name -> (direct.getOrElse(name, Set.empty[TypeName]) ++
        shapes.parents.getOrElse(name, Nil) ++
        shapes.reads.getOrElse(name, Nil).flatMap(namesOf))
    }.toMap
  }

  // What the ν completion reads off the body (`Fixpoint.Lazy`): each constructor's fields with the
  // aliases the body defines expanded at their heads, classified into continuations and read in
  // their definition's view of the scope, and the names each definition's by-name and function
  // fields mention.
  private def lazily(
      shapes: Shapes,
      views: TypeName => Scope => Scope
  ): Fixpoint.Lazy[TypeName, Scope] = {
    val arms = shapes.arms.view.mapValues(_.map(_.map(dealias(shapes.aliases)))).toMap
    val allNames = shapes.arms.keySet.union(shapes.parents.keySet)
    Fixpoint.Lazy(
      arms =
        arms.map((name, memberArms) => name -> memberArms.map(classified(allNames, views(name)))),
      parents = shapes.parents,
      lazily = arms.view.mapValues(_.flatten.flatMap(lazyTargets).toSet).toMap
    )
  }

  // A field's type with the aliases the body defines expanded at its head: `tail: Thunk` with
  // `type Thunk = () => Cell` is as lazy as `tail: () => Cell`.
  @tailrec
  private def dealias(aliases: Map[TypeName, Option[Type]], seen: Set[TypeName] = Set.empty)(
      tpe: Type
  ): Type = tpe match {
    case Type.Name(n) if !seen(TypeName.of(n)) =>
      aliases.get(TypeName.of(n)).flatten match {
        case Some(body) => dealias(aliases, seen + TypeName.of(n))(body)
        case None       => tpe
      }
    case other => other
  }

  // The definitions a field names where a constructor does not demand them.
  private def lazyTargets(param: Type): Set[TypeName] = param match {
    case Type.ByName(inner)   => namesOf(inner)
    case f: Type.FunctionType => namesOf(f.res)
    case _                    => Set.empty
  }

  private def classified(allNames: Set[TypeName], view: Scope => Scope)(
      arm: List[Type]
  ): Arm[TypeName, Scope] = {
    val classified = arm.map(param => param -> continuation(param, allNames, view))
    val rest = classified.collect { case (param, None) => param }
    Arm(
      classified.flatMap(_._2),
      scope => Counter.domain(view(scope))(rest),
      rest.flatMap(namesOf).toSet
    )
  }

  // A continuation parameter carries a defined name in a position the constructor never
  // demands on the way to building the value. Function fields require a domain free of
  // defined names — a domain mentioning the cycle is a negative occurrence, which
  // coiteration cannot pass (`case class P(t: P => P)` keeps its μ reading).
  private def continuation(
      param: Type,
      allNames: Set[TypeName],
      view: Scope => Scope
  ): Option[Continuation[TypeName, Scope]] = {
    def edge(target: String, kind: Continuation.Kind, domain: List[Type]) =
      Continuation[TypeName, Scope](
        TypeName.of(target),
        kind,
        scope => Counter.typeIn(view(scope))(param),
        scope => Counter.domain(view(scope))(domain)
      )
    param match {
      case Type.ByName(inner) =>
        bareTarget(inner).map(edge(_, Continuation.Kind.Deterministic, Nil))
      case Type.Function.After_4_6_0(Type.FuncParamClause(clause), result) =>
        val clean = clause.forall(p => namesOf(p).forall(n => !allNames.contains(n)))
        bareTarget(result).filter(_ => clean).map { target =>
          val kind =
            if (clause.isEmpty) Continuation.Kind.Deterministic
            else Continuation.Kind.FunctionField
          edge(target, kind, clause)
        }
      case Type.Apply.After_4_6_0(callee, Type.ArgClause(List(Type.Name(target))))
          if bareName(callee) == "Option" =>
        Some(edge(target, Continuation.Kind.OptionBranch, Nil))
      case _ => None
    }
  }

  // The single name an occurrence targets: `X` itself, or `Option[X]`. Uses the current
  // `Apply` shape (`After_4_6_0`) — the plain unapply silently fails to match it.
  private def bareTarget(tpe: Type): Option[String] = tpe match {
    case Type.Name(name) => Some(name)
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(List(Type.Name(name))))
        if bareName(callee) == "Option" =>
      Some(name)
    case _ => None
  }

  private def bareName(tpe: Type): String =
    Counter.nameOf(tpe).fold("")(_.value)

  private def namesOf(tpe: Type): Set[TypeName] =
    TypeSubstitution.typeNames(tpe).map(TypeName.of).toSet

}
