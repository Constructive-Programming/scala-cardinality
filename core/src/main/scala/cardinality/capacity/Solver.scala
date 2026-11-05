package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.types.*

// The fixed-point machinery behind the counter: Kleene iteration with widening over the
// equations `Counter.equations` builds, and the coinductive completion of lazy cycles
// (docs/type-arithmetic.md §8). `Scope` and `Walk` drive it through `Solver.solve`.

/** What the solver reads off a body's syntax besides its equations: the aliases' bodies, the
  * constructor arms and the sealed parents with their children, every name already the key the
  * system solves it under — a member of an object joins its enclosing body's system as `O.T`.
  */
final private[cardinality] case class Shapes(
    aliases: Map[TypeName, Option[Type]],
    arms: Map[TypeName, List[List[Type]]],
    parents: Map[TypeName, List[TypeName]],
) {

  def ++(other: Shapes): Shapes =
    Shapes(aliases ++ other.aliases, arms ++ other.arms, parents ++ other.parents)

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
    )
  }

  // The types a constructor stores, its context bounds' evidence included.
  private def stored(tparams: List[Type.Param], ctor: Ctor.Primary): List[Type] =
    Trees.fields(ctor) ++ Counter.evidence(tparams)

}

private[cardinality] object Solver {

  // Solves the body's system of equations by Kleene iteration from the empty type until stable.
  // Recognized productive cycles widen to their tier (`Size.widen`) and are omitted from later
  // rounds so consumers settle without demoting ε₀ to ω. Once `budget` expires, mutual recursion
  // widens names that grew earlier while leaving first-time growth along acyclic chains intact.
  // `views` gives each name the scope its own syntax reads in: an object member's arms are read
  // with the object's names in front, exactly as its equation reads them. The names left on a
  // lazy cycle the ν completion could not finish are recorded as blocked: their μ value counts
  // the finite values only, so a read of them is a reason, not a number.
  private[cardinality] def solve(
      entries: List[(TypeName, Named)],
      shapes: Shapes,
      base: Scope,
      views: TypeName => Scope => Scope = _ => identity,
  ): Scope = {
    val eqs = entries.map { case (name, named) => name -> named.equation }
    val deps = dependencies(shapes, eqs.map(_._1).toSet)
    val succ: TypeName => Set[TypeName] = n => deps.getOrElse(n, Set.empty)
    val reaches = deps.keySet.map(name => name -> closure(succ, succ(name), Set.empty)).toMap
    val cyclic = deps.keySet.filter(name => reaches(name).contains(name))
    // A cycle through a non-positive position has no count: its definitions hold the unknown's ω
    // and are never iterated, so no consumer reads a number off the equation.
    val positivity = Positivity.of(
      shapes,
      entries.map((n, named) => n -> named.params).toMap,
      reaches,
      (owner, callee, arity) => slotsOf(views(owner)(base), callee, arity)
    )
    val blocks = positivity.blocked.map((n, c) => n -> blocking(c))
    val negative = blocks.keySet
    val marked = Scope.blockedLens.modify(_ ++ blocks)(
      Scope.slotsLens.modify(_ ++ positivity.slots)(base ++ negative.map(_ -> EffectiveOmega))
    )
    val system = Equations(eqs, cyclic, negative)
    val mu = iterate(system, marked, frozen = negative)
    val ctx = NuContext.of(shapes, views)
    val groups = cycleGroups(ctx).filterNot((members, _) => members.exists(negative))
    val completed = groups.flatMap { case (members, attached) => members.union(attached) }.toSet
    val lazily = blocked(shapes, reaches, completed) -- negative
    Scope.blockedLens.modify(_ ++ lazily.map(_ -> Blocked.Lazy))(
      greatestFixpoint(system, ctx, groups, mu)
    )
  }

  /** A system's equations: which of them are on a cycle of the syntactic graph, and which hold a
    * fixed value no round re-evaluates (`fixed`).
    */
  final private case class Equations(
      eqs: List[(TypeName, Scope => Size)],
      cyclic: Set[TypeName],
      fixed: Set[TypeName]
  )

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

  // The widening iteration. `base` seeds every equation — except the frozen ones — with the
  // empty type, and a frozen name keeps the value `base` gives it instead of being
  // re-evaluated, which is how the caller re-derives consumers around a settled cycle. A name the
  // budget guard widens (below) is on a cycle neither the graph nor positivity saw: blocked, so
  // every read of it is a reason, never the number.
  private def iterate(system: Equations, base: Scope, frozen: Set[TypeName]): Scope = {
    val Equations(eqs, cyclic, _) = system
    val seeded =
      base ++ eqs.collect {
        case (name, _) if !frozen(name) => name -> (NothingSize: Size)
      }
    val rounds = eqs.size + 8

    @tailrec
    def step(
        scope: Scope,
        grewLastRound: Set[TypeName],
        grewEver: Set[TypeName],
        widened: Set[TypeName],
        budget: Int
    ): Scope = {
      val evaluated = eqs.collect {
        case (name, equation) if !frozen(name) && !widened(name) => name -> equation(scope)
      }.toMap
      val grown = evaluated.collect { case (name, size) if scope(name) != size => name }.toSet
      if (grown.isEmpty)
        Scope.blockedLens.modify(_ ++ (widened -- cyclic).map(_ -> Blocked.Unseen))(
          scope ++ evaluated
        )
      else {
        val lastRounds = if (budget > 0) grewLastRound else grewEver
        // An acyclic chain settles within as many rounds as it has links once its inputs settle,
        // so a name still growing a whole budget after the first one ran out is on a cycle the
        // graph could not see, and takes a cycle's widening: a termination guard no fixture
        // reaches.
        val recursive = if (budget > -rounds) cyclic else grown
        val toWiden = grown.intersect(lastRounds).intersect(recursive)
        step(
          scope = scope ++ evaluated ++ toWiden.map(name => name -> evaluated(name).widen),
          grewLastRound = grown,
          grewEver = grewEver.union(grown),
          widened = widened.union(toWiden),
          budget = budget - 1
        )
      }
    }

    step(
      seeded,
      grewLastRound = Set.empty,
      grewEver = Set.empty,
      widened = Set.empty,
      budget = rounds
    )
  }

  // Expands `succ` transitively from `frontier` until nothing new is reachable.
  @tailrec
  private def closure(
      succ: TypeName => Set[TypeName],
      frontier: Set[TypeName],
      seen: Set[TypeName]
  ): Set[TypeName] = {
    val next = frontier.flatMap(succ).diff(seen)
    if (next.isEmpty) seen else closure(succ, next, seen.union(next))
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
      name -> (direct.getOrElse(name, Set.empty[TypeName]) ++ shapes.parents.getOrElse(name, Nil))
    }.toMap
  }

  // A lazy cycle: a by-name or function field (`tail: => X`, `f: D => X`) whose type names a
  // definition that reaches back to the field's own definition. A value of the cycle can be tied
  // by a lazy knot (`lazy val c: Cell = Cell(S(true, c))`), which only the ν completion counts;
  // when it did not complete the cycle, every definition on it is blocked.
  private def blocked(
      shapes: Shapes,
      reaches: Map[TypeName, Set[TypeName]],
      completed: Set[TypeName]
  ): Set[TypeName] =
    shapes.arms.toList.flatMap { (owner, memberArms) =>
      def back(m: TypeName): Boolean = m == owner || reaches.get(m).exists(_(owner))
      val onCycle =
        memberArms.flatten.map(dealias(shapes.aliases)).flatMap(lazyTargets).exists(back)
      if (!onCycle || completed(owner)) Nil
      else owner :: reaches.getOrElse(owner, Set.empty).filter(back).toList
    }.toSet

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

  // The greatest fixed point of lazy recursion (docs/type-arithmetic.md §8): the μ
  // iterate above counts the finite constructor trees; a parameter the constructor never
  // demands lets a value unfold forever. A *continuation edge* is an occurrence of a
  // defined name in a lazy position:
  //   - `=> X`, `=> Option[X]` or the thunk `() => X`: one continuation per node — the
  //     cycle is deterministic and the infinite part is its per-lap label space raised to
  //     ω (the `Size.pow` finite-program reading): `νX.X` counts 1, an endless `Boolean`
  //     stream ω, an ε₀-tier label space such as `String`-valued streams ε₀;
  //   - a strict `Option[X]` field (per node a `Some(tower)` beside the hole) or an
  //     inhabited-domain function field `D => X` (one continuation per input of `D`, as
  //     `lazy val e = One(Two(_ => e))` shows): the cycle branches, and every branching
  //     unfolding is generated by a finite program, so the infinite part saturates at ω —
  //     unless the per-lap label space itself reaches ε₀, which branching may not demote,
  //     and unless it is empty, in which case branching cannot conjure values at all.
  // Coiteration is blocked — the cycle skipped, leaving the sound μ under-count — when a
  // continuing arm demands the cycle the hard way: a strict self argument (`x: X`), a
  // mention nested inside a tuple, `Set[X]` or a function domain (`X => Y`).
  //
  // Cycles are the strongly-connected groups of the continuation graph; a sealed parent is
  // a pass-through node whose holed children are one hop apart, and more than one child
  // continuing through it is itself the per-node branch. Each completed cycle combines its μ
  // value with the infinite contribution, then collapses an infinite total to its highest tier.
  // Finite totals stay unchanged. The label-space environment is re-computed from the immutable
  // μ base rather than accumulated — adding a contribution per round would count solver rounds
  // as coefficients. A cycle may consume another cycle's ν, so the environment is refined
  // until stable, within a budget that only guards the loop.
  private def greatestFixpoint(
      system: Equations,
      ctx: NuContext,
      groups: List[(Set[TypeName], Set[TypeName])],
      mu: Scope
  ): Scope =
    if (groups.isEmpty) mu
    else {
      // Contributions are single-tier values, so a cycle can only move another cycle once;
      // the budget is a guard against a pathological dependency order, not a semantic.
      val budget = groups.size * 4 + 8

      @tailrec
      def refine(env: Scope, rounds: Int): Scope = {
        val contributions = groups.foldLeft(Map.empty[TypeName, Size]) {
          case (acc, (members, attached)) =>
            infiniteOf(ctx)(members, attached, env).fold(acc) { infinite =>
              members.union(attached).foldLeft(acc) { (a, name) =>
                a.updated(name, a.getOrElse(name, NothingSize) + infinite)
              }
            }
        }
        val next = contributions.foldLeft(env) {
          case (scope, (name, infinite)) =>
            scope.updated(name, completeCoinduction(mu.getOrElse(name, NothingSize), infinite))
        }
        if (next == env || rounds <= 0) next else refine(next, rounds - 1)
      }

      val settled = refine(mu, budget)
      val frozen = groups.flatMap { case (members, attached) => members.union(attached) }.toSet

      // Re-derive everything outside the cycles from the frozen cycle values. Widening during
      // the iteration is a way of stopping it, not a result: a consumer widened only because a
      // cyclic dependency was still growing lands here on the value its own definition has, so
      // a definition and a reference to it always agree, and a chain of consumers carries the
      // ν count all the way down.
      iterate(system, settled, frozen ++ system.fixed)
    }

  // Collapse the completed lazy type, not an enclosing sum or source/signature total. Guard
  // widening with hasInfinite: finite results, including zero and one, must remain finite.
  // Both recursive definitions and built-in lazy collections use this boundary.
  private[cardinality] def completeCoinduction(finite: Size, infinite: Size): Size = {
    val total = finite + infinite
    if (total.hasInfinite) total.widen else total
  }

  // The solver's fixed context: constructor arms, the sealed parents they pass through,
  // the arms classified into continuations, and the resulting parent -> holed children
  // map.
  final private case class NuContext(
      shapes: Shapes,
      classified: Map[TypeName, List[ClassifiedArm]],
      holedChildren: Map[TypeName, Set[TypeName]],
      views: TypeName => Scope => Scope,
  ) {

    def arms: Map[TypeName, List[List[Type]]] = shapes.arms

    def parents: Map[TypeName, List[TypeName]] = shapes.parents
  }

  private object NuContext {

    def of(shapes: Shapes, views: TypeName => Scope => Scope): NuContext = {
      val arms = shapes.arms.view.mapValues(_.map(_.map(dealias(shapes.aliases)))).toMap
      val classified = classifyArms(arms, shapes.arms.keySet.union(shapes.parents.keySet))
      val holedChildren: Map[TypeName, Set[TypeName]] = shapes.parents.keySet.toList.map { parent =>
        parent -> classified.collect {
          case (name, memberArms)
              if memberArms.exists(_.continuations.exists(_.target == parent)) =>
            name
        }.toSet
      }.toMap
      NuContext(shapes, classified, holedChildren, views)
    }

  }

  // Strongly-connected groups of the continuation graph that actually cycle (a self edge
  // counts), paired with the sealed parents they pass through. A parent leading to a
  // child outside the group breaks the cycle's determinism in a way the label space
  // cannot express: drop the group.
  private def cycleGroups(ctx: NuContext): List[(Set[TypeName], Set[TypeName])] = {
    def successors(name: TypeName): Set[TypeName] =
      ctx
        .classified(name)
        .flatMap(_.continuations)
        .flatMap { edge =>
          if (ctx.arms.contains(edge.target)) Some(edge.target)
          else ctx.holedChildren.getOrElse(edge.target, Set.empty[TypeName])
        }
        .toSet

    val reach = ctx.arms.keySet
      .map(name => name -> closure(successors, Set(name), Set(name)))
      .toMap
    ctx.arms.keySet.toList
      .map(name => ctx.arms.keySet.filter(m => reach(name).contains(m) && reach(m).contains(name)))
      .filter(group => group.exists(name => successors(name).intersect(group).nonEmpty))
      .toList
      .distinct
      .flatMap { group =>
        val targets =
          group.flatMap(name => ctx.classified(name).flatMap(_.continuations).map(_.target))
        val attached = targets.filter(ctx.parents.contains)
        if (attached.exists(parent => !group.subsetOf(ctx.holedChildren(parent)))) None
        else Some((group, attached))
      }
      .distinct
  }

  // The infinite part a strongly-connected group contributes at the given scope, or None
  // when it is blocked: some member has no continuing arm, or a continuing arm demands
  // the cycle outright. A deterministic cycle gets its per-lap label space raised to ω; an
  // empty label space admits no node at all, so branching cannot conjure values from it. Any
  // branch — two continuations in one arm, a strict `Option[X]` field, an inhabited function
  // field, or a sealed parent with several continuing children — saturates at ω under the §4
  // finite-program reading, which counts one program per unfolding, unless the label space
  // itself reaches the ε₀ tier, which branching does not demote.
  private def infiniteOf(ctx: NuContext)(
      members: Set[TypeName],
      attached: Set[TypeName],
      scope: Scope
  ): Option[Size] = {
    val cyc = members.union(attached)
    val perMember =
      members.toList.flatMap(name =>
        continuingArms(ctx.classified(name), cyc, ctx.views(name)(scope)).toList
      )
    if (perMember.length != members.size) None
    else {
      val branchy =
        attached.exists(parent => ctx.holedChildren(parent).size >= 2) || perMember.exists(_._2)
      val lap = perMember.map(_._1).foldLeft(UnitSize: Size)(_ * _) // per-lap label space
      Some(cycleContribution(branchy = branchy, lap = lap))
    }
  }

  // One cycle infinite part: an empty per-lap label space admits no node at all, so branching
  // cannot conjure values from it. A branch saturates at ω under the §4 finite-program
  // reading, which counts one program per unfolding, unless the label space itself reaches the
  // ε₀ tier, which branching does not demote. A deterministic cycle raises its label space
  // to ω.
  private def cycleContribution(branchy: Boolean, lap: Size): Size =
    if (lap.isZero) NothingSize
    else if (branchy) if (lap.hasInfinite) EffectiveEpsilon0 else EffectiveOmega
    else lap.pow(EffectiveOmega)

  // A member's per-lap space and branch flag, or None when blocked: no arm continues the
  // cycle, or a continuing arm's rest demands it (`x: X` outright, tuples, `Set[X]`,
  // negative occurrences — anything the continuation forms do not cover).
  private def continuingArms(
      memberArms: List[ClassifiedArm],
      cyc: Set[TypeName],
      scope: Scope
  ): Option[(Size, Boolean)] = {
    val continuing = memberArms.filter(_.continuations.exists(c => cyc.contains(c.target)))
    if (
      continuing.isEmpty || continuing.exists(_.rest.exists(p => namesOf(p).exists(cyc.contains)))
    ) None
    else
      Some(
        (
          continuing.map(armSpace(scope, cyc)).foldLeft(NothingSize: Size)(_ + _),
          continuing.size >= 2 || continuing.exists { arm =>
            val cycleEdges = arm.continuations.filter(c => cyc.contains(c.target))
            cycleEdges.size >= 2 || cycleEdges.exists { edge =>
              edge.kind == Continuation.OptionBranch ||
              edge.kind == Continuation.FunctionField &&
              Counter.domain(scope)(edge.domain).larger(UnitSize)
            }
          }
        )
      )
  }

  // One arm's per-lap space: the product of its ordinary parameters — external
  // continuations included, since they evaluate to a settled size — times, for each
  // cycle-targeting function field, its domain: the space of inputs the unfolding answers
  // per node.
  private def armSpace(scope: Scope, cyc: Set[TypeName])(arm: ClassifiedArm): Size = {
    val restSpace = Counter.domain(scope)(arm.rest)
    arm.continuations.foldLeft(restSpace) { (s, edge) =>
      if (!cyc.contains(edge.target)) s * Counter.typeIn(scope)(edge.param)
      else s * Counter.domain(scope)(edge.domain)
    }
  }

  final private case class Continuation(
      param: Type,
      target: TypeName,
      kind: Continuation.Kind,
      domain: List[Type]
  )

  private object Continuation {
    sealed trait Kind
    case object Deterministic extends Kind // `=> X`, `=> Option[X]`, `() => X`
    case object OptionBranch extends Kind // strict `Option[X]`: Some(tower) beside the hole
    case object FunctionField extends Kind // `D => X`: one continuation per input of D
  }

  final private case class ClassifiedArm(continuations: List[Continuation], rest: List[Type])

  private def classifyArms(
      arms: Map[TypeName, List[List[Type]]],
      allNames: Set[TypeName]
  ): Map[TypeName, List[ClassifiedArm]] =
    arms.map {
      case (name, memberArms) =>
        name -> memberArms.map { arm =>
          val conts = arm.flatMap(param => continuation(param, allNames))
          ClassifiedArm(conts, arm.filterNot(param => continuation(param, allNames).isDefined))
        }
    }

  // A continuation parameter carries a defined name in a position the constructor never
  // demands on the way to building the value. Function fields require a domain free of
  // defined names — a domain mentioning the cycle is a negative occurrence, which
  // coiteration cannot pass (`case class P(t: P => P)` keeps its μ reading).
  private def continuation(param: Type, allNames: Set[TypeName]): Option[Continuation] =
    param match {
      case Type.ByName(inner) =>
        bareTarget(inner).map(target =>
          Continuation(param, TypeName.of(target), Continuation.Deterministic, Nil)
        )
      case Type.Function.After_4_6_0(Type.FuncParamClause(clause), result) =>
        val clean = clause.forall(p => namesOf(p).forall(n => !allNames.contains(n)))
        bareTarget(result).filter(_ => clean).map { target =>
          val kind = if (clause.isEmpty) Continuation.Deterministic else Continuation.FunctionField
          Continuation(param, TypeName.of(target), kind, clause)
        }
      case Type.Apply.After_4_6_0(callee, Type.ArgClause(List(Type.Name(target))))
          if bareName(callee) == "Option" =>
        Some(Continuation(param, TypeName.of(target), Continuation.OptionBranch, Nil))
      case _ => None
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

  private[cardinality] def bareName(tpe: Type): String =
    Counter.nameOf(tpe).fold("")(_.value)

  private def namesOf(tpe: Type): Set[TypeName] =
    TypeSubstitution.typeNames(tpe).map(TypeName.of).toSet

}
