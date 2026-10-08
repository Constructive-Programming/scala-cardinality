package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.types.*

// The fixed-point machinery behind the counter: Kleene iteration with widening over the
// equations `Counter.equations` builds, and the coinductive completion of lazy cycles
// (docs/type-arithmetic.md §8). `Scope` and `Walk` drive it through `Solver.solve`.
private[cardinality] object Solver {

  // Solves the body's system of equations by Kleene iteration: every name starts at the
  // empty type and all equations are re-evaluated together (so the order of mutual and
  // forward references does not matter) until the map is stable. A stable value is the
  // least fixed point — finite types land there immediately, and degenerate recursion with
  // no base constructor (`case class Loop(next: Loop)`, i.e. `μX.X`) lands on `NothingSize`
  // rather than the old unknown-name `EffectiveOmega`.
  //
  // A recognized productive cycle never stabilises under coefficient-preserving addition:
  // the estimate for `Nat` climbs `1, 2, 3, …` for ever. Instead of re-evaluating those
  // names, the iteration widens them to the tier they have already grown into (`Size.widen`)
  // and leaves them out of later rounds, so whatever depends on them settles on a widened
  // value. Widening is the solver's own loss of precision; addition itself keeps every
  // coefficient, and a widening never demotes an ε₀ estimate to ω.
  //
  // The `budget` only accelerates mutual recursion, where two names take turns growing and
  // never grow in consecutive rounds: once it expires, a name that grew earlier is widened
  // on its next growth, while one growing for the first time (a long forward-reference chain
  // still settling) is left alone. Either branch shrinks the set of names that may still
  // grow, so the iteration terminates.
  private[cardinality] def solve(
      entries: List[(TypeName, Named)],
      stats: List[Stat],
      base: Scope
  ): Scope = {
    val eqs = entries.map { case (name, named) => name -> named.equation }
    val cyclic = cyclicNames(stats, eqs.map(_._1).toSet)
    val mu = iterate(eqs, cyclic, base, frozen = Set.empty)
    greatestFixpoint(eqs, stats, mu, cyclic)
  }

  // The widening iteration. `base` seeds every equation — except the frozen ones — with the
  // empty type, and a frozen name keeps the value `base` gives it instead of being
  // re-evaluated, which is how the caller re-derives consumers around a settled cycle.
  private def iterate(
      eqs: List[(TypeName, Scope => Size)],
      cyclic: Set[TypeName],
      base: Scope,
      frozen: Set[TypeName]
  ): Scope = {
    val seeded =
      base ++ eqs.collect {
        case (name, _) if !frozen(name) => name -> (NothingSize: Size)
      }

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
      if (grown.isEmpty) scope ++ evaluated
      else {
        val lastRounds = if (budget > 0) grewLastRound else grewEver
        val toWiden = grown.intersect(lastRounds).intersect(cyclic)
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
      budget = eqs.size + 8
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
  private def cyclicNames(stats: List[Stat], defined: Set[TypeName]): Set[TypeName] = {
    val mentions: Stat => Option[(TypeName, Set[TypeName])] = {
      case d: Defn.Type if Counter.isOpaqueAlias(d) => Some(Counter.nameOf(d) -> Set.empty)
      case d: Defn.Type                             => Some(Counter.nameOf(d) -> namesOf(d.body))
      case d: Defn.Enum                             =>
        Some(
          TypeName.of(d.name.value) -> d.templ.body.stats
            .flatMap {
              case c: Defn.EnumCase => paramTypes(c.ctor)
              case _                => Nil
            }
            .flatMap(namesOf)
            .toSet
        )
      case d: Defn.Class if Counter.isConcreteClass(d) =>
        Some(Counter.nameOf(d) -> paramTypes(d.ctor).flatMap(namesOf).toSet)
      case _ => None
    }
    val direct = stats.flatMap(mentions).toMap
    val children = Counter.sealedSums(stats, Counter.definedNames(stats))
    val deps = defined.map { name =>
      name -> (direct.getOrElse(name, Set.empty[TypeName]) ++ children.getOrElse(name, Nil))
    }.toMap
    val succ: TypeName => Set[TypeName] = name => deps.getOrElse(name, Set.empty)
    deps.keySet.filter(name => closure(succ, succ(name), Set.empty).contains(name))
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
      eqs: List[(TypeName, Scope => Size)],
      stats: List[Stat],
      mu: Scope,
      cyclic: Set[TypeName]
  ): Scope = {
    val ctx = nuContext(stats)
    val groups = cycleGroups(ctx)
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
      iterate(eqs, cyclic, settled, frozen)
    }
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
      arms: Map[TypeName, List[List[Type]]],
      parents: Map[TypeName, List[TypeName]],
      classified: Map[TypeName, List[ClassifiedArm]],
      holedChildren: Map[TypeName, Set[TypeName]]
  )

  private def nuContext(stats: List[Stat]): NuContext = {
    val arms = constructorArms(stats)
    val defined = Counter.definedNames(stats)
    val parents = Counter.sealedSums(stats, defined)
    val classified = classifyArms(arms, arms.keySet.union(parents.keySet))
    val holedChildren: Map[TypeName, Set[TypeName]] = parents.keySet.toList.map { parent =>
      parent -> classified.collect {
        case (name, memberArms) if memberArms.exists(_.continuations.exists(_.target == parent)) =>
          name
      }.toSet
    }.toMap
    NuContext(arms, parents, classified, holedChildren)
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
      members.toList.flatMap(name => continuingArms(ctx.classified(name), cyc, scope).toList)
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
    else if (branchy) branchContribution(lap)
    else lap.pow(EffectiveOmega)

  private def branchContribution(lap: Size): Size =
    if (lap.hasInfinite) EffectiveEpsilon0 else EffectiveOmega

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
              domainSpace(scope)(edge.domain).larger(UnitSize)
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
    val restSpace = domainSpace(scope)(arm.rest)
    arm.continuations.foldLeft(restSpace) { (s, edge) =>
      if (!cyc.contains(edge.target)) s * Counter.typeIn(scope)(edge.param)
      else s * domainSpace(scope)(edge.domain)
    }
  }

  private def domainSpace(scope: Scope)(domain: List[Type]): Size =
    domain.foldLeft(UnitSize: Size)((s, p) => s * Counter.typeIn(scope)(p))

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

  // The constructor arms of a definition: a class has one, an enum one per case, with
  // singleton cases contributing an empty arm. Only concrete definitions appear; sealed
  // parents are pass-through nodes, not arms of their own.
  private def constructorArms(stats: List[Stat]): Map[TypeName, List[List[Type]]] =
    stats.collect {
      case d: Defn.Class if Counter.isConcreteClass(d) =>
        Counter.nameOf(d) -> List(paramTypes(d.ctor))
      case d: Defn.Enum =>
        Counter.nameOf(d) -> d.templ.body.stats.collect {
          case c: Defn.EnumCase         => paramTypes(c.ctor)
          case _: Defn.RepeatedEnumCase => Nil
        }
    }.toMap

  private def paramTypes: Ctor.Primary => List[Type] =
    _.paramClauses.flatMap(_.values).flatMap(_.decltpe).toList

  // The single name an occurrence targets: `X` itself, or `Option[X]`. Uses the current
  // `Apply` shape (`After_4_6_0`) — the plain unapply silently fails to match it.
  private def bareTarget(tpe: Type): Option[String] = tpe match {
    case Type.Name(name) => Some(name)
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(List(Type.Name(name))))
        if bareName(callee) == "Option" =>
      Some(name)
    case _ => None
  }

  private[cardinality] def bareName: Type => String = {
    case Type.Name(n)                 => n
    case Type.Select(_, Type.Name(n)) => n
    case _                            => ""
  }

  // The parent type of an `extends` clause: `Init`'s *name* field is the anonymous
  // method-name slot, not the parent — the type is the first `Init` field. `extends S`,
  // `extends S(1)` and `extends a.b.S` all yield `S`; anything shaped differently is
  // ignored (""), matching no local name.
  private[cardinality] def initParent: Init => String = init => bareName(init.tpe)

  private def namesOf(tpe: Type): Set[TypeName] =
    tpe.collect { case Type.Name(n) => TypeName.of(n) }.toSet

}
