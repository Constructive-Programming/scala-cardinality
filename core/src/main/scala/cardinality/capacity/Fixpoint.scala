package cardinality.capacity

import scala.annotation.tailrec

// The fixed point both front ends solve a system of stored-size equations by: Kleene iteration
// with widening, and the coinductive completion of lazy cycles (docs/type-arithmetic.md §8). It
// knows definitions only as keys `K` and their values only through a store `S`; what a definition
// is, and how its fields are classified, is each front end's: `Solver` over the sources' syntax,
// `compiled.Stored` (its recursion) over the compiled trees.

/** Where a system's values are kept: read, written, and the definitions marked blocked. */
private[cardinality] trait Store[K, S] {

  extension (store: S) {
    def value(name: K): Size
    def withValues(values: Iterable[(K, Size)]): S
    def withBlocked(blocked: Iterable[(K, Blocked)]): S
  }

}

private[cardinality] object Fixpoint {

  /** A field a constructor never demands on the way to building its value, naming a definition of
    * the system (`target`): its size where it does not continue the cycle, and its function domain.
    */
  final case class Continuation[K, S](
      target: K,
      kind: Continuation.Kind,
      size: S => Size,
      domain: S => Size
  )

  object Continuation {

    enum Kind {
      case Deterministic // `=> X`, `=> Option[X]`, `() => X`
      case OptionBranch // strict `Option[X]`: Some(tower) beside the hole
      case FunctionField // `D => X`: one continuation per input of D
    }

  }

  /** One constructor of a definition: its continuations, and the rest of its fields with the
    * definitions they name.
    */
  final case class Arm[K, S](
      continuations: List[Continuation[K, S]],
      rest: S => Size,
      names: Set[K]
  )

  /** What the ν completion reads: each definition's constructors, the sealed parents with their
    * children, and the definitions each one names lazily (a by-name field or a function's result).
    */
  final case class Lazy[K, S](
      arms: Map[K, List[Arm[K, S]]],
      parents: Map[K, List[K]],
      lazily: Map[K, Set[K]]
  ) {

    // Each parent's children with a constructor that continues through it.
    lazy val holedChildren: Map[K, Set[K]] = parents.keySet.map { parent =>
      parent -> arms.collect {
        case (name, memberArms) if memberArms.exists(_.continuations.exists(_.target == parent)) =>
          name
      }.toSet
    }.toMap

  }

  /** Solves a system of equations whose dependency graph is read (`reaches`) and whose definitions
    * on a cycle through a non-positive position (`fixed`) hold a fixed value in `marked`: the least
    * fixed point, then the ν completion of the lazy cycles; the definitions left on a lazy cycle it
    * did not complete are blocked.
    */
  def solve[K, S](
      eqs: List[(K, S => Size)],
      fixed: Set[K],
      marked: S,
      nu: Lazy[K, S],
      reaches: Map[K, Set[K]]
  )(using Store[K, S]): S = {
    val cyclic = reaches.keySet.filter(name => reaches(name)(name))
    val mu = iterate(eqs, cyclic, marked, frozen = fixed)
    val groups = cycleGroups(nu).filterNot((members, _) => members.exists(fixed))
    val completed = groups.flatMap { case (members, attached) => members.union(attached) }.toSet
    val incomplete = blocked(nu.lazily, reaches, completed) -- fixed
    // Re-derive everything outside the completed cycles from their frozen values. Widening during
    // the iteration is a way of stopping it, not a result: a consumer widened only because a
    // cyclic dependency was still growing lands here on the value its own definition has, so a
    // definition and a reference to it always agree, and a chain of consumers carries the ν count
    // all the way down.
    val settled =
      if (groups.isEmpty) mu
      else iterate(eqs, cyclic, greatestFixpoint(nu, groups, mu), completed ++ fixed)
    settled.withBlocked(incomplete.map(_ -> Blocked.Lazy))
  }

  /** What each definition reaches through the dependency graph. */
  def reaches[K](deps: Map[K, Set[K]]): Map[K, Set[K]] = {
    val succ: K => Set[K] = n => deps.getOrElse(n, Set.empty)
    deps.keySet.map(name => name -> closure(succ, succ(name), Set.empty)).toMap
  }

  // The widening iteration. `base` seeds every equation — except the frozen ones — with the
  // empty type, and a frozen name keeps the value `base` gives it instead of being
  // re-evaluated, which is how the caller re-derives consumers around a settled cycle. A name the
  // budget guard widens (below) is on a cycle neither the graph nor positivity saw: blocked, so
  // every read of it is a reason, never the number.
  private def iterate[K, S](eqs: List[(K, S => Size)], cyclic: Set[K], base: S, frozen: Set[K])(
      using Store[K, S]
  ): S = {
    val seeded =
      base.withValues(eqs.collect {
        case (name, _) if !frozen(name) => name -> (NothingSize: Size)
      })
    val rounds = eqs.size + 8

    @tailrec
    def step(
        scope: S,
        grewLastRound: Set[K],
        grewEver: Set[K],
        widened: Set[K],
        budget: Int
    ): S = {
      val evaluated = eqs.collect {
        case (name, equation) if !frozen(name) && !widened(name) => name -> equation(scope)
      }.toMap
      val grown = evaluated.collect { case (name, size) if scope.value(name) != size => name }.toSet
      if (grown.isEmpty)
        scope.withValues(evaluated).withBlocked((widened -- cyclic).map(_ -> Blocked.Unseen))
      else {
        val lastRounds = if (budget > 0) grewLastRound else grewEver
        // An acyclic chain settles within as many rounds as it has links once its inputs settle,
        // so a name still growing a whole budget after the first one ran out is on a cycle the
        // graph could not see, and takes a cycle's widening: a termination guard (no fixture
        // reaches it since a member read off a value is a dependency, `capacity.Shapes.reads`).
        val recursive = if (budget > -rounds) cyclic else grown
        val toWiden = grown.intersect(lastRounds).intersect(recursive)
        step(
          scope = scope.withValues(evaluated ++ toWiden.map(name => name -> evaluated(name).widen)),
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
  private def closure[K](succ: K => Set[K], frontier: Set[K], seen: Set[K]): Set[K] = {
    val next = frontier.flatMap(succ).diff(seen)
    if (next.isEmpty) seen else closure(succ, next, seen.union(next))
  }

  // A lazy cycle: a by-name or function field (`tail: => X`, `f: D => X`) whose type names a
  // definition that reaches back to the field's own definition. A value of the cycle can be tied
  // by a lazy knot (`lazy val c: Cell = Cell(S(true, c))`), which only the ν completion counts;
  // when it did not complete the cycle, every definition on it is blocked.
  private def blocked[K](
      lazily: Map[K, Set[K]],
      reaches: Map[K, Set[K]],
      completed: Set[K]
  ): Set[K] =
    lazily.toList.flatMap { (owner, targets) =>
      def back(m: K): Boolean = m == owner || reaches.get(m).exists(_(owner))
      if (!targets.exists(back) || completed(owner)) Nil
      else owner :: reaches.getOrElse(owner, Set.empty).filter(back).toList
    }.toSet

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
  private def greatestFixpoint[K, S](nu: Lazy[K, S], groups: List[(Set[K], Set[K])], mu: S)(using
      Store[K, S]
  ): S = {
    // Contributions are single-tier values, so a cycle can only move another cycle once;
    // the budget is a guard against a pathological dependency order, not a semantic.
    val budget = groups.size * 4 + 8

    @tailrec
    def refine(env: S, rounds: Int): S = {
      val contributions = groups.foldLeft(Map.empty[K, Size]) {
        case (acc, (members, attached)) =>
          infiniteOf(nu)(members, attached, env).fold(acc) { infinite =>
            members.union(attached).foldLeft(acc) { (a, name) =>
              a.updated(name, a.getOrElse(name, NothingSize) + infinite)
            }
          }
      }
      val next = contributions.map { (name, infinite) =>
        name -> completeCoinduction(mu.value(name), infinite)
      }
      if (next.forall((name, size) => env.value(name) == size) || rounds <= 0)
        env.withValues(next)
      else refine(env.withValues(next), rounds - 1)
    }

    refine(mu, budget)
  }

  // Collapse the completed lazy type, not an enclosing sum or source/signature total. Guard
  // widening with hasInfinite: finite results, including zero and one, must remain finite.
  // Both recursive definitions and built-in lazy collections use this boundary.
  private[cardinality] def completeCoinduction(finite: Size, infinite: Size): Size = {
    val total = finite + infinite
    if (total.hasInfinite) total.widen else total
  }

  // Strongly-connected groups of the continuation graph that actually cycle (a self edge
  // counts), paired with the sealed parents they pass through. A parent leading to a
  // child outside the group breaks the cycle's determinism in a way the label space
  // cannot express: drop the group.
  private def cycleGroups[K, S](ctx: Lazy[K, S]): List[(Set[K], Set[K])] = {
    def successors(name: K): Set[K] =
      ctx
        .arms(name)
        .flatMap(_.continuations)
        .flatMap { edge =>
          if (ctx.arms.contains(edge.target)) Some(edge.target)
          else ctx.holedChildren.getOrElse(edge.target, Set.empty[K])
        }
        .toSet

    val reach = ctx.arms.keySet
      .map(name => name -> closure(successors, Set(name), Set(name)))
      .toMap
    ctx.arms.keySet.toList
      .map(name => ctx.arms.keySet.filter(m => reach(name).contains(m) && reach(m).contains(name)))
      .filter(group => group.exists(name => successors(name).intersect(group).nonEmpty))
      .distinct
      .flatMap { group =>
        val targets =
          group.flatMap(name => ctx.arms(name).flatMap(_.continuations).map(_.target))
        val attached = targets.filter(ctx.parents.contains)
        if (attached.exists(parent => !group.subsetOf(ctx.holedChildren(parent)))) None
        else Some((group, attached))
      }
      .distinct
  }

  // The infinite part a strongly-connected group contributes at the given scope, or None
  // when it is blocked: some member has no continuing arm, or a continuing arm demands
  // the cycle outright. A branch is two continuations in one arm, a strict `Option[X]` field, an
  // inhabited function field, or a sealed parent with several continuing children.
  private def infiniteOf[K, S](ctx: Lazy[K, S])(
      members: Set[K],
      attached: Set[K],
      scope: S
  ): Option[Size] = {
    val cyc = members.union(attached)
    val perMember =
      members.toList.flatMap(name => continuingArms(ctx.arms(name), cyc, scope).toList)
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
  private def continuingArms[K, S](
      memberArms: List[Arm[K, S]],
      cyc: Set[K],
      scope: S
  ): Option[(Size, Boolean)] = {
    val continuing = memberArms.filter(_.continuations.exists(c => cyc.contains(c.target)))
    if (continuing.isEmpty || continuing.exists(_.names.exists(cyc.contains))) None
    else
      Some(
        (
          continuing.map(armSpace(scope, cyc)).foldLeft(NothingSize: Size)(_ + _),
          continuing.size >= 2 || continuing.exists { arm =>
            val cycleEdges = arm.continuations.filter(c => cyc.contains(c.target))
            cycleEdges.size >= 2 || cycleEdges.exists { edge =>
              edge.kind == Continuation.Kind.OptionBranch ||
              edge.kind == Continuation.Kind.FunctionField && edge.domain(scope).larger(UnitSize)
            }
          }
        )
      )
  }

  // One arm's per-lap space: the product of its ordinary parameters — external
  // continuations included, since they evaluate to a settled size — times, for each
  // cycle-targeting function field, its domain: the space of inputs the unfolding answers
  // per node.
  private def armSpace[K, S](scope: S, cyc: Set[K])(arm: Arm[K, S]): Size =
    arm.continuations.foldLeft(arm.rest(scope)) { (s, edge) =>
      if (!cyc.contains(edge.target)) s * edge.size(scope)
      else s * edge.domain(scope)
    }

}
