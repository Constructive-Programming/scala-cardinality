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
    * definitions they name where the constructor demands them (`names`) and those they name only
    * under a by-name type or a function's result (`deferred`: `p: (Boolean, () => X)`).
    */
  final case class Arm[K, S](
      continuations: List[Continuation[K, S]],
      rest: S => Size,
      names: Set[K],
      deferred: Set[K]
  )

  /** Each field's names `(demanded, deferred)`, joined: an arm's `names` and `deferred`. */
  def joined[K](held: List[(Set[K], Set[K])]): (Set[K], Set[K]) =
    (held.flatMap(_._1).toSet, held.flatMap(_._2).toSet)

  /** What the ν completion reads: each definition's constructors, and the sealed parents with their
    * children.
    */
  final case class Lazy[K, S](arms: Map[K, List[Arm[K, S]]], parents: Map[K, List[K]])

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
    // A definition between the cycle's nodes (the source front end's alias `type T = () => X`)
    // is read again from the values a lap reads the nodes at.
    val nodes = nu.arms.keySet ++ nu.parents.keySet
    val between = eqs.map(_._1).filter { k =>
      !nodes(k) && !fixed(k) && reaches.getOrElse(k, Set.empty).exists(nodes)
    }
    val read = iterate(eqs, cyclic, _: S, eqs.map(_._1).toSet -- between)
    val (cycles, closed, readable) = cycleGroups(nu, mu, read)
    val groups = cycles.filterNot(_.exists(fixed))
    val completed = groups.flatten.toSet
    // What each definition names where its constructor does not demand it (a strict `Option[X]`
    // demands it).
    val lazily = nu.arms.map { (owner, arms) =>
      owner -> arms.flatMap { arm =>
        arm.continuations.filter(_.kind != Continuation.Kind.OptionBranch).map(_.target) ++
          arm.deferred
      }.toSet
    }
    val incomplete =
      blocked(lazily, reaches, closed -- cycles.filter(_.exists(fixed)).flatten) -- fixed
    // Re-derive everything outside the completed cycles from their frozen values. Widening during
    // the iteration is a way of stopping it, not a result: a consumer widened only because a
    // cyclic dependency was still growing lands here on the value its own definition has, so a
    // definition and a reference to it always agree, and a chain of consumers carries the ν count
    // all the way down.
    val settled =
      if (groups.isEmpty) mu
      else iterate(eqs, cyclic, greatestFixpoint(readable, groups, mu, read), completed ++ fixed)
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
  // iterate above counts the finite constructor trees; a field the constructor never
  // demands lets a value unfold forever. A *lazy edge* is an occurrence of a defined name
  // the constructor does not demand: a continuation field (`=> X`, `() => X`, `D => X`, a
  // strict `Option[X]`) or a name under a by-name type or a function's result anywhere in
  // another field (`p: (Boolean, () => X)`, `Option[() => X]`). A name a field holds
  // otherwise is a *strict edge*, and a sealed parent is a node whose strict edges are its
  // children. A cycle is a strongly-connected group of the nodes that hold a value with a lazy
  // edge: a value is built in finitely many strict steps around lazy knots (`Kx(Nil, () =>
  // k)` for `Kx(kids: List[Kx], f: () => Kx)`), so a strict cycle inside it has a base. A node
  // that holds none keeps its μ value, 0 (`K9(k: K9, m: => K9)`).
  //
  // Each node's per-lap label space is what its continuing arms hold with every cycle node
  // read as one value (a parent holds nothing of its own). A cycle is deterministic when each
  // node has one continuing arm with one hole: its infinite part is the label space raised to
  // ω (the `Size.pow` finite-program reading): `νX.X` counts 1, an endless `Boolean` stream
  // ω, a `String`-valued one ε₀. Otherwise it branches, and every branching unfolding is
  // generated by a finite program, so the infinite part saturates at ω, unless the label
  // space itself reaches the infinite tier (a list's lengths, `List[() => X]`: ε₀), which
  // branching may not demote, and unless it is empty, in which case branching cannot conjure
  // values at all. Branching is read with every cycle node as two values: a field is linear in
  // the cycle where it doubles.
  //
  // Each completed cycle combines its μ value with the infinite contribution, then collapses an
  // infinite total to its highest tier. Finite totals stay unchanged. The label-space
  // environment is re-computed from the immutable μ base rather than accumulated — adding a
  // contribution per round would count solver rounds as coefficients. A cycle may consume
  // another cycle's ν, so the environment is refined until stable, within a budget that only
  // guards the loop.
  private def greatestFixpoint[K, S](
      nu: Lazy[K, S],
      groups: List[Set[K]],
      mu: S,
      read: S => S
  )(using Store[K, S]): S = {
    // Contributions are single-tier values, so a cycle can only move another cycle once;
    // the budget is a guard against a pathological dependency order, not a semantic.
    val budget = groups.size * 4 + 8

    @tailrec
    def refine(env: S, rounds: Int): S = {
      val next = groups.flatMap { group =>
        val infinite = infiniteOf(nu, read)(group, env)
        group.map(name => name -> completeCoinduction(mu.value(name), infinite))
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

  // A node's strict edges: the names its arms demand, or a sealed parent's children.
  private def strictEdges[K, S](ctx: Lazy[K, S], name: K): Set[K] =
    ctx.parents.get(name).fold(named(ctx, name)(_.names))(_.toSet)

  private def named[K, S](ctx: Lazy[K, S], name: K)(targets: Arm[K, S] => Set[K]): Set[K] =
    ctx.arms.getOrElse(name, Nil).flatMap(targets).toSet

  private def lazyTargets[K, S](arm: Arm[K, S]): Set[K] =
    arm.continuations.map(_.target).toSet ++ arm.deferred

  // The completed cycles, the nodes no lazy cycle leaves open (a completed one's, one on no lazy
  // cycle, one that holds no value), and the system as the cycles read it:
  // a node that holds a value keeps the arms that hold one, each naming only the nodes its space
  // depends on (`o: Option[(Nothing, Y)]` holds no `Y`). A cycle is a strongly-connected group of
  // the graph of every edge between the nodes that hold a value, with a lazy edge. A node read as
  // empty only because an arm names one of the nodes both strictly and lazily may hold one: a group
  // through it is not completed.
  private def cycleGroups[K, S](nu: Lazy[K, S], mu: S, read: S => S)(using
      Store[K, S]
  ): (List[Set[K]], Set[K], Lazy[K, S]) = {
    val nodes = nu.arms.keySet ++ nu.parents.keySet
    // Without a lazy edge nothing is completed, and no reading of the nodes is needed.
    val alive =
      if (nu.arms.values.exists(_.exists(lazyTargets(_).nonEmpty))) inhabited(nu, nodes, mu, read)
      else nodes
    val held = read(mu.withValues(nodes.map(k => k -> (if (alive(k)) UnitSize else NothingSize))))
    def holds(arm: Arm[K, S])(n: K) =
      alive(n) && arm.rest(read(held.withValues(List(n -> NothingSize)))) != arm.rest(held)
    val ctx = nu.copy(arms = nu.arms.map { (name, arms) =>
      name -> (if (!alive(name)) arms
               else
                 arms.collect {
                   case arm
                       if !arm.rest(held).isZero && arm.continuations
                         .forall(!_.size(held).isZero) =>
                     arm.copy(
                       names = arm.names.filter(holds(arm)),
                       deferred = arm.deferred.filter(holds(arm))
                     )
                 })
    })
    val unsure = (nodes -- alive).filter { name =>
      ctx.arms.getOrElse(name, Nil).exists(arm => arm.names.intersect(arm.deferred).nonEmpty)
    }
    val live = alive ++ unsure
    def lazyEdges(name: K) = named(ctx, name)(lazyTargets)
    def successors(name: K) = (strictEdges(ctx, name) ++ lazyEdges(name)).intersect(live)
    val reach = live.map(name => name -> closure(successors, successors(name), Set.empty)).toMap
    val (groups, open) = live.toList
      .map(name => live.filter(m => reach(name)(m) && reach(m)(name)))
      .distinct
      .filter(group => group.exists(lazyEdges(_).exists(group)))
      .partition(!_.exists(unsure))
    (groups, nodes -- open.flatten, ctx)
  }

  // The nodes that hold a value: one built in finitely many strict steps, each lazy edge tied to a
  // node that may hold one (a least fixed point of construction inside a greatest one of the
  // knots), or one with a finite value (μ).
  private def inhabited[K, S](ctx: Lazy[K, S], nodes: Set[K], mu: S, read: S => S)(using
      Store[K, S]
  ): Set[K] = {
    def at(holds: K => Boolean) =
      read(mu.withValues(nodes.map(k => k -> (if (holds(k)) UnitSize else NothingSize))))
    def lives(alive: Set[K], possible: Set[K])(name: K): Boolean =
      ctx.parents.get(name) match {
        case Some(children) => children.exists(alive)
        case None           =>
          // A continuation names its target alone; the rest names a node strictly or lazily.
          val tied = at(possible)
          ctx.arms(name).exists { arm =>
            val lazily = arm.deferred -- arm.names
            !arm.rest(at(k => alive(k) || lazily(k) && possible(k))).isZero &&
            arm.continuations.forall(!_.size(tied).isZero)
          }
      }
    @tailrec
    def grow(alive: Set[K], possible: Set[K]): Set[K] = {
      val next = alive ++ (nodes -- alive).filter(lives(alive, possible))
      if (next == alive) alive else grow(next, possible)
    }
    @tailrec
    def shrink(possible: Set[K]): Set[K] = {
      val alive = grow(nodes.filterNot(mu.value(_).isZero), possible)
      if (alive == possible) alive else shrink(alive)
    }
    shrink(nodes)
  }

  // The infinite part a cycle contributes at the given scope. A branch is a node with several
  // continuing arms or children in the cycle, or an arm with several holes or one that may hold
  // other than one value per hole: a strict `Option[X]`, an inhabited function field, a field
  // whose space is not linear in the cycle's (`Option[() => X]`, `(() => X, () => X)`).
  private def infiniteOf[K, S](ctx: Lazy[K, S], read: S => S)(group: Set[K], scope: S)(using
      Store[K, S]
  ): Size = {
    val one = read(scope.withValues(group.map(_ -> UnitSize)))
    val two = read(scope.withValues(group.map(_ -> BooleanSize)))
    // Each node's label space per lap, and whether it branches.
    val perNode = group.toList.map { name =>
      ctx.parents.get(name) match {
        case Some(children) => (UnitSize, children.count(group) >= 2)
        case None           =>
          val continuing = ctx.arms(name).filter { arm =>
            (lazyTargets(arm) ++ arm.names).exists(group) && !armSpace(one, group)(arm).isZero
          }
          (
            continuing.map(armSpace(one, group)).foldLeft(NothingSize: Size)(_ + _),
            continuing.size >= 2 || continuing.exists(branches(group, one, two))
          )
      }
    }
    val lap = perNode.map(_._1).foldLeft(UnitSize: Size)(_ * _)
    cycleContribution(branchy = perNode.exists(_._2), lap = lap)
  }

  private def branches[K, S](group: Set[K], one: S, two: S)(arm: Arm[K, S]): Boolean = {
    val edges = arm.continuations.filter(c => group(c.target))
    val held = (arm.names ++ arm.deferred).exists(group)
    edges.size + (if (held) 1 else 0) >= 2 ||
    edges.exists { edge =>
      edge.kind == Continuation.Kind.OptionBranch ||
      edge.kind == Continuation.Kind.FunctionField && edge.domain(one).larger(UnitSize)
    } ||
    held && arm.rest(two) != arm.rest(one) + arm.rest(one)
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

  // One arm's per-lap space at a scope that reads every cycle node as one (or two) values: its
  // other fields, those that hold the cycle included, its continuations outside the cycle, and,
  // for each one in it, its domain: the inputs the unfolding answers per node.
  private def armSpace[K, S](at: S, group: Set[K])(arm: Arm[K, S]): Size =
    arm.continuations.foldLeft(arm.rest(at)) { (s, edge) =>
      if (!group(edge.target)) s * edge.size(at)
      else s * edge.domain(at)
    }

}
