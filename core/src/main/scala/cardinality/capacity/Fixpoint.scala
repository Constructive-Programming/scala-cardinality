package cardinality.capacity

import scala.annotation.tailrec

// The fixed point a system of stored-size equations is solved by: Kleene iteration with widening,
// and the coinductive completion of lazy cycles (docs/site/arithmetic/counting-data.md §8). It knows definitions
// only as keys `K` and their values only through a store `S`; what a definition is, and how its
// fields are classified, is `compiled.Stored`'s (its recursion) over the compiled trees.

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
    * the system (`target`): its size at given values of the definitions (off the cycle, or held
    * beside another hole), and its function domain.
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

  /** Solves a system of equations whose dependency graph is read (`reaches`), from the store
    * `empty`: the least fixed point, then the ν completion of the lazy cycles; the definitions left
    * on a lazy cycle it did not complete are blocked.
    */
  def solve[K, S](eqs: List[(K, S => Size)], empty: S, nu: Lazy[K, S], reaches: Map[K, Set[K]])(
      using Store[K, S]
  ): S = {
    val cyclic = reaches.keySet.filter(name => reaches(name)(name))
    val mu = iterate(eqs, cyclic, empty, frozen = Set.empty)
    // A definition between the cycle's nodes (the source front end's alias `type T = () => X`)
    // is read again from the values a lap reads the nodes at.
    val nodes = nu.arms.keySet ++ nu.parents.keySet
    val between = eqs.map(_._1).filter { k =>
      !nodes(k) && reaches.getOrElse(k, Set.empty).exists(nodes)
    }
    val read = iterate(eqs, cyclic, _: S, eqs.map(_._1).toSet -- between)
    val (groups, closed, readable) = cycleGroups(nu, mu, read)
    val completed = groups.flatten.toSet
    // What each definition names where its constructor does not demand it (a strict `Option[X]`
    // demands it).
    val lazily = nu.arms.map { (owner, arms) =>
      owner -> arms.flatMap { arm =>
        arm.continuations.filter(_.kind != Continuation.Kind.OptionBranch).map(_.target) ++
          arm.deferred
      }.toSet
    }
    val incomplete = blocked(lazily, reaches, closed)
    // Re-derive everything outside the completed cycles from their frozen values. Widening during
    // the iteration is a way of stopping it, not a result: a consumer widened only because a
    // cyclic dependency was still growing lands here on the value its own definition has, so a
    // definition and a reference to it always agree, and a chain of consumers carries the ν count
    // all the way down.
    // `around(group)` keeps the other completed cycles and re-derives the rest, the group's own
    // nodes from the empty type.
    val around = (group: Set[K]) => iterate(eqs, cyclic, _: S, completed -- group)
    val settled =
      if (groups.isEmpty) mu
      else around(Set.empty)(greatestFixpoint(readable, groups, mu, read, around))
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

  // The greatest fixed point of lazy recursion (docs/site/arithmetic/counting-data.md §8): the μ
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
  // Each cycle's infinite part is its per-lap label space raised to ω (the `Size.pow`
  // finite-program reading): `νX.X` counts 1, an endless `Boolean` stream ω, a `String`-valued one
  // or a list's lengths (`List[() => X]`) ε₀, and an empty label space 0. What a lap may choose is
  // read in `infiniteOf`.
  //
  // Each completed cycle combines its finite value with the infinite contribution, then collapses
  // an infinite total to its highest tier. Finite totals stay unchanged. Each round a cycle's
  // finite value and label space are re-derived (`around`: the other completed cycles keep their
  // values, everything else is re-derived from the empty type) rather than accumulated —
  // adding a contribution per round would count solver rounds as coefficients. A cycle may
  // consume another cycle's ν, even through a sum's arm or a definition outside both (`Pt = Pa +
  // Pb` with `Pb` an endless stream: μ 0, ν ω), so the environment is refined until stable,
  // within a budget that only guards the loop.
  private def greatestFixpoint[K, S](
      nu: Lazy[K, S],
      groups: List[Set[K]],
      mu: S,
      read: S => S,
      around: Set[K] => S => S
  )(using Store[K, S]): S = {
    // Cycles consume each other without a loop (a loop would join them), so a round settles at
    // least one more level of that order: the budget is a guard, not a semantic.
    val budget = groups.size * 4 + 8

    @tailrec
    def refine(env: S, rounds: Int): S = {
      val next = groups.flatMap { group =>
        val scope = around(group)(env)
        val infinite = infiniteOf(nu, read, group, scope)
        group.map(name => name -> completeCoinduction(scope.value(name), infinite))
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
    // An arm holds a node when its fields change with it: read at no value and at one, at a width
    // above every constant (a sum rounds to its widest summand: `Either[Int, Option[X]]`) and at ε₀
    // (an infinite factor absorbs a finite change). Under an empty factor nothing changes; a name all
    // three miss sits beside an infinite term of the fields at one, where the lap is infinite anyway.
    def holds(arm: Arm[K, S])(n: K) = alive(n) && {
      def at(size: Size) = arm.rest(read(held.withValues(List(n -> size))))
      val wide = (BigInt(1) << 40).max(arm.rest(held).finite.bits * 4)
      List(UnitSize, FiniteSize(wide), EffectiveEpsilon0).exists(at(_) != at(NothingSize))
    }
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

  // The infinite part a cycle contributes at the given scope: its per-lap label space raised to ω
  // (the `Size.pow` finite-program reading). A node's label space is the sum of its continuing
  // arms' labels, a parent's the number of its children in the cycle (which one the lap passes
  // through). An arm with one hole reads every cycle node as one value: taking a value that leaves
  // the cycle there ends the unfolding, which the finite part counts (`Pa(t: => Pt)` beside `Pb
  // extends Pt`). An arm with two holes or more continues through one of them while the others
  // hold any value of their node, finite or not, its own cycle's included: a label like any other
  // (`Bq2(t: => Bq2, b: Bq)`, `b` any `Bq`: ε₀; `Rb(b: Boolean, l: => Rb, r: => Rb)`, `r` any `Rb`:
  // ε₀). The node's values grow with the infinite part, so the two are read together until they
  // settle. Holes alone are no choice: `Tt(l: => Tt, r: => Tt)` and `Nf(f: Int => Nf)` hold one
  // value, νX.X·X = 1.
  private def infiniteOf[K, S](ctx: Lazy[K, S], read: S => S, group: Set[K], scope: S)(using
      Store[K, S]
  ): Size = {
    def at(value: K => Size) = read(scope.withValues(group.map(k => k -> value(k))))
    val one = at(_ => UnitSize)
    def label(held: S, manyHoles: Boolean, arm: Arm[K, S]): Size = {
      val in = if (manyHoles) held else one
      arm.continuations.foldLeft(arm.rest(in)) { (s, edge) =>
        s * (if (group(edge.target) && !manyHoles) edge.domain(in) else edge.size(in))
      }
    }
    def lap(held: S): Size =
      group.toList
        .map { name =>
          ctx.parents.get(name) match {
            case Some(children) => TinySize(children.count(group).toByte)
            case None           =>
              ctx
                .arms(name)
                .filter(arm => (lazyTargets(arm) ++ arm.names).exists(group))
                .map(arm => label(held, manyHoles(arm, group, at, one), arm))
                .foldLeft(NothingSize: Size)(_ + _)
          }
        }
        .foldLeft(UnitSize: Size)(_ * _)
    // Labels only grow with the values held, and the tiers stop at ε₀: a few rounds settle it.
    @tailrec
    def settle(infinite: Size, rounds: Int): Size = {
      val l = lap(at(k => Size.order.max(UnitSize, scope.value(k) + infinite)))
      val next = if (l.isZero) NothingSize else l.pow(EffectiveOmega)
      if (next == infinite || rounds <= 0) next else settle(next, rounds - 1)
    }
    settle(NothingSize, 4)
  }

  // Whether an arm of the cycle `group` (read by `at`, `one` at every node one value) has two
  // holes or more: its continuations into the cycle (a function field over two inputs or more
  // being several) and its fields' degree in the cycle. The degree is read from how the fields'
  // width grows with the cycle's at two widths far above its constants: by that width once per
  // hole (`(Int, () => X)`: one; `Option[() => X]`: one; `(() => X, () => X)`: two), and past the
  // finite tier for an exponent (`String => X`).
  private def manyHoles[K, S](
      arm: Arm[K, S],
      group: Set[K],
      at: (K => Size) => S,
      one: S
  ): Boolean = {
    val edges = arm.continuations
      .filter(c => group(c.target))
      .map { edge =>
        if (edge.kind == Continuation.Kind.FunctionField && edge.domain(one).larger(UnitSize)) 2
        else 1
      }
      .sum
    def width(bits: BigInt) = arm.rest(at(_ => FiniteSize(bits)))
    // Wider than every constant of the fields (each at most their width at one: a sum rounds to
    // its widest summand, `Either[Long => Boolean, X]`), so the holes' growth shows.
    val wide = (BigInt(1) << 40).max(arm.rest(one).finite.bits * 4)
    val (narrow, broad) = (width(wide), width(wide * 2))
    val degree =
      if (!(arm.names ++ arm.deferred).exists(group)) BigInt(0)
      else if (narrow.hasInfinite || broad.hasInfinite) BigInt(2)
      else ((broad.finite.bits - narrow.finite.bits) * 2 + wide) / (wide * 2) // rounded
    degree + edges >= 2
  }

}
