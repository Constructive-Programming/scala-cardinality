import scala.annotation.tailrec
import scala.meta.*

object Counter {
  def source: Source => Size = s => body(s.stats, Scope.empty)

  /** What the definitions of a source declare: every typed field, term and method contributes the
    * size of its declared type, and the contributions are added up rather than multiplied, so an
    * object with two `String => String` methods and a `String` field reports `2ε₀ + ω`. See
    * `signatureIn` for what counts as a member.
    */
  def sourceSignature: Source => Size = s => signatureBody(s.stats, Scope.empty)

  def stat: Stat => Size = statIn(Scope.empty)

  /** The member signature of a single definition, summed the same way as `sourceSignature`.
    */
  def defnSignature: Defn => Size = signatureIn(Scope.empty)

  def defn: Defn => Size = defnIn(Scope.empty)

  def ctor: Ctor.Primary => Size = ctorIn(Scope.empty)

  def param: Term.Param => Size = paramIn(Scope.empty)

  def `type`: Type => Size = typeIn(Scope.empty)

  // The names of the types a source defines, each mapped to its cardinality, so that a
  // field can refer to a sibling, forward or recursive definition by name. The system of
  // equations is solved by Kleene iteration from the empty type — see `solve`.
  private type Scope = Map[String, Size]

  private object Scope {
    val empty: Scope = Map.empty
  }

  private def body(stats: List[Stat], scope: Scope): Size = {
    val solved = solve(equations(stats), stats, scope)
    stats.foldLeft(NothingSize: Size)((acc, st) => acc + statIn(solved)(st))
  }

  // The equations a body defines: one per named type, plus one per sealed parent the body
  // provides subtypes for. Each equation recomputes its cardinality from a scope, so the
  // system can be iterated as a whole. Abstract traits and classes have no cardinality of
  // their own and get an equation only when their subtypes appear in the same body.
  private def equations(stats: List[Stat]): List[(String, Scope => Size)] = {
    val defined = stats.flatMap {
      case d: Defn.Type if d.mods.exists(_.is[Mod.Opaque]) =>
        Some(d.name.value -> ((_: Scope) => UnitSize))
      case d: Defn.Type => Some(d.name.value -> ((sc: Scope) => typeIn(sc)(d.body)))
      case d: Defn.Enum => Some(d.name.value -> ((sc: Scope) => enumSize(sc)(d)))
      case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) =>
        Some(d.name.value -> ((sc: Scope) => ctorIn(sc)(d.ctor)))
      case d: Defn.Object => Some(d.name.value -> ((_: Scope) => UnitSize))
      case _              => None
    }
    defined ++ sealedSums(stats, defined.map(_._1).toSet).toList.map {
      case (parent, children) =>
        parent -> ((sc: Scope) =>
          children.foldLeft(NothingSize: Size)((acc, child) =>
            acc + sc.getOrElse(child, NothingSize)
          )
        )
    }
  }

  // A sealed trait or sealed abstract class stands for the sum of the body's subtypes that
  // extend it, so a recursive reference through the parent (`Succ(n: Nat)`) resolves. The
  // sum is complete only when every local subtype is concrete: an abstract or trait subtype
  // (or subtypes defined elsewhere) leaves the parent unknown, which keeps the old
  // `EffectiveOmega` fallback instead of claiming a too-small sum. Cross-file sealed
  // hierarchies are future work.
  private def sealedSums(stats: List[Stat], defined: Set[String]): Map[String, List[String]] = {
    val sealedNames: Set[String] = stats
      .collect {
        case d: Defn.Trait if d.mods.exists(_.is[Mod.Sealed]) => d.name.value
        case d: Defn.Class
            if d.mods.exists(_.is[Mod.Sealed]) && d.mods.exists(_.is[Mod.Abstract]) =>
          d.name.value
      }
      .toSet
      .diff(defined)
    val subtypes = stats.collect {
      case d: Defn.Class  => (d.name.value, d.templ.inits.map(initParent))
      case d: Defn.Object => (d.name.value, d.templ.inits.map(initParent))
      case d: Defn.Enum   => (d.name.value, d.templ.inits.map(initParent))
    }
    sealedNames.iterator
      .map(parent =>
        parent -> subtypes.collect { case (child, parents) if parents.contains(parent) => child }
      )
      .filter { case (_, children) => children.nonEmpty && children.forall(defined.contains) }
      .toMap
  }

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
  private def solve(
      eqs: List[(String, Scope => Size)],
      stats: List[Stat],
      base: Scope
  ): Scope = {
    val cyclic = cyclicNames(stats, eqs.map(_._1).toSet)
    val mu = iterate(eqs, cyclic, base, frozen = Set.empty)
    greatestFixpoint(eqs, stats, mu, cyclic)
  }

  // The widening iteration. `base` seeds every equation — except the frozen ones — with the
  // empty type, and a frozen name keeps the value `base` gives it instead of being
  // re-evaluated, which is how the caller re-derives consumers around a settled cycle.
  private def iterate(
      eqs: List[(String, Scope => Size)],
      cyclic: Set[String],
      base: Scope,
      frozen: Set[String]
  ): Scope = {
    val seeded =
      base ++ eqs.collect {
        case (name, _) if !frozen(name) => name -> (NothingSize: Size)
      }

    @tailrec
    def step(
        scope: Scope,
        grewLastRound: Set[String],
        grewEver: Set[String],
        widened: Set[String],
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

  // The dependency graph of the body equations: every name a definition mentions anywhere in
  // its syntax, plus the children of a sealed parent (whose value is their sum). Only a name
  // that can reach itself may be widened, so an acyclic chain of forward references settles
  // exactly, however many rounds it takes.
  private def cyclicNames(stats: List[Stat], defined: Set[String]): Set[String] = {
    val mentions: Stat => Option[(String, Set[String])] = {
      case d: Defn.Type if d.mods.exists(_.is[Mod.Opaque]) => Some(d.name.value -> Set.empty)
      case d: Defn.Type                                    => Some(d.name.value -> namesOf(d.body))
      case d: Defn.Enum                                    =>
        Some(
          d.name.value -> d.templ.body.stats
            .flatMap {
              case c: Defn.EnumCase => paramTypes(c.ctor)
              case _                => Nil
            }
            .flatMap(namesOf)
            .toSet
        )
      case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) =>
        Some(d.name.value -> paramTypes(d.ctor).flatMap(namesOf).toSet)
      case _ => None
    }
    val concrete = stats.collect {
      case d: Defn.Type                                        => d.name.value
      case d: Defn.Enum                                        => d.name.value
      case d: Defn.Object                                      => d.name.value
      case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) => d.name.value
    }.toSet
    val direct = stats.flatMap(mentions).toMap
    val children = sealedSums(stats, concrete)
    val deps = defined.map { name =>
      name -> (direct.getOrElse(name, Set.empty[String]) ++ children.getOrElse(name, Nil))
    }.toMap
    @tailrec
    def reach(frontier: Set[String], seen: Set[String]): Set[String] = {
      val next = frontier.flatMap(deps.getOrElse(_, Set.empty)).diff(seen)
      if (next.isEmpty) seen else reach(next, seen.union(next))
    }

    deps.keySet.filter(name => reach(deps.getOrElse(name, Set.empty), Set.empty).contains(name))
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
      eqs: List[(String, Scope => Size)],
      stats: List[Stat],
      mu: Scope,
      cyclic: Set[String]
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
        val contributions = groups.foldLeft(Map.empty[String, Size]) {
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
  private def completeCoinduction(finite: Size, infinite: Size): Size = {
    val total = finite + infinite
    if (total.hasInfinite) total.widen else total
  }

  // The solver's fixed context: constructor arms, the sealed parents they pass through,
  // the arms classified into continuations, and the resulting parent -> holed children
  // map.
  final private case class NuContext(
      arms: Map[String, List[List[Type]]],
      parents: Map[String, List[String]],
      classified: Map[String, List[ClassifiedArm]],
      holedChildren: Map[String, Set[String]]
  )

  private def nuContext(stats: List[Stat]): NuContext = {
    val arms = constructorArms(stats)
    // Names with an equation of their own (a sealed parent does not — its equation is
    // exactly the sum sealedSums computes), matching the guard `equations` uses so a
    // pass-through parent is never diffed away by its own name.
    val defined = stats.collect {
      case d: Defn.Type                                        => d.name.value
      case d: Defn.Enum                                        => d.name.value
      case d: Defn.Object                                      => d.name.value
      case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) => d.name.value
    }.toSet
    val parents = sealedSums(stats, defined)
    val classified = classifyArms(arms, arms.keySet.union(parents.keySet))
    val holedChildren: Map[String, Set[String]] = parents.keySet.toList.map { parent =>
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
  private def cycleGroups(ctx: NuContext): List[(Set[String], Set[String])] = {
    def successors(name: String): Set[String] =
      ctx
        .classified(name)
        .flatMap(_.continuations)
        .flatMap { edge =>
          if (ctx.arms.contains(edge.target)) Some(edge.target)
          else ctx.holedChildren.getOrElse(edge.target, Set.empty)
        }
        .toSet

    @tailrec
    def close(frontier: Set[String], seen: Set[String]): Set[String] = {
      val next = frontier.flatMap(successors).diff(seen)
      if (next.isEmpty) seen else close(next, seen.union(next))
    }

    val reach = ctx.arms.keySet.map(name => name -> close(Set(name), Set(name))).toMap
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
      members: Set[String],
      attached: Set[String],
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
      cyc: Set[String],
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
  private def armSpace(scope: Scope, cyc: Set[String])(arm: ClassifiedArm): Size = {
    val restSpace = arm.rest.foldLeft(UnitSize: Size)((s, p) => s * typeIn(scope)(p))
    arm.continuations.foldLeft(restSpace) { (s, edge) =>
      if (!cyc.contains(edge.target)) s * typeIn(scope)(edge.param)
      else s * domainSpace(scope)(edge.domain)
    }
  }

  private def domainSpace(scope: Scope)(domain: List[Type]): Size =
    domain.foldLeft(UnitSize: Size)((s, p) => s * typeIn(scope)(p))

  final private case class Continuation(
      param: Type,
      target: String,
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
      arms: Map[String, List[List[Type]]],
      allNames: Set[String]
  ): Map[String, List[ClassifiedArm]] =
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
  private def continuation(param: Type, allNames: Set[String]): Option[Continuation] = param match {
    case Type.ByName(inner) =>
      bareTarget(inner).map(target => Continuation(param, target, Continuation.Deterministic, Nil))
    case Type.Function.After_4_6_0(Type.FuncParamClause(clause), result) =>
      val clean = clause.forall(p => namesOf(p).forall(n => !allNames.contains(n)))
      bareTarget(result).filter(_ => clean).map { target =>
        val kind = if (clause.isEmpty) Continuation.Deterministic else Continuation.FunctionField
        Continuation(param, target, kind, clause)
      }
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(List(Type.Name(target))))
        if bareName(callee) == "Option" =>
      Some(Continuation(param, target, Continuation.OptionBranch, Nil))
    case _ => None
  }

  // The constructor arms of a definition: a class has one, an enum one per case, with
  // singleton cases contributing an empty arm. Only concrete definitions appear; sealed
  // parents are pass-through nodes, not arms of their own.
  private def constructorArms(stats: List[Stat]): Map[String, List[List[Type]]] =
    stats.collect {
      case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) =>
        d.name.value -> List(paramTypes(d.ctor))
      case d: Defn.Enum =>
        d.name.value -> d.templ.body.stats.collect {
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

  private def bareName: Type => String = {
    case Type.Name(n)                 => n
    case Type.Select(_, Type.Name(n)) => n
    case _                            => ""
  }

  // The parent type of an `extends` clause: `Init`'s *name* field is the anonymous
  // method-name slot, not the parent — the type is the first `Init` field. `extends S`,
  // `extends S(1)` and `extends a.b.S` all yield `S`; anything shaped differently is
  // ignored (""), matching no local name.
  private def initParent: Init => String = init => bareName(init.tpe)

  // The per-lap label space of a cycle: every member contributes the sum over its hole
  // arms of the product of that arm's non-hole parameters, all at the finite (μ) counts.
  // Members with a single holed arm (the common `case class St(h: Boolean, t: => St)`
  // shape) contribute just that arm's product.

  private def namesOf(tpe: Type): Set[String] =
    tpe.collect { case Type.Name(n) => n }.toSet

  // A statement's contribution to a source total. A concrete class, enum or object that the
  // solver gave an equation contributes that solved value rather than a fresh evaluation of
  // the same syntax: with coefficient-preserving addition, evaluating twice counts the
  // definition's base summand twice — `Q = 1 + Q` would contribute `ω + 2` where a reference
  // to `Q` contributes `ω + 1`.
  private def statIn(scope: Scope): Stat => Size = {
    case p: Pkg                                              => body(p.body.stats, scope)
    case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) =>
      scope.getOrElse(d.name.value, ctorIn(scope)(d.ctor))
    case d: Defn.Enum   => scope.getOrElse(d.name.value, enumSize(scope)(d))
    case d: Defn.Object => scope.getOrElse(d.name.value, UnitSize)
    // scalameta's `Stat` is not sealed and hides `Stat.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other statement — declarations, imports and
    // exports, bare terms, aliases — defines no values of its own.
    case d: Defn => defnIn(scope)(d)
    case _       => NothingSize
  }

  private def defnIn(scope: Scope): Defn => Size = {
    // Abstract classes contribute no inhabitants of their own; only their concrete
    // subclasses do.
    case c: Defn.Class if c.mods.exists(_.is[Mod.Abstract]) => NothingSize
    case c: Defn.Class                                      => ctorIn(scope)(c.ctor)
    // An enum's cardinality is the sum over its cases; the enum's own constructor
    // arguments are shared state, not extra inhabitants.
    case e: Defn.Enum => enumSize(scope)(e)
    // A module (including a `case object`) is a single instance.
    case _: Defn.Object            => UnitSize
    case _: Defn.Val | _: Defn.Var => UnitSize
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — type members, methods, givens,
    // enum cases — defines no values of its own.
    case _ => NothingSize
  }

  private def enumSize(scope: Scope): Defn.Enum => Size =
    _.templ.body.stats.foldLeft(NothingSize: Size) {
      case (acc, c: Defn.EnumCase)         => acc + ctorIn(scope)(c.ctor)
      case (acc, r: Defn.RepeatedEnumCase) => r.cases.foldLeft(acc)((a, _) => a + UnitSize)
      case (acc, _)                        => acc
    }

  private def ctorIn(scope: Scope): Ctor.Primary => Size =
    _.paramClauses.flatMap(_.values).foldLeft(UnitSize: Size)(_ * paramIn(scope)(_))

  private def paramIn(scope: Scope): Term.Param => Size =
    _.decltpe.fold(EffectiveOmega: Size)(typeIn(scope))

  // The member signature of a body: a static inventory of what a definition declares, summed
  // rather than multiplied. Every typed field, term and method contributes the size of its
  // declared type — `String => String` is ε₀, `String` is ω, `Boolean` is 2 — and nested
  // definitions contribute their own members once. The container itself adds nothing: an object
  // is a single value, but its signature is the sum of its members, which is what makes
  // `2ε₀ + ω` (two `String => String` methods and one `String` field) readable off a module.
  //
  // The member surface is the accessor surface: every parameter of a case class or an enum
  // case, the `val`/`var` parameters of a plain class, and the values, variables and methods a
  // body declares, abstract declarations included. A parameter without an accessor is part of
  // how a class is built, not of what it exposes, and synthesized accessors are not counted on
  // top of the parameters that produce them.
  private def signatureBody(stats: List[Stat], scope: Scope): Size = {
    val solved = solve(equations(stats), stats, scope)
    stats.foldLeft(NothingSize: Size)((acc, st) => acc + signatureIn(solved)(st))
  }

  private def signatureIn(scope: Scope): Stat => Size = {
    case p: Pkg        => signatureBody(p.body.stats, scope)
    case d: Defn.Class =>
      ctorSignature(scope)(caseParams = d.mods.exists(_.is[Mod.Case]), d.ctor) +
        signatureBody(d.templ.body.stats, scope)
    case d: Defn.Trait  => signatureBody(d.templ.body.stats, scope)
    case d: Defn.Object => signatureBody(d.templ.body.stats, scope)
    // An enum's own parameters reach every case, so they are members too.
    case d: Defn.Enum =>
      ctorSignature(scope)(caseParams = true, d.ctor) + signatureBody(d.templ.body.stats, scope)
    case d: Defn.EnumCase => ctorSignature(scope)(caseParams = true, d.ctor)
    // A value, variable or field contributes the size of its declared type; several patterns
    // (`val a, b: Int`) declare one member each, so their contributions add.
    case d: Defn.Val => declared(scope)(d.pats.size, d.decltpe)
    case d: Defn.Var => declared(scope)(d.pats.size, d.decltpe)
    case d: Decl.Val => declared(scope)(d.pats.size, Some(d.decltpe))
    case d: Decl.Var => declared(scope)(d.pats.size, Some(d.decltpe))
    // A method contributes its function space: the codomain raised to the product of its
    // parameters, with the same arrow and empty-domain rules `typeIn` uses.
    case d: Defn.Def => methodSignature(scope)(d.paramClauses, d.decltpe)
    case d: Decl.Def => methodSignature(scope)(d.paramClauses, Some(d.decltpe))
    // Everything else — type members, givens, extension groups, secondary constructors — is not
    // a declared value or method of the definition.
    case _ => NothingSize
  }

  private def ctorSignature(scope: Scope)(caseParams: Boolean, ctor: Ctor.Primary): Size = {
    val members = ctor.paramClauses.flatMap(_.values).filter(isAccessor(caseParams, _))
    members.foldLeft(NothingSize: Size)(_ + paramIn(scope)(_))
  }

  // A parameter is a signature member when the definition exposes it: every parameter of a case
  // class or an enum case, and the val/var parameters of a plain class.
  private def isAccessor(caseParams: Boolean, param: Term.Param): Boolean =
    caseParams || param.mods.exists(isAccessorMod)

  private def isAccessorMod(mod: Mod): Boolean = mod.is[Mod.ValParam] || mod.is[Mod.VarParam]

  private def declared(scope: Scope)(patterns: Int, decltpe: Option[Type]): Size =
    (0 until patterns).foldLeft(NothingSize: Size) { (acc, _) =>
      acc + decltpe.fold(EffectiveOmega: Size)(typeIn(scope))
    }

  private def methodSignature(scope: Scope)(
      paramClauses: Seq[Term.ParamClause],
      decltpe: Option[Type]
  ): Size = {
    val domain = paramClauses
      .flatMap(_.values)
      .foldLeft(UnitSize: Size)((acc, param) => acc * paramIn(scope)(param))
    arrow(decltpe.fold(EffectiveOmega: Size)(typeIn(scope)), domain)
  }

  private def typeIn(scope: Scope): Type => Size = {
    case Type.Name("Nothing")    => NothingSize
    case Type.Name("Unit")       => UnitSize
    case Type.Name("EmptyTuple") => UnitSize
    case Type.Name("Boolean")    => BooleanSize
    case Type.Name("Byte")       => ByteSize
    case Type.Name("Short")      => ShortSize
    case Type.Name("Char")       => CharSize
    case Type.Name("Int")        => IntSize
    case Type.Name("Long")       => LongSize
    case Type.Name("Float")      => FloatSize
    case Type.Name("Double")     => DoubleSize

    // Qualification (`scala.Boolean`) does not change cardinality: drop the qualifier.
    case Type.Select(_, name) => typeIn(scope)(name)

    // Literal types (`true`, `42`, `'a'`) and singleton types (`None.type`) have exactly
    // one inhabitant: the value itself.
    case _: Lit            => UnitSize
    case _: Type.Singleton => UnitSize

    // A by-name parameter has the cardinality of its underlying type.
    case Type.ByName(tpe) => typeIn(scope)(tpe)

    // Products: tuples (and named tuples) multiply the sizes of their fields.
    case Type.Tuple(elems) =>
      elems.foldLeft(UnitSize: Size)((acc, e) => acc * typeIn(scope)(tupleElem(e)))
    case Type.ApplyInfix(l, Type.Name("*:"), r) => typeIn(scope)(l) * typeIn(scope)(r)

    // Sums: unions add the sizes of their branches, unless both branches describe the
    // exact same type, in which case they fully overlap and count once.
    case Type.ApplyInfix(l, Type.Name("|"), r) =>
      if (l.structure == r.structure) typeIn(scope)(l) else typeIn(scope)(l) + typeIn(scope)(r)

    // Intersection: the overlap of two types is their meet, which is exact when (as here)
    // one is a subtype of the other.
    case Type.ApplyInfix(l, Type.Name("&"), r) => typeIn(scope)(l).min(typeIn(scope)(r))

    // Exponentials: a function type's cardinality is codomain ^ domain; multi-argument
    // and curried functions multiply/nest the same way. Context functions behave as functions.
    case Type.Function.After_4_6_0(Type.FuncParamClause(params), result) =>
      arrow(typeIn(scope)(result), domain(scope)(params))
    case Type.ContextFunction.After_4_6_0(Type.FuncParamClause(params), result) =>
      arrow(typeIn(scope)(result), domain(scope)(params))

    // Type constructors that add to the algebra: Option/Either (sums), Set (powerset),
    // and Map/PartialFunction (functions into an Option of the codomain).
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => applied(scope)(callee, args)

    // A name the source defines takes the cardinality of its definition.
    case Type.Name(name) if scope.contains(name) => scope(name)

    // scalameta's `Type` is not sealed, and several variants (`Type.And`, `Type.Or`,
    // `Type.Method`, `Type.ImplicitFunction`, `Type.Quasi`) are `private[meta]`, so an
    // exhaustive match is impossible. Every remaining form is unbounded or not yet
    // modelled — an unresolved name such as `String` or `BigInt`, a name defined in another
    // file, a refinement, an existential, a Scala 3 capture type — and counts as
    // effectively infinite.
    // ponytail: resolve sealed hierarchies and type parameters across files when needed
    case _ => EffectiveOmega
  }

  // `codomain ^ domain`, except that an empty domain gives 0 rather than the set-theoretic 1,
  // `0^0` included. This is a constructivist approach: Scala is eager, a call evaluates its
  // argument first, and no argument of an uninhabited type can be constructed, so a function
  // from one can never run. Only function types take this rule; `Set` and `Map` over
  // `Nothing` still hold their one empty value.
  private def arrow(codomain: Size, domain: Size): Size =
    if (domain == NothingSize) NothingSize else codomain.pow(domain)

  // `Set` is the powerset (`2 ^ element`), and `Map`/`PartialFunction` are functions into
  // an Option of the codomain (`(|V| + 1) ^ |K|`). `Size.pow` already has the arithmetic
  // `Set` and `Map` need: a finite base over an infinite exponent stays countable (the
  // finite subsets of `String`), and only an infinite base over an infinite exponent is
  // uncountable.
  private def applied(scope: Scope): (Type, List[Type]) => Size = {
    case (Type.Name("Option"), List(t))    => UnitSize + typeIn(scope)(t)
    case (Type.Name("Either"), List(l, r)) => typeIn(scope)(l) + typeIn(scope)(r)
    case (Type.Name("Set"), List(t))       => BooleanSize.pow(typeIn(scope)(t))
    case (Type.Name("Map"), List(k, v))    => (typeIn(scope)(v) + UnitSize).pow(typeIn(scope)(k))
    case (Type.Name("PartialFunction"), List(a, b)) =>
      (typeIn(scope)(b) + UnitSize).pow(typeIn(scope)(a))
    // A linear collection of an empty element type has a single inhabitant (the empty
    // collection); otherwise its unbounded length makes it effectively infinite, which
    // the fallback returns.
    case (Type.Name("List" | "Vector" | "Seq" | "IndexedSeq" | "Array"), List(t))
        if typeIn(scope)(t) == NothingSize =>
      UnitSize
    // `LazyList` and `Stream` are the greatest fixed point `νX. 1 + A*X` (§8): all the
    // finite ones — ℵ₀ over any nonempty finitely-countable alphabet — plus the infinite
    // streams, the alphabet's choice space per position raised to ℵ₀. Collapse that completed
    // lazy type to its highest infinite tier; ordinary sums around it still keep coefficients.
    // An empty alphabet has the single empty list, exactly as for `List`.
    case (Type.Name("LazyList" | "Stream"), List(t)) =>
      val elem = typeIn(scope)(t)
      if (elem == NothingSize) UnitSize
      else completeCoinduction(EffectiveOmega, elem.pow(EffectiveOmega))
    case _ => EffectiveOmega
  }

  // A function's domain is the product of its parameter types; `Unit` (a single empty
  // tuple) when it takes none.
  private def domain(scope: Scope)(params: List[Type]): Size =
    params.foldLeft(UnitSize: Size)((acc, p) => acc * typeIn(scope)(p))

  // A named-tuple element (`a: Boolean`) carries its type inside a TypedParam; every
  // other tuple element already is its own type.
  private def tupleElem: Type => Type = {
    case Type.TypedParam.After_4_7_8(_, tpe, _) => tpe
    case other                                  => other
  }

}
