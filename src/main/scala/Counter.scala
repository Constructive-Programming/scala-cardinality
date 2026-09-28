import scala.annotation.tailrec
import scala.meta.*

object Counter {
  def source: Source => Size = s => body(s.stats, Scope.empty)

  def stat: Stat => Size = statIn(Scope.empty)

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
    val solved = solve(stats, scope)
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
      case d: Defn.Class  => (d.name.value, d.templ.inits.map(_.name.syntax))
      case d: Defn.Object => (d.name.value, d.templ.inits.map(_.name.syntax))
      case d: Defn.Enum   => (d.name.value, d.templ.inits.map(_.name.syntax))
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
  // A name that grows in two consecutive rounds is productive recursion: its chain
  // (`1, 2, 3, …` for `Nat`) is strictly increasing forever but its least fixed point is
  // countable, so it is pinned straight to `EffectiveOmega` (ℵ₀) and dependents re-evaluate
  // once more; the algebra already saturates the genuinely uncountable payloads on its
  // own. The `budget` only accelerates mutual recursion, where two names take turns
  // growing and never grow in consecutive rounds: once it expires, a name that grew
  // earlier is pinned on its next growth, while one growing for the first time (a long
  // forward-reference chain still settling) is left alone. Either branch shrinks the set
  // of unpinned names that may still grow, so the iteration terminates.
  private def solve(stats: List[Stat], base: Scope): Scope = {
    val eqs = equations(stats)
    val seeded = base ++ eqs.map { case (name, _) => name -> (NothingSize: Size) }

    @tailrec
    def iterate(
        scope: Scope,
        grewLastRound: Set[String],
        grewEver: Set[String],
        pinned: Set[String],
        budget: Int
    ): Scope = {
      val evaluated =
        eqs.collect { case (name, equation) if !pinned(name) => name -> equation(scope) }.toMap
      val grown = evaluated.collect { case (name, size) if scope(name) != size => name }.toSet
      if (grown.isEmpty) scope ++ evaluated
      else {
        val lastRounds = if (budget > 0) grewLastRound else grewEver
        val toPin = grown.intersect(lastRounds)
        iterate(
          scope = scope ++ evaluated ++ toPin.map(_ -> (EffectiveOmega: Size)),
          grewLastRound = grown,
          grewEver = grewEver.union(grown),
          pinned = pinned.union(toPin),
          budget = budget - 1
        )
      }
    }

    val mu =
      iterate(
        seeded,
        grewLastRound = Set.empty,
        grewEver = Set.empty,
        pinned = Set.empty,
        budget = 8
      )
    greatestFixpoint(stats, mu)
  }

  // The greatest fixed point of lazy recursion (docs/type-arithmetic.md §8): the μ
  // iterate above counts the finite constructor trees; a cycle through a lazy hole — a
  // parameter the constructor never demands, `=> X`, `=> Option[X]` or the thunk `() => X` —
  // admits infinite values too. Each recognized cycle adds its per-lap label space raised
  // to ℵ₀, the finite-program reading `Size.pow` already implements: `νX.X` counts 1, the
  // conaturals gain their one infinite tower (absorbed by the ℵ₀ of finite depths), an
  // endless `Boolean` stream gains ℵ₀, and a countable alphabet reaches `EffectiveTau`.
  // Cycles that are not exactly one recognized hole per member — branching successors, a
  // hole buried deeper than the accepted forms, or a member's other fields referring back
  // into the cycle — are skipped rather than guessed at, leaving the sound μ under-count.
  private def greatestFixpoint(stats: List[Stat], mu: Scope): Scope = {
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
    val edges = holeEdges(arms)

    // Resolve a hole target to the next concrete member: one hop when the target is
    // itself a cycle member, and through a sealed parent only when exactly one of its
    // children continues the cycle (a choice between two continuing children is a
    // branching successor, which is skipped). The resolved walk must return to the
    // start: a tail leading into someone else's cycle is not a cycle of its own.
    def resolve(name: String): Option[String] =
      edges.get(name).flatMap {
        case (target, _, _) =>
          if (edges.contains(target)) Some(target)
          else
            parents.get(target).flatMap { children =>
              children.filter(child => edges.get(child).exists(_._1 == target)) match {
                case List(child) => Some(child)
                case _           => None
              }
            }
      }

    def walk(start: String, at: String, seen: Set[String], steps: Int): Option[Set[String]] =
      if (at == start) Some(seen)
      else if (steps <= 0 || seen.contains(at)) None
      else resolve(at).flatMap(next => walk(start, next, seen + at, steps - 1))

    val cycles = edges.keys.toList
      .flatMap(name => resolve(name).flatMap(next => walk(name, next, Set(name), edges.size)))
      .distinct
      .map { members =>
        val passThrough =
          members.iterator.map(name => edges(name)._1).filter(parents.contains).toSet
        val all = members.union(passThrough)
        // A non-hole parameter that mentions the cycle makes the per-lap choice depend on
        // the value being unfolded, which the label formula cannot express — skip it.
        val restParams = members.iterator.flatMap(name => edges(name)._3.flatten).toList
        if (restParams.exists(p => namesOf(p).exists(all.contains))) None
        else Some((all, lapSpace(edges, mu)(members)))
      }
      .flatten

    cycles.foldLeft(mu) {
      case (scope, (members, lap)) =>
        val infinite = lap.pow(EffectiveOmega)
        members.foldLeft(scope)((sc, name) => sc.updated(name, sc(name) + infinite))
    }
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

  // name -> (lazy target, hole arms, non-hole parameters of each hole arm), for members
  // whose every holed arm has exactly one hole and all holed arms share one target. A
  // member with two holes in one arm, or holes pointing at different successors, gets no
  // edge — those are the branching shapes §8 warns against counting by formula.
  private def holeEdges(
      arms: Map[String, List[List[Type]]]
  ): Map[String, (String, Int, List[List[Type]])] =
    arms.iterator.flatMap {
      case (name, memberArms) =>
        val holed = memberArms.flatMap { arm =>
          val (lazyParams, rest) = arm.partition(param => holeOf(param).isDefined)
          if (lazyParams.isEmpty) Nil
          else lazyParams.flatMap(holeOf).distinct.map(target => (target, lazyParams.size, rest))
        }
        holed match {
          case Nil => None
          case (target, _, _) :: others
              if others.forall(o => o._1 == target && o._2 == 1) && holed.forall(_._2 == 1) =>
            Some(name -> (target, holed.size, holed.map(_._3)))
          case _ => None
        }
    }.toMap

  // A hole is a lazy recursive occurrence the constructor never demands, in exactly the
  // forms §8 pins down: `=> X`, `=> Option[X]`, `() => X`, `() => Option[X]`.
  private def holeOf(param: Type): Option[String] = param match {
    case Type.ByName(inner)                                          => bareTarget(inner)
    case Type.Function.After_4_6_0(Type.FuncParamClause(Nil), inner) => bareTarget(inner)
    case _                                                           => None
  }

  private def bareTarget(tpe: Type): Option[String] = tpe match {
    case Type.Name(name) => Some(name)
    case Type.Apply(callee, Type.ArgClause(List(Type.Name(name))))
        if bareName(callee) == "Option" =>
      Some(name)
    case _ => None
  }

  private def bareName: Type => String = {
    case Type.Name(n)                 => n
    case Type.Select(_, Type.Name(n)) => n
    case _                            => ""
  }

  // The per-lap label space of a cycle: every member contributes the sum over its hole
  // arms of the product of that arm's non-hole parameters, all at the finite (μ) counts.
  // Members with a single holed arm (the common `case class St(h: Boolean, t: => St)`
  // shape) contribute just that arm's product.
  private def lapSpace(
      edges: Map[String, (String, Int, List[List[Type]])],
      mu: Scope
  )(members: Set[String]): Size =
    members.toList.foldLeft(NothingSize: Size) { (acc, name) =>
      val armSpaces = edges(name)._3.map(rest => rest.foldLeft(UnitSize: Size)(_ * typeIn(mu)(_)))
      acc + armSpaces.foldLeft(NothingSize: Size)(_ + _)
    }

  private def namesOf(tpe: Type): Set[String] =
    tpe.collect { case Type.Name(n) => n }.toSet

  private def statIn(scope: Scope): Stat => Size = {
    case p: Pkg  => body(p.body.stats, scope)
    case d: Defn => defnIn(scope)(d)
    // scalameta's `Stat` is not sealed and hides `Stat.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other statement — declarations, imports and
    // exports, bare terms — defines no values of its own.
    case _ => NothingSize
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
    // streams, the alphabet's choice space per position raised to ℵ₀. An empty alphabet
    // collapses to the single empty list, exactly as for `List`.
    case (Type.Name("LazyList" | "Stream"), List(t)) =>
      val elem = typeIn(scope)(t)
      if (elem == NothingSize) UnitSize
      else EffectiveOmega + elem.pow(EffectiveOmega)
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
