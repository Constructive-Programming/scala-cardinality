import scala.meta.*

object Counter {
  def source: Source => Size = s => body(s.stats, Scope.empty)

  def stat: Stat => Size = statIn(Scope.empty)

  def defn: Defn => Size = defnIn(Scope.empty)

  def ctor: Ctor.Primary => Size = ctorIn(Scope.empty)

  def param: Term.Param => Size = paramIn(Scope.empty)

  def `type`: Type => Size = typeIn(Scope.empty)

  // The names of the types a source defines, each mapped to its cardinality, so that a
  // field can refer to a sibling definition by name. A definition is added as its
  // statement is passed, so a forward or recursive reference stays unknown — and falls
  // back to `EffectiveOmega` — rather than looping.
  private type Scope = Map[String, Size]

  private object Scope {
    val empty: Scope = Map.empty
  }

  private def body(stats: List[Stat], scope: Scope): Size = {
    val (total, _) = stats.foldLeft((NothingSize: Size, scope)) {
      case ((acc, sc), st) =>
        val extended = definitions(sc)(st).fold(sc) { case (name, size) => sc.updated(name, size) }
        (acc + statIn(sc)(st), extended)
    }
    total
  }

  // The named types a statement introduces, with their cardinality. Abstract traits and
  // classes have no cardinality of their own, so they are not named.
  private def definitions(scope: Scope): Stat => Option[(String, Size)] = {
    case d: Defn.Type if d.mods.exists(_.is[Mod.Opaque]) => Some(d.name.value -> UnitSize)
    case d: Defn.Type => Some(d.name.value -> typeIn(scope)(d.body))
    case d: Defn.Enum => Some(d.name.value -> enumSize(scope)(d))
    case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) =>
      Some(d.name.value -> ctorIn(scope)(d.ctor))
    case d: Defn.Object => Some(d.name.value -> UnitSize)
    case _              => None
  }

  private def statIn(scope: Scope): Stat => Size = {
    case p: Pkg  => body(p.body.stats, scope)
    case d: Defn => defnIn(scope)(d)
    // A body-less method declaration carries the same signature as a definition, so it is the
    // same exponential.
    case d: Decl.Def =>
      methodSize(scope, typeParametersOf(d.paramClauseGroups), d.paramClauses, Some(d.decltpe))
    // scalameta's `Stat` is not sealed and hides `Stat.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other statement — imports and exports, bare
    // terms, other declarations — defines no values of its own.
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
    // A module (including a `case object`) is a single instance, and so is a definition
    // that binds a value: a `val` or a `var`.
    case _: Defn.Object            => UnitSize
    case _: Defn.Val | _: Defn.Var => UnitSize
    // A method is an exponential: its result type raised to the product of its parameter
    // types.
    case d: Defn.Def =>
      methodSize(scope, typeParametersOf(d.paramClauseGroups), d.paramClauses, d.decltpe)
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — type members, givens,
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

  // The type parameters of a definition or declaration. scalameta moved them into the
  // parameter-clause groups; the flat `tparams` accessor is deprecated.
  private def typeParametersOf(groups: List[Member.ParamClauseGroup]): List[Type.Param] =
    groups.headOption.fold(List.empty[Type.Param])(_.tparamClause.values)

  // A method is an exponential: the result type raised to the product of its parameter
  // types. With no declared result type it binds the singleton it computes, so `def f = 1`
  // counts as one value. A method with type parameters is counted by its free theorem
  // instead; see `freeTheorem`.
  private def methodSize(
      scope: Scope,
      typeParams: List[Type.Param],
      params: Seq[Term.ParamClause],
      result: Option[Type]
  ): Size = {
    val parameters = params.flatMap(_.values).toList
    val codomain = result.fold(UnitSize: Size)(typeIn(scope))
    val domain = parameters.foldLeft(UnitSize: Size)((acc, p) => acc * paramIn(scope)(p))
    val declared = parameters.map(_.decltpe)
    val variables = typeParams.map(_.name.value).toSet
    val free =
      if variables.isEmpty || declared.exists(_.isEmpty) then None
      else
        functorTheorem(typeParams, declared.flatten, result)
          .orElse(freeTheorem(scope, variables, declared.flatten, result))
    free.getOrElse(codomain.pow(domain))
  }

  // An occurrence in a product type: either a type variable, or a constant whose cardinality
  // is already known.
  private enum Slot {
    case Variable(name: String)
    case Constant(size: Size)
  }

  // The free theorem for a polymorphic method: each position of the result that is a type
  // variable picks one of the occurrences the parameters supply, while a constant position
  // keeps its own cardinality. So `A => A` is 1, `A => Boolean` is 2, `(A, A) => A` is 2
  // (either supplied value) and `A => (A, A)` is 1. `None` for any type outside tuples of
  // variables and constants — a sum, a function or a type constructor over a variable — which
  // the exponential rule in `methodSize` handles instead.
  private def freeTheorem(
      scope: Scope,
      variables: Set[String],
      parameters: List[Type],
      result: Option[Type]
  ): Option[Size] = {
    val produced = result match
      case None    => Some(List.empty[Slot])
      case Some(t) => slotTypes(scope, variables)(t)
    for
      supplied <- sequence(parameters.map(slotTypes(scope, variables)))
      slots <- produced
    yield
      val supply = occurrences(supplied.flatten)
      slots.foldLeft(UnitSize: Size) {
        case (acc, Slot.Variable(name)) => acc * supplySize(supply.getOrElse(name, BigInt(0)))
        case (acc, Slot.Constant(size)) => acc * size
      }
  }

  private def slotTypes(scope: Scope, variables: Set[String]): Type => Option[List[Slot]] = {
    case Type.Name(name) if variables.contains(name) => Some(List(Slot.Variable(name)))
    case t: Type.Name                                => Some(List(Slot.Constant(typeIn(scope)(t))))
    case t: Type.Select                              => Some(List(Slot.Constant(typeIn(scope)(t))))
    case Type.Tuple(elements)                        =>
      sequence(elements.map(slotTypes(scope, variables))).map(_.flatten)
    case Type.ApplyInfix(left, Type.Name("*:"), right) =>
      for
        ls <- slotTypes(scope, variables)(left)
        rs <- slotTypes(scope, variables)(right)
      yield ls ++ rs
    case _ => None
  }

  private def occurrences(slots: List[Slot]): Map[String, BigInt] =
    slots
      .collect { case Slot.Variable(name) => name }
      .groupMapReduce(identity)(_ => BigInt(1))(_ + _)

  private def supplySize(count: BigInt): Size =
    if count <= 127 then TinySize(count.toByte) else FiniteSize(Size.bits(count))

  private def sequence[A](values: List[Option[A]]): Option[List[A]] =
    values.foldRight(Option(List.empty[A])) { (value, acc) =>
      for
        rest <- acc
        v <- value
      yield v :: rest
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
      typeIn(scope)(result).pow(domain(scope)(params))
    case Type.ContextFunction.After_4_6_0(Type.FuncParamClause(params), result) =>
      typeIn(scope)(result).pow(domain(scope)(params))

    // Type constructors that add to the algebra: Option/Either (sums), Set (powerset),
    // and Map/PartialFunction (functions into an Option of the codomain).
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => applied(scope)(callee, args)

    // A name the source defines takes the cardinality of its definition.
    case Type.Name(name) if scope.contains(name) => scope(name)

    // scalameta's `Type` is not sealed, and several variants (`Type.And`, `Type.Or`,
    // `Type.Method`, `Type.ImplicitFunction`, `Type.Quasi`) are `private[meta]`, so an
    // exhaustive match is impossible. Every remaining form is unbounded or not yet
    // modelled — an unresolved name such as `String` or `BigInt`, a refinement, an
    // existential, a Scala 3 capture type — and counts as effectively infinite.
    // ponytail: resolve sealed hierarchies when needed
    case _ => EffectiveOmega
  }

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
    case (Type.Name("List" | "Vector" | "Seq" | "IndexedSeq" | "Array" | "LazyList"), List(t))
        if typeIn(scope)(t) == NothingSize =>
      UnitSize
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

  // A higher-kinded type parameter with a `Functor` context bound is an opaque functor whose
  // only abstract member is `map`: its concrete members (`as`, `void`, `fproduct`, ...) are
  // all derivable from `map`, so they bring no further inhabitants, and by the free theorem
  // `map`'s type has exactly one implementation. A chain of maps collapses into a single map
  // of the composed function, so the result is one of the supplied `F[_]` values — returned
  // as is, or mapped along a chain of the supplied argument functions.
  private def functorTheorem(
      typeParams: List[Type.Param],
      parameters: List[Type],
      result: Option[Type]
  ): Option[Size] =
    typeParams.collect {
      case p if p.tparamClause.values.nonEmpty && p.bounds.context.exists(isFunctor) => p.name.value
    } match
      case List(functor) =>
        val parts = parameters.map { parameter =>
          functorArgument(functor)(parameter)
            .map(Left(_))
            .orElse(functionEdge(parameter).map(Right(_)))
        }
        for
          parsed <- sequence(parts)
          produced <- result.flatMap(functorArgument(functor))
        yield
          val supplied = parsed.collect { case Left(variable) => variable }
          val edges = parsed.collect { case Right(edge) => edge }
          val direct = BigInt(supplied.count(_ == produced))
          val viaMaps = supplied.map(variable => chains(edges, variable, produced))
          (Some(direct) :: viaMaps)
            .foldLeft(Option(BigInt(0)))((acc, count) =>
              for {
                a <- acc
                b <- count
              } yield a + b
            )
            .fold(EffectiveOmega: Size)(supplySize)
      case _ => None

  private def isFunctor(tpe: Type): Boolean = tpe match
    case Type.Name("Functor")    => true
    case Type.Select(_, functor) => isFunctor(functor)
    case _                       => false

  // An argument `F[X]`, for the one functor `F` in play.
  private def functorArgument(functor: String): Type => Option[String] = {
    case Type.Apply.After_4_6_0(Type.Name(`functor`), Type.ArgClause(List(Type.Name(variable)))) =>
      Some(variable)
    case _ => None
  }

  // An argument function `X => Y`.
  private def functionEdge: Type => Option[(String, String)] = {
    case Type.Function.After_4_6_0(Type.FuncParamClause(List(Type.Name(from))), Type.Name(to)) =>
      Some((from, to))
    case _ => None
  }

  // The number of non-empty chains of argument functions from one type variable to another.
  // `None` when a loop can be taken on the way, which makes the number of chains unbounded.
  private def chains(edges: List[(String, String)], from: String, to: String): Option[BigInt] = {
    val adjacency = edges.groupMap(_._1)(_._2)

    def reaches(variable: String, seen: Set[String]): Boolean =
      adjacency
        .getOrElse(variable, Nil)
        .exists(target => target == to || (!seen(target) && reaches(target, seen + target)))

    def paths(variable: String, visiting: Set[String]): Option[BigInt] =
      if visiting(variable) then None
      else
        adjacency
          .getOrElse(variable, Nil)
          .filter(target => target == to || reaches(target, Set(target)))
          .foldLeft(Option(BigInt(0))) { (acc, target) =>
            for
              total <- acc
              step <- if target == to then Some(BigInt(1)) else paths(target, visiting + variable)
            yield total + step
          }

    paths(from, Set.empty)
  }

}
