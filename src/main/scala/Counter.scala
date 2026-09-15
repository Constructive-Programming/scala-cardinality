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

}
