import scala.meta.*

object Counter {
  def source: Source => Size = s => sum(s.stats)

  def stat: Stat => Size = {
    case p: Pkg  => sum(p.body.stats)
    case d: Defn => defn(d)
    case _       => NothingSize
  }

  def defn: Defn => Size = {
    // Abstract classes contribute no inhabitants of their own; only their concrete
    // subclasses do.
    case c: Defn.Class if c.mods.exists(_.is[Mod.Abstract]) => NothingSize
    case c: Defn.Class                                      => ctor(c.ctor)
    // A module (including a `case object`) is a single instance.
    case _: Defn.Object => UnitSize
    case _: Defn.Val    => UnitSize
    case _              => NothingSize
  }

  def ctor: Ctor.Primary => Size =
    _.paramClauses.flatMap(_.values).foldLeft(UnitSize: Size)(_ * param(_))

  def param: Term.Param => Size = _.decltpe.fold(EffectiveOmega: Size)(`type`)

  def `type`: Type => Size = {
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
    case Type.Select(_, name) => `type`(name)

    // Literal types (`true`, `42`, `'a'`) and singleton types (`None.type`) have exactly
    // one inhabitant: the value itself.
    case _: Lit            => UnitSize
    case _: Type.Singleton => UnitSize

    // A by-name parameter has the cardinality of its underlying type.
    case Type.ByName(tpe) => `type`(tpe)

    // Products: tuples (and named tuples) multiply the sizes of their fields.
    case Type.Tuple(elems) => elems.foldLeft(UnitSize: Size)((acc, e) => acc * `type`(tupleElem(e)))
    case Type.ApplyInfix(l, Type.Name("*:"), r) => `type`(l) * `type`(r)

    // Sums: unions add the sizes of their branches, unless both branches describe the
    // exact same type, in which case they fully overlap and count once.
    case Type.ApplyInfix(l, Type.Name("|"), r) =>
      if (l.structure == r.structure) `type`(l) else `type`(l) + `type`(r)

    // Intersection: the overlap of two types is their meet, which is exact when (as here)
    // one is a subtype of the other.
    case Type.ApplyInfix(l, Type.Name("&"), r) => `type`(l).min(`type`(r))

    // Exponentials: a function type's cardinality is codomain ^ domain; multi-argument
    // and curried functions multiply/nest the same way. Context functions behave as functions.
    case Type.Function.After_4_6_0(Type.FuncParamClause(params), result) =>
      `type`(result).pow(domain(params))
    case Type.ContextFunction.After_4_6_0(Type.FuncParamClause(params), result) =>
      `type`(result).pow(domain(params))

    // Type constructors that add to the algebra: Option/Either (sums), Set (powerset),
    // and Map/PartialFunction (functions into an Option of the codomain).
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => applied(callee, args)

    // ponytail: String, BigInt, List[_], user-defined types all count as effectively infinite; resolve sealed hierarchies when needed
    case _ => EffectiveOmega
  }

  private def applied: (Type, List[Type]) => Size = {
    case (Type.Name("Option"), List(t))    => UnitSize + `type`(t)
    case (Type.Name("Either"), List(l, r)) => `type`(l) + `type`(r)
    case (Type.Name("Set"), List(t))       => finiteStructures(BooleanSize, `type`(t))
    case (Type.Name("Map"), List(k, v))    => finiteStructures(`type`(v) + UnitSize, `type`(k))
    case (Type.Name("PartialFunction"), List(a, b)) =>
      finiteStructures(`type`(b) + UnitSize, `type`(a))
    // A linear collection of an empty element type has a single inhabitant (the empty
    // collection); otherwise its unbounded length makes it effectively infinite, which
    // the fallback returns.
    case (Type.Name("List" | "Vector" | "Seq" | "IndexedSeq" | "Array" | "LazyList"), List(t))
        if `type`(t) == NothingSize =>
      UnitSize
    case _ => EffectiveOmega
  }

  // `Set`, `Map` and `PartialFunction` collect *finite* structures, so their exponential
  // formulas hold only over finite arguments; an infinite argument leaves only countably
  // many of them (e.g. the finite subsets of `String` are countable), not `EffectiveTau`.
  private def finiteStructures(base: Size, exponent: Size): Size = exponent match {
    case _: TinySize | _: FiniteSize => base.pow(exponent)
    case _                           => EffectiveOmega
  }

  // A function's domain is the product of its parameter types; `Unit` (a single empty
  // tuple) when it takes none.
  private def domain(params: List[Type]): Size =
    params.foldLeft(UnitSize: Size)((acc, p) => acc * `type`(p))

  // A named-tuple element (`a: Boolean`) carries its type inside a TypedParam; every
  // other tuple element already is its own type.
  private def tupleElem: Type => Type = {
    case Type.TypedParam.After_4_7_8(_, tpe, _) => tpe
    case other                                  => other
  }

  private def sum(stats: Seq[Stat]): Size = stats.foldLeft(NothingSize: Size)(_ + stat(_))
}
