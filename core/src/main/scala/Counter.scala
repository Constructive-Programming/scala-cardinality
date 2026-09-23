package cardinality

import scala.collection.mutable
import scala.meta.*

object Counter {

  // Trees are parsed as Scala 3; printing one with any other dialect makes scalameta reprint it
  // under Scala 2 rules, which cannot spell Scala 3's modifiers at all.
  private given scala3: Dialect = dialects.Scala3

  /** What every definition a source introduces holds together: the sum over the concrete classes,
    * enums, modules and top-level values it defines, nested definitions included.
    */
  def source: Source => Size = s => walk(s.stats, Scope.empty, Nil, top = true).contributes

  /** Every definition a source introduces, in source order, each with the cardinality of its type
    * and the names that stopped the calculator from bounding it — the number and the reason.
    */
  def definitions: Source => List[Definition] = s =>
    walk(s.stats, Scope.empty, Nil, top = true).definitions

  def stat: Stat => Size = statIn(Scope.empty)

  def defn: Defn => Size = defnIn(Scope.empty)

  def ctor: Ctor.Primary => Size = ctorIn(Scope.empty)

  def param: Term.Param => Size = paramIn(Scope.empty)

  def `type`: Type => Size = typeIn(Scope.empty)

  // The names of the types a source defines, each mapped to its cardinality, so that a field can
  // refer to a sibling definition by name. A definition is added as its statement is passed, so a
  // forward or recursive reference stays unknown — and falls back to `EffectiveOmega` — rather
  // than looping. The scope also collects the names it could not resolve, which is what lets a
  // report say why a size is unbounded instead of just printing ω.
  final private class Scope(
      names: Map[String, Size],
      unresolvedNames: mutable.LinkedHashSet[String],
  ) {
    def size(name: String): Option[Size] = names.get(name)

    def updated(name: String, size: Size): Scope =
      new Scope(names.updated(name, size), unresolvedNames)

    /** The same names with a fresh record of unresolved ones, so that one definition's reasons are
      * not read as another's.
      */
    def measured: Scope = new Scope(names, mutable.LinkedHashSet.empty)

    def unresolved: List[String] = unresolvedNames.toList.sorted

    /** Records a type the calculator could not bound: an unknown name, an unmodelled type
      * constructor, or a collection whose length is unbounded.
      */
    def note(tpe: Type): Unit = { unresolvedNames += describe(tpe); () }
  }

  private object Scope {
    val empty: Scope = new Scope(Map.empty, mutable.LinkedHashSet.empty)
  }

  // What one statement adds to its body: the cardinality it contributes, the definitions it
  // introduces (which a report lists) and the names a later sibling can refer to.
  final private case class Introduced(
      contributes: Size,
      definitions: List[Definition],
      names: List[(String, Size)]
  ) {

    /** Adds what the body of this definition introduces. Nested definitions are their own
      * inhabitants — and their own rows — so they add to both, while the names they introduce stay
      * local to the body.
      */
    def inside(scope: Scope, prefix: List[String], stats: List[Stat]): Introduced = {
      val body = walk(stats, scope, prefix, top = false)
      copy(
        contributes = contributes + body.contributes,
        definitions = definitions ++ body.definitions
      )
    }

    /** Names this definition for the statements of its own body: a nested definition may refer to
      * the one that encloses it.
      */
    def withName(name: String, size: Size): Introduced = copy(names = name -> size :: names)
  }

  private object Introduced {
    val none: Introduced = Introduced(NothingSize, Nil, Nil)
  }

  // Walks statements in source order, accumulating the names they introduce so that a later
  // statement can refer to an earlier one, and returns what they hold together with the
  // definitions they introduce, named relative to `prefix` (the enclosing packages and
  // definitions). `top` marks the body of a source or package, where a value definition is an
  // inhabitant of the program rather than state derived inside a class.
  private def walk(
      stats: List[Stat],
      scope: Scope,
      prefix: List[String],
      top: Boolean
  ): Introduced = {
    val (contributes, _, definitions) =
      stats.foldLeft((NothingSize: Size, scope, Vector.empty[Definition])) {
        case ((acc, sc, found), st) =>
          val introduced = statement(sc, prefix, top)(st)
          val extended = introduced.names.foldLeft(sc) {
            case (s, (name, size)) => s.updated(name, size)
          }
          (acc + introduced.contributes, extended, found ++ introduced.definitions)
      }
    Introduced(contributes, definitions.toList, Nil)
  }

  // A statement in a body: a package opens a nested one, a definition is measured and named, and
  // every other statement — declarations, imports and exports, bare terms — introduces nothing.
  private def statement(scope: Scope, prefix: List[String], top: Boolean): Stat => Introduced = {
    case p: Pkg        => walk(p.body.stats, scope, prefix ++ p.ref.syntax.split('.'), top)
    case p: Pkg.Object => walk(p.templ.body.stats, scope, prefix :+ p.name.value, top)
    case d: Defn       => defnWalk(scope, prefix, top)(d)
    case _             => Introduced.none
  }

  private def defnWalk(scope: Scope, prefix: List[String], top: Boolean): Defn => Introduced = {
    // What a definition contributes to its source is `defnIn`'s call — one rule for every kind of
    // definition; the walk adds the rows a report reads and the descent into the bodies a
    // definition opens. Where a row shows something else than the contribution it is because a
    // reference to the definition is worth more than the definition itself adds: an alias names
    // another type's values, an opaque type hides them, an abstract type has none of its own.
    case d: Defn.Class if d.mods.exists(_.is[Mod.Abstract]) =>
      Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Abstract, None)), Nil)
        .inside(scope, prefix :+ d.name.value, d.templ.body.stats)
    case d: Defn.Class =>
      measured(scope) { s =>
        val size = defnIn(s)(d)
        introduced(prefix, d, Definition.Kind.Class, Some(size), size, s.unresolved)
          .withName(d.name.value, size)
          .inside(s.updated(d.name.value, size), prefix :+ d.name.value, d.templ.body.stats)
      }
    case d: Defn.Trait =>
      Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Abstract, None)), Nil)
        .inside(scope, prefix :+ d.name.value, d.templ.body.stats)
    // An enum's cardinality is the sum over its cases, which `defnIn` returns; the enum's own
    // constructor arguments are shared state, not extra inhabitants.
    case d: Defn.Enum =>
      measured(scope) { s =>
        val size = defnIn(s)(d)
        introduced(prefix, d, Definition.Kind.Enum, Some(size), size, s.unresolved)
          .withName(d.name.value, size)
          .inside(s.updated(d.name.value, size), prefix :+ d.name.value, d.templ.body.stats)
      }
    // A module (including a `case object`) is a single instance.
    case d: Defn.Object =>
      measured(scope) { s =>
        val size = defnIn(s)(d)
        introduced(prefix, d, Definition.Kind.Object, Some(size), size, Nil)
          .withName(d.name.value, size)
          .inside(s.updated(d.name.value, size), prefix :+ d.name.value, d.templ.body.stats)
      }
    // An opaque type hides what it holds: a reference to it is worth a single value outside the
    // scope that defines it, and the definition adds none of its own.
    case d: Defn.Type if d.mods.exists(_.is[Mod.Opaque]) =>
      measured(scope) { s =>
        introduced(prefix, d, Definition.Kind.Opaque, Some(UnitSize), defnIn(s)(d), Nil)
          .withName(d.name.value, UnitSize)
      }
    // A reference to the alias holds the aliased type's values; the alias itself adds none.
    case d: Defn.Type =>
      measured(scope) { s =>
        val aliased = typeIn(s)(d.body)
        introduced(prefix, d, Definition.Kind.Alias, Some(aliased), defnIn(s)(d), s.unresolved)
          .withName(d.name.value, aliased)
      }
    // An enum case is counted by its enum; it has no body of its own.
    case _: Defn.EnumCase | _: Defn.RepeatedEnumCase => Introduced.none
    // A value at the top level of a source is one inhabitant. A value inside a class or object
    // body is state derived from the fields, which already count it, so it adds nothing.
    case d @ (_: Defn.Val | _: Defn.Var) if top =>
      measured(scope) { s =>
        val size = defnIn(s)(d)
        introduced(prefix, d, Definition.Kind.Value, Some(size), size, Nil)
      }
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — nested values, methods, givens —
    // defines no inhabitants of its own.
    case _ => Introduced.none
  }

  // What a report can point at when the calculator cannot bound a type: the type's name where it
  // has one — `String`, `A`, `NonEmptyList` — and what kind of type it is where it has none, since
  // a match type or a refinement has no name to print. Anything left is described by its source
  // text on one line, so that a report row stays a row.
  private def describe(tpe: Type): String = tpe match {
    case name: Type.Name               => name.value
    case select: Type.Select           => select.name.value
    case applied: Type.Apply           => describe(applied.tpe)
    case infix: Type.ApplyInfix        => infix.op.value
    case annotate: Type.Annotate       => describe(annotate.tpe)
    case existential: Type.Existential => describe(existential.tpe)
    case refine: Type.Refine           =>
      refine.tpe.fold("a refinement")(inner => s"a refinement of ${describe(inner)}")
    case _: Type.Match        => "a match type"
    case _: Type.Lambda       => "a type lambda"
    case _: Type.PolyFunction => "a polymorphic function type"
    case _: Type.Wildcard     => "a wildcard"
    case other                => other.syntax.replaceAll("\\s+", " ")
  }

  // Measures one definition with a scope that records only its own unresolved names.
  private def measured(scope: Scope)(f: Scope => Introduced): Introduced = f(scope.measured)

  // A definition as its source sees it: the row a report lists it as, and the cardinality it
  // contributes. The row is measured with the definition's own unresolved names, kept apart from
  // its siblings'.
  private def introduced(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      size: Option[Size],
      contributes: Size,
      unresolved: List[String],
  ): Introduced =
    Introduced(contributes, List(row(prefix, d, kind, size, unresolved)), Nil)

  private def row(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      size: Option[Size],
      unresolved: List[String] = Nil,
  ): Definition =
    Definition((prefix :+ named(d)).mkString("."), kind, params(d), size, unresolved, line(d))

  // scalameta counts lines from zero; a report points at the line an editor shows.
  private def line(d: Defn): Int = d.pos.startLine + 1

  // The type parameters a definition declares; a value declares none.
  private def params(d: Defn): List[String] = d match {
    case c: Defn.Class => c.tparamClause.values.map(_.name.value)
    case c: Defn.Trait => c.tparamClause.values.map(_.name.value)
    case c: Defn.Enum  => c.tparamClause.values.map(_.name.value)
    case c: Defn.Type  => c.tparamClause.values.map(_.name.value)
    case _             => Nil
  }

  // Values and variables are named by the patterns they bind; every other definition the report
  // lists carries its name directly. `Defn` is not sealed, so the last case stands in for the
  // members the walk never names.
  private def named(d: Defn): String = d match {
    case v: Defn.Val => v.pats.map(_.syntax).mkString(", ")
    case v: Defn.Var => v.pats.map(_.syntax).mkString(", ")
    case m: Member   => m.name.value
    case other       => other.syntax
  }

  // What a body holds together: what `Counter.source` reports for a source is this walk over its
  // top-level statements, and what `stat` reports for a package statement is the same walk over
  // the statements the package contains.
  private def body(stats: List[Stat], scope: Scope): Size =
    walk(stats, scope, Nil, top = true).contributes

  private def statIn(scope: Scope): Stat => Size = {
    case p: Pkg        => body(p.body.stats, scope)
    case p: Pkg.Object => body(p.templ.body.stats, scope)
    case d: Defn       => defnIn(scope)(d)
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

    // A name the source defines takes the cardinality of its definition. Any other name is
    // unbounded, and is recorded so that a report can say which name it was.
    case t: Type.Name => scope.size(t.value).getOrElse { scope.note(t); EffectiveOmega }

    // scalameta's `Type` is not sealed, and several variants (`Type.And`, `Type.Or`,
    // `Type.Method`, `Type.ImplicitFunction`, `Type.Quasi`) are `private[meta]`, so an
    // exhaustive match is impossible. Every remaining form is unbounded or not yet
    // modelled — an unresolved name such as `String` or `BigInt`, a refinement, an
    // existential, a Scala 3 capture type — and counts as effectively infinite. Each one is
    // recorded, so that a report can name what it could not bound.
    // ponytail: resolve sealed hierarchies when needed
    case t =>
      scope.note(t)
      EffectiveOmega
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
    // A type constructor the calculator does not model — including a collection whose element
    // type is unbounded, whose length is what makes it infinite — is recorded by name, so that a
    // report can say what it could not bound.
    case (callee, _) =>
      scope.note(callee)
      EffectiveOmega
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
