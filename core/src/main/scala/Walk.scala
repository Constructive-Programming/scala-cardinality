package cardinality

import scala.meta.*

// What a source holds together, definition by definition: the walk that solves a body, measures
// every definition it introduces and names it for a report. See `Counter`.

private[cardinality] object Walk {

  final private[cardinality] case class Introduced(
      contributes: Size,
      definitions: List[Definition]
  ) {

    /** Adds what the body of this definition introduces. Nested definitions are their own
      * inhabitants — and their own rows — so they add to both: `object Wrapper { case class Pair(a:
      * Boolean, b: Boolean) }` holds five values, the module and the four pairs.
      */
    def inside(scope: Scope, prefix: List[String], stats: List[Stat]): Introduced = {
      val body = of(stats, scope, prefix, top = false)
      copy(
        contributes = contributes + body.contributes,
        definitions = definitions ++ body.definitions
      )
    }

  }

  private[cardinality] object Introduced {
    val none: Introduced = Introduced(NothingSize, Nil)
  }

  // Walks statements in source order, solving each body's equations once so that a definition's
  // size is the value a reference to it has — recursion, forward references and cycles included —
  // and returns what the body holds together with the definitions it introduces, named relative
  // to `prefix` (the enclosing packages and definitions). `top` marks the body of a source or
  // package, where a value definition is an inhabitant of the program rather than state derived
  // inside a class.
  def of(
      stats: List[Stat],
      scope: Scope,
      prefix: List[String],
      top: Boolean
  ): Introduced = {
    val entries = Counter.equations(stats)
    val surroundings = scope.withImports(imports(stats)).withOpen(openNames(stats))
    val solved = Counter.solve(entries, stats, surroundings.withDefinitions(entries))
    stats.foldLeft(Introduced.none) { (acc, st) =>
      val introduced = statement(solved, prefix, top)(st)
      Introduced(
        contributes = acc.contributes + introduced.contributes,
        definitions = acc.definitions ++ introduced.definitions
      )
    }
  }

  // The abstractions a body leaves open. A sealed parent is not one of them: its sum is the sum of
  // the children the body (or the package) defines.
  private[cardinality] def openNames(stats: List[Stat]): Set[TypeName] =
    stats.collect {
      case d: Defn.Trait if !d.mods.exists(_.is[Mod.Sealed]) => TypeName.of(d.name.value)
      case d: Defn.Class if d.mods.exists(_.is[Mod.Abstract]) && !d.mods.exists(_.is[Mod.Sealed]) =>
        TypeName.of(d.name.value)
    }.toSet

  // The names a body's imports bind. A direct import of a name keeps a reference to it out of the
  // library's hands — the import decides what the name means and the calculator does not follow
  // imports — while a wildcard import binds nothing by name and is left as one of the assumptions
  // `Library` documents.
  private def imports(stats: List[Stat]): Set[TypeName] =
    stats
      .collect { case i: Import => i.importers }
      .flatten
      .flatMap(_.importees)
      .flatMap {
        case Importee.Name(name)    => Some(TypeName.of(name.value))
        case Importee.Rename(_, to) => Some(TypeName.of(to.value))
        case _                      => None
      }
      .toSet

  // A statement in a body: a package opens a nested one, a definition is measured and named, and
  // every other statement — declarations, imports and exports, bare terms — introduces nothing.
  // A package also moves the read's home, which is the package the library's lookups start from.
  private def statement(scope: Scope, prefix: List[String], top: Boolean): Stat => Introduced = {
    case p: Pkg =>
      val path = p.ref.syntax.split('.').toList
      of(p.body.stats, scope.withHome(TypeName.path(p.ref.syntax)), prefix ++ path, top)
    case p: Pkg.Object =>
      of(
        p.templ.body.stats,
        scope.withHome(prefix.map(TypeName.of) :+ TypeName.of(p.name.value)),
        prefix :+ p.name.value,
        top,
      )
    case d: Defn => defnWalk(scope, prefix, top)(d)
    case _       => Introduced.none
  }

  // One definition: the row a report reads, the cardinality it contributes to its body, and the
  // definitions its own body introduces. Where a row shows something else than the contribution it
  // is because a reference to the definition is worth more than the definition itself adds: an
  // alias names another type's values, an opaque type hides them, an abstract type has none of its
  // own.
  private def defnWalk(scope: Scope, prefix: List[String], top: Boolean): Defn => Introduced = {
    case d: Defn.Class if d.mods.exists(_.is[Mod.Abstract]) =>
      abstractRow(scope, prefix, d)
        .inside(scope.withBinders(Counter.binders(d)), prefix :+ d.name.value, d.templ.body.stats)
    case d: Defn.Class =>
      measured(scope.withBinders(Counter.binders(d))) { s =>
        val size = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Class, Some(size), size, s.unbound)
          .inside(
            s.updated(TypeName.of(d.name.value), size),
            prefix :+ d.name.value,
            d.templ.body.stats
          )
      }
    case d: Defn.Trait =>
      abstractRow(scope, prefix, d)
        .inside(scope.withBinders(Counter.binders(d)), prefix :+ d.name.value, d.templ.body.stats)
    // An enum's cardinality is the sum over its cases, which the solver gives it; the enum's own
    // constructor arguments are shared state, not extra inhabitants.
    case d: Defn.Enum =>
      measured(scope.withBinders(Counter.binders(d))) { s =>
        val size = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Enum, Some(size), size, s.unbound)
          .inside(
            s.updated(TypeName.of(d.name.value), size),
            prefix :+ d.name.value,
            d.templ.body.stats
          )
      }
    // A module (including a `case object`) is a single instance. It never reads the name from the
    // scope: a companion object shares its name with a type, and the scope keeps the type's value
    // under it, so the module's own value is one here and in the row a report lists it as.
    case d: Defn.Object =>
      measured(scope) { s =>
        introduced(prefix, d, Definition.Kind.Object, Some(UnitSize), UnitSize, Nil)
          .inside(s, prefix :+ d.name.value, d.templ.body.stats)
      }
    // An opaque type hides what it holds: a reference to it is worth a single value outside the
    // scope that defines it, and the definition adds none of its own.
    case d: Defn.Type if d.mods.exists(_.is[Mod.Opaque]) =>
      measured(scope) { s =>
        introduced(prefix, d, Definition.Kind.Opaque, Some(UnitSize), Counter.defnIn(s)(d), Nil)
      }
    // A constructor alias names a *function* on types, which has no value space of its own: the
    // row is a shape, not a question, and applying it is where a size would come from.
    case d: Defn.Type if d.body.isInstanceOf[Type.Lambda] =>
      Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Constructor, None)))
    // A reference to the alias holds the aliased type's values; the alias itself adds none.
    case d: Defn.Type =>
      measured(scope.withBinders(Counter.binders(d))) { s =>
        val aliased = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Alias, Some(aliased), NothingSize, s.unbound)
      }
    // An enum case is counted by its enum; it has no body of its own.
    case _: Defn.EnumCase | _: Defn.RepeatedEnumCase => Introduced.none
    // A value at the top level of a source is one inhabitant. A value inside a class or object
    // body is state derived from the fields, which already count it, so it adds nothing.
    case d @ (_: Defn.Val | _: Defn.Var) if top =>
      measured(scope) { s =>
        val size = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Value, Some(size), size, Nil)
      }
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — nested values, methods, givens —
    // defines no inhabitants of its own.
    case _ => Introduced.none
  }

  // The row an abstract definition reads. It has no inhabitants of its own — the children do — but
  // a *sealed* parent has a solved sum, and a reference to it reads that sum, so the row shows the
  // value and the reasons of the children the sum is made of. `sealed trait Nat` with its cases in
  // the source set reads `ω`, and a parent nobody summed stays a dash.
  private def abstractRow(scope: Scope, prefix: List[String], d: Defn): Introduced = {
    val name = named(d)
    scope.definition(name) match {
      case Some(definition) if definition.children.nonEmpty =>
        measured(scope.withBinders(Counter.binders(d))) { s =>
          // The children are read here so that the reasons the sum rests on land in this row's
          // record rather than in theirs.
          definition.children.foreach(child => s.definition(child).foreach(_.equation(s)))
          introduced(prefix, d, Definition.Kind.Abstract, s.size(name), NothingSize, s.unbound)
        }
      case _ =>
        Introduced(
          NothingSize,
          List(row(prefix, d, Definition.Kind.Abstract, scope.resolve(name))),
        )
    }
  }

  // The cardinality a reference to a definition has, and the names that stopped the calculator
  // from bounding *this* definition. A definition the body's equations named — a class, enum,
  // module or alias — is read from the solved scope, so a definition and a reference to it always
  // agree; its own syntax is evaluated anyway, in a scope that records only this definition's
  // unresolved names, which is what a report row reads. Anything the equations did not name — a
  // top-level value in particular — is worth what its own syntax is worth.
  private def sizeOf(scope: Scope)(d: Defn): Size = d match {
    case t: Defn.Type =>
      val aliased = Counter.typeIn(scope)(t.body)
      scope.size(TypeName.of(t.name.value)).getOrElse(aliased)
    case other =>
      val measured = Counter.defnIn(scope)(other)
      scope.size(named(other)).getOrElse(measured)
  }

  // What a report can point at when the calculator cannot bound a type: the type's name where it
  // has one — `String`, `A`, `NonEmptyList` — and what kind of type it is where it has none, since
  // a match type or a refinement has no name to print. Anything left is described by its source
  // text on one line, so that a report row stays a row.
  private[cardinality] def describe(tpe: Type): String = tpe match {
    case name: Type.Name               => name.value
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

  // Measures one definition with a scope that records only its own reasons.
  private def measured(scope: Scope)(f: Scope => Introduced): Introduced = f(scope.measured)

  // A definition as its source sees it: the row a report lists it as, and the cardinality it
  // contributes. The row carries the definition's own reasons, kept apart from its siblings'.
  private def introduced(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      size: Option[Size],
      contributes: Size,
      unbound: List[Definition.Unbound],
  ): Introduced =
    Introduced(contributes, List(row(prefix, d, kind, size, unbound)))

  private def row(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      size: Option[Size],
      unbound: List[Definition.Unbound] = Nil,
  ): Definition =
    Definition((prefix :+ named(d).value).mkString("."), kind, params(d), size, unbound, line(d))

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
  private def named(d: Defn): TypeName = d match {
    case v: Defn.Val => TypeName.of(v.pats.map(_.syntax).mkString(", "))
    case v: Defn.Var => TypeName.of(v.pats.map(_.syntax).mkString(", "))
    case m: Member   => TypeName.of(m.name.value)
    case other       => TypeName.of(other.syntax)
  }

  // What a body holds together: what `Counter.source` reports for a source is this walk over its
  // top-level statements, and what `stat` reports for a package statement is the same walk over
  // the statements the package contains.
  def body(stats: List[Stat], scope: Scope): Size =
    of(stats, scope, Nil, top = true).contributes

  // The equations a body defines: one per named type, plus one per sealed parent the body
  // provides subtypes for. Each equation recomputes its cardinality from a scope, so the
  // system can be iterated as a whole. Abstract traits and classes have no cardinality of
  // their own and get an equation only when their subtypes appear in the same body.
  //
  // The type parameters travel with the equation, because a *reference* to a generic definition
  // is an instantiation: `Pair[Boolean]` is `Pair`'s equation read with `A` bound to 2. A sealed
  // parent keeps none: its sum is the sum of its children as the body defines them.
  //
  // A type claims its name over the module that shares it: a companion object and its class are
  // both called `Modify` (`class Modify` / `object Modify` is the usual shape in real code), but
  // only one of them is what a *type* reference means, and a companion object that won the name
  // would report a function-valued class as the single value of its module. The module keeps its
  // equation only where no type shares the name — which is what a sealed parent sums a
  // `case object` child by — and the walk counts the module itself as one value either way.
  // Behind the scope's map the modules therefore come first and the types last, so the type's
  // equation is the one a name keeps.
}
