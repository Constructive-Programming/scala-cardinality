package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.types.*

// What a source holds together, definition by definition: the walk that solves a body, measures
// every definition it introduces and names it for a report. See `Counter`.

private[cardinality] object Walk {

  // Trees are parsed and reprinted under Scala 3 dialect rules.
  private given scala3: Dialect = dialects.Scala3

  final private[cardinality] case class Introduced(
      contributes: Size,
      definitions: List[Definition]
  ) {

    /** Adds nested definitions and their inhabitants from the body of this definition; `solve` is
      * false for an object's body, which its enclosing body already solved.
      */
    def inside(
        scope: Scope,
        prefix: List[String],
        stats: List[Stat],
        solve: Boolean = true
    ): Introduced = {
      val body = of(stats, scope, prefix, top = false, solve)
      copy(
        contributes = contributes + body.contributes,
        definitions = definitions ++ body.definitions
      )
    }

  }

  private[cardinality] object Introduced {
    val none: Introduced = Introduced(NothingSize, Nil)
  }

  /** One file's statements in one package, with what that file adds to every read of them — the
    * package clauses it opens and the clauses of the bodies around them — and the file (`unit`).
    */
  final private[cardinality] case class Part(
      path: List[TypeName],
      stats: List[Stat],
      layer: Layer,
      unit: Int,
      packageObject: Option[Pkg.Object] = None
  )

  // Walks statements in source order, solving each body's equations once so a definition's size is
  // the value a reference to it has — recursion, forward references and cycles included — naming
  // definitions relative to `prefix`. `top` marks a source or package body, where a value is an
  // inhabitant of the program rather than derived state.
  def of(
      stats: List[Stat],
      scope: Scope,
      prefix: List[String],
      top: Boolean,
      solve: Boolean = true
  ): Introduced = {
    val body = if (solve) solved(stats, scope) else scope
    val clauses = Imports.before(stats)
    stats.foldLeft(Introduced.none) { (acc, st) =>
      val introduced = statement(body.within(Layer(imports = clauses(st))), prefix, top)(st)
      Introduced(
        contributes = acc.contributes + introduced.contributes,
        definitions = acc.definitions ++ introduced.definitions
      )
    }
  }

  /** The scope a body reads in: its equations solved — its objects' members with them, under their
    * keys — its term names and open abstractions recorded, and its definitions' members bound: the
    * same scope every row of the body is measured in, with the clauses written before the row's
    * statement, which `Reading` lends to the method side. A type the body defines shadows an
    * enclosing definition's parameter of that name.
    */
  private[cardinality] def solved(stats: List[Stat], scope: Scope): Scope = {
    val layer = Layer(types = typeNames(stats), terms = termsOf(stats))
    val base = Scope.recordingLens.replace(false)(scope).within(layer)
    system(List(Body(stats, identity, Imports.before(stats))), base)
  }

  /** A package's statements across files, solved as one system: package members see each other, and
    * each file's definitions read with what that file adds (its clauses and imports). A name only
    * another file defines is a package member of lower precedence than this file's wildcards.
    */
  private[cardinality] def solvedParts(parts: List[Part], scope: Scope): Scope = {
    val names = parts
      .groupMapReduce(_.unit)(p => typeNames(p.stats) ++ termsOf(p.stats).keySet)(_ ++ _)
    val all = names.values.flatten.toSet
    val shared = Layer(terms = parts.flatMap(p => termsOf(p.stats)).toMap)
    val bodies = parts.map { p =>
      val foreign = Imports(foreign = all -- names(p.unit))
      val layer = p.layer.copy(imports = p.layer.imports ++ foreign)
      Body(p.stats, _.within(layer), Imports.before(p.stats))
    }
    system(bodies, Scope.recordingLens.replace(false)(scope).within(shared))
  }

  // One body of a system: its statements, the view its own syntax reads in, and the clauses each
  // statement reads with (keyed apart in the solver's dependency graph, as the view hides them).
  final private case class Body(
      stats: List[Stat],
      view: Scope => Scope,
      clauses: Stat => Imports
  ) {
    val modules: List[Namespaces.Module] = Namespaces.modules(stats)

    private val defined = statements(stats)

    def viewAt(st: Stat)(scope: Scope): Scope = view(scope).within(Layer(imports = clauses(st)))

    def viewOf(name: TypeName)(scope: Scope): Scope =
      defined.get(name).fold(view(scope))(viewAt(_)(scope))

  }

  // Solving reads no row's reasons: a row re-reads its own syntax once the body is solved. Every
  // equation reads with the binders of the place it was written and with its own statement's view,
  // and knows itself (`Scope.readingItself`), whoever reads it.
  private def system(bodies: List[Body], base: Scope): Scope = {
    val stats = bodies.flatMap(_.stats)
    val views = bodies.flatMap(b => Namespaces.views(b.modules, b.viewAt))
    def keys(of: List[Stat] => Set[TypeName]): Set[TypeName] =
      of(stats) ++ views.flatMap((m, _) => of(m.stats).map(m.key))
    val families = SealedFamilies
      .of(
        bodies.map(_.stats),
        bodies.flatMap(_.modules),
        base.library.synonyms,
        sites(bodies, views)
      )
    val withNames = Function.chain(
      List(
        Scope.openLens.modify(_ ++ keys(openNames)),
        Scope.membersOfLens.modify(_ ++ bodies.flatMap(b => instanceMembers(b.stats))),
        Scope.unsummedLens.modify(_ ++ SealedFamilies.unsummed(families)),
        Scope.termsLens.modify(_ ++ views.flatMap(_._1.keyedTerms)),
        Scope.refusedLens.modify(_ ++ views.flatMap(_._1.refusals)),
      )
    )(base)
    val site = Scope.bindersLens.get(withNames)
    val entries = bodies.flatMap { b =>
      Counter.equations(b.stats).map { (name, eq) =>
        val at: Scope => Scope = b.viewOf(name)
        (name -> within(eq, at)) -> at
      } ++ Namespaces.lifted(b.modules, b.viewAt)
    } ++ SealedFamilies.equations(families).map(_ -> (identity: Scope => Scope))
    val marked = entries.map { case ((name, eq), _) => name -> mark(eq, site) }
    val surroundings = Scope.definitionsLens.modify(marked.toMap ++ _)(withNames)
    val shapes = bodies
      .map(b => Namespaces.shapes(b.stats, b.modules, b.clauses))
      .foldLeft(Shapes(Map.empty, Map.empty, SealedFamilies.edges(families)))(_ ++ _)
    val arms = entries.map { case ((name, _), view) => name -> view }.toMap
    def solve(around: Scope): Scope =
      Solver.solve(marked, shapes, around, name => arms.getOrElse(name, identity))
    // A body's types contribute their *members* as well, so a value of one can be qualified
    // (`x.B`), as the library does across files; an object's types under their keys. A member's
    // own syntax reads in the body of the type that declares it.
    def members(over: => Scope): List[(TypeName, Members)] =
      bodies.flatMap(b => typeMembers(b.stats, identity, st => () => b.viewAt(st)(over))) ++
        views.flatMap((m, view) => typeMembers(m.stats, m.key, st => () => m.at(st)(view(over))))
    def holding(scope: Scope, owned: List[(TypeName, Members)]): Scope =
      owned.foldLeft(scope) {
        case (s, (name, members)) => Scope.membersLens.modify(_.updated(name, members))(s)
      }
    val first = solve(surroundings)
    // An equation that reads a member of one of the body's own classes through a value (`q.X`),
    // or one an object inherits from them, reads it in a second solve, against the members of the
    // first: the first solve has no class body to read it in yet.
    val solved =
      if (!stats.exists(declaresTypes) && !Inheritance.fromBody(stats)) first
      else {
        lazy val provisional: Scope = holding(first, members(provisional))
        solve(holding(surroundings, members(provisional)))
      }
    lazy val result: Scope = holding(solved, members(result))
    result
  }

  // The view each statement of a system reads its own syntax in: its body's, at the statement, or
  // its object's.
  private def sites(
      bodies: List[Body],
      views: List[(Namespaces.Module, Scope => Scope)]
  ): Stat => Option[Scope => Scope] = {
    val at = new java.util.IdentityHashMap[Stat, Scope => Scope]
    bodies.foreach(b => b.stats.foreach(st => at.put(st, b.viewAt(st))))
    views.foreach((m, view) => m.stats.foreach(st => at.put(st, sc => m.at(st)(view(sc)))))
    st => Option(at.get(st))
  }

  // Whether a statement is a class, trait or enum whose body declares a type member.
  private def declaresTypes(st: Stat): Boolean = st match {
    case d: (Defn.Class | Defn.Trait | Defn.Enum) =>
      template(d).exists(_.isInstanceOf[Defn.Type | Decl.Type])
    case o: Defn.Object => template(o).exists(declaresTypes)
    case _              => false
  }

  private def within(named: Named, view: Scope => Scope): Named =
    named.viewed(view)

  // An equation that reads with `site`'s binders and knows itself, so a reference to it with its
  // own parameters inside its own equation is a recursion — and the same spelling anywhere else is
  // not.
  private def mark(named: Named, site: List[Binder]): Named = {
    lazy val self: Named = named.copy(
      equation = sc =>
        named.equation(
          Scope.selfLens.replace(Some(Itself.of(self)))(Scope.bindersLens.replace(site)(sc))
        ),
      read = sc => named.read(Scope.bindersLens.replace(site)(sc))
    )
    self
  }

  /** Each package body of a source (`unit`), flattened to the path an editor shows (`package a`
    * then `package b` is `a.b`), with what the file adds to reads inside it: chained clauses open
    * each enclosing package, and the clauses written before a nested package body reach it.
    */
  private[cardinality] def parts(stats: List[Stat], unit: Int): List[Part] =
    parts(stats, Nil, Nil, Imports.none, unit, None)

  private def parts(
      stats: List[Stat],
      prefix: List[String],
      opened: List[List[TypeName]],
      outer: Imports,
      unit: Int,
      packageObject: Option[Pkg.Object]
  ): List[Part] = {
    val clauses = Imports.before(stats)
    val here = Layer(imports = outer, opened = Some(opened))
    val own = stats.filter(packageBody(prefix, _).isEmpty)
    Part(prefix.map(TypeName.of), own, here, unit, packageObject) ::
      stats.flatMap(st =>
        packageBody(prefix, st).toList.flatMap((path, inner) =>
          parts(
            inner,
            path,
            path.map(TypeName.of) :: opened,
            outer ++ clauses(st),
            unit,
            Some(st).collect { case o: Pkg.Object => o }
          )
        )
      )
  }

  /** A package clause's or a package object's body: the package path it is read in, and its
    * statements.
    */
  private[cardinality] def packageBody(
      prefix: List[String],
      st: Stat
  ): Option[(List[String], List[Stat])] =
    st match {
      case p: Pkg        => Some((prefix ++ p.ref.syntax.split('.').toList, p.body.stats))
      case p: Pkg.Object => Some((prefix :+ p.name.value, p.templ.body.stats))
      case _             => None
    }

  // The type names a body defines, an abstract type member among them: it shadows every outer
  // type of its name, and each implementor chooses what it holds.
  private[cardinality] def typeNames(stats: List[Stat]): Set[TypeName] =
    statements(stats).keySet ++ abstractTypes(stats)

  private def abstractTypes(stats: List[Stat]): Set[TypeName] =
    stats.collect { case t: Decl.Type => TypeName.of(t.name.value) }.toSet

  // The abstract type members a class, trait or enum template declares. Inside the template `M` is
  // `this.M`, which each instance chooses, so a row reading it waits for that choice as for a
  // parameter; an object's abstract member has one instance and stays open.
  private def instanceMembers(stats: List[Stat]): Set[TypeName] =
    stats.headOption.flatMap(owner) match {
      case Some(_: (Defn.Class | Defn.Trait | Defn.Enum)) => abstractTypes(stats)
      case _                                              => Set.empty
    }

  // The definition whose body a statement is in, if any.
  @tailrec
  private def owner(tree: Tree): Option[Defn] = tree.parent match {
    case Some(d: Defn) => Some(d)
    case Some(other)   => owner(other)
    case None          => None
  }

  /** Each type a body defines, by the statement that defines it. */
  private[cardinality] def statements(stats: List[Stat]): Map[TypeName, Stat] =
    stats.collect {
      case d: (Defn.Class | Defn.Trait | Defn.Enum | Defn.Type) => named(d) -> d
    }.toMap

  private def typeMembers(
      stats: List[Stat],
      key: TypeName => TypeName,
      around: Stat => () => Scope
  ): List[(TypeName, Members)] =
    stats.collect {
      case d: (Defn.Class | Defn.Trait | Defn.Enum) =>
        key(named(d)) -> members(d, key(named(d)), around(d))
    }

  // What a definition's own body reads with: its type parameters, declared members, and term
  // names — all scoped to that body, innermost over the enclosing ones.
  private def declared(scope: Scope, d: Defn): Scope = {
    val withBinders =
      Scope.bindersLens
        .modify(Counter.binders(d) ++ _)(Inheritance.enter(scope, d, terms(d).keySet))
    // An object is a namespace, not a type: its members are read through its key, not its name.
    val withMembers = d match {
      case _: Defn.Object => withBinders
      case _              =>
        val owned = members(d, named(d), () => scope)
        Scope.membersLens.modify(_.updated(named(d), owned))(withBinders)
    }
    Scope.termsLens.modify(_ ++ terms(d))(withMembers)
  }

  /** The scope a definition's body reads in, entered as the walk enters it — an object through its
    * view, a class or enum with its own value bound, any of them with its binders — with `solve`
    * standing in for solving the body's statements.
    */
  private[cardinality] def bodyScope(scope: Scope, d: Defn)(
      solve: (List[Stat], Scope) => Scope
  ): Scope = {
    val own = Scope.bindersLens.modify(Counter.binders(d) ++ _)(scope)
    d match {
      case o: Defn.Object => moduleView(scope, o).getOrElse(solve(template(d), declared(scope, d)))
      case c: Defn.Class if Counter.isConcreteClass(c) =>
        solve(template(d), declared(own, d).updated(named(d), sizeOf(own)(d)))
      case e: Defn.Enum =>
        val body = EnumCompanions.bodyScope(Inheritance.enter(own, e), e)
        solve(template(d), body.updated(named(d), sizeOf(own)(d)))
      case _ => solve(template(d), declared(scope, d))
    }
  }

  // An object's body reads in its view of the enclosing body, which solved its members under
  // their keys: its rows report the values a qualified reference from outside reads.
  private[capacity] def moduleView(scope: Scope, d: Defn.Object): Option[Scope] =
    scope.term(named(d)).collect {
      case Bound.Module(prefix, _) => Namespaces.Module(named(d), prefix, d).view(scope)
    }

  // The type members a body declares: what a qualified reference can resolve to. A member with a
  // body reads as written, in the body that declares it; an abstract one is supplied by
  // implementors. `around` is the scope the definition is written in.
  private[cardinality] def members(d: Defn, owner: TypeName, around: () => Scope): Members = {
    val stats = template(d)
    new Members(
      owner,
      Counter.parameterNames(d),
      stats.collect {
        case t: Defn.Type if !t.mods.exists(_.is[Mod.Opaque]) => TypeName.of(t.name.value) -> t.body
      }.toMap,
      stats.collect { case t: Decl.Type => TypeName.of(t.name.value) }.toSet,
      d,
      around,
    )
  }

  /** The term names a definition's body binds: its constructor parameters, with their declared
    * types, its self alias (`self: Prism =>`, of the type it declares; `self =>` is `this`, of no
    * type read here), and the members its template binds (`termsOf`).
    */
  private[cardinality] def terms(d: Defn): Map[TypeName, Bound] =
    parameters(d).toMap ++ selfAlias(d) ++ termsOf(template(d))

  private def selfAlias(d: Defn): Option[(TypeName, Bound)] =
    templateOf(d)
      .flatMap(_.body.selfOpt)
      .filter(s => s.name.value.nonEmpty && s.name.value != "_")
      .map(s => TypeName.of(s.name.value) -> Bound.Value(s.decltpe))

  /** The value parameters a constructor or a method binds, each with its declared type. */
  private[cardinality] def parameters(d: Defn): List[(TypeName, Bound)] = {
    val params = d match {
      case c: (Defn.Class | Defn.Trait | Defn.Enum) => c.ctor.paramClauses.flatMap(_.values)
      case m: Defn.Def => m.paramClauseGroups.flatMap(_.paramClauses).flatMap(_.values)
      case _           => Nil
    }
    params.toList.map(p => TypeName.of(p.name.value) -> Bound.Value(p.decltpe))
  }

  /** The term names a body binds, each with what it denotes: an object, under `key`'s prefix; a
    * value, with the type it declares — or, for `val x = new C`, the class it constructs — or none,
    * which no qualified reference reads through; a method or a given.
    */
  private[cardinality] def termsOf(
      stats: List[Stat],
      key: TypeName => TypeName = identity
  ): Map[TypeName, Bound] =
    stats.flatMap {
      case o: Defn.Object =>
        List(
          named(o) -> Bound.Module(key(named(o)), Exposure.of(o.templ.body.stats, o.templ.inits))
        )
      case v: Defn.Val => values(v.pats, v.decltpe.orElse(constructed(v.rhs)))
      case v: Defn.Var => values(v.pats, v.decltpe)
      case v: Decl.Val => values(v.pats, Some(v.decltpe))
      case d: Defn.Def => List(TypeName.of(d.name.value) -> Bound.Value(None))
      case d: Decl.Def => List(TypeName.of(d.name.value) -> Bound.Value(None))
      case g: Defn.GivenAlias if g.name.value.nonEmpty =>
        List(TypeName.of(g.name.value) -> Bound.Value(None))
      case _ => Nil
    }.toMap

  // The names a `val` binds. A single variable has the declared or constructed type; every
  // variable of a tuple, extractor or typed pattern is a value whose type is not read here (a typed
  // one, `(p: Prism, n)`, has the type it is ascribed).
  private def values(pats: List[Pat], known: Option[Type]): List[(TypeName, Bound)] =
    pats.flatMap {
      case Pat.Var(name) => List(TypeName.of(name.value) -> Bound.Value(known))
      case pattern       => variables(pattern)
    }

  private def variables(pattern: Tree): List[(TypeName, Bound)] = pattern match {
    case Pat.Typed(Pat.Var(name), tpe) => List(TypeName.of(name.value) -> Bound.Value(Some(tpe)))
    case Pat.Var(name)                 => List(TypeName.of(name.value) -> Bound.Value(None))
    case other                         => other.children.flatMap(variables)
  }

  // The class `new C(...)` constructs, without a refinement: the value's type, read syntactically.
  private def constructed(rhs: Term): Option[Type] = rhs match {
    case Term.New(init) => Some(init.tpe)
    case _              => None
  }

  /** The members a definition's body holds; a value or an enum case declares none. */
  private[capacity] def template(d: Defn): List[Stat] = templateOf(d).fold(Nil)(_.body.stats)

  private[capacity] def templateOf(d: Defn): Option[Template] = d match {
    case c: (Defn.Class | Defn.Trait | Defn.Enum | Defn.Object) => Some(c.templ)
    case _                                                      => None
  }

  // The abstractions a body leaves open — an abstract type member among them, whose value each
  // implementor chooses; a sealed parent is not one, its sum being the children's.
  private def openNames(stats: List[Stat]): Set[TypeName] =
    abstractions(stats) ++ abstractTypes(stats)

  // The unsealed traits and abstract classes a body declares.
  private def abstractions(stats: List[Stat]): Set[TypeName] =
    stats.collect {
      case d: Defn.Trait if !Counter.isSealed(d)                               => named(d)
      case d: Defn.Class if Counter.isAbstractClass(d) && !Counter.isSealed(d) => named(d)
    }.toSet

  // A statement in a body: a package clause opens its package (where the library's lookups start,
  // innermost clause first: `package a; package b` sees `a.b`'s members and `a`'s, a single
  // `package a.b` only `a.b`'s), a definition is measured and named, the rest introduce nothing.
  private def statement(scope: Scope, prefix: List[String], top: Boolean)(st: Stat): Introduced =
    packageBody(prefix, st) match {
      case Some((path, stats)) => of(stats, opening(scope, path), path, top)
      case None                =>
        st match {
          case d: Defn => defnWalk(scope, prefix, top)(d)
          case _       => Introduced.none
        }
    }

  private[cardinality] def opening(scope: Scope, path: List[String]): Scope =
    Scope.openedLens.modify(path.map(TypeName.of) :: _)(scope)

  // One definition: the row a report reads, what it contributes to its body, and what its own
  // body introduces. A row may show more than the contribution — an alias names another type's
  // values, an opaque type hides them, an abstract type has none of its own.
  private def defnWalk(scope: Scope, prefix: List[String], top: Boolean): Defn => Introduced = {
    case d: Defn.Class if Counter.isAbstractClass(d) => openRow(scope, prefix, d)
    case d: Defn.Class                               =>
      sizedRow(scope, prefix)(d) { (s, size) =>
        declared(s, d).updated(named(d), size)
      }
    case d: Defn.Trait => openRow(scope, prefix, d)
    // An enum's cardinality is the sum over its cases, which the solver gives it; its own
    // constructor arguments are shared state.
    case d: Defn.Enum =>
      sizedRow(scope, prefix)(d) { (s, size) =>
        EnumCompanions.bodyScope(Inheritance.enter(s, d), d).updated(named(d), size)
      }
    // A module (including a `case object`) is a single instance: a term, never a type, so a
    // companion's value is its own and the type keeps the name.
    case d: Defn.Object =>
      measured(scope, d) { s =>
        val row = introduced(prefix, d, Definition.Kind.Object, Nil, Some(UnitSize), UnitSize)
        moduleView(s, d) match {
          case Some(view) => row.inside(view, prefix :+ named(d).value, template(d), solve = false)
          case None       => row.inside(declared(s, d), prefix :+ named(d).value, template(d))
        }
      }
    // An opaque type's row reads its representation here, where it is visible — `|A|` for
    // `opaque type Direct[X, A] = A` — and adds no inhabitants of its own; a reference reads the
    // representation only where it is visible too, and is one value elsewhere (`OpaqueScopes`).
    case d: Defn.Type if Counter.isOpaqueAlias(d) =>
      measured(scope, d) { s =>
        val represented = Counter.typeIn(s)(d.body)
        introduced(
          prefix,
          d,
          Definition.Kind.Opaque,
          s.unbound,
          Some(represented),
          Counter.defnIn(s)(d)
        )
      }
    // A constructor alias names a *function* on types: the row is a shape, not a question.
    case d: Defn.Type if d.body.isInstanceOf[Type.Lambda] =>
      Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Constructor, None)))
    // A reference to the alias holds the aliased type's values; the alias itself adds none.
    case d: Defn.Type =>
      measured(scope, d) { s =>
        val aliased = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Alias, s.unbound, Some(aliased), NothingSize)
      }
    // An enum case is counted by its enum; it has no body of its own.
    case _: Defn.EnumCase | _: Defn.RepeatedEnumCase => Introduced.none
    // A top-level value holds one value of its declared type, none when that type is empty (and
    // carries the type's reasons); inside a body it is derived state, counting nothing extra.
    case d @ (_: Defn.Val | _: Defn.Var) if top =>
      measured(scope, d) { s =>
        val size = Counter.defnIn(s)(d)
        introduced(prefix, d, Definition.Kind.Value, s.unbound, Some(size), size)
      }
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — nested values, methods, givens —
    // defines no inhabitants of its own.
    case _ => Introduced.none
  }

  // The row an abstract definition reads: no inhabitants of its own, but a *sealed* parent shows
  // the solved sum of its children (`sealed trait Nat` reads `ω`), with the reasons its equation
  // reads — its own parameters and its children's; a sealed parent the system does not sum names
  // that, and why; an open one stays a dash.
  private def abstractRow(scope: Scope, prefix: List[String], d: Defn): Introduced = {
    val name = named(d)
    scope.definition(name) match {
      case Some(family) =>
        measured(scope, d) { s =>
          family.equation(s)
          introduced(prefix, d, Definition.Kind.Abstract, s.unbound, s.size(name), NothingSize)
        }
      case _ if scope.unsummed(name).isDefined =>
        val reason = List(Scope.unsummedReason(name, scope.unsummed(name)))
        Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Abstract, None, reason)))
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

  // Measures one definition with a scope that records only its own reasons, reading as the
  // definition's own equation does — so a reference to itself is its recursion — and noting a
  // lazy cycle the solver could not complete.
  private def measured(scope: Scope, d: Defn)(f: Scope => Introduced): Introduced = {
    val own = Scope.selfLens.replace(scope.definition(named(d)).map(Itself.of))(scope)
    val s = Counter.signature(d)(own).measured
    s.blocking(named(d)).foreach(s.noteBlocked(named(d), _, s.definition(named(d))))
    f(s)
  }

  // A definition as its source sees it: the row a report lists it as, and the cardinality it
  // contributes. The row carries the definition's own reasons, kept apart from its siblings'.
  private def introduced(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      unbound: List[Definition.Unbound],
      size: Option[Size],
      contributes: Size,
  ): Introduced =
    Introduced(contributes, List(row(prefix, d, kind, size, unbound)))

  // A concrete class or enum: measured with its binders in scope, contributing the solved size
  // to its body; the body's own definitions are introduced beneath the updated scope.
  private def sizedRow(scope: Scope, prefix: List[String])(
      d: Defn.Class | Defn.Enum
  )(withSize: (Scope, Size) => Scope): Introduced =
    measured(scope, d) { s =>
      val size = sizeOf(s)(d)
      val kind = d match
        case _: Defn.Class => Definition.Kind.Class
        case _: Defn.Enum  => Definition.Kind.Enum
      introduced(prefix, d, kind, s.unbound, Some(size), size)
        .inside(withSize(s, size), instances(prefix, d), template(d))
    }

  // An abstraction the body leaves open: its row reads through `abstractRow`, and its declared
  // members carry into the body below.
  private def openRow(scope: Scope, prefix: List[String], d: Defn): Introduced =
    abstractRow(scope, prefix, d).inside(declared(scope, d), instances(prefix, d), template(d))

  // The prefix a class, trait or enum body names its members under. They are members of each
  // instance, which Scala writes `O#T`; a member of an object or package is `O.T`. So a class and
  // its companion that both declare `T` give two rows with two names. An enum's cases belong to
  // its companion, but they have no rows of their own. The instance mark is a segment of its own,
  // which no name can be, so a class `+` (`+#T`) and an object `+#` (`+#.T`) stay apart.
  private def instances(prefix: List[String], d: Defn): List[String] =
    prefix :+ named(d).value :+ Instance

  private val Instance = ""

  private def row(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      size: Option[Size],
      unbound: List[Definition.Unbound] = Nil,
  ): Definition =
    Definition(
      qualified(prefix :+ named(d).value),
      kind,
      Counter.parameters(d).map(_.name.value),
      size,
      unbound,
      d.pos.startLine + 1
    )

  // Path segments joined by `.`; the instance mark reads `#` and joins its owner to its member.
  private def qualified(segments: List[String]): String =
    segments
      .zip(None :: segments.map(Some(_)))
      .map {
        case (Instance, _)             => "#"
        case (segment, None)           => segment
        case (segment, Some(Instance)) => segment
        case (segment, _)              => s".$segment"
      }
      .mkString

  // Values and variables are named by the patterns they bind; every other definition the report
  // lists carries its name directly. `Defn` is not sealed, so the last case stands in for the
  // members the walk never names.
  private[cardinality] def named(d: Defn): TypeName = d match {
    case v: (Defn.Val | Defn.Var) => TypeName.of(v.pats.map(Spelling.of).mkString(", "))
    case m: Member                => TypeName.of(m.name.value)
    case other                    => TypeName.of(Spelling.of(other))
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
  // A type owns its name; a module is a term. A companion object and its class are both called
  // `Modify`, but only the class is what a *type* reference means, and a companion that took the
  // name would report a function-valued class — or an open trait — as the single value of its
  // module. So objects get no equation at all: the walk counts each as one value, a sealed parent
  // sums a `case object` child as one, and `O.T` reads the members its body solved under keys.
}
