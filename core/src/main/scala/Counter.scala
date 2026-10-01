package cardinality

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.*

object Counter {

  // Trees are parsed as Scala 3; printing one with any other dialect makes scalameta reprint it
  // under Scala 2 rules, which cannot spell Scala 3's modifiers at all.
  private given scala3: Dialect = dialects.Scala3

  /** What every definition a source introduces holds together: the sum over the concrete classes,
    * enums, modules and top-level values it defines, nested definitions included. The other
    * supplied sources are in scope too, the way `definitions` reads them.
    */
  def source(s: Source, library: Library = Library.empty): Size =
    body(s.stats, Scope.empty.withLibrary(library))

  /** Every definition a source introduces, in source order, each with the cardinality of its type
    * and the names that stopped the calculator from bounding it — the number and the reason.
    *
    * `library` is what the other supplied sources define: a report reads a library's sources as one
    * set, so a type defined in a sibling file resolves instead of counting as an unknown name, and
    * `Library.empty` reads the source on its own.
    */
  def definitions(s: Source, library: Library = Library.empty): List[Definition] =
    walk(s.stats, Scope.empty.withLibrary(library), Nil, top = true).definitions

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

  // The names of the types a source defines, each mapped to its cardinality, so that a field can
  // refer to a sibling, forward or recursive definition by name. The system of equations is
  // solved by Kleene iteration from the empty type — see `solve` — so the scope is a solved
  // one: the walk hands every definition the value a reference to it has, not a fresh reading of
  // the same syntax. The scope also collects the names it could not resolve, which is what lets
  // a report say why a size is unbounded instead of just printing ω.
  //
  // The notes are shared by the scopes one body's iteration derives from — the solver threads a
  // scope through every equation — while `measured` starts a fresh record for a definition whose
  // reasons are about to be read, so one definition's reasons are not read as another's.
  //
  // Besides the solved values, a scope carries what a *generic* reference needs: the definitions
  // themselves (`definitions`, each with its type parameters and its equation), the frames those
  // parameters are bound in while an instantiation is being read, and the names whose
  // substitution is in progress. See `applied`.
  /** What a read carries besides the names it is solving: the definitions of the bodies in scope,
    * the instantiation frames, the names whose substitution is in progress, the library the file
    * leans on, the package it is read in, and the names its bodies import.
    */
  final private case class World(
      definitions: Map[String, Named] = Map.empty,
      frames: List[Map[String, Size]] = Nil,
      active: Set[String] = Set.empty,
      library: Library = Library.empty,
      home: List[String] = Nil,
      imports: Set[String] = Set.empty,
  )

  final private class Scope(
      private val values: Map[String, Size],
      private val notes: mutable.LinkedHashSet[String],
      private val world: World,
  ) {

    private def definitions: Map[String, Named] = world.definitions

    private def frames: List[Map[String, Size]] = world.frames

    private def active: Set[String] = world.active

    private def library: Library = world.library

    private def home: List[String] = world.home

    private def imports: Set[String] = world.imports

    def size(name: String): Option[Size] = values.get(name)

    def contains(name: String): Boolean = values.contains(name)

    def apply(name: String): Size = values(name)

    def getOrElse(name: String, default: => Size): Size = values.getOrElse(name, default)

    /** What a type parameter stands for in the instantiation being read, innermost binder first. */
    def frame(name: String): Option[Size] = frames.collectFirst(Function.unlift(_.get(name)))

    /** The definition a name resolves to, so that `C[args]` can be read as an instantiation. */
    def definition(name: String): Option[Named] = definitions.get(name)

    /** The value a name has: the file's own, or the library's when the file defines none. A name
      * the body imports is left to the import: the calculator does not follow imports, so a library
      * lookup for it would be a guess at what the import binds.
      */
    def resolve(name: String): Option[Size] =
      values.get(name).orElse(if (imported(name)) None else library.value(name, home))

    /** The same for a definition the library supplied, with the package frame it was read in. */
    def libraryDefinition(name: String): Option[(Scope, Named)] =
      if (imported(name)) None else library.definition(name, home)

    def imported(name: String): Boolean = imports(name)

    /** The names the package the current read is in defines, for the library's own lookups. */
    def defines(name: String): Boolean = values.contains(name) || definitions.contains(name)

    def definedNames: Set[String] = values.keySet ++ definitions.keySet

    def updated(name: String, size: Size): Scope =
      new Scope(values.updated(name, size), notes, world)

    /** Names added by the solver's own round, which carries no notes of its own. */
    def ++(entries: Iterable[(String, Size)]): Scope =
      new Scope(values ++ entries, notes, world)

    /** The definitions the bodies now in scope introduce; an inner body shadows an outer name. */
    def withDefinitions(entries: List[(String, Named)]): Scope =
      new Scope(values, notes, world.copy(definitions = entries.toMap ++ definitions))

    /** The package path a reference is read in, which decides what the library lends it. */
    def withHome(path: List[String]): Scope =
      new Scope(values, notes, world.copy(home = path))

    /** The names a body imports; an inner import shadows an outer name for the whole body. */
    def withImports(names: Set[String]): Scope =
      new Scope(values, notes, world.copy(imports = imports ++ names))

    /** The definitions of other sources this read can lean on. */
    def withLibrary(other: Library): Scope =
      new Scope(values, notes, world.copy(library = other))

    /** Reading a definition the library supplied: its own package's names replace the file's
      * lexical ones, so its references resolve where it was written, while the reasons it records
      * stay the reading row's.
      */
    def inPackage(pkg: Scope): Scope =
      new Scope(
        pkg.values,
        notes,
        world.copy(definitions = pkg.definitions, library = pkg.library, home = pkg.home),
      )

    /** Reading one instantiation of a definition: its parameters bound to what the arguments are
      * worth, innermost frame first.
      */
    def instantiated(frame: Map[String, Size]): Scope =
      new Scope(values, notes, world.copy(frames = frame :: frames))

    /** Marking a name whose substitution is in progress, so a cycle through an applied reference
      * terminates the way a cycle through a bare name does.
      */
    def substituting(name: String): Scope =
      new Scope(values, notes, world.copy(active = active + name))

    def isSubstituting(name: String): Boolean = active(name)

    /** The same names with a fresh record of unresolved ones. */
    def measured: Scope =
      new Scope(values, mutable.LinkedHashSet.empty, world)

    /** The names this scope could not bound, sorted for a report. */
    def unresolved: List[String] = notes.toList.sorted

    /** Records a type the calculator could not bound: an unknown name or an unmodelled type
      * constructor.
      */
    def note(tpe: Type): Unit = { notes += describe(tpe); () }

    /** Records a name directly, for a reason that is not a type's own syntax. */
    def note(name: String): Unit = { notes += name; () }
  }

  private object Scope {

    val empty: Scope = new Scope(Map.empty, mutable.LinkedHashSet.empty, World())

  }

  /** One named definition a reference can resolve: the type parameters it declares, and what its
    * type is worth with those parameters in scope.
    */
  final private case class Named(params: List[String], equation: Scope => Size)

  /** The definitions the other supplied sources introduce, so that a reference can leave the file
    * it is written in: a report reads a library's published sources as one set, and a type in a
    * sibling file resolves instead of counting as an unknown name.
    *
    * A name a *single* package of the source set defines resolves to that definition, whatever
    * package the reference is written in — the shape an import usually has. A name two packages
    * define stays unresolved rather than guessed, because an import could mean either. What the
    * sources do not contain is not modelled: a name only a classpath dependency defines still
    * reports as unresolved, and an import of the same spelling from outside the sources is the one
    * assumption a resolved name rests on.
    *
    * Each package's top-level definitions are solved as one system, so a definition can reference a
    * sibling file's names, and a sealed hierarchy that spans files sums as a whole.
    */
  final class Library private (
      private val packages: Map[List[String], Scope],
      private val unique: Map[String, Scope]
  ) {

    /** The value a name defined outside the file being read has, when the library defines it. */
    private[Counter] def value(name: String, home: List[String]): Option[Size] =
      frame(name, home).flatMap(_.size(name))

    /** The definition such a name resolves to, with the package it was read in. */
    private[Counter] def definition(name: String, home: List[String]): Option[(Scope, Named)] =
      frame(name, home).flatMap(pkg => pkg.definition(name).map(pkg -> _))

    // The package a name read in `home` resolves to: its own when it defines the name — package
    // members are in scope without an import — else the one package across the source set that
    // does, which is the shape an import of a single name has.
    private def frame(name: String, home: List[String]): Option[Scope] =
      packages.get(home).filter(_.defines(name)).orElse(unique.get(name).filter(_.defines(name)))

  }

  object Library {

    /** No other sources: a source read on its own, as a single file is. */
    val empty: Library = new Library(Map.empty, Map.empty)

    /** Reads the top-level definitions of every source, grouped by the package they are read in,
      * and solves each package's names as one system.
      */
    def of(sources: List[Source]): Library = {
      val packages = packageStatements(sources).groupBy(_._1).map {
        case (path, found) =>
          val stats = found.flatMap(_._2)
          val entries = equations(stats)
          path -> solve(entries, stats, Scope.empty.withDefinitions(entries).withHome(path))
      }
      val unique = packages.values.toList
        .flatMap(scope => scope.definedNames.toList.map(name => name -> scope))
        .groupBy(_._1)
        .collect { case (name, List((_, scope))) => name -> scope }
      new Library(packages, unique)
    }

    // A source's statements grouped by the package they are read in, nested packages flattened to
    // the path an editor shows (`package a` then `package b` is `a.b`).
    private def packageStatements(sources: List[Source]): List[(List[String], List[Stat])] =
      sources.flatMap(source => packageStatements(source.stats, Nil))

    private def packageStatements(
        stats: List[Stat],
        prefix: List[String]
    ): List[(List[String], List[Stat])] = {
      val direct = stats.filterNot(st => st.is[Pkg] || st.is[Pkg.Object])
      val nested = stats.flatMap {
        case p: Pkg        => packageStatements(p.body.stats, prefix ++ p.ref.syntax.split('.'))
        case p: Pkg.Object => packageStatements(p.templ.body.stats, prefix :+ p.name.value)
        case _             => Nil
      }
      (prefix -> direct) :: nested
    }

  }

  // What one statement adds to its body: the cardinality it contributes and the definitions it
  // introduces, which a report lists.
  final private case class Introduced(contributes: Size, definitions: List[Definition]) {

    /** Adds what the body of this definition introduces. Nested definitions are their own
      * inhabitants — and their own rows — so they add to both: `object Wrapper { case class Pair(a:
      * Boolean, b: Boolean) }` holds five values, the module and the four pairs.
      */
    def inside(scope: Scope, prefix: List[String], stats: List[Stat]): Introduced = {
      val body = walk(stats, scope, prefix, top = false)
      copy(
        contributes = contributes + body.contributes,
        definitions = definitions ++ body.definitions
      )
    }

  }

  private object Introduced {
    val none: Introduced = Introduced(NothingSize, Nil)
  }

  // Walks statements in source order, solving each body's equations once so that a definition's
  // size is the value a reference to it has — recursion, forward references and cycles included —
  // and returns what the body holds together with the definitions it introduces, named relative
  // to `prefix` (the enclosing packages and definitions). `top` marks the body of a source or
  // package, where a value definition is an inhabitant of the program rather than state derived
  // inside a class.
  private def walk(
      stats: List[Stat],
      scope: Scope,
      prefix: List[String],
      top: Boolean
  ): Introduced = {
    val entries = equations(stats)
    val imported = scope.withImports(imports(stats))
    val solved = solve(entries, stats, imported.withDefinitions(entries))
    stats.foldLeft(Introduced.none) { (acc, st) =>
      val introduced = statement(solved, prefix, top)(st)
      Introduced(
        contributes = acc.contributes + introduced.contributes,
        definitions = acc.definitions ++ introduced.definitions
      )
    }
  }

  // The names a body's imports bind. A direct import of a name keeps a reference to it out of the
  // library's hands — the import decides what the name means and the calculator does not follow
  // imports — while a wildcard import binds nothing by name and is left as one of the assumptions
  // `Library` documents.
  private def imports(stats: List[Stat]): Set[String] =
    stats
      .collect { case i: Import => i.importers }
      .flatten
      .flatMap(_.importees)
      .flatMap {
        case Importee.Name(name)    => Some(name.value)
        case Importee.Rename(_, to) => Some(to.value)
        case _                      => None
      }
      .toSet

  // A statement in a body: a package opens a nested one, a definition is measured and named, and
  // every other statement — declarations, imports and exports, bare terms — introduces nothing.
  // A package also moves the read's home, which is the package the library's lookups start from.
  private def statement(scope: Scope, prefix: List[String], top: Boolean): Stat => Introduced = {
    case p: Pkg =>
      val path = p.ref.syntax.split('.').toList
      walk(p.body.stats, scope.withHome(path), prefix ++ path, top)
    case p: Pkg.Object =>
      walk(p.templ.body.stats, scope.withHome(prefix :+ p.name.value), prefix :+ p.name.value, top)
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
      Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Abstract, None)))
        .inside(scope, prefix :+ d.name.value, d.templ.body.stats)
    case d: Defn.Class =>
      measured(scope) { s =>
        val size = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Class, Some(size), size, s.unresolved)
          .inside(s.updated(d.name.value, size), prefix :+ d.name.value, d.templ.body.stats)
      }
    case d: Defn.Trait =>
      Introduced(NothingSize, List(row(prefix, d, Definition.Kind.Abstract, None)))
        .inside(scope, prefix :+ d.name.value, d.templ.body.stats)
    // An enum's cardinality is the sum over its cases, which the solver gives it; the enum's own
    // constructor arguments are shared state, not extra inhabitants.
    case d: Defn.Enum =>
      measured(scope) { s =>
        val size = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Enum, Some(size), size, s.unresolved)
          .inside(s.updated(d.name.value, size), prefix :+ d.name.value, d.templ.body.stats)
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
        introduced(prefix, d, Definition.Kind.Opaque, Some(UnitSize), defnIn(s)(d), Nil)
      }
    // A reference to the alias holds the aliased type's values; the alias itself adds none.
    case d: Defn.Type =>
      measured(scope) { s =>
        val aliased = sizeOf(s)(d)
        introduced(prefix, d, Definition.Kind.Alias, Some(aliased), NothingSize, s.unresolved)
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

  // The cardinality a reference to a definition has, and the names that stopped the calculator
  // from bounding *this* definition. A definition the body's equations named — a class, enum,
  // module or alias — is read from the solved scope, so a definition and a reference to it always
  // agree; its own syntax is evaluated anyway, in a scope that records only this definition's
  // unresolved names, which is what a report row reads. Anything the equations did not name — a
  // top-level value in particular — is worth what its own syntax is worth.
  private def sizeOf(scope: Scope)(d: Defn): Size = d match {
    case t: Defn.Type =>
      val aliased = typeIn(scope)(t.body)
      scope.size(t.name.value).getOrElse(aliased)
    case other =>
      val measured = defnIn(scope)(other)
      scope.size(named(other)).getOrElse(measured)
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
  // contributes. The row carries the definition's own unresolved names, kept apart from its
  // siblings'.
  private def introduced(
      prefix: List[String],
      d: Defn,
      kind: Definition.Kind,
      size: Option[Size],
      contributes: Size,
      unresolved: List[String],
  ): Introduced =
    Introduced(contributes, List(row(prefix, d, kind, size, unresolved)))

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
  private def equations(stats: List[Stat]): List[(String, Named)] = {
    val modules = stats.flatMap {
      case d: Defn.Object => Some(d.name.value -> Named(Nil, (_: Scope) => UnitSize))
      case _              => None
    }
    val types = stats.flatMap {
      case d: Defn.Type if d.mods.exists(_.is[Mod.Opaque]) =>
        Some(d.name.value -> Named(parameters(d), (_: Scope) => UnitSize))
      case d: Defn.Type =>
        Some(d.name.value -> Named(parameters(d), (sc: Scope) => typeIn(sc)(d.body)))
      case d: Defn.Enum =>
        Some(d.name.value -> Named(parameters(d), (sc: Scope) => enumSize(sc)(d)))
      case d: Defn.Class if !d.mods.exists(_.is[Mod.Abstract]) =>
        Some(d.name.value -> Named(parameters(d), (sc: Scope) => ctorIn(sc)(d.ctor)))
      case _ => None
    }
    val defined = modules ++ types
    defined ++ sealedSums(stats, defined.map(_._1).toSet).toList.map {
      case (parent, children) =>
        parent -> Named(
          Nil,
          (sc: Scope) =>
            children.foldLeft(NothingSize: Size)((acc, child) =>
              acc + sc.getOrElse(child, NothingSize)
            )
        )
    }
  }

  // The type parameters a definition declares, in the order its arguments are supplied in.
  private def parameters(d: Defn): List[String] = d match {
    case c: Defn.Class => c.tparamClause.values.map(_.name.value)
    case c: Defn.Trait => c.tparamClause.values.map(_.name.value)
    case c: Defn.Enum  => c.tparamClause.values.map(_.name.value)
    case c: Defn.Type  => c.tparamClause.values.map(_.name.value)
    case _             => Nil
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
      entries: List[(String, Named)],
      stats: List[Stat],
      base: Scope
  ): Scope = {
    val eqs = entries.map { case (name, named) => name -> named.equation }
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
  private def initParent: Init => String = {
    case Init(tpe, _, _) => bareName(tpe)
  }

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
    case p: Pkg.Object                                       => body(p.templ.body.stats, scope)
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
    val entries = equations(stats)
    val solved = solve(entries, stats, scope.withDefinitions(entries))
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
    // A type parameter stands for whatever the instantiation supplied — an innermost binder wins
    // over a builtin of the same name, as it does in Scala.
    case t: Type.Name if scope.frame(t.value).isDefined => scope.frame(t.value).get

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
    // Map/PartialFunction (functions into an Option of the codomain), and any definition the
    // source set names as an instantiation — see `applied`.
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => applied(scope)(callee, args)

    // A name the file does not define may still be a package sibling's or another supplied
    // source's — the library resolves it — and any other name is unbounded, recorded so that a
    // report can say which name it was.
    case t: Type.Name => scope.resolve(t.value).getOrElse { scope.note(t); EffectiveOmega }

    // scalameta's `Type` is not sealed, and several variants (`Type.And`, `Type.Or`,
    // `Type.Method`, `Type.ImplicitFunction`, `Type.Quasi`) are `private[meta]`, so an
    // exhaustive match is impossible. Every remaining form is unbounded or not yet
    // modelled — a name defined in another file, a refinement, an existential, a Scala 3
    // capture type — and counts as effectively infinite. Each one is recorded, so that a report
    // can name what it could not bound.
    // ponytail: resolve sealed hierarchies and type parameters across files when needed
    case t =>
      scope.note(t)
      EffectiveOmega
  }

  // `codomain ^ domain`, except that an empty domain gives 0 rather than the set-theoretic 1,
  // `0^0` included. This is a constructivist approach: Scala is eager, a call evaluates its
  // argument first, and no argument of an uninhabited type can be constructed, so a function
  // from one can never run. Only function types take this rule; `Set` and `Map` over
  // `Nothing` still hold their one empty value.
  private def arrow(codomain: Size, domain: Size): Size =
    if (domain == NothingSize) NothingSize else codomain.pow(domain)

  // `C[args]`: an *instantiation* of a definition the scope names reads that definition's equation
  // with its type parameters bound to what the arguments are worth — `Pair[Boolean]` is 4, not an
  // unknown constructor. What is left of the builtin algebra (`Option`, `Either`, `Set`, the
  // collections) is read below.
  private def applied(scope: Scope)(callee: Type, args: List[Type]): Size =
    instantiation(scope, callee, args).getOrElse(builtin(scope)(callee, args))

  // The size of `C[args]` when `C` is a definition the scope knows with that many parameters, or
  // None when it is not — a builtin, an arity that does not match, a name no definition has.
  //
  // Two recursive readings stay careful. An instantiation that is exactly the definition's own
  // parameters (`Node[A]` inside `Node`'s equation) is what the solver already solved under the
  // name, so it borrows that fixed point — that is how a parameterised recursion keeps its μ/ν
  // reading. Any other instantiation entered while its own name is being substituted is a cycle
  // the name-keyed solver has no fixed point for, so it keeps the old fallback: ω with the name as
  // the reason.
  private def instantiation(scope: Scope, callee: Type, args: List[Type]): Option[Size] = {
    val found = for
      name <- nameOf(callee)
      resolved <- scope
        .definition(name)
        .map((scope, _))
        .orElse(scope.libraryDefinition(name))
      if resolved._2.params.size == args.size
    yield (name, resolved._1, resolved._2)
    found.map {
      case (name, context, named) =>
        val self = named.params.zip(args).forall((param, arg) => bareName(arg) == param)
        if (self && context.size(name).isDefined) context(name)
        else if (context.isSubstituting(name)) {
          scope.note(callee)
          EffectiveOmega
        } else {
          val frame = named.params.zip(args).map((param, arg) => param -> typeIn(scope)(arg)).toMap
          named.equation(context.substituting(name).instantiated(frame))
        }
    }
  }

  // The simple name a type constructor is spelled with: `Pair`, or `data.Pair`'s last segment — a
  // qualified name resolves by its own name, as it does for a bare reference.
  private def nameOf(tpe: Type): Option[String] = tpe match {
    case Type.Name(name)              => Some(name)
    case Type.Select(_, Type.Name(n)) => Some(n)
    case _                            => None
  }

  // The type constructors the algebra models itself: `Option`/`Either` (sums), `Set` (powerset),
  // `Map`/`PartialFunction` (functions into an Option of the codomain), and the collections whose
  // unbounded length is their infinity. `Size.pow` already has the arithmetic `Set` and `Map`
  // need: a finite base over an infinite exponent stays countable (the finite subsets of
  // `String`), and only an infinite base over an infinite exponent is uncountable.
  private def builtin(scope: Scope): (Type, List[Type]) => Size = {
    case (Type.Name("Option"), List(t))    => UnitSize + typeIn(scope)(t)
    case (Type.Name("Either"), List(l, r)) => typeIn(scope)(l) + typeIn(scope)(r)
    case (Type.Name("Set"), List(t))       => BooleanSize.pow(typeIn(scope)(t))
    case (Type.Name("Map"), List(k, v))    => (typeIn(scope)(v) + UnitSize).pow(typeIn(scope)(k))
    case (Type.Name("PartialFunction"), List(a, b)) =>
      (typeIn(scope)(b) + UnitSize).pow(typeIn(scope)(a))
    // A linear collection of an empty element type has a single inhabitant (the empty
    // collection); over any other element type its unbounded length makes it countably
    // infinite, which is a number the algebra knows rather than an unknown, so it reports no
    // reason of its own — an unresolved element type reports its own.
    case (Type.Name("List" | "Vector" | "Seq" | "IndexedSeq" | "Array"), List(t)) =>
      if (typeIn(scope)(t) == NothingSize) UnitSize else EffectiveOmega
    // `LazyList` and `Stream` are the greatest fixed point `νX. 1 + A*X` (§8): all the
    // finite ones — ℵ₀ over any nonempty finitely-countable alphabet — plus the infinite
    // streams, the alphabet's choice space per position raised to ℵ₀. Collapse that completed
    // lazy type to its highest infinite tier; ordinary sums around it still keep coefficients.
    // An empty alphabet has the single empty list, exactly as for `List`.
    case (Type.Name("LazyList" | "Stream"), List(t)) =>
      val elem = typeIn(scope)(t)
      if (elem == NothingSize) UnitSize
      else completeCoinduction(EffectiveOmega, elem.pow(EffectiveOmega))
    // A type constructor the calculator does not model is recorded by name, so that a report can
    // say what it could not bound.
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
