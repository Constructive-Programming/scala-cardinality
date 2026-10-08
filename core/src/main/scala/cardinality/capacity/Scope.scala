package cardinality.capacity

import scala.collection.mutable
import scala.meta.*

import cardinality.types.*

// The state one read carries: the names a body solves, what a reference can resolve to, and the
// library of the other supplied sources. `Counter` reads with it; the walk builds it.

// The names of the types a source defines, each mapped to its cardinality, so a field can refer
// to a sibling, forward or recursive definition by name. The equations are solved by Kleene
// iteration from the empty type (`Solver`), so the walk hands every definition the value a
// reference has; unresolved names are collected so a report can say *why* a size is unbounded.
// Notes are shared across a body's iteration; `measured` starts a fresh record per definition.
// A scope also carries what a *generic* reference needs: the definitions themselves, their
/** What a read carries besides solved names: definitions, frames, library, home, imports. */
final private[cardinality] case class World(
    definitions: Map[TypeName, Named] = Map.empty,
    frames: List[Map[TypeName, Size]] = Nil,
    active: Set[TypeName] = Set.empty,
    library: Library = Library.empty,
    home: List[TypeName] = Nil,
    imports: Set[TypeName] = Set.empty,
    binders: List[Binder] = Nil,
    open: Set[TypeName] = Set.empty,
    members: Map[TypeName, Members] = Map.empty,
    terms: Map[TypeName, Type] = Map.empty,
)

/** The type members a body declares: what qualified references (`Outer.B`, `Foo[A].B`) resolve to;
  * abstract ones are supplied by implementors.
  */
final private[cardinality] case class Members(
    params: List[TypeName],
    declared: Map[TypeName, Type],
    abstractMembers: Set[TypeName],
)

/** A declared type parameter and its own arity: `F[_]` has one, `F[_, _]` two, plain `A` none. */
final private[cardinality] case class Binder(name: TypeName, arity: Int) {

  def higherKinded: Boolean = arity > 0
}

final private[cardinality] class Scope(
    private val values: Map[TypeName, Size],
    private val notes: mutable.LinkedHashSet[Definition.Unbound],
    private val world: World,
) {

  private def definitions: Map[TypeName, Named] = world.definitions

  private def frames: List[Map[TypeName, Size]] = world.frames

  private def active: Set[TypeName] = world.active

  private def library: Library = world.library

  private def members: Map[TypeName, Members] = world.members

  private def terms: Map[TypeName, Type] = world.terms

  /** The members of a definition in scope: the file's own first, the library's after. */
  def membersOf(owner: TypeName): Option[Members] =
    members.get(owner).orElse(library.members(owner, home))

  /** The type a value's declaration gives it, which an instance-qualified member needs. */
  def declaredType(term: TypeName): Option[Type] = terms.get(term)

  /** The size this read binds a type parameter to: `case scope.Parameter(size)`. */
  object Parameter {

    def unapply(tpe: Type): Option[Size] = tpe match {
      case Type.Name(name) => frame(TypeName.of(name))
      case _               => None
    }

  }

  /** The members a definition's body declares, and the values whose declared types it gives. */
  def withMembers(owner: TypeName, declared: Members): Scope =
    new Scope(values, notes, world.copy(members = world.members.updated(owner, declared)))

  def withTerms(declared: Map[TypeName, Type]): Scope =
    new Scope(values, notes, world.copy(terms = declared ++ terms))

  private def home: List[TypeName] = world.home

  private def imports: Set[TypeName] = world.imports

  private[cardinality] def openNames: Set[TypeName] = world.open

  def size(name: TypeName): Option[Size] = values.get(name)

  def contains(name: TypeName): Boolean = values.contains(name)

  def apply(name: TypeName): Size = values(name)

  def getOrElse(name: TypeName, default: => Size): Size = values.getOrElse(name, default)

  /** What a type parameter stands for in the instantiation being read, innermost binder first. */
  def frame(name: TypeName): Option[Size] = frames.collectFirst(Function.unlift(_.get(name)))

  /** The definition a name resolves to, so that `C[args]` can be read as an instantiation. */
  def definition(name: TypeName): Option[Named] = definitions.get(name)

  /** The value a name has: the file's own, else the library's; an imported name resolves neither.
    */
  def resolve(name: TypeName): Option[Size] =
    values.get(name).orElse(if (imported(name)) None else library.value(name, home))

  /** The same for a definition the library supplied, with the package frame it was read in. */
  def libraryDefinition(name: TypeName): Option[(Scope, Named)] =
    if (imported(name)) None else library.definition(name, home)

  def imported(name: TypeName): Boolean = imports(name)

  def defines(name: TypeName): Boolean =
    values.contains(name) || definitions.contains(name) || members.contains(name)

  def definedNames: Set[TypeName] = values.keySet ++ definitions.keySet ++ members.keySet

  def updated(name: TypeName, size: Size): Scope =
    new Scope(values.updated(name, size), notes, world)

  /** Names added by the solver's own round, which carries no notes of its own. */
  def ++(entries: Iterable[(TypeName, Size)]): Scope =
    new Scope(values ++ entries, notes, world)

  /** The definitions the bodies now in scope introduce; an inner body shadows an outer name. */
  def withDefinitions(entries: List[(TypeName, Named)]): Scope =
    new Scope(values, notes, world.copy(definitions = entries.toMap ++ definitions))

  /** The package path a reference is read in, which decides what the library lends it. */
  def withHome(path: List[TypeName]): Scope =
    new Scope(values, notes, world.copy(home = path))

  /** The names a body imports; an inner import shadows an outer name for the whole body. */
  def withImports(names: Set[TypeName]): Scope =
    new Scope(values, notes, world.copy(imports = imports ++ names))

  /** The definition's declared type parameters, in scope while its body and its row are read. */
  def withBinders(declared: List[Binder]): Scope =
    new Scope(values, notes, world.copy(binders = declared ++ world.binders))

  /** Abstractions a body leaves open (unsealed traits, abstract classes): a reference to one is
    * unbounded, not unknown.
    */
  def withOpen(names: Set[TypeName]): Scope =
    new Scope(values, notes, world.copy(open = names ++ world.open))

  /** The definitions of other sources this read can lean on. */
  def withLibrary(other: Library): Scope =
    new Scope(values, notes, world.copy(library = other))

  /** A library definition read where it was written: its package's names resolve, not the file's.
    */
  def inPackage(pkg: Scope): Scope =
    new Scope(
      pkg.values,
      notes,
      pkg.world.copy(frames = world.frames, active = world.active, imports = imports),
    )

  /** One instantiation: the definition's parameters bound to what the arguments are worth,
    * innermost frame first.
    */
  def instantiated(frame: Map[TypeName, Size]): Scope =
    new Scope(values, notes, world.copy(frames = frame :: frames))

  /** Marks a substitution in progress, so a cycle through an applied reference terminates like one
    * through a bare name.
    */
  def substituting(name: TypeName): Scope =
    new Scope(values, notes, world.copy(active = active + name))

  def isSubstituting(name: TypeName): Boolean = active(name)

  /** The same names with a fresh record of unresolved ones. */
  def measured: Scope =
    new Scope(values, mutable.LinkedHashSet.empty, world)

  /** What this scope could not bound, by kind and in the order it met them. */
  def unbound: List[Definition.Unbound] = notes.toList

  /** Records a type the calculator could not bound: a name, a binder, an open abstraction, or
    * unmodelled syntax — an unplaceable name is a binder first, unknown only then.
    */
  def note(tpe: Type): Unit = { notes += reason(tpe); () }

  /** Records a name directly, for a reason that is not a type's own syntax. */
  def note(name: String): Unit = { notes += Definition.Unbound.Unknown(name); () }

  /** Records an abstraction a member is supplied by, named as the reference is written. */
  def noteOpen(reference: String): Unit = { notes += Definition.Unbound.Open(reference); () }

  // Why a type could not be bounded: the binder it stands for, the syntax it is, or the name
  // itself when it is neither.
  private def reason(tpe: Type): Definition.Unbound = tpe match {
    case Type.Name(name) =>
      binder(TypeName.of(name))
        .orElse(open(TypeName.of(name)))
        .getOrElse(Definition.Unbound.Unknown(name))
    case other => Definition.Unbound.Syntax(Walk.describe(other))
  }

  // The binder a name stands for, when a definition in scope declares it.
  private def binder(name: TypeName): Option[Definition.Unbound] =
    world.binders
      .find(_.name == name)
      .map(binder =>
        if (binder.higherKinded) Definition.Unbound.HigherKinded(name.value, binder.arity)
        else Definition.Unbound.Parameter(name.value)
      )

  // An unsealed abstraction: any subtype anywhere may add values, so a reference to it has no
  // bound at all — the bodies in scope know their own, and the library knows its packages'.
  private def open(name: TypeName): Option[Definition.Unbound] =
    if (openNames(name) || library.open(name)) Some(Definition.Unbound.Open(name.value)) else None

}

private[cardinality] object Scope {

  val empty: Scope = new Scope(Map.empty, mutable.LinkedHashSet.empty, World())

}

/** One named definition a reference can resolve: the type parameters it declares, and what its type
  * is worth with those parameters in scope.
  */
final private[cardinality] case class Named(
    params: List[TypeName],
    equation: Scope => Size,
    matchType: Option[Type] = None,
    children: List[TypeName] = Nil
)

/** The definitions the other supplied sources introduce, so a reference can leave its file: a
  * report reads the published sources as one set, and a sibling file's type resolves instead of
  * counting as unknown.
  *
  * A name exactly one package defines resolves wherever it is written (the shape an import usually
  * has); a name two packages define stays unresolved rather than guessed. Nothing outside the
  * source set is modelled: a classpath-only name reports unresolved, and an import of the same
  * spelling is the one assumption a resolved name rests on. Each package's top-level definitions
  * solve as one system, so siblings reference each other and a sealed hierarchy spanning files sums
  * as a whole.
  */
final class Library private[cardinality] (
    private val packages: Map[List[TypeName], Scope],
    private val unique: Map[TypeName, Scope]
) {

  /** The value a name defined outside the file being read has, when the library defines it. */
  private[cardinality] def value(name: TypeName, home: List[TypeName]): Option[Size] =
    frame(name, home).flatMap(_.size(name))

  /** The definition such a name resolves to, with the package it was read in. */
  private[cardinality] def definition(
      name: TypeName,
      home: List[TypeName]
  ): Option[(Scope, Named)] =
    frame(name, home).flatMap(pkg => pkg.definition(name).map(pkg -> _))

  /** Whether some package of the source set defines the name as an open abstraction. */
  private[cardinality] def open(name: TypeName): Boolean = packages.values.exists(_.openNames(name))

  /** The members a package's definitions declare, for a qualified reference across files. */
  private[cardinality] def members(owner: TypeName, home: List[TypeName]): Option[Members] =
    frame(owner, home).flatMap(_.membersOf(owner))

  // The package a name read in `home` resolves to: its own when it defines the name — package
  // members are in scope without an import — else the one package across the source set that
  // does, which is the shape an import of a single name has.
  private def frame(name: TypeName, home: List[TypeName]): Option[Scope] =
    packages.get(home).filter(_.defines(name)).orElse(unique.get(name).filter(_.defines(name)))

}

object Library {

  // Trees are parsed as Scala 3: printing one under any other dialect reprints it under Scala 2
  // rules, which cannot spell Scala 3's modifiers (`inline def` throws while printing).
  private given scala3: Dialect = dialects.Scala3

  /** No other sources: a source read on its own, as a single file is. */
  val empty: Library = new Library(Map.empty, Map.empty)

  /** Reads the top-level definitions of every source, grouped by the package they are read in, and
    * solves each package's names as one system.
    */
  def of(sources: List[Source]): Library = {
    val packages = packageStatements(sources).groupBy(_._1).map {
      case (path, found) =>
        val stats = found.flatMap(_._2)
        val entries = Counter.equations(stats)
        val base = Scope.empty
          .withDefinitions(entries)
          .withOpen(Walk.openNames(stats))
          .withHome(path)
        val definitions = stats.collect { case d: Defn => d }
        val solved = Solver.solve(entries, stats, base)
        val withMembers = definitions.foldLeft(solved) { (scope, d) =>
          scope.withMembers(TypeName.of(Walk.name(d)), Walk.members(d))
        }
        path -> withMembers
    }
    val unique = packages.values.toList
      .flatMap(scope => scope.definedNames.toList.map(name => name -> scope))
      .groupBy(_._1)
      .collect { case (name, List((_, scope))) => name -> scope }
    new Library(packages, unique)
  }

  // A source's statements grouped by the package they are read in, nested packages flattened to
  // the path an editor shows (`package a` then `package b` is `a.b`).
  private def packageStatements(sources: List[Source]): List[(List[TypeName], List[Stat])] =
    sources.flatMap(source => packageStatements(source.stats, List.empty[TypeName]))

  private def packageStatements(
      stats: List[Stat],
      prefix: List[TypeName]
  ): List[(List[TypeName], List[Stat])] = {
    val direct = stats.filterNot(st => st.is[Pkg] || st.is[Pkg.Object])
    val nested = stats.flatMap {
      case p: Pkg        => packageStatements(p.body.stats, prefix ++ TypeName.path(p.ref.syntax))
      case p: Pkg.Object =>
        packageStatements(p.templ.body.stats, prefix :+ TypeName.of(p.name.value))
      case _ => Nil
    }
    (prefix -> direct) :: nested
  }

}

// What one statement adds to its body: the cardinality it contributes and the definitions it
// introduces, which a report lists.
