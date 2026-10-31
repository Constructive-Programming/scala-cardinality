package cardinality.capacity

import scala.collection.mutable
import scala.meta.*

import cardinality.types.*
import dev.constructive.eo.generics.lens
import dev.constructive.eo.optics.Lens

// The state one read carries: the names a body solves, what a reference can resolve to, and the
// library of the other supplied sources. `Counter` reads with it; the walk builds it.
//
// The names of the types a source defines are each mapped to their cardinality, so a field can
// refer to a sibling, forward or recursive definition by name. The equations are solved by Kleene
// iteration from the empty type (`Solver`), so the walk hands every definition the value a
// reference has; unresolved names are collected so a report can say *why* a size is unbounded.
// Notes are shared across a body's iteration; `measured` starts a fresh record per definition.

/** What a read carries besides solved names: definitions, frames, binders and members, what each
  * term name in scope denotes, the library, the package clauses the read is inside (innermost
  * first), the import and export clauses in scope, the body's families and how the read is made.
  */
final private[cardinality] case class World(
    definitions: Map[TypeName, Named] = Map.empty,
    frames: List[Map[TypeName, Size]] = Nil,
    binders: List[Binder] = Nil,
    members: Map[TypeName, Members] = Map.empty,
    terms: Map[TypeName, Bound] = Map.empty,
    library: Library = Library.empty,
    opened: List[List[TypeName]] = Nil,
    imports: Imports = Imports.none,
    families: Families = Families(),
    session: Session = Session(),
)

/** The abstractions a body holds: open ones, sealed families it does not sum, and the definitions
  * on a lazy cycle the solver could not complete.
  */
final private[cardinality] case class Families(
    open: Set[TypeName] = Set.empty,
    unsummed: Set[TypeName] = Set.empty,
    blocked: Set[TypeName] = Set.empty,
)

/** How a read is made: `template` frames bind a definition's own parameters while its reasons are
  * collected, `active` guards re-entry, `self` is the definition whose own equation is read,
  * `recording` marks a row's own measurement, and `taint` collects a reference's reasons.
  */
final private[cardinality] case class Session(
    template: Boolean = false,
    active: Set[TypeName] = Set.empty,
    self: Option[Named] = None,
    recording: Boolean = false,
    taint: Option[Taint] = None,
)

/** What a term name denotes where it is read: an object, by the key prefix its members are solved
  * under, or a value (a `val`, a parameter, a `def`) with the type it is known to have, if any.
  * Scala resolves a path's first name innermost first over all of them.
  */
private[cardinality] enum Bound {
  case Module(prefix: TypeName)
  case Value(declared: Option[Type])
}

/** Where a bare type name binds, in Scala's order: a body around the read; a clause the estimate
  * does not follow (an explicit import or export, or a wildcard over a namespace no source
  * supplies); a namespace a wildcard names or a package the read's clauses open, under the key it
  * solves the name by; or nothing the sources hold.
  */
private[cardinality] enum Binding {
  case Here
  case Clause(reason: Definition.Unbound)
  case Elsewhere(in: Scope, key: TypeName)
  case Free
}

/** One lexical layer a body adds to the scope it reads in: the type names it defines, the term
  * names it binds, the clauses in effect and, for a file of a package, the package clauses that
  * file opens. `rekey` gives the keys an enclosing system solved the types under (an object's);
  * without it the body is about to solve them itself.
  */
final private[cardinality] case class Layer(
    types: Set[TypeName] = Set.empty,
    terms: Map[TypeName, Bound] = Map.empty,
    imports: Imports = Imports.none,
    opened: Option[List[List[TypeName]]] = None,
    rekey: Option[TypeName => TypeName] = None,
)

/** One reference's collection of reasons: each definition it reads adds its reasons once. */
final private[cardinality] class Taint {

  private val seen =
    java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Named, java.lang.Boolean])

  def visit(named: Named): Boolean = seen.add(named)
}

/** The type members a class, trait or enum (`owner`) declares: what an instance path (`x.B`)
  * resolves to; abstract ones are supplied by implementors. `body` builds the scope the
  * definition's own body reads in, where a member's syntax means what it was written to mean.
  */
final private[cardinality] class Members(
    val owner: TypeName,
    val params: List[TypeName],
    val declared: Map[TypeName, Type],
    val abstractMembers: Set[TypeName],
    body: () => Scope,
) {

  lazy val inside: Scope = body()
}

/** A declared type parameter and its own arity: `F[_]` has one, `F[_, _]` two, plain `A` none. */
final private[cardinality] case class Binder(name: TypeName, arity: Int) {

  def higherKinded: Boolean = arity > 0
}

/** A sealed parent's subtype, as its defining statement says: a module counts its one value, and a
  * class's type parameters are existential from the parent's side — a parent the estimate sums is
  * extended without arguments, so no instantiation of it fixes them.
  */
final private[cardinality] case class Child(name: TypeName, module: Boolean, params: List[TypeName])

final private[cardinality] class Scope(
    private val values: Map[TypeName, Size],
    private val notes: mutable.LinkedHashSet[Definition.Unbound],
    private val world: World,
) {

  /** The members a namespace solved under `key`. */
  def membersAt(key: TypeName): Option[Members] = world.members.get(key)

  /** The size this read binds a type parameter to: `case scope.Parameter(size)`. */
  object Parameter {

    def unapply(tpe: Type): Option[Size] = tpe match {
      case Type.Name(name) => frame(TypeName.of(name))
      case _               => None
    }

  }

  /** A type parameter in scope that no instantiation bound: `case scope.Free(binder)`. */
  object Free {

    def unapply(tpe: Type): Option[Type.Name] = tpe match {
      case name: Type.Name if unboundBinder(TypeName.of(name.value)) => Some(name)
      case _                                                         => None
    }

  }

  def size(name: TypeName): Option[Size] = values.get(name)

  def apply(name: TypeName): Size = values(name)

  def getOrElse(name: TypeName, default: => Size): Size = values.getOrElse(name, default)

  /** What a type parameter stands for in the instantiation being read, innermost binder first. */
  def frame(name: TypeName): Option[Size] = world.frames.collectFirst(Function.unlift(_.get(name)))

  /** Whether a name is a type parameter of a definition around the read, bound or not. */
  def isBinder(name: TypeName): Boolean =
    frame(name).isDefined || world.binders.exists(_.name == name)

  /** The definition a name resolves to, so that `C[args]` can be read as an instantiation. */
  def definition(name: TypeName): Option[Named] = world.definitions.get(name)

  /** Where a bare type name binds. A body around the read outranks every clause — except that, for
    * a file read with the rest of its package, a wildcard of the file outranks a name only another
    * file defines. Then an explicit clause, then the wildcards (latest first), then the packages
    * the read's clauses open; a package the library has not solved yet is a reason of its own.
    */
  def binding(name: TypeName): Binding =
    exported(name).getOrElse {
      if (here(name) && !(world.imports.foreign(name) && wildcard(name).isDefined)) Binding.Here
      else
        world.imports.named.get(name) match {
          case Some(provenance) =>
            Binding.Clause(Definition.Unbound.Imported(name.value, provenance))
          case None =>
            wildcard(name)
              .orElse(world.library.frame(name, world).map(Binding.Elsewhere(_, name)))
              .orElse(world.library.pending(name, world).map(Binding.Clause(_)))
              .getOrElse(Binding.Free)
        }
    }

  // An export wildcard defines members of the body it is written in, which outrank every binding
  // outside that body; the body's own definitions, and those of bodies inside it, outrank it.
  private def exported(name: TypeName): Option[Binding] =
    wildcards.indices.iterator
      .filter(wildcards(_).exported)
      .flatMap(wildcardAt(_, name))
      .nextOption()

  // Whether a body around the read binds the name: a value, a definition, members, or an
  // abstraction with no value of its own.
  private def here(name: TypeName): Boolean =
    values.contains(name) || world.definitions.contains(name) || world.members.contains(name) ||
      world.families.open(name) || world.families.unsummed(name)

  // The first wildcard, latest first, that binds the name: a supplied namespace that defines it, or
  // one the sources do not supply, which may. Each reads its path with the wildcards before it. Two
  // wildcards of one body that both define it are an ambiguity Scala rejects: no binding to read.
  private def wildcard(name: TypeName): Option[Binding] = {
    val found = wildcards.indices.iterator.flatMap(i => wildcardAt(i, name).map(i -> _))
    found.nextOption().map {
      case (i, defined: Binding.Elsewhere) =>
        val rivals = wildcards.indices.drop(i + 1).filter(j => wildcards(j).beside(wildcards(i)))
        if (rivals.exists(j => wildcardAt(j, name).exists(_.isInstanceOf[Binding.Elsewhere])))
          Binding.Clause(
            Definition.Unbound.Imported(name.value, "ambiguous: several wildcard imports bind it")
          )
        else defined
      case (_, other) => other
    }
  }

  private def wildcardAt(i: Int, name: TypeName): Option[Binding] = {
    val w = wildcards(i)
    if (w.excluded(name)) None
    else
      Namespaces.target(earlier(i), w.path).member(name, w) match {
        case Some(Namespaces.Found.Defined(in, key)) => Some(Binding.Elsewhere(in, key))
        case Some(Namespaces.Found.Reason(why))      => Some(Binding.Clause(why))
        case None                                    => None
      }
  }

  /** This scope reading the `i`-th wildcard's path: only the wildcards written before it apply. */
  def earlier(i: Int): Scope =
    new Scope(
      values,
      notes,
      world.copy(imports = world.imports.copy(wildcards = world.imports.wildcards.drop(i + 1)))
    )

  /** The wildcards in scope, latest first. */
  def wildcards: List[Wildcard] = world.imports.wildcards

  /** The value a name has: a body's around the read, or a namespace's. */
  def resolve(name: TypeName): Option[Size] = binding(name) match {
    case Binding.Here               => if (values.contains(name)) solved(this, name) else None
    case Binding.Elsewhere(in, key) => solved(borrow(in), key)
    case _                          => None
  }

  /** A name `in` solved, read here: its value, noting a lazy cycle the solver could not complete
    * and, while a reference's reasons are collected, the reasons the value rests on.
    */
  def solved(in: Scope, name: TypeName): Option[Size] =
    in.size(name).map { size =>
      if (in.isBlocked(name)) noteLazyCycle(name)
      if (world.session.taint.isDefined) in.definition(name).foreach(taintOf(_, in))
      size
    }

  /** Whether a reference's reasons are being collected. */
  def collecting: Boolean = world.session.taint.isDefined

  /** What a term name denotes here, innermost binding first — unless, for a file read with the rest
    * of its package, only another file binds it and a wildcard of this file names it too.
    */
  def term(name: TypeName): Option[Bound] =
    world.terms.get(name).filterNot { _ =>
      world.imports.foreign(name) && wildcards.indices.exists { i =>
        val w = wildcards(i)
        !w.excluded(name) && Namespaces.target(earlier(i), w.path).bindsTerm(name)
      }
    }

  /** Where an explicit clause binds a name, which the estimate does not follow. */
  def clause(name: TypeName): Option[String] = world.imports.named.get(name)

  def library: Library = world.library

  /** The packages the read's clauses open, innermost first; the empty package with none. */
  def openedPackages: List[List[TypeName]] = if (world.opened.isEmpty) List(Nil) else world.opened

  /** Whether a key names an abstraction this scope leaves open. */
  def isOpen(name: TypeName): Boolean = world.families.open(name)

  /** Whether a name is a sealed family this scope does not sum. */
  def isUnsummed(name: TypeName): Boolean = world.families.unsummed(name)

  /** Whether the solver left a name on a lazy cycle it could not complete. */
  def isBlocked(name: TypeName): Boolean = world.families.blocked(name)

  /** Whether a name is a type parameter in scope that no instantiation has bound. */
  def unboundBinder(name: TypeName): Boolean =
    frame(name).isEmpty && world.binders.exists(_.name == name)

  /** Whether `named`'s own equation is what is being read: a reference to it with its own
    * parameters is a recursion, not an instantiation.
    */
  def readingItself(named: Named): Boolean = world.session.self.exists(_ eq named)

  /** Whether the sources bind a name here, which takes it from the standard library's base type of
    * that spelling: a root import is the outermost binding there is.
    */
  def binds(name: TypeName): Boolean = binding(name) != Binding.Free

  /** `other`'s names and world, recording into this scope's notes: how a member another scope
    * solved — a library package, an object, a class body — is read for a row, so its reasons become
    * the row's.
    */
  def borrow(other: Scope): Scope = {
    val session = world.session
    new Scope(
      other.values,
      notes,
      other.world.copy(
        frames = Nil,
        session = other.world.session.copy(
          template = false,
          active = other.world.session.active ++ session.active,
          recording = session.recording,
          taint = if (session.recording) Some(session.taint.getOrElse(new Taint)) else None,
        ),
      ),
    )
  }

  /** This scope reading an instantiation: `frame` is the only one the equation reads. */
  def instantiating(frame: Map[TypeName, Size]): Scope =
    new Scope(
      values,
      notes,
      world.copy(frames = List(frame), session = world.session.copy(template = false))
    )

  /** Notes the reasons `named` rests on, read in `context`, transitively: every solved name its
    * equation reads adds its own, once per definition. Its own parameters are read as bound — a
    * reference supplies them or reads the template on purpose — and every other binder stays a
    * reason, whichever definition reads it.
    */
  def taintOf(named: Named, context: Scope): Unit =
    if (world.session.recording) {
      val session = Some(world.session.taint.getOrElse(new Taint))
      if (session.forall(_.visit(named))) {
        named.equation(reading(context, named.params, session))
        reading(context, Nil, session).readChildren(named.children, session)
      }
    }

  /** Reads a sealed parent's subtypes for the reasons the parent's sum rests on — transitively
    * while a reference collects them, directly for the parent's own row. A subtype's type parameter
    * ranges over every type from the parent's side: no instantiation of the parent fixes it, so it
    * is a reason of its own, never a parameter the parent depends on.
    */
  def readChildren(children: List[Child], session: Option[Taint]): Unit =
    children.filterNot(_.module).foreach { child =>
      definition(child.name).filter(n => session.forall(_.visit(n))).foreach { named =>
        if (child.params.nonEmpty) notes += Scope.unpinnedReason(child)
        named.equation(reading(this, child.params, session))
      }
    }

  // A scope that records into this one's notes, reading `context`'s names with `bound` as the
  // template's own parameters: bound, and never a reason of the reader's.
  private def reading(context: Scope, bound: List[TypeName], session: Option[Taint]): Scope =
    new Scope(
      context.values,
      notes,
      context.world.copy(
        frames = List(bound.map(_ -> (UnitSize: Size)).toMap),
        session = context.world.session.copy(template = true, recording = true, taint = session),
      ),
    )

  /** The names a body reads in, from a scope that solved them under their keys: its own types under
    * their plain names — shadowing the enclosing body's, and its explicit clauses shadowing both —
    * its term names, its clauses, its package clauses, and the enclosing binders it does not
    * shadow. A type the body defines is its own, whichever other file of a package defines it too.
    */
  def within(layer: Layer): Scope = {
    val named = layer.imports.named.keySet
    val hidden = layer.types ++ named
    val keys = layer.rekey.toList.flatMap(rekey => layer.types.map(n => n -> rekey(n)))
    def own[V](map: Map[TypeName, V]): Map[TypeName, V] =
      (map -- hidden) ++ keys.flatMap((n, key) => map.get(key).map(n -> _))
    def ownSet(set: Set[TypeName]): Set[TypeName] =
      (set -- hidden) ++ keys.collect { case (n, key) if set(key) => n }
    val imports = (world.imports ++ layer.imports).definingInside(layer.types ++ layer.terms.keySet)
    new Scope(
      own(values),
      notes,
      world.copy(
        definitions = own(world.definitions),
        members = own(world.members),
        families = Families(
          ownSet(world.families.open),
          ownSet(world.families.unsummed),
          ownSet(world.families.blocked)
        ),
        terms = (world.terms -- named) ++ layer.terms,
        imports = imports.copy(foreign = imports.foreign -- layer.types -- layer.terms.keySet),
        opened = layer.opened.getOrElse(world.opened),
        binders = world.binders.filterNot(b => hidden(b.name)),
      ),
    )
  }

  /** Whether this scope solves, defines or holds the members of a name. */
  def defines(name: TypeName): Boolean = here(name)

  def updated(name: TypeName, size: Size): Scope =
    new Scope(values.updated(name, size), notes, world)

  /** Names added by the solver's own round, which carries no notes of its own. */
  def ++(entries: Iterable[(TypeName, Size)]): Scope =
    new Scope(values ++ entries, notes, world)

  /** The same names with a fresh record of unresolved ones: a row's (or a slot's) own reading. */
  def measured: Scope =
    new Scope(
      values,
      mutable.LinkedHashSet.empty,
      world.copy(session = world.session.copy(recording = true, taint = None))
    )

  /** What this scope could not bound, by kind and in the order it met them. */
  def unbound: List[Definition.Unbound] = notes.toList

  /** Records a type the calculator could not bound: a name, a binder, an open abstraction, or
    * unmodelled syntax — an unplaceable name is a binder first, unknown only then. A template's own
    * parameter, applied, is the reference's to supply, not a reason.
    */
  def note(tpe: Type): Unit = tpe match {
    case Type.Name(name) if world.session.template && frame(TypeName.of(name)).isDefined => ()
    case _ => notes += reason(tpe); ()
  }

  /** Records a reason directly. */
  def note(reason: Definition.Unbound): Unit = { notes += reason; () }

  /** Records a name directly, for a reason that is not a type's own syntax. */
  def note(name: String): Unit = note(Definition.Unbound.Unknown(name))

  /** Records an abstraction a member is supplied by, named as the reference is written. */
  def noteOpen(reference: String): Unit = note(Definition.Unbound.Open(reference))

  /** Records a sealed family whose subtypes the estimate does not sum. */
  def noteUnsummed(name: TypeName): Unit = note(Scope.unsummedReason(name))

  /** Records that a value rests on a lazy cycle the solver could not complete: its least fixed
    * point counts the finite values only, and the cyclic ones a by-name or function field lets a
    * program build are not counted, so the number would be an under-count.
    */
  def noteLazyCycle(name: TypeName): Unit =
    note(Definition.Unbound.Syntax(s"lazy cycle not completed: ${name.value}"))

  // Why a type could not be bounded: the binder it stands for, the syntax it is, or the name
  // itself when it is neither.
  private def reason(tpe: Type): Definition.Unbound = tpe match {
    case Type.Name(name) =>
      val named = TypeName.of(name)
      binder(named).getOrElse(nameReason(named))
    // A qualified name the sources do not resolve is a name, as written.
    case select: Type.Select => Definition.Unbound.Unknown(TypeSubstitution.described(select))
    case other               => Definition.Unbound.Syntax(TypeSubstitution.described(other))
  }

  // Why a bare name has no value where it binds: an abstraction there (open, or a family not
  // summed), the clause that binds it, or the name.
  private def nameReason(name: TypeName): Definition.Unbound = {
    def kind(in: Scope, key: TypeName): Definition.Unbound =
      if (in.isOpen(key)) Definition.Unbound.Open(name.value)
      else if (in.isUnsummed(key)) Scope.unsummedReason(name)
      else Definition.Unbound.Unknown(name.value)
    binding(name) match {
      case Binding.Here               => kind(this, name)
      case Binding.Elsewhere(in, key) => kind(in, key)
      case Binding.Clause(reason)     => reason
      case Binding.Free               => Definition.Unbound.Unknown(name.value)
    }
  }

  // The binder a name stands for, when a definition in scope declares it.
  private def binder(name: TypeName): Option[Definition.Unbound] =
    world.binders
      .find(_.name == name)
      .map(binder =>
        if (binder.higherKinded) Definition.Unbound.HigherKinded(name.value, binder.arity)
        else Definition.Unbound.Parameter(name.value)
      )

}

private[cardinality] object Scope {

  val empty: Scope = new Scope(Map.empty, mutable.LinkedHashSet.empty, World())

  /** The reason a sealed family the estimate does not sum carries, in its row and in a reference.
    */
  def unsummedReason(name: TypeName): Definition.Unbound =
    Definition.Unbound.Syntax(s"sealed family not summed: ${name.value}")

  /** The reason a sealed parent carries for a subtype whose parameters it does not fix. */
  def unpinnedReason(child: Child): Definition.Unbound = {
    val written = child.name.value + child.params.map(_.value).mkString("[", ", ", "]")
    Definition.Unbound.Syntax(s"sealed child parameter not pinned: $written")
  }

  private val worldLens =
    Lens[Scope, World](_.world, (s, w) => new Scope(s.values, s.notes, w))

  private val familiesLens = worldLens.andThen(lens[World](_.families))
  private val sessionLens = worldLens.andThen(lens[World](_.session))

  val definitionsLens = worldLens.andThen(lens[World](_.definitions))
  val libraryLens = worldLens.andThen(lens[World](_.library))
  val openedLens = worldLens.andThen(lens[World](_.opened))
  val bindersLens = worldLens.andThen(lens[World](_.binders))
  val membersLens = worldLens.andThen(lens[World](_.members))
  val termsLens = worldLens.andThen(lens[World](_.terms))
  val openLens = familiesLens.andThen(lens[Families](_.open))
  val unsummedLens = familiesLens.andThen(lens[Families](_.unsummed))
  val blockedLens = familiesLens.andThen(lens[Families](_.blocked))
  val activeLens = sessionLens.andThen(lens[Session](_.active))
  val selfLens = sessionLens.andThen(lens[Session](_.self))
  val recordingLens = sessionLens.andThen(lens[Session](_.recording))
  val taintLens = sessionLens.andThen(lens[Session](_.taint))

}

/** One named definition a reference can resolve: the type parameters it declares, what its type is
  * worth with those parameters in scope, and the scope its own syntax reads in (`read`, from the
  * scope a reader hands it) — what a match type's case body is read in.
  */
final private[cardinality] case class Named(
    params: List[TypeName],
    equation: Scope => Size,
    matchType: Option[Type] = None,
    children: List[Child] = Nil,
    read: Scope => Scope = identity,
) {

  /** The same definition read through `view`: how a body hands its own syntax its lexical scope. */
  def viewed(view: Scope => Scope): Named =
    copy(equation = sc => equation(view(sc)), read = sc => read(view(sc)))

}
