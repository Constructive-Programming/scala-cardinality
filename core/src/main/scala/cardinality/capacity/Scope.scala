package cardinality.capacity

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.*

import cardinality.types.*
import dev.constructive.eo.generics.lens
import dev.constructive.eo.optics.Lens

// The state one read carries: each type a body defines mapped to its solved cardinality (Kleene
// iteration from the empty type, `Solver`), what a reference can resolve to, and the library of
// the other supplied sources. Unresolved names are collected so a report can say *why* a size is
// unbounded; `measured` starts a fresh record per definition.

/** What a read carries besides solved names; `opened` are its package clauses, innermost first,
  * `slots` which slots of each generic definition solved around it are positive (`Positivity`), and
  * `templates` the templates whose bodies the read is inside that have parents (`Inheritance`).
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
    slots: Map[TypeName, Positivity.Slots] = Map.empty,
    templates: List[Inheriting] = Nil,
)

/** The abstractions a body holds: open ones, the abstract type members of a class or trait template
  * (`members`, each instance's to choose), sealed families it does not sum, the definitions the
  * solver left on a cycle without a count (`blocked`), and the type names that bind here to a
  * declaration the estimate does not read (`refused`, each with why: an enum's class case).
  */
final private[cardinality] case class Families(
    open: Set[TypeName] = Set.empty,
    unsummed: Map[TypeName, String] = Map.empty,
    blocked: Map[TypeName, Blocked] = Map.empty,
    members: Set[TypeName] = Set.empty,
    refused: Map[TypeName, Definition.Unbound] = Map.empty,
)

/** Why the solver gives a definition on a cycle no count: a lazy cycle the ν completion did not
  * finish (its μ value counts the finite values only); a recursion through a function's domain,
  * which no fixed point of the model counts, or through a constructor whose use of its argument the
  * estimate does not read (`Positivity`); or a recursion through a type member read off a value
  * (`x: q.X`), which the solver reads over an earlier solve and never iterates (`Solver`).
  */
private[cardinality] enum Blocked(val reason: String) {
  case Lazy extends Blocked("lazy cycle not completed")
  case Negative extends Blocked("recursion through a function's domain")

  case Unread(constructor: String)
      extends Blocked("recursion through a constructor the estimate does not read")

  case Unseen extends Blocked("recursion through a type member of a value")

  // A constructor a recursion runs through follows the name, after the separator.
  def about(name: String): String = this match {
    case Unread(constructor) => s"$reason: $name (through $constructor)"
    case _                   => s"$reason: $name"
  }

  /** Whether the definition has no count at all, not only an incomplete one. */
  def uncounted: Boolean = this != Lazy
}

/** How a read is made: `template` frames bind a definition's own parameters while its reasons are
  * collected; `placeholders` stand for parameters no instantiation fixed; `arguments` are what the
  * frame's names were written as; `active` guards re-entry; `self` and `summing` say whose own
  * equation is read; `recursion` notices an instantiation's recursion on its parameters;
  * `recording` and `taint` collect a row's reasons; `saturating` marks the method side's reading.
  */
final private[cardinality] case class Session(
    template: Boolean = false,
    placeholders: Set[TypeName] = Set.empty,
    arguments: Map[TypeName, Argument] = Map.empty,
    active: Set[TypeName] = Set.empty,
    self: Option[Itself] = None,
    summing: List[Itself] = Nil,
    recursion: Option[Recursion] = None,
    recording: Boolean = false,
    taint: Option[Taint] = None,
    saturating: Boolean = false,
)

/** The type an instantiation bound a parameter to, as the reference wrote it, and the scope that
  * reads it: a match type over the parameter reduces on this syntax, its parts read where written.
  */
final private[cardinality] case class Argument(tpe: Type, in: Scope)

/** The definition whose own equation is being read, and the names that stand for its parameters
  * there, slot by slot — its own, or, while a sealed parent reads a subtype, the subtype's
  * parameters each slot binds (none for a slot pinned bottom-up) — so that a reference applying it
  * to them is its recursion.
  */
final private[cardinality] case class Itself(named: Named, own: List[Option[TypeName]])

private[cardinality] object Itself {

  /** A definition reading its own equation: its parameters stand for themselves. */
  def of(named: Named): Itself = Itself(named, named.params.map(Some(_)))
}

/** What a term name denotes where it is read: an object, by the key prefix its members are solved
  * under (and what a clause over it imports, `Exposure`), or a value (a `val`, a parameter, a
  * `def`) with the type it is known to have, if any. Scala resolves a path's first name innermost
  * first over all of them.
  */
private[cardinality] enum Bound {
  case Module(prefix: TypeName, exposure: Exposure = Exposure.none)
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

/** One lexical layer a body adds to the scope it reads in. `rekey` gives the keys an enclosing
  * system solved its types under (an object's); without it the body is about to solve them.
  */
final private[cardinality] case class Layer(
    types: Set[TypeName] = Set.empty,
    terms: Map[TypeName, Bound] = Map.empty,
    imports: Imports = Imports.none,
    opened: Option[List[List[TypeName]]] = None,
    rekey: Option[TypeName => TypeName] = None,
)

/** One instantiation of `named` being solved for its own least fixed point: the approximation its
  * recursion on its own parameters reads (`value`), and whether it read one.
  */
final private[cardinality] class Recursion(val named: Named, val value: Size) {

  private var read = false

  def recursed: Boolean = read

  def notice(): Size = { read = true; value }
}

/** One reference's collection of reasons: each definition it reads adds its reasons once, and a
  * definition on a lazy cycle is named once, whichever key or plain name it is read by.
  */
final private[cardinality] class Taint(val values: Boolean = false) {

  private val seen = Taint.identities()

  private val lazies = Taint.identities()

  // What the reference read whose size is not whether it has a value, in the order met (`ValueRows`).
  private val readings = mutable.LinkedHashSet.empty[Overcount]

  def visit(named: Named): Boolean = seen.add(named)

  def blockedOnce(named: Named): Boolean = lazies.add(named)

  def read(overcount: Overcount): Unit = { readings += overcount; () }

  def overcounts: List[Overcount] = readings.toList
}

private object Taint {

  // Definitions by identity: two equal-looking equations are different definitions.
  def identities(): java.util.Set[Named] =
    java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Named, java.lang.Boolean])

}

/** The type members a class, trait or enum (`owner`) declares: what an instance path (`x.B`)
  * resolves to; abstract ones are supplied by implementors. `defn` is the definition, and `around`
  * builds the scope it is written in — where its parent clauses read (`Inheritance`); its own body
  * reads in `inside`, where a member's syntax means what it was written to mean.
  */
final private[cardinality] class Members(
    val owner: TypeName,
    val params: List[TypeName],
    val declared: Map[TypeName, Type],
    val abstractMembers: Set[TypeName],
    val defn: Defn,
    around: () => Scope,
) {

  lazy val outside: Scope = around()

  lazy val inside: Scope = Walk.bodyScope(outside, defn)(Walk.solved)
}

/** A declared type parameter and its own arity: `F[_]` has one, `F[_, _]` two, plain `A` none. */
final private[cardinality] case class Binder(name: TypeName, arity: Int) {

  def higherKinded: Boolean = arity > 0
}

/** A sealed parent's subtype, under the key its system solves it by: a module counts its one value;
  * any other is read with each of its parameters bound to the parent parameter its slot binds
  * (`bindings`, subtype parameter to parent parameter), or by its solved value when it has none.
  * `standard` gives, from the scope the family is read in, why it is not summed there: a name the
  * subtype's clause, fields or closedness take for the standard library's that a source binds;
  * `built`, why a module is not known to be built.
  */
final private[cardinality] case class Child(
    name: TypeName,
    module: Boolean,
    bindings: List[(TypeName, TypeName)] = Nil,
    standard: Scope => Option[String] = _ => None,
    built: Scope => Option[String] = _ => None,
)

final private[cardinality] class Scope(
    private val values: Map[TypeName, Size],
    private val notes: mutable.LinkedHashSet[Definition.Unbound],
    private val world: World,
    private val outer: Option[Scope] = None,
) {

  /** The members a namespace solved under `key`. */
  def membersAt(key: TypeName): Option[Members] = world.members.get(key)

  /** The same scope with a record of unresolved names of its own (its enclosing scopes' too): what
    * a reading of it notes stays in the reading, never in the scope a library holds for others.
    */
  private[cardinality] def detached: Scope =
    new Scope(values, mutable.LinkedHashSet.from(notes), world, outer.map(_.detached))

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
    * file defines. Then an explicit clause (followed to the member it names), then the wildcards
    * (latest first), then the packages the read's clauses open; a package the library has not
    * solved yet is a reason of its own.
    */
  def binding(name: TypeName): Binding =
    exported(name).orElse(Inheritance.bind(name, world.templates)).getOrElse {
      if (here(name) && !(world.imports.foreign(name) && wildcard(name).isDefined)) Binding.Here
      else
        world.imports.types
          .get(name)
          .flatMap(selected(name, _))
          .orElse(wildcard(name))
          .orElse(world.library.member(name, world, this))
          .getOrElse(Binding.Free)
    }

  // An explicit clause binds the name to the member its path names, read with the clauses written
  // before it: a supplied namespace's member under its key, or a reason naming the clause — or no
  // type at all when the namespace has only a term of that name, the one thing the clause imports.
  private def selected(name: TypeName, selector: Selector): Option[Binding] =
    Namespaces.target(withClauses(selector.prior), selector.path).selected(selector).map {
      case Namespaces.Found.Defined(in, key) => Binding.Elsewhere(in, key)
      case Namespaces.Found.Reason(why)      => Binding.Clause(why.renamed(name))
    }

  /** This scope reading with `imports` in place of its own clauses: where a clause's path is read.
    */
  def withClauses(imports: Imports): Scope =
    new Scope(values, notes, world.copy(imports = imports), outer)

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
      world.families.open(name) || world.families.unsummed.contains(name) ||
      world.families.refused.contains(name)

  // The first wildcard, latest first, that binds the name: a supplied namespace that defines it, or
  // one the sources do not supply, which may. Each reads its path with the wildcards before it. Two
  // wildcards of one body that both bind the name are an ambiguity Scala rejects (E049), so one
  // that may is no binding where another of its body defines the name, and two that define it
  // leave no binding to read.
  private def wildcard(name: TypeName): Option[Binding] = {
    val found = wildcards.indices.iterator.flatMap(i => wildcardAt(i, name).map(i -> _))
    found.nextOption().map { (i, binding) =>
      val beside = wildcards.indices.drop(i + 1).filter(j => wildcards(j).beside(wildcards(i)))
      (binding +: beside.flatMap(wildcardAt(_, name))).collect {
        case defined: Binding.Elsewhere => defined
      } match {
        case Seq()        => binding
        case Seq(defined) => defined
        case _            =>
          Binding.Clause(
            Definition.Unbound.Imported(name.value, "ambiguous: several wildcard imports bind it")
          )
      }
    }
  }

  private def wildcardAt(i: Int, name: TypeName): Option[Binding] = {
    val w = wildcards(i)
    if (w.excluded(name) || w.givens) None
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
      world.copy(imports = world.imports.copy(wildcards = world.imports.wildcards.drop(i + 1))),
      outer
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
      in.blocking(name).foreach(noteBlocked(name, _, in.definition(name)))
      if (world.session.taint.isDefined)
        in.definition(name).foreach(named => taintOf(named, in.home(name, named)))
      size
    }

  /** What a term name denotes here, innermost binding first — unless, for a file read with the rest
    * of its package, only another file binds it and a wildcard of this file names it too.
    */
  def term(name: TypeName): Option[Bound] =
    world.terms.get(name).filterNot { _ =>
      world.imports.foreign(name) && wildcards.indices.exists { i =>
        val w = wildcards(i)
        !w.excluded(name) && Namespaces.target(earlier(i), w.path).bindsTerm(name, w)
      }
    }

  /** The explicit clause that binds a term name, or a type name, if one does. */
  def clause(name: TypeName): Option[Selector] = world.imports.terms.get(name)

  def typeClause(name: TypeName): Option[Selector] = world.imports.types.get(name)

  def library: Library = world.library

  /** The packages the read's clauses open, innermost first; the empty package with none. */
  def openedPackages: List[List[TypeName]] = if (world.opened.isEmpty) List(Nil) else world.opened

  /** Whether a key names an abstraction this scope leaves open. */
  def isOpen(name: TypeName): Boolean = world.families.open(name)

  /** Why a sealed family this scope does not sum is not summed, if it is one. */
  def unsummed(name: TypeName): Option[String] = world.families.unsummed.get(name)

  /** Why a name that binds here to a declaration the estimate does not read is refused, if it is.
    */
  def refused(name: TypeName): Option[Definition.Unbound] = world.families.refused.get(name)

  /** Why the solver left a name on a cycle without a count, if it did. */
  def blocking(name: TypeName): Option[Blocked] = world.families.blocked.get(name)

  /** Which slots of a generic definition solved around the read are positive, if one is. */
  def slots(name: TypeName): Option[Positivity.Slots] = world.slots.get(name)

  /** Whether a name is a type parameter in scope that no instantiation has bound. */
  def unboundBinder(name: TypeName): Boolean =
    frame(name).isEmpty && world.binders.exists(_.name == name)

  /** Whether a name is a parameter the reading has not fixed: one no instantiation bound, or one a
    * template reading binds only to read a definition's reasons — a reference's to supply.
    */
  def unfixed(name: TypeName): Boolean =
    if (frame(name).isDefined) world.session.template || world.session.placeholders(name)
    else unboundBinder(name)

  /** The names that stand for `named`'s parameters when its own equation is what is being read: a
    * reference applying it to them is a recursion, not an instantiation.
    */
  def readingItself(named: Named): Option[List[Option[TypeName]]] =
    (world.session.self.toList ++ world.session.summing).find(_.named eq named).map(_.own)

  /** The approximation the instantiation of `named` being solved reads for its own recursion,
    * marking that the recursion was read.
    */
  def noticeRecursion(named: Named): Option[Size] =
    world.session.recursion.filter(_.named eq named).map(_.notice())

  /** Whether the sources bind a name here, which takes it from the standard library's base type of
    * that spelling: a root import is the outermost binding there is.
    */
  def binds(name: TypeName): Boolean = binding(name) != Binding.Free

  /** The declaration a bare type name denotes here, for telling two reads apart: the definition it
    * binds (one per declaration), the standard library's when no source binds it (`Some(None)`), or
    * nothing comparable — a type parameter, a clause's reason, an abstraction with no equation.
    */
  def declaration(name: TypeName): Option[Option[Named]] =
    if (isBinder(name)) None
    else
      binding(name) match {
        case Binding.Free               => Some(None)
        case Binding.Here               => definition(name).map(Some(_))
        case Binding.Elsewhere(in, key) => in.definition(key).map(Some(_))
        case Binding.Clause(_)          => None
      }

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
          recursion = session.recursion,
          saturating = session.saturating,
          recording = session.recording,
          taint = if (session.recording) Some(session.taint.getOrElse(new Taint)) else None,
        ),
      ),
      other.outer,
    )
  }

  /** Where `named`, bound to `name` here, was solved: the outermost scope around this read that
    * binds the name to that very definition. Its equation reads there — through its own statement's
    * view — never in a body nested inside, whose own types would shadow the names it reads.
    */
  @tailrec
  def home(name: TypeName, named: Named): Scope = outer match {
    case Some(o) if o.definition(name).exists(_ eq named) => o.home(name, named)
    case _                                                => this
  }

  /** `named`, bound to `name` here, read where it was solved, recording into this scope's notes. */
  def origin(name: TypeName, named: Named): Scope = {
    val at = home(name, named)
    if (at eq this) this else borrow(at)
  }

  /** This scope reading an instantiation: `frame` is the only one the equation reads,
    * `placeholders` are its names that stand for parameters the reader has not fixed, and
    * `arguments` what its names were bound to as written.
    */
  def instantiating(
      frame: Map[TypeName, Size],
      placeholders: Set[TypeName] = Set.empty,
      arguments: Map[TypeName, Argument] = Map.empty
  ): Scope =
    new Scope(
      values,
      notes,
      world.copy(
        frames = List(frame),
        session = world.session
          .copy(template = false, placeholders = placeholders, arguments = arguments, summing = Nil)
      ),
      outer
    )

  /** What the instantiation being read bound a frame name to, as written. */
  def argument(name: TypeName): Option[Argument] =
    frame(name).flatMap(_ => world.session.arguments.get(name))

  /** Notes the reasons `named` rests on, read in `context`, transitively: every solved name its
    * equation reads adds its own, once per definition — a sealed parent's equation reads its
    * subtypes. Its own parameters are read as bound — a reference supplies them or reads the
    * template on purpose — and every other binder stays a reason, whichever definition reads it.
    */
  def taintOf(named: Named, context: Scope): Unit =
    if (world.session.recording) {
      val session = world.session.taint.getOrElse(new Taint)
      if (session.visit(named)) {
        val _ = named.equation(reading(context, named.params, session))
      }
    }

  // A scope that records into this one's notes, reading `context`'s names with `bound` as the
  // template's own parameters: bound, and never a reason of the reader's.
  private def reading(context: Scope, bound: List[TypeName], session: Taint): Scope =
    new Scope(
      context.values,
      notes,
      context.world.copy(
        frames = List(bound.map(_ -> (UnitSize: Size)).toMap),
        session = context.world.session
          .copy(template = true, recording = true, taint = Some(session)),
      ),
      context.outer,
    )

  /** The names a body reads in, from a scope that solved them under their keys: its own types under
    * their plain names — shadowing the enclosing body's, and its explicit clauses shadowing both —
    * its term names, its clauses, its package clauses, and the enclosing binders it does not
    * shadow. A type the body defines is its own, whichever other file of a package defines it too.
    */
  def within(layer: Layer): Scope = {
    val named = layer.imports.types.keySet
    val hidden = layer.types ++ named
    val keys = layer.rekey.toList.flatMap(rekey => layer.types.map(n => n -> rekey(n)))
    // `--` walks what it removes: a package's thousands of types against a few entries is walked
    // from the smaller side, to the same result.
    def own[V](map: Map[TypeName, V]): Map[TypeName, V] =
      (if (hidden.size > map.size) map.filterNot((k, _) => hidden(k)) else map -- hidden) ++
        keys.flatMap((n, key) => map.get(key).map(n -> _))
    def ownSet(set: Set[TypeName]): Set[TypeName] =
      (if (hidden.size > set.size) set.filterNot(hidden) else set -- hidden) ++
        keys.collect { case (n, key) if set(key) => n }
    val families = world.families
    val imports = (world.imports ++ layer.imports).definingInside(layer.types ++ layer.terms.keySet)
    new Scope(
      own(values),
      notes,
      world.copy(
        definitions = own(world.definitions),
        members = own(world.members),
        families = Families(
          ownSet(families.open),
          own(families.unsummed),
          own(families.blocked),
          ownSet(families.members),
          own(families.refused)
        ),
        terms = (world.terms -- layer.imports.terms.keySet) ++ layer.terms,
        slots = own(world.slots),
        templates = world.templates.map(_.defining(layer.types, layer.terms.keySet)),
        imports = imports.copy(foreign = imports.foreign -- layer.types -- layer.terms.keySet),
        opened = layer.opened.getOrElse(world.opened),
        binders = world.binders.filterNot(b => hidden(b.name)),
      ),
      Some(this),
    )
  }

  /** Whether this scope solves, defines or holds the members of a name. */
  def defines(name: TypeName): Boolean = here(name)

  def updated(name: TypeName, size: Size): Scope =
    new Scope(values.updated(name, size), notes, world, outer)

  /** Names added by the solver's own round, which carries no notes of its own. */
  def ++(entries: Iterable[(TypeName, Size)]): Scope =
    new Scope(values ++ entries, notes, world, outer)

  /** The same names with a fresh record of unresolved ones: a row's (or a slot's) own reading,
    * which collects the reasons of every definition it reads — a number resting on a sibling's
    * unknown name is no more bounded than the sibling.
    */
  def measured: Scope = recorded(new Taint)

  /** The same for a value row, which also notes what is not known to be built (`Builds`). */
  def measuredValue: Scope = recorded(new Taint(values = true))

  def readingValue: Boolean = world.session.taint.exists(_.values)

  private def recorded(taint: Taint): Scope =
    new Scope(
      values,
      mutable.LinkedHashSet.empty,
      world.copy(session = world.session.copy(recording = true, taint = Some(taint))),
      outer
    )

  /** Whether this is the reading lent to the method side (`Session.saturating`). */
  def saturating: Boolean = world.session.saturating

  /** What this scope could not bound, by kind and in the order it met them. */
  def unbound: List[Definition.Unbound] = notes.toList

  /** Records a type the calculator could not bound: a name, a binder, an open abstraction, or
    * unmodelled syntax — an unplaceable name is a binder first, unknown only then. A template's own
    * parameter, applied, is the reference's to supply, not a reason.
    */
  def note(tpe: Type): Unit = tpe match {
    case Type.Name(name) if frame(TypeName.of(name)).isDefined && unfixed(TypeName.of(name)) => ()
    case _ => notes += reason(tpe); ()
  }

  /** Records a reason directly. */
  def note(reason: Definition.Unbound): Unit = { notes += reason; () }

  /** Records that the reference read a size that is not whether it has a value (`ValueRows`). */
  def note(overcount: Overcount): Unit = world.session.taint.foreach(_.read(overcount))

  def overcounts: List[Overcount] = world.session.taint.fold(Nil)(_.overcounts)

  /** Records a name directly, for a reason that is not a type's own syntax. */
  def note(name: String): Unit = note(Definition.Unbound.Unknown(name))

  /** Records an abstraction a member is supplied by, named as the reference is written. */
  def noteOpen(reference: String): Unit = note(Definition.Unbound.Open(reference))

  /** Records a sealed family whose subtypes the estimate does not sum, and why. */
  def noteUnsummed(name: TypeName, because: String): Unit =
    note(Scope.unsummedReason(name, Some(because)))

  /** Records that a value rests on a cycle the solver gives no count, once per definition. */
  def noteBlocked(name: TypeName, blocked: Blocked, named: Option[Named]): Unit =
    if (named.forall(n => world.session.taint.forall(_.blockedOnce(n))))
      note(Definition.Unbound.Refusal(blocked.about(name.value)))

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
      in.refused(key)
        .getOrElse(
          if (in.world.families.members(key)) Definition.Unbound.Member(name.value)
          else if (in.isOpen(key)) Definition.Unbound.Open(name.value)
          else if (in.unsummed(key).isDefined) Scope.unsummedReason(name, in.unsummed(key))
          else Definition.Unbound.Unknown(name.value)
        )
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

  // A fresh scope each time: its record of unresolved names is mutable, never shared.
  def empty: Scope = new Scope(Map.empty, mutable.LinkedHashSet.empty, World())

  /** Whether a name denotes one declaration in every one of `readers`: the standard library's in
    * all, or one definition (by identity) in all.
    */
  def sameDeclaration(readers: List[Scope], name: TypeName): Boolean =
    Traversals.defined(readers.map(_.declaration(name))) match {
      case Some(first :: rest) =>
        rest.forall(other =>
          other.isEmpty == first.isEmpty && other.forall(o => first.exists(_ eq o))
        )
      case _ => false
    }

  /** The reason a sealed family the estimate does not sum carries, in its row and in a reference:
    * the family, and why its subtypes are not its whole value space.
    */
  def unsummedReason(name: TypeName, because: Option[String]): Definition.Unbound =
    Definition.Unbound.Refusal(
      s"sealed family not summed: ${name.value}" + because.fold("")(b => s" ($b)")
    )

  private val worldLens =
    Lens[Scope, World](_.world, (s, w) => new Scope(s.values, s.notes, w, s.outer))

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
  val slotsLens = worldLens.andThen(lens[World](_.slots))
  val templatesLens = worldLens.andThen(lens[World](_.templates))
  val membersOfLens = familiesLens.andThen(lens[Families](_.members))
  val refusedLens = familiesLens.andThen(lens[Families](_.refused))
  val activeLens = sessionLens.andThen(lens[Session](_.active))
  val selfLens = sessionLens.andThen(lens[Session](_.self))
  val summingLens = sessionLens.andThen(lens[Session](_.summing))
  val recursionLens = sessionLens.andThen(lens[Session](_.recursion))
  val recordingLens = sessionLens.andThen(lens[Session](_.recording))
  val saturatingLens = sessionLens.andThen(lens[Session](_.saturating))

}

/** One named definition a reference can resolve: the type parameters it declares, what its type is
  * worth with those parameters in scope, the scope its own syntax reads in (`read`, from the scope
  * a reader hands it) — what a match type's case body is read in — and for an opaque alias, its
  * definition (`OpaqueScopes`).
  */
final private[cardinality] case class Named(
    params: List[TypeName],
    equation: Scope => Size,
    matchType: Option[Type] = None,
    children: List[Child] = Nil,
    read: Scope => Scope = identity,
    opaque: Option[Defn.Type] = None,
) {

  /** The same definition read through `view`: how a body hands its own syntax its lexical scope. */
  def viewed(view: Scope => Scope): Named =
    copy(
      equation = sc => equation(view(sc)),
      read = sc => read(view(sc))
    )

}
