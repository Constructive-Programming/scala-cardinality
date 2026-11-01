package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{MethodAnalysis, Resolver, StdlibModel}
import cardinality.types.{Spelling, TypeApplications}

import MethodAnalysis.{Frame, Resolved, TypeEntry}

/** Type names and member objects a template body inherits from its parents.
  *
  * Scala binds a simple name inside a template body to the template's members — inherited ones
  * included — before any enclosing declaration or import: with `type X = Unit` at package level and
  * `trait HasX { type X = Boolean }`, `X` inside `final class C extends HasX` is `Boolean` (a Scala
  * 3.9.0 fixture compiles `def g(): X = true` there). The first segment of a path is a name too:
  * `c.X` and `import c.*` read an inherited `object c`. A nearer declaration, a method's or the
  * template's own, still wins. An enclosing class, object, method or block that also declares the
  * name makes the reference ambiguous (E049), so a valid program never asks this reading to choose
  * there. A parent outside the source set may declare the name too: where the meaning would
  * otherwise come from a package, an import or the builtins — the bindings an inherited member
  * silently shadows — the reading is refused with that parent named, unless the parent is one of
  * the root and marker types that declare no members.
  *
  * The member is read under the arguments the `extends` chain gives its declaring parent
  * (`InheritedScopes`), and only from inside the body: a parent clause or a constructor parameter
  * sits outside it and sees the enclosing scope. Parent clauses are themselves resolved this way,
  * at their own position, so a template can extend a member class its enclosing template inherits.
  */
final private[cardinality] class InheritedTypeMembers(lookup: SourceLookup, resolver: Resolver)
    extends InheritedObjects {
  import InheritedTypeMembers.{Ancestry, Located, Location, Member, Outside}

  private val ancestries = mutable.Map.empty[String, Ancestry]
  private val members = mutable.Map.empty[(String, String, String), Option[Member]]

  // A name's meaning at one reference: a member reading and a type reading both ask for it.
  private val located =
    mutable.Map.empty[(String, String, Option[(String, Int)], String), Option[Location]]

  /** The inherited member a simple name means at the context's reference, with its declaring
    * scope's type environment, or why that meaning is not known; None when a nearer declaration or
    * no parent answers. A dotted name's first segment is read by the lookup (`at`).
    */
  def find(name: String, context: ResolutionContext): Option[Either[String, Member]] =
    locate(name, context).map(_.map(_._2))

  /** As `find`, with the template whose body inherits the member: the receiver whose `this` the
    * member is read on.
    */
  def locate(name: String, context: ResolutionContext): Option[Location] =
    if (name.contains('.')) None
    else
      located.getOrElseUpdate(
        (name, context.frame.id, context.at, context.useSite.id),
        scan(name, context)
      )

  private def scan(name: String, context: ResolutionContext): Option[Location] =
    context.at.flatMap { (file, position) =>
      // Inner frames first. Past a body with an unread parent, only a declaration of an
      // enclosing class, object, method or block keeps its meaning.
      @tailrec
      def walk(chain: List[Frame], unread: Option[String]): Option[Location] =
        chain match {
          case Nil                                                   => unread.map(Left(_))
          case frame :: _ if lookup.declaredIn(frame, name).nonEmpty =>
            unread.filter(_ => lookup.isPackage(frame)).map(Left(_))
          case frame :: outer if inBody(frame, file, position) =>
            val ancestry = ancestryOf(frame)
            val inherited =
              if (ancestry.names(name)) member(frame, name, context.useSite) else None
            (inherited, unread) match {
              case (Some(found), None)     => Some(Right(frame -> found))
              case (Some(_), Some(reason)) => Some(Left(reason))
              case (None, _) => walk(outer, unread.orElse(unreadReason(name, ancestry)))
            }
          case _ :: outer => walk(outer, unread)
        }
      walk(context.frame.chain, None)
    }

  /** The parents of a template's supplied ancestry that are outside the source set, as spelled. */
  def unsuppliedAncestors(frame: Frame): List[String] = ancestryOf(frame).unsupplied

  def at(head: InheritedObjects.Head): InheritedObjects.Path =
    if (!inBody(head.frame, head.file, head.position)) None
    else {
      val ancestry = ancestryOf(head.frame)
      if (ancestry.objects(head.name)) objectPath(head.frame, head.name)
      else unreadReason(head.name, ancestry).map(Left(_))
    }

  // An inherited member object as a path: its declaring parent read with its own binders, since a
  // path carries no type arguments. Under a parent the chain instantiates, the object is a member
  // of another type than the path reads, so the reading is refused.
  private def objectPath(frame: Frame, head: String): InheritedObjects.Path =
    InheritedScopes
      .walk(List(frame -> resolver.typeParameters(frame)), resolver, frame)
      .collect { case Right(step) => step }
      .flatMap(step =>
        lookup
          .moduleIn(step.declared, head)
          .filter(module => module.tree.forall(visible(_, step.declared, frame)))
          .map(step -> _)
      )
      .headOption
      .map { (step, module) =>
        if (resolver.typeParameters(step.declared).isEmpty) Right(module.path)
        else
          Left(
            s"inherited member path not resolved: $head (type arguments of ${Spelling.of(step.parent.tpe)})"
          )
      }

  private def unreadReason(name: String, ancestry: Ancestry): Option[String] =
    ancestry.unsupplied.headOption.map(parent =>
      s"inherited type member not resolved: $name (unsupplied parent $parent)"
    )

  private def inBody(frame: Frame, file: String, position: Int): Boolean =
    frame.input == file && frame.tree.flatMap(TypeApplications.template).exists { templ =>
      templ.body.pos.start <= position && position < templ.body.pos.end
    }

  /** The declaration an `extends` clause names, read at the clause's own position in the scope
    * around the template, with the environment of the scope that declares it; None for a parent
    * outside the source set.
    */
  def parent(init: Init, from: Frame): Either[String, Option[Member]] =
    located(init, from).map {
      case Located.Supplied(member) => Some(member)
      case _                        => None
    }

  /** Whether a parent clause names a type whose members are not read: one an import binds to a path
    * outside the source set, one found nowhere unless it is a root or marker type, or one the
    * lookup cannot pin down.
    */
  def unsupplied(init: Init, from: Frame): Boolean = located(init, from) match {
    case Right(Located.Supplied(_))          => false
    case Right(Located.Unsupplied(external)) => !InheritedTypeMembers.memberless(external)
    case _                                   => true
  }

  /** A parent clause outside the source set, or None for a supplied parent or one the lookup cannot
    * pin down.
    */
  def outside(init: Init, from: Frame): Option[Outside] = located(init, from) match {
    case Right(Located.Unsupplied(external)) => Some(external)
    case _                                   => None
  }

  private def located(init: Init, from: Frame): Either[String, Located] = {
    val name = InheritedScopes.typeName(init.tpe)
    val outer = from.parent.getOrElse(from)
    val context =
      ResolutionContext(outer, Map.empty, Set.empty, at = Some(from.input -> init.pos.start))
    find(name, context) match {
      case Some(found) => found.map(Located.Supplied(_))
      case None        =>
        lookup.resolveName(
          name,
          outer,
          context.at,
          resolver.accessible,
          resolver.inspect,
          this
        ) match {
          case Found.Entry(entry) =>
            Right(Located.Supplied(entry -> resolver.typeParameters(entry.owner)))
          case Found.Blocked(reason)        => Left(reason)
          case Found.Foreign(path, binding) => Right(unsupplied(path, binding, context))
          case Found.Absent => Right(unsupplied(name, StdlibModel.undeclared, context))
        }
    }
  }

  // A parent the stdlib model reads under its path is the root imports' declaration only where no
  // channel binds the name otherwise — an unsupplied wildcard beside the clause may declare its own
  // `Serializable` or `IterableOnce`.
  private def unsupplied(path: String, binding: String, context: ResolutionContext): Located = {
    val modelled = InheritedTypeMembers.markerName(path) || StdlibModel.entry(path).nonEmpty
    Located.Unsupplied(
      Outside(path, binding, Option.when(modelled)(resolver.rootShadow(path, context)).flatten)
    )
  }

  // The names and objects every supplied ancestor declares, and the ancestors outside the source
  // set, by template: parent resolution alone, so asking costs no argument reading until a name is
  // really inherited. A template met again while its own ancestry is read (an invalid cycle) adds
  // nothing.
  private def ancestryOf(frame: Frame): Ancestry =
    ancestries.get(frame.id) match {
      case Some(known) => known
      case None        =>
        ancestries.update(frame.id, Ancestry(Set.empty, Set.empty, Nil))
        val found = InheritedScopes
          .breadthFirst(frame.parents.map(frame -> _), Set.empty[String]) {
            case ((from, init), seen) =>
              located(init, from) match {
                case Right(Located.Supplied((entry, _))) =>
                  entry.self match {
                    case Some(declared) if seen(declared.id) => (Nil, Nil, seen)
                    case Some(declared)                      =>
                      (
                        List(Right(declared)),
                        declared.parents.map(declared -> _),
                        seen + declared.id
                      )
                    // An alias or abstract type as a parent: what it declares is not read.
                    case None => (List(Left(init.tpe.syntax)), Nil, seen)
                  }
                case Right(Located.Unsupplied(external))
                    if InheritedTypeMembers.memberless(external) =>
                  (Nil, Nil, seen)
                case _ => (List(Left(init.tpe.syntax)), Nil, seen)
              }
          }
        val declared = found.collect { case Right(frame) => frame }
        val ancestry = Ancestry(
          declared.flatMap(lookup.declaredNames).toSet,
          declared.flatMap(lookup.declaredModules).toSet,
          found.collect { case Left(parent) => parent }
        )
        ancestries.update(frame.id, ancestry)
        ancestry
    }

  // An alias is the member's meaning wherever it is declared (Scala lets an alias be overridden
  // only by an equal one); an abstract declaration or a class member is read as itself.
  private def member(frame: Frame, name: String, useSite: Frame): Option[Member] =
    members.getOrElseUpdate(
      (frame.id, name, useSite.id), {
        val found = InheritedScopes
          .walk(List(frame -> resolver.typeParameters(frame)), resolver, useSite)
          .collect { case Right(step) => step }
          .flatMap(step =>
            lookup
              .declaredIn(step.declared, name)
              .filter(entry => visible(entry.tree, entry.owner, useSite))
              .map(_ -> step.env)
          )
        found.find((entry, _) => alias(entry)).orElse(found.headOption)
      }
    )

  /** Whether a member is inherited: a private one is not, a qualified `private[p]` one is where `p`
    * reaches.
    */
  def visible(tree: Tree, owner: Frame, useSite: Frame): Boolean = tree match {
    case d: Stat.WithMods if d.mods.exists(_.is[Mod.Private]) =>
      resolver.accessible(d, owner, useSite)
    case _ => true
  }

  private def alias(entry: TypeEntry): Boolean = entry.tree match {
    case d: Defn.Type => !d.mods.exists(_.is[Mod.Opaque])
    case _            => false
  }

}

private[cardinality] object InheritedTypeMembers {

  /** A member found, with the type environment of the scope that declares it. */
  type Member = (TypeEntry, Map[String, Resolved])

  /** An inherited member with the template whose body inherits it, or why it is not known. */
  type Location = Either[String, (Frame, Member)]

  /** What a template inherits: the type names and objects its supplied ancestors declare, and its
    * ancestors outside the source set (as spelled), whose declarations are unknown.
    */
  final case class Ancestry(names: Set[String], objects: Set[String], unsupplied: List[String])

  /** A parent clause outside the source set: the path it names (as an import binds it, or as
    * written when nothing does), how the name was bound, and the channel through which a
    * declaration other than the root imports' may bind it, for a path the stdlib model reads.
    */
  final case class Outside(path: String, binding: String, shadow: Option[String])

  /** Where a parent clause's name leads: a supplied declaration, or outside the source set. */
  enum Located {
    case Supplied(member: Member)
    case Unsupplied(outside: Outside)
  }

  /** Whether a parent outside the source set declares no type member: a root or marker type, or a
    * parent the stdlib model summarizes, that no channel binds to another declaration.
    */
  def memberless(outside: Outside): Boolean = outside.shadow.isEmpty && markerName(outside.path)

  /** Whether a parent spelled this way, through the root imports, is a stdlib parent the model
    * knows to declare no type member: a root or marker type (`Dynamic` only enables a rewrite),
    * `Product`, `Equals` or `IterableOnce`. Only a name that resolves to nothing is one of them —
    * an import may bind `Serializable` to anything.
    */
  def markerName(name: String): Boolean = StdlibModel.parent(name).nonEmpty

}
