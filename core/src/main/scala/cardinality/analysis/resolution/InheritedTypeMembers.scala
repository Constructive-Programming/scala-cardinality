package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.{ResolutionContext, ScopeTerms}
import cardinality.analysis.{MethodAnalysis, Resolver, StdlibModel}
import cardinality.types.Trees.hasOpaque
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
  import InheritedTypeMembers.{Ancestry, Located, Location, Member, MemberKind, Outside}

  private val ancestries = mutable.Map.empty[String, Ancestry]
  // The export clauses being read for a name: met again through inheritance, the body is taken to
  // declare it, and lexical lookup, which follows exports itself, answers.
  private val exporting = mutable.Set.empty[(String, String)]
  private val members = mutable.Map.empty[(String, String, String), List[Member]]

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
          case Nil                                 => unread.map(Left(_))
          case frame :: _ if declares(frame, name) =>
            unread.filter(_ => lookup.isPackage(frame)).map(Left(_))
          case frame :: outer if inBody(frame, file, position) =>
            val ancestry = ancestryOf(frame)
            val inherited =
              if (ancestry.names(name)) member(frame, name, context.useSite) else None
            (inherited, unread) match {
              case (Some(found), None)     => Some(Right(frame -> found))
              case (Some(_), Some(reason)) => Some(Left(reason))
              case (None, _)               =>
                walk(
                  outer,
                  unread.orElse(unreadReason(name, ancestry, exportsName(_, name), MemberKind.Type))
                )
            }
          case _ :: outer => walk(outer, unread)
        }
      walk(context.frame.chain, None)
    }

  /** The parents of a template's supplied ancestry that are outside the source set, as spelled. */
  def unsuppliedAncestors(frame: Frame): List[String] = ancestryOf(frame).unsupplied

  /** A template's supplied base classes (itself and its supplied ancestors), and whether they are
    * all of them: no parent is left unread.
    */
  def baseClasses(frame: Frame): (Set[String], Boolean) = {
    val ancestry = ancestryOf(frame)
    (ancestry.classes + frame.id, ancestry.unread.isEmpty)
  }

  // A body declares a name by a declaration or by an export clause, which defines an alias member
  // — one an exported object inherits included.
  private def declares(frame: Frame, name: String): Boolean =
    lookup.declaredIn(frame, name).nonEmpty || exportsName(frame, name)

  private def exportsName(frame: Frame, name: String): Boolean =
    lookup.exporting(frame) && {
      val key = frame.id -> name
      exporting(key) || {
        exporting += key
        try lookup.exportsName(frame, name, resolver.accessible, this)
        finally exporting -= key
      }
    }

  // What `import O.*`, `import O.T` and `O.T` read when the object `O` only inherits `T`: the member
  // under its declaring parent, followed only where that parent takes no type arguments (a path
  // carries none), and refused where an ancestor outside the sources may declare it. A protected
  // member is selected only from inside `O` or its declaring parent.
  override def inheritedBy(module: Frame, name: String, from: Frame): InheritedObjects.Selected = {
    val ancestry = ancestryOf(module)
    val declared = Option
      .when(ancestry.names(name))(
        pick(
          declarations(module, name, module).filter((entry, _) =>
            selectable(entry.tree, entry.owner, module, from)
          )
        )
      )
      .flatten
      .map { (entry, _) =>
        if (resolver.typeParameters(entry.owner).isEmpty) Right(entry)
        else
          Left(
            s"inherited member path not resolved: $name (type arguments of ${entry.owner.path.last})"
          )
      }
    declared.orElse(
      unreadReason(name, ancestry, exportsName(_, name), MemberKind.Type).map(Left(_))
    )
  }

  // A member object is no given, and a protected one is selected only from inside the template or
  // its declaring parent.
  override def inheritedTerm(
      template: Frame,
      name: String,
      givens: Boolean,
      from: Frame
  ): InheritedObjects.Path = {
    val ancestry = ancestryOf(template)
    Option
      .when(!givens && ancestry.objects(name))(objectPath(template, name, Some(from)))
      .flatten
      .orElse(
        Option.when(ancestry.imported(givens)(name))(
          Left(s"path through a value not followed: $name")
        )
      )
      .orElse(unreadReason(name, ancestry, lookup.mayExport(_, name), MemberKind.Term).map(Left(_)))
  }

  def at(head: InheritedObjects.Head): InheritedObjects.Path =
    if (!inBody(head.frame, head.file, head.position)) None
    else {
      val ancestry = ancestryOf(head.frame)
      if (ancestry.objects(head.name)) objectPath(head.frame, head.name)
      // An inherited value: a path through it selects a member of its type.
      else if (ancestry.values(head.name))
        Some(Left(s"path through a value not followed: ${head.name}"))
      else
        unreadReason(head.name, ancestry, lookup.mayExport(_, head.name), MemberKind.Term)
          .map(Left(_))
    }

  // An inherited member object as a path: its declaring parent read with its own binders, since a
  // path carries no type arguments. Under a parent the chain instantiates, the object is a member
  // of another type than the path reads, so the reading is refused.
  private def objectPath(
      frame: Frame,
      head: String,
      from: Option[Frame] = None
  ): InheritedObjects.Path =
    InheritedScopes
      .walk(List(frame -> resolver.typeParameters(frame)), resolver, frame)
      .collect { case Right(step) => step }
      .flatMap(step =>
        lookup
          .moduleIn(step.declared, head)
          .filter(module =>
            module.tree.forall(tree =>
              visible(tree, step.declared, frame) &&
                from.forall(selectable(tree, step.declared, frame, _))
            )
          )
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

  // Why a name a template may inherit is not read: an ancestor this reading does not read may
  // declare any name; one whose export clauses define the name defines an alias member this
  // reading does not follow (`exported`: for a type name, one its selectors or the supplied
  // namespaces its wildcards open define; for a path's head, a term name any selector may spell).
  private def unreadReason(
      name: String,
      ancestry: Ancestry,
      exported: Frame => Boolean,
      kind: MemberKind
  ): Option[String] =
    ancestry.unread.headOption
      .orElse(
        ancestry.exporters
          .find(exported)
          .map(frame => InheritedTypeMembers.exportsOf(frame.path.last))
      )
      .map(InheritedTypeMembers.unresolved(kind, name, _))

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
          case Found.Blocked(reason, _)     => Left(reason)
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

  // What a template inherits (`Ancestry`), by template: parent resolution alone, so asking costs no
  // argument reading until a name is really inherited. A template met again while its own ancestry
  // is read (an invalid cycle) adds nothing.
  private def ancestryOf(frame: Frame): Ancestry =
    ancestries.get(frame.id) match {
      case Some(known) => known
      case None        =>
        ancestries.update(frame.id, Ancestry.none)
        val found = InheritedScopes
          .breadthFirst(frame.parents.map(frame -> _), Set.empty[String]) {
            case ((from, init), seen) =>
              val location = located(init, from)
              location match {
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
                    case None => (List(Left(unread(init, location))), Nil, seen)
                  }
                case Right(Located.Unsupplied(external))
                    if InheritedTypeMembers.memberless(external) =>
                  (Nil, Nil, seen)
                case _ => (List(Left(unread(init, location))), Nil, seen)
              }
          }
        val declared = found.collect { case Right(frame) => frame }
        val (spelled, channels) = found.collect { case Left(parent) => parent }.unzip
        // An ancestor's export clauses define members it passes on like its declarations: the
        // names they define are not read through it.
        val ancestry = Ancestry(
          declared.flatMap(lookup.declaredNames).toSet,
          declared.flatMap(lookup.declaredModules).toSet,
          declared.flatMap(InheritedTerms.values).toSet,
          declared.flatMap(InheritedTerms.imported(_, givens = false)).toSet,
          declared.flatMap(InheritedTerms.imported(_, givens = true)).toSet,
          spelled,
          channels,
          declared.filter(lookup.exporting),
          declared.map(_.id).toSet
        )
        ancestries.update(frame.id, ancestry)
        ancestry
    }

  // A parent this reading does not read, as spelled and as the channel a reason names.
  private def unread(init: Init, location: Either[String, Located]): (String, String) = {
    val parent = init.tpe.syntax
    parent -> (location match {
      // A marker or model parent another declaration may stand behind.
      case Right(Located.Unsupplied(Outside(_, _, Some(route)))) =>
        s"parent $parent, which may be bound $route"
      case Left(_) => s"parent $parent, whose declaration is not resolved"
      case _       => s"unsupplied parent $parent"
    })
  }

  private def member(frame: Frame, name: String, useSite: Frame): Option[Member] =
    pick(declarations(frame, name, useSite))

  // Every inherited declaration of the name, nearest level first.
  private def declarations(frame: Frame, name: String, useSite: Frame): List[Member] =
    members.getOrElseUpdate(
      (frame.id, name, useSite.id),
      InheritedScopes
        .walk(List(frame -> resolver.typeParameters(frame)), resolver, useSite)
        .collect { case Right(step) => step }
        .flatMap(step =>
          lookup
            .declaredIn(step.declared, name)
            .filter(entry => visible(entry.tree, entry.owner, useSite))
            .map(_ -> step.env)
        )
    )

  // An alias is the member's meaning wherever it is declared (Scala lets an alias be overridden
  // only by an equal one); an abstract declaration or a class member is read as itself.
  private def pick(found: List[Member]): Option[Member] =
    found.find((entry, _) => alias(entry)).orElse(found.headOption)

  // A protected member is selected through an object only inside that object or the template that
  // declares it.
  private def selectable(tree: Tree, owner: Frame, module: Frame, from: Frame): Boolean =
    !ScopeTerms.protectedMember(tree) ||
      from.chain.exists(scope => scope.id == module.id || scope.id == owner.id)

  /** Whether a member is inherited: a private one is not, a qualified `private[p]` one is where `p`
    * reaches.
    */
  def visible(tree: Tree, owner: Frame, useSite: Frame): Boolean = tree match {
    case d: Stat.WithMods if d.mods.exists(_.is[Mod.Private]) =>
      resolver.accessible(d, owner, useSite)
    case _ => true
  }

  private def alias(entry: TypeEntry): Boolean = entry.tree match {
    case d: Defn.Type => !d.mods.hasOpaque
    case _            => false
  }

}

private[cardinality] object InheritedTypeMembers {

  /** A member found, with the type environment of the scope that declares it. */
  type Member = (TypeEntry, Map[String, Resolved])

  /** The namespace an inherited name is read in: a type name, or a path's first name (a term). */
  enum MemberKind(val label: String) {
    case Type extends MemberKind("type")
    case Term extends MemberKind("term")
  }

  /** Why a name a template may inherit is not read, on both sides (the stored estimate's
    * `Inheritance` lends constructor slots this reading compares): `channel` says how a parent may
    * define it.
    */
  def unresolved(kind: MemberKind, name: String, channel: String): String =
    s"inherited ${kind.label} member not resolved: $name ($channel)"

  /** The channel through which a parent's export clauses may define a name. */
  def exportsOf(parent: String): String = s"exports of $parent"

  /** An inherited member with the template whose body inherits it, or why it is not known. */
  type Location = Either[String, (Frame, Member)]

  /** What a template inherits: the type names, objects and values its supplied ancestors declare
    * (and the values a wildcard, or a `given` selector, over the template's object imports), its
    * ancestors outside the source set (as spelled), whose declarations are unknown, every channel
    * through which it may inherit any name this reading does not know — such an ancestor, or a
    * marker parent another declaration may stand behind — the supplied ancestors whose export
    * clauses define members, which this reading does not follow for the names they define, and the
    * frames of the supplied ancestors.
    */
  final case class Ancestry(
      names: Set[String],
      objects: Set[String],
      values: Set[String],
      wildcardTerms: Set[String],
      givenTerms: Set[String],
      unsupplied: List[String],
      unread: List[String],
      exporters: List[Frame],
      classes: Set[String]
  ) {

    def imported(givens: Boolean): Set[String] = if (givens) givenTerms else wildcardTerms
  }

  object Ancestry {

    val none: Ancestry =
      Ancestry(Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Nil, Nil, Nil, Set.empty)

  }

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
