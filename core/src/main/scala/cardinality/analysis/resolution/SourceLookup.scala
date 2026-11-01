package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.{Tree, Type}

import cardinality.analysis.*
import cardinality.analysis.model.{ResolutionContext, ScopeTerms}
import cardinality.types.TypeApplications

import MethodAnalysis.{Frame, TypeEntry}

/** What a simple-name resolution reached: one declaration, a refusal to read past the name with the
  * reason, a declaration outside the source set, or nothing — which is where the model's own
  * vocabulary may still answer.
  */
sealed private[cardinality] trait Found

private[cardinality] object Found {
  final case class Entry(entry: TypeEntry) extends Found

  /** A lookup that reached neither a supplied declaration nor the builtins, with its reason. */
  sealed trait Failed extends Found {
    def reason: String
  }

  final case class Blocked(reason: String) extends Failed

  // An import binds the name to a path outside the source set: a declaration that exists, but was
  // not supplied — or one the stdlib model reads (`import java.util.List`).
  final case class Foreign(path: String, binding: String) extends Failed {

    /** Why a name bound outside the sources is not read: the information is absent. */
    def reason: String = s"declaration not supplied: $path ($binding)"
  }

  case object Absent extends Found

  /** What a type written as a name (`T`, `p.T`) denotes where it is read; None for any other type.
    */
  def named(
      resolver: Resolver,
      tpe: Type,
      name: String,
      context: ResolutionContext
  ): Option[Found] =
    tpe match {
      case _: Type.Name | _: Type.Select => Some(resolver.declaration(tpe, name, context))
      case _                             => None
    }

}

/** The member objects a template body inherits, which the first segment of a path may name: at a
  * position inside `frame`'s body, the inherited object's path, or the reason a parent outside the
  * source set leaves the name unread; None where the frame inherits nothing under the name.
  */
private[cardinality] trait InheritedObjects {
  def at(head: InheritedObjects.Head): InheritedObjects.Path

  /** The type member a supplied object inherits under `name`, as a path read in `from` selects it,
    * or why it is not read; None when no ancestor declares one `from` may select.
    */
  def inheritedBy(module: Frame, name: String, from: Frame): InheritedObjects.Selected = None

}

private[cardinality] object InheritedObjects {

  /** A path's first segment, read inside `frame` at a position of `file`. */
  final case class Head(name: String, frame: Frame, file: String, position: Int)

  type Path = Option[Either[String, List[String]]]

  /** An inherited type member a path selects, or why it is not read. */
  type Selected = Option[Either[String, TypeEntry]]

  val none: InheritedObjects = _ => None
}

/** Declaration buckets preserve insertion order and ambiguity; lookup never scans unrelated types.
  *
  * Import clauses bind simple names inside the block that holds them, from the clause's own
  * position onward. A reference whose scope holds such a binding resolves through it or is refused
  * there: the enclosing packages and the model's builtin vocabulary are out of reach of that name,
  * exactly as the compiler denies them when the import brings an unavailable target. Scala's
  * precedence decides the rest: a declaration of the same file or a named import before a wildcard,
  * and a wildcard before a package member of another file — a wildcard of a stdlib namespace the
  * model lists included, for the names it lists (`listed`: the declaration such a wildcard in one
  * scope binds a name to, before a position).
  */
final private[cardinality] class SourceLookup(
    types: List[TypeEntry],
    packages: List[Frame],
    modules: List[Frame],
    imports: Map[String, List[ImportBinding]],
    listed: (String, Frame, Option[(String, Int)]) => Option[String],
    exports: Map[String, List[ExportClause]] = Map.empty
) {
  private val lexical = types.groupBy(entry => entry.owner.id -> entry.name)
  private val names = types.groupMapReduce(_.owner.id)(entry => Set(entry.name))(_ ++ _)

  private val qualified = types
    .filterNot(_.owner.chain.exists(_.local))
    .groupBy(_.key)

  private val packagePaths = packages.groupBy(_.path)
  private val packageIds = packages.map(_.id).toSet
  private val packageRoots = packages.flatMap(_.path.headOption).toSet
  private val moduleIndex = modules.flatMap(m => m.parent.map(p => (p.id, m.path.last) -> m)).toMap
  private val moduleNames = moduleIndex.keys.groupMapReduce(_._1)(key => Set(key._2))(_ ++ _)
  private val modulePaths = modules.groupBy(_.path)

  // The paths a wildcard can open: packages (with every prefix), objects and type declarations.
  private val namespaces: Set[String] =
    packages.flatMap(_.path.inits).map(_.mkString(".")).toSet ++
      modules.map(_.path.mkString(".")) ++ qualified.keySet

  def peers(path: List[String]): List[Frame] = packagePaths.getOrElse(path, Nil)

  // A constructor's frame shares its class's id and path but holds none of its body: its parameters
  // are typed outside the template (Scala 3.9.0 reads `class C(x: Any) { type Any = Boolean }` with
  // `scala.Any`), so neither the body's declarations nor the template's path are read from it.
  private def constructorView(frame: Frame): Boolean =
    frame.stats.isEmpty && frame.tree
      .flatMap(TypeApplications.template)
      .exists(_.body.stats.nonEmpty)

  /** The type declarations one frame's own body makes under a name, and every name it declares. */
  def declaredIn(frame: Frame, name: String): List[TypeEntry] =
    lexical.getOrElse(frame.id -> name, Nil)

  def declaredNames(frame: Frame): Set[String] = names.getOrElse(frame.id, Set.empty)

  /** The object one frame's own body declares under a name, and every object it declares. */
  def moduleIn(frame: Frame, name: String): Option[Frame] = moduleIndex.get(frame.id -> name)

  def declaredModules(frame: Frame): Set[String] = moduleNames.getOrElse(frame.id, Set.empty)

  def isPackage(frame: Frame): Boolean = packageIds(frame.id)

  /** Whether the frame's own body holds an export clause. */
  def exporting(frame: Frame): Boolean = exports.contains(frame.id)

  /** Whether an export clause of the frame's own body may define a term under the name: a selector
    * spelling it, or a wildcard that does not leave it out.
    */
  def mayExport(frame: Frame, name: String): Boolean =
    exports.getOrElse(frame.id, Nil).exists(clause => ExportClause.mayDefine(clause.importer, name))

  /** Whether an export clause of the frame's own body defines a member under the name, reading the
    * members exported objects inherit through `objects`.
    */
  def exportsName(
      frame: Frame,
      name: String,
      accessible: (Tree, Frame, Frame) => Boolean,
      objects: InheritedObjects
  ): Boolean =
    Reader(accessible, _ => (), objects).exported(name, frame, Set.empty).nonEmpty

  // The definition-only contract, for callers that resolve outside any reference position:
  // a list whose size carries ambiguity, exactly as before imports were modeled.
  def lookup(
      name: String,
      frame: Frame,
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): List[TypeEntry] = Reader(accessible, inspect, InheritedObjects.none).definitions(name, frame)

  def resolveName(
      name: String,
      frame: Frame,
      at: Option[(String, Int)],
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit,
      objects: InheritedObjects = InheritedObjects.none
  ): Found = {
    val reader = Reader(accessible, inspect, objects)
    name.split('.').toList match {
      case List(simple) => reader.simple(simple, frame, at)
      case segments     => reader.dotted(segments, frame, at)
    }
  }

  /** One resolution's access rule, budget and view of inherited member objects. */
  private case class Reader(
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit,
      objects: InheritedObjects
  ) {

    def definitions(name: String, frame: Frame): List[TypeEntry] =
      lexicalHit(name, frame).map(_._1).getOrElse(candidates(name, frame))

    def simple(name: String, frame: Frame, at: Option[(String, Int)]): Found = {
      val definition = lexicalHit(name, frame)
      val member = nearestMember(name, frame, definition)
      binding(name, frame, at) match {
        case Some((depth, targets)) =>
          member match {
            // A declaration nearer than every binding for the name keeps it, import or not; an
            // export defines a member, so it keeps the name against an import of its own body too.
            case Some(Member(found, d, exported)) if d < depth || (exported && d == depth) => found
            case _ => imported(name, targets, definition, depth)
          }
        case None =>
          member match {
            case Some(Member(found, _, _)) => found
            // A wildcard import outranks a package member of another file (one of the same file is
            // a definition, found above; Scala reports E049 when both bind the name).
            case None =>
              wildcards(name, frame, at) match {
                case Found.Absent =>
                  candidates(name, frame) match {
                    case Nil     => Found.Absent
                    case List(e) => Found.Entry(e)
                    case _       => Found.Blocked(s"ambiguous type: $name")
                  }
                case found => found
              }
          }
      }
    }

    // The nearest member of an enclosing body under the name: a declaration, or an alias an export
    // clause defines (Scala 3 reference, export clauses: exported type members are aliased by type
    // definitions of the template). An export ranks with the body's own declarations, so it hides
    // every outer declaration; both in one body is a double definition, refused.
    private def nearestMember(
        name: String,
        frame: Frame,
        definition: Option[(List[TypeEntry], Int)]
    ): Option[Member] = {
      val declared = definition.map((found, d) => Member(entryOrAmbiguous(found, name), d, false))
      val exported = frame.chain.iterator.zipWithIndex
        .filterNot((scope, _) => constructorView(scope))
        .flatMap((scope, index) =>
          this.exported(name, scope, Set.empty).map(Member(_, index, true))
        )
        .nextOption()
      (declared, exported) match {
        case (Some(d), Some(e)) if d.depth == e.depth =>
          Some(
            Member(Found.Blocked(s"ambiguous type: $name (declared and exported)"), d.depth, true)
          )
        case (Some(d), Some(e)) => Some(if (e.depth < d.depth) e else d)
        case (d, e)             => d.orElse(e)
      }
    }

    /** The member an export clause of `scope` defines under `name`; None when none defines it. */
    def exported(name: String, scope: Frame, seen: Set[String]): Option[Found] =
      if (seen(scope.id)) Some(Found.Blocked(s"cyclic export: $name"))
      else
        exports.getOrElse(scope.id, Nil).flatMap(defines(_, name, scope, seen + scope.id)) match {
          case Nil   => None
          case found =>
            found.distinct match {
              case List(one) => Some(one)
              case _         => Some(Found.Blocked(s"ambiguous export: $name"))
            }
        }

    // What one exporter defines under the name: the member its explicit selector forwards, the
    // member of that name its wildcard's namespace holds, or a refusal where the namespace is not
    // read — a prefix that is no path, or one outside the sources, may define any name.
    private def defines(
        clause: ExportClause,
        name: String,
        scope: Frame,
        seen: Set[String]
    ): Option[Found] =
      clause.binding match {
        case None =>
          Option.when(ExportClause.mayDefine(clause.importer, name))(
            Found.Blocked(s"export not followed: $name (export of ${clause.prefix})")
          )
        case Some(binding) =>
          val at = before(scope.input, binding)
          binding.named.collectFirst { case (`name`, segments) => segments } match {
            case Some(segments) => Some(forwarded(Target(segments, scope, at), name, seen))
            case None           =>
              binding
                .opening(name)
                .headOption
                .flatMap(prefix => opened(Target(prefix, scope, at), name, seen))
          }
      }

    // An explicit export selector's target, read where the clause stands: what is absent is named
    // through the import that bound its prefix, if one did, then by the export.
    private def forwarded(target: Target, name: String, seen: Set[String]): Found =
      through(target, seen) { origin =>
        val bound = origin.fold(Origin.named(target.segments, name, "export"))(
          _.exportedBy(target.scope)
        )
        Found.Foreign(bound.path, bound.binding)
      }

    // A path read through whatever binds its first segment; `missing` answers where no supplied
    // member is behind it, given the import that bound the segment.
    private def through(target: Target, seen: Set[String])(
        missing: Option[Origin] => Found
    ): Found =
      absolute(target.segments, target.scope, target.at) match {
        case Left(reason)           => Found.Blocked(reason)
        case Right((paths, origin)) => first(paths, target.scope, seen).getOrElse(missing(origin))
      }

    // A wildcard export's member under the name: the first supplied namespace its prefix denotes
    // answers, a declaration or nothing; a prefix no supplied namespace answers may define anything.
    private def opened(prefix: Target, name: String, seen: Set[String]): Option[Found] =
      absolute(prefix.segments, prefix.scope, prefix.at) match {
        case Left(reason)      => Some(Found.Blocked(reason))
        case Right((paths, _)) =>
          paths.find(path => namespaces(path.mkString("."))) match {
            case Some(path) =>
              Some(member(path :+ name, prefix.scope, seen)).filter(_ != Found.Absent)
            case None =>
              Some(
                Found.Blocked(
                  s"wildcard export not supplied: $name (wildcard export of " +
                    s"${prefix.segments.mkString(".")})"
                )
              )
          }
      }

    // The nearest of a path's absolute readings that denotes a member.
    private def first(paths: List[List[String]], from: Frame, seen: Set[String]): Option[Found] =
      paths.iterator.map(member(_, from, seen)).find(_ != Found.Absent)

    /** The type a qualified path denotes among the sources: a declaration, else a member an object
      * or package at its prefix exports or an object inherits; Absent where none of them has it.
      */
    def member(path: List[String], from: Frame, seen: Set[String]): Found =
      declared(path, from) match {
        case Nil =>
          val owners =
            modulePaths.getOrElse(path.init, Nil) ++ packagePaths.getOrElse(path.init, Nil)
          owners.iterator
            .flatMap(exported(path.last, _, seen))
            .nextOption()
            .orElse {
              modulePaths.getOrElse(path.init, Nil) match {
                case List(module) =>
                  objects
                    .inheritedBy(module, path.last, from)
                    .map(_.fold(Found.Blocked(_), Found.Entry(_)))
                case _ => None
              }
            }
            .getOrElse(Found.Absent)
        case found => entryOrAmbiguous(found, path.mkString("."))
      }

    private def imported(
        name: String,
        targets: List[Target],
        definition: Option[(List[TypeEntry], Int)],
        depth: Int
    ): Found = {
      val read = targets.distinct.map(reach)
      read.collectFirst { case Left(reason) => reason } match {
        case Some(reason) => Found.Blocked(reason)
        case None         =>
          val hits = read.collect { case Right(found) => found }
          hits.flatMap(_._1).distinct match {
            // The target's own head may be bound by an import (`import lib.scala` before
            // `import scala.Option`): the path it denotes is then read through that binding.
            case Nil =>
              val origin = hits
                .collectFirst { case (_, Some(bound)) => bound }
                .getOrElse(Origin.named(targets.head.segments, name))
              Found.Foreign(origin.path, origin.binding)
            case List(entry) =>
              definition match {
                // Clause and declaration in one block: the program's own name, or nothing
                // decidable here.
                case Some((found, d)) if d == depth =>
                  if (found.contains(entry)) Found.Entry(entry)
                  else Found.Blocked(s"ambiguous import binding: $name")
                case _ => Found.Entry(entry)
              }
            case _ => Found.Blocked(s"ambiguous import binding: $name")
          }
      }
    }

    /** A dotted name: its first segment is bound like a simple term name, and the rest is read
      * inside whatever that binding denotes.
      */
    def dotted(segments: List[String], frame: Frame, at: Option[(String, Int)]): Found =
      through(Target(segments, frame, at), Set.empty)(
        _.fold(Found.Absent: Found)(origin => Found.Foreign(origin.path, origin.binding))
      )

    private def binding(
        name: String,
        frame: Frame,
        at: Option[(String, Int)]
    ): Option[(Int, List[Target])] =
      at.flatMap { (file, position) =>
        frame.chain.iterator.zipWithIndex
          .map { (scope, index) =>
            inspect("import bucket")
            val targets = visible(scope, file, position).flatMap { clause =>
              clause.named.collect {
                case (bound, segments) if bound == name =>
                  Target(segments, scope, before(file, clause))
              }
            }
            (index, targets)
          }
          .find { case (_, targets) => targets.nonEmpty }
      }

    // The declarations a path's candidates name, first by nearness — one bucket or none — with the
    // import that bound the path's first segment, if one did.
    private def reach(target: Target): Either[String, (List[TypeEntry], Option[Origin])] =
      absolute(target.segments, target.scope, target.at).flatMap { (paths, origin) =>
        first(paths, target.scope, Set.empty) match {
          case Some(Found.Entry(entry))   => Right((List(entry), origin))
          case Some(failed: Found.Failed) => Left(failed.reason)
          case _                          => Right((Nil, origin))
        }
      }

    private def lexicalHit(name: String, frame: Frame): Option[(List[TypeEntry], Int)] =
      frame.chain.iterator.zipWithIndex
        .map { (scope, index) =>
          inspect("lookup bucket")
          (if (constructorView(scope)) Nil else lexical.getOrElse(scope.id -> name, Nil), index)
        }
        .find { case (entries, _) => entries.nonEmpty }

    // A simple name not declared in the file is a member of a package or template the reference's
    // scope opens, nearest first. A clause opens exactly the path it spells: `package a.b` opens
    // `a.b` and not `a` (Scala 3.9.0 reports `Not found` for a member of `a` there), while chained
    // clauses `package a; package b` are two frames and open both.
    private def candidates(name: String, frame: Frame): List[TypeEntry] =
      anchored(frame, name.takeWhile(_ != '.'), name.contains('.')).iterator
        .map(path => declared(path :+ name, frame))
        .find(_.nonEmpty)
        .getOrElse(Nil)

    private def declared(path: List[String], from: Frame): List[TypeEntry] = {
      inspect("import target")
      qualified.getOrElse(path.mkString("."), Nil).filter { entry =>
        inspect("lookup candidate")
        accessible(entry.tree, entry.owner, from)
      }
    }

    /** The absolute paths a written path may denote, nearest first, and whether an import bound its
      * first segment. A `_root_` path is itself. A relative one whose first segment is bound — by a
      * same-file object, an inherited member object or an import before the position — is read
      * through that binding; otherwise it is anchored at each path the scope opens, then at the
      * root. Left when the first segment is bound to something unread.
      */
    def absolute(
        segments: List[String],
        frame: Frame,
        at: Option[(String, Int)]
    ): Either[String, (List[List[String]], Option[Origin])] =
      segments match {
        case SourceIndex.Root :: rest => Right((List(rest), None))
        case head :: rest             =>
          at.flatMap((file, position) =>
            bound(InheritedObjects.Head(head, frame, file, position), frame.chain, None)
          ) match {
            case Some(found) =>
              found.map((paths, origin) => (paths.map(_ ++ rest), origin.map(_.selecting(rest))))
            case None => Right((anchored(frame, head, path = true).map(_ ++ segments), None))
          }
        case Nil => Right((Nil, None))
      }

    // The binding of a path's first segment, innermost scope first: an object the scope declares in
    // this file — or a value, which is refused — a member object its template inherits, then its
    // imports. Past a body with an unread parent only an enclosing class, object, method or block
    // declaration keeps its meaning (an inherited one there would be E049); a package's, an
    // import's or the default reading is refused with the parent named.
    @tailrec
    private def bound(
        head: InheritedObjects.Head,
        chain: List[Frame],
        unread: Option[String]
    ): Option[Either[String, (List[List[String]], Option[Origin])]] =
      chain match {
        case Nil            => unread.map(Left(_))
        case scope :: outer =>
          val here = head.copy(frame = scope)
          moduleIn(scope, head.name) match {
            case Some(module) =>
              Some(unread.filter(_ => isPackage(scope)).toLeft((List(module.path), None)))
            case None if value(scope, head.name) =>
              Some(Left(unread.getOrElse(s"path through a value not followed: ${head.name}")))
            case None =>
              objects.at(here) match {
                case Some(Right(path)) => Some(unread.toLeft((List(path), None)))
                case inherited         =>
                  val pending = unread.orElse(inherited.flatMap(_.left.toOption))
                  importedPrefix(here) match {
                    case Some(found) =>
                      Some(
                        pending.toLeft(()).flatMap(_ => found.map((paths, o) => (paths, Some(o))))
                      )
                    case None => bound(head, outer, pending)
                  }
              }
          }
      }

    // A parameter, value, method or given of the scope under the name: a path through it selects a
    // member of the value's type, which no object or package of the name answers.
    private def value(scope: Frame, name: String): Boolean =
      scope.params.exists(_.name.value == name) ||
        (if (isPackage(scope)) peers(scope.path) else List(scope))
          .exists(frame => ScopeTerms.values(frame.stats)(name))

    // A scope's imports binding a path's first segment before the position: a named import, read
    // at its own clause, or a wildcard whose namespace holds the segment — with the path written.
    private def importedPrefix(
        head: InheritedObjects.Head
    ): Option[Either[String, (List[List[String]], Origin)]] = {
      val InheritedObjects.Head(name, scope, file, position) = head
      val clauses = visible(scope, file, position)
      val named = clauses.flatMap { clause =>
        clause.named.collect {
          case (bound, segments) if bound == name => segments -> before(file, clause)
        }
      }
      if (named.nonEmpty)
        Some(
          MethodAnalysis
            .sequence(named.map((segments, at) => absolute(segments, scope, at).map(_._1)))
            .map(paths => paths.flatten.distinct -> Origin.named(named.head._1, name))
        )
      else {
        val opened = MethodAnalysis
          .sequence(
            clauses.flatMap(clause =>
              clause
                .opening(name)
                .map(prefix => absolute(prefix, scope, before(file, clause)).map(_._1))
            )
          )
          .map(_.flatten.filter(path => namespaces((path :+ name).mkString("."))).map(_ :+ name))
        opened match {
          case Right(Nil) => None
          case found      =>
            Some(found.map(paths => paths -> Origin(paths.head.mkString("."), "wildcard import")))
        }
      }
    }

    // The nearest scope whose wildcards bind the name: a supplied member, or a declaration of a
    // listed stdlib namespace — both in one scope are an ambiguity.
    private def wildcards(name: String, frame: Frame, at: Option[(String, Int)]): Found =
      at match {
        case None                   => Found.Absent
        case Some((file, position)) =>
          frame.chain.iterator
            .map { scope =>
              inspect("import bucket")
              val supplied = MethodAnalysis
                .sequence(visible(scope, file, position).flatMap { clause =>
                  clause
                    .opening(name)
                    .map(prefix =>
                      reach(Target(prefix :+ name, scope, before(file, clause))).map(_._1)
                    )
                })
                .map(_.flatten.distinct)
              supplied -> listed(name, scope, at)
            }
            .find((supplied, outside) => supplied != Right(Nil) || outside.nonEmpty) match {
            case Some((Right(Nil), Some(fqn)))    => Found.Foreign(fqn, "wildcard import")
            case Some((Right(List(entry)), None)) => Found.Entry(entry)
            case Some((Right(_), _))     => Found.Blocked(s"ambiguous wildcard import: $name")
            case Some((Left(reason), _)) => Found.Blocked(reason)
            case None                    => Found.Absent
          }
      }

  }

  /** The import that bound a name or a path's first segment: the path it binds and how. */
  private case class Origin(path: String, binding: String) {

    def selecting(rest: List[String]): Origin =
      if (rest.isEmpty) this else Origin((path :: rest).mkString("."), s"selected through $binding")

    def exportedBy(scope: Frame): Origin =
      copy(binding =
        s"$binding, exported by ${scope.path.lastOption.getOrElse("the empty package")}"
      )

  }

  private object Origin {

    /** A clause (`import` or `export`) whose selector spells the bound name, or renames it. */
    def named(segments: List[String], name: String, clause: String = "import"): Origin = {
      val target = segments.filterNot(_ == SourceIndex.Root)
      Origin(
        target.mkString("."),
        s"${if (target.lastOption.contains(name)) "named" else "renamed"} $clause $name"
      )
    }

  }

  /** The nearest member of an enclosing body under a name: what it denotes, the depth of the body
    * in the reference's chain, and whether an export clause defines it.
    */
  private case class Member(found: Found, depth: Int, exported: Boolean)

  /** A named import's target, with the scope and position its own prefix is read at. */
  private case class Target(segments: List[String], scope: Frame, at: Option[(String, Int)])

  // A clause's prefix is read where the clause stands: only earlier clauses have bound names.
  private def before(file: String, clause: ImportBinding): Option[(String, Int)] =
    Some(file -> (clause.start - 1))

  private def entryOrAmbiguous(found: List[TypeEntry], name: String): Found =
    found match {
      case List(entry) => Found.Entry(entry)
      case _           => Found.Blocked(s"ambiguous type: $name")
    }

  /** The import clauses of one scope visible at a reference; with no reference position, every
    * clause of the scope.
    */
  def clauses(scope: Frame, at: Option[(String, Int)]): List[ImportBinding] =
    at.fold(imports.getOrElse(scope.id, Nil))((file, position) => visible(scope, file, position))

  // The paths a scope opens, nearest first — each frame of its chain contributes its own — then the
  // root. The empty package's members are invisible from a named package, even in the same file;
  // the top-level packages are not, so a path whose first segment is one is read from the root.
  private def anchored(frame: Frame, head: String, path: Boolean): List[List[String]] = {
    val named = frame.chain.exists(scope => packageIds(scope.id) && scope.path.nonEmpty)
    (frame.chain.filterNot(constructorView).map(_.path) :+ Nil).distinct.filter(anchor =>
      anchor.nonEmpty || !named || (path && packageRoots(head))
    )
  }

  private def visible(scope: Frame, file: String, position: Int): List[ImportBinding] =
    imports
      .getOrElse(scope.id, Nil)
      .filter(binding => scope.input == file && binding.start <= position)

}
