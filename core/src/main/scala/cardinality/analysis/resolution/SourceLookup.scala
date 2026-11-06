package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.Tree

import cardinality.analysis.*
import cardinality.analysis.model.Anchor
import cardinality.types.{ListedNamespaces, Traversals, TypeApplications}

import MethodAnalysis.{Frame, TypeEntry}
import LookupBindings.*

/** Declaration buckets preserve insertion order and ambiguity; lookup never scans unrelated types.
  *
  * Import clauses bind simple names inside the block that holds them, from the clause's own
  * position onward. A reference whose scope holds such a binding resolves through it or is refused
  * there: the enclosing packages and the model's builtin vocabulary are out of reach of that name,
  * exactly as the compiler denies them when the import brings an unavailable target. Scala's
  * precedence decides the rest: a declaration of the same file or a named import before a wildcard,
  * and a wildcard before a package member of another file or a root package. A wildcard over a
  * namespace no source supplies may bind any name, so a name — or a path's first name — that would
  * otherwise come from those is not read; a stdlib namespace `ListedNamespaces` lists binds only
  * what it declares (`listed`: the namespace a wildcard prefix written in a scope opens before a
  * position, when its first name is the library's).
  */
final private[cardinality] class SourceLookup(
    types: List[TypeEntry],
    packages: List[Frame],
    modules: List[Frame],
    imports: Map[String, List[ImportBinding]],
    listed: (List[String], Frame, Option[Anchor]) => Option[ListedNamespaces.Namespace],
    exports: Map[String, List[ExportClause]] = Map.empty
) {
  private val lexical = types.groupBy(entry => entry.owner.id -> entry.name)
  private val names = types.groupMapReduce(_.owner.id)(entry => Set(entry.name))(_ ++ _)

  // The paths a wildcard or `given` import can open, and what they hold.
  private val supplied = new SuppliedNamespaces(types, packages, modules)
  private val qualified = supplied.qualified
  private val packagePaths = supplied.packagePaths

  private val packageIds = packages.map(_.id).toSet
  private val packageRoots = packages.flatMap(_.path.headOption).toSet
  private val moduleIndex = modules.flatMap(m => m.parent.map(p => (p.id, m.path.last) -> m)).toMap
  private val moduleNames = moduleIndex.keys.groupMapReduce(_._1)(key => Set(key._2))(_ ++ _)

  def peers(path: List[String]): List[Frame] = packagePaths.getOrElse(path, Nil)

  // The blocks of one package in the scope's own file — the scope alone for any other body. Scala
  // gives a definition of the reference's compilation unit, made available by a package clause, the
  // highest precedence wherever in the file its block stands (spec ch. 2; Scala 3.9.0 reads a
  // sibling `package p { type Two = Boolean }` over `import zzz.*`: probes/review-A7/r9).
  private def blocks(scope: Frame): List[Frame] =
    if (isPackage(scope)) blocksByFile.getOrElse(scope.path -> scope.input, Nil) else List(scope)

  // The values a package's blocks declare, every file's together, indexed once.
  private lazy val packageValues: Map[List[String], Set[String]] =
    packagePaths.map((path, frames) => path -> frames.flatMap(_.values).toSet)

  // A package's blocks by file, indexed once: a package can span thousands of files.
  private lazy val blocksByFile: Map[(List[String], String), List[Frame]] =
    packagePaths.toList
      .flatMap((path, frames) =>
        frames.groupBy(_.input).map((input, inFile) => (path, input) -> inFile)
      )
      .toMap

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

  private val packageInits = packages.flatMap(_.path.inits).toSet

  /** Whether a path is a package of the sources or a prefix of one. */
  def packageNamespace(path: List[String]): Boolean = path.nonEmpty && packageInits(path)

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
      at: Option[Anchor],
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

  /** Why the first name of a path written in a scope before a position is in doubt — a wildcard or
    * `given` import over a namespace no source supplies may bind it — when it is: the path may then
    * denote a namespace outside the sources, whatever it would otherwise be.
    */
  def doubtedHeads(
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit,
      objects: InheritedObjects
  ): (List[String], Frame, Option[Anchor]) => Option[String] = {
    val reader = Reader(accessible, inspect, objects)
    (prefix, scope, at) =>
      reader.absolute(prefix, scope, at).left.toOption.collect {
        case Unread.Doubted(head, why, _) => s"$head $why"
      }
  }

  /** One resolution's access rule, budget and view of inherited member objects. */
  private case class Reader(
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit,
      objects: InheritedObjects
  ) {

    private lazy val terms = new TermMembers(
      supplied,
      exports,
      objects,
      accessible,
      (segments, scope, at) => absolute(segments, scope, at).map(_._1)
    )

    def definitions(name: String, frame: Frame): List[TypeEntry] =
      lexicalHit(name, frame).map(_._1).getOrElse(candidates(name, frame))

    def simple(name: String, frame: Frame, at: Option[Anchor]): Found = {
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
        // A wildcard import outranks a package member of another file (one of the same file is a
        // definition, found above; Scala reports E049 when both bind the name).
        case None =>
          member.fold(
            wildcards(name, frame, at).fold(doubted(name, frame, _), packaged(name, frame, _))
          )(
            _.found
          )
      }
    }

    // A name a wildcard over a namespace no source supplies may bind, which nothing nearer binds:
    // another file's package member of the name is not read — the conversion search still sees
    // the declaration it would be (`Found.Blocked.hidden`) —, and a name no source declares is
    // left to the root imports' reading, which refuses it through the same wildcard
    // (`ShadowedBuiltins`, which asks `doubtedHeads` about a wildcard whose prefix is in doubt).
    private def doubted(name: String, frame: Frame, doubt: Opening.Doubt): Found =
      packaged(name, frame, Found.Absent) match {
        case Found.Absent => Found.Absent
        case found        => hiding(name, doubt, Some(found))
      }

    private def hiding(name: String, doubt: Opening.Doubt, found: Option[Found]): Found =
      Found.Blocked.hiding(s"unresolved type: $name (${doubt.why})", found)

    // What another file's package member makes of the name, where no wildcard binds it otherwise.
    private def packaged(name: String, frame: Frame, wildcard: Found): Found =
      if (wildcard != Found.Absent) wildcard
      else
        candidates(name, frame) match {
          case Nil     => Found.Absent
          case List(e) => Found.Entry(e)
          case _       => Found.Blocked(s"ambiguous type: $name")
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
        case Left(doubted @ Unread.Doubted(_, _, fallback)) =>
          first(fallback, target.scope, seen).fold(missing(None)) { found =>
            Found.Blocked.hiding(doubted.reason(target.segments.mkString(".")), Some(found))
          }
        case Left(unread)           => Found.Blocked(unread.reason(target.segments.mkString(".")))
        case Right((paths, origin)) => first(paths, target.scope, seen).getOrElse(missing(origin))
      }

    // A wildcard export's member under the name: the first supplied namespace its prefix denotes
    // answers, a declaration or nothing; a prefix no supplied namespace answers may define anything.
    private def opened(prefix: Target, name: String, seen: Set[String]): Option[Found] =
      absolute(prefix.segments, prefix.scope, prefix.at) match {
        case Left(unread)      => Some(Found.Blocked(unread.reason(prefix.segments.mkString("."))))
        case Right((paths, _)) =>
          paths.find(supplied.closed) match {
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
      * or package at its prefix exports or an object or package object inherits; Absent where none
      * of them has it. An enum's class case is a type this reading does not hold (`import E.*`
      * binds the case class `E.A` over another file's `A`: Scala 3.9.0, probes/review-A7/r16).
      */
    def member(path: List[String], from: Frame, seen: Set[String]): Found =
      declared(path, from) match {
        case Nil =>
          supplied
            .bodies(path.init)
            .iterator
            .flatMap(exported(path.last, _, seen))
            .nextOption()
            .orElse(
              supplied
                .template(path.init)
                .flatMap(objects.inheritedBy(_, path.last, from))
                .map(_.fold(Found.Blocked(_), Found.Entry(_)))
            )
            .orElse(
              Option.when(supplied.enumCase(path))(
                Found.Blocked(s"enum case type not read: ${path.mkString(".")}")
              )
            )
            .getOrElse(Found.Absent)
        case found => entryOrAmbiguous(found, path.mkString("."))
      }

    private def imported(
        name: String,
        targets: List[Either[String, Target]],
        definition: Option[(List[TypeEntry], Int)],
        depth: Int
    ): Found = {
      val read = targets.distinct.map(_.flatMap(reach(_, name)))
      Traversals.sequence(read) match {
        case Left(reason) => Found.Blocked(reason)
        case Right(hits)  =>
          hits.flatMap(_._1).distinct match {
            // The target's own head may be bound by an import (`import lib.scala` before
            // `import scala.Option`): the path it denotes is then read through that binding.
            case Nil =>
              val origin = hits
                .collectFirst { case (_, Some(bound)) => bound }
                .getOrElse(Origin.named(written(targets), name))
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
    def dotted(segments: List[String], frame: Frame, at: Option[Anchor]): Found =
      through(Target(segments, frame, at), Set.empty)(
        _.fold(Found.Absent: Found)(origin => Found.Foreign(origin.path, origin.binding))
      )

    private def binding(
        name: String,
        frame: Frame,
        at: Option[Anchor]
    ): Option[(Int, List[Either[String, Target]])] =
      at.flatMap {
        case Anchor(file, position) =>
          frame.chain.iterator.zipWithIndex
            .map { (scope, index) =>
              inspect("import bucket")
              (index, visible(scope, file, position).flatMap(selected(_, name, scope, file)))
            }
            .find { case (_, targets) => targets.nonEmpty }
      }

    // The declarations a named import's target names, first by nearness — one bucket or none —
    // with the import that bound the path's first segment, if one did.
    private def reach(
        target: Target,
        name: String
    ): Either[String, (List[TypeEntry], Option[Origin])] =
      absolute(target.segments, target.scope, target.at) match {
        // A wildcard that may bind the import's first name hides only a member of the sources:
        // elsewhere the import names a declaration outside them, read as such.
        case Left(Unread.Doubted(_, _, fallback))
            if first(fallback, target.scope, Set.empty).isEmpty =>
          Right((Nil, None))
        case Left(unread) =>
          Left(unread.reason(name, s"imported from ${target.segments.mkString(".")}, "))
        case Right((paths, origin)) =>
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
          val declared =
            if (constructorView(scope)) Nil
            else blocks(scope).flatMap(block => lexical.getOrElse(block.id -> name, Nil))
          (declared, index)
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
      * root. Left when the first segment is bound to something unread, or a wildcard over a
      * namespace no source supplies may bind it.
      */
    def absolute(
        segments: List[String],
        frame: Frame,
        at: Option[Anchor]
    ): Either[Unread, Reading] =
      segments match {
        case SourceIndex.Root :: rest => Right((List(rest), None))
        case head :: rest             =>
          at.flatMap(anchor =>
            bound(
              InheritedObjects.Head(head, frame, anchor.source, anchor.offset),
              frame.chain,
              Pending.none
            )
          ) match {
            case Some(found) =>
              found.left
                .map(_.selecting(rest))
                .map((paths, origin) => (paths.map(_ ++ rest), origin.map(_.selecting(rest))))
            case None =>
              Right((nearest(anchored(frame, head, path = true), head).map(_ ++ rest), None))
          }
        case Nil => Right((Nil, None))
      }

    // The binding of a path's first segment, innermost scope first (`declaredHead`). Past a body
    // with an unread parent only an enclosing class, object, method or block declaration keeps its
    // meaning (an inherited one there would be E049); a package's, an import's or the default
    // reading is refused with the parent named. A wildcard over a namespace no source supplies
    // (`pending.doubt`) outranks only an outer scope's wildcards, the packages the scope opens and
    // the root: an enclosing declaration or a named import keeps the name, since Scala rejects both
    // binding it (E049), and one of its own scope's supplied wildcards does too.
    @tailrec
    private def bound(
        head: InheritedObjects.Head,
        chain: List[Frame],
        pending: Pending
    ): Option[Either[Unread, Reading]] =
      chain match {
        case Nil =>
          pending.unread
            .map(Unread.Refused(_))
            .orElse(
              pending.doubt
                .map(d => Unread.Doubted(head.name, d.passed, d.fallback ++ defaultPaths(head)))
            )
            .map(Left(_))
        case scope :: outer =>
          declaredHead(head.copy(frame = scope), pending) match {
            case Right(found) => found
            case Left(next)   => bound(head, outer, next)
          }
      }

    // What one scope binds a path's first name to: an object it declares in this file, a value —
    // refused —, a member object its template inherits, then its imports; Left with what the outer
    // scopes read the name under where none of them binds it.
    private def declaredHead(
        here: InheritedObjects.Head,
        pending: Pending
    ): Pending.Step = {
      val scope = here.frame
      def refused = pending.unread.map(Unread.Refused(_))
      blocks(scope).iterator.flatMap(moduleIn(_, here.name)).nextOption() match {
        case Some(module) =>
          Right(Some(refused.filter(_ => isPackage(scope)).toLeft((List(module.path), None))))
        case None if value(scope, here.name) =>
          Right(Some(Left(refused.getOrElse(TermMembers.value(here.name)))))
        case None =>
          objects.at(here) match {
            case Some(Right(path)) => Right(Some(refused.toLeft((List(path), None))))
            case inherited         =>
              importedHead(here, pending.parent(inherited.flatMap(_.left.toOption)))
          }
      }
    }

    // What a scope's imports bind a path's first name to. An inner wildcard that may bind the name
    // shadows this scope's: the namespace this one opens is what the name would otherwise denote.
    private def importedHead(
        here: InheritedObjects.Head,
        pending: Pending
    ): Pending.Step = {
      def bind(found: Either[Unread, (List[List[String]], Origin)]) =
        Right(
          Some(
            pending.unread
              .map(Unread.Refused(_))
              .toLeft(())
              .flatMap(_ => found.map((paths, o) => (paths, Some(o))))
          )
        )
      (importedPrefix(here), pending.doubt) match {
        case (Some(Prefix.Named(found)), _) => bind(found)
        case (Some(Prefix.Opened(Right((paths, _)))), Some(inner)) if pending.unread.isEmpty =>
          Right(Some(Left(Unread.Doubted(here.name, inner.passed, paths))))
        case (Some(Prefix.Opened(found)), _) => bind(found)
        case (Some(d: Prefix.Doubt), _)      => Left(pending.doubting(d))
        case (None, _)                       => Left(pending)
      }
    }

    // The paths a first segment denotes when nothing around the reference binds it.
    private def defaultPaths(head: InheritedObjects.Head): List[List[String]] =
      nearest(anchored(head.frame, head.name, path = true), head.name)

    // A first name nothing around the reference binds is the nearest member of that name of the
    // packages the scope opens, then the root: the one namespace the sources have there (`c` in
    // package `a.b` is `a.b.c` when the sources declare it, whatever the root `c` holds — Scala
    // 3.9.0, probes/review-A7/round2/anchors); with none, each anchor in turn.
    private def nearest(anchors: List[List[String]], head: String): List[List[String]] =
      anchors
        .map(_ :+ head)
        .find(supplied.denotes)
        .fold(anchors.map(_ :+ head))(List(_))

    // A parameter, value, method or given of the scope under the name: a path through it selects a
    // member of the value's type, which no object or package of the name answers.
    private def value(scope: Frame, name: String): Boolean =
      scope.params.exists(_.name.value == name) ||
        (if (isPackage(scope)) packageValues.getOrElse(scope.path, Set.empty)(name)
         else scope.values(name))

    // A scope's imports binding a path's first segment before the position: a named import, read
    // at its own clause; a wildcard whose namespace holds the segment — with the path written — or
    // a member of a listed stdlib namespace; else a wildcard or `given` import that may bind it.
    private def importedPrefix(head: InheritedObjects.Head): Option[Prefix] = {
      val named = visible(head.frame, head.file, head.position).flatMap(
        selected(_, head.name, head.frame, head.file)
      )
      if (named.isEmpty) openedPrefix(head)
      else
        Some(
          Prefix.Named(
            Traversals
              .sequence(named.map(_.left.map(Unread.Refused(_)).flatMap(read)))
              .map(paths => paths.flatten.distinct -> Origin.named(written(named), head.name))
          )
        )
    }

    private def read(target: Target): Either[Unread, List[List[String]]] =
      absolute(target.segments, target.scope, target.at).map(_._1)

    private def openedPrefix(head: InheritedObjects.Head): Option[Prefix] = {
      val openings = this.openings(head, Space.Terms)
      val opened = Traversals
        .sequence(openings.collect {
          case Opening.Refused(reason) => Left(Unread.Refused(reason))
          case Opening.Supplied(paths) => terms.wildcard(paths, head.name, head.frame)
          case Opening.Entered(paths)  => Right(paths)
          case Opening.Listed(fqn)     => Right(List(fqn.split('.').toList))
        })
        .map(_.flatten)
      opened match {
        case Right(Nil) =>
          Opening
            .doubt(openings)
            .map(doubt =>
              Prefix.Doubt(
                doubt.passed,
                Opening.fallbacks(openings, head.name).filter(supplied.denotes)
              )
            )
        case found =>
          Some(
            Prefix.Opened(
              found.map(paths => paths -> Origin(paths.head.mkString("."), "wildcard import"))
            )
          )
      }
    }

    // The nearest scope whose wildcards bind the name: a supplied member, or a declaration of a
    // listed stdlib namespace — both in one scope are an ambiguity. A wildcard that may bind it
    // (Left) shadows every outer scope's; in its own scope a supplied member keeps the name, since
    // Scala rejects two wildcards of one scope that both bind it (E049).
    private def wildcards(
        name: String,
        frame: Frame,
        at: Option[Anchor]
    ): Either[Opening.Doubt, Found] =
      at.fold(Right(Found.Absent))(anchor =>
        scan(InheritedObjects.Head(name, frame, anchor.source, anchor.offset), frame.chain, None)
      )

    @tailrec
    private def scan(
        head: InheritedObjects.Head,
        chain: List[Frame],
        doubt: Option[Opening.Doubt]
    ): Either[Opening.Doubt, Found] =
      chain match {
        case Nil            => doubt.toLeft(Found.Absent)
        case scope :: outer =>
          inspect("import bucket")
          val openings = this.openings(head.copy(frame = scope), Space.Types)
          wildcardMember(head.name, scope, openings) match {
            case Some(found) => Right(doubt.fold(found)(hiding(head.name, _, Some(found))))
            case None        =>
              // A wildcard whose prefix is in doubt hides the member it would otherwise open.
              val held = first(Opening.fallbacks(openings, head.name), scope, Set.empty)
              (doubt.orElse(Opening.doubt(openings)), held) match {
                case (Some(passed), Some(_)) => Right(hiding(head.name, passed, held))
                case (passed, _)             => scan(head, outer, passed)
              }
          }
      }

    // What one scope's wildcards bind a type name to, if one binds it.
    private def wildcardMember(
        name: String,
        scope: Frame,
        openings: List[Opening]
    ): Option[Found] = {
      val read = Traversals.sequence(openings.collect {
        case Opening.Refused(reason) => Left(reason)
        case Opening.Supplied(paths) =>
          first(paths.map(_ :+ name), scope, Set.empty) match {
            case Some(Found.Entry(entry))   => Right(List(entry))
            case Some(failed: Found.Failed) => Left(failed.reason)
            case _                          => Right(Nil)
          }
      })
      val listedMember = openings.collectFirst { case Opening.Listed(fqn) => fqn }
      (read.map(_.flatten.distinct), listedMember) match {
        case (Right(Nil), None)         => None
        case (Right(Nil), Some(fqn))    => Some(Found.Foreign(fqn, "wildcard import"))
        case (Right(List(entry)), None) => Some(Found.Entry(entry))
        case (Right(_), _)              => Some(Found.Blocked(s"ambiguous wildcard import: $name"))
        case (Left(reason), _)          => Some(Found.Blocked(reason))
      }
    }

    // What each import clause of a scope written before a position binds a name to, in `space`:
    // its wildcard, its `given` selectors (terms alone), and — for an importer whose prefix is no
    // path — the members of a value, which may bind any name and are not followed.
    private def openings(at: InheritedObjects.Head, space: Space): List[Opening] =
      visible(at.frame, at.file, at.position).flatMap { clause =>
        val from = before(at.file, clause)
        clause
          .opening(at.name)
          .map(prefix => opening(Target(prefix, at.frame, from), at.name, space)) ++
          space
            .givens(clause)
            .map(prefix => givenOpening(Target(prefix, at.frame, from), at.name)) ++
          clause.unfollowed
            .flatMap(space.unfollowed(_, at.name, clause.hidden))
            .map(Opening.Doubt(_, None, Nil))
      }

    // What one wildcard clause binds a name to, in Scala's type or term namespace: the namespaces
    // its prefix denotes, when one is closed; a subpackage of a package the sources spell only as a
    // prefix; a member of a listed stdlib namespace; a refusal where the prefix rests on something
    // unread; or a doubt — a namespace no source supplies or one open to dependencies, one the
    // model lists without its members, or one whose own first name such a wildcard may bind.
    private def opening(prefix: Target, name: String, space: Space): Opening = {
      val written = s"${prefix.segments.mkString(".")}.*"
      over(prefix, written) {
        case paths if paths.exists(supplied.closed)  => Opening.Supplied(paths)
        case paths if paths.exists(supplied.denotes) =>
          if (paths.exists(path => supplied.denotes(path :+ name))) Opening.Supplied(paths)
          else Opening.Doubt(written, None, Nil)
        case _ =>
          listed(prefix.segments, prefix.scope, prefix.at)
            .fold(Some(ListedNamespaces.Binding.Unlisted))(space.binding(_, name)) match {
            case Some(ListedNamespaces.Binding.Member(fqn)) => Opening.Listed(fqn)
            case Some(ListedNamespaces.Binding.Unlisted)    => Opening.Doubt(written, None, Nil)
            case None                                       => Opening.Unbound
          }
      }
    }

    // What a `given` selector binds a term name to: a given or implicit member of a closed
    // namespace (`TermMembers`), nothing else there; any name a given of a namespace no source
    // supplies may have (Scala 3.9.0 binds `compose` through `import zzz.given`:
    // probes/review-A7/r11). It opens no namespace a doubt falls back to.
    private def givenOpening(prefix: Target, name: String): Opening = {
      val written = s"${prefix.segments.mkString(".")}.given"
      over(prefix, written) {
        case paths if paths.exists(supplied.closed) =>
          terms
            .givens(paths, name, prefix.scope)
            .fold(Opening.Unbound)(
              _.fold(unread => Opening.Refused(unread.reason(name)), Opening.Entered(_))
            )
        case _ => Opening.Doubt(written, None, Nil)
      } match {
        case doubt: Opening.Doubt => doubt.copy(fallback = Nil)
        case other                => other
      }
    }

    // A clause's prefix read where the clause stands: refused where it rests on something unread,
    // in doubt where its first name is — with the supplied namespaces it would otherwise denote —,
    // and otherwise what `paths` makes of the absolute paths it denotes.
    private def over(prefix: Target, written: String)(
        paths: List[List[String]] => Opening
    ): Opening =
      absolute(prefix.segments, prefix.scope, prefix.at) match {
        case Left(Unread.Doubted(head, why, fallback)) =>
          Opening.Doubt(written, Some(Through(head, why)), fallback.filter(supplied.denotes))
        case Left(Unread.Refused(reason)) => Opening.Refused(reason)
        case Right((denoted, _))          => paths(denoted)
      }

  }

  /** The import clauses of one scope visible at a reference; with no reference position, every
    * clause of the scope.
    */
  def clauses(scope: Frame, at: Option[Anchor]): List[ImportBinding] =
    at.fold(imports.getOrElse(scope.id, Nil))(anchor =>
      visible(scope, anchor.source, anchor.offset)
    )

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
