package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.Tree

import cardinality.analysis.*

import MethodAnalysis.{Frame, TypeEntry}

/** What a simple-name resolution reached: one declaration, a refusal to read past the name with the
  * reason, a declaration outside the source set, or nothing — which is where the model's own
  * vocabulary may still answer.
  */
private[cardinality] enum Found {
  case Entry(entry: TypeEntry)
  case Blocked(reason: String)
  // An import binds the name to a path outside the source set: a declaration that exists, but was
  // not supplied.
  case Foreign(reason: String)
  case Absent
}

/** The member objects a template body inherits, which the first segment of a path may name: at a
  * position inside `frame`'s body, the inherited object's path, or the reason a parent outside the
  * source set leaves the name unread; None where the frame inherits nothing under the name.
  */
private[cardinality] trait InheritedObjects {
  def at(head: InheritedObjects.Head): InheritedObjects.Path
}

private[cardinality] object InheritedObjects {

  /** A path's first segment, read inside `frame` at a position of `file`. */
  final case class Head(name: String, frame: Frame, file: String, position: Int)

  type Path = Option[Either[String, List[String]]]

  val none: InheritedObjects = _ => None
}

/** Declaration buckets preserve insertion order and ambiguity; lookup never scans unrelated types.
  *
  * Import clauses bind simple names inside the block that holds them, from the clause's own
  * position onward. A reference whose scope holds such a binding resolves through it or is refused
  * there: the enclosing packages and the model's builtin vocabulary are out of reach of that name,
  * exactly as the compiler denies them when the import brings an unavailable target. Scala's
  * precedence decides the rest: a declaration of the same file or a named import before a wildcard,
  * and a wildcard before a package member of another file.
  */
final private[cardinality] class SourceLookup(
    types: List[TypeEntry],
    packages: List[Frame],
    modules: List[Frame],
    imports: Map[String, List[ImportBinding]]
) {
  private val lexical = types.groupBy(entry => entry.owner.id -> entry.name)
  private val names = types.groupMapReduce(_.owner.id)(entry => Set(entry.name))(_ ++ _)

  private val qualified = types
    .filterNot(_.owner.chain.exists(_.local))
    .groupBy(entry => (entry.owner.path :+ entry.name).mkString("."))

  private val packagePaths = packages.groupBy(_.path)
  private val packageIds = packages.map(_.id).toSet
  private val packageRoots = packages.flatMap(_.path.headOption).toSet
  private val moduleIndex = modules.flatMap(m => m.parent.map(p => (p.id, m.path.last) -> m)).toMap
  private val moduleNames = moduleIndex.keys.groupMapReduce(_._1)(key => Set(key._2))(_ ++ _)

  // The paths a wildcard can open: packages (with every prefix), objects and type declarations.
  private val namespaces: Set[String] =
    packages.flatMap(_.path.inits).map(_.mkString(".")).toSet ++
      modules.map(_.path.mkString(".")) ++ qualified.keySet

  def peers(path: List[String]): List[Frame] = packagePaths.getOrElse(path, Nil)

  /** The type declarations one frame's own body makes under a name, and every name it declares. */
  def declaredIn(frame: Frame, name: String): List[TypeEntry] =
    lexical.getOrElse(frame.id -> name, Nil)

  def declaredNames(frame: Frame): Set[String] = names.getOrElse(frame.id, Set.empty)

  /** The object one frame's own body declares under a name, and every object it declares. */
  def moduleIn(frame: Frame, name: String): Option[Frame] = moduleIndex.get(frame.id -> name)

  def declaredModules(frame: Frame): Set[String] = moduleNames.getOrElse(frame.id, Set.empty)

  def isPackage(frame: Frame): Boolean = packageIds(frame.id)

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
      binding(name, frame, at) match {
        case Some((depth, targets)) =>
          definition match {
            // A declaration nearer than every binding for the name keeps it, import or not.
            case Some((found, d)) if d < depth => entryOrAmbiguous(found, name)
            case _                             => imported(name, targets, definition, depth)
          }
        case None =>
          definition match {
            case Some((found, _)) => entryOrAmbiguous(found, name)
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

    private def imported(
        name: String,
        targets: List[Target],
        definition: Option[(List[TypeEntry], Int)],
        depth: Int
    ): Found = {
      val hits = targets.distinct.map(target => hit(target.segments, target.scope, target.at))
      hits.collectFirst { case Left(reason) => reason } match {
        case Some(reason) => Found.Blocked(reason)
        case None         =>
          hits.collect { case Right(found) => found }.flatten.distinct match {
            case Nil =>
              val origins = targets.map(_.segments.filterNot(_ == SourceIndex.Root).mkString("."))
              Found.Foreign(
                s"unresolved type: $name (imported from ${origins.distinct.mkString(", ")})"
              )
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
    def dotted(segments: List[String], frame: Frame, at: Option[(String, Int)]): Found = {
      val name = segments.mkString(".")
      absolute(segments, frame, at) match {
        case Left(reason)           => Found.Blocked(reason)
        case Right((paths, origin)) =>
          (paths.iterator.map(declared(_, frame)).find(_.nonEmpty), origin) match {
            case (Some(List(entry)), _) => Found.Entry(entry)
            case (Some(_), _)           => Found.Blocked(s"ambiguous type: $name")
            case (None, Some(path))     =>
              Found.Foreign(s"unresolved type: $name (imported from $path)")
            case (None, None) => Found.Absent
          }
      }
    }

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

    // The declarations a path's candidates name, first by nearness: one bucket or none.
    private def hit(
        segments: List[String],
        scope: Frame,
        at: Option[(String, Int)]
    ): Either[String, List[TypeEntry]] =
      absolute(segments, scope, at).map((paths, _) =>
        paths.iterator.map(declared(_, scope)).find(_.nonEmpty).getOrElse(Nil)
      )

    private def lexicalHit(name: String, frame: Frame): Option[(List[TypeEntry], Int)] =
      frame.chain.iterator.zipWithIndex
        .map { (scope, index) =>
          inspect("lookup bucket")
          (lexical.getOrElse(scope.id -> name, Nil), index)
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
    ): Either[String, (List[List[String]], Option[String])] =
      segments match {
        case SourceIndex.Root :: rest => Right((List(rest), None))
        case head :: rest             =>
          at.flatMap((file, position) =>
            bound(InheritedObjects.Head(head, frame, file, position), frame.chain, None)
          ) match {
            case Some(found) =>
              found.map((paths, origin) =>
                (paths.map(_ ++ rest), origin.map(path => (path :: rest).mkString(".")))
              )
            case None => Right((anchored(frame, head, path = true).map(_ ++ segments), None))
          }
        case Nil => Right((Nil, None))
      }

    // The binding of a path's first segment, innermost scope first: an object the scope declares in
    // this file, a member object its template inherits, then its imports. Past a body with an
    // unread parent only an enclosing class, object, method or block declaration keeps its meaning
    // (an inherited one there would be E049); a package's, an import's or the default reading is
    // refused with the parent named.
    @tailrec
    private def bound(
        head: InheritedObjects.Head,
        chain: List[Frame],
        unread: Option[String]
    ): Option[Either[String, (List[List[String]], Option[String])]] =
      chain match {
        case Nil            => unread.map(Left(_))
        case scope :: outer =>
          val here = head.copy(frame = scope)
          moduleIn(scope, head.name) match {
            case Some(module) =>
              Some(unread.filter(_ => isPackage(scope)).toLeft((List(module.path), None)))
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

    // A scope's imports binding a path's first segment before the position: a named import, read
    // at its own clause, or a wildcard whose namespace holds the segment — with the path written.
    private def importedPrefix(
        head: InheritedObjects.Head
    ): Option[Either[String, (List[List[String]], String)]] = {
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
            .map(paths =>
              paths.flatten.distinct -> named.head._1.filterNot(_ == SourceIndex.Root).mkString(".")
            )
        )
      else {
        val opened = MethodAnalysis
          .sequence(
            clauses.flatMap(clause =>
              clause.wildcards
                .map(prefix => absolute(prefix, scope, before(file, clause)).map(_._1))
            )
          )
          .map(_.flatten.filter(path => namespaces((path :+ name).mkString("."))).map(_ :+ name))
        opened match {
          case Right(Nil) => None
          case found      => Some(found.map(paths => paths -> paths.head.mkString(".")))
        }
      }
    }

    private def wildcards(name: String, frame: Frame, at: Option[(String, Int)]): Found =
      at match {
        case None                   => Found.Absent
        case Some((file, position)) =>
          frame.chain.iterator
            .map { scope =>
              inspect("import bucket")
              MethodAnalysis
                .sequence(visible(scope, file, position).flatMap { clause =>
                  clause.wildcards.map(prefix => hit(prefix :+ name, scope, before(file, clause)))
                })
                .map(_.flatten.distinct)
            }
            .find(_ != Right(Nil)) match {
            case Some(Right(List(entry))) => Found.Entry(entry)
            case Some(Right(_))           => Found.Blocked(s"ambiguous wildcard import: $name")
            case Some(Left(reason))       => Found.Blocked(reason)
            case None                     => Found.Absent
          }
      }

  }

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

  /** The prefixes the wildcard clauses of one scope open at a reference; with no reference
    * position, every clause of the scope.
    */
  def wildcardPrefixes(scope: Frame, at: Option[(String, Int)]): List[List[String]] =
    at.fold(imports.getOrElse(scope.id, Nil))((file, position) => visible(scope, file, position))
      .flatMap(_.wildcards)

  // The paths a scope opens, nearest first — each frame of its chain contributes its own — then the
  // root. The empty package's members are invisible from a named package, even in the same file;
  // the top-level packages are not, so a path whose first segment is one is read from the root.
  private def anchored(frame: Frame, head: String, path: Boolean): List[List[String]] = {
    val named = frame.chain.exists(scope => packageIds(scope.id) && scope.path.nonEmpty)
    (frame.chain.map(_.path) :+ Nil).distinct.filter(anchor =>
      anchor.nonEmpty || !named || (path && packageRoots(head))
    )
  }

  private def visible(scope: Frame, file: String, position: Int): List[ImportBinding] =
    imports
      .getOrElse(scope.id, Nil)
      .filter(binding => scope.input == file && binding.start <= position)

}
