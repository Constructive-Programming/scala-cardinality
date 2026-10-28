package cardinality.analysis.resolution

import scala.meta.Tree

import cardinality.analysis.*

import MethodAnalysis.{Frame, TypeEntry}

/** What a simple-name resolution reached: one declaration, a refusal to read past the name with the
  * reason, or nothing — which is where the model's own vocabulary may still answer.
  */
private[cardinality] enum Found {
  case Entry(entry: TypeEntry)
  case Blocked(reason: String)
  case Absent
}

/** Declaration buckets preserve insertion order and ambiguity; lookup never scans unrelated types.
  *
  * Import clauses bind simple names inside the block that holds them, from the clause's own
  * position onward. A reference whose scope holds such a binding resolves through it or is refused
  * there: the enclosing packages and the model's builtin vocabulary are out of reach of that name,
  * exactly as the compiler denies them when the import brings an unavailable target. A nearer
  * declaration keeps the name from a farther clause; a wildcard binds last — a declaration, a named
  * import or an enclosing package first.
  */
final private[cardinality] class SourceLookup(
    types: List[TypeEntry],
    packages: List[Frame],
    imports: Map[String, List[ImportBinding]]
) {
  private val lexical = types.groupBy(entry => entry.owner.id -> entry.name)

  private val qualified = types
    .filterNot(_.owner.chain.exists(_.local))
    .groupBy(entry => (entry.owner.path :+ entry.name).mkString("."))

  private val packagePaths = packages.groupBy(_.path)
  private val packageIds = packages.map(_.id).toSet

  def peers(path: List[String]): List[Frame] = packagePaths.getOrElse(path, Nil)
  def isPackage(frame: Frame): Boolean = packageIds(frame.id)

  // The definition-only contract, for callers that resolve outside any reference position:
  // a list whose size carries ambiguity, exactly as before imports were modeled.
  def lookup(
      name: String,
      frame: Frame,
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): List[TypeEntry] =
    lexicalHit(name, frame, inspect).map(_._1).getOrElse {
      candidates(name, frame, accessible, inspect)
    }

  def resolveName(
      name: String,
      frame: Frame,
      at: Option[(String, Int)],
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): Found = {
    val definition = lexicalHit(name, frame, inspect)
    binding(name, frame, at, inspect) match {
      case Some((depth, targets)) =>
        definition match {
          // A declaration nearer than every binding for the name keeps it, import or not.
          case Some((found, d)) if d < depth => entryOrAmbiguous(found, name)
          case _                             =>
            val hits = targets.distinct
              .map(segments => importHit(segments, frame, accessible, inspect))
              .filter(_.nonEmpty)
              .flatten
              .distinct
            hits match {
              case Nil =>
                Found.Blocked(
                  s"unresolved type: $name (imported from " +
                    s"${targets.distinct.map(_.mkString(".")).mkString(", ")})"
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
      case None =>
        definition match {
          case Some((found, _)) => entryOrAmbiguous(found, name)
          case None             =>
            candidates(name, frame, accessible, inspect) match {
              case Nil     => wildcards(name, frame, at, accessible, inspect)
              case List(e) => Found.Entry(e)
              case _       => Found.Blocked(s"ambiguous type: $name")
            }
        }
    }
  }

  private def entryOrAmbiguous(found: List[TypeEntry], name: String): Found =
    found match {
      case List(entry) => Found.Entry(entry)
      case _           => Found.Blocked(s"ambiguous type: $name")
    }

  private def lexicalHit(
      name: String,
      frame: Frame,
      inspect: String => Unit
  ): Option[(List[TypeEntry], Int)] =
    frame.chain.iterator.zipWithIndex
      .map { (scope, index) =>
        inspect("lookup bucket")
        (lexical.getOrElse(scope.id -> name, Nil), index)
      }
      .find { case (entries, _) => entries.nonEmpty }

  // A dotted clause like `package example.sub` sits inside every enclosing package, whose
  // members are in scope there. Each chain frame contributes its full path and every
  // proper prefix of it, nearest first — the candidates nested package clauses would try.
  private def candidates(
      name: String,
      frame: Frame,
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): List[TypeEntry] = {
    val paths =
      frame.chain.flatMap(f => f.path.inits.toList.reverse.map(p => (p :+ name).mkString(".")))
    paths.iterator
      .map { full =>
        inspect("lookup bucket")
        qualified.getOrElse(full, Nil).filter { entry =>
          inspect("lookup candidate")
          accessible(entry.tree, entry.owner, frame)
        }
      }
      .find(_.nonEmpty)
      .getOrElse(Nil)
  }

  private def binding(
      name: String,
      frame: Frame,
      at: Option[(String, Int)],
      inspect: String => Unit
  ): Option[(Int, List[List[String]])] =
    at.flatMap { (file, position) =>
      frame.chain.iterator.zipWithIndex
        .map { (scope, index) =>
          inspect("import bucket")
          val targets =
            visible(scope, file, position).flatMap(_.named).collect {
              case (bound, segments) if bound == name => segments
            }
          (index, targets)
        }
        .find { case (_, targets) => targets.nonEmpty }
    }

  // An import prefix is a path the compiler resolves from the enclosing scopes outward, so the
  // target is anchored at every package prefix of the use-site chain, nearest first — the same
  // walk a package clause's own names get.
  private def importHit(
      segments: List[String],
      frame: Frame,
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): List[TypeEntry] =
    frame.chain
      .flatMap(scope => scope.path.inits.toList.reverse.map(p => (p ++ segments).mkString(".")))
      .iterator
      .map { key =>
        inspect("import target")
        qualified.getOrElse(key, Nil)
      }
      .find(_.nonEmpty)
      .getOrElse(Nil)
      .filter { entry =>
        inspect("lookup candidate")
        accessible(entry.tree, entry.owner, frame)
      }

  private def wildcards(
      name: String,
      frame: Frame,
      at: Option[(String, Int)],
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): Found =
    at match {
      case None                   => Found.Absent
      case Some((file, position)) =>
        frame.chain.iterator
          .map { scope =>
            inspect("import bucket")
            visible(scope, file, position)
              .flatMap(_.wildcards)
              .flatMap { prefix => importHit(prefix :+ name, scope, accessible, inspect) }
              .distinct
          }
          .find(_.nonEmpty) match {
          case Some(List(entry)) => Found.Entry(entry)
          case Some(_)           => Found.Blocked(s"ambiguous wildcard import: $name")
          case None              => Found.Absent
        }
    }

  private def visible(scope: Frame, file: String, position: Int): List[ImportBinding] =
    imports
      .getOrElse(scope.id, Nil)
      .filter(binding => scope.input == file && binding.start <= position)

}
