package cardinality.analysis.resolution

import scala.meta.Tree

import cardinality.analysis.*

import MethodAnalysis.{Frame, TypeEntry}

/** Declaration buckets preserve insertion order and ambiguity; lookup never scans unrelated types.
  */
final private[cardinality] class SourceLookup(
    types: List[TypeEntry],
    packages: List[Frame]
) {
  private val lexical = types.groupBy(entry => entry.owner.id -> entry.name)

  private val qualified = types
    .filterNot(_.owner.chain.exists(_.local))
    .groupBy(entry => (entry.owner.path :+ entry.name).mkString("."))

  private val packagePaths = packages.groupBy(_.path)
  private val packageIds = packages.map(_.id).toSet

  def peers(path: List[String]): List[Frame] = packagePaths.getOrElse(path, Nil)
  def isPackage(frame: Frame): Boolean = packageIds(frame.id)

  def lookup(
      name: String,
      frame: Frame,
      accessible: (Tree, Frame, Frame) => Boolean,
      inspect: String => Unit
  ): List[TypeEntry] = {
    val local = frame.chain.iterator
      .map { scope =>
        inspect("lookup bucket")
        lexical.getOrElse(scope.id -> name, Nil)
      }
      .find(_.nonEmpty)
    local.getOrElse {
      // A dotted clause like `package example.sub` sits inside every enclosing package, whose
      // members are in scope there. Each chain frame contributes its full path and every
      // proper prefix of it, nearest first — the candidates nested package clauses would try.
      val candidates =
        frame.chain.flatMap(f => f.path.inits.toList.reverse.map(p => (p :+ name).mkString(".")))
      candidates.iterator
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
  }

}
