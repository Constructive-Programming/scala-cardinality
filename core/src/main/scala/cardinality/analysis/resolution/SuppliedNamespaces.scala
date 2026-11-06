package cardinality.analysis.resolution

import scala.meta.{Defn, Pkg, XtensionClassifiable}

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.model.ScopeTerms
import cardinality.types.Trees
import cardinality.types.Trees.hasCase

/** What the sources supply at an absolute path that a wildcard or `given` import opens, or whose
  * last segment such an import may bind. A path is a term namespace: a package, an object or the
  * companion of a case class or an enum — never a type, which no clause opens and no path's first
  * name denotes.
  *
  * A package some supplied file declares a member in is read as complete: no dependency adds to it
  * (the closed-package assumption, docs/research/stdlib-model.md). A package the sources spell only
  * as the prefix of a deeper one (`zzz` for `package zzz.inner`) holds its subpackages and whatever
  * a dependency declares there — Scala 3.9.0 binds a `zzz.Two` of another compilation unit through
  * `import zzz.*` (probes/review-A7/r14) — so it is open, like a namespace no source supplies.
  */
final private[resolution] class SuppliedNamespaces(
    types: List[TypeEntry],
    packages: List[Frame],
    modules: List[Frame]
) {

  val qualified: Map[String, List[TypeEntry]] =
    types.filterNot(_.owner.chain.exists(_.local)).groupBy(_.key)

  val packagePaths: Map[List[String], List[Frame]] = packages.groupBy(_.path)
  val modulePaths: Map[List[String], List[Frame]] = modules.groupBy(_.path)

  private val companions = qualified.values.flatten.collect {
    case TypeEntry(name, d: Defn.Class, owner, _) if d.mods.hasCase =>
      owner.path :+ name
    case TypeEntry(name, _: Defn.Enum, owner, _) => owner.path :+ name
  }

  private val members: Set[List[String]] =
    packages.filter(SuppliedNamespaces.declaresMembers).map(_.path).toSet ++ modules.map(_.path) ++
      companions

  private val packageNames: Set[List[String]] = packages.flatMap(_.path.inits).toSet

  /** A namespace a wildcard over it binds exactly its own members of: an object, a companion, or a
    * package a supplied file declares a member in.
    */
  def closed(path: List[String]): Boolean = members(path)

  /** Any namespace a path may denote among the sources, a package's prefix included. */
  def denotes(path: List[String]): Boolean = members(path) || packageNames(path)

  def subpackage(path: List[String]): Boolean = packageNames(path)

  def bodies(path: List[String]): List[Frame] =
    modulePaths.getOrElse(path, Nil) ++ packagePaths.getOrElse(path, Nil)

  /** The object or package object whose parents add members to the namespace. */
  def template(path: List[String]): Option[Frame] =
    modulePaths.getOrElse(path, Nil) match {
      case List(module) => Some(module)
      case _            => packagePaths.getOrElse(path, Nil).find(_.tree.exists(_.is[Pkg.Object]))
    }

  /** Whether the path names a class case of a supplied enum: a type `import E.*` binds, which this
    * reading does not hold.
    */
  def enumCase(path: List[String]): Boolean =
    path.nonEmpty && qualified.getOrElse(path.init.mkString("."), Nil).exists {
      case TypeEntry(_, d: Defn.Enum, _, _) => ScopeTerms.classCases(d).contains(path.last)
      case _                                => false
    }

}

private[resolution] object SuppliedNamespaces {

  def declaresMembers(frame: Frame): Boolean =
    Trees.declaresPackageMember(frame.stats, frame.tree.exists(_.is[Pkg.Object]))

}
