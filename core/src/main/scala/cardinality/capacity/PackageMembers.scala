package cardinality.capacity

import scala.meta.*

import cardinality.types.TypeName

/** What a package holds beyond its files' own definitions: the members its package object's parents
  * pass on (`inherits`) and the aliases its files' top-level export clauses define (Scala 3
  * reference, export clauses: they define members of the enclosing scope — for a top-level clause,
  * of the package). A name a read inside the package finds nowhere nearer is a package member of
  * another file's precedence, so either may bind it over the root imports (Scala 3.9.0 reads
  * `lib.U(b: Boolean)`'s `Boolean` as `basep.Base`'s `Int` through `package object lib extends
  * basep.Base`: `StoredScopesSpec`'s `packageParents`). Neither is followed: either may bind any
  * name the files do not define, which is a reason. `exposure` is what a clause from outside the
  * package imports of it.
  */
final private[capacity] case class PackageMembers(
    inherits: Boolean,
    exports: Boolean,
    exposure: Exposure
) {

  /** How a type `name` binds as a member of the package at `path`: a reason, or nothing. */
  def bind(name: TypeName, path: List[TypeName]): Option[Binding] =
    passedOn(path).map(why => Binding.Clause(Definition.Unbound.Imported(name.value, why)))

  /** What a path's first name the package's files do not define names as a member of the package:
    * the reason a parent or an export may define one; nothing where none can.
    */
  def head(path: List[TypeName]): Option[Namespaces.Target] =
    passedOn(path).map(why => Namespaces.Target.Unknown(Some(why)))

  // Why a name the files do not define may still be a member of the package at `path`.
  private def passedOn(path: List[TypeName]): Option[String] =
    Exposure
      .passedOn(inherits, exports)
      .map(member => s"may be a member of ${PackageMembers.owner(path)}, $member not followed")

}

private[capacity] object PackageMembers {

  /** A package as a reason names it: dotted, or the empty package. */
  def owner(path: List[TypeName]): String =
    if (path.isEmpty) "the empty package" else s"package ${path.map(_.value).mkString(".")}"

  /** What one package's files add to it: `parts` are its package clauses and package objects. */
  def of(parts: List[Walk.Part]): PackageMembers = {
    val inherits = parts.exists(_.packageObject.exists(_.templ.inits.nonEmpty))
    val stats = parts.flatMap(_.stats)
    val exports = stats.exists(_.is[Export])
    PackageMembers(inherits, exports, Exposure(stats, inherits, exports))
  }

}
