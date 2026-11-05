package cardinality.capacity

import scala.meta.*

import cardinality.types.TypeName

/** What a package holds beyond its files' own definitions: the members its package object's parents
  * pass on (`inherits`) and the aliases its files' top-level export clauses define (Scala 3
  * reference, export clauses: they define members of the enclosing scope — for a top-level clause,
  * of the package). A name a read inside the package finds nowhere nearer is a package member of
  * another file's precedence, so either may bind it over the root imports (Scala 3.9.0 reads
  * `lib.U(b: Boolean)`'s `Boolean` as `basep.Base`'s `Int` through `package object lib extends
  * basep.Base`, and `lib3.W`'s through `export Src.*` in another file: `StoredScopesSpec`'s
  * `packageParents`, `packageExports`). An export clause is followed when it can be read from the
  * package alone (`exports`); a parent, or any other export clause, may bind any name the files do
  * not define, which is a reason (`unread`). `exposure` is what a clause from outside the package
  * imports of it, which follows neither.
  */
final private[capacity] case class PackageMembers(
    inherits: Boolean,
    exports: List[PackageMembers.Exported],
    unread: Boolean,
    exposure: Exposure
) {

  /** How a type `name` binds as a member of the package at `path`, read in `in` (the scope that
    * solved the package's objects): an exported alias of an object the package defines, a reason,
    * or nothing.
    */
  def bind(name: TypeName, path: List[TypeName], in: Scope): Option[Binding] =
    exports.iterator
      .flatMap(_.bind(name, in))
      .nextOption()
      .orElse(passedOn(path).map(PackageMembers.refused(name)))

  /** What a path's first name the package's files do not define names as a member of the package: a
    * term an export passes on — the exported object's own member, or a reason — or the reason a
    * parent or an unfollowed export may define one; nothing where none can.
    */
  def head(first: TypeName, path: List[TypeName], in: Scope): Option[Namespaces.Target] =
    exports.iterator
      .flatMap(_.head(first, in))
      .nextOption()
      .orElse(passedOn(path).map(why => Namespaces.Target.Unknown(Some(why))))

  // Why a name the files do not define may still be a member of the package at `path`.
  private def passedOn(path: List[TypeName]): Option[String] =
    Exposure
      .passedOn(inherits, unread)
      .map(member => s"may be a member of ${PackageMembers.owner(path)}, $member not followed")

}

private[capacity] object PackageMembers {

  // A name a clause would bind, refused for why.
  private def refused(name: TypeName)(why: String): Binding =
    Binding.Clause(Definition.Unbound.Imported(name.value, why))

  /** A package as a reason names it: dotted, or the empty package. */
  def owner(path: List[TypeName]): String =
    if (path.isEmpty) "the empty package" else s"package ${path.map(_.value).mkString(".")}"

  /** A top-level export clause over an object the package defines, read where its file has no
    * import before it: `module`'s members it selects — every one a wildcard does not hide (a given
    * only by name: Scala 3 reference, export clauses), or the named ones (`selected`, by the name
    * each binds).
    */
  final case class Exported(
      module: TypeName,
      wildcard: Boolean,
      hidden: Set[TypeName],
      selected: Map[TypeName, TypeName]
  ) {

    // The object's member the clause binds under `name`: the object's own definition, or a reason
    // when the object may pass on a member it does not define, or keeps a private one.
    def bind(name: TypeName, in: Scope): Option[Binding] =
      member(name, in).flatMap { (member, exposure) =>
        val key = Namespaces.key(module, member)
        exposure
          .withheldMember(member, module)
          .map(refused(name))
          .orElse(Option.when(in.defines(key))(Binding.Elsewhere(in, key)))
          .orElse(exposure.unreadMember(module).map(refused(name)))
      }

    // The same for a path's first name: the namespace or value the object's member is, or none
    // for a given a wildcard does not export.
    def head(name: TypeName, in: Scope): Option[Namespaces.Target] =
      member(name, in).flatMap { (member, exposure) =>
        val found = Namespaces.Target.Obj(module, in, exposure).select(member)
        exposure.withheldMember(member, module) match {
          case Some(why) => Some(Namespaces.Target.Unknown(Some(why)))
          case None if found != Namespaces.Target.Absent =>
            Option.unless(!selected.contains(name) && exposure.isGiven(member))(found)
          case None =>
            exposure.unreadMember(module).map(why => Namespaces.Target.Unknown(Some(why)))
        }
      }

    // The member of the object the clause binds under `name`, with what the object exposes.
    private def member(name: TypeName, in: Scope): Option[(TypeName, Exposure)] =
      selected
        .get(name)
        .orElse(Option.when(wildcard && !hidden(name))(name))
        .flatMap(member => in.term(module).collect { case Bound.Module(_, e) => member -> e })

  }

  /** What one package's files add to it: `parts` are its package clauses and package objects. */
  def of(parts: List[Walk.Part]): PackageMembers = {
    val modules = parts.flatMap(_.stats.collect { case o: Defn.Object => o.name.value }).toSet
    val clauses = parts.flatMap { part =>
      part.stats.collect { case e: Export => e }.map(exported(part, modules, _))
    }
    val inherits = parts.exists(_.packageObject.exists(_.templ.inits.nonEmpty))
    val stats = parts.flatMap(_.stats)
    PackageMembers(
      inherits,
      clauses.flatten.flatten,
      clauses.exists(_.exists(_.isEmpty)),
      Exposure(stats, inherits, stats.exists(_.is[Export]))
    )
  }

  // Each importer of a clause, read from the package alone: over a plain name of an object the
  // package defines, in a file that imports nothing before it. Any other is None (not followed).
  private def exported(part: Walk.Part, modules: Set[String], e: Export): List[Option[Exported]] = {
    val unimported = part.layer.imports == Imports.none &&
      part.stats.takeWhile(_ ne e).forall(!_.is[Import])
    e.importers.map { importer =>
      importer.ref match {
        case Term.Name(module) if unimported && modules(module) =>
          Some(
            Exported(
              TypeName.of(module),
              importer.importees.exists(_.is[Importee.Wildcard]),
              Imports.hidden(importer),
              importer.importees.collect {
                case Importee.Name(n)          => TypeName.of(n.value) -> TypeName.of(n.value)
                case Importee.Rename(from, to) => TypeName.of(to.value) -> TypeName.of(from.value)
              }.toMap
            )
          )
        case _ => None
      }
    }
  }

}
