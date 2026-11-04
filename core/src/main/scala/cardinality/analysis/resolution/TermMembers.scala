package cardinality.analysis.resolution

import scala.meta.{Defn, Stat, Tree, XtensionClassifiable}

import cardinality.analysis.ExportClause
import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.model.ScopeTerms

import LookupBindings.{before, Unread}

/** What an import clause over a supplied namespace binds a path's first name to: its members of
  * that term name the clause imports.
  *
  * Scala imports through a wildcard every accessible member but a given, through a `given` selector
  * the given and implicit ones (Scala 3 reference, given imports); a private member is no import
  * outside the scopes it is accessible in (Scala 3.9.0 binds `compose` under `import Lib.*` to
  * another file's package when `Lib`'s is private: probes/review-A7/round2/private). A namespace's
  * members are its own definitions — a package's subpackages among them —, the aliases its export
  * clauses define and, for an object or a package object, what its parents pass on (Scala 3.9.0
  * reads `compose.C` under `import Lib.*` through `object Lib extends Base`'s inherited `compose`:
  * round2/givenparent2). An object, a subpackage or a case class's or enum's companion is a
  * namespace the rest of the path is read in; a value, method or given is a value whose type's
  * member the path selects, not followed; a parent outside the sources may declare any name.
  */
final private[resolution] class TermMembers(
    supplied: SuppliedNamespaces,
    exports: Map[String, List[ExportClause]],
    objects: InheritedObjects,
    accessible: (Tree, Frame, Frame) => Boolean,
    absolute: (List[String], Frame, Option[(String, Int)]) => Either[Unread, List[List[String]]]
) {

  /** What a wildcard over the namespaces at `paths` binds a path's first name to, read in `from`
    * (Scala 3.9.0 binds `compose` through `import Lib.*` to `val Lib.compose` over another file's
    * package: probes/review-A7/r15); over a package the sources spell only as a prefix, a
    * subpackage; nothing (Right(Nil)) where it has no member of the name.
    */
  def wildcard(
      paths: List[List[String]],
      name: String,
      from: Frame
  ): Either[Unread, List[List[String]]] =
    paths.iterator
      .flatMap(path =>
        if (supplied.closed(path)) bound(path, name, givens = false, from)
        else Option.when(supplied.subpackage(path :+ name))(Right(List(path :+ name)))
      )
      .nextOption()
      .getOrElse(Right(Nil))

  /** What a `given` selector over the namespaces at `paths` binds a term name to, among the closed
    * ones.
    */
  def givens(
      paths: List[List[String]],
      name: String,
      from: Frame
  ): TermMembers.Bound =
    paths.filter(supplied.closed).iterator.flatMap(bound(_, name, givens = true, from)).nextOption()

  /** The member the namespace at `path` binds `name` to, read in `from`: the namespace it is
    * (Right) or why it is not read; None when the clause imports no member of that name.
    */
  def bound(
      path: List[String],
      name: String,
      givens: Boolean,
      from: Frame,
      seen: Set[String] = Set.empty
  ): TermMembers.Bound =
    own(path, name, givens, from)
      .orElse(exported(path, name, givens, seen))
      .orElse(
        supplied
          .template(path)
          .flatMap(objects.inheritedTerm(_, name, givens, from))
          .map(_.fold(reason => Left(Unread.Refused(reason)), found => Right(List(found))))
      )

  // The namespace's own definitions of the name the clause imports, and its subpackage.
  private def own(
      path: List[String],
      name: String,
      givens: Boolean,
      from: Frame
  ): TermMembers.Bound = {
    val defined = supplied
      .bodies(path)
      .flatMap(frame =>
        ScopeTerms.defining(frame.stats, name).filter(stat => reachable(stat, frame, from))
      )
      .filter(ScopeTerms.imports(_, givens))
    if (defined.exists(ScopeTerms.namespace)) Some(Right(List(path :+ name)))
    else if (defined.nonEmpty) Some(Left(TermMembers.value(name)))
    else Option.when(!givens && supplied.subpackage(path :+ name))(Right(List(path :+ name)))
  }

  // A member is accessible where `accessible` says, and an object's private member inside its
  // companion class too (which shares the object's path).
  private def reachable(stat: Stat, owner: Frame, from: Frame): Boolean =
    accessible(stat, owner, from) ||
      (owner.tree.exists(_.is[Defn.Object]) && from.chain.exists(_.path == owner.path))

  // The alias an export clause of the namespace defines under the name: the member its explicit
  // selector or its wildcard forwards, read as an import of the same kind over its prefix; a
  // clause whose prefix is no supplied path, or one met again, is not followed.
  private def exported(
      path: List[String],
      name: String,
      givens: Boolean,
      seen: Set[String]
  ): TermMembers.Bound =
    supplied
      .bodies(path)
      .iterator
      .flatMap(frame => exports.getOrElse(frame.id, Nil).map(frame -> _))
      .filter((_, clause) =>
        if (givens) ScopeTerms.opensGivens(clause.importer)
        else ExportClause.mayDefine(clause.importer, name)
      )
      .flatMap((frame, clause) => forwarded(clause, frame, seen)(name, givens))
      .nextOption()

  private def forwarded(
      clause: ExportClause,
      frame: Frame,
      seen: Set[String]
  )(name: String, givens: Boolean): TermMembers.Bound = {
    val unfollowed = Left(TermMembers.unfollowed(name, clause.prefix))
    clause.binding match {
      case _ if seen(frame.id) => Some(unfollowed)
      case None                => Some(unfollowed)
      case Some(binding)       =>
        val at = before(frame.input, binding)
        val target = binding.named.collectFirst { case (`name`, segments) => segments }
        val prefixes = if (givens) binding.givens else binding.opening(name)
        target
          .map(segments => segments.init -> segments.last)
          .orElse(prefixes.headOption.map(_ -> name))
          .flatMap((prefix, member) =>
            absolute(prefix, frame, at) match {
              case Left(unread) => Some(Left(unread))
              case Right(paths) =>
                paths.find(supplied.closed) match {
                  case Some(found) => bound(found, member, givens, frame, seen + frame.id)
                  case None        => Some(unfollowed)
                }
            }
          )
    }
  }

}

private[resolution] object TermMembers {

  /** What a clause binds a head to: the namespace it is, or why it is not read; None for nothing.
    */
  type Bound = Option[Either[Unread, List[List[String]]]]

  /** Why a path through a value is not read. */
  def value(name: String): Unread = Unread.Refused(s"path through a value not followed: $name")

  private def unfollowed(name: String, prefix: String): Unread =
    Unread.Refused(s"export not followed: $name (export of $prefix)")

}
