package cardinality.analysis.resolution

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.model.Anchor
import cardinality.analysis.{BuiltinTypes, ImportBinding, SourceIndex}
import cardinality.types.ListedNamespaces

/** What `SourceLookup` reads a path's first segment and a wildcard clause as: the import that bound
  * a name, why a segment is not read, and what one wildcard or one scope's imports bind a name to.
  */
private[resolution] object LookupBindings {

  /** Why a path's first segment is not read: a binding this reading does not follow, or a wildcard
    * over a namespace no source supplies that may bind it (`why`) where it would otherwise denote
    * `fallback`.
    */
  enum Unread {
    case Refused(reason: String)
    case Doubted(head: String, why: String, fallback: List[List[String]])

    /** Why `subject` — a path through the segment, or a name an import of such a path binds (how,
      * in `via`) — is not read.
      */
    def reason(subject: String, via: String = ""): String = this match {
      case Refused(reason)       => reason
      case Doubted(head, why, _) => s"unresolved type: $subject ($via$head $why)"
    }

    def selecting(rest: List[String]): Unread = this match {
      case doubted: Doubted => doubted.copy(fallback = doubted.fallback.map(_ ++ rest))
      case refused          => refused
    }

  }

  /** The namespace a name is read in: a type name, or a term — a path's first name among them —
    * which a `given` selector may bind too, and which a listed stdlib namespace declares apart. A
    * type name the root imports bind and the analyzer reads is one a namespace whose members the
    * table does not list binds only where the table says so, as on the stored side.
    */
  enum Space {
    case Types, Terms

    /** The prefixes a clause's `given` selectors open in this namespace: none for a type name. */
    def givens(clause: ImportBinding): List[List[String]] = this match {
      case Types => Nil
      case Terms => clause.givens
    }

    /** An importer whose prefix is no path, as written, where it may bind the name here. */
    def unfollowed(
        prefix: ImportBinding.Unfollowed,
        name: String,
        hidden: Set[String]
    ): Option[String] = this match {
      case Types => prefix.wildcard(name, hidden)
      case Terms => prefix.wildcard(name, hidden).orElse(prefix.givenSelector)
    }

    /** How a wildcard over a listed stdlib namespace binds the name in this namespace. */
    def binding(
        namespace: ListedNamespaces.Namespace,
        name: String
    ): Option[ListedNamespaces.Binding] =
      this match {
        case Types => namespace.typeBinding(name, root = BuiltinTypes.rootNames(name))
        case Terms => namespace.termBinding(name)
      }

  }

  /** What one wildcard clause binds a name to (see `Reader.opening`): the supplied namespaces it
    * opens, the namespace it binds a path's first name to (a `given` selector's implicit object), a
    * listed stdlib namespace's member, a refusal, a doubt, or nothing.
    */
  enum Opening {
    case Supplied(paths: List[List[String]])
    case Entered(paths: List[List[String]])
    case Listed(fqn: String)
    case Refused(reason: String)

    /** A wildcard that may bind any name, as written; the first name its own prefix rests on, when
      * that is what is in doubt; and the supplied namespaces the prefix would otherwise denote.
      */
    case Doubt(written: String, through: Option[Through], fallback: List[List[String]])
    case Unbound
  }

  /** A clause prefix's first name in doubt, and why. */
  final case class Through(head: String, why: String)

  object Opening {

    /** The latest of one scope's wildcards that may bind any name. */
    def doubt(openings: List[Opening]): Option[Doubt] =
      openings.reverseIterator.collectFirst { case doubt: Doubt => doubt }

    /** The supplied paths the doubted wildcards among `openings` would otherwise open `name` at. */
    def fallbacks(openings: List[Opening], name: String): List[List[String]] =
      openings.flatMap {
        case Doubt(_, _, fallback) => fallback.map(_ :+ name)
        case _                     => Nil
      }

    extension (doubt: Doubt) {

      /** Why a name the wildcard may bind is not read (`may be imported by cats.*`). */
      def why: String =
        doubt.through.fold(ListedNamespaces.mayBind(doubt.written))(through =>
          ListedNamespaces.mayBindThrough(doubt.written, through.head, through.why)
        )

      /** Why a path's first name the wildcard may bind is not read: a wildcard whose own prefix is
        * in doubt passes that doubt on.
        */
      def passed: String = doubt.through.fold(ListedNamespaces.mayBind(doubt.written))(_.why)
    }

  }

  /** The absolute paths a written path denotes, nearest first, with the import that bound its first
    * segment, if one did.
    */
  type Reading = (List[List[String]], Option[Origin])

  /** What a path's first segment is read under past the scopes walked so far: a parent of theirs
    * this reading does not read (which refuses what an outer scope would bind), and a wildcard of
    * theirs that may bind it.
    */
  final case class Pending(unread: Option[String], doubt: Option[Prefix.Doubt]) {

    def parent(reason: Option[String]): Pending = copy(unread = unread.orElse(reason))

    def doubting(d: Prefix.Doubt): Pending = copy(doubt = doubt.orElse(Some(d)))
  }

  object Pending {
    val none: Pending = Pending(None, None)

    /** One scope's reading of a first segment: its binding, or the state the outer scopes read. */
    type Step = Either[Pending, Option[Either[Unread, Reading]]]
  }

  /** What one scope's imports bind a path's first segment to: a doubt carries the wildcard's own
    * reason on (`passed`), with the supplied paths the segment would otherwise denote.
    */
  enum Prefix {
    case Named(found: Either[Unread, (List[List[String]], Origin)])
    case Opened(found: Either[Unread, (List[List[String]], Origin)])
    case Doubt(passed: String, fallback: List[List[String]])
  }

  /** The import that bound a name or a path's first segment: the path it binds and how. */
  final case class Origin(path: String, binding: String) {

    def selecting(rest: List[String]): Origin =
      if (rest.isEmpty) this else Origin((path :: rest).mkString("."), s"selected through $binding")

    def exportedBy(scope: Frame): Origin =
      copy(binding =
        s"$binding, exported by ${scope.path.lastOption.getOrElse("the empty package")}"
      )

  }

  object Origin {

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
  final case class Member(found: Found, depth: Int, exported: Boolean)

  /** A named import's target, with the scope and position its own prefix is read at. */
  final case class Target(segments: List[String], scope: Frame, at: Option[Anchor])

  // The targets a clause's explicit selectors bind a name to, each read where the clause stands —
  // or, for an importer whose prefix is no path, a value's member, which is not followed.
  def selected(
      clause: ImportBinding,
      name: String,
      scope: Frame,
      file: String
  ): List[Either[String, Target]] =
    clause.named.collect {
      case (`name`, segments) =>
        Right(Target(segments, scope, before(file, clause)))
    } ++ clause.unfollowed
      .filter(_.names(name))
      .map(prefix => Left(s"path through a value not followed: ${prefix.written}"))

  // The first target's path as written, for the binding a reason names.
  def written(targets: List[Either[String, Target]]): List[String] =
    targets.collectFirst { case Right(target) => target.segments }.getOrElse(Nil)

  // A clause's prefix is read where the clause stands: only earlier clauses have bound names.
  def before(file: String, clause: ImportBinding): Option[Anchor] =
    Some(Anchor(file, clause.start - 1))

  def entryOrAmbiguous(found: List[TypeEntry], name: String): Found =
    found match {
      case List(entry) => Found.Entry(entry)
      case _           => Found.Blocked(s"ambiguous type: $name")
    }

}
