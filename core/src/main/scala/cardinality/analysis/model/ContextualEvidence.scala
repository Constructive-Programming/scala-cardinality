package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation

import Inhabitation.Shape

/** Which anonymous contextual parameters a body can actually use.
  *
  * An anonymous `using T` is reached only through implicit search, where a more deeply nested
  * candidate wins outright and one level is ranked by rules Scala has changed between versions. Two
  * candidates of one type declared by the same template or method make every search for it an
  * error. The model counts the parameter only when every other candidate at its level or deeper is
  * a class type of a template its own type's template is provably unrelated to.
  */
private[cardinality] object ContextualEvidence {

  /** One contextual value in scope: nesting level (outermost 0), the template, method or block that
    * declares it, label, type when read (shape, context-function results, declarations named),
    * whether only implicit search reaches it, and whether an unread type is already reported.
    */
  final case class Candidate(
      level: Int,
      site: String,
      label: String,
      shape: Option[Shape],
      anonymous: Boolean,
      reported: Boolean = false,
      nominal: Nominal = Nominal(None, None),
      results: List[Shape] = Nil
  )

  /** A type's identity (`TypeIdentities`), and the final class or the template it applies (with its
    * base classes, all supplied), if any.
    */
  final case class Nominal(
      identity: Option[String],
      finalClass: Option[String],
      lineage: Option[Lineage] = None
  )

  /** The template a class type applies, its supplied base classes, and whether they are all. */
  final case class Lineage(head: String, bases: Set[String], complete: Boolean)

  /** A `using` or `implicit` parameter, or a context-function literal's: a given of its body. */
  def contextual(param: Term.Param): Boolean =
    param.mods.exists(m => m.is[Mod.Using] || m.is[Mod.Implicit]) || literal(param)

  /** A parameter no name reaches: `using T`, or a context-function literal's `_: T`. */
  def anonymous(param: Term.Param): Boolean =
    param.name.is[Name.Anonymous] || param.name.is[Name.Placeholder]

  private def literal(param: Term.Param): Boolean =
    param.parent.flatMap(_.parent).exists(_.is[Term.ContextFunction])

  /** One key per owner and position: anonymous parameters all have the empty name. */
  def key(owner: List[String], index: Int): String = s"<using#$index@${owner.mkString(".")}>"

  /** Whether a statement defines a contextual value: `Some(None)`, a candidate whose type is not
    * read, which overlaps every anonymous evidence it may compete with.
    */
  def offered(stat: Stat): Option[Option[Type]] = stat match {
    case _: (Defn.GivenAlias | Decl.Given | Defn.Given) => Some(None)
    case d: (Defn.Val | Decl.Val | Defn.Def | Decl.Def | Defn.Object)
        if MemberSyntax.implicitModifier(d.mods) =>
      Some(None)
    case _ => None
  }

  /** An import or export that may bring a contextual value: every selector but an unimport — a
    * named one too, since `import H.s` imports the given `s` (a Scala 3.9.0 fixture: E172 against
    * the class's own evidence), and a wildcard brings old-style implicits. Unseen candidates.
    */
  def importsCandidates(stat: Stat): Boolean = stat match {
    case i: Import => i.importers.exists(bringsAny)
    case e: Export => e.importers.exists(bringsAny)
    case _         => false
  }

  private def bringsAny(importer: Importer): Boolean =
    importer.importees.exists(!_.is[Importee.Unimport])

  /** A diagnostic per anonymous candidate a search may not select. An unread type that is already a
    * diagnostic blocks the count by its own reason, so it is not repeated as an overlap.
    */
  def unreachable(
      candidates: List[Candidate],
      @annotation.unused bounded: String => Boolean
  ): List[String] =
    candidates.filter(c => c.anonymous && c.shape.nonEmpty).flatMap { evidence =>
      candidates
        .filter(other => (other ne evidence) && other.level >= evidence.level)
        .filterNot(other => other.shape.isEmpty && other.reported)
        .collectFirst {
          case other if ambiguous(evidence, other) =>
            s"ambiguous anonymous evidence: ${evidence.label}"
          case other if overlaps(evidence, other) =>
            s"anonymous evidence selection not resolved: ${evidence.label} (overlaps ${other.label})"
        }
    }

  // Only the same type declared at one site is an error: against a candidate inherited from a
  // parent Scala prefers the derived class's own (a 3.9.0 fixture selects the parameter), so that
  // pair is an unresolved overlap. A shape is no identity: `P1[A]` and `P2[A]` may share one.
  private def ambiguous(evidence: Candidate, other: Candidate): Boolean =
    other.site == evidence.site && evidence.nominal.identity.nonEmpty &&
      other.nominal.identity == evidence.nominal.identity

  // A search selects the evidence for its type, and finds the other for its own: two class types of
  // distinct templates relate only through a base class. Anything else is taken to overlap.
  private def overlaps(evidence: Candidate, other: Candidate): Boolean =
    other.shape.isEmpty || classes(evidence.nominal, other.nominal).getOrElse(true)

  private def classes(left: Nominal, right: Nominal): Option[Boolean] =
    (left.lineage, right.lineage) match {
      case (Some(l), Some(r)) if l.head != r.head =>
        val related = l.bases(r.head) || r.bases(l.head)
        Option.when(related || (l.complete && r.complete))(related)
      case _ => None
    }

}
