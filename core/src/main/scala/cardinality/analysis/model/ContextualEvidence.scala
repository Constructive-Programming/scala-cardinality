package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.inhabitation.*

import Inhabitation.Shape

/** Which anonymous contextual parameters a body can actually use.
  *
  * An anonymous `using T` is reached only through implicit search, where a more deeply nested
  * candidate wins outright and one level is ranked by rules Scala has changed between versions. Two
  * candidates of one type declared by the same template or method make every search for it an
  * error. A candidate of type `A ?=> R` also answers searches for `R`, and one for `A ?=> R` finds
  * a plain `R`. The model counts the parameter only when every type a search may select it for is
  * provably unrelated to every type another candidate at its level or deeper answers.
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

  /** A type's identity (`TypeIdentities`) and the final concrete class it applies, if any. */
  final case class Nominal(identity: Option[String], finalClass: Option[String])

  def contextual(param: Term.Param): Boolean =
    param.mods.exists(m => m.is[Mod.Using] || m.is[Mod.Implicit])

  def anonymous(param: Term.Param): Boolean = param.name.is[Name.Anonymous]

  /** One key per owner and position: anonymous parameters all have the empty name. */
  def key(owner: List[String], index: Int): String = s"<using#$index@${owner.mkString(".")}>"

  /** The type a contextual definition offers to implicit search when it is one plain type; None for
    * a conditional given, an implicit conversion or object (a candidate of unread type).
    */
  def offered(stat: Stat): Option[Option[Type]] = stat match {
    case g: Defn.GivenAlias => Some(Option.when(g.paramClauseGroups.isEmpty)(g.decltpe))
    case g: Decl.Given      => Some(Option.when(g.paramClauseGroups.isEmpty)(g.decltpe))
    case g: Defn.Given      =>
      Some(g.templ.inits match {
        case List(single) if g.paramClauseGroups.isEmpty => Some(single.tpe)
        case _                                           => None
      })
    case v: Defn.Val if implicitly(v.mods) => Some(v.decltpe)
    case v: Decl.Val if implicitly(v.mods) => Some(Some(v.decltpe))
    case d: Defn.Def if implicitly(d.mods) =>
      Some(d.decltpe.filter(_ => d.paramClauseGroups.isEmpty))
    case d: Decl.Def if implicitly(d.mods) =>
      Some(Option.when(d.paramClauseGroups.isEmpty)(d.decltpe))
    case o: Defn.Object if implicitly(o.mods) => Some(None)
    case _                                    => None
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

  private def implicitly(mods: List[Mod]): Boolean = mods.exists(_.is[Mod.Implicit])

  /** A diagnostic per anonymous candidate a search may not select. An unread type that is already a
    * diagnostic blocks the count by its own reason, so it is not repeated as an overlap.
    */
  def unreachable(candidates: List[Candidate], bounded: String => Boolean): List[String] =
    candidates.filter(c => c.anonymous && c.shape.nonEmpty).flatMap { evidence =>
      candidates
        .filter(other => (other ne evidence) && other.level >= evidence.level)
        .filterNot(other => other.shape.isEmpty && other.reported)
        .collectFirst {
          case other if ambiguous(evidence, other) =>
            s"ambiguous anonymous evidence: ${evidence.label}"
          case other if overlaps(evidence, other, bounded) =>
            s"anonymous evidence selection not resolved: ${evidence.label} (overlaps ${other.label})"
        }
    }

  // Only the same type declared at one site is an error: against a candidate inherited from a
  // parent Scala prefers the derived class's own (a 3.9.0 fixture selects the parameter), so that
  // pair is an unresolved overlap. A shape is no identity: `P1[A]` and `P2[A]` may share one.
  private def ambiguous(evidence: Candidate, other: Candidate): Boolean =
    other.site == evidence.site && evidence.nominal.identity.nonEmpty &&
      other.nominal.identity == evidence.nominal.identity

  // A search selects the evidence for its type or a result of it, and finds the other for its own.
  private def overlaps(evidence: Candidate, other: Candidate, bounded: String => Boolean): Boolean =
    (evidence.shape, other.shape) match {
      case (Some(_), Some(_)) if distinctFinal(evidence.nominal, other.nominal) => false
      case (Some(mine), Some(theirs))                                           =>
        (mine :: evidence.results).exists(wanted =>
          (theirs :: other.results).exists(found => !provablyApart(wanted, found, bounded))
        )
      case _ => true
    }

  // A binder with a subtype bound may relate to anything its bound reaches: no type mentioning one
  // is provably unrelated.
  private def provablyApart(mine: Shape, theirs: Shape, bounded: String => Boolean): Boolean = {
    val mentionsBounded = List(mine, theirs).exists(shape =>
      Shape.exists(shape) {
        case Shape.Atom(id) => bounded(id)
        case _              => false
      }
    )
    if (mentionsBounded) false else unrelated(mine, theirs)
  }

  // Two distinct final concrete classes: neither is a subtype of the other.
  private def distinctFinal(left: Nominal, right: Nominal): Boolean =
    (left.finalClass, right.finalClass) match {
      case (Some(l), Some(r)) => l != r
      case _                  => false
    }

  /** Neither type can be a subtype of the other in the body. Distinct unbounded binders, and a
    * binder against a nominal type (only top and bottom relate to every binder); two distinct
    * applications of rigid constructors (invariant, so only equal keys relate); functions and
    * products at different arities or with an unrelated component. Sums, the empty product, the
    * empty sum, paths, existentials and evidence are assumed related. `provablyApart` rules out
    * bounded binders first: every `Shape.Atom` here is unbounded.
    */
  private def unrelated(left: Shape, right: Shape): Boolean = (left, right) match {
    case _ if left == right                                                   => false
    case (_: Shape.Atom | _: Shape.Applied, _: Shape.Atom | _: Shape.Applied) => true
    case (_: Shape.Atom | _: Shape.Applied, other)                            => nominal(other)
    case (other, _: Shape.Atom | _: Shape.Applied)                            => nominal(other)
    case (Shape.Function(lp, lr), Shape.Function(rp, rr))                     =>
      lp.size != rp.size || lp.lazyZip(rp).exists(unrelated) || unrelated(lr, rr)
    case (Shape.Product(l), Shape.Product(r)) if l.nonEmpty && r.nonEmpty =>
      l.size != r.size || l.lazyZip(r).exists(unrelated)
    case _ => false
  }

  // A type with a class of its own, neither top nor bottom.
  private def nominal(shape: Shape): Boolean = shape match {
    case Shape.Product(fields)   => fields.nonEmpty
    case Shape.Sum(alternatives) => alternatives.nonEmpty
    case _: Shape.Function       => true
    case _: Shape.Repeated       => true
    case _: Shape.Evidence       => true
    case _                       => false
  }

}
