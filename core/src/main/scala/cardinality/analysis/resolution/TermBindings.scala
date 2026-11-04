package cardinality.analysis.resolution

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.Resolver
import cardinality.analysis.model.ScopeTerms

/** What a simple term name denotes at a reference, as far as the scopes around it tell. */
private[cardinality] enum TermBinding {

  /** A parameter of an enclosing method, class or extension. */
  case Parameter

  /** The nearest defining scope: one frame, or the frames of the package's other files. */
  case Defined(scopes: List[Frame])

  /** An inherited member, export or import that may bind the name (`certain` where it names it). */
  case Bound(why: String, certain: Boolean = false)

  case Unbound
}

/** The nearest binding of a term name, scope by scope outward, in Scala's precedence (register
  * D11): where an inherited member, an export or an import may bind it, no declaration is read.
  */
final private[cardinality] class TermBindings(levels: Frame => List[Frame], resolver: Resolver) {

  /** The nearest binding of a term `name` from the reference's chain (at offset `at`, an import
    * binds from its position on).
    */
  def nearest(name: String, chain: List[Frame], at: Option[Int] = None): TermBinding =
    chain.iterator.flatMap(scope(name, _, at)).nextOption().getOrElse(TermBinding.Unbound)

  /** How one scope binds a term `name`, if it does. */
  def scope(name: String, frame: Frame, at: Option[Int] = None): Option[TermBinding] =
    if (frame.params.exists(_.name.value == name)) Some(TermBinding.Parameter)
    else
      inherited(name, frame)
        .orElse(
          Option.when(ScopeTerms.defined(frame.stats)(name))(TermBinding.Defined(List(frame)))
        )
        .orElse(clauses(name, frame, at))
        .orElse(packaged(name, frame))

  /** Whether the scope that declares a class binds a term of the class's name to anything but the
    * class and its companion: a member it inherits or another definition, or, for a class of
    * another file of the package, a binding of the reference's own file, which outranks it.
    */
  def beside(name: String, frame: Frame, owner: Frame): Boolean =
    inherited(name, frame).nonEmpty || ScopeTerms.values(frame.stats)(name) ||
      (frame.id != owner.id && outranks(name, frame, owner))

  // What the reference's own file binds in the package scope of a class another file declares.
  private def outranks(name: String, frame: Frame, owner: Frame): Boolean =
    clauses(name, frame).nonEmpty ||
      resolver
        .scopes(frame)
        .exists(peer => peer.id != owner.id && ScopeTerms.values(peer.stats)(name))

  // A template's inherited members, and those of the synthetic object that holds top-level
  // definitions, overload or override its own.
  private def inherited(name: String, frame: Frame): Option[TermBinding] =
    Option
      .when(frame.isTemplate || resolver.isPackage(frame))(
        InheritedTerms.inherits(frame, levels(frame), name, resolver)
      )
      .flatten
      .map(parent =>
        TermBinding.Bound(s"which a template may inherit from ${parent.parent}", parent.certain)
      )

  // An export defines a member throughout its template; an import binds below it.
  private def clauses(name: String, frame: Frame, at: Option[Int] = None): Option[TermBinding] = {
    val imports = frame.stats.filter(stat => at.forall(stat.pos.end <= _))
    bound(ScopeTerms.exported(frame.stats), name, "which an export may define")
      .orElse(bound(ScopeTerms.imported(imports), name, "which an import may bind"))
  }

  // Clauses whose explicit selectors spell the name bind it; one that may bind any name may too.
  private def bound(spelled: Option[Set[String]], name: String, why: String) =
    Option.when(spelled.forall(_(name)))(TermBinding.Bound(why, certain = spelled.nonEmpty))

  // A member of the package another file defines, which every binding of the reference's own file
  // outranks.
  private def packaged(name: String, frame: Frame): Option[TermBinding] =
    Option
      .when(resolver.isPackage(frame))(
        resolver
          .scopes(frame)
          .filter(other => other.id != frame.id && ScopeTerms.defined(other.stats)(name))
      )
      .filter(_.nonEmpty)
      .map(TermBinding.Defined(_))

}
