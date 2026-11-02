package cardinality.analysis.capability

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{sequence, Frame}
import cardinality.analysis.Resolver
import cardinality.analysis.model.Throws
import cardinality.analysis.resolution.{InheritedScopes, TermBinding, TermBindings}
import cardinality.types.TypeApplications

/** Whether instances of a template are ever built (register D26). Every constructor, a subclass's
  * included, runs the primary constructor of each level of the template's linearization: its
  * parents' arguments, each strict `val`/`var` initializer and each statement (`Throws`). Where one
  * of them only throws, no instance exists.
  *
  * A simple name there is the root imports' only where nothing binds it at that point: the level's
  * parameters, members and inherited members (a subclass's never), the scopes around the level, and
  * the imports written before the statement — an import binds from its position on, an export
  * throughout. A parent's arguments see no member: no `this` exists yet. Where a wildcard import,
  * an unsupplied parent, an unread ancestor or a self type may bind the name, a statement that
  * throws if the name is the root imports' leaves the construction unresolved.
  */
final private[cardinality] class Construction(
    resolver: Resolver,
    steps: List[InheritedScopes.Step]
) {
  import Construction.Verdict

  /** Whether a template whose linearization is `levels` (itself first) is built. `excluded` is the
    * measured declaration, whose body is the choice being counted, not a fixed fact.
    */
  def of(levels: List[Frame], excluded: Option[Tree]): Verdict =
    Verdict.all(levels.map(level(_, excluded)))

  /** Whether the object holding a package's top-level definitions is built: a package object, or
    * the synthetic one of each file, which calling one of its definitions builds with every strict
    * top-level `val` and `var` of the file in that package.
    */
  def topLevel(pkg: Frame, excluded: Option[Tree]): Verdict =
    if (templateOf(pkg).nonEmpty) level(pkg, excluded)
    else Verdict.all(Throws.evaluated(pkg.stats, excluded).map(judge(_, pkg.chain, Set.empty, pkg)))

  private def level(frame: Frame, excluded: Option[Tree]): Verdict =
    templateOf(frame).fold(Verdict.Built) { template =>
      val parameters = frame.params.map(_.name.value).toSet
      val arguments =
        Throws.arguments(template.inits).map(judge(_, frame.chain.tail, parameters, frame))
      val body =
        Throws.evaluated(template.body.stats, excluded).map(judge(_, frame.chain, Set.empty, frame))
      Verdict.all(arguments ++ body)
    }

  private def templateOf(frame: Frame): Option[Template] = frame.tree.flatMap {
    case p: Pkg.Object => Some(p.templ)
    case other         => TypeApplications.template(other)
  }

  // Certain where every name the term would throw through is the root imports'; unresolved where
  // only a binding this reading does not enumerate keeps it from that.
  private def judge(term: Term, chain: List[Frame], parameters: Set[String], owner: Frame) =
    if (!Throws.only(term, _ => true)) Verdict.Built
    else {
      val open = chain.exists(unread)
      val binding = (name: String) =>
        if (parameters(name)) TermBinding.Parameter
        else terms.nearest(name, chain, Some(term.pos.start))
      if (Throws.only(term, name => !open && binding(name) == TermBinding.Unbound)) Verdict.Never
      else if (Throws.only(term, name => mayBeRoot(binding(name))))
        Verdict.Unresolved(s"construction not resolved: ${label(owner)}")
      else Verdict.Built
    }

  private def mayBeRoot(binding: TermBinding): Boolean = binding match {
    case TermBinding.Unbound           => true
    case TermBinding.Bound(_, certain) => !certain
    case _                             => false
  }

  private val ancestors = mutable.Map.empty[String, Option[List[Frame]]]

  // A template's supplied ancestors: the walk the measurement read, or one of its own.
  private def ancestorsOf(frame: Frame): Option[List[Frame]] =
    ancestors.getOrElseUpdate(
      frame.id,
      if (templateOf(frame).isEmpty) Some(Nil)
      else if (steps.exists(_.root.id == frame.id)) Some(InheritedScopes.levels(steps, frame))
      else
        sequence(
          InheritedScopes.walk(List(frame -> resolver.typeParameters(frame)), resolver, frame)
        ).toOption
          .map(_.map(_.declared))
    )

  private val terms = new TermBindings(ancestorsOf(_).getOrElse(Nil), resolver)

  // A scope whose members this reading cannot list: an ancestor not read, a self type's, or a
  // package object's parents (which its frame does not record).
  private def unread(frame: Frame): Boolean =
    ancestorsOf(frame).isEmpty || templateOf(frame).exists { template =>
      template.body.selfOpt.exists(_.decltpe.nonEmpty) ||
      (frame.tree.exists(_.is[Pkg.Object]) && template.inits.nonEmpty)
    }

  private def label(frame: Frame): String =
    if (frame.path.isEmpty) "<empty package>" else frame.path.mkString(".")

}

private[cardinality] object Construction {

  enum Verdict {
    case Built, Never
    case Unresolved(reason: String)

    /** Whether instances exist, or why that is unknown. */
    def built: Either[String, Boolean] = this match {
      case Built              => Right(true)
      case Never              => Right(false)
      case Unresolved(reason) => Left(reason)
    }

  }

  object Verdict {

    /** Every part runs: one that never completes leaves no instance, whatever the others do. */
    def all(parts: List[Verdict]): Verdict =
      if (parts.contains(Never)) Never
      else parts.collectFirst { case u: Unresolved => u }.getOrElse(Built)

  }

}
