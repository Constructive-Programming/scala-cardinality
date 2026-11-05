package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.model.Throws
import cardinality.types.TypeApplications

import MethodAnalysis.Frame

/** Whether instances of a template are ever built (register D26). Every constructor, a subclass's
  * included, runs the primary constructor of each level of the template's linearization: its
  * parents' arguments, each strict `val`/`var` initializer and each statement (`Throws`). Where one
  * of them may only throw, whether an instance exists is not resolved here.
  */
private[cardinality] object Construction {

  enum Verdict {
    case Built
    case Unresolved(reason: String)

    /** Whether instances exist, or why that is unknown. */
    def built: Either[String, Unit] = this match {
      case Built              => Right(())
      case Unresolved(reason) => Left(reason)
    }

  }

  object Verdict {

    /** Every part runs: the first that is not resolved decides. */
    def all(parts: List[Verdict]): Verdict =
      parts.collectFirst { case u: Unresolved => u }.getOrElse(Built)

  }

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
    else Verdict.all(Throws.evaluated(pkg.stats, excluded).map(judge(_, pkg)))

  private def level(frame: Frame, excluded: Option[Tree]): Verdict =
    templateOf(frame).fold(Verdict.Built) { template =>
      val arguments = Throws.arguments(template.inits).map(judge(_, frame))
      val body = Throws.evaluated(template.body.stats, excluded).map(judge(_, frame))
      Verdict.all(arguments ++ body)
    }

  private def templateOf(frame: Frame): Option[Template] = frame.tree.flatMap {
    case p: Pkg.Object => Some(p.templ)
    case other         => TypeApplications.template(other)
  }

  private def judge(term: Term, owner: Frame): Verdict =
    if (!Throws.only(term, _ => true)) Verdict.Built
    else Verdict.Unresolved(s"construction not resolved: ${label(owner)}")

  private def label(frame: Frame): String =
    if (frame.path.isEmpty) "<empty package>" else frame.path.mkString(".")

}
