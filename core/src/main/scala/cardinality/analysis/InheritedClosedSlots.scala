package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.{ClosedSpaces, Inhabitation}
import cardinality.types.TypeApplications

import Inhabitation.Binding
import MethodAnalysis.Frame

/** Which closed inherited declarations of one template are free choices.
  *
  * A closed declaration — `def n: Int`, `def g(x: Int): Int` — is split as a value of its space, so
  * it must be one member that a subclass supplies, read once. Three readings break that:
  *
  *   - The template fixes its receiver (an object, a given, a final class) or keeps the member in a
  *     field of its own: Scala then requires the member to be implemented, so it is a value the
  *     template's own environment determines, not a choice. It is dropped — as a concrete member
  *     written in the template is — and the field, already bound, is the member.
  *   - An open template or one of its supplied parents implements it: a subclass may still override
  *     it, and how such a member counts is the capability records' decision, so it is refused.
  *   - The template restates it, or reaches it along several parent paths: a class has one member
  *     per name and signature, so a restatement with the same shape and parameter spelling is
  *     dropped, and one with another shape (an overload, or a narrowed type) is refused.
  *
  * A declaration two enclosing templates both keep is two members (`Outer.this.n`, `Inner.this.n`)
  * under one binding, so it is refused; one kept by a single template and dropped by the others is
  * that template's member. A generic parent is bound once, with the type arguments of the template
  * that reached it first, so another template keeps one of its declarations only by refusing it
  * (`borrowed`): its own arguments may differ.
  *
  * A declaration whose shape holds a type parameter is judged for identity only: the same member
  * reached twice — `def g(x: Int): X` in two parents — is one capability, an overload (parameters
  * that read differently) is another, and anything else is refused. Whether a template implements
  * one is the capability records' decision.
  */
final private[cardinality] class InheritedClosedSlots(
    root: Frame,
    ancestors: List[Frame],
    borrowed: Set[String]
) {
  import InheritedClosedSlots.*

  private val kept = mutable.Map.empty[String, List[Member]]

  /** The template and the supplied parents its walk reached: the frames whose declarations it
    * reads.
    */
  val frames: List[Frame] = root :: ancestors

  /** The verdict on one bound declaration of the template or one of its supplied parents, in the
    * order they were bound: the template's own declarations first.
    */
  def verdict(declaring: Frame, name: String, binding: Binding): Verdict =
    judged(declaring, name, binding) match {
      case Keep if borrowed(declaring.id) =>
        Refuse(s"inherited declaration $name is bound with another template's type arguments")
      case verdict => verdict
    }

  private def judged(declaring: Frame, name: String, binding: Binding): Verdict =
    if (declaring.id == root.id) restated(declaring, name, binding)
    else if (fields(root)(name)) Drop
    else if (!ClosedSpaces.closed(binding.shape)) restated(declaring, name, binding)
    else if (fixed(root)) Drop
    else
      frames.find(concrete(_)(name)) match {
        case Some(implementer) =>
          Refuse(
            s"inherited declaration $name is implemented in ${implementer.path.last}; " +
              "implemented members are not modelled as capabilities"
          )
        case None => restated(declaring, name, binding)
      }

  // One member per name and parameter types: a declaration with the shape and parameter spelling of
  // one already kept is that member again, one whose parameters read differently is an overload
  // (kept for an open shape, refused for a closed one, as before), and any other is refused.
  private def restated(declaring: Frame, name: String, binding: Binding): Verdict = {
    val member = Member(binding.shape, spelling(declaring, name))
    val prior = kept.getOrElse(name, Nil)
    if (prior.exists(_.same(member))) Drop
    else if (prior.isEmpty || (member.open && prior.forall(_.overloads(member)))) {
      kept.update(name, prior :+ member)
      Keep
    } else Refuse(s"inherited declaration identity not resolved: $name")
  }

}

private[cardinality] object InheritedClosedSlots {

  enum Verdict {
    case Keep
    case Drop
    case Refuse(reason: String)
  }

  // A kept declaration: its bound shape and the spelling of its parameter types, when one
  // declaration of the name is found to spell them.
  final private case class Member(shape: Inhabitation.Shape, spelling: Option[List[String]]) {

    def same(other: Member): Boolean =
      shape == other.shape && spelling.nonEmpty && spelling == other.spelling

    def open: Boolean = !ClosedSpaces.closed(shape)

    def overloads(other: Member): Boolean = parameters(shape) != parameters(other.shape)

    private def parameters(of: Inhabitation.Shape): List[Inhabitation.Shape] = of match {
      case Inhabitation.Shape.Function(arguments, _) => arguments
      case _                                         => Nil
    }

  }

  // The parameter types the frame's one declaration of a name spells; a field spells none.
  private def spelling(frame: Frame, name: String): Option[List[String]] =
    frame.stats.flatMap {
      case d: Decl.Def if d.name.value == name =>
        List(
          d.paramClauseGroups
            .flatMap(_.paramClauses)
            .flatMap(_.values)
            .map(_.decltpe.fold("")(_.syntax))
        )
      case d: Decl.Val if d.pats.exists(TypeApplications.patVarName.getOption(_).contains(name)) =>
        List(Nil)
      case _ => Nil
    } match {
      case List(parameters)           => Some(parameters)
      case Nil if fields(frame)(name) => Some(Nil)
      case _                          => None
    }

  export Verdict.*

  /** One declaration's verdict over every enclosing template that reaches it. */
  def combine(name: String, verdicts: List[Verdict]): Verdict =
    verdicts.collectFirst { case refused: Refuse => refused }.getOrElse {
      verdicts.count(_ == Keep) match {
        case 0 => Drop
        case 1 => Keep
        case _ => Refuse(s"inherited declaration $name is a member of several enclosing templates")
      }
    }

  // A receiver no subclass can replace: every member it inherits is implemented by now.
  private def fixed(frame: Frame): Boolean = frame.tree.exists {
    case _: Defn.Object | _: Defn.Given | _: Pkg.Object => true
    case d: Defn.Class                                  => d.mods.exists(_.is[Mod.Final])
    case _                                              => false
  }

  // Constructor parameters that are members: `val`/`var` ones, and a case class's first clause.
  private def fields(frame: Frame): Set[String] = frame.tree match {
    case Some(d: Defn.Class) =>
      val clauses = d.ctor.paramClauses.toList
      val caseFields =
        if (d.mods.exists(_.is[Mod.Case])) clauses.headOption.toList.flatMap(_.values) else Nil
      (caseFields ++ clauses
        .flatMap(_.values)
        .filter(_.mods.exists { mod =>
          mod.is[Mod.ValParam] || mod.is[Mod.VarParam]
        })).map(_.name.value).toSet
    case _ => Set.empty
  }

  // The term members a frame defines rather than declares, fields included.
  private def concrete(frame: Frame): Set[String] =
    fields(frame) ++ frame.stats.flatMap {
      case d: Defn.Def    => List(d.name.value)
      case d: Defn.Object => List(d.name.value)
      case d: Defn.Val    => d.pats.flatMap(TypeApplications.patVarName.getOption)
      case d: Defn.Var    => d.pats.flatMap(TypeApplications.patVarName.getOption)
      case _              => Nil
    }

}
