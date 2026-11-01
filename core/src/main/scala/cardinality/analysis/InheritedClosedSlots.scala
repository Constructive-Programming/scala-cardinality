package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.{ClosedSpaces, Inhabitation}
import cardinality.analysis.resolution.FinalImplementations
import cardinality.types.TypeApplications

import Inhabitation.Binding
import MethodAnalysis.Frame

/** Which closed inherited declarations of one template are free choices.
  *
  * A closed declaration — `def n: Int`, `def g(x: Int): Int` — is split as a value of its space, so
  * it must be one member that a subclass supplies, read once. Three readings break that:
  *
  *   - The template fixes its receiver (an object, a given, a final concrete class) or keeps the
  *     member in a field of its own: Scala then requires the member to be implemented, so it is a
  *     value the template's own environment determines, not a choice. It is dropped — as a concrete
  *     member written in the template is — and the field, already bound, is the member.
  *   - An open template or one of its supplied parents implements it: a subclass may still override
  *     it, and how such a member counts is the capability records' decision, so it is refused.
  *   - The template restates it, or reaches it along several parent paths: a class has one member
  *     per name and signature, so a restatement under the same key with the same shape and
  *     parameter spelling is dropped, and one with another shape (a narrowed type) is refused.
  *
  * Each template judges its own slots (a parent two enclosing templates extend is two members,
  * `Outer.this.n` and `Inner.this.n`), and a callable's key is the member `DeclaredMembers`
  * settled. A declaration whose shape holds a type parameter is judged for identity only; whether a
  * template implements one is the capability records' decision.
  */
final private[cardinality] class InheritedClosedSlots(root: Frame, ancestors: List[Frame]) {
  import InheritedClosedSlots.*

  private val kept = mutable.Map.empty[String, List[Member]]

  /** The template and the supplied parents its walk reached: the frames whose declarations it
    * reads.
    */
  val frames: List[Frame] = root :: ancestors

  /** The verdict on one declaration bound as a member of the template — its own or a supplied
    * parent's, under the member name and the binding key — in the order they were bound: the
    * template's own declarations first.
    */
  def verdict(declaring: Frame, name: String, key: String, binding: Binding): Verdict =
    if (declaring.id == root.id)
      if (plainParameter(root, name)) Keep else restated(declaring, name, key, binding)
    else if (fields(root)(name)) Drop
    else if (!ClosedSpaces.closed(binding.shape)) restated(declaring, name, key, binding)
    else if (fixed(root)) Drop
    else
      frames.find(concrete(_)(name)) match {
        case Some(implementer) =>
          Refuse(
            s"inherited declaration $name is implemented in ${implementer.path.last}; " +
              "implemented members are not modelled as capabilities"
          )
        case None => restated(declaring, name, key, binding)
      }

  // One member per key: a declaration with the shape and parameter spelling of one already kept
  // under its key is that member again, one whose parameters read differently is an overload (kept
  // for an open shape, refused for a closed one), and any other is refused.
  private def restated(declaring: Frame, name: String, key: String, binding: Binding): Verdict = {
    val member = Member(binding.shape, spelling(declaring, name))
    val prior = kept.getOrElse(key, Nil)
    if (prior.exists(_.same(member))) Drop
    else if (prior.isEmpty || (member.open && prior.forall(_.overloads(member)))) {
      kept.update(key, prior :+ member)
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
      case d: Decl.Val if TypeApplications.binds(name, d.pats) => List(Nil)
      case _                                                   => Nil
    } match {
      case List(parameters)           => Some(parameters)
      case Nil if fields(frame)(name) => Some(Nil)
      case _                          => None
    }

  export Verdict.*

  // A receiver no subclass can replace and that has instances: every member it inherits is
  // implemented by now. An `abstract final` class implements nothing by being final.
  private def fixed(frame: Frame): Boolean =
    frame.tree.exists(tree => FinalImplementations.isFinal(tree) || tree.is[Pkg.Object])

  // A parameter that is no field is a value of the scope, not a member: it neither restates nor
  // implements an inherited declaration of its name (`SimpleLens(get: S => A)` beside `CanGet.get`).
  private def plainParameter(frame: Frame, name: String): Boolean =
    frame.params.exists(_.name.value == name) && !fields(frame)(name)

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

  /** The term members a frame defines rather than declares, fields included. */
  def concrete(frame: Frame): Set[String] =
    fields(frame) ++ frame.stats.flatMap {
      case d: Defn.Def    => List(d.name.value)
      case d: Defn.Object => List(d.name.value)
      case d: Defn.Val    => d.pats.flatMap(TypeApplications.patVarName.getOption)
      case d: Defn.Var    => d.pats.flatMap(TypeApplications.patVarName.getOption)
      case _              => Nil
    }

}
