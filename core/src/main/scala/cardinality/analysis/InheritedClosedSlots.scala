package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.capability.ReceiverMembers
import cardinality.analysis.inhabitation.{ClosedSpaces, Inhabitation}
import cardinality.analysis.resolution.FinalImplementations
import cardinality.types.TypeApplications

import Inhabitation.Binding
import MethodAnalysis.Frame

/** Which closed inherited declarations of one template are free choices.
  *
  * A closed declaration — `def n: Int`, `def g(x: Int): Int` — is split as a value of its space, so
  * it must be one member that a subclass supplies, read once. Two readings break that:
  *
  *   - The template fixes its receiver (an object, a given, a final concrete class) or keeps the
  *     member in a field of its own: Scala then requires the member to be implemented, so it is a
  *     value the template's own environment determines, not a choice. It is dropped — as a concrete
  *     member written in the template is — and the field, already bound, is the member.
  *   - The template restates it, or reaches it along several parent paths: a class has one member
  *     per name and signature, so a restatement under the same key with the same shape and
  *     parameter spelling is dropped, and one with another shape (a narrowed type) is refused.
  *
  * An open template's implemented declarations never reach this judge (the receiver tables classify
  * the implementing member instead, capabilities C5). Each template judges its own slots
  * (`Outer.this.n` and `Inner.this.n` are two), keyed as `DeclaredMembers` settled.
  */
final private[cardinality] class InheritedClosedSlots(root: Frame) {
  import InheritedClosedSlots.*

  private val kept = mutable.Map.empty[String, List[Member]]

  /** The verdict on one declaration bound as a member of the template — its own or a supplied
    * parent's, under the member name and the binding key — in the order they were bound: the
    * template's own declarations first.
    */
  def verdict(declaring: Frame, name: String, key: String, binding: Binding): Verdict =
    if (declaring.id == root.id)
      if (plainParameter(root, name)) Keep else restated(declaring, name, key, binding)
    else if (ReceiverMembers.fields(root)(name)) Drop
    else if (ClosedSpaces.closed(binding.shape) && fixed(root)) Drop
    else restated(declaring, name, key, binding)

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

  // A kept declaration: its bound shape and its parameter types' spelling, when one spells them.
  final private case class Member(shape: Inhabitation.Shape, spelling: Option[List[String]]) {

    // A parameterless method reads as the value it returns, so `def m: Boolean` and `val m:
    // Boolean` restate one member.
    def same(other: Member): Boolean =
      value(shape) == value(other.shape) && spelling.nonEmpty && spelling == other.spelling

    private def value(of: Inhabitation.Shape): Inhabitation.Shape = of match {
      case Inhabitation.Shape.Function(Nil, result) => result
      case other                                    => other
    }

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
      case List(parameters)                           => Some(parameters)
      case Nil if ReceiverMembers.fields(frame)(name) => Some(Nil)
      case _                                          => None
    }

  export Verdict.*

  // No subclass replaces the receiver and it has instances (not `abstract final`): every member it
  // inherits is implemented.
  private def fixed(frame: Frame): Boolean =
    frame.tree.exists(tree => FinalImplementations.isFinal(tree) || tree.is[Pkg.Object])

  // A non-field parameter is a scope value, no member (`SimpleLens(get)` beside `CanGet.get`).
  private def plainParameter(frame: Frame, name: String): Boolean =
    frame.params.filterNot(ReceiverMembers.fieldParams(frame).contains).exists(_.name.value == name)

  /** The term members a frame defines rather than declares, fields included. */
  def concrete(frame: Frame): Set[String] =
    ReceiverMembers.fields(frame) ++ frame.stats.flatMap {
      case d: Defn.Def    => List(d.name.value)
      case d: Defn.Object => List(d.name.value)
      case d: Defn.Val    => TypeApplications.boundNames(d.pats)
      case d: Defn.Var    => TypeApplications.boundNames(d.pats)
      case _              => Nil
    }

}
