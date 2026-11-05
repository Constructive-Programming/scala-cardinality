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
  * it must be one member that a subclass supplies, read once. A template that fixes its receiver
  * (an object, a given, a final concrete class) or keeps the member in a field of its own
  * determines it, and a template that restates it, or reaches it along several parent paths, has
  * one member per name and signature: neither is read here, so either is refused.
  *
  * An open template's implemented declarations never reach this judge (the receiver tables classify
  * the implementing member instead, capabilities C5). Each template judges its own slots
  * (`Outer.this.n` and `Inner.this.n` are two), keyed as `DeclaredMembers` settled.
  */
final private[cardinality] class InheritedClosedSlots(root: Frame) {
  import InheritedClosedSlots.*

  private val kept = mutable.Set.empty[String]

  /** Why one declaration bound as a member of the template — its own or a supplied parent's, under
    * the member name and the binding key — is refused, in the order they were bound; none: kept.
    */
  def refusal(declaring: Frame, name: String, key: String, binding: Binding): Option[String] =
    if (declaring.id == root.id && plainParameter(root, name)) None
    else if (declaring.id != root.id && determined(name, binding))
      Some(s"inherited declaration determined by its template: $name")
    else Option.unless(kept.add(key))(s"inherited declaration identity not resolved: $name")

  private def determined(name: String, binding: Binding): Boolean =
    ReceiverMembers.fields(root)(name) || ClosedSpaces.closed(binding.shape) && fixed(root)

}

private[cardinality] object InheritedClosedSlots {

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
