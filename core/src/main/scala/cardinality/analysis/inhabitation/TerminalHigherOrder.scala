package cardinality.analysis.inhabitation

import dev.constructive.eo.optics.Plated

import Inhabitation.Shape
import Inhabitation.Shape.*

/** A sufficient, not necessary, certificate for higher-order application: capabilities must stay in
  * the atom/product/ordinary-arrow fragment, functional arguments must be first-order with atomic
  * leaf results, and every higher-order head must have an atomic terminal result absent from all
  * callable inputs (including functional argument codomains).
  */
private[cardinality] object TerminalHigherOrder {

  def certified(environment: List[Shape]): Boolean = {
    val calls = environment.flatMap(Shape.functionPrism.getOption).map(Shape.uncurried.get)
    val higher = calls.filter(_._1.exists(Shape.functionPrism.getOption(_).nonEmpty))
    val inputAtoms =
      calls.flatMap(_._1).flatMap(Plated.universe(_)).flatMap(Shape.atomPrism.getOption).toSet
    environment.forall(readable(_, allowFunctionalInput = true)) &&
    higher.nonEmpty && higher.forall {
      case (_, atom: Atom) => !inputAtoms(atom)
      case _               => false
    }
  }

  private def readable(shape: Shape, allowFunctionalInput: Boolean): Boolean = shape match {
    case f: Function =>
      val (parameters, result) = Shape.uncurried.get(f)
      parameters.forall(p =>
        if (allowFunctionalInput) readable(p, allowFunctionalInput = false)
        else Shape.dataAtoms.get(p).nonEmpty
      ) &&
      (if (!allowFunctionalInput || parameters.exists(Shape.functionPrism.getOption(_).nonEmpty))
         Shape.atomPrism.getOption(result).nonEmpty
       else Shape.dataAtoms.get(result).nonEmpty)
    case other => Shape.dataAtoms.get(other).nonEmpty
  }

}
