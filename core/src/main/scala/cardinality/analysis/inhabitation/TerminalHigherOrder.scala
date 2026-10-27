package cardinality.analysis.inhabitation

import Inhabitation.Shape
import Inhabitation.Shape.*

/** A sufficient, not necessary, certificate for higher-order application.
  *
  * All capabilities must be readable in the atom/product/ordinary-arrow fragment. Functional
  * arguments are first-order, with atomic leaf results. Every higher-order head has an atomic
  * terminal result absent from all callable inputs, including functional argument codomains. A
  * higher-order application therefore cannot occur inside an argument derivation: its result can
  * neither be that derivation's leaf nor feed an intermediate call. Argument derivations use the
  * existing first-order grammar. Arrow introductions add a bounded set of atom/product binders,
  * never another higher-order head. Ordinary currying does not alter this proof, and first-order
  * productive cycles remain cycles in that finite grammar.
  */
private[cardinality] object TerminalHigherOrder {

  def uncurry(shape: Shape): (List[Shape], Shape) = Shape.uncurried.get(shape)

  def certified(environment: List[Shape]): Boolean = {
    val calls = environment.flatMap(Shape.functionPrism.getOption).map(uncurry)
    val higher = calls.filter(_._1.exists(Shape.functionPrism.getOption(_).nonEmpty))
    val readable = environment.forall {
      case f: Function =>
        val (parameters, result) = uncurry(f)
        parameters.forall(input) &&
        (if (parameters.exists(Shape.functionPrism.getOption(_).nonEmpty))
           Shape.atomPrism.getOption(result).nonEmpty
         else data(result))
      case shape => data(shape)
    }
    val inputAtoms = calls.flatMap(_._1).flatMap(atoms).toSet
    readable && higher.nonEmpty && higher.forall {
      case (_, atom: Atom) => !inputAtoms(atom)
      case _               => false
    }
  }

  private def input(shape: Shape): Boolean = shape match {
    case f: Function =>
      val (parameters, result) = uncurry(f)
      parameters.forall(data) && Shape.atomPrism.getOption(result).nonEmpty
    case shape => data(shape)
  }

  private def data(shape: Shape): Boolean = Shape.dataAtoms.get(shape).nonEmpty

  private def atoms(shape: Shape): List[Atom] = Shape.callableAtoms.get(shape)

}
