package cardinality.analysis.inhabitation

import Inhabitation.Shape
import Inhabitation.Shape.*

/** A sufficient, not necessary, certificate for higher-order application.
  *
  * All capabilities must be readable in the atom/product/ordinary-arrow fragment. Functional
  * arguments are first-order, with atomic leaf results. Every higher-order head has an atomic
  * terminal result absent from all callable inputs, including functional argument codomains.
  *
  * A higher-order application therefore cannot occur inside an argument derivation: its result can
  * neither be that derivation's leaf nor feed an intermediate call. Argument derivations use the
  * existing first-order grammar. Arrow introductions add a bounded set of atom/product binders,
  * never another higher-order head. Ordinary currying does not alter this proof, and first-order
  * productive cycles remain cycles in that finite grammar.
  *
  * Checking just the chosen head or functional domains would miss cross-head feedback. Mixed
  * unsupported capabilities are rejected rather than silently discarded.
  */
private[cardinality] object TerminalHigherOrder {

  def uncurry(shape: Shape): (List[Shape], Shape) = shape match {
    case Function(parameters, result) =>
      val (rest, leaf) = uncurry(result)
      (parameters ++ rest, leaf)
    case leaf => (Nil, leaf)
  }

  def certified(environment: List[Shape]): Boolean = {
    val calls = environment.collect { case f: Function => uncurry(f) }
    val higher = calls.filter(_._1.exists(_.isInstanceOf[Function]))
    val readable = environment.forall {
      case f: Function =>
        val (parameters, result) = uncurry(f)
        parameters.forall(input) &&
        (if (parameters.exists(_.isInstanceOf[Function])) result.isInstanceOf[Atom]
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
      parameters.forall(data) && result.isInstanceOf[Atom]
    case shape => data(shape)
  }

  private def data(shape: Shape): Boolean = shape match {
    case Atom(_)         => true
    case Product(fields) => fields.forall(data)
    case _               => false
  }

  private def atoms(shape: Shape): List[Atom] = shape match {
    case atom: Atom                   => List(atom)
    case Product(fields)              => fields.flatMap(atoms)
    case Function(parameters, result) => parameters.flatMap(atoms) ++ atoms(result)
    case _                            => Nil // Readability rejects these before certification.
  }

}
