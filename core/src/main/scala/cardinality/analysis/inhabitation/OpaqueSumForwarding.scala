package cardinality.analysis.inhabitation

import cardinality.types.Traversals

import Inhabitation.Shape
import Shape.*

/** A deliberately small completeness certificate, not an opaque-sum elimination grammar.
  *
  * A context whose sum-producing callables are all unreachable, and whose reachable atoms each have
  * exactly one normal form, builds an atom or a flat sum of atoms in as many ways as those atoms
  * have values: zero or one.
  *
  * Reachability is computed before ignoring a callable: unavailable parameters are a proof of
  * non-application, not permission to discard an unsupported diagnostic. Counts saturate at two;
  * only zero/one proofs are used, never a saturated count as an exact answer.
  */
private[cardinality] object OpaqueSumForwarding {
  private case class Call(inputs: List[Atom], output: Shape)

  def count(environment: List[Shape], target: Shape): Option[Int] =
    for {
      calls <- Traversals.defined(
        environment.flatMap(Shape.functionPrism.getOption).map(callable)
      )
      if environment.forall {
        case _: Atom | _: Function => true
        case _                     => false
      }
      if calls.exists(c => Shape.sumPrism.getOption(c.output).nonEmpty)
      atoms = environment.flatMap(Shape.atomPrism.getOption)
      counts = ordinaryCounts(atoms, calls)
      reachable = calls.filter(_.inputs.forall(a => counts(a) > 0))
      sums = reachable.filter(c => Shape.sumPrism.getOption(c.output).nonEmpty)
      if sums.isEmpty
      if reachable.forall(_.inputs.forall(a => counts(a) == 1))
      answer <- ordinary(target, counts)
    } yield answer

  private def ordinary(target: Shape, counts: Map[Atom, Int]): Option[Int] =
    target match {
      case a: Atom if counts(a) <= 1 => Some(counts(a))
      case Sum(alternatives) if alternatives.size <= 2 && alternatives.forall(flat) =>
        Traversals.defined(alternatives.map(ordinary(_, counts))).map(_.sum)
      case _ => None
    }

  private def ordinaryCounts(atoms: List[Atom], calls: List[Call]): Map[Atom, Int] = {
    val names = (atoms ++ calls.flatMap(_.inputs) ++ calls.collect {
      case Call(_, a: Atom) => a
    }).distinct
    var counts = names.map(_ -> 0).toMap.withDefaultValue(0)
    var changed = true
    while (changed) {
      val next = names
        .map { atom =>
          val applications = calls.collect {
            case Call(inputs, `atom`) => inputs.foldLeft(1)((n, a) => (n * counts(a)).min(2))
          }.sum
          atom -> (atoms.count(_ == atom) + applications).min(2)
        }
        .toMap
        .withDefaultValue(0)
      changed = next != counts
      counts = next
    }
    counts
  }

  // Refuse projections, curried/higher-order outputs, nested sums and all unknown shapes. Those
  // can expose capabilities that this certificate's atomic reachability calculation does not see.
  private def callable(function: Function): Option[Call] =
    for {
      inputs <- Traversals.defined(function.parameters.map(Shape.dataAtoms.get)).map(_.flatten)
      if function.result match {
        case _: Atom | Product(Nil) => true
        case Sum(alternatives)      =>
          alternatives.forall(flat)
        case _ => false
      }
    } yield Call(inputs, function.result)

  private def flat(shape: Shape): Boolean = shape match {
    case _: Atom | Product(Nil) => true
    case _                      => false
  }

}
