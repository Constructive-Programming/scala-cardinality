package cardinality

import Inhabitation.Shape
import Shape.*

/** A deliberately small completeness certificate, not an opaque-sum elimination grammar.
  *
  * There is at most one reachable binary observation, with distinct atomic payloads and at most one
  * Unit alternative. Its argument tuple has exactly one normal form. Payloads have no ambient
  * producers and cannot enable any application, including recalling the observed callable.
  * Consequently the only endomaps of that sum are forwarding and (when present) constant Unit. Case
  * analysis into a uniquely determined unrelated atom is redundant.
  *
  * Reachability is computed before ignoring a callable: unavailable parameters are a proof of
  * non-application, not permission to discard an unsupported diagnostic. Counts saturate at two;
  * only zero/one proofs are used, never a saturated count as an exact answer.
  */
private[cardinality] object OpaqueSumForwarding {
  private case class Call(inputs: List[Atom], output: Shape)

  def count(environment: List[Shape], target: Shape): Option[Int] =
    for {
      calls <- sequence(environment.collect { case f: Function => callable(f) })
      if environment.forall {
        case _: Atom | _: Function => true
        case _                     => false
      }
      if calls.exists(_.output.isInstanceOf[Sum])
      atoms = environment.collect { case a: Atom => a }
      counts = ordinaryCounts(atoms, calls)
      reachable = calls.filter(_.inputs.forall(a => counts(a) > 0))
      sums = reachable.filter(_.output.isInstanceOf[Sum])
      if sums.size <= 1
      if reachable.forall(_.inputs.forall(a => counts(a) == 1))
      answer <- certify(sums.headOption, calls, counts, target)
    } yield answer

  private def certify(
      observation: Option[Call],
      calls: List[Call],
      counts: Map[Atom, Int],
      target: Shape
  ): Option[Int] = observation match {
    case None                                      => ordinary(target, counts)
    case Some(Call(_, output @ Sum(alternatives))) =>
      val payloads = alternatives.collect { case a: Atom => a }
      if (!isolated(alternatives, calls, counts)) None
      else if (target == output)
        Some(1 + alternatives.count(_ == Product(Nil)))
      else
        target match {
          case a: Atom if !payloads.contains(a) && counts(a) <= 1 => Some(counts(a))
          case Sum(Nil)                                           => Some(0)
          case _                                                  => None
        }
    case _ => None
  }

  private def isolated(
      alternatives: List[Shape],
      calls: List[Call],
      counts: Map[Atom, Int]
  ): Boolean = {
    val payloads = alternatives.collect { case a: Atom => a }
    // Expose ALL alternatives together: a stronger rejection than any single branch needs.
    // Any newly enabled application must first consume one of these payloads, so checking the
    // first layer also proves there is no downstream producer/observation closure to explore.
    val exposed = counts ++ payloads.map(_ -> 1)
    val consumers = calls.filter(_.inputs.forall(a => exposed(a) > 0))
    alternatives.size == 2 && alternatives.forall(flat) && payloads.nonEmpty &&
    payloads.distinct.size == payloads.size && payloads.forall(a => counts(a) == 0) &&
    !consumers.exists(_.inputs.exists(payloads.contains))
  }

  private def ordinary(target: Shape, counts: Map[Atom, Int]): Option[Int] =
    target match {
      case a: Atom if counts(a) <= 1 => Some(counts(a))
      case Product(Nil)              => Some(1)
      case Sum(alternatives) if alternatives.size <= 2 && alternatives.forall(flat) =>
        sequence(alternatives.map(ordinary(_, counts))).map(_.sum)
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
      inputs <- sequence(function.parameters.map(parameters)).map(_.flatten)
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

  private def parameters(shape: Shape): Option[List[Atom]] = shape match {
    case a: Atom         => Some(List(a))
    case Product(fields) => sequence(fields.map(parameters)).map(_.flatten)
    case _               => None
  }

  private def sequence[A](values: List[Option[A]]): Option[List[A]] =
    values.foldRight(Option(List.empty[A]))((value, rest) =>
      for {
        item <- value
        items <- rest
      } yield item :: items
    )

}
