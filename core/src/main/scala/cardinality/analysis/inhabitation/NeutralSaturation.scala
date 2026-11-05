package cardinality.analysis.inhabitation

import scala.collection.mutable

import cardinality.types.Traversals

import Inhabitation.Shape
import Shape.*

/** Elimination of supplied callables with a sum or ground result (capabilities C10).
  *
  * A goal whose environment holds `p : P̄ → Σᵢ Yᵢ` may case on `p(t̄)` for any arguments `t̄` it
  * can build. In a pure, total calculus a case commutes with everything around it, and casing twice
  * on one scrutinee is redundant (β and η for sums), so every term equals one that cases on each
  * such neutral exactly once, as soon as its arguments exist — the saturation discipline of focused
  * proof search. The solver therefore adds every new neutral `p(t̄)` to the environment as a value
  * of its sum, once per argument tuple and path (an eliminated neutral leaves a marker), and its
  * split counts the branches independently; answering with `p(t̄)` itself is the η-case of that
  * split and needs no rule of its own. A ground result is a closed sum of literals, eliminated the
  * same way by the uniform closed split.
  *
  * Exactness needs the argument tuples to be finite and known: every argument is first-order data
  * whose inhabitants here are neutral terms — variables, and applications of producers that are not
  * eliminated themselves — and no chain of producers leads from what a neutral's cases bind back to
  * its own arguments (else `p(x)`, `p(step(x))`, … is an unbounded family). Distinct neutral terms
  * are distinct values for some supply of the free producers, so their branches combine freely; an
  * absurd branch is collapsed by the solver's per-node absurd rule, as for any split.
  */
private[cardinality] object NeutralSaturation {

  /** One producer path of a supplied value: the value's path and the path's index among its own. */
  final case class Callee(value: List[String], index: Int)

  /** A neutral term, by structure: two distinct terms never share an identity, whatever the names
    * of the values they read.
    */
  enum Neutral {
    case Variable(path: List[String])
    case Call(callee: Callee, arguments: List[Neutral])
  }

  /** One producer path of a supplied callable: who it is, the name a reason gives it, its type. */
  final case class Path(callee: Callee, name: String, parameters: List[Shape], output: Shape)

  /** The neutrals a goal may eliminate (each with its result), and why each eliminable path that is
    * not saturated here keeps a reason.
    */
  final case class Plan(neutrals: List[(Neutral, Shape)], refused: Map[Callee, String])

  object Plan {
    val empty: Plan = Plan(Nil, Map.empty)
  }

  /** A result the goal eliminates by cases: a nonempty sum or a ground value. */
  def eliminable(output: Shape): Boolean = output match {
    case Sum(alternatives) => alternatives.nonEmpty
    case _: Ground         => true
    case _                 => false
  }

  // Why an argument space is not enumerated.
  private enum Refusal {
    case Unbounded, Unenumerated
  }

  private def reason(refusal: Refusal, path: Path): String = refusal match {
    case Refusal.Unbounded    => s"sum elimination over an unbounded argument space: ${path.name}"
    case Refusal.Unenumerated =>
      s"sum elimination over an argument space not enumerated: ${path.name}"
  }

  /** Every neutral the first-order `values` and the callables' `paths` make available, eliminated or
    * not yet, and the refusals.
    */
  def plan(values: List[(List[String], Shape)], paths: List[Path]): Plan = {
      val terms = new Terms(values, paths.filterNot(p => eliminable(p.output)))
      val graph = new Graph(paths)
      val readings = paths.filter(p => eliminable(p.output)).map { path =>
        val arguments =
          if (graph.unbounded(path)) Left(Refusal.Unbounded)
          else Traversals.sequence(path.parameters.map(terms.of(_, Set.empty))).map(tuples)
        path -> arguments
      }
      Plan(
        readings.collect {
          case (path, Right(found)) =>
            found.map(args => Neutral.Call(path.callee, args) -> path.output)
        }.flatten,
        readings.collect {
          case (path, Left(refusal)) => path.callee -> reason(refusal, path)
        }.toMap
      )
  }

  private def tuples(arguments: List[List[Neutral]]): List[List[Neutral]] =
    arguments.foldRight(List(List.empty[Neutral]))((options, rest) =>
      for {
        first <- options
        others <- rest
      } yield first :: others
    )

  // The neutral terms of a first-order type: its variables, and the applications of the producers
  // that build it. A type met again while it is being enumerated has an unbounded family unless it
  // has no term at all (a cycle with no base builds nothing).
  final private class Terms(
      values: List[(List[String], Shape)],
      producers: List[Path]
  ) {
    private val known = mutable.Map.empty[Shape, Either[Refusal, List[Neutral]]]
    private val cyclic = mutable.Set.empty[Shape]

    def of(shape: Shape, open: Set[Shape]): Either[Refusal, List[Neutral]] = shape match {
      case _: Atom | _: Applied if open(shape) =>
        cyclic += shape
        Right(Nil)
      case _: Atom | _: Applied => known.getOrElse(shape, atomic(shape, open))
      case _                    => Left(Refusal.Unenumerated)
    }

    // Only a reading made outside every cycle is final: one inside reads its ancestor as empty.
    private def atomic(shape: Shape, open: Set[Shape]): Either[Refusal, List[Neutral]] = {
        val variables = values.collect { case (path, `shape`) => Neutral.Variable(path) }
        val applications = producers.filter(_.output == shape).map { producer =>
          Traversals
            .sequence(producer.parameters.map(of(_, open + shape)))
            .map(tuples(_).map(Neutral.Call(producer.callee, _)))
        }
        val found = Traversals.sequence(applications).map(variables ++ _.flatten).flatMap { terms =>
          Either.cond(terms.isEmpty || !cyclic(shape), terms, Refusal.Unbounded)
        }
        if (open.isEmpty) known.update(shape, found)
        found
      }

  }

  // Which first-order types a path's arguments lead to: its result, or what an eliminated result's
  // cases bind; a callable it returns or binds leads to its own result.
  final private class Graph(paths: List[Path]) {

    private val edges: List[(Shape, Shape)] = paths.flatMap { path =>
      val targets = reached(path.output)
      path.parameters.flatMap(reached).flatMap(source => targets.map(source -> _))
    }

    // A path is unbounded when what its cases bind leads back to its own argument types.
    def unbounded(path: Path): Boolean = {
      val arguments = path.parameters.flatMap(reached).toSet
      @annotation.tailrec
      def reach(frontier: Set[Shape], seen: Set[Shape]): Boolean =
        frontier.exists(arguments) || {
          val next = edges.collect { case (from, to) if frontier(from) && !seen(to) => to }.toSet
          next.nonEmpty && reach(next, seen ++ next)
        }
      val start = reached(path.output).toSet
      reach(start, start)
    }

    private def reached(shape: Shape): List[Shape] = shape match {
      case _: Atom | _: Applied => List(shape)
      case Function(_, result)  => reached(result)
      case _: Product | _: Sum  => Shape.children(shape).flatMap(reached)
      case _                    => Nil
    }

  }

}
