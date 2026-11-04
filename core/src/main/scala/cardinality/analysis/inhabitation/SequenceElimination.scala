package cardinality.analysis.inhabitation

import Inhabitation.{Binding, Count, Shape}

/** Counting with supplied sequences of opaque elements, one environment per length.
  *
  * A sequence is `Σₙ Eⁿ` and a function out of a sum is a product of functions, so an
  * implementation is a family `(f_n̄)` indexed by the sequences' lengths, `f_n̄` an inhabitant of
  * `Γ(n̄)` — each sequence replaced by that many fresh elements — selected by a total length test
  * (`lengthCompare`, total on an infinite `Seq` too) and bound by positional patterns. With `c(n̄)`
  * the count in `Γ(n̄)`:
  *
  *   - `c(0̄) = 0`: the empty sequences are an input with no answer, so none is total — `0`;
  *   - `c(0̄) ≥ 1` and `c(k̄) ≥ 2` for some `k̄ ≠ 0̄`: weakening carries the length-0 answer to
  *     every length, and two answers at `k̄` stay apart at `k̄ + eᵢ` for a sequence `i` with
  *     `kᵢ ≥ 1` (the valuation that told them apart repeats an element `i` already has), so every
  *     longer length along `i` has its own choice — infinitely many families, which finite programs
  *     bound by ℵ₀ (register D2): `ω`;
  *   - otherwise `?`. Two answers at length 0 alone decide nothing: an element type with no
  *     instance (`A => Nothing` beside an `a: A`) makes every longer environment absurd.
  *
  * Lengths 0 to 2 of at most three sequences are counted (27 environments), each by the whole
  * solver, so an element that is itself a sequence is eliminated the same way.
  */
private[cardinality] object SequenceElimination {

  private val MaxSequences = 3

  private val Lengths = 0 to 2

  def count(
      rest: List[Binding],
      sequences: List[RepeatedArguments.Sequence],
      target: Shape,
      solve: (List[Binding], Shape) => Count
  ): Count =
    if (sequences.size > MaxSequences)
      Count.Unresolved(
        List(
          s"sequence elimination budget exceeded: ${sequences.size} sequences " +
            s"(at most $MaxSequences)"
        )
      )
    else {
      def variant(lengths: List[Int]): Count =
        solve(rest ++ sequences.zip(lengths).flatMap(elements), target)
      val vectors = lengthVectors(sequences.size)
      variant(vectors.head) match {
        case Count.Finite(zero) if zero == 0 => Count.Finite(0)
        case unresolved: Count.Unresolved    => unresolved
        case empty                           =>
          vectors.tail.iterator
            .map(variant)
            .collectFirst {
              case unresolved: Count.Unresolved   => unresolved
              case several if atLeastTwo(several) => Count.Countable
            }
            .getOrElse(Count.Unresolved(List(undecided(atLeastTwo(empty)))))
      }
    }

  private def undecided(emptyChoice: Boolean): String =
    if (emptyChoice)
      "sequence elimination: two answers only for empty sequences up to length 2 " +
        "(a longer environment may be absurd)"
    else "sequence elimination: goal unique up to length 2 but not structurally singleton"

  // The fresh elements standing for one sequence at one length.
  private def elements(sequence: RepeatedArguments.Sequence, length: Int): List[Binding] =
    (1 to length).toList.map(i => Binding(s"${sequence.id}#$i", sequence.shape.element))

  // Every length vector, the empty one first.
  private def lengthVectors(m: Int): List[List[Int]] =
    (1 to m).foldLeft(List(List.empty[Int]))((vectors, _) =>
      for {
        vector <- vectors
        length <- Lengths.toList
      } yield vector :+ length
    )

  private def atLeastTwo(count: Count): Boolean = count match {
    case Count.Finite(value)                    => value >= 2
    case Count.Symbolic(_)                      => true
    case Count.Countable                        => true
    case Count.Bounded(_) | Count.Unresolved(_) => false
  }

}
