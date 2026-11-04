package cardinality.types

import cats.syntax.all.*

/** Traversals of a list of partial results: the first failure wins, the successes keep their order
  * (cats' `sequence` for the two short-circuit shapes the analyses share).
  */
private[cardinality] object Traversals {

  def sequence[E, A](values: Iterable[Either[E, A]]): Either[E, List[A]] = values.toList.sequence

  /** Every value, when all are defined. */
  def defined[A](values: Iterable[Option[A]]): Option[List[A]] = values.toList.sequence

}
