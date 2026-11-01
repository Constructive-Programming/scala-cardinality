package cardinality.types

/** Traversals of a list of partial results: the first failure wins, the successes keep their order.
  */
private[cardinality] object Traversals {

  def sequence[E, A](values: Iterable[Either[E, A]]): Either[E, List[A]] =
    values.foldRight(Right(Nil): Either[E, List[A]])((v, acc) => v.flatMap(a => acc.map(a :: _)))

  /** Every value, when all are defined. */
  def defined[A](values: Iterable[Option[A]]): Option[List[A]] =
    values.foldRight(Option(List.empty[A]))((v, acc) => v.flatMap(a => acc.map(a :: _)))

}
