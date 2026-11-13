package cardinality.types

import cats.syntax.all.*
import dev.constructive.eo.data.PSVec
import dev.constructive.eo.optics.Plated
import dev.constructive.eo.schemes.Schemes

/** Generic traversals: of a list of partial results, where the first failure wins and the successes
  * keep their order (cats' `sequence` for the two short-circuit shapes the analyses share), and
  * plates and hylomorphisms over trees.
  */
private[cardinality] object Traversals {

  def sequence[E, A](values: Iterable[Either[E, A]]): Either[E, List[A]] = values.toList.sequence

  /** Every value, when all are defined. */
  def defined[A](values: Iterable[Option[A]]): Option[List[A]] = values.toList.sequence

  /** The plate of a tree type, from each node's children and how to rebuild it from new ones. */
  def plated[S](parts: S => (List[S], List[S] => S)): Plated[S] =
    Plated.fromChildren(s => parts(s)._1, (s, kids) => parts(s)._2(kids))

  /** Unfold a seed into its children, then fold the results back up. */
  def hylo[S, A](expand: S => List[S], merge: (S, List[A]) => A): S => A =
    Schemes
      .hylo[S, A](
        seed => PSVec.from(expand(seed)),
        (seed, kids) => merge(seed, kids.toList)
      )
      .get

}
