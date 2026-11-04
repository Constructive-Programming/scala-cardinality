package cardinality.types

import scala.annotation.tailrec

/** Reachability in a finite directed graph, given as each node's successors. */
private[cardinality] object Graphs {

  /** Every node a path from `frontier` reaches, the frontier included. */
  @tailrec def reachable[A](
      graph: Map[A, Set[A]],
      frontier: Set[A],
      seen: Set[A] = Set.empty[A]
  ): Set[A] = {
    val next = frontier -- seen
    if (next.isEmpty) seen
    else reachable(graph, next.flatMap(graph.getOrElse(_, Set.empty)), seen ++ next)
  }

  /** The nodes on a cycle: each one a path of one step or more leads back to. */
  def cyclic[A](graph: Map[A, Set[A]]): Set[A] =
    graph.keySet.filter(node => reachable(graph, graph.getOrElse(node, Set.empty)).contains(node))

}
