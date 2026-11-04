package cardinality.types

import scala.annotation.tailrec

import dev.constructive.eo.optics.Plated

/** Searches over every sub-term (`Plated.universe`) without building the universe first: a
  * pre-order walk over each node's children vector, so `exists` stops at the first match and
  * `select` makes one pass, in the order `Plated.universe(s).flatMap(f)` gives.
  */
private[cardinality] object Universe {

  def exists[S](s: S)(predicate: S => Boolean)(using P: Plated[S]): Boolean = {
    @tailrec def loop(stack: List[S]): Boolean = stack match {
      case Nil    => false
      case h :: t => predicate(h) || loop(push(h, t))
    }
    loop(s :: Nil)
  }

  /** Every sub-term `f` selects, in pre-order: `Plated.universe(s).flatMap(f)` in one pass. */
  def select[S, B](s: S)(f: S => Option[B])(using P: Plated[S]): List[B] = {
    val found = List.newBuilder[B]
    @tailrec def loop(stack: List[S]): Unit = stack match {
      case Nil    => ()
      case h :: t =>
        f(h).foreach(found += _)
        loop(push(h, t))
    }
    loop(s :: Nil)
    found.result()
  }

  // The node's children ahead of the rest of the stack, first child on top.
  private def push[S](node: S, rest: List[S])(using P: Plated[S]): List[S] = {
    val kids = P.childrenVec(node)
    @tailrec def down(i: Int, acc: List[S]): List[S] =
      if (i < 0) acc else down(i - 1, kids(i) :: acc)
    down(kids.length - 1, rest)
  }

}
