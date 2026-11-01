package cardinality.analysis.resolution

import scala.annotation.tailrec

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.Resolver

/** One receiver's supplied levels in Scala's linearization order, and their base-class relation.
  *
  * For `class C extends C1 with … with Cn`, L(C) = C, L(Cn) +⃗ … +⃗ L(C1), where +⃗ keeps each
  * element's last occurrence (SLS §5.1.2); a member overrides a matching one of a level it precedes
  * (§5.1.4). That is not the order a breadth-first walk first reaches a level in: with `trait C
  * extends A`, `trait B extends A` and `trait R extends C with B`, L(R) = R, B, C, A, so B's member
  * overrides A's although the walk reaches A from C first. A parent outside the sources is no
  * level: what it may declare is reported elsewhere (`ForeignParents`).
  */
final private[cardinality] class Linearization(levels: List[Frame], resolver: Resolver) {
  private val byId = levels.map(level => level.id -> level).toMap

  /** Each level's supplied parents, in `extends` order. */
  val parents: Map[String, List[String]] = levels.map { level =>
    level.id -> level.parents.flatMap(init =>
      resolver
        .parent(init, level)
        .toOption
        .flatten
        .flatMap(_._1.self)
        .map(_.id)
        .filter(byId.contains)
    )
  }.toMap

  /** The root's linearization, the root first. */
  val order: List[Frame] = levels.headOption.toList
    .flatMap(root => linearize(root.id, Set.empty))
    .flatMap(byId.get)

  /** The levels a level's member is overridden from: those before it in the root's linearization.
    */
  def preceding(id: String): Set[String] =
    order.takeWhile(_.id != id).map(_.id).toSet

  /** A level and every level it extends: what a body written there sees as `this`'s members. */
  def bases(id: String): Set[String] = {
    @tailrec
    def from(pending: List[String], seen: Set[String]): Set[String] = pending match {
      case Nil          => seen
      case next :: rest =>
        val fresh = parents.getOrElse(next, Nil).filterNot(seen)
        from(fresh ++ rest, seen ++ fresh)
    }
    from(List(id), Set(id))
  }

  // A cyclic `extends` is no Scala program; the guard only keeps the reading finite.
  private def linearize(id: String, visiting: Set[String]): List[String] =
    if (visiting(id)) Nil
    else
      id :: parents.getOrElse(id, Nil).foldLeft(List.empty[String]) { (acc, parent) =>
        linearize(parent, visiting + id).filterNot(acc.contains) ++ acc
      }

}
