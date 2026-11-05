package cardinality.analysis.inhabitation

import scala.annotation.tailrec

import Inhabitation.Shape
import Inhabitation.Shape.*

/** Proofs are constraints and identity-preserving transports, not opaque functions or fresh data.
  *
  * Endpoints are free atoms or rigid applications: structural shapes erase nominal identity, so
  * equality of their representations is not enough to prove Scala type equality. Available equality
  * proofs identify endpoints locally. Proofs appearing under a callable or a sum do not become
  * assumptions. A subtype proof, a proof with a structural side and a proof the goal must build are
  * refused.
  */
private[cardinality] object EvidenceAnalysis {

  def resolve(name: String, left: Shape, right: Shape): Either[String, Shape] =
    if (endpoint(left) && endpoint(right)) Right(Evidence(left, right, name.endsWith("=:=")))
    else
      Left(
        s"unsupported evidence relationship: $name requires free-atom or rigid-application endpoints"
      )

  /** A shape a proof may relate by identity: a free atom or a rigid application. */
  def endpoint(shape: Shape): Boolean = shape match {
    case _: Atom | _: Applied => true
    case _                    => false
  }

  /** How the proofs a goal's environment makes available rewrite a shape. */
  def context(shapes: List[Shape]): Either[String, Shape => Either[String, Shape]] = {
    // Only already available proofs participate: do not descend into sums or callables.
    val proofs = shapes.flatMap(Shape.evidencePrism.getOption)
    if (proofs.forall(p => p.equality && endpoint(p.left) && endpoint(p.right))) {
      val equality = new EqualityClosure(proofs)
      Right(shape => equality.settle(shape).flatMap(discharged.get(_)))
    } else
      Left("unsupported evidence relationship: only equality proofs between endpoints are read")
  }

  /** Reflexive transitive closure of both directions of the equality proofs. */
  final private class EqualityClosure(proofs: List[Evidence]) {

    private val edges = proofs.flatMap(p => List(p.left -> p.right, p.right -> p.left))

    private def reachable(start: Shape): List[Shape] = expand(Set(start)).toList.sortBy(_.toString)

    @tailrec private def expand(found: Set[Shape]): Set[Shape] = {
      val next = found ++ edges.collect { case (from, to) if found(from) => to }
      if (next == found) found else expand(next)
    }

    private val representatives = proofs
      .flatMap(p => List(p.left, p.right))
      .distinct
      .map(endpoint => endpoint -> reachable(endpoint).head)
      .toMap

    def canonical(shape: Shape): Shape = representatives.getOrElse(shape, shape)

    // A representative may be an application whose own arguments another proof rewrites: rewrite
    // until nothing changes. Each step replaces an occurrence by its class's representative, which
    // depends on the class alone, so a rewriting that ends unfolds every class at most once along a
    // chain; one still changing after a step per class has met a class again and never ends
    // (`A =:= F[A]`, an equation no finite type solves for every F) — refused.
    def settle(shape: Shape): Either[String, Shape] =
      settled(shape, representatives.values.toSet.size + 1)

    @tailrec private def settled(shape: Shape, fuel: Int): Either[String, Shape] = {
      val next = canonicalized.get(shape)(this)
      if (next == shape) Right(shape)
      else if (fuel == 0)
        Left("unsupported evidence relationship: an equality proof keeps unfolding")
      else settled(next, fuel - 1)
    }

  }

  /** The atoms a shape holds as values: an application's arguments are keys, not values. */
  def valueAtoms(shape: Shape): List[Shape] = shape match {
    case atom: Atom => List(atom)
    case _: Applied => Nil
    case other      => Shape.children(other).flatMap(valueAtoms)
  }

  private val canonicalized =
    Shape.cata[EqualityClosure => Shape] { (node, kids) => equality =>
      node match {
        case atom: Atom       => equality.canonical(atom)
        case applied: Applied => equality.canonical(Shape.rebuild(applied, kids)(_(equality)))
        case _                => Shape.rebuild(node, kids)(_(equality))
      }
    }

  private val discharged =
    Shape.cata[Either[String, Shape]] { (node, kids) =>
      node match {
        case _: Evidence => Left("unsupported evidence constraint: a proof as a goal")
        case _           => Shape.tryRebuild(node, kids)
      }
    }

}
