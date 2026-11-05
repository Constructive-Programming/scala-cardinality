package cardinality.analysis.resolution

import scala.meta.XtensionSyntax

import cardinality.analysis.inhabitation.EvidenceAnalysis
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.types.Graphs

import BinderBounds.{Bound, Declared}

/** The binders whose bounds, read together, are not counted.
  *
  * Clauses whose bounds refer back to their binder (E140) are refused before (`Kinds.invalid`).
  *
  *   - An upper bound whose structure holds its binder again, directly or through other bounds (`A
  *     <: P[A]`), describes a type by a fixpoint: every value of A holds a smaller value of A, so
  *     for an invariant final `P` no value exists at all, while `A <: Option[A]` admits `None`.
  *     Counting needs that inhabitation argument, which the shapes do not carry.
  *   - `AnyRef`, `AnyVal` and `Matchable` above a binder admit no type unless every structural
  *     lower bound below it is an instance of that class: `[A >: Flag <: AnyVal]` is accepted by
  *     3.9.0 and every call rejected (E057). The lower bound's class is read from its declaration;
  *     where it is not fixed there, the binder is refused.
  */
private[cardinality] object BoundChecks {

  /** Why each refused binder is refused, by atom. */
  def refusals(
      declared: List[(String, Declared)],
      evidence: List[Shape.Evidence]
  ): Map[String, String] = {
    val recursive = Graphs.cyclic(declared.map((id, bounds) => id -> holds(bounds)).toMap)
    val edges = subtypeEdges(declared, evidence)
    declared.flatMap { (id, bounds) =>
      val syntax = bounds.parameter.syntax
      if (recursive(id)) Some(id -> s"recursive bound not read: $syntax")
      else
        Option
          .when(!classesHold(id, bounds, declared, edges))(
            id -> s"structural lower bound under a class bound requires class analysis: $syntax"
          )
    }.toMap
  }

  /** The proofs an environment makes available: evidence values, alone or inside a product. */
  def available(shapes: List[Shape]): List[Shape.Evidence] = shapes.flatMap {
    case proof: Shape.Evidence => List(proof)
    case Shape.Product(fields) => available(fields)
    case _                     => Nil
  }

  // The binders an upper bound reads a value as, or holds as values of its structure.
  private def holds(bounds: Declared): Set[String] = bounds.bounds.flatMap {
    case Bound.Upper(bound) =>
      EvidenceAnalysis.valueAtoms(bound).collect { case Shape.Atom(id) => id }
    case _ => Nil
  }.toSet

  // Every class bound of the binder holds for each structural lower bound of a binder below it.
  private def classesHold(
      id: String,
      bounds: Declared,
      declared: List[(String, Declared)],
      edges: Map[Shape, Set[Shape]]
  ): Boolean = {
    val classes = bounds.bounds.collect { case Bound.Class(name) => name }
    val atom: Shape = Shape.Atom(id)
    classes.isEmpty || declared.forall { (below, lowers) =>
      !Graphs.reachable(edges, Set(Shape.Atom(below))).contains(atom) ||
      lowers.bounds.forall {
        case Bound.Lower(bound, instances) if !EvidenceAnalysis.endpoint(bound) =>
          classes.forall(name => instances.exists(found => name == "Matchable" || found == name))
        case _ => true
      }
    }
  }

  // The subtype edges between nominal endpoints: atomic bounds and available evidence.
  private def subtypeEdges(
      declared: List[(String, Declared)],
      evidence: List[Shape.Evidence]
  ): Map[Shape, Set[Shape]] = {
    val fromBounds = declared.flatMap { (id, bounds) =>
      bounds.bounds.collect {
        case Bound.Upper(bound) if EvidenceAnalysis.endpoint(bound)    => Shape.Atom(id) -> bound
        case Bound.Lower(bound, _) if EvidenceAnalysis.endpoint(bound) => bound -> Shape.Atom(id)
      }
    }
    val fromEvidence = evidence.filter(proof => endpoints(proof)).flatMap { proof =>
      val forward = proof.left -> proof.right
      if (proof.equality) List(forward, proof.right -> proof.left) else List(forward)
    }
    (fromBounds ++ fromEvidence).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
  }

  private def endpoints(proof: Shape.Evidence): Boolean =
    EvidenceAnalysis.endpoint(proof.left) && EvidenceAnalysis.endpoint(proof.right)

}
