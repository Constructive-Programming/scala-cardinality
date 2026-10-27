package cardinality.analysis.inhabitation

import cardinality.analysis.*

import Inhabitation.Shape
import Inhabitation.Shape.*

/** Proofs are constraints and identity-preserving transports, not opaque functions or fresh data.
  *
  * Only free-atom endpoints are supported. Structural shapes erase nominal identity, so equality of
  * their representations is not enough to prove Scala type equality. Available atomic equality
  * proofs identify atoms locally; subtype proofs instead add directed, provenance-preserving views.
  * Proofs appearing under a callable or a sum do not become assumptions.
  */
private[cardinality] object EvidenceAnalysis {

  def resolve(name: String, left: Shape, right: Shape): Either[String, Shape] =
    (left, right) match {
      case (_: Atom, _: Atom) => Right(Evidence(left, right, name.endsWith("=:=")))
      case _ => Left(s"unsupported evidence relationship: $name requires free-atom endpoints")
    }

  final case class Context(
      rewrite: Shape => Either[String, Shape],
      views: Shape => List[Shape]
  )

  def context(shapes: List[Shape], target: Shape): Either[String, Context] = {
    for {
      proofs <- AvailableProofs.extract(shapes)
      equality = new EqualityClosure(proofs)
      subtypes = new SubtypeClosure(proofs, equality)
      _ <- SupportedInteractions.validate(shapes :+ target, subtypes.directed)
    } yield Context(shape => rewritten.get(shape)(equality, subtypes), subtypes.reachable)
  }

  private def atomic(shape: Shape): Boolean = Shape.atomPrism.getOption(shape).nonEmpty

  private object AvailableProofs {

    private val endpointError =
      "unsupported evidence relationship: proofs require free-atom endpoints"

    // Only already available proofs participate: do not descend into sums or callables.
    def extract(shapes: List[Shape]): Either[String, List[Evidence]] = {
      val proofs = shapes.flatMap(Shape.evidencePrism.getOption)
      MethodAnalysis.sequence(proofs.map(validate)).map(_ => proofs)
    }

    def validate(proof: Evidence): Either[String, Unit] =
      if (atomic(proof.left) && atomic(proof.right)) Right(())
      else Left(endpointError)

  }

  /** Reflexive transitive closure; equality supplies both directions, subtyping only one. */
  final private class Closure(edges: List[(Shape, Shape)]) {
    def reachable(start: Shape): Set[Shape] = expand(Set(start))

    private def expand(found: Set[Shape]): Set[Shape] = {
      val next = found ++ edges.collect { case (from, to) if found(from) => to }
      if (next == found) found else expand(next)
    }

  }

  final private class EqualityClosure(proofs: List[Evidence]) {

    private val closure = new Closure(
      proofs.filter(_.equality).flatMap(p => List(p.left -> p.right, p.right -> p.left))
    )

    private val representatives = proofs
      .flatMap(p => List(p.left, p.right))
      .distinct
      .map { atom =>
        atom -> closure.reachable(atom).toList.sortBy(_.toString).head
      }
      .toMap

    def canonical(shape: Shape): Shape = representatives.getOrElse(shape, shape)

    def proves(proof: Evidence): Boolean = canonical(proof.left) == canonical(proof.right)
  }

  final private class SubtypeClosure(proofs: List[Evidence], equality: EqualityClosure) {

    private val edges = proofs.filterNot(_.equality).map { proof =>
      equality.canonical(proof.left) -> equality.canonical(proof.right)
    }

    private val closure = new Closure(edges)

    val directed: Boolean = edges.exists { case (from, to) => from != to }

    def reachable(start: Shape): List[Shape] =
      closure.reachable(equality.canonical(start)).toList.sortBy(_.toString)

    def proves(proof: Evidence): Boolean =
      reachable(proof.left).contains(equality.canonical(proof.right))

  }

  private object SupportedInteractions {

    private val complexCheck =
      Shape.cata[Boolean] { (node, kids) =>
        node match {
          case _: Atom | _: Evidence => false
          case Product(_)            => kids.exists(identity)
          case _                     => true
        }
      }

    def validate(shapes: List[Shape], directed: Boolean): Either[String, Unit] =
      if (directed && shapes.exists(complex))
        Left(
          "unsupported subtype evidence interaction: only atomic values and products are supported"
        )
      else Right(())

    private def complex(shape: Shape): Boolean = complexCheck.get(shape)

  }

  private val rewritten =
    Shape.cata[(EqualityClosure, SubtypeClosure) => Either[String, Shape]] {
      (node, kids) => (equality, subtypes) =>
        node match {
          case atom: Atom =>
            Right(equality.canonical(atom))
          case proof: Evidence =>
            rewriteProof(proof, equality, subtypes)
          case existential: Existential =>
            Right(ExistentialInputs.mapAtoms(existential)(atom => equality.canonical(atom)))
          case _ =>
            MethodAnalysis
              .sequence(kids.toList.map(_(equality, subtypes)))
              .map(rebuilt => Shape.rebuild(node, rebuilt)(identity))
        }
    }

  private def rewriteProof(
      proof: Evidence,
      equality: EqualityClosure,
      subtypes: SubtypeClosure
  ): Either[String, Shape] =
    AvailableProofs.validate(proof).flatMap { _ =>
      val proved = if (proof.equality) equality.proves(proof) else subtypes.proves(proof)
      if (proved) Right(Product(Nil))
      else Left("unsupported evidence constraint: no available proof for distinct binders")
    }

}
