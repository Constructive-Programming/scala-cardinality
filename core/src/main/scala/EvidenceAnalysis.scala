package cardinality

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
    val proofs = shapes.collect { case proof: Evidence => proof }
    val atoms = proofs.flatMap(p => List(p.left, p.right)).distinct
    val equalities = proofs.filter(_.equality)
    def equivalent(start: Shape): Set[Shape] = {
      def expand(found: Set[Shape]): Set[Shape] = {
        val next = found ++ equalities.flatMap { proof =>
          if (found(proof.left) || found(proof.right)) List(proof.left, proof.right) else Nil
        }
        if (next == found) found else expand(next)
      }
      expand(Set(start))
    }
    val representatives = atoms.map { atom =>
      atom -> equivalent(atom).toList.sortBy(_.toString).head
    }.toMap
    def canonical(shape: Shape): Shape = representatives.getOrElse(shape, shape)
    val edges = proofs.filterNot(_.equality).map(p => canonical(p.left) -> canonical(p.right))
    def reachable(start: Shape): List[Shape] = {
      def expand(found: Set[Shape]): Set[Shape] = {
        val next = found ++ edges.collect { case (from, to) if found(from) => to }
        if (next == found) found else expand(next)
      }
      expand(Set(canonical(start))).toList.sortBy(_.toString)
    }
    val directed = edges.exists { case (from, to) => from != to }
    if (proofs.exists(p => !atomic(p.left) || !atomic(p.right)))
      Left("unsupported evidence relationship: proofs require free-atom endpoints")
    else if (directed && (shapes :+ target).exists(complex))
      Left(
        "unsupported subtype evidence interaction: only atomic values and products are supported"
      )
    else {
      def rewrite(shape: Shape): Either[String, Shape] = shape match {
        case atom: Atom                                                  => Right(canonical(atom))
        case Evidence(left, right, _) if !atomic(left) || !atomic(right) =>
          Left("unsupported evidence relationship: proofs require free-atom endpoints")
        case Evidence(left, right, equality) =>
          val proved =
            if (equality) canonical(left) == canonical(right)
            else reachable(left).contains(canonical(right))
          if (proved) Right(Product(Nil))
          else Left("unsupported evidence constraint: no available proof for distinct binders")
        case Product(fields) =>
          MethodAnalysis.sequence(fields.map(rewrite)).map(Product(_))
        case Sum(alternatives) =>
          MethodAnalysis.sequence(alternatives.map(rewrite)).map(Sum(_))
        case Function(parameters, result) =>
          for {
            args <- MethodAnalysis.sequence(parameters.map(rewrite))
            output <- rewrite(result)
          } yield Function(args, output)
        case Repeated(element)        => rewrite(element).map(Repeated(_))
        case existential: Existential =>
          Right(ExistentialInputs.mapAtoms(existential)(atom => canonical(atom)))
        case Singleton(id, underlying) =>
          underlying match {
            case Some(value) => rewrite(value).map(shape => Singleton(id, Some(shape)))
            case None        => Right(Singleton(id, None))
          }
      }
      Right(Context(rewrite, reachable))
    }
  }

  private def atomic(shape: Shape): Boolean = shape.isInstanceOf[Atom]

  private def complex(shape: Shape): Boolean = shape match {
    case _: Atom | _: Evidence => false
    case Product(fields)       => fields.exists(complex)
    case _                     => true
  }

}
