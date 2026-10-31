package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation, Natural}
import cardinality.analysis.resolution.ShadowedBuiltins
import cardinality.capacity.{Size, SizeOptics}

import Inhabitation.{Count, Shape}
import MethodAnalysis.{Frame, Resolved, Target}

/** How a constructor's positions are counted: the product of their value spaces.
  *
  * A closed position — one whose method-side shape holds no type parameter — is a value space,
  * never a choice of supplied values: its factor is that space, exact when it is finite. An
  * infinite one keeps the estimate's tier, and where the estimate cannot read it, the tier the size
  * algebra gives the same shape (`ε₀` for a function space between infinite types), so a name
  * collision elsewhere in the sources does not change the answer. The two readings must agree where
  * both exist — finite against finite within the estimate's bound, infinite against infinite — or
  * the position is refused: one of them read a different type. A position the method side cannot
  * read keeps the estimate's reading, unless the method side refused a builtin name the estimate
  * reads as the builtin. A type parameter's slot is left to the shape model, which counts the
  * values the caller supplies. A position mixing type parameters with ground values has no agreed
  * reading — its value space and its supplied choices differ — so it is refused; one with no type
  * parameter that the closed algebra cannot read (a singleton beside a value) takes the estimate's
  * reading or is refused. Only the constructor's own positions are value spaces: an enclosing
  * template's parameters are captures.
  */
final private[cardinality] class ConstructorPositions(
    target: Target,
    resolver: Resolver,
    variables: Map[String, Resolved],
    resolveIn: (Type, Frame) => Resolved
) {

  private enum Position {
    case Shaped
    case Exact(space: Card)
    case Estimated(size: Size)
    case Refused(reason: String)
  }

  import Position.*

  private val read = mutable.Map.empty[(String, String), Position]

  /** Whether the shape model binds this position: always outside a constructor. */
  def leftToShape(param: Term.Param, owner: Frame): Boolean = classify(param, owner) match {
    case Exact(_) | Estimated(_) => false
    case Shaped | Refused(_)     => true
  }

  /** Why the constructor's own positions have no value-space reading, if one has none. */
  def refusals: List[String] =
    target.frame.params.map(classify(_, target.frame)).collect { case Refused(reason) => reason }

  /** The exact factors and the estimate's factors of the constructor's own valued positions. */
  def factors: (List[Card], List[Size]) =
    target.frame.params
      .map(classify(_, target.frame))
      .collect {
        case Exact(space)    => Left(space)
        case Estimated(size) => Right(size)
      }
      .partitionMap(identity)

  // An enclosing template's parameter is a capture of the nested constructor, bound like a
  // method's: only the constructor's own positions are its value spaces.
  private def classify(param: Term.Param, owner: Frame): Position =
    if (!target.constructor || owner.id != target.frame.id) Shaped
    else read.getOrElseUpdate(owner.id -> param.name.value, position(param, owner))

  private def position(param: Term.Param, owner: Frame): Position = param.decltpe match {
    case None                                                      => Shaped
    case Some(named: Type.Name) if variables.contains(named.value) => Shaped
    case Some(tpe)                                                 =>
      lazy val estimate = resolver.reading(target.input).value(tpe)
      resolveIn(tpe, owner) match {
        case Right(shape) if ClosedSpaces.closed(shape) =>
          Natural
            .attempt(ClosedSpaces.space(shape))
            .fold(Refused(_), closedPosition(param.name.value, shape, _, estimate))
        case Right(shape) if ClosedSpaces.mixed(shape) =>
          Refused(
            s"constructor position mixes type parameters with transparent ground values: ${param.name.value}"
          )
        // No type parameter, so a value space: the shape model would count the functions of the
        // position's own value instead.
        case Right(shape) if ClosedSpaces.valued(shape) =>
          estimate
            .map(Estimated(_))
            .getOrElse(
              Refused(s"constructor position has no value-space reading: ${param.name.value}")
            )
        // The estimate reads the builtin the method side refused: it would rest on the same name.
        case Left(reason) if ShadowedBuiltins.shadows(reason) => Refused(reason)
        case _                                                => estimate.fold(Shaped)(Estimated(_))
      }
  }

  private def closedPosition(
      name: String,
      shape: Shape,
      space: Card,
      estimate: Option[Size]
  ): Position =
    space match {
      case Card.Omega =>
        estimate match {
          case None                           => Estimated(ClosedSpaces.estimate(shape))
          case Some(size) if size.hasInfinite => Estimated(size)
          case Some(size)                     => disagree(name, space, size)
        }
      case Card.Exact(value) =>
        estimate.filterNot(agrees(value, _)).fold(Exact(space))(disagree(name, space, _))
    }

  // A literal lies within a finite estimate's capacity. A symbolic value is at least 2^65536, so it
  // fits only a capacity that wide, or an infinite estimate, which may saturate a power that large.
  private def agrees(value: Natural, size: Size): Boolean = value match {
    case Natural.Lit(n) =>
      !size.hasInfinite && SizeOptics.exactFiniteValue
        .getOption(size)
        .fold(BigInt(Size.bits(n)) <= size.finite.bits)(n <= _)
    case _ => size.hasInfinite || size.finite.bits >= Natural.MaxLiteralBits
  }

  private def disagree(name: String, space: Card, size: Size): Position =
    Refused(
      s"constructor position readings disagree: $name (value space ${Count.of(space).render}, " +
        s"estimate ${size.render})"
    )

}
