package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation, Natural}
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.resolution.{ShadowedBuiltins, TopTypes}
import cardinality.capacity.{Definition, FinitePart, Size}
import cardinality.types.TypeSubstitution

import Inhabitation.{Count, Shape}
import MethodAnalysis.{Frame, Resolved, Target}

/** How a constructor's positions are counted: the product of their value spaces.
  *
  * A closed position — one whose method-side shape holds no type parameter — is a value space,
  * never a choice of supplied values: its factor is that space, exact when it is finite. An
  * infinite one keeps the estimate's tier. The estimate reads the position where its constructor is
  * written, binding each name as Scala does, and the method side's reading stands only where the
  * two agree — the size algebra reads the method side's type as the estimate reads its own (the
  * same count, or the same capacity width), infinite against infinite — or the position is refused:
  * one of them read a different type. A position the method side cannot read keeps the estimate's
  * reading, unless the method side refused a builtin name the estimate reads as the builtin. A type
  * parameter's slot is left to the shape model, which counts the values the caller supplies. A
  * position mixing type parameters with ground values has no agreed reading — its value space and
  * its supplied choices differ — so it is refused; one with no type parameter that the closed
  * algebra cannot read (a singleton beside a value) takes the estimate's reading. Only the
  * constructor's own positions are value spaces: an enclosing template's parameters are captures.
  *
  * Where the estimate declines a position, no number can stand for it: the shape model counts a
  * position as an argument the constructor is handed — the convention for one whose values depend
  * on a type parameter — so a ground position it counts has the wrong quantity (`|U|^|W|`, not
  * `|W|`), and the method side's own value space would have nothing to agree with. Any refusal of a
  * ground position makes the row `?`. A position over a type parameter stays the shape model's,
  * unless the estimate refused it for a declaration of the sources it could not read whole: a
  * clause it does not follow, a family it does not sum, a cycle it did not solve — a number there
  * would rest on that declaration.
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
    case Declined(reasons: List[String])
  }

  import Position.*

  private val read = mutable.Map.empty[(String, String), Position]

  /** Whether the shape model binds this position: always outside a constructor. */
  def leftToShape(param: Term.Param, owner: Frame): Boolean = classify(param, owner) match {
    case Exact(_) | Estimated(_)           => false
    case Shaped | Refused(_) | Declined(_) => true
  }

  /** Why the constructor's own positions have no value-space reading, if one has none. */
  def refusals: List[String] = own.collect { case Refused(reason) => reason }

  /** The count with the reasons the estimate declined the constructor's own positions for: none
    * leaves it as it is; any makes it `?`, joined to the reasons it already has.
    */
  def declining(count: Count): Count = {
    val declined = own.collect { case Declined(reasons) => reasons }.flatten
    count match {
      case Count.Unresolved(reasons) =>
        Count.Unresolved(ConstructorPositions.join(reasons, declined))
      case _ if declined.nonEmpty => Count.Unresolved(ConstructorPositions.join(Nil, declined))
      case counted                => counted
    }
  }

  /** The exact factors and the estimate's factors of the constructor's own valued positions. */
  def factors: (List[Card], List[Size]) =
    own
      .collect {
        case Exact(space)    => Left(space)
        case Estimated(size) => Right(size)
      }
      .partitionMap(identity)

  private def own: List[Position] = target.frame.params.map(classify(_, target.frame))

  // An enclosing template's parameter is a capture of the nested constructor, bound like a
  // method's: only the constructor's own positions are its value spaces.
  private def classify(param: Term.Param, owner: Frame): Position =
    if (!target.constructor || owner.id != target.frame.id) Shaped
    else read.getOrElseUpdate(owner.id -> param.name.value, position(param, owner))

  private def position(param: Term.Param, owner: Frame): Position = param.decltpe match {
    case None                                                      => Shaped
    case Some(named: Type.Name) if variables.contains(named.value) => Shaped
    case Some(tpe)                                                 =>
      // The estimate stores `Any` as everything and `null` as a value; asked about the position as
      // the implementation model reads it, the two readings meet on the same type. Where the
      // rewrite cannot reach a top, or a container may merge what it identifies, only the method
      // side answers — but a name the estimate cannot bind still declines the position, read as
      // written.
      val context = ResolutionContext(owner, variables, Set.empty)
      val unread = TopTypes.unestimable(tpe, context, resolver)
      val slot = unread.fold(TopTypes.implementationSlot(tpe, context, resolver))(_ => None)
      val reading = resolver.reading(target.input).read(slot.getOrElse(tpe), tpe)
      declined(tpe, reading).getOrElse(
        readPosition(param.name.value, tpe, owner, unread, slot.flatMap(_ => reading.toOption))
      )
  }

  // The reasons the estimate declined a position for, rendered as the row names them, when no number
  // the shape model gives can stand for it.
  private def declined(
      tpe: Type,
      reading: Either[List[Definition.Unbound], Size]
  ): Option[Position] =
    reading.left.toOption.flatMap { reasons =>
      val ground = TypeSubstitution.typeNames(tpe).forall(n => !variables.contains(n))
      Option
        .when(ground || reasons.exists(ConstructorPositions.declarationRefusal))(
          reasons.flatMap(ConstructorPositions.render(tpe, _))
        )
        .filter(_.nonEmpty)
        .map(Declined(_))
    }

  private def readPosition(
      name: String,
      tpe: Type,
      owner: Frame,
      unread: Option[String],
      estimate: Option[Size]
  ): Position = {
    // A value space the estimate is not asked about, or cannot read.
    def unestimated = Refused(
      unread.fold(s"constructor position has no value-space reading: $name")(why =>
        s"constructor position reads $why: $name"
      )
    )
    resolveIn(tpe, owner) match {
      case Right(shape) if ClosedSpaces.closed(shape) =>
        Natural
          .attempt(ClosedSpaces.space(shape))
          .fold(Refused(_), closedPosition(name, shape, _, estimate))
      case Right(shape) if ClosedSpaces.mixed(shape) =>
        Refused(s"constructor position mixes type parameters with transparent ground values: $name")
      // No type parameter, so a value space: the shape model would count the functions of the
      // position's own value instead.
      case Right(shape) if ClosedSpaces.valued(shape) => estimate.fold(unestimated)(Estimated(_))
      // The estimate reads the builtin the method side refused: it would rest on the same name.
      case Left(reason) if ShadowedBuiltins.shadows(reason) => Refused(reason)
      case Left(_) if unread.nonEmpty                       => unestimated
      // A type parameter's slot: the shape model reads an alias's top as `()` itself.
      case _ => estimate.fold(Shaped)(Estimated(_))
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
        estimate.filterNot(agrees(value, shape, _)).fold(Exact(space))(disagree(name, space, _))
    }

  // A literal agrees when the size algebra reads the method side's type as the estimate reads its
  // own: the same count up to 127, where both are exact, and the same capacity width above it, where
  // both round alike. A literal merely within the estimate's capacity is not enough: a position the
  // method side reads through another declaration (`q.X` through an object `q` where the estimate
  // reads the parameter `q`) can hold fewer values than the type the estimate names. A symbolic
  // value is at least 2^65536, so it fits only a capacity that wide, or an infinite estimate, which
  // may saturate a power that large.
  private def agrees(value: Natural, shape: Shape, size: Size): Boolean = value match {
    case Natural.Lit(_) =>
      val image = ClosedSpaces.estimate(shape)
      !size.hasInfinite && !image.hasInfinite && sameWidth(image.finite, size.finite)
    case _ => size.hasInfinite || size.finite.bits >= Natural.MaxLiteralBits
  }

  private def sameWidth(image: FinitePart, estimate: FinitePart): Boolean =
    (image, estimate) match {
      case (FinitePart.Exact(a), FinitePart.Exact(b))          => a == b
      case (_: FinitePart.Exact, _) | (_, _: FinitePart.Exact) => false
      case _                                                   => image.bits == estimate.bits
    }

  private def disagree(name: String, space: Card, size: Size): Position =
    Refused(
      s"constructor position readings disagree: $name (value space ${Count.of(space).render}, " +
        s"estimate ${size.render})"
    )

}

private object ConstructorPositions {

  // The estimate's reasons that name a declaration of the sources it could not read whole.
  private val declarations =
    List("sealed family not summed", "lazy cycle not completed", "cross-package cycle not solved")

  def declarationRefusal(reason: Definition.Unbound): Boolean = reason match {
    case _: Definition.Unbound.Imported  => true
    case Definition.Unbound.Syntax(what) => declarations.exists(what.startsWith)
    case _                               => false
  }

  def render(tpe: Type, reason: Definition.Unbound): Option[String] = reason match {
    case Definition.Unbound.Unknown(name)              => Some(s"unresolved type: $name")
    case Definition.Unbound.Imported(name, provenance) =>
      Some(s"unresolved type: $name ($provenance)")
    case Definition.Unbound.Open(name)             => Some(s"open to implementations: $name")
    case Definition.Unbound.Syntax("a match type") => Some(s"unreduced match type in ${tpe.syntax}")
    case Definition.Unbound.Syntax(what)           => Some(what)
    case _                                         => None
  }

  // A declined position's reason joins the row's unless the row already names the same thing: the
  // same text up to its parenthesised provenance — the one that says where the name comes from is
  // kept — or, for an unreduced match type, the shape model's inert one.
  def join(reasons: List[String], declined: List[String]): List[String] =
    declined.foldLeft(reasons) { (acc, r) =>
      if (r.startsWith("unreduced match type") && acc.exists(_.startsWith("inert match type"))) acc
      else
        acc.indexWhere(a => head(a) == head(r)) match {
          case -1                                       => acc :+ r
          case at if acc(at) == head(r) && r != head(r) => acc.updated(at, r)
          case _                                        => acc
        }
    }

  private def head(reason: String): String = reason.indexOf(" (") match {
    case -1 => reason
    case at => reason.take(at)
  }

}
