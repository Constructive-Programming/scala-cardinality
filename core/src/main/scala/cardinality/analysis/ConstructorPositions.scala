package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation, Natural}
import cardinality.analysis.model.{ContextBounds, ResolutionContext}
import cardinality.analysis.resolution.{ShadowedBuiltins, TopTypes}
import cardinality.capacity.{Definition, FinitePart, Size}
import cardinality.types.{Spelling, TypeSubstitution}

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

  private val read = mutable.Map.empty[(String, Int), Position]

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
  // method's: only the constructor's own positions are its value spaces. A position is keyed by its
  // place, since every anonymous parameter has the empty name.
  private def classify(param: Term.Param, owner: Frame): Position =
    if (!target.constructor || owner.id != target.frame.id) Shaped
    else
      read.getOrElseUpdate(owner.id -> owner.params.indexWhere(_ eq param), position(param, owner))

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
      val written = ContextBounds.written(param, tpe, owner.typeParams)
      val reading = resolver.reading(target.input).read(slot.getOrElse(tpe), written)
      val shaped = resolveIn(tpe, owner)
      declined(tpe, reading, shaped.isLeft).getOrElse(
        readPosition(param.name.value, shaped, unread, slot.flatMap(_ => reading.toOption))
      )
  }

  // The reasons the estimate declined a position for, rendered as the row names them, when no number
  // the shape model gives can stand for it. Where the method side cannot read the position either,
  // its own reason stands in the row, and a name the estimate found no declaration of is left to it.
  private def declined(
      tpe: Type,
      reading: Either[List[Definition.Unbound], Size],
      unreadByShape: Boolean
  ): Option[Position] =
    reading.left.toOption.flatMap { reasons =>
      val ground = TypeSubstitution.typeNames(tpe).forall(n => !variables.contains(n))
      val kept =
        if (unreadByShape) reasons.filterNot(ConstructorPositions.namesDeclaration) else reasons
      Option
        .when(ground || reasons.exists(ConstructorPositions.declarationRefusal))(
          kept.flatMap(ConstructorPositions.render(tpe, _))
        )
        .filter(_.nonEmpty)
        .map(Declined(_))
    }

  private def readPosition(
      name: String,
      shaped: Resolved,
      unread: Option[String],
      estimate: Option[Size]
  ): Position = {
    // A value space the estimate is not asked about, or cannot read.
    def unestimated = Refused(
      unread.fold(s"constructor position has no value-space reading: $name")(why =>
        s"constructor position reads $why: $name"
      )
    )
    shaped match {
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
  // method side reads through another declaration (`x.T` through another package's object `x` where
  // the estimate reads a nearer package's `val x`) can hold fewer values than the type the estimate
  // names. A symbolic value is at least 2^65536, so it fits only a capacity that wide, or an infinite
  // estimate, which may saturate a power that large.
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

  // A finite estimate above 127 is a capacity, and readings are compared by its width (`agrees`),
  // so the reason names the width: `≤2^32` would read as agreeing with a value space of 2.
  private def disagree(name: String, space: Card, size: Size): Position = {
    val estimate = size.finite match {
      case FinitePart.Capacity(bits) if !size.hasInfinite => s"a capacity of width $bits bits"
      case _                                              => size.render
    }
    Refused(
      s"constructor position readings disagree: $name (value space ${Count.of(space).render}, " +
        s"estimate $estimate)"
    )
  }

}

private object ConstructorPositions {

  // The estimate's reasons that name a declaration of the sources it could not read whole.
  def declarationRefusal(reason: Definition.Unbound): Boolean = reason match {
    case _: (Definition.Unbound.Imported | Definition.Unbound.Refusal) => true
    case _                                                             => false
  }

  /** Whether the estimate declined for a name it found no declaration of. */
  def namesDeclaration(reason: Definition.Unbound): Boolean = reason match {
    case _: (Definition.Unbound.Unknown | Definition.Unbound.Imported) => true
    case _                                                             => false
  }

  def render(tpe: Type, reason: Definition.Unbound): Option[String] = reason match {
    case Definition.Unbound.Unknown(name)              => Some(s"unresolved type: $name")
    case Definition.Unbound.Imported(name, provenance) =>
      Some(s"unresolved type: $name ($provenance)")
    case Definition.Unbound.Open(name)             => Some(s"open to implementations: $name")
    case Definition.Unbound.Syntax("a match type") =>
      Some(s"unreduced match type: ${Spelling.of(tpe)}")
    case Definition.Unbound.Syntax(what)  => Some(what)
    case Definition.Unbound.Refusal(what) => Some(what)
    case _                                => None
  }

  // A declined position's reason joins the row's unless the row already names the same thing: the
  // same text up to its parenthesised provenance — the one that says where the name comes from is
  // kept — for an unreduced match type, the shape model's inert one, or the same family refused
  // for the same cause in the method side's own words.
  def join(reasons: List[String], declined: List[String]): List[String] =
    declined.foldLeft(reasons) { (acc, r) =>
      if (r.startsWith("unreduced match type") && acc.exists(_.startsWith("inert match type"))) acc
      else if (restated(acc, r)) acc
      else
        acc.indexWhere(a => head(a) == head(r)) match {
          case -1                                       => acc :+ r
          case at if acc(at) == head(r) && r != head(r) => acc.updated(at, r)
          case _                                        => acc
        }
    }

  // The estimate's family refusal (`sealed family not summed: F (why)`) beside the method side's
  // refusal of F for the same cause.
  private def restated(reasons: List[String], reason: String): Boolean = reason match {
    case Unsummed(family, why) =>
      sameCause.exists((method, estimate) =>
        why.startsWith(estimate) && reasons.contains(s"$method: $family")
      )
    case _ => false
  }

  private val Unsummed = """sealed family not summed: (\S+) \((.*)\)""".r

  // A family refusal's cause as each reading words it.
  private val sameCause = List(
    "sealed family has anonymous instances in its file" -> "an anonymous instance in its file",
    "sealed family has a given instance outside the fragment" -> "a given instance in its file",
    "sealed family is extended through an alias" -> "a subtype extends it through an alias",
  )

  private def head(reason: String): String = reason.indexOf(" (") match {
    case -1 => reason
    case at => reason.take(at)
  }

}
