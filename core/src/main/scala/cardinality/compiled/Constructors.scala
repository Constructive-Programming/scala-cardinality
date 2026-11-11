package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.inhabitation.{Card, ClosedSpaces as Spaces, Inhabitation}
import cardinality.analysis.resolution.TopTypes
import cardinality.capacity.Definition.Unbound
import cardinality.capacity.Size
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.Symbol
import tastyquery.Trees.{ClassDef, DefDef, ValDef}

/** A class's row: the count of its primary constructor, the product of its positions' value spaces
  * (slice S3; v1.md §2, "constructor = a method").
  *
  *   - A position whose shape holds no type parameter is a value space, never a choice of supplied
  *     values: its factor is that space, exact where it is finite, the size algebra's tier where it
  *     is not.
  *   - The stored-size estimate reads the same position, and the method side's reading stands only
  *     where the two agree (the same count, or the same capacity width; infinite against infinite):
  *     the two algebras differ on a function from an empty type, and such a position is refused.
  *     Where the estimate has a reason not to size a ground position (an open class), no number
  *     stands for it and the row is `?` with that reason; where its only reason is a type
  *     parameter, the method side's reading stands alone.
  *   - A type parameter's slot is left to the shape model, which counts the values the caller
  *     supplies (`Pair[A](x: A, y: A)` has four). A position mixing type parameters with ground
  *     values has no agreed reading and is refused.
  *
  * Not read yet: a position the method side refuses (a recursive type), which takes the estimate's
  * tier; one the estimate does not read; one over a singleton or evidence.
  */
final private[compiled] class Constructors(shapes: Shapes, stored: Stored)(using Context) {
  import Constructors.Position
  import Unread.Uncovered

  /** The count of a class's primary constructor. */
  def count(constructor: DefDef): Either[Unread, Count] = {
    val owner = constructor.symbol.owner.asClass
    val self = owner.displayFullName
    val binders = constructor.paramLists.flatMap(_.toOption.toList.flatten)
    // The class's type parameters and the constructor's copies of them, each one atom.
    val atoms = binders.map(binder => Shape.Atom(s"$self[${binder.name}]"))
    val variables = binders.map(_.symbol: Symbol) ++ owner.typeParams
    val env = Shapes.Env(variables.zip(atoms ++ atoms).toMap, constructor.symbol)
    val parameters = Shapes.parameters(constructor)
    for {
      tree <- Shapes.tree(owner)
      _ <- Traversals.sequence(binders.map(Readings.unbounded))
      _ <- Readings.plainInputs(parameters)
      placed <- Traversals.sequence(parameters.map(position(_, tree, env, variables.toSet)))
    } yield Constructors.product(self, placed)
  }

  private def position(
      parameter: ValDef,
      tree: ClassDef,
      env: Shapes.Env,
      variables: Set[Symbol]
  ): Either[Unread, Position] = {
    val name = parameter.name.toString
    val tpe = parameter.tpt.toType
    for {
      // A position is an input: a top type in it is one value. One the method side refuses takes
      // the estimate's tier, which is not read yet.
      shape <- shapes.of(tpe, env).map(TopTypes.polarized(_, false)).left.map {
        case Unread.Refused(reason) =>
          Uncovered(s"a constructor position the method side refuses: $reason")
        case uncovered => uncovered
      }
      estimate <- stored.position(tree, tpe).left.map(Uncovered(_))
      // A ground position is one whose written type names no type parameter of the class.
      read <- Constructors.read(name, shape, estimate, !World.mentions(tpe, variables))
    } yield read
  }

}

private[compiled] object Constructors {

  // One position: left to the shape model, a value space (exact, or a tier), or why the row has
  // no number.
  private enum Position {
    case Shaped(name: String, shape: Shape)
    case Exact(space: Card)
    case Estimated(size: Size)
    case Unresolved(reasons: List[String])
  }

  import Position.*

  private def read(
      name: String,
      shape: Shape,
      estimate: Stored.Row,
      ground: Boolean
  ): Either[Unread, Position] = {
    val declined = estimate.unbound.flatMap(rendered)
    // The estimate's size is compared only where it gave no reason: with one (a type parameter),
    // the method side stands alone.
    val size = estimate.size.filter(_ => estimate.unbound.isEmpty)
    if (ground && declined.nonEmpty) Right(Unresolved(declined))
    else if (Spaces.closed(shape))
      Right(Spaces.counted(shape) match {
        case Left(reason) => Unresolved(List(reason))
        // A top in an output (`f: Unit => Any`) is ω here and one value in the estimate, which
        // reads every top as an input: the two are not compared, and the tier is the shape's.
        case Right(Card.Omega) if TopTypes.unbounded(shape) => Estimated(Spaces.estimate(shape))
        case Right(space) if size.exists(!agrees(space, shape, _)) =>
          Unresolved(size.toList.map(Spaces.disagreement(name, space, _)))
        case Right(Card.Omega) => Estimated(size.getOrElse(Spaces.estimate(shape)))
        case Right(space)      => Exact(space)
      })
    else if (Spaces.mixed(shape))
      Right(
        Unresolved(
          List(s"constructor position mixes type parameters with transparent ground values: $name")
        )
      )
    else if (Spaces.valued(shape))
      Left(Unread.Uncovered(s"a constructor position over a singleton or evidence: $name"))
    else Right(Shaped(name, shape))
  }

  // The estimate's reason as a method row words it.
  private def rendered(reason: Unbound): Option[String] = reason match {
    case Unbound.Unknown(name) => Some(s"unresolved type: $name")
    case Unbound.Open(name)    => Some(s"open to implementations: $name")
    case Unbound.Syntax(what)  => Some(what)
    case Unbound.Refusal(what) => Some(what)
    // A type parameter or an abstract member is what the instance chooses, no reason of a row.
    case _: (Unbound.Parameter | Unbound.HigherKinded | Unbound.Member | Unbound.Imported) => None
  }

  // Whether the estimate agrees with the value space (`ClosedSpaces.agrees`): infinite against
  // infinite.
  private def agrees(space: Card, shape: Shape, size: Size): Boolean = space match {
    case Card.Omega        => size.hasInfinite
    case Card.Exact(value) => Spaces.agrees(value, shape, size)
  }

  // The product of the positions' value spaces with the shape model's count of the slots left to
  // it; any position without a number leaves the row `?`.
  private def product(self: String, positions: List[Position]): Count =
    positions.collect { case Unresolved(reasons) => reasons }.flatten match {
      case Nil =>
        val shaped = positions.collect { case Shaped(name, shape) => name -> shape }
        Count.product(
          positions.collect { case Exact(space) => space },
          positions.collect { case Estimated(size) => size },
          Inhabitation.count(
            shaped.map((name, shape) => Binding(s"$self.$name", shape, Some(name))),
            Shape.Product(shaped.map(_._2)),
            maxDepth = Readings.Depth
          )
        )
      case reasons => Count.Unresolved(reasons.distinct)
    }

}
