package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.inhabitation.{Card, ClosedSpaces as Spaces}
import cardinality.analysis.resolution.{StuckProjections, TopTypes}
import cardinality.capacity.Definition.Unbound
import cardinality.capacity.{NothingSize, Size}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.{Symbol, TypeMemberSymbol}
import tastyquery.Trees.{ClassDef, DefDef, ValDef}
import tastyquery.Types.{AppliedType, ByNameType, Type, TypeRef}

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
  *   - A recursive position, which the method side refuses, takes the estimate's size where its
  *     type is estimable (`estimable`), as the old reading does (`fromEstimate`).
  *   - A type parameter is an atom, or a rigid constructor (`F[_]`, `F[_[_]]`, `Shapes.binder`);
  *     its slot is left to the shape model, which counts the values the caller supplies
  *     (`Pair[A](x: A, y: A)` has four), and for a class in a class those the instance around it
  *     holds, on which it is built (`Receivers`). A position mixing type parameters with ground
  *     values has no agreed reading and is refused.
  *   - A singleton or evidence beside values, with no type parameter, is a value space: beside a
  *     top the method side's, a singleton its one value; else the estimate's size.
  *   - A secondary constructor's row is a method from its parameters to the primary's arguments
  *     (`secondary`).
  *
  * A bounded type parameter's bounds are proofs, as a method's are (`Bounds`). Not read yet, where
  * a position names it: a type parameter declared variant, or a constructor with a bound; a
  * position the method side refuses for another reason than recursion, or whose type is not
  * estimable; one the estimate does not read; evidence beside a top. An anonymous contextual
  * parameter is a position like any other once `Readings` has found search selects it
  * (`Readings#searched`).
  */
final private[compiled] class Constructors(shapes: Shapes, stored: Stored, receivers: Receivers)(
    using Context
) {
  import Constructors.Position
  import Constructors.Position.Shaped
  import Unread.Uncovered

  private val bounds = new Bounds(shapes)

  /** The count of a class's constructor, built on an instance of each class `around` it (outermost
    * first): a position over a type parameter may take a value that instance holds. A secondary
    * constructor's is `secondary`'s.
    */
  def count(constructor: DefDef, around: List[ClassDef]): Either[Unread, Count] =
    if (!Readings.primary(constructor.symbol)) secondary(constructor, around)
    else {
      val owner = constructor.symbol.owner.asClass
      val variables = Shapes.typeParameters(constructor) ++ owner.typeParams
      for {
        (tree, env, held) <- built(constructor, around)
        placed <- Traversals.sequence(
          Shapes.parameters(constructor).map(position(_, tree, env, variables.toSet))
        )
        // The proofs of the bounds of the parameters the positions or the instances around mention.
        proofs <- bounds.proofs(held.map(_.shape) ++ placed.collect { case Shaped(_, s) => s }, env)
      } yield Constructors.product(owner.displayFullName, placed, held, proofs)
    }

  // A secondary constructor's row, a method from its parameters to the primary constructor's
  // arguments: its self-invocation calls another constructor of the class, and every chain of them
  // ends in the primary one, so an implementation chooses those arguments, never a value of the
  // class (the old `SecondaryConstructors`). A by-name argument is its value, a repeated one its
  // sequence. The instance it builds does not exist yet; what a body after the self-invocation
  // declares is another row (`<init#k>`).
  private def secondary(constructor: DefDef, around: List[ClassDef]): Either[Unread, Count] = {
    val self = constructor.symbol.displayFullName
    for {
      (tree, env, held) <- built(constructor, around)
      inputs <- shapes.inputs(Shapes.parameters(constructor), env)
      chosen <- shapes.inputs(Shapes.parameters(tree.rhs.constr), env)
      own = held ++ Shapes.parameters(constructor).lazyZip(inputs).map { (p, shape) =>
        Binding(s"$self.${p.name}", shape, Some(p.name.toString))
      }
      result = Shape.Product(chosen)
      // The proofs of the bounds of the parameters the row mentions.
      proofs <- bounds.proofs(result :: own.map(_.shape), env)
      _ <- StuckProjections
        .refusal(result :: own.map(_.shape), proofs.map(_.shape))
        .map(Unread.Refused(_))
        .toLeft(())
    } yield Readings.count(own ++ proofs, result)
  }

  // The class's tree, the environment a constructor's positions are read in, and what the
  // instances `around` it hold. The primary constructor's copies of the class's type parameters
  // are bound as the class's are, for a secondary constructor's row.
  private def built(
      constructor: DefDef,
      around: List[ClassDef]
  ): Either[Unread, (ClassDef, Shapes.Env, List[Binding])] = {
    val owner = constructor.symbol.owner.asClass
    val copies = Shapes.typeParameters(constructor)
    val variables = copies ++ owner.typeParams
    for {
      tree <- Shapes.tree(owner)
      // The class's type parameters and the constructor's copies of them: each an atom, or a
      // rigid constructor (`Shapes.binder`).
      atoms = owner.typeParams.map(Shapes.binder(owner, _, copies))
      outer <- receivers.atoms(around)
      // A bounded abstract type member of a receiver around it is not read: what the instance
      // around fixes it to is not read beside its bounds.
      _ <- outer
        .map(_._1)
        .collectFirst {
          case member: TypeMemberSymbol if Shapes.bounded(member) =>
            Unread.Uncovered(s"a bounded abstract type member around a constructor: ${member.name}")
        }
        .toLeft(())
      env = Shapes.Env.of(
        outer ++ variables.zip(atoms ++ atoms) ++ Shapes.typeParameters(tree.rhs.constr).zip(atoms),
        constructor.symbol
      )
      // Where the primary constructor has no parameters the row has one value, whatever the
      // instances around hold; else a secondary one may choose the primary's arguments from them.
      held <-
        if (Shapes.parameters(tree.rhs.constr).isEmpty) Right(Nil)
        else receivers.bindings(around, constructor.symbol, env, Set.empty)
    } yield (tree, env, held)
  }

  private def position(
      parameter: ValDef,
      tree: ClassDef,
      env: Shapes.Env,
      variables: Set[Symbol]
  ): Either[Unread, Position] = {
    val name = parameter.name.toString
    val tpe = parameter.tpt.toType
    // A ground position is one whose written type names no type parameter of the class.
    val ground = !World.mentions(tpe, variables)
    def estimate = stored.position(tree, tpe).left.map(Uncovered(_))
    // A position is an input: a top type in it is one value.
    shapes.of(tpe, env).map(TopTypes.polarized(_, false)) match {
      case Left(Unread.Refused(reason))
          if reason.startsWith(Shapes.Recursive) && Constructors.estimable(tpe) =>
        estimate.map(Constructors.fromEstimate(reason, _, ground))
      case Left(Unread.Refused(reason)) =>
        Left(Uncovered(s"a constructor position the method side refuses: $reason"))
      case shaped =>
        for {
          shape <- shaped
          sized <- estimate
          read <- Constructors.read(name, shape, sized, ground)
        } yield read
    }
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
    def closed(shape: Shape) = Spaces.counted(shape) match {
      case Left(reason) => Unresolved(List(reason))
      // A top in an output (`f: Unit => Any`) is ω here and one value in the estimate, which
      // reads every top as an input: the two are not compared, and the tier is the shape's.
      case Right(Card.Omega) if TopTypes.unbounded(shape) => Estimated(Spaces.estimate(shape))
      case Right(space) if size.exists(!agrees(space, shape, _)) =>
        Unresolved(size.toList.map(Spaces.disagreement(name, space, _)))
      case Right(Card.Omega) => Estimated(size.getOrElse(Spaces.estimate(shape)))
      case Right(space)      => Exact(space)
    }
    def unvalued = Unread.Uncovered(s"a constructor position over a singleton or evidence: $name")
    if (ground && declined.nonEmpty) Right(Unresolved(declined))
    else if (Spaces.closed(shape)) Right(closed(shape))
    else if (Spaces.mixed(shape))
      Right(
        Unresolved(
          List(s"constructor position mixes type parameters with transparent ground values: $name")
        )
      )
    // A singleton or evidence beside values and no type parameter: a value space, which the
    // shape model would count the functions of. Beside a top, the method side reads it, a stable
    // path's singleton as its one value; else the estimate's size stands (the old
    // `ConstructorPositions.readPosition`).
    // A value space of the estimate's that the method side's reading of it (`pointed`) contradicts
    // is refused, and so is the estimate's empty space where the method side has none to compare:
    // the two algebras differ on a function from an empty type.
    else if (Spaces.valued(shape)) {
      val pointed = Spaces.pointed(shape).map(closed)
      if (TopTypes.holds(shape)) pointed.toRight(unvalued)
      else
        pointed
          .collect { case refused: Unresolved => refused }
          .orElse(size.filter(pointed.nonEmpty || _ != NothingSize).map(Estimated(_)))
          .toRight(unvalued)
    } else Right(Shaped(name, shape))
  }

  // A recursive position, which the method side refuses, is the estimate's, as the old reading
  // takes it: its reasons other than an unresolved name make the row `?` where one names a
  // declaration it could not read or the position is ground; with no reason at all, and a size
  // that says a value exists (not an absurd function's 0, an intersection's bound), its size is
  // the position's; otherwise the method side's refusal stands.
  private def fromEstimate(reason: String, estimate: Stored.Row, ground: Boolean): Position = {
    val declined = estimate.unbound.filterNot(_.isInstanceOf[Unbound.Unknown]).flatMap(rendered)
    val declaration = estimate.unbound.exists(_.isInstanceOf[Unbound.Refusal])
    if ((ground || declaration) && declined.nonEmpty) Unresolved(declined)
    else
      estimate.size
        .filter(_ => estimate.unbound.isEmpty && !estimate.undecided)
        .fold(Unresolved(List(reason)))(Estimated(_))
  }

  // Whether the old reading hands a position the method side refuses to the estimate: only class
  // references, applied types and by-name types, none of them a top type, `Null` or `Nothing`,
  // through aliases (no union, intersection, singleton, refinement or other form).
  private def estimable(tpe: Type)(using Context): Boolean = tpe.dealias match {
    case by: ByNameType       => estimable(by.resultType)
    case applied: AppliedType =>
      estimable(applied.tycon) && applied.args.forall {
        case arg: Type => estimable(arg)
        case _         => false
      }
    case ref: TypeRef => ref.optSymbol.forall(symbol => !Unestimable(symbol.displayFullName))
    case _            => false
  }

  private val Unestimable = Stored.Tops ++ Set("scala.Null", "scala.Nothing")

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
  // it; any position without a number, or stuck projections no single instantiation reduces
  // (`StuckProjections`, under the bounds' proofs), leave the row `?`.
  private def product(
      self: String,
      positions: List[Position],
      held: List[Binding],
      proofs: List[Binding]
  ): Count = {
    val shaped = positions.collect { case Shaped(name, shape) => name -> shape }
    val stuck = StuckProjections.refusal(held.map(_.shape) ++ shaped.map(_._2), proofs.map(_.shape))
    positions.collect { case Unresolved(reasons) => reasons }.flatten ++ stuck match {
      case Nil =>
        Count.product(
          positions.collect { case Exact(space) => space },
          positions.collect { case Estimated(size) => size },
          Readings.count(
            held ++ proofs ++ shaped.map((name, shape) =>
              Binding(s"$self.$name", shape, Some(name))
            ),
            Shape.Product(shaped.map(_._2))
          )
        )
      case reasons => Count.Unresolved(reasons.distinct)
    }
  }

}
