package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Shape
import tastyquery.Symbols.*
import tastyquery.Types.*

/** What a type is read in: the shapes its type parameters stand for, the definition measured (what
  * it may see decides an opaque type), the classes being read (a cycle is refused), and whether the
  * type is compared with another as a type (`nominal`), where a type of the module is not its
  * shape. A type parameter `unread` is the reason of a type that names it, and of nothing else: an
  * unused parameter's kind tells nothing. A value `preceding` is built before the measured
  * definition's receiver, which it therefore never is. A class's type parameter `written` with an
  * argument is that type as the site naming the class wrote it, read there: a match type over the
  * parameter is the match on the argument. A written type lambda's parameter `applied` to an
  * argument is that argument, read where it was applied (`Shapes.bound`).
  */
final private[compiled] case class ShapeEnv(
    binders: Map[Symbol, Shape],
    site: Symbol,
    visiting: Set[ClassSymbol] = Set.empty,
    nominal: Boolean = false,
    unread: Map[Symbol, Unread] = Map.empty,
    preceding: Boolean = false,
    written: Map[Symbol, (Type, ShapeEnv)] = Map.empty,
    applied: List[(TypeParamRef, (Type, ShapeEnv))] = Nil
) {

  /** The argument a written type lambda's parameter is applied to, with where it was applied. */
  def argument(ref: TypeParamRef): Option[(Type, ShapeEnv)] =
    applied.collectFirst {
      case (own, at) if (own.binder eq ref.binder) && own.paramNum == ref.paramNum => at
    }

  /** Whether a type parameter is one of the environment's, read or not. */
  def binds(parameter: Symbol): Boolean =
    binders.contains(parameter) || unread.contains(parameter)

  /** The environment with the binders of `outer`, a site that wrote a type read here, beside its
    * own; none where one symbol is bound differently at the two sites.
    */
  def under(outer: ShapeEnv): Option[ShapeEnv] =
    Option.unless(
      outer.binders.exists((symbol, meaning) => binders.get(symbol).exists(_ != meaning))
    )(
      copy(
        binders = outer.binders ++ binders,
        unread = outer.unread ++ unread,
        applied = outer.applied ++ applied
      )
    )

  /** The environment of `outer`, a site that wrote a type lambda, where each of the lambda's
    * parameters is the argument `applied` here: a reading of its body at the site that wrote it.
    */
  def within(outer: ShapeEnv, lambda: TypeLambda, arguments: List[Type]): ShapeEnv =
    copy(
      binders = outer.binders,
      unread = outer.unread,
      written = outer.written,
      applied = lambda.paramRefs.zip(arguments.map(_ -> this)) ++ outer.applied
    )

  /** What a type parameter of the environment stands for, or why it is not read. */
  def binding(parameter: Symbol): Either[Unread, Shape] =
    unread.get(parameter).toLeft(binders(parameter))

}

private[compiled] object ShapeEnv {

  /** An environment of type parameters each read or not (`Shapes.binder`). */
  def of(read: Iterable[(Symbol, Either[Unread, Shape])], site: Symbol): ShapeEnv =
    ShapeEnv(
      read.collect { case (symbol, Right(shape)) => symbol -> shape }.toMap,
      site,
      unread = read.collect { case (symbol, Left(why)) => symbol -> why }.toMap
    )

}
