package cardinality.compiled

import cardinality.capacity.Definition.Unbound
import cardinality.capacity.{
  BaseTypes,
  BooleanSize,
  EffectiveEpsilon0,
  EffectiveOmega,
  NothingSize,
  Size,
  UnitSize
}

import Stored.Held

/** The size algebra of the stored side (docs/site/arithmetic/counting-data.md): products, sums and
  * arrows of what types hold, and the standard library's types it models (`standard`).
  */
private[compiled] object Algebra {

  // The standard library's types the algebra models, by the class the compiler bound.
  def standard(name: String, sizes: List[Held], input: Boolean): Either[String, Held] =
    nullary(name, sizes, input).map(Right(_)).getOrElse(applied(name, sizes, input))

  // The modelled classes without type arguments.
  private def nullary(name: String, sizes: List[Held], input: Boolean): Option[Held] =
    (name, sizes) match {
      // As the method side reads an input: a top type is one value, and `Null` is empty, since
      // the method side counts no `null` (v1.md §2).
      case (_, Nil) if input && Tops(name) => Some(Held(UnitSize))
      case ("scala.Null", Nil) if input    => Some(Held(NothingSize))
      case (Based(size), Nil)              => Some(Held(size))
      // A throwable of the root imports is an open class: any subclass's values are its own.
      case (Throwables(simple), Nil) => Some(Held(EffectiveOmega, List(Unbound.Open(simple))))
      case ("scala.AnyVal", Nil)     => Some(Held(EffectiveOmega, List(Unbound.Unknown("AnyVal"))))
      case _                         => None
    }

  // The modelled classes applied to type arguments, and the tuples.
  private def applied(name: String, sizes: List[Held], input: Boolean): Either[String, Held] =
    (name, sizes) match {
      case _ if name.matches("""scala\.(Tuple\d+|\*:)""") => Right(product(sizes))
      case (_, parameters :+ result) if name.matches("""scala\.(Context)?Function\d+""") =>
        Right(arrow(result, product(parameters).domain))
      case ("scala.Option", List(element))                   => Right(element.map(UnitSize + _))
      case ("scala.util.Either", List(left, right))          => Right(left.zip(right)(_ + _))
      case ("scala.collection.immutable.Set", List(element)) =>
        Right(element.map(e => lifted(input)(e, BooleanSize.pow(e))))
      case ("scala.collection.immutable.Map", List(key, value)) =>
        Right(key.zip(value)((k, v) => lifted(input)(k, (v + UnitSize).pow(k))))
      case ("scala.PartialFunction", List(key, value)) =>
        Right(key.domain.zip(value)((k, v) => (v + UnitSize).pow(k)))
      case (_, List(element)) if Sequences(name) =>
        Right(element.map(linear(input)))
      // The finite lists and the infinite streams together, collapsed to the highest tier.
      case (_, List(element)) if Lazy(name) =>
        Right(element.map { e =>
          if (e == NothingSize) UnitSize else (EffectiveOmega + e.pow(EffectiveOmega)).widen
        })
      case _ => Left(s"a class the algebra does not model: $name")
    }

  def product(sizes: List[Held]): Held = sizes.foldLeft(Held(UnitSize))(_.zip(_)(_ * _))
  def sum(sizes: List[Held]): Held = sizes.foldLeft(Held(NothingSize))(_.zip(_)(_ + _))

  // A function type is codomain ^ domain; from an empty domain the algebra says 0, though the
  // absurd function is a value.
  def arrow(codomain: Held, domain: Held): Held =
    if (domain.size == NothingSize)
      domain.zip(codomain)((_, _) => NothingSize).copy(undecided = true)
    else codomain.zip(domain)(_.pow(_))

  // A linear collection over an empty element type holds the empty one; over any other it is at
  // least ω and, by its one-element term, its element's tier.
  def linear(saturating: Boolean)(element: Size): Size =
    if (element == NothingSize) UnitSize else lifted(saturating)(element, EffectiveOmega)

  // A finite collection holds each one-element collection, so it is never below its element's
  // tier; the method side saturates at ω, and so does a reading compared with it.
  private def lifted(saturating: Boolean)(element: Size, collection: Size): Size =
    if (!saturating && rises(element, collection)) Size.order.max(collection, EffectiveEpsilon0)
    else collection

  // Whether a collection above one value holds an element beyond ω's tier.
  private def rises(element: Size, collection: Size): Boolean =
    collection.larger(UnitSize) && element.epsilon > 0

  // The base types of `BaseTypes` that are classes, by fully qualified name.
  private val Based: Map[String, Size] =
    (List("Unit", "Boolean", "Byte", "Short", "Char", "Int", "Long", "Float", "Double", "Any")
      .++(List("Matchable", "Null"))
      .map(name => s"scala.$name" -> name) ++ List(
      "java.lang.String" -> "String",
      "java.lang.Object" -> "Object",
      "scala.math.BigInt" -> "BigInt",
      "scala.math.BigDecimal" -> "BigDecimal"
    )).flatMap((qualified, name) => BaseTypes.get(name).map(qualified -> _)).toMap

  val Tops =
    Set("scala.Any", "scala.AnyVal", "java.lang.Object", "scala.Matchable")

  // The throwables the root imports bind, by fully qualified name: the name a row gives each.
  private val Throwables: Map[String, String] =
    BaseTypes.throwables.map((simple, fqn) => fqn -> simple)

  private val Sequences: Set[String] =
    Set("List", "Vector", "Seq", "IndexedSeq").map(
      "scala.collection.immutable." + _
    ) + "scala.Array"

  private val Lazy: Set[String] = Set("LazyList", "Stream").map("scala.collection.immutable." + _)

}
