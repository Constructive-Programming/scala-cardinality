package cardinality.capacity

import scala.meta.Type

/** The base types the algebra models: the names it resolves without the sources, and what each is
  * worth.
  *
  * One table, so a reference and a report read the same list. `Null` is one value — `null` — and
  * `Any`, `AnyRef` (`Object`) and `Matchable` hold every value a finite program builds: ω, as
  * `String` (v1.md §2). `AnyVal` stays unmodelled.
  */
private[cardinality] object BaseTypes {

  /** A modelled base type, as a pattern: `case BaseTypes(size) => size` on a `Type`. */
  def unapply(tpe: Type): Option[Size] = tpe match {
    case named: Type.Name => get(named.value)
    case _                => None
  }

  /** The same table, for a caller that is not matching on it. */
  def get(name: String): Option[Size] = sizes.get(name)

  /** A throwable the root imports bind: its fully qualified name, and whether the `scala` package
    * aliases it (every one but `IllegalStateException`, which only `java.lang` binds).
    */
  final case class RootThrowable(fqn: String, scalaAlias: Boolean)

  /** The throwables the root imports bind, by simple name. Each is an open class: a value of one
    * may be of any subclass, so the estimate leaves it to its implementations rather than guess a
    * size — the method side reads them as data (register D26).
    */
  val throwables: Map[String, RootThrowable] =
    (List(
      "Throwable",
      "Exception",
      "Error",
      "RuntimeException",
      "NullPointerException",
      "ClassCastException",
      "IndexOutOfBoundsException",
      "UnsupportedOperationException",
      "IllegalArgumentException",
      "NumberFormatException"
    ).map(name => name -> RootThrowable(s"java.lang.$name", scalaAlias = true)) ++ List(
      "IllegalStateException" -> RootThrowable("java.lang.IllegalStateException", false),
      "NoSuchElementException" -> RootThrowable("java.util.NoSuchElementException", true)
    )).toMap

  private val sizes: Map[String, Size] = Map(
    "Nothing" -> NothingSize,
    "Unit" -> UnitSize,
    "EmptyTuple" -> UnitSize,
    "Boolean" -> BooleanSize,
    // A `String` holds what finite programs can type: countably many, the article's `String ~ ω`,
    // and the same space both halves of the report read.
    "String" -> EffectiveOmega,
    // An arbitrary-precision number: countably many finite values, as a `String`.
    "BigInt" -> EffectiveOmega,
    "BigDecimal" -> EffectiveOmega,
    "Byte" -> ByteSize,
    "Short" -> ShortSize,
    "Char" -> CharSize,
    "Int" -> IntSize,
    "Long" -> LongSize,
    "Float" -> FloatSize,
    "Double" -> DoubleSize,
    "Any" -> EffectiveOmega,
    "AnyRef" -> EffectiveOmega,
    "Object" -> EffectiveOmega,
    "Matchable" -> EffectiveOmega,
    "Null" -> UnitSize,
  )

}
