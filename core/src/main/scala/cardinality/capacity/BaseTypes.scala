package cardinality.capacity

/** The base types the algebra models: the names it resolves without the sources, and what each is
  * worth.
  *
  * One table, so a reference and a report read the same list. `Null` is one value — `null` — and
  * `Any`, `AnyRef` (`Object`) and `Matchable` hold every value a finite program builds: ω, as
  * `String` (v1.md §2). `AnyVal` stays unmodelled.
  */
private[cardinality] object BaseTypes {

  /** A modelled base type's size, by its name. */
  def get(name: String): Option[Size] = sizes.get(name)

  /** The throwables the root imports bind, by simple name, with their fully qualified names. Each
    * is an open class: a value of one may be of any subclass, so the estimate leaves it to its
    * implementations rather than guess a size — the method side reads them as data (register D26).
    */
  val throwables: Map[String, String] =
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
    ).map(name => name -> s"java.lang.$name") ++ List(
      "IllegalStateException" -> "java.lang.IllegalStateException",
      "NoSuchElementException" -> "java.util.NoSuchElementException"
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
