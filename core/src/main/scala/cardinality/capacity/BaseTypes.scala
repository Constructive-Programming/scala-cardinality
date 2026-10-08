package cardinality.capacity

import scala.meta.Type

/** The base types the algebra models: the names it resolves without the sources, and what each is
  * worth.
  *
  * One table, so a reference and a report read the same list. `Null` is one value — `null` — and
  * `Any` is the top of the lattice: nothing the analysis can place is above the ε₀ tier and `Any`
  * holds everything, so it sits there. Other top-ish names (`AnyRef`, `Matchable`) stay unmodelled
  * rather than guessed.
  */
private[cardinality] object BaseTypes {

  /** A modelled base type, as a pattern: `case BaseTypes(size) => size` on a `Type`. */
  def unapply(tpe: Type): Option[Size] = tpe match {
    case named: Type.Name => get(named.value)
    case _                => None
  }

  /** The same table, for a caller that is not matching on it. */
  def get(name: String): Option[Size] = sizes.get(name)

  private val sizes: Map[String, Size] = Map(
    "Nothing" -> NothingSize,
    "Unit" -> UnitSize,
    "EmptyTuple" -> UnitSize,
    "Boolean" -> BooleanSize,
    "Byte" -> ByteSize,
    "Short" -> ShortSize,
    "Char" -> CharSize,
    "Int" -> IntSize,
    "Long" -> LongSize,
    "Float" -> FloatSize,
    "Double" -> DoubleSize,
    "Any" -> EffectiveEpsilon0,
    "Null" -> UnitSize,
  )

}
