package cardinality.analysis.resolution

import cardinality.analysis.inhabitation.Inhabitation.{Head, Shape}

/** Literal types (`1`, `"a"`, `true`, `'c'`): each has one value, the constant it spells, never
  * `null` (fixture m3). A goal of one has one implementation and an input adds nothing, its value
  * being a ground literal any body writes: a singleton with no widened binding. Two are one type
  * only for the same constant of the same class (`1` ≠ `1L`, `0.0` ≠ `-0.0` by `Double.equals`),
  * the identity a rigid constructor's argument keys by.
  */
private[cardinality] object LiteralTypes {

  private val Prefix = "literal:"

  private def named(constant: (String, String)): String = s"$Prefix${constant._1}:${constant._2}"

  /** The identity of the literal type of a constant, read from its value (a compiled tree keeps a
    * floating zero's sign).
    */
  def identity(value: Any): Option[String] = valued(value).map(named)

  private def valued(value: Any): Option[(String, String)] = value match {
    case v: Int     => Some("Int" -> v.toString)
    case v: Long    => Some("Long" -> v.toString)
    case v: Float   => Some("Float" -> java.lang.Float.floatToIntBits(v).toString)
    case v: Double  => Some("Double" -> java.lang.Double.doubleToLongBits(v).toString)
    case v: Char    => Some("Char" -> v.toInt.toString)
    case v: Boolean => Some("Boolean" -> v.toString)
    case v: String  => Some("String" -> v)
    case _          => None
  }

  /** The key of a literal type's singleton, when a binder was instantiated with one. */
  object Keyed {

    def unapply(shape: Shape): Option[Shape] = shape match {
      case Shape.Singleton(id, None) if literal(id) => Some(nominal(id))
      case _                                        => None
    }

  }

  /** Whether a singleton identity is a literal type's. */
  def literal(id: String): Boolean = id.startsWith(Prefix)

  /** Whether a shape is a literal type's one value. */
  def literal(shape: Shape): Boolean = Keyed.unapply(shape).nonEmpty

  /** A literal type's identity as the key of a rigid constructor's argument. */
  def nominal(id: String): Shape = Shape.Applied(Head.Nominal(id), Nil)

}
