package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.{Head, Shape}
import cardinality.types.Spelling

/** Literal types (`1`, `"a"`, `true`, `'c'`): each has one value, the constant it spells, never
  * `null` (fixture m3). A goal of one has one implementation and an input adds nothing, its value
  * being a ground literal any body writes: a singleton with no widened binding. Two are one type
  * only for the same constant of the same class (`1` ≠ `1L`, `0.0` ≠ `-0.0` by `Double.equals`),
  * the identity a rigid constructor's argument keys by.
  */
private[cardinality] object LiteralTypes {

  private val Prefix = "literal:"

  // The identity of a literal type: its class and its constant. `null` and `()` spell no type.
  private def spelledConstant(lit: Lit): Option[String] = constant(lit).map(named)

  private def named(constant: (String, String)): String = s"$Prefix${constant._1}:${constant._2}"

  /** The identity of the literal type of a constant, read from its value (a compiled tree keeps a
    * floating zero's sign, which the parser does not).
    */
  def identity(value: Any): Option[String] = valued(value).map(named)

  /** The class of a literal type's value, by its simple name. */
  def className(lit: Lit): Option[String] = constant(lit).map(_._1)

  // A floating constant is the value its spelling rounds to at its own width, compared by its bits
  // as `equals` compares it. The parser keeps a literal's magnitude but not the sign of a zero
  // (`-0.0` reads as `0.0`), and the two zeros are two types: a zero is not read.
  private def constant(lit: Lit): Option[(String, String)] =
    lit match {
      case l: Lit.Float  => Option(l.format.toFloat).filter(_ != 0f).flatMap(valued)
      case l: Lit.Double => Option(l.format.toDouble).filter(_ != 0d).flatMap(valued)
      case l @ (_: Lit.Int | _: Lit.Long | _: Lit.Char | _: Lit.Boolean | _: Lit.String) =>
        valued(l.value)
      case _ => None
    }

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

  /** The one value a literal type has, as a shape. */
  def shape(lit: Lit): Either[String, Shape] = known(lit).map(Shape.Singleton(_, None))

  /** The key a literal type has as an argument of a rigid constructor. */
  def key(lit: Lit): Either[String, Shape] = known(lit).map(nominal)

  private def known(lit: Lit): Either[String, String] =
    spelledConstant(lit).toRight(lit match {
      case _: Lit.Float | _: Lit.Double =>
        s"literal type of a floating zero not read (its sign is not kept): ${Spelling.of(lit)}"
      case _ => s"unsupported literal type: ${Spelling.of(lit)}"
    })

  /** The key of a field name of named tuple syntax: the `String` literal type it spells. */
  def name(field: String): Shape = nominal(s"${Prefix}String:$field")

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
