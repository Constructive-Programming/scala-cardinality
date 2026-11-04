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
  private def spelledConstant(lit: Lit): Option[String] =
    constant(lit).map((cls, value) => s"$Prefix$cls:$value")

  /** The class of a literal type's value, by its simple name. */
  def className(lit: Lit): Option[String] = constant(lit).map(_._1)

  /** The class of the value a literal type's identity names, by its simple name. */
  def className(id: String): String = id.stripPrefix(Prefix).takeWhile(_ != ':')

  // A floating constant is the value its spelling rounds to at its own width, compared by its bits
  // as `equals` compares it. The parser keeps a literal's magnitude but not the sign of a zero
  // (`-0.0` reads as `0.0`), and the two zeros are two types: a zero is not read.
  private def constant(lit: Lit): Option[(String, String)] =
    lit match {
      case l: Lit.Int   => Some("Int" -> l.value.toString)
      case l: Lit.Long  => Some("Long" -> l.value.toString)
      case l: Lit.Float =>
        val value = l.format.toFloat
        Option.when(value != 0f)("Float" -> java.lang.Float.floatToIntBits(value).toString)
      case l: Lit.Double =>
        val value = l.format.toDouble
        Option.when(value != 0d)("Double" -> java.lang.Double.doubleToLongBits(value).toString)
      case l: Lit.Char    => Some("Char" -> l.value.toInt.toString)
      case l: Lit.Boolean => Some("Boolean" -> l.value.toString)
      case l: Lit.String  => Some("String" -> l.value)
      case _              => None
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

  private def nominal(id: String): Shape = Shape.Applied(Head.Nominal(id), Nil)

}
