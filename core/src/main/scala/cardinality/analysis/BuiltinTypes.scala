package cardinality.analysis

import cardinality.analysis.inhabitation.{Card, EvidenceAnalysis, Inhabitation}
import cardinality.analysis.resolution.TopTypes

import Inhabitation.Shape

/** The standard declarations the shape model reads by their fully qualified names, by application
  * arity. `Unit` is one construction, `Nothing` none, `Boolean` one bit; every other library
  * declaration the model reads (`Option`, `Either`, tuples, functions, sequences) is
  * `StdlibModel`'s. Ground scalars are transparent — literals with a total equality — sized as the
  * JVM stores them, `Float` / `Double` under `equals` (one NaN, two zeros: raw NaN bits are
  * platform-dependent), `String` the free monoid over `Char`: ω. A top type is one value until its
  * place is known: `TopTypes` raises it to ω in an output.
  */
private[cardinality] object BuiltinTypes {

  /** The builtin or model entry a name denotes at this arity (or why it is not counted); None when
    * the name is neither.
    */
  def known(name: String, args: List[Shape]): Option[Either[String, Shape]] =
    scalar(name, args).orElse(StdlibModel.entry(name).map(StdlibModel.read(_, args)))

  private def scalar(name: String, args: List[Shape]): Option[Either[String, Shape]] =
    (name.stripPrefix("scala."), args) match {
      case ("Unit", Nil)                    => Some(Right(Shape.Product(Nil)))
      case (_, Nil) if TopTypes.named(name) => Some(Right(TopTypes.terminal))
      case ("Nothing" | "Null", Nil)        => Some(Right(Shape.Sum(Nil)))
      case ("Boolean", Nil)                 =>
        Some(Right(Shape.Sum(List(Shape.Product(Nil), Shape.Product(Nil)))))
      case (ground, Nil) if spellings.contains(ground) => Some(Right(spellings(ground)))
      case ("=:=" | "<:<", List(l, r))                 => Some(EvidenceAnalysis.resolve(name, l, r))
      case _                                           => None
    }

  private def words(bits: Int): BigInt = BigInt(2).pow(bits)

  private val grounds: Map[String, Card] = Map(
    "Byte" -> Card.exact(words(8)),
    "Short" -> Card.exact(words(16)),
    "Char" -> Card.exact(words(16)),
    "Int" -> Card.exact(words(32)),
    "Long" -> Card.exact(words(64)),
    // 2^24 − 2 NaN bit patterns are one value under `equals`.
    "Float" -> Card.exact(words(32) - words(24) + 3),
    // 2^53 − 2 NaN bit patterns are one value under `equals`.
    "Double" -> Card.exact(words(64) - words(53) + 3),
    "String" -> Card.Omega
  )

  /** The shape a scalar's name reads as (`Int`, `Boolean`, …). */
  def scalarShape(name: String): Shape =
    scalar(name, Nil).flatMap(_.toOption).get

  /** `Byte`'s ground shape: what a byte stream or buffer yields. */
  val byte: Shape = Shape.Ground("Byte", grounds("Byte"))

  private val spellings: Map[String, Shape] =
    grounds.map((name, size) => name -> (Shape.Ground(name, size): Shape)) ++
      List("java.lang.String" -> Shape.Ground("String", Card.Omega))

}
