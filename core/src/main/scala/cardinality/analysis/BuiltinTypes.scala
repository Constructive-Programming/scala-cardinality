package cardinality.analysis

import cardinality.analysis.inhabitation.{Card, EvidenceAnalysis, Inhabitation}

import Inhabitation.Shape

/** The names the shape model spells itself when no source declares them, by application arity.
  * `Unit` is one construction, `Nothing` none, `Boolean` one bit; `Option` and `Either` count
  * cases. Ground scalars are transparent — literals with a total equality — sized as the JVM stores
  * them, `Float` / `Double` under `equals` (one NaN, two zeros: raw NaN bits are
  * platform-dependent), `String` the free monoid over `Char`: ω.
  */
private[cardinality] object BuiltinTypes {

  /** The builtin a name denotes at this arity, or the reason no builtin answers it. */
  def resolve(name: String, args: List[Shape]): Either[String, Shape] =
    (name.stripPrefix("scala."), args) match {
      case ("Unit", Nil)             => Right(Shape.Product(Nil))
      case ("Nothing" | "Null", Nil) => Right(Shape.Sum(Nil))
      case ("Boolean", Nil)          =>
        Right(Shape.Sum(List(Shape.Product(Nil), Shape.Product(Nil))))
      case (ground, Nil) if spellings.contains(ground) => Right(spellings(ground))
      case ("Option", List(a))                    => Right(Shape.Sum(List(Shape.Product(Nil), a)))
      case ("Either" | "util.Either", List(l, r)) => Right(Shape.Sum(List(l, r)))
      case ("=:=" | "<:<", List(l, r))            => EvidenceAnalysis.resolve(name, l, r)
      case ("Function1", List(l, r))              => Right(Shape.Function(List(l), r))
      case _                                      => Left(s"unresolved type: $name")
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

  /** The ground names, bare: what an import or export could put a different meaning behind. */
  val groundNames: Set[String] = grounds.keySet

  private val spellings: Map[String, Shape] =
    grounds.map((name, size) => name -> (Shape.Ground(name, size): Shape)) ++
      List("java.lang.String", "Predef.String").map(_ -> Shape.Ground("String", Card.Omega))

}
