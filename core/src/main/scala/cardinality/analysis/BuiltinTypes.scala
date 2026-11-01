package cardinality.analysis

import cardinality.analysis.inhabitation.{Card, EvidenceAnalysis, Inhabitation}
import cardinality.analysis.resolution.TopTypes

import Inhabitation.Shape

/** The names the shape model spells itself when no source declares them, by application arity.
  * `Unit` is one construction, `Nothing` none, `Boolean` one bit; `Option` and `Either` count
  * cases, a `TupleN` is the tuple it spells and a `FunctionN` the function. Ground scalars are
  * transparent — literals with a total equality — sized as the JVM stores them, `Float` / `Double`
  * under `equals` (one NaN, two zeros: raw NaN bits are platform-dependent), `String` the free
  * monoid over `Char`: ω. The top types are the terminal type, `Unit`'s shape (`TopTypes`).
  */
private[cardinality] object BuiltinTypes {

  /** The builtin a name denotes at this arity, or the reason no builtin answers it. */
  def resolve(name: String, args: List[Shape]): Either[String, Shape] =
    (name.stripPrefix("scala."), args) match {
      case ("Unit", Nil)                              => Right(Shape.Product(Nil))
      case (top, Nil) if TopTypes.named(top).nonEmpty => Right(TopTypes.terminal)
      case ("Nothing" | "Null", Nil)                  => Right(Shape.Sum(Nil))
      case ("Boolean", Nil)                           =>
        Right(Shape.Sum(List(Shape.Product(Nil), Shape.Product(Nil))))
      case (ground, Nil) if spellings.contains(ground) => Right(spellings(ground))
      case ("Option", List(a))                    => Right(Shape.Sum(List(Shape.Product(Nil), a)))
      case ("Either" | "util.Either", List(l, r)) => Right(Shape.Sum(List(l, r)))
      case ("=:=" | "<:<", List(l, r))            => EvidenceAnalysis.resolve(name, l, r)
      case (Tuple(arity), fields) if fields.size == arity      => Right(Shape.Product(fields))
      case (Function(arity), types) if types.size == arity + 1 =>
        Right(Shape.Function(types.init, types.last))
      case _ => Left(s"unresolved type: $name")
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

  private object Tuple {
    def unapply(name: String): Option[Int] = numbered(name, "Tuple", 1)
  }

  private object Function {
    def unapply(name: String): Option[Int] = numbered(name, "Function", 0)
  }

  private def numbered(name: String, prefix: String, from: Int): Option[Int] =
    name
      .stripPrefix(prefix)
      .toIntOption
      .filter(n => name.startsWith(prefix) && n >= from && n <= 22)

  /** The `scala` types a key may name without a supplied declaration — each a nominal type of its
    * own, keyed by its canonical spelling, with the number of type arguments it takes. Shared by
    * every reading of type identity (override identity, overload and evidence identity, the
    * arguments of a rigid constructor), so the readings agree on what a builtin name is.
    */
  val nominalArity: Map[String, Int] =
    (List(
      "Unit",
      "Nothing",
      "Null",
      "Boolean",
      "Any",
      "AnyRef",
      "AnyVal",
      "Matchable",
      "EmptyTuple"
    )
      .map(_ -> 0) ++ groundNames.map(_ -> 0) ++
      List("Option" -> 1, "Either" -> 2, "=:=" -> 2, "<:<" -> 2) ++
      (1 to 22).map(n => s"Tuple$n" -> n) ++ (0 to 22).map(n => s"Function$n" -> (n + 1))).toMap

  /** Every name the model reads as a builtin type when no source declares it: what an import or an
    * export must not put a different meaning behind.
    */
  val nominalNames: Set[String] = nominalArity.keySet

  /** The canonical name a builtin spelling denotes: one name per type, so `scala.util.Either` and
    * `Either`, `java.lang.String` and `String`, `Object` and `AnyRef` are one key.
    */
  def canonical(name: String): Option[String] =
    name.stripPrefix("_root_.").stripPrefix("scala.") match {
      case "util.Either"                        => Some("Either")
      case "java.lang.String" | "Predef.String" => Some("String")
      case "Object" | "java.lang.Object"        => Some("AnyRef")
      case other                                => Option.when(nominalArity.contains(other))(other)
    }

}
