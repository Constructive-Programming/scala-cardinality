package cardinality.analysis

import cardinality.analysis.inhabitation.{Card, EvidenceAnalysis, Inhabitation}
import cardinality.analysis.resolution.TopTypes

import Inhabitation.Shape

/** The names the shape model spells itself when no source declares them, by application arity.
  * `Unit` is one construction, `Nothing` none, `Boolean` one bit; every other library declaration
  * the model reads (`Option`, `Either`, tuples, functions, sequences) is `StdlibModel`'s. Ground
  * scalars are transparent — literals with a total equality — sized as the JVM stores them, `Float`
  * / `Double` under `equals` (one NaN, two zeros: raw NaN bits are platform-dependent), `String`
  * the free monoid over `Char`: ω. A top type is one value until its place is known: `TopTypes`
  * raises it to ω in an output.
  */
private[cardinality] object BuiltinTypes {

  /** The builtin or model entry a name denotes at this arity (or why it is not counted); None when
    * the name is neither.
    */
  def known(name: String, args: List[Shape]): Option[Either[String, Shape]] =
    scalar(name, args).orElse(StdlibModel.entry(name).map(StdlibModel.read(_, args)))

  private def scalar(name: String, args: List[Shape]): Option[Either[String, Shape]] =
    (name.stripPrefix("scala."), args) match {
      case ("Unit", Nil)                              => Some(Right(Shape.Product(Nil)))
      case (top, Nil) if TopTypes.named(top).nonEmpty => Some(Right(TopTypes.terminal))
      case ("Nothing" | "Null", Nil)                  => Some(Right(Shape.Sum(Nil)))
      case ("Boolean", Nil)                           =>
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

  /** The ground names, bare: what an import or export could put a different meaning behind. */
  val groundNames: Set[String] = grounds.keySet

  private val spellings: Map[String, Shape] =
    grounds.map((name, size) => name -> (Shape.Ground(name, size): Shape)) ++
      List("java.lang.String", "Predef.String").map(_ -> Shape.Ground("String", Card.Omega))

  // The scalar declarations the model reads, by the `scala` member each simple name spells, with
  // the number of type arguments each takes; `String` is `Predef`'s and `java.lang`'s, and `Object`
  // is `AnyRef`.
  private val scalars: Map[String, Int] =
    (List("Unit", "Nothing", "Null", "Boolean", "Any", "AnyRef", "AnyVal", "Matchable") ++
      groundNames).map(_ -> 0).toMap ++ Map("=:=" -> 2, "<:<" -> 2)

  private val scalarSpellings: Map[String, String] =
    scalars.keySet.flatMap(name => List(name -> name, s"scala.$name" -> name)).toMap ++
      List("scala.Predef.String", "java.lang.String").map(_ -> "String") ++
      List("Object", "java.lang.Object").map(_ -> "AnyRef")

  /** The simple names the root imports bind to builtins or model entries: what an import or an
    * export must not put a different meaning behind.
    */
  val rootNames: Set[String] = scalars.keySet ++ StdlibModel.rootNames ++ TopTypes.names

  /** The canonical key a builtin or model spelling denotes: one key per declaration, so
    * `scala.util.Either` and `Either`, `java.lang.String` and `String`, `Object` and `AnyRef` are
    * one key — the model entry's fully qualified name, or the `scala` member a scalar is. Shared by
    * every reading of type identity (override identity, overload and evidence identity, the
    * arguments of a rigid constructor), so the readings agree on what a builtin name is. A path
    * relative to a root import or an anchored simple name denotes none (`StdlibModel.Spelling`).
    */
  def canonical(name: String): Option[String] =
    StdlibModel
      .canonical(name)
      .orElse(StdlibModel.Spelling.of(name) match {
        case StdlibModel.Spelling.Rooted(path, _) => scalarSpellings.get(path).map(key)
        case _                                    => None
      })

  /** The number of type arguments the declaration a canonical key names takes, or why the model
    * does not know it.
    */
  def arity(canonical: String): Either[String, Int] =
    StdlibModel
      .entry(canonical)
      .fold(Right(scalars(canonical.stripPrefix("scala."))))(StdlibModel.arity)

  private def key(scalar: String): String = s"scala.$scalar"

  /** A ground shape's key: a scalar's `scala` member, or a throwable's own entry. */
  def groundKey(ground: String): String = StdlibModel.canonical(ground).getOrElse(key(ground))

}
