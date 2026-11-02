package cardinality.types

/** The stdlib namespaces whose wildcard imports both the method side and the stored-value estimate
  * know (docs/research/stdlib-model.md). A wildcard outranks another file's package member and the
  * root imports (register D11), so one over a namespace the sources do not supply may bind any
  * name; one listed here binds the members it lists (scala-library 3.9.0 class files and TASTy),
  * or, when they are not listed, any name but a root package and the root spellings each side reads
  * — except the root spellings the namespace is known to declare (`quotes.reflect`'s `Singleton`
  * and `Symbol`, its only type members the root imports also bind).
  */
private[cardinality] object ListedNamespaces {

  /** A member a listed namespace declares, by its qualified name, or perhaps one it does not list.
    */
  enum Binding {
    case Member(fqn: String)
    case Unlisted
  }

  /** A namespace a wildcard import may open: its path as written; the term path its first name must
    * denote for the prefix to be the library's (none for a root package's member); the type and
    * term names it declares, when the model lists them; what it is; and, when its types are not
    * listed, the root spellings among them (`rootTypes`).
    */
  final case class Namespace(
      path: String,
      anchor: Option[List[String]],
      types: Option[Set[String]],
      terms: Option[Set[String]],
      what: String,
      rootTypes: Set[String] = Set.empty
  ) {

    /** How a wildcard over the namespace binds a type name; `root` says the root imports bind it
      * and the reading side reads it, which an unlisted namespace binds only as `rootTypes` says.
      */
    def typeBinding(name: String, root: Boolean): Option[Binding] =
      if (types.isEmpty && rootTypes(name)) Some(Binding.Member(s"$path.$name"))
      else binding(name, types, root)

    def termBinding(name: String): Option[Binding] = binding(name, terms, roots(name))

    private def binding(name: String, listed: Option[Set[String]], root: Boolean) =
      listed match {
        case Some(names) => Option.when(names(name))(Binding.Member(s"$path.$name"))
        case None        => Option.unless(root)(Binding.Unlisted)
      }

    def full: List[String] = anchor.fold(segments)(_ ++ segments.drop(1))

    private def segments: List[String] = path.split('.').toList
  }

  val roots: Set[String] = Set("scala", "java")

  val quoted: List[(String, Int)] =
    List("Expr" -> 1, "ExprMap" -> 0, "FromExpr" -> 1, "Quotes" -> 0, "ToExpr" -> 1, "Type" -> 1)

  // `scala.quoted`'s term members (scala-library 3.9.0: its objects, the top-level `quotes` and
  // the subpackage `runtime`).
  private val quotedTerms =
    Set("Expr", "Exprs", "FromExpr", "ToExpr", "Type", "Varargs", "quotes", "runtime")

  // `scala.jdk.CollectionConverters`' members (scala-library 3.9.0): the conversion classes, each
  // with its implicit conversion of the same name.
  private val converters: Set[String] =
    (List(
      "Iterator",
      "Iterable",
      "Collection",
      "List",
      "Set",
      "Map",
      "ConcurrentMap",
      "Enumeration",
      "Dictionary",
      "Properties"
    ).map(_ + "HasAsScala") ++ List(
      "Iterator",
      "Iterable",
      "Buffer",
      "MutableSeq",
      "Seq",
      "MutableSet",
      "Set",
      "MutableMap",
      "Map",
      "ConcurrentMap"
    ).map(_ + "HasAsJava")).toSet

  private val reflection = "compile-time reflection"

  private val all: List[Namespace] = List(
    Namespace("scala.quoted", None, Some(quoted.map(_._1).toSet), Some(quotedTerms), reflection),
    Namespace(
      "scala.jdk.CollectionConverters",
      None,
      Some(converters),
      Some(converters),
      "implicit conversions between Scala and Java collections"
    ),
    // `Quotes#reflectModule`'s members, reached through the term `scala.quoted.quotes`: 323 type
    // members (the scala-library 3.9.0 TASTy of `Quotes`), of which `Singleton` and `Symbol` are
    // the names the root imports also bind (every `scala.*`, `scala.Predef` and `java.lang.*` type
    // checked); no term `scala` or `java`.
    Namespace(
      "quotes.reflect",
      Some(List("scala", "quoted", "quotes")),
      None,
      None,
      reflection,
      Set("Singleton", "Symbol")
    )
  )

  /** The listed namespace a wildcard prefix spells (a `_root_` anchor dropped); whether its first
    * name denotes the library's is the caller's to check, against the namespace's anchor.
    */
  def of(prefix: List[String]): Option[Namespace] = {
    val path = prefix.dropWhile(_ == "_root_").mkString(".")
    all.find(_.path == path)
  }

  def at(full: List[String]): Option[Namespace] = all.find(_.full == full)

  /** Why a name is not read where a wildcard over a namespace no source supplies may bind it. */
  def mayBind(written: String): String = s"may be imported by $written"

  def mayBindThrough(written: String, head: String, why: String): String =
    s"${mayBind(written)}: $head $why"

}
