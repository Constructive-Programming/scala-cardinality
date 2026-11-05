package cardinality.analysis.expansion

import cardinality.analysis.model.UniversalMembers

/** What an inline body may reach of the standard library without making an expansion depend on its
  * call site (register D27).
  *
  * Every member listed is a regular method of Scala 3.9.0's scala-library — no `inline` flag, no
  * `using` or `implicit` clause — as the compiler reads its TASTy (fixture a6 `inventory`: `Any`,
  * `AnyRef`, the scalar classes, `String`, `Array`, `Option`, `Either`, the function classes,
  * `NonFatal`, `math` and the listed companions declare no inline member; `Predef` declares
  * `assert`, `eq`, `ne`, `nn`, `runtimeChecked`, `summon` and `valueOf` inline, and every tuple
  * class inherits `Tuple`'s inline operations, so only its fields are listed). A name outside the
  * table is not resolved, so the body is refused.
  */
private[expansion] object LibraryInlines {

  /** The members of every value but `isInstanceOf`, which tests a type (`BodyWalk`). */
  val universal: Set[String] = UniversalMembers.any - "isInstanceOf"

  /** The members only a class instance has (`AnyRef`'s), final there. */
  val references: Set[String] = UniversalMembers.anyRef

  private val comparisons = Set("<", "<=", ">", ">=")

  private val conversions =
    Set("toByte", "toChar", "toShort", "toInt", "toLong", "toFloat", "toDouble")

  private val arithmetic = Set("+", "-", "*", "/", "%", "unary_+", "unary_-") ++ comparisons

  private val integral =
    arithmetic ++ conversions ++ Set("&", "|", "^", "<<", ">>", ">>>", "unary_~")

  private val options = Set(
    "map",
    "flatMap",
    "fold",
    "getOrElse",
    "isEmpty",
    "nonEmpty",
    "isDefined",
    "get",
    "orElse",
    "filter",
    "filterNot",
    "exists",
    "forall",
    "contains",
    "foreach",
    "collect"
  )

  private val eithers = Set(
    "map",
    "flatMap",
    "fold",
    "getOrElse",
    "isLeft",
    "isRight",
    "left",
    "right",
    "swap",
    "toOption",
    "contains",
    "exists",
    "forall",
    "foreach",
    "orElse",
    "filterOrElse"
  )

  private val strings = Set(
    "length",
    "charAt",
    "substring",
    "isEmpty",
    "startsWith",
    "endsWith",
    "contains",
    "indexOf",
    "lastIndexOf",
    "concat",
    "+",
    "trim",
    "toLowerCase",
    "toUpperCase",
    "compareTo",
    "equalsIgnoreCase"
  )

  private val functions = Set("apply", "andThen", "compose", "curried", "tupled")

  /** Instance members by canonical class name (without its `scala.` prefix). */
  private val classes: Map[String, Set[String]] =
    List("Int", "Long", "Short", "Byte", "Char").map(_ -> integral).toMap ++
      List("Double", "Float").map(_ -> (arithmetic ++ conversions)).toMap ++ Map(
        "Boolean" -> Set("&&", "||", "unary_!", "&", "|", "^"),
        "String" -> strings,
        "java.lang.String" -> strings,
        "Array" -> Set("apply", "length", "update", "clone"),
        "Option" -> options,
        "Some" -> (options + "value"),
        "util.Either" -> eithers,
        "util.Left" -> (eithers + "value"),
        "util.Right" -> (eithers + "value")
      ) ++ (0 to 22).map(n => s"Function$n" -> functions).toMap ++
      (1 to 22).map(n => s"Tuple$n" -> (1 to n).map(i => s"_$i").toSet).toMap


  /** A library value or object a root import binds under a simple name. */
  private val rooted: Map[String, String] = Map(
    "Some" -> "Some",
    "None" -> "None",
    "Option" -> "Option",
    "Left" -> "util.Left",
    "Right" -> "util.Right",
    "Either" -> "util.Either",
    "math" -> "math",
    "Nil" -> "collection.immutable.Nil",
    "::" -> "collection.immutable.::"
  )

  /** What a root import binds under a simple name, as far as an inline body may use it. */
  enum Root {
    case Module(name: String)
    case Package(path: List[String])
  }

  def root(name: String): Option[Root] =
    rooted
      .get(name)
      .map(Root.Module(_))
      .orElse(Option.when(name == "scala" || name == "java")(Root.Package(List(name))))

  /** The members a library class's instances offer, when the table lists the class. */
  def members(key: String): Option[Set[String]] = classes.get(canonical(key))

  private val unary = Set("unary_-", "unary_+", "unary_~", "unary_!")

  /** How many argument clauses a listed class member takes at most: a Scala member declared without
    * parameters none, every other one.
    */
  def clauses(name: String): Int = if (parameterless(name)) 0 else 1

  private val parameterless: Set[String] =
    Set("isEmpty", "nonEmpty", "isDefined", "get", "isLeft", "isRight", "left", "right", "swap") ++
      Set("toOption", "value", "length", "curried", "tupled") ++ unary ++ conversions ++
      (1 to 22).map(i => s"_$i")

  /** Whether a library class may be extended by one with an inline member: the function traits and
    * `Conversion` are open, every other class the table lists is final or sealed in the library
    * (fixture a6 `inventory`).
    */
  def extensible(key: String): Boolean = functionClass(key)

  /** Whether a path names `scala.FunctionN` or `scala.Conversion`. */
  def functionClass(path: String): Boolean = conversionClass(path) || (canonical(path) match {
    case s"Function$arity" => arity.toIntOption.exists(n => n >= 0 && n <= 22)
    case _                 => false
  })

  /** Whether a path names `scala.Conversion`. */
  def conversionClass(path: String): Boolean = canonical(path) == "Conversion"

  /** Whether a function class declares a term of the name (besides `Any`'s). */
  def functionMember(name: String): Boolean = functions(name)

  private def canonical(key: String): String = key.stripPrefix("scala.")


  private val packages: Set[List[String]] = Set(
    List("scala"),
    List("java"),
    List("scala", "compiletime"),
    List("scala", "quoted"),
    List("scala", "util"),
    List("scala", "util", "control"),
    List("scala", "collection"),
    List("scala", "collection", "immutable")
  )

  /** Whether a path names a library package an inline body may select through. */
  def libraryPackage(path: List[String]): Boolean = packages(path)

  /** Whether a path names `scala.compiletime` or a member of it. */
  def compileTime(path: List[String]): Option[String] =
    path match {
      case "scala" :: "compiletime" :: rest => Some(("scala.compiletime" :: rest).mkString("."))
      case _                                => None
    }

}
