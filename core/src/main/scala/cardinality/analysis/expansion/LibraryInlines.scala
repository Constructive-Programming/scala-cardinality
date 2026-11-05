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

  /** The library's objects an inline body may select from, with their members, and the class of the
    * value an object is (`None` is an `Option`).
    */
  private val modules: Map[String, (Set[String], Option[String])] = Map(
    "Some" -> (Set("apply", "unapply"), None),
    "None" -> (options, Some("Option")),
    "Option" -> (Set("apply", "empty", "when", "unless"), None),
    "util.Left" -> (Set("apply", "unapply"), None),
    "util.Right" -> (Set("apply", "unapply"), None),
    "util.Either" -> (Set("cond"), None),
    "util.control.NonFatal" -> (Set("apply", "unapply"), None),
    "math" -> (Set("max", "min", "abs", "signum", "floorDiv", "floorMod"), None),
    "collection.immutable.Nil" -> (Set.empty, None),
    "collection.immutable.::" -> (Set("apply", "unapply"), None)
  )

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

  /** `Predef` functions a body may call — regular methods, no clause filled implicitly — with the
    * argument clauses each takes.
    */
  private val functionsOfPredef: Map[String, Int] = Map(
    "identity" -> 1,
    "println" -> 1,
    "print" -> 1,
    "???" -> 0,
    "locally" -> 1,
    "require" -> 1,
    "classOf" -> 0
  )

  /** The library functions whose type argument is a runtime class (`BodyTypes.reified`). */
  val reifying: Set[String] = Set("classOf")

  /** What a root import binds under a simple name, as far as an inline body may use it. */
  enum Root {
    case Module(name: String)
    case Function(arity: Int)
    case Package(path: List[String])
    case Refused(what: String)
  }

  def root(name: String): Option[Root] =
    rooted
      .get(name)
      .map(Root.Module(_))
      .orElse(functionsOfPredef.get(name).map(Root.Function(_)))
      .orElse(Option.when(name == "scala" || name == "java")(Root.Package(List(name))))
      .orElse(Option.when(scalaPackages(name))(Root.Package(List("scala", name))))
      .orElse(predefRefusal(name).map(Root.Refused(_)))

  // `Predef`'s inline methods, whose bodies are not supplied, and the searches it offers.
  private def predefRefusal(name: String): Option[String] = name match {
    case "summon" | "implicitly" => Some(s"resolves a given with $name, which this check does not")
    case "valueOf" | "assert"    => Some(s"calls scala.Predef.$name, an inline method not supplied")
    case _                       => None
  }

  /** The members a library object offers, when the table lists it (`name` without `scala.`). */
  def module(name: String): Option[Set[String]] = modules.get(name).map(_._1)

  /** The class a library object's `apply` builds. */
  def constructed(name: String): Option[String] =
    Option.when(Set("Some", "util.Left", "util.Right")(name))(name)

  /** The class whose members a library object's value has, if it is a value of one. */
  def moduleClass(name: String): Option[String] = modules.get(name).flatMap(_._2)

  /** How many argument clauses a listed member of a library object takes: a value's member as its
    * class's, `Option.when` and `unless` two, `Option.empty` none, every other one.
    */
  def moduleClauses(module: String, name: String): Int =
    moduleClass(module).filter(members(_).exists(_(name))) match {
      case Some(cls)                           => clauses(cls, name)
      case None if Set("when", "unless")(name) => 2
      case None if name == "empty"             => 0
      case None if universal(name)             => inheritedClauses(name)
      case None                                => 1
    }

  /** The class a listed member of a library object returns, where the table knows it. */
  def moduleResult(module: String, name: String): Option[String] =
    moduleClass(module) match {
      case Some(cls) => result(cls, name)
      case None      => Option.when(name == "apply")(module).flatMap(constructed)
    }

  /** The members a library class's instances offer, when the table lists the class. */
  def members(key: String): Option[Set[String]] = classes.get(canonical(key))

  /** The class a listed member returns, where its arguments' types do not decide it: a test is a
    * `Boolean`, a length, an index or a comparison an `Int`, a conversion its target, a unary
    * operator its receiver's class. A binary operator's result widens to its argument's type (`1 +
    * 2L` is a `Long`, `1 + "a"` a `String`), which this reading does not type: None.
    */
  def result(key: String, name: String): Option[String] =
    members(key)
      .filter(_(name))
      .flatMap(_ => results.get(name).orElse(Option.when(unary(name))(canonical(key))))

  private val unary = Set("unary_-", "unary_+", "unary_~", "unary_!")

  /** How many argument clauses a listed class member takes at most: a Scala member declared without
    * parameters none, `Option.fold` two, every other one — a Java method declared `()` (every
    * `String` member here) included, which an empty clause applies.
    */
  def clauses(key: String, name: String): Int = canonical(key) match {
    case "String" | "java.lang.String"       => 1
    case "Option" | "Some" if name == "fold" => 2
    case _ if parameterless(name)            => 0
    case _                                   => 1
  }

  private val parameterless: Set[String] =
    Set("isEmpty", "nonEmpty", "isDefined", "get", "isLeft", "isRight", "left", "right", "swap") ++
      Set("toOption", "value", "length", "curried", "tupled") ++ unary ++ conversions ++
      (1 to 22).map(i => s"_$i")

  /** How many argument clauses a member every value or every case class inherits takes: `##`, the
    * casts and `Product`'s and `Enum`'s parameterless members none, the others (`Any`'s are
    * declared `()`) one.
    */
  def inheritedClauses(name: String): Int =
    if (inheritedParameterless(name)) 0 else 1

  private val inheritedParameterless: Set[String] = Set(
    "##",
    "asInstanceOf",
    "isInstanceOf",
    "productArity",
    "productPrefix",
    "productIterator",
    "productElementNames",
    "ordinal"
  )

  private val results: Map[String, String] =
    (comparisons ++ Set(
      "==",
      "!=",
      "&&",
      "||",
      "isEmpty",
      "nonEmpty",
      "isDefined",
      "isLeft",
      "isRight",
      "exists",
      "forall",
      "contains",
      "startsWith",
      "endsWith",
      "equals",
      "equalsIgnoreCase"
    )).map(_ -> "Boolean").toMap ++
      Set("length", "hashCode", "indexOf", "lastIndexOf", "compareTo").map(_ -> "Int").toMap ++
      conversions.map(c => c -> c.stripPrefix("to")).toMap ++
      Map(
        "toOption" -> "Option",
        "charAt" -> "Char",
        "substring" -> "String",
        "trim" -> "String",
        "toLowerCase" -> "String",
        "toUpperCase" -> "String",
        "concat" -> "String"
      )

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

  // The `scala` package's subpackages a root import binds by their simple names.
  private val scalaPackages = Set("compiletime", "quoted", "util", "collection")

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

  /** The library object a dotted path names, its `scala.` prefix dropped. */
  def libraryModule(path: List[String]): Option[String] =
    Option.when(path.headOption.contains("scala"))(path.tail.mkString(".")).filter(modules.contains)

  /** Whether a path names `scala.compiletime` or a member of it, `scala.quoted`'s likewise. */
  def compileTime(path: List[String]): Option[String] =
    path match {
      case "scala" :: "compiletime" :: rest => Some(("scala.compiletime" :: rest).mkString("."))
      case "scala" :: "quoted" :: rest      => Some(("scala.quoted" :: rest).mkString("."))
      case _                                => None
    }

  /** `scala.compiletime`'s term members, which a wildcard import of it binds. */
  val compileTimeMembers: Set[String] = Set(
    "asMatchable",
    "Erased",
    "erasedValue",
    "constValue",
    "constValueOpt",
    "constValueTuple",
    "summonFrom",
    "summonInline",
    "summonAll",
    "error",
    "codeOf",
    "requireConst",
    "uninitialized",
    "deferred",
    "byName",
    "ops",
    "testing"
  )

}
