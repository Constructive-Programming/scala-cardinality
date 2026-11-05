package cardinality.types

/** The namespaces whose wildcard imports both the method side and the stored-value estimate know
  * (docs/research/stdlib-model.md): the stdlib's, and the dependency namespaces the analyzed
  * corpora's wildcards open, listed from their artifacts (cats-core 2.13.0, kyo 1.0.0-RC7). A
  * wildcard outranks another file's package member and the root imports (register D11), so one over
  * a namespace the sources do not supply may bind any name; one listed here binds the members it
  * lists (scala-library 3.9.0 class files and TASTy; the dependencies' sources jars and class
  * entries), or, when they are not listed, any name but a root package and the root spellings each
  * side reads — except the root spellings the namespace is known to declare (`quotes.reflect`'s
  * `Singleton` and `Symbol`, its only type members the root imports also bind).
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

  // The `scala` package's subpackages and objects a relative prefix may start with, which the root
  // import of `scala.*` binds (scala-library 3.9.0).
  private val scalaMembers: Set[String] = Set(
    "Predef",
    "annotation",
    "beans",
    "caps",
    "collection",
    "compiletime",
    "concurrent",
    "deriving",
    "io",
    "jdk",
    "math",
    "quoted",
    "ref",
    "reflect",
    "runtime",
    "sys",
    "util"
  )

  // Every type of the JDK's `java.*` / `javax.*` packages and of scala-library 3.9.0 whose simple
  // name the root imports bind to another declaration, by package (JDK 25 `jimage list`, the
  // scala-library and scala3-library jars' entries, against `java.lang.*`, `scala.*` with the
  // `scala` package's aliases and `Predef`'s). A package missing here declares no such name.
  private val platformRoots: Map[String, Set[String]] = Map(
    "java.awt" -> Set("List"),
    "java.io" -> Set("Console"),
    "java.lang" -> Set(
      "Boolean",
      "Byte",
      "Double",
      "Float",
      "Iterable",
      "Long",
      "Short",
      "StringBuilder"
    ),
    "java.lang.reflect" -> Set("Array", "Proxy"),
    "java.math" -> Set("BigDecimal"),
    "java.net" -> Set("Proxy"),
    "java.sql" -> Set("Array"),
    "java.util" -> Set("Enumeration", "Iterator", "List", "Map", "Set", "Vector"),
    "java.util.function" -> Set("Function"),
    "java.util.stream" -> Set("Stream"),
    "javax.swing.text.html" -> Set("Map", "Option"),
    "scala.annotation.meta" -> Set("languageFeature"),
    "scala.collection" -> Set("IndexedSeq", "Map", "Seq", "Set"),
    "scala.collection.concurrent" -> Set("Map"),
    "scala.collection.immutable" -> Set("Iterable"),
    "scala.collection.mutable" -> Set("Cloneable", "IndexedSeq", "Iterable", "Map", "Seq", "Set"),
    "scala.compiletime.testing" -> Set("Error"),
    "scala.reflect" -> Set("Enum", "Selectable"),
    "scala.runtime.stdLibPatches" -> Set("Predef", "language"),
    "scala.sys.process" -> Set("Process", "ProcessBuilder", "ProcessImpl"),
    "scala.util.control" -> Set("Exception")
  )

  // A wildcard over the JDK or the standard library: its members are not listed, but the names the
  // root imports bind that it declares are, by their full rooted name (`import java.util.*` binds
  // `List` to `java.util.List`). A relative prefix headed by a member of `scala` is that member's.
  private def platform(path: List[String]): Option[Namespace] = {
    val full = path match {
      case head :: _ if scalaMembers(head) => "scala" :: path
      case _                               => path
    }
    Option.when(full.headOption.exists(Set("java", "javax", "scala")))(
      Namespace(
        full.mkString("."),
        None,
        None,
        None,
        "the JDK or the standard library",
        platformRoots.getOrElse(full.mkString("."), Set.empty)
      )
    )
  }

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

  // kyo's package members (kyo 1.0.0-RC7: prelude, kernel, data, stats-registry, schema,
  // schema-json and the artifacts they pull, from the sources jars and the entries). The
  // terms are its top-level objects, definitions, givens and extensions; the types add its
  // classes, traits, enums and top-level type aliases.
  private val kyoTypes: Set[String] =
    """
|Abort Access Admission AllowUnsafe AmbiguousVariantMatchException Ansi Aspect
|AtomicBoolean AtomicInt AtomicLong AtomicRef Base64 Batch Builder ByteSize Changeset
|Check CheckFailed Choice Chunk ChunkBuilder Clock Closed Codec CommandException
|Compare CompileTimeFlag ConcreteTag Console Const ConstValue
|ConstructorRejectedException Convert Counter CounterGauge DecodeException Dict
|DictBuilder Discriminator DoubleAdder DuplicateRepresentationException Duration
|DynamicFlag Emit Env Field FieldNameCollisionException Fields
|FileAccessDeniedException FileAlreadyExistsException
|FileAtomicMoveUnsupportedException FileDirectoryNotEmptyException FileIOException
|FileInvalidPathException FileIsADirectoryException FileLockCleanupException
|FileLockException FileLockOwnershipLostException FileLockTimeoutException
|FileLockUnavailableException FileNotADirectoryException FileNotFoundException
|FileReadException FileStructureException FileSystem FileSystemException
|FileSystemOperation FileWatchException FileWatchInvalidatedException
|FileWriteException FileWriteStalledException Flag FlagChoiceParseException
|FlagDuplicateNameException FlagException FlagExpressionParseException
|FlagNameException FlagParseException FlagPlatform FlagRegistrationException
|FlagRolloutParseException FlagValidationException FlagValidationFailedException
|FlagValueParseException Focus Frame Gauge Glob GlobMacro GlobParseException Histogram
|HostFileSystem Hub Id Instant Interrupted IsFatal Json JsonLines Jsonl Kyo KyoApp
|KyoAppInterrupts KyoException Latch Layer LimitExceededException Local Log LongAdder
|Maybe Memo Meter MissingFieldException MissingTagKeyException Modify
|NavigationException NoVariantMatchException OrderedDict OrderedDictBuilder
|ParseException PathNotFoundException PathRead PathWatch PathWrite
|PermissionDeniedException Pipe Poll ProgramNotFoundException Random RangeException
|Record RecordDecodeException Rejected Render RepresentationUnsupportedException Result
|Retry Rollout Schedule Schema SchemaException SchemaIndexOutOfBoundsException
|SchemaNotSerializableException Scope SecureRandom SecureRandomPlatformSpecific
|ShallowTag Signal Sink Span Stat StaticFlag Stream StreamCompression
|StreamCoreExtensions StreamSystemExtensions Structure System Tag Timeout
|TrailingInputException TransformException TransformFailedException
|TruncatedInputException TypeMap TypeMismatchException UUID UUIDCoreExtensions
|UUIDGenerator UnknownFieldException UnknownVariantException ValidationException
|ValidationFailedException Var VariantNameCollisionException
|WorkingDirectoryNotFoundException async bug""".stripMargin.split("\\s+").toSet

  private val kyoTerms: Set[String] =
    """
|Abort Ansi Aspect Base64 Batch ByteSize Check Choice Chunk ChunkBuilder ConcreteTag
|ConstValue Dict DictBuilder Discriminator Duration Emit Env Field Fields Frame Glob
|GlobMacro Instant IsFatal Kyo KyoException Layer Local Loop Maybe Memo OrderedDict
|OrderedDictBuilder Pipe Poll Record Render Result Schedule ShallowTag Sink Span Stream
|Tag TypeMap UUID Var asUnit bug bytes day days discard gb gib glob hour hours isNull
|kb kib mb mib micro micros milli millis minute minutes month months nano nanos render
|second seconds tib week weeks year years""".stripMargin.split("\\s+").toSet

  // The dependency namespaces the analyzed corpora's wildcards open, listed from their artifacts.
  // cats-core 2.13.0 (sources jar): the syntax objects hold only ops-converter methods, and the
  // syntax traits and the To*Ops parents they extend declare no type members; the instances
  // object's members are its given methods. kyo 1.0.0-RC7 (sources jars and entries across the
  // ten artifacts): its classes, traits, objects, definitions and top-level type aliases. None
  // declares a name the root imports or the model bind.
  private def syntaxObject(name: String, terms: Set[String], types: Set[String] = Set.empty) =
    Namespace(
      s"cats.syntax.$name",
      None,
      Some(types),
      Some(terms),
      "cats syntax extensions"
    )

  private val dependencyNamespaces: List[Namespace] = List(
    syntaxObject("functor", Set("toFunctorOps", "catsSyntaxFunctorTuple2Ops", "catsSyntaxIfF")),
    syntaxObject(
      "either",
      Set(
        "catsSyntaxEither",
        "catsSyntaxEitherBinCompat0",
        "catsSyntaxEitherId",
        "catsSyntaxEitherIdBinCompat0",
        "catsSyntaxEitherObject",
        "catsSyntaxLeft",
        "catsSyntaxRight"
      ),
      Set("CatchOnlyPartiallyApplied")
    ),
    syntaxObject(
      "flatMap",
      Set(
        "catsSyntaxFlatMapIdOps",
        "catsSyntaxFlatMapOps",
        "catsSyntaxFlatMapOptionOps",
        "catsSyntaxFlatten",
        "catsSyntaxIfM",
        "toFlatMapOps"
      )
    ),
    syntaxObject("coflatMap", Set("toCoflatMapOps")),
    syntaxObject(
      "applicative",
      Set(
        "catsSyntaxApplicative",
        "catsSyntaxApplicativeByName",
        "catsSyntaxApplicativeByValue",
        "catsSyntaxApplicativeId"
      )
    ),
    Namespace(
      "cats.instances.function",
      None,
      Some(Set.empty),
      Some(
        """catsKernelBandForFunction0 catsKernelBandForFunction1 catsKernelBoundedSemilatticeForFunction0
          |catsKernelBoundedSemilatticeForFunction1 catsKernelCommutativeGroupForFunction0
          |catsKernelCommutativeGroupForFunction1 catsKernelCommutativeMonoidForFunction0
          |catsKernelCommutativeMonoidForFunction1 catsKernelCommutativeSemigroupForFunction0
          |catsKernelCommutativeSemigroupForFunction1 catsKernelEqForFunction0 catsKernelGroupForFunction0
          |catsKernelGroupForFunction1 catsKernelHashForFunction0 catsKernelMonoidForFunction0
          |catsKernelMonoidForFunction1 catsKernelOrderForFunction0 catsKernelPartialOrderForFunction0
          |catsKernelSemigroupForFunction0 catsKernelSemigroupForFunction1 catsKernelSemilatticeForFunction0
          |catsKernelSemilatticeForFunction1 catsSddDeferForFunction0 catsSyntaxCommutativeMonadForFunction0
          |catsSyntaxCommutativeSemigroupForFunction0 catsSyntaxFlatMapForFunction0 catsSyntaxFunctorForFunction0
          |catsSyntaxMonoidForFunction0 catsSyntaxSemigroupForFunction0 catsSyntaxTraverseForFunction0
          |catsUnzipForFunction0 commasForFunction0 commasForFunction1""".stripMargin
          .split("\\s+")
          .toSet
      ),
      "cats instances"
    ),
    Namespace("kyo", None, Some(kyoTypes), Some(kyoTerms), "kyo effects and data")
  )

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
    ),
    // `scala.compiletime.ops`' members (scala-library 3.9.0): the objects `any`, `boolean`,
    // `double`, `float`, `int`, `long` and `string`, whose type operators are their own members.
    Namespace(
      "scala.compiletime.ops",
      None,
      Some(Set.empty),
      Some(Set("any", "boolean", "double", "float", "int", "long", "string")),
      "compile-time type operators"
    )
  ) ++ dependencyNamespaces

  /** The listed namespace a wildcard prefix spells (a `_root_` anchor dropped); whether its first
    * name denotes the library's is the caller's to check, against the namespace's anchor.
    */
  def of(prefix: List[String]): Option[Namespace] = {
    val path = prefix.dropWhile(_ == "_root_")
    all.find(_.path == path.mkString(".")).orElse(platform(path))
  }

  def at(full: List[String]): Option[Namespace] =
    all.find(_.full == full).orElse(platform(full))

  /** Why a name is not read where a wildcard over a namespace no source supplies may bind it. */
  def mayBind(written: String): String = s"may be imported by $written"

  def mayBindThrough(written: String, head: String, why: String): String =
    s"${mayBind(written)}: $head $why"

}
