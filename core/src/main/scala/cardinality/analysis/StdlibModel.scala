package cardinality.analysis

import cardinality.analysis.inhabitation.Inhabitation

import Inhabitation.Shape

/** The standard-library declarations the model reads without their sources: a small table keyed by
  * fully qualified name, versioned with the library API it describes.
  *
  * Each entry reads its declaration one way. A product, sum or function is the shape its values
  * have: a `final case class` with a public total constructor and public fields holds exactly the
  * product of its fields (`Left[A, B]` its `A`, `Some[A]` its `A`), `NamedTuple[N, V]` is an opaque
  * alias with total conversions to and from `V`. A sequence — `List`, `Vector`, `IndexedSeq`,
  * `ArraySeq`, `Seq`, an `Array` — is `μX. 1 + E × X`: pure, total programs observe only its length
  * and its elements by position (identity and mutation are outside the contract), so the shared
  * `Repeated` shape loses nothing. `Seq` also admits an infinite `LazyList`, which changes no
  * count: a total program reads a finite prefix of it (continuity), and a goal of one needs an
  * element, with which the finite sequences already give ω. An array is built only with its
  * element's `ClassTag`, so one over a type parameter is refused. An excluded entry is outside the
  * counting model for good: reading an `Iterator` or a `ByteBuffer` changes it, a `Map` or `Set` is
  * built and read by testing keys for equality, `scala.quoted` is compile-time reflection. An
  * unmodelled entry is a class or interface whose members the model does not hold (`Throwable`,
  * `Product`, `java.util.List`, the final `Integer`).
  *
  * The table answers where Scala's root imports would — after every declaration, import and
  * wildcard of the sources — so a name the sources bind is never the model's. It also lists the
  * stdlib namespaces whose wildcard imports it knows, and the parents whose members it knows, each
  * checked against the Scala 3.9.0 compiler (none declares a type the root imports spell).
  */
private[cardinality] object StdlibModel {

  /** The table's version: Scala 3.9.0's library API, on 2.13 collections (`Seq` is
    * `immutable.Seq`). A reason naming it says what the table does not hold.
    */
  val version = "stdlib-model/1"

  /** How a reason says a name is declared nowhere this reading looks. */
  val undeclared = s"not declared in the sources or $version"

  /** How the model reads an entry. */
  enum Reading {

    /** The shape the arguments give its values. */
    case Structural(shape: List[Shape] => Shape)

    /** A finite sequence of its argument; an array needs its element's class to be built. */
    case Sequence(tagged: Boolean)

    /** Outside the counting model, for the reason given. */
    case Excluded(why: String)

    /** A class or interface whose members the model does not hold; open when a subclass outside the
      * library may extend it.
      */
    case Unmodelled(open: Boolean)
  }

  import Reading.*

  /** One declaration: its fully qualified name, its type parameters, its reading, and the shape its
    * values have spelled over those parameters (the body of the type lambda an unapplied spelling
    * denotes; empty for an entry the model does not read).
    */
  final case class Entry(fqn: String, parameters: List[String], reading: Reading, body: String) {

    /** The type constructor an unapplied spelling denotes. */
    def lambda: String = parameters.mkString("[", ", ", s"] =>> $body")
  }

  private def numbered(prefix: String, count: Int): List[String] =
    (1 to count).map(i => s"$prefix$i").toList

  private def tuple(n: Int): Entry = {
    val parameters = numbered("T", n)
    Entry(s"scala.Tuple$n", parameters, Structural(Shape.Product(_)), parameters.mkString(" × "))
  }

  private def function(n: Int): Entry = {
    val parameters = numbered("T", n) :+ "R"
    val domain = parameters.init.mkString("(", ", ", ")")
    Entry(
      s"scala.Function$n",
      parameters,
      Structural(types => Shape.Function(types.init, types.last)),
      s"$domain => R"
    )
  }

  // A case class whose values are one of its arguments: `Left[A, B]` holds an `A`.
  private def wrapper(fqn: String, parameters: List[String], field: Int): Entry =
    Entry(
      fqn,
      parameters,
      Structural(types => Shape.Product(List(types(field)))),
      parameters(field)
    )

  private def sequence(fqn: String): Entry =
    Entry(fqn, List("E"), Sequence(fqn == "scala.Array"), "Σₙ Eⁿ")

  private def excluded(why: String)(fqn: String, arity: Int): Entry =
    Entry(fqn, numbered("T", arity), Excluded(why), "")

  private def unmodelled(open: Boolean)(fqn: String): Entry = Entry(fqn, Nil, Unmodelled(open), "")

  private val SinglePass = "single-pass: reading it changes it"
  private val Equality = "built and read by testing keys for equality"
  private val Reflection = "compile-time reflection"
  private val Collected = "garbage-collector state"

  // The type declarations of `scala.quoted` the model names, with their type parameter counts.
  private val Quoted =
    List("Expr" -> 1, "ExprMap" -> 0, "FromExpr" -> 1, "Quotes" -> 0, "ToExpr" -> 1, "Type" -> 1)

  // The root imports, weakest first: `java.lang.*`, `scala.*` (with the `scala` package's aliases)
  // and `Predef.*`. Each binds a simple name and may be spelled through its own fully qualified
  // prefix — never a relative one: a top-level package `Predef` outranks `scala.Predef` (Scala
  // 3.9.0 fixture).
  private enum Root(val prefix: String) {
    case JavaLang extends Root("java.lang.")
    case ScalaPackage extends Root("scala.")
    case Predef extends Root("scala.Predef.")
  }

  // The throwables the root imports bind, with the import that binds each: the `scala` package
  // aliases all but `IllegalStateException`, and `NoSuchElementException` to `java.util`'s (Scala
  // 3.9.0 fixtures).
  private val Exceptions: List[(String, Root)] =
    List(
      "Throwable",
      "Exception",
      "Error",
      "RuntimeException",
      "NullPointerException",
      "ClassCastException",
      "IndexOutOfBoundsException",
      "UnsupportedOperationException",
      "IllegalArgumentException",
      "NumberFormatException"
    ).map(name => s"java.lang.$name" -> Root.ScalaPackage) ++ List(
      "java.lang.IllegalStateException" -> Root.JavaLang,
      "java.util.NoSuchElementException" -> Root.ScalaPackage
    )

  private val entries: List[Entry] =
    (1 to 22).map(tuple).toList ++ (0 to 22).map(function) ++ List(
      Entry("scala.EmptyTuple", Nil, Structural(_ => Shape.Product(Nil)), "1"),
      Entry(
        "scala.NamedTuple.NamedTuple",
        List("N", "V"),
        Structural(types => types(1)),
        "V"
      ),
      wrapper("scala.Some", List("A"), 0),
      wrapper("scala.util.Left", List("A", "B"), 0),
      wrapper("scala.util.Right", List("A", "B"), 1),
      Entry(
        "scala.Option",
        List("A"),
        Structural(types => Shape.Sum(Shape.Product(Nil) :: types)),
        "1 + A"
      ),
      Entry("scala.util.Either", List("L", "R"), Structural(Shape.Sum(_)), "L + R")
    ) ++ List(
      "scala.collection.immutable.List",
      "scala.collection.immutable.Vector",
      "scala.collection.immutable.IndexedSeq",
      "scala.collection.immutable.Seq",
      "scala.collection.immutable.ArraySeq",
      "scala.Array"
    ).map(sequence) ++ List(
      excluded(SinglePass)("scala.collection.Iterator", 1),
      excluded(SinglePass)("scala.collection.IterableOnce", 1),
      excluded(SinglePass)("java.util.Iterator", 1),
      excluded(SinglePass)("java.nio.ByteBuffer", 0),
      excluded(SinglePass)("java.io.InputStream", 0),
      excluded(Equality)("scala.collection.immutable.Map", 2),
      excluded(Equality)("scala.collection.immutable.Set", 1),
      excluded(Collected)("java.lang.ref.WeakReference", 1),
      excluded(Collected)("java.lang.ref.ReferenceQueue", 1)
    ) ++ Quoted.map((name, arity) => excluded(Reflection)(s"scala.quoted.$name", arity)) ++
      (Exceptions.map(_._1) ++ List(
        "scala.Product",
        "scala.Equals",
        "java.lang.CharSequence",
        "java.util.List",
        "java.util.Map",
        "java.util.Set",
        "java.util.Collection",
        "java.util.ArrayList",
        "java.util.HashMap",
        "java.util.concurrent.ConcurrentHashMap"
      )).map(unmodelled(open = true)) :+ unmodelled(open = false)("java.lang.Integer")

  private val byName: Map[String, Entry] = entries.map(entry => entry.fqn -> entry).toMap

  // Each name under the root import that binds it, and under that import's own prefix.
  private def spelledBy(bindings: List[(String, String, Root)]): Map[String, String] =
    bindings.flatMap((name, fqn, root) => List(name -> fqn, s"${root.prefix}$name" -> fqn)).toMap

  // Simple names the root imports bind to entries, with the root that binds them: the `scala`
  // package's own members, then its aliases and `Predef`'s.
  private val spellings: List[(String, String, Root)] =
    entries.map(_.fqn).collect {
      case fqn if fqn.startsWith(Root.ScalaPackage.prefix) && !simple(fqn).contains('.') =>
        (simple(fqn), fqn, Root.ScalaPackage)
    } ++ List(
      ("Either", "scala.util.Either", Root.ScalaPackage),
      ("Left", "scala.util.Left", Root.ScalaPackage),
      ("Right", "scala.util.Right", Root.ScalaPackage),
      ("List", "scala.collection.immutable.List", Root.ScalaPackage),
      ("Vector", "scala.collection.immutable.Vector", Root.ScalaPackage),
      ("IndexedSeq", "scala.collection.immutable.IndexedSeq", Root.ScalaPackage),
      ("Seq", "scala.collection.immutable.Seq", Root.ScalaPackage),
      ("Iterator", "scala.collection.Iterator", Root.ScalaPackage),
      ("IterableOnce", "scala.collection.IterableOnce", Root.ScalaPackage),
      ("Map", "scala.collection.immutable.Map", Root.Predef),
      ("Set", "scala.collection.immutable.Set", Root.Predef),
      ("Integer", "java.lang.Integer", Root.JavaLang),
      ("CharSequence", "java.lang.CharSequence", Root.JavaLang)
    ) ++ Exceptions.map((fqn, root) => (fqn.split('.').last, fqn, root))

  private def simple(fqn: String): String = fqn.stripPrefix(Root.ScalaPackage.prefix)

  private val spelled: Map[String, String] = spelledBy(spellings)

  /** The simple names the root imports bind to entries: what an import or export could rebind. */
  val rootNames: Set[String] = spellings.map(_._1).toSet

  /** The root packages: a path from one of them names a library declaration, wherever it is read,
    * unless the sources declare a package of that name inside one of theirs.
    */
  val roots: Set[String] = Set("scala", "java")

  /** How a spelling may name a declaration of the root imports: through them (a simple name, or a
    * path from a root package, `_root_`-anchored or not), relative to a root import
    * (`Predef.String`, `util.Either` — a top-level package of its first name outside the sources
    * outranks the root import, Scala 3.9.0 fixture), or as an anchored simple name, which is a
    * top-level declaration.
    */
  enum Spelling {
    case Rooted(name: String, anchored: Boolean)
    case Relative(name: String, head: String)
    case TopLevel(name: String)
  }

  object Spelling {

    def of(written: String): Spelling = {
      val name = written.stripPrefix(s"${SourceIndex.Root}.")
      (written != name, name.takeWhile(_ != '.')) match {
        case (true, head) if head == name                                => TopLevel(name)
        case (anchored, head) if anchored || head == name || roots(head) => Rooted(name, anchored)
        case (_, head)                                                   => Relative(name, head)
      }
    }

    /** An import's target path, whose `_root_` anchor the import reading drops: a simple one could
      * only be written anchored.
      */
    def imported(path: String): Spelling = if (path.contains('.')) of(path) else TopLevel(path)

  }

  // The path a spelling names through the root imports, if it names one.
  private def rooted(name: String): Option[String] = Spelling.of(name) match {
    case Spelling.Rooted(path, _) => Some(path)
    case _                        => None
  }

  /** The entry a spelling denotes once no declaration of the sources answers it: a root-import
    * name, or a qualified one through the root imports' own prefixes or the entry's fully qualified
    * name, `_root_`-anchored or not.
    */
  def entry(name: String): Option[Entry] =
    rooted(name).flatMap(path => spelled.get(path).flatMap(byName.get).orElse(byName.get(path)))

  /** The canonical name of an entry's spelling: one name per declaration. */
  def canonical(name: String): Option[String] = entry(name).map(_.fqn)

  /** The number of type arguments an entry the model reads takes, or why it does not hold them: an
    * unmodelled class is recorded by name alone, and an excluded entry's parameters are not read
    * (`scala.quoted.Type[T <: AnyKind]` takes a constructor as well).
    */
  def arity(entry: Entry): Either[String, Int] = entry.reading match {
    case _: (Structural | Sequence) => Right(entry.parameters.size)
    // The reason the reading refuses an entry it does not read, whatever the arguments.
    case _ => read(entry, Nil).map(_ => entry.parameters.size)
  }

  private def outside(entry: Entry, why: String): String =
    s"outside the counting model: ${entry.fqn} ($why)"

  private def unmodelled(open: Boolean, what: String, entry: Entry): String =
    s"${if (open) "open " else ""}stdlib $what with unmodelled members: ${entry.fqn}"

  /** What a model spelling applied to these arguments denotes, or why it is not counted. */
  def read(entry: Entry, args: List[Shape]): Either[String, Shape] = entry.reading match {
    case Excluded(why)                                  => Left(outside(entry, why))
    case Unmodelled(open)                               => Left(unmodelled(open, "type", entry))
    case _ if args.isEmpty && entry.parameters.nonEmpty =>
      Left(
        s"higher-kinded argument: ${entry.fqn} (type constructor ${entry.lambda}, " +
          "needs higher-kinded instantiation)"
      )
    case _ if args.size != entry.parameters.size =>
      Left(s"type argument arity: ${entry.fqn} takes ${entry.parameters.size}, given ${args.size}")
    case Structural(shape) => Right(shape(args))
    // `new Array[E](0)` needs a `ClassTag[E]`, which Scala 3.9.0 synthesizes for every element type
    // but a type parameter, an abstract type member or a singleton of one — `Nothing`, `Null`, an
    // object, a singleton of a class type, an opaque type and `F[A]` included (fixtures) — so only
    // a rigid element is refused. The reading has no polarity: an input array is refused too.
    case Sequence(tagged) if tagged && rigid(args.head) =>
      Left(
        s"Array of an abstract element type is not read: ${entry.fqn} " +
          "(building one needs ClassTag evidence, and the reading has no polarity)"
      )
    case Sequence(_) => Right(Shape.Repeated(args.head))
  }

  // A type parameter or abstract member, read through an existential's opening and a singleton's
  // widened type (`x.type` for `x: A` has no `ClassTag` either). An object's singleton has one.
  private def rigid(element: Shape): Boolean = element match {
    case _: Shape.Atom                     => true
    case Shape.Existential(_, opened)      => rigid(opened)
    case Shape.Singleton(_, Some(widened)) => rigid(widened)
    case _                                 => false
  }

  /** A stdlib namespace a wildcard import may open: its path; the term path its first name must
    * denote for the prefix to be the library's (none for a root package's member); the type names
    * it declares, when the model lists them; and what it is. None of them spells a name the root
    * imports bind (Scala 3.9.0 fixtures), so a wildcard over one never shadows the model's own
    * spellings.
    */
  final case class Namespace(
      path: String,
      anchor: Option[List[String]],
      types: Option[Set[String]],
      what: String
  )

  // `scala.jdk.CollectionConverters`' members (scala-library 2.13): the conversion classes only.
  private val converters: List[String] =
    List(
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
    ).map(_ + "HasAsJava")

  private val namespaces: List[Namespace] = List(
    Namespace("scala.quoted", None, Some(Quoted.map(_._1).toSet), Reflection),
    Namespace(
      "scala.jdk.CollectionConverters",
      None,
      Some(converters.toSet),
      "implicit conversions between Scala and Java collections"
    ),
    // `Quotes#reflectModule`'s members, reached through the term `scala.quoted.quotes`.
    Namespace("quotes.reflect", Some(List("scala", "quoted", "quotes")), None, Reflection)
  )

  /** The stdlib namespace a wildcard prefix spells (a `_root_` anchor dropped); whether its first
    * name denotes the library's is the caller's to check, against the namespace's anchor.
    */
  def namespace(prefix: List[String]): Option[Namespace] = {
    val path = prefix.dropWhile(_ == SourceIndex.Root).mkString(".")
    namespaces.find(_.path == path)
  }

  /** Why a stdlib parent the model does not summarize, or summarizes without its term members,
    * leaves a template's members unknown.
    */
  def parentReason(entry: Entry): String = entry.reading match {
    case Excluded(why)    => outside(entry, why)
    case Unmodelled(open) => unmodelled(open, "parent", entry)
    case _                => unmodelled(open = true, "parent", entry)
  }

  /** A stdlib parent the model knows declares no type member, with the term members a subclass must
    * implement and the concrete ones it may override — or, when `listed` is false, term members the
    * model does not hold (`Product`).
    */
  final case class Summary(abstracts: Set[String], overridable: Set[String], listed: Boolean)

  /** A stdlib parent the model knows, spelled through the root imports. Root and marker types
    * declare no member a count reads (their universal members are outside the contract).
    */
  def parent(name: String): Option[Summary] =
    rooted(name).flatMap(path =>
      parents.get(canonical(path).orElse(markerSpelled.get(path)).getOrElse(path))
    )

  // The root and marker types that are not entries, by the root import that binds each:
  // `java.io.Serializable` only by its own path.
  private val markers: List[(String, String, Root)] =
    List("Any", "AnyRef", "Matchable", "AnyVal", "Serializable", "Dynamic").map(name =>
      (name, s"scala.$name", Root.ScalaPackage)
    ) :+ ("Object", "java.lang.Object", Root.JavaLang)

  private val markerSpelled: Map[String, String] = spelledBy(markers)

  private val parents: Map[String, Summary] = {
    val memberless = Summary(Set.empty, Set.empty, listed = true)
    ("java.io.Serializable" :: markers.map(_._2)).map(_ -> memberless).toMap ++
      List("scala.Product", "scala.Equals").map(
        _ -> Summary(Set.empty, Set.empty, listed = false)
      ) +
      // `IterableOnce` (scala-library 2.13, read with javap): abstract `iterator`, default
      // `knownSize` and `stepper`.
      ("scala.collection.IterableOnce" -> Summary(
        Set("iterator"),
        Set("knownSize", "stepper"),
        listed = true
      ))
  }

}
