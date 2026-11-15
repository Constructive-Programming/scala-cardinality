package cardinality.analysis

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation}
import cardinality.analysis.resolution.TopTypes
import cardinality.capacity.BaseTypes

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
  * element's `ClassTag`, so one over a type parameter is refused. Reading is no effect (the
  * no-side-effects assumption), so a single-pass `Iterator` or `IterableOnce` is the sequence it
  * yields, and an `InputStream` or `ByteBuffer` the bytes it yields. Key equality is lawful (the
  * lawful-equality assumption), so a `Map` or `Set` is its abstract value: the finite partial
  * function or finite subset it holds, read over closed arguments as the total function it
  * tabulates (`K => Option[V]`, `E => Boolean`, the same value space: an infinite domain saturates
  * at ω, like the finite maps). Over an open argument its keys are tested for an equality no
  * parametric program has, so it is refused. An excluded entry is outside the counting model for
  * good: `scala.quoted` is compile-time reflection, and a `ClassTag`, `TypeTest` or `Mirror`
  * reveals the type it is evidence for. A throwable is data (register D26): its pure members read
  * strings and throwables, and its constructors build infinitely many values they tell apart, so it
  * is an ω transparent value, as `String` is; so is an arbitrary-precision number (`BigInt`,
  * `BigDecimal`): countably many finite values, each a literal of a total equality. An unmodelled
  * entry is a class or interface whose members the model does not hold (`Product`,
  * `java.lang.CharSequence`). The table answers for a declaration's fully qualified name (Scala
  * 3.9.0).
  */
private[cardinality] object StdlibModel {

  /** How the model reads an entry. */
  enum Reading {

    /** The shape the arguments give its values. */
    case Structural(shape: List[Shape] => Shape)

    /** A finite sequence of its argument; an array needs its element's class to be built. */
    case Sequence(tagged: Boolean)

    /** A finite map or set under lawful key equality: over closed arguments, the total function it
      * tabulates.
      */
    case Tabulated(shape: List[Shape] => Shape)

    /** Outside the counting model, for the reason given. */
    case Excluded(reason: Exclusion)

    /** An open class or interface whose members the model does not hold. */
    case Unmodelled

    /** A throwable (register D26) or an arbitrary-precision number: an ω transparent value. */
    case Transparent
  }

  /** Why an entry is outside the counting model: a closed set of reasons. */
  enum Exclusion(val why: String) {
    case Reflection extends Exclusion("compile-time reflection")

    case Revealing
        extends Exclusion("type-revealing evidence: it tells instantiations apart at run time")

    case Collected extends Exclusion("garbage-collector state")
  }

  import Reading.*
  import Exclusion.{Collected, Reflection, Revealing}

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

  // An optional value: none, or one of its argument.
  private def optional(fqn: String): Entry =
    Entry(fqn, List("A"), Structural(types => Shape.Sum(Shape.Product(Nil) :: types)), "1 + A")

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

  private def excluded(reason: Exclusion)(fqn: String, arity: Int): Entry =
    Entry(fqn, numbered("T", arity), Excluded(reason), "")

  private def unmodelled(fqn: String): Entry = Entry(fqn, Nil, Unmodelled, "")

  // The declarations outside the model's own whose values are a sequence, a finite map or set, or
  // a scalar the model already reads (JDK 25 and scala-library 3.9.0).
  private object Equivalent {

    val sequences: List[String] = List(
      "scala.collection.Seq",
      "scala.collection.IndexedSeq",
      "scala.collection.Iterable",
      "scala.collection.immutable.Iterable",
      "scala.collection.mutable.Seq",
      "scala.collection.mutable.IndexedSeq",
      "scala.collection.mutable.Iterable",
      "scala.collection.mutable.Buffer",
      "scala.collection.mutable.ArrayBuffer",
      "scala.collection.mutable.ListBuffer",
      "java.lang.Iterable",
      "java.util.Collection",
      "java.util.List",
      "java.util.ArrayList",
      "java.util.LinkedList",
      "java.util.Vector",
      "java.util.stream.Stream"
    )

    val maps: List[String] = List(
      "scala.collection.Map",
      "scala.collection.mutable.Map",
      "scala.collection.concurrent.Map",
      "java.util.Map",
      "java.util.HashMap",
      "java.util.LinkedHashMap",
      "java.util.TreeMap",
      "java.util.concurrent.ConcurrentMap",
      "java.util.concurrent.ConcurrentHashMap"
    )

    val sets: List[String] = List(
      "scala.collection.Set",
      "scala.collection.mutable.Set",
      "java.util.Set",
      "java.util.HashSet",
      "java.util.LinkedHashSet",
      "java.util.TreeSet"
    )

    val boxed: List[(String, String)] = List(
      "java.lang.Boolean" -> "Boolean",
      "java.lang.Byte" -> "Byte",
      "java.lang.Short" -> "Short",
      "java.lang.Character" -> "Char",
      "java.lang.Integer" -> "Int",
      "java.lang.Long" -> "Long",
      "java.lang.Float" -> "Float",
      "java.lang.Double" -> "Double"
    )

  }

  private val finiteMap = "scala.collection.immutable.Map"
  private val finiteSet = "scala.collection.immutable.Set"

  // A finite partial function from its key, read as the function it tabulates.
  private def map(fqn: String): Entry =
    Entry(
      fqn,
      List("K", "V"),
      Tabulated(types => Shape.Function(List(types.head), Shape.Sum(List(unit, types(1))))),
      "K => 1 + V"
    )

  // A finite subset of its element, read as its indicator function.
  private def set(fqn: String): Entry =
    Entry(
      fqn,
      List("E"),
      Tabulated(types => Shape.Function(types, Shape.Sum(List(unit, unit)))),
      "E => 2"
    )

  // A boxed scalar: the scalar it boxes (`null` is outside the model).
  private def boxed(fqn: String, scalar: String): Entry =
    Entry(fqn, Nil, Structural(_ => BuiltinTypes.scalarShape(scalar)), scalar)

  private def transparent(fqn: String): Entry = Entry(fqn, Nil, Transparent, "ω")

  // The bytes a single-pass stream or buffer yields.
  private def bytes(fqn: String): Entry =
    Entry(fqn, Nil, Structural(_ => Shape.Repeated(BuiltinTypes.byte)), "Σₙ Byteⁿ")

  private val unit = Shape.Product(Nil)

  // Evidence whose methods test a value's class or enumerate a type's cases and fields: a body
  // holding `ClassTag[A]` (or a `Manifest`) can match `case a: A` and so tell `A = Int` from
  // `A = String`, which no parametric body can (Scala 3 reference, "TypeTest"); every
  // `scala.deriving.Mirror` names its type's cases or fields, and `Mirror.Sum.ordinal` tells them
  // apart.
  private val Revealed = List(
    "scala.reflect.ClassTag" -> 1,
    "scala.reflect.TypeTest" -> 2,
    "scala.reflect.Typeable" -> 1,
    "scala.reflect.Manifest" -> 1,
    "scala.deriving.Mirror" -> 0,
    "scala.deriving.Mirror.Product" -> 0,
    "scala.deriving.Mirror.Sum" -> 0,
    "scala.deriving.Mirror.Singleton" -> 0,
    "scala.deriving.Mirror.Of" -> 1,
    "scala.deriving.Mirror.ProductOf" -> 1,
    "scala.deriving.Mirror.SumOf" -> 1
  )

  // The `scala.quoted` types and their arity (scala3-library 3.9.0).
  private val Quoted: List[(String, Int)] =
    List("Expr" -> 1, "ExprMap" -> 0, "FromExpr" -> 1, "Quotes" -> 0, "ToExpr" -> 1, "Type" -> 1)

  // The throwables the root imports bind. Their API and why it reads as data:
  // docs/research/pure-code.md §3.
  private val Exceptions: List[String] = BaseTypes.throwables.values.toList.sorted

  // The arbitrary-precision numbers.
  private val Numbers = List("scala.math.BigInt", "scala.math.BigDecimal")

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
      optional("scala.Option"),
      Entry("scala.util.Either", List("L", "R"), Structural(Shape.Sum(_)), "L + R")
    ) ++ List(
      "scala.collection.immutable.List",
      "scala.collection.immutable.Vector",
      "scala.collection.immutable.IndexedSeq",
      "scala.collection.immutable.Seq",
      "scala.collection.immutable.ArraySeq",
      "scala.Array",
      "scala.collection.Iterator",
      "scala.collection.IterableOnce",
      "java.util.Iterator"
    ).map(sequence) ++
      // Declarations of other packages that hold the same value space (v1.md §2): mutability is a
      // `var` read as a `val`, and ordering or representation is not observed — each keeps its own
      // name, so a rigid constructor's argument still tells them apart.
      Equivalent.sequences.map(sequence) ++ (finiteMap :: Equivalent.maps).map(map) ++
      (finiteSet :: Equivalent.sets).map(set) ++ Equivalent.boxed.map(boxed) ++ List(
        optional("java.util.Optional"),
        Entry(
          "java.util.function.Function",
          List("T", "R"),
          Structural(types => Shape.Function(types.init, types.last)),
          "T => R"
        )
      ) ++ List(
        bytes("java.nio.ByteBuffer"),
        bytes("java.io.InputStream"),
        excluded(Collected)("java.lang.ref.WeakReference", 1),
        excluded(Collected)("java.lang.ref.ReferenceQueue", 1)
      ) ++ Quoted.map((name, arity) => excluded(Reflection)(s"scala.quoted.$name", arity)) ++
      Revealed.map((fqn, arity) => excluded(Revealing)(fqn, arity)) ++
      (Exceptions ++ Numbers).map(transparent) ++
      List("scala.Product", "scala.Equals", "java.lang.CharSequence").map(unmodelled)

  private val byName: Map[String, Entry] = entries.map(entry => entry.fqn -> entry).toMap

  /** The entry of a fully qualified name. */
  def entry(name: String): Option[Entry] = byName.get(name)

  private def outside(entry: Entry, why: String): String =
    s"outside the counting model: ${entry.fqn} ($why)"

  private def unmodelled(entry: Entry): String =
    s"open stdlib type with unmodelled members: ${entry.fqn}"

  /** What a model spelling applied to these arguments denotes, or why it is not counted. */
  def read(entry: Entry, args: List[Shape]): Either[String, Shape] = entry.reading match {
    case Excluded(reason)                               => Left(outside(entry, reason.why))
    case Unmodelled                                     => Left(unmodelled(entry))
    case _ if args.isEmpty && entry.parameters.nonEmpty =>
      Left(
        s"higher-kinded argument: ${entry.fqn} (type constructor ${entry.lambda}, " +
          "needs higher-kinded instantiation)"
      )
    case _ if args.size != entry.parameters.size =>
      Left(s"type argument arity: ${entry.fqn} takes ${entry.parameters.size}, given ${args.size}")
    case Structural(shape)                                 => Right(shape(args))
    case Tabulated(_) if !args.forall(ClosedSpaces.closed) =>
      Left(
        s"${entry.fqn} over an open type is not read (its keys are tested for an equality no " +
          "parametric program has)"
      )
    // Keys and values are content, as a sequence's elements are: the function the container
    // tabulates is no arrow of the program, so their tops take the container's place.
    case Tabulated(shape) => Right(shape(args.map(TopTypes.keyed)))
    case Transparent      => Right(Shape.Ground(entry.fqn, Card.Omega))
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

}
