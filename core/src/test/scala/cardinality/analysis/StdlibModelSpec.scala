package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.analysis.inhabitation.NaturalSyntax.pow
import org.specs2.Specification
import org.specs2.execute.Result

/** The stdlib model: library declarations read by fully qualified name, after every binding of the
  * sources; honest reasons for names nobody supplied; and the parents outside the sources an open
  * template's environment inherits. Every fixture with a number is valid Scala 3.9.0 (compiled with
  * stub bodies this session); the refusal fixtures name a dependency nobody supplied.
  */
class StdlibModelSpec extends Specification {

  def is = s2"""
    Products, sums and functions
      Left and Right hold one argument                              $leftRight
      Some, EmptyTuple and Option                                   $someEmpty
      TupleN, scala.TupleN and tuple syntax are one product          $tuples
      FunctionN is the function it spells                           $functions
      tuple spellings keep one override identity                    $tupleIdentity
      FunctionN and arrow spellings keep one override identity      $functionIdentity
      Either spellings are one evidence identity                    $eitherIdentity
    Singleton terms
      None and Nil singletons have their one value                  $singletonTerms
      an unresolved path's singleton is the one value it names      $unresolvedSingleton
      an inaccessible source module's singleton stays refused       $inaccessibleSingleton
    Sequences
      List, Vector, Seq, IndexedSeq and ArraySeq introduce Nil/Cons  $introduction
      an Array of a concrete element is a sequence                  $arrays
      an Array of a class or of an empty type is a sequence         $classArrays
      an Array of a type parameter needs its ClassTag               $abstractArray
      an Array of a singleton needs its widened type's ClassTag     $singletonArrays
      an override over List keeps its identity                      $fromList
    Excluded and open declarations
      single-pass types are read as the sequence they yield         $singlePass
      Map and Set are their abstract values over closed arguments    $keyed
      Map and Set over an open argument are refused                  $openKeys
      compile-time reflection is outside the model                  $quoted
      Java collections name their FQN; throwables, BigInt are data   $open
      boxed scalars and Optional count as what they hold            $finalUnmodelled
      an unapplied entry is an operator, applied where it is read   $unapplied
      a union does not discriminate a model sequence's class        $unionClass
    Absent declarations
      a named import names the path it binds                         $namedImport
      a selection through an import names the full path              $selected
      a renamed import names the path and the name                   $renamed
      a name found nowhere says the information is absent           $nowhere
      a gap inside the supplied sources stays unresolved            $gap
      a named import of a builtin reads the builtin                 $importedBuiltin
      an import's own head may be rebound                           $importedHead
    Shadowing
      a source declaration shadows the model                        $sourceShadows
      an import of a model path reads the model                     $importedModel
      only fully qualified paths reach the model                    $relativePaths
      an unsupplied wildcard rebinds no model name                  $wildcardRefusal
      a supplied namespace answers instead                          $wildcardSupplied
      a JDK wildcard reads List by its rooted name, supplied or not  $javaWildcard
      listed stdlib namespaces shadow no model name                 $listedNamespaces
      dependency namespaces are listed from their artifacts         $dependencyNamespaces
      a listed namespace's member reads the model                   $namespaceMember
      a listed namespace counts only through the library's head     $listedHead
      a listed namespace's member outranks another file's member    $listedPrecedence
      a supplied namespace's missing member names the wildcard      $wildcardOrigin
      a package scala of the sources outranks the root package      $scalaSubpackage
    Foreign parents
      an open template's foreign parent is reported                 $openParent
      a fixed receiver's parents are implemented                    $fixedParent
      a constructor chooses only its parameters                     $constructorExempt
      a nested object reads its open owner's parents                $nestedObject
      IterableOnce leaves iterator to the sources                   $iterableOnce
      IterableOnce declares no type member                          $iterableTypes
      a model parent under an unsupplied wildcard is the model      $wildcardParent
      a sealed template whose subtypes are fixed is a fixed receiver $sealedParent
      a parent whose standing its own search asks is the model's    $selfReachingParent
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def countIn(sources: (String, String)*)(name: String): Count =
    MethodAnalysis.analyze(inputs(sources*)).find(_.name == name).get.count

  private def count(code: String, name: String): Count = countIn("Example.scala" -> code)(name)

  private def refused(found: Count, fragment: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  private def clean(found: Count, fragment: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beFalse
      case _                         => ok
    }

  // `Right[L, R]` holds an R: `Right(r.value)` is the only Either[L2, R] (no L2 exists); a Left
  // holds an L. Either[R, R] from an R: Left(r) or Right(r).
  def leftRight = {
    val code = """object R {
      |  def widenLeft[L, R, L2](r: Right[L, R]): Either[L2, R] = ???
      |  def widenRight[L, R, R2](l: Left[L, R]): Either[L, R2] = ???
      |  def both[L, R](r: Right[L, R]): Either[R, R] = ???
      |}""".stripMargin
    (count(code, "R.widenLeft") === Count.Finite(1))
      .and(count(code, "R.widenRight") === Count.Finite(1))
      .and(count(code, "R.both") === Count.Finite(2))
  }

  // Some(a) only; () only; None or the given Some.
  def someEmpty = {
    val code = """object S {
      |  def some[A](a: A): Some[A] = ???
      |  def empty: EmptyTuple = ???
      |  def opt[A](s: Some[A]): Option[A] = ???
      |}""".stripMargin
    (count(code, "S.some") === Count.Finite(1))
      .and(count(code, "S.empty") === Count.Finite(1))
      .and(count(code, "S.opt") === Count.Finite(2))
  }

  // swap: (p._2, p._1); dup: (a, a); pick: either field; three fields of one A: 3.
  def tuples = {
    val code = """object T {
      |  def swap[A, B](p: Tuple2[A, B]): (B, A) = ???
      |  def dup[A](a: A): scala.Tuple2[A, A] = ???
      |  def pick[A](p: Tuple2[A, A]): A = ???
      |  def third[A](p: Tuple3[A, A, A]): A = ???
      |}""".stripMargin
    (count(code, "T.swap") === Count.Finite(1))
      .and(count(code, "T.dup") === Count.Finite(1))
      .and(count(code, "T.pick") === Count.Finite(2))
      .and(count(code, "T.third") === Count.Finite(3))
  }

  // f(a, a), f(f(a, a), a), … — ω; a thunk is its one value; scala.Function1 is `A => B`.
  def functions = {
    val code = """object F {
      |  def fold[A](f: Function2[A, A, A], a: A): A = ???
      |  def force[A](f: Function0[A]): A = ???
      |  def apply[A, B](f: scala.Function1[A, B], a: A): B = ???
      |}""".stripMargin
    (count(code, "F.fold") === Count.Countable)
      .and(count(code, "F.force") === Count.Finite(1))
      .and(count(code, "F.apply") === Count.Finite(1))
  }

  // The implementation spells `(X, A)` where the trait applies `Tuple2`: one member, read once — the
  // reviewed count stays 1 (fa._2).
  def tupleIdentity =
    count(
      """trait Accessor[F[_, _]] { def get[X, A](fa: F[X, A]): A }
        |object Accessor {
        |  given tupleAccessor: Accessor[Tuple2] with
        |    def get[X, A](fa: (X, A)): A = fa._2
        |}""".stripMargin,
      "Accessor.tupleAccessor.get"
    ) === Count.Finite(1)

  // scalac accepts an override that respells `Unit => Unit` as `Function1[Unit, Unit]`, so the two
  // are one member: `Impl.pick` is not a capability of itself. `o` has one value, so the answers
  // are a and b — 2, as for `Base.pick`, never ω.
  def functionIdentity = {
    val code = """trait Base[A] { def pick(a: A, b: A, o: Left[Unit, Unit => Unit]): A }
      |trait Impl[A] extends Base[A] {
      |  override def pick(a: A, b: A, o: Left[Unit, Function1[Unit, Unit]]): A
      |}
      |trait Hof[A] { def pick(a: A, b: A, h: Unit => Unit): A }
      |trait HofImpl[A] extends Hof[A] {
      |  override def pick(a: A, b: A, h: Function1[Unit, Unit]): A
      |}""".stripMargin
    (count(code, "Impl.pick") === Count.Finite(2))
      .and(count(code, "HofImpl.pick") === Count.Finite(2))
  }

  // `Either` is `scala.util.Either`, however qualified: two anonymous evidences of one type are
  // ambiguous to `summon` (Scala 3.9.0: "Ambiguous given instances"), never two distinct proofs.
  def eitherIdentity = {
    def pair(second: String) =
      count(s"object E { def f[A](using Either[A, A])(using $second[A, A]): A = ??? }", "E.f")
    List("scala.util.Either", "scala.Either", "_root_.scala.Either", "_root_.scala.util.Either")
      .map(spelling => refused(pair(spelling), "ambiguous anonymous evidence"))
      .reduce(_.and(_))
  }

  // A singleton type's only value is its path's (draft specification ch. 3): `None.type` is
  // inhabited by `None` alone, so a goal of it is 1 — the implementation returning the value. The
  // table gives the library's two parameterless root-imported objects a canonical identity, so
  // both spellings of one object are that one value. A path outside the sources is read the same
  // way (D-terminal): the written `.type` is a stable path, the sources typecheck, and the one
  // value it names is the implementation. A source module the use site may not reach keeps its
  // refusal: the reading would need the module's member environment, not just its existence.
  def singletonTerms = {
    val code = "object N { def nn: None.type = ???; def nil: Nil.type = ??? }"
    (count(code, "N.nn") === Count.Finite(1))
      .and(count(code, "N.nil") === Count.Finite(1))
      .and(count("object N { def q: scala.None.type = ??? }", "N.q") === Count.Finite(1))
  }

  def unresolvedSingleton =
    count("object N { def wide: far.away.Unsupplied.type = ??? }", "N.wide") === Count.Finite(1)

  def inaccessibleSingleton = {
    val code = "object O { private object H }\nobject N { def out: O.H.type = ??? }"
    refused(count(code, "N.out"), "inaccessible or ambiguous singleton path: O.H")
  }

  // Nil only without an element; with one, every length; List[Nothing] has only Nil.
  def introduction = {
    val code = """import scala.collection.immutable.ArraySeq
      |object S {
      |  def none[A]: List[A] = ???
      |  def some[A](a: A): Vector[A] = ???
      |  def seq[A](a: A): Seq[A] = ???
      |  def indexed[A]: IndexedSeq[A] = ???
      |  def wrapped[A](a: A): ArraySeq[A] = ???
      |  def nothing: List[Nothing] = ???
      |}""".stripMargin
    (count(code, "S.none") === Count.Finite(1))
      .and(count(code, "S.some") === Count.Countable)
      .and(count(code, "S.seq") === Count.Countable)
      .and(count(code, "S.indexed") === Count.Finite(1))
      .and(count(code, "S.wrapped") === Count.Countable)
      .and(count(code, "S.nothing") === Count.Finite(1))
  }

  // Closed elements are transparent data: every length of Byte literals (ω); an Array[Any] is a
  // sequence of `()`, one per length (ω).
  def arrays = {
    val code = """object A {
      |  def bytes(n: Int): Array[Byte] = ???
      |  def anys(n: Int): Array[Any] = ???
      |}""".stripMargin
    (count(code, "A.bytes") === Count.Countable).and(count(code, "A.anys") === Count.Countable)
  }

  // Scala 3.9.0 builds `new Array[E](0)` for every E but a type parameter or an abstract member —
  // `Null`, an empty sealed trait, `Box[A]` and `List[A]` included (fixtures): an empty element
  // type leaves only the empty array (1; Boolean => 2 beside it), and an array of a class is a
  // sequence like any other — every length of x (ω), headOption over one (c(0) = 1, c(1) = 2: ω).
  def classArrays = {
    val code = """sealed trait Void
      |final case class Box[A](a: A)
      |object A {
      |  def empty(b: Boolean): Array[Null] = ???
      |  def void(xs: Array[Void]): Boolean = ???
      |  def wrap[A](x: Box[A]): Array[Box[A]] = ???
      |  def lists[A](xs: List[A]): Array[List[A]] = ???
      |  def read[A](xs: Array[Box[A]]): Option[Box[A]] = ???
      |}""".stripMargin
    (count(code, "A.empty") === Count.Finite(1))
      .and(count(code, "A.void") === Count.Finite(2))
      .and(count(code, "A.wrap") === Count.Countable)
      .and(count(code, "A.lists") === Count.Countable)
      .and(count(code, "A.read") === Count.Countable)
  }

  // `Array(a)` and `new Array[A](0)` both need a `ClassTag[A]` the signature does not supply; the
  // reading has no polarity, so an input — which needs no ClassTag — is refused too, and the
  // reason says so.
  def abstractArray = {
    val code = """object A {
      |  def wrap[A](a: A): Array[A] = ???
      |  def read[A](xs: Array[A]): Boolean = ???
      |}""".stripMargin
    List("A.wrap", "A.read")
      .map(name =>
        refused(
          count(code, name),
          "Array of an abstract element type is not read: scala.Array (building one needs " +
            "ClassTag evidence, and the reading has no polarity)"
        )
      )
      .reduce(_.and(_))
  }

  private val rigidArray = "Array of an abstract element type is not read"

  // Review fixtures arr11–arr13 (Scala 3.9.0): `x.type` for `x: A` (or a field `a: A`) has no
  // ClassTag — `new Array[x.type](0)`, `Array.empty` and `Array(x)` are E172 — so no total program
  // returns `Array[x.type]` and `opt` can only be None (true counts 0 and 1): refused, never ω. A
  // singleton of a class type, of a String and an object's singleton type have one: ω (every
  // length of the one value).
  def singletonArrays = {
    val code = """final case class Box[A](a: A)
      |object O
      |final class C[A](val a: A) { def u: Array[a.type] = ??? }
      |object T {
      |  def single[A](x: A): Array[x.type] = ???
      |  def opt[A](x: A): Option[Array[x.type]] = ???
      |  def boxed[A](x: Box[A]): Array[x.type] = ???
      |  def text(x: String): Array[x.type] = ???
      |  def module(): Array[O.type] = ???
      |}""".stripMargin
    refused(count(code, "T.single"), rigidArray)
      .and(refused(count(code, "T.opt"), rigidArray))
      .and(refused(count(code, "C.u"), rigidArray))
      .and(count(code, "T.boxed") === Count.Countable)
      .and(count(code, "T.text") === Count.Countable)
      .and(count(code, "T.module") === Count.Countable)
  }

  // `forList` implements the trait's `fromList` (List keys alike in both): ω by sequence
  // elimination — Nil at length 0, every list of e₁ at length 1.
  def fromList =
    count(
      """trait FromList[F[_]] { def fromList[A](xs: List[A]): F[A] }
        |object FromList {
        |  given forList: FromList[List] with
        |    def fromList[A](xs: List[A]): List[A] = xs
        |}""".stripMargin,
      "FromList.forList.fromList"
    ) === Count.Countable

  // Reading is no effect (v1.md §2), so a single-pass type is the sequence it yields, `Σₙ Eⁿ`:
  // from a List, every Iterator of its elements by length (ω, as for a List result); an Iterator
  // of Nothing is the empty one (1); a Boolean of an IterableOnce[Boolean] reads a finite prefix,
  // one of ω programs. A ByteBuffer or an InputStream is the bytes it yields, a closed space of ω
  // values: ω Booleans of one, ω streams from nothing.
  def singlePass = {
    val code = """import java.nio.ByteBuffer
      |import java.io.InputStream
      |object X {
      |  def it[A](xs: List[A]): Iterator[A] = ???
      |  def none[A](xs: Iterator[A]): Iterator[Nothing] = ???
      |  def once(xs: IterableOnce[Boolean]): Boolean = ???
      |  def jit[A](xs: java.util.Iterator[A]): List[A] = ???
      |  def buf(b: ByteBuffer): Boolean = ???
      |  def in(): InputStream = ???
      |}""".stripMargin
    (count(code, "X.it") === Count.Countable)
      .and(count(code, "X.none") === Count.Finite(1))
      .and(count(code, "X.once") === Count.Countable)
      .and(count(code, "X.jit") === Count.Countable)
      .and(count(code, "X.buf") === Count.Countable)
      .and(count(code, "X.in") === Count.Countable)
  }

  // Key equality is lawful (v1.md §2): a Map[K, V] is a finite partial function, (|V| + 1)^|K|
  // values for a finite K, and a Set[E] a finite subset, 2^|E|. Over `1 | 2`: Set has 4 values, so
  // a Set from an `x: 1 | 2` has 4^2 = 16 implementations; Map[1 | 2, Boolean] has 3^2 = 9 values,
  // and a Boolean of a Map[1 | 2, 1 | 2] (3^2 = 9 values) 2^9 = 512. Set[Nothing] holds only the
  // empty set (a Boolean of it: 2), and so does Map[String, Nothing] (1). Over an infinite key the
  // finite maps and sets are countably many: ω. A rigid F tells Set[String] from List[String] (0:
  // no program between them) and keeps one key per Set spelling (1). Set[Boolean] has 2^2 = 4
  // values, and Map[Color, Boolean] over a two-case enum 3^2 = 9. A top among the keys keeps the
  // container's polarity (v1.md §2): as a parameter it is one value, so a Set[Any] input is empty
  // or not (2 values, 2^2 = 4 Booleans of it) and a Map[Any, Boolean] input 3 (2^3 = 8); as a
  // result it is ε₀, so Set[Any] is 2^ε₀ and Map[Any, Boolean] 3^ε₀, both ε₀ in the size algebra.
  def keyed = {
    val code = """object X {
      |  def s(): Set[1 | 2] = ???
      |  def from(x: 1 | 2): Set[1 | 2] = ???
      |  def m(): Map[1 | 2, Boolean] = ???
      |  def read(m: Map[1 | 2, 1 | 2]): Boolean = ???
      |  def empty(s: Set[Nothing]): Boolean = ???
      |  def nothing(): Map[String, Nothing] = ???
      |  def strings(): Set[String] = ???
      |  def keys(m: Map[String, Int]): Boolean = ???
      |  def asList[F[_]](x: F[Set[String]]): F[List[String]] = ???
      |  def same[F[_]](x: F[Set[String]]): F[scala.collection.immutable.Set[String]] = x
      |  def flags(): Set[Boolean] = ???
      |  def colors(): Map[Color, Boolean] = ???
      |  def setIn(s: Set[Any]): Boolean = ???
      |  def setOut(): Set[Any] = ???
      |  def mapIn(m: Map[Any, Boolean]): Boolean = ???
      |  def mapOut(): Map[Any, Boolean] = ???
      |}
      |enum Color { case Red, Green }""".stripMargin
    (count(code, "X.s") === Count.Finite(4))
      .and(count(code, "X.from") === Count.Finite(16))
      .and(count(code, "X.m") === Count.Finite(9))
      .and(count(code, "X.read") === Count.Finite(512))
      .and(count(code, "X.empty") === Count.Finite(2))
      .and(count(code, "X.nothing") === Count.Finite(1))
      .and(count(code, "X.strings") === Count.Countable)
      .and(count(code, "X.keys") === Count.Countable)
      .and(count(code, "X.asList") === Count.Finite(0))
      .and(count(code, "X.same") === Count.Finite(1))
      .and(count(code, "X.flags") === Count.Finite(4))
      .and(count(code, "X.colors") === Count.Finite(9))
      .and(count(code, "X.setIn") === Count.Finite(4))
      .and(count(code, "X.setOut").render === "ε₀")
      .and(count(code, "X.mapIn") === Count.Finite(8))
      .and(count(code, "X.mapOut").render === "ε₀")
  }

  // Over a type parameter a Map or Set tests its keys for an equality no parametric program has
  // (`Set[A]` from two A's may be {a}, {b} or {a, b} only if a ≠ b can be asked): refused.
  def openKeys = {
    val code = """object X {
      |  def m[K, V](k: K): Map[K, V] = ???
      |  def s[A](a: A, b: A): Set[A] = ???
      |  def v[V](m: Map[String, V]): Option[V] = ???
      |}""".stripMargin
    val open = "over an open type is not read"
    refused(count(code, "X.m"), s"scala.collection.immutable.Map $open")
      .and(refused(count(code, "X.s"), s"scala.collection.immutable.Set $open"))
      .and(refused(count(code, "X.v"), s"scala.collection.immutable.Map $open"))
  }

  // A one-valued goal keeps its one implementation beside an excluded input; an empty one does
  // not: it needs every input's inhabitation.
  def quoted = {
    val code = """object Q {
      |  def q(using scala.quoted.Quotes): Option[Int] = ???
      |  def log(msg: Boolean)(using scala.quoted.Quotes): Unit = ()
      |  def fail(using scala.quoted.Quotes): Nothing = ???
      |}""".stripMargin
    refused(count(code, "Q.q"), "outside the counting model: scala.quoted.Quotes")
      .and(count(code, "Q.log") === Count.Finite(1))
      .and(refused(count(code, "Q.fail"), "outside the counting model: scala.quoted.Quotes"))
  }

  def open = {
    val code = """import java.util.List
      |object O {
      |  def boom(flag: Boolean): IllegalArgumentException = ???
      |  def f[A](a: A): List[A] = ???
      |  def boxed(i: Integer): Boolean = ???
      |  def missing(b: Boolean): NoSuchElementException = ???
      |  def big(b: Boolean): BigInt = ???
      |  def decimal(b: Boolean): scala.math.BigDecimal = ???
      |}""".stripMargin
    // A throwable is built from any message (register D26): `new IllegalArgumentException(s)` for
    // each string s, which `getMessage` tells apart — ω, `java.util`'s `NoSuchElementException` too.
    // `java.util.List` holds the sequences a Scala List does (v1.md §2): every list of a, ω.
    (count(code, "O.boom") === Count.Countable)
      .and(count(code, "O.f") === Count.Countable)
      .and(count(code, "O.missing") === Count.Countable)
      // An arbitrary-precision number is one of countably many literals, as a `String` is.
      .and(count(code, "O.big") === Count.Countable)
      .and(count(code, "O.decimal") === Count.Countable)
  }

  // A boxed `java.lang.Integer` holds the values an Int does (`null` aside): a Boolean of one is
  // 2^(2^32), and `F[Integer]` is still not `F[Int]` (0). `scala.Equals` is an open trait.
  def finalUnmodelled = {
    val code = """object O {
      |  def boxed(i: Integer): Boolean = ???
      |  def keyed[F[_]](x: F[Integer]): F[Int] = ???
      |  def opt(o: java.util.Optional[Boolean]): Option[Boolean] = ???
      |  def eq(e: Equals): Boolean = ???
      |}""".stripMargin
    (count(code, "O.boxed").render === "2^(2^32)")
      .and(count(code, "O.keyed") === Count.Finite(0))
      .and(count(code, "O.opt") === Count.Finite(27))
      .and(refused(count(code, "O.eq"), "open stdlib type with unmodelled members: scala.Equals"))
  }

  // An unapplied entry in a higher-kinded position is an operator (D14), applied in its own scope:
  // `Box[Either]` holds an `Either[Unit, Unit]` (2 values: 2^2 functions to Boolean), `One[Option]`
  // an `Option[Boolean]` (3^3 functions to itself). Read where a proper type is expected — no
  // Scala program — it names its type lambda.
  def unapplied = {
    val code = """final case class Box[F[_, _]](v: F[Unit, Unit])
      |final case class One[F[_]](v: F[Boolean])
      |object U {
      |  def box(b: Box[Either]): Boolean = ???
      |  def opt(o: One[Option]): Option[Boolean] = ???
      |  def proper(x: Option): Boolean = ???
      |}""".stripMargin
    (count(code, "U.box") === Count.Finite(4))
      .and(count(code, "U.opt") === Count.Finite(27))
      .and(
        refused(
          count(code, "U.proper"),
          "higher-kinded argument: scala.Option (type constructor [A] =>> 1 + A, " +
            "needs higher-kinded instantiation)"
        )
      )
  }

  // `Vector[Int] | String` would be a sum only with Vector's runtime class, which A2's table does
  // not hold: refused, saying the model holds the declaration (not that it is absent).
  def unionClass =
    refused(
      count("object U { def v(x: Vector[Int] | String): Boolean = ??? }", "U.v"),
      "Vector has no runtime class this reading tells apart (stdlib model)"
    )

  def namedImport =
    count("import org.apache.avro.Schema\nobject U { def f(s: Schema): Schema = s }", "U.f") ===
      Count.Unresolved(
        List("declaration not supplied: org.apache.avro.Schema (named import Schema)")
      )

  def selected =
    refused(
      count(
        "import org.apache.avro.Schema\nobject U { def g(t: Schema.Type): Boolean = ??? }",
        "U.g"
      ),
      "declaration not supplied: org.apache.avro.Schema.Type (selected through named import Schema)"
    )

  def renamed =
    count("import zz.{Widget as W}\nobject U { def f(w: W): W = w }", "U.f") ===
      Count.Unresolved(List("declaration not supplied: zz.Widget (renamed import W)"))

  // With nothing in scope that could bind it, and beside a wildcard of a listed dependency
  // namespace: kyo is listed from its artifacts (kyo 1.0.0-RC7), so the wildcard's doubt names
  // the declaration it binds instead of the whole namespace.
  def nowhere =
    (count("object U { def f(x: Frame): Boolean = ??? }", "U.f") ===
      Count.Unresolved(List(s"declaration not supplied: Frame (${StdlibModel.undeclared})")))
      .and(
        refused(
          count("import kyo.*\nobject U { def f(x: Frame): Boolean = ??? }", "U.f"),
          "declaration not supplied: kyo.Frame (wildcard import)"
        )
      )

  // `Hidden` is declared — in a package `b` does not open: a reading gap, not absent information.
  def gap =
    countIn(
      "A.scala" -> "package a\nfinal case class Hidden()",
      "B.scala" -> "package b\nobject U { def f(h: Hidden): Hidden = h }"
    )("b.U.f") === Count.Unresolved(List("unresolved type: Hidden"))

  // `import scala.Unit` binds the builtin Unit, `import java.lang.String` the String: 1 each, and
  // `import scala.Boolean` the bit (2).
  def importedBuiltin = {
    val code = """import scala.Unit
      |import scala.Boolean
      |import java.lang.String
      |import scala.Predef.String as PS
      |object T {
      |  def u[A](a: A): Unit = ()
      |  def b(): Boolean = true
      |  def s[A](x: String, a: A): A = a
      |  def p[A](x: PS, a: A): A = a
      |}""".stripMargin
    (count(code, "T.u") === Count.Finite(1))
      .and(count(code, "T.b") === Count.Finite(2))
      .and(count(code, "T.s") === Count.Finite(1))
      .and(count(code, "T.p") === Count.Finite(1))
  }

  // `import lib.scala` rebinds the head of the next import: its `List` is `lib.scala`'s, absent.
  def importedHead =
    count(
      "import lib.scala\nimport scala.collection.immutable.List\n" +
        "object T { def l[A](a: A): List[A] = ??? }",
      "T.l"
    ) === Count.Unresolved(
      List(
        "declaration not supplied: lib.scala.collection.immutable.List " +
          "(selected through named import scala)"
      )
    )

  // The program's `List` holds exactly one A: a.
  def sourceShadows =
    count(
      "final case class List[A](only: A)\nobject S { def f[A](a: A): List[A] = ??? }",
      "S.f"
    ) ===
      Count.Finite(1)

  // `import scala.util.Either` binds the library's Either: Left(a) or Right(a).
  def importedModel =
    count("import scala.util.Either\nobject E { def e[A](a: A): Either[A, A] = ??? }", "E.e") ===
      Count.Finite(2)

  // A top-level package `util` outside the sources would outrank `scala.util` (Scala 3.9.0 fixture:
  // `summon[util.Either[Int, Int] =:= _root_.util.Either[Int, Int]]` compiles beside one), but none
  // is assumed (the never-shadowed assumption), so `util.Either` is the model's like
  // `scala.util.Either` and `_root_.scala.util.Either`: Left(a) or Right(a). A root-anchored simple
  // name is a top-level declaration, never a root import's.
  def relativePaths = {
    val code = """object P {
      |  def rel[A](a: A): util.Either[A, A] = ???
      |  def full[A](a: A): scala.util.Either[A, A] = ???
      |  def rooted[A](a: A): _root_.scala.util.Either[A, A] = ???
      |  def top[A](a: A): _root_.Option[A] = ???
      |}""".stripMargin
    (count(code, "P.rel") === Count.Finite(2))
      .and(count(code, "P.full") === Count.Finite(2))
      .and(count(code, "P.rooted") === Count.Finite(2))
      .and(refused(count(code, "P.top"), "declaration not supplied: _root_.Option"))
  }

  // A wildcard import outranks the root imports, but an unsupplied `lib` is assumed to declare no
  // Option, Unit or Int (the never-shadowed assumption): Some(x) or None, and the one Unit.
  def wildcardRefusal = {
    val code =
      "package p\nimport lib.*\nobject T { def f[A](x: A): Option[A] = ???; def u(x: Int): Unit = () }"
    (count(code, "p.T.f") === Count.Finite(2)).and(count(code, "p.T.u") === Count.Finite(1))
  }

  // Supplied, `lib`'s own Option[A] holds two A's: (x, x) only.
  def wildcardSupplied =
    countIn(
      "Use.scala" -> "package p\nimport lib.*\nobject T { def f[A](x: A): Option[A] = ??? }",
      "Lib.scala" -> "package lib\nfinal case class Option[A](first: A, second: A)"
    )("p.T.f") === Count.Finite(1)

  // `java.util.*` brings `java.util.List`, which outranks `scala.List`: `List` is that rooted name,
  // whose values are the sequences a Scala List holds (ω from an a). `F[List[A]]` there is
  // `F[java.util.List[A]]`, not the Scala one (0), and the rooted spelling is the same type (1).
  // `java.awt.List` is a component the model does not read: with no declaration of it supplied, it
  // is refused by its rooted name; with one (a dependency stub of `java.desktop`), it is read as
  // that declaration, like any supplied class — a final class of one Boolean field: 2 values, so
  // 2^2 maps to Boolean.
  def javaWildcard = {
    val awt = "package java.awt\nfinal class List(val multiple: Boolean)"
    val use = "import java.awt.*\nobject A { def l(a: List): Boolean = ??? }"
    val code = """import java.util.*
      |object W {
      |  def l[A](a: A): List[A] = ???
      |  def k[F[_], A](x: F[List[A]]): F[scala.List[A]] = ???
      |  def j[F[_], A](x: F[List[A]]): F[java.util.List[A]] = x
      |}""".stripMargin
    (count(code, "W.l") === Count.Countable)
      .and(count(code, "W.k") === Count.Finite(0))
      .and(count(code, "W.j") === Count.Finite(1))
      .and(
        refused(
          count("import java.awt.*\nobject A { def l[A](a: A): List[A] = ??? }", "A.l"),
          "java.awt.List"
        )
      )
      .and(
        countIn("Use.scala" -> use, "tasty/java.desktop/java/awt/List.scala" -> awt)(
          "A.l"
        ) === Count.Finite(4)
      )
  }

  // scala.quoted, scala.jdk.CollectionConverters and quotes.reflect declare no model spelling
  // (Scala 3.9.0 fixtures: `summon[Option[Int] =:= scala.Option[Int]]` and the like compile under
  // each): Option[Boolean] from a Boolean is 3^2 = 9 under all three.
  def listedNamespaces = {
    val quoted = "import scala.quoted.*\nobject Q { def f(b: Boolean): Option[Boolean] = ??? }"
    val converters =
      "import scala.jdk.CollectionConverters.*\nobject C { def f(b: Boolean): Option[Boolean] = ??? }"
    val reflect = """import scala.quoted.*
      |object R {
      |  def m(using Quotes): Boolean = {
      |    import quotes.reflect.*
      |    def f(b: Boolean): Option[Boolean] = ???
      |    true
      |  }
      |}""".stripMargin
    (count(quoted, "Q.f") === Count.Finite(9))
      .and(count(converters, "C.f") === Count.Finite(9))
      .and(refused(count(reflect, "R.m.f"), "outside the counting model: scala.quoted.Quotes"))
      .and(clean(count(reflect, "R.m.f"), "builtin name"))
  }

  // `Quotes` under `scala.quoted.*` is the library's: excluded. `TypeRepr` under `quotes.reflect.*`
  // is not enumerated: absent, with the namespace named.
  def namespaceMember = {
    val code = """import scala.quoted.*
      |object Q {
      |  def g(q: Quotes): Boolean = ???
      |  def m(using Quotes): Boolean = {
      |    import quotes.reflect.*
      |    def t(r: TypeRepr): Boolean = ???
      |    true
      |  }
      |}""".stripMargin
    refused(count(code, "Q.g"), "outside the counting model: scala.quoted.Quotes")
      .and(
        refused(
          count(code, "Q.m.t"),
          "declaration not supplied: TypeRepr (" + StdlibModel.undeclared +
            ", may be a member of quotes.reflect (compile-time reflection)"
        )
      )
  }

  // A listed namespace counts only where its first name is the library's: `quotes` bound to
  // `scala.quoted.quotes` (a named import or a `scala.quoted` wildcard before the clause), `scala`
  // not rebound by an import. Elsewhere `quotes.reflect` is `lib.quotes.reflect` or a top-level
  // package outside the sources, which is assumed to declare no `Int` (the never-shadowed
  // assumption): any Int, 2^32 (inside `m`, the captured `Quotes` refuses `g` for its own reason).
  // So is a `scala.quoted` whose `scala` is `lib.scala`.
  def listedHead = {
    def g(imports: String) = count(s"$imports\nobject T { def g(): Int = ??? }", "T.g")
    def reflected(imports: String) = count(
      s"$imports\nobject T {\n  def m(using scala.quoted.Quotes): Boolean = {\n" +
        "    import quotes.reflect.*\n    def g(): Int = ???\n    true\n  }\n}",
      "T.m.g"
    )
    val anyInt = Count.Finite(BigInt(2).pow(32))
    (g("import lib.quotes\nimport quotes.reflect.*") === anyInt)
      .and(g("import scala.quoted.*\nimport _root_.quotes.reflect.*") === anyInt)
      .and(g("import quotes.reflect.*") === anyInt)
      .and(g("import lib.scala\nimport scala.quoted.*") === anyInt)
      .and(g("import lib.scala\nimport scala.jdk.CollectionConverters.*") === anyInt)
      .and(refused(reflected("import lib.quotes"), "outside the counting model: scala.quoted"))
      .and(clean(reflected("import lib.quotes"), "builtin name"))
      .and(clean(reflected("import scala.quoted.quotes"), "builtin name"))
      .and(clean(reflected("import scala.quoted.*"), "builtin name"))
  }

  // Review fixture q1 (Scala 3.9.0 reads `e.a` in B.scala as `scala.quoted.Expr`'s member, E008): a
  // wildcard import outranks a package member of another file, so `Expr` is the excluded library
  // type, never `q1.Expr` (2). A name the namespace does not declare is the package's: `Pair` = 2.
  def listedPrecedence = {
    val use = countIn(
      "A.scala" -> """package q1
        |final case class Expr[A](a: A, b: A)
        |final case class Pair[A](a: A, b: A)""".stripMargin,
      "B.scala" -> """package q1
        |import scala.quoted.*
        |object O {
        |  def f[A](e: Expr[A]): A = ???
        |  def g[A](p: Pair[A]): A = ???
        |}""".stripMargin
    )
    refused(
      use("q1.O.f"),
      "outside the counting model: scala.quoted.Expr (compile-time reflection)"
    )
      .and(use("q1.O.g") === Count.Finite(2))
  }

  // The dependency namespaces the corpora's wildcards open, listed from their artifacts
  // (cats-core 2.13.0, kyo 1.0.0-RC7): the root spellings survive their wildcards — Int's
  // 2^32 values and b's 2 give Boolean^(2^33) — a member a namespace declares is refused as
  // imported, and a name it does not declare is not shadowed.
  def dependencyNamespaces = {
    val cats = "import cats.syntax.functor.*\nobject C { def f(i: Int, b: Boolean): Boolean = ??? }"
    val kyo = "import _root_.kyo.*\nobject K { def f(i: Int, b: Boolean): Boolean = ??? }"
    val member = "import _root_.kyo.*\nobject K { def f(s: Stream): Boolean = ??? }"
    (count(cats, "C.f") === Count.Symbolic(pow(2, BigInt("8589934592"))))
      .and(count(kyo, "K.f") === Count.Symbolic(pow(2, BigInt("8589934592"))))
      .and(refused(count(member, "K.f"), "declaration not supplied: kyo.Stream (wildcard import)"))
  }

  // `lib.sub` is a package the sources supply, `Missing` a member of it they do not: absent,
  // through the wildcard that reached the package.
  def wildcardOrigin =
    refused(
      countIn(
        "Lib.scala" -> "package lib.sub\nfinal case class Present(x: Int)",
        "Use.scala" -> "package app\nimport lib.*\nobject U { def f(t: sub.Missing): Boolean = ??? }"
      )("app.U.f"),
      "declaration not supplied: lib.sub.Missing (selected through wildcard import)"
    )

  // A supplied package `r7.scala` is what `scala` names inside `package r7` (its unsupplied part
  // may declare `Option`): refused. Without one, no package outside the sources is assumed to be
  // named `scala` inside a source package.
  def scalaSubpackage =
    refused(
      countIn(
        "Lib.scala" -> "package r7.scala\ntype Int = Boolean",
        "Use.scala" -> "package r7\nobject T { def h[A](a: A): scala.Option[A] = ??? }"
      )("r7.T.h"),
      "builtin name scala.Option may be shadowed by a declaration: package r7.scala (a package of the sources)"
    )

  // `Base` may declare abstract members a subclass of the trait provides: capabilities nobody can
  // list.
  def openParent =
    refused(
      count("import lib.Base\ntrait T extends Base { def f[A](a: A): A = ??? }", "T.f"),
      "inherited parent declaration not supplied: lib.Base (named import Base)"
    ).and(
      refused(
        count("trait T extends Base { def f[A](a: A): A = ??? }", "T.f"),
        s"inherited parent declaration not supplied: Base (${StdlibModel.undeclared})"
      )
    )

  // An object, a given and a final class implement every inherited member: concrete code adds no
  // choice — 1 (a), 1 (a).
  def fixedParent =
    (count("import lib.Base\nobject O extends Base { def f[A](a: A): A = ??? }", "O.f") ===
      Count.Finite(1))
      .and(
        count("import lib.Base\nfinal class C extends Base { def f[A](a: A): A = ??? }", "C.f") ===
          Count.Finite(1)
      )

  // `Err(m: Boolean)` has 2 values whatever RuntimeException declares: its parameters are read
  // outside its body. `In(b: Boolean)` is read inside `Outer`'s, where the unsupplied `lib.Base` may
  // declare `type Boolean = Int` (2^32), so no number stands (it was 2 before register D29). A
  // non-final case class's method is not exempt.
  def constructorExempt = {
    val code = "import lib.Base\nfinal class Err(m: Boolean) extends RuntimeException\n" +
      "case class K[A](a: A) extends Base { def g(x: A): A = ??? }\nclass K2 extends K[Int](1)\n" +
      "trait Outer extends Base { final case class In(b: Boolean) }"
    (count(code, "Err.<init>") === Count.Finite(2))
      .and(count(code, "K.<init>") === Count.Finite(1))
      .and(
        refused(
          count(code, "Outer.In.<init>"),
          "inherited type member not resolved: Boolean (unsupplied parent Base)"
        )
      )
      .and(refused(count(code, "K.g"), "inherited parent declaration not supplied: lib.Base"))
  }

  def nestedObject =
    refused(
      count(
        "import lib.Base\nclass Outer extends Base { object In { def h[A](a: A): A = ??? } }\n" +
          "class Outer2 extends Outer",
        "Outer.In.h"
      ),
      "inherited parent declaration not supplied: lib.Base"
    )

  // Without `iterator` the trait leaves an Iterator-valued member to its subclasses; with it alone,
  // a subclass may still override `knownSize` (an Int f could read) or `stepper`. With all three
  // defined, f is Boolean => Boolean: 4.
  def iterableOnce = {
    val bare = "trait T extends IterableOnce[Int] { def f(b: Boolean): Boolean = ??? }"
    val implemented = "trait T extends IterableOnce[Int] {\n" +
      "  def iterator: Iterator[Int] = ???\n  def f(b: Boolean): Boolean = ???\n}"
    // Final, so no subclass of the open trait replaces what f may read.
    val complete = """import scala.collection.{Stepper, StepperShape}
      |trait T extends IterableOnce[Int] {
      |  final def iterator: Iterator[Int] = ???
      |  final override def knownSize: Int = 0
      |  final override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Int, S]): S = ???
      |  def f(b: Boolean): Boolean = ???
      |}""".stripMargin
    refused(
      count(bare, "T.f"),
      "inherited parent declaration not implemented: scala.collection.IterableOnce.iterator"
    ).and(
      refused(
        count(implemented, "T.f"),
        "inherited stdlib member a subclass may override: scala.collection.IterableOnce.knownSize"
      )
    ).and(
      refused(
        count(implemented, "T.f"),
        "inherited stdlib member a subclass may override: scala.collection.IterableOnce.stepper"
      )
    ).and(count(complete, "T.f") === Count.Finite(4))
  }

  // Inside a template extending IterableOnce, `Int` is scala.Int and `Mine` the package's alias (a
  // 3.9.0 fixture checks `summon[Mine =:= Boolean]` and `summon[Int =:= scala.Int]` there): the
  // parent declares no type member.
  def iterableTypes =
    count(
      "type Mine = Boolean\nfinal class T extends IterableOnce[Int] {\n" +
        "  def iterator: Iterator[Int] = ???\n  def f(m: Mine): Int = ???\n}",
      "T.f"
    ) === Count.Finite(BigInt(2).pow(64))

  // `lib` could declare its own `Serializable` or `IterableOnce` (with `type Int = Boolean`, Scala
  // 3.9.0 compiles `def g(): Int = true` in `O`), but an unsupplied namespace is assumed to rebind
  // no builtin name (the never-shadowed assumption): the parents are the model's marker and
  // summary, so `f` is a alone (1) and `g` any Int (2^32).
  def wildcardParent =
    (count("import lib.*\ntrait T extends Serializable { def f[A](a: A): A = ??? }", "T.f") ===
      Count.Finite(1)).and(
      countIn(
        "A.scala" -> "package r\nimport lib.*\ntrait T extends IterableOnce[String]",
        "B.scala" -> "package r\nobject O extends T { def g(): Int = ??? }"
      )("r.O.g") === Count.Finite(BigInt(2).pow(32))
    )

  // P's instances are D1's, a final class: no subclass overrides `stepper`, so the receiver's own
  // family is what stays unread. With a non-final child, a subclass may.
  def sealedParent = {
    val body = "{\n  def iterator: Iterator[Int] = ???\n  override def knownSize: Int = 0\n" +
      "  def g(b: Boolean): String = ???\n}"
    val closed = s"sealed trait P extends IterableOnce[Int] $body\nfinal class D1 extends P"
    val open =
      s"sealed trait P extends IterableOnce[Int] $body\nclass D1 extends P\nclass D2 extends D1"
    val overridable = "inherited stdlib member a subclass may override"
    refused(count(closed, "P.g"), "receiver is a sealed family this measurement does not split: P")
      .and(clean(count(closed, "P.g"), overridable))
      .and(refused(count(open, "P.g"), overridable))
  }

  // Whether `Serializable` in `T`'s parents is the marker depends on what `import O.*` brings, and
  // `O` inherits `T`'s parents: the search for that channel meets itself. A search that meets itself
  // finds no declaration of the name there, and an unresolved step rebinds no builtin name (the
  // never-shadowed assumption), so the counts are Scala 3.9.0's reading with `scala.Serializable`:
  // 4, 4 and 2^(2^33). Each lookup recomputes the channel, in one analysis whose lookups share the
  // index's caches, and every one agrees.
  def selfReachingParent = {
    val code = """import O.*
      |trait T extends Serializable { def h(b: Boolean): Boolean = b }
      |object O extends T
      |object U {
      |  def f(b: Boolean): Boolean = b
      |  def g(i: Int, b: Boolean): Boolean = b
      |}""".stripMargin
    // One analysis, so every lookup shares the index's caches.
    val counts = MethodAnalysis.analyze(inputs("Example.scala" -> code)).map(e => e.name -> e.count)
    val found = counts.toMap
    (counts.map(_._1).filter(Set("T.h", "U.f", "U.g")) === List("T.h", "U.f", "U.g"))
      .and(found("T.h") === Count.Finite(4))
      .and(found("U.f") === Count.Finite(4))
      .and(found("U.g").render === "2^(2^33)")
  }

}
