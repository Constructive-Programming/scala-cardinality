package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Count
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
    Sequences
      List, Vector, Seq, IndexedSeq and ArraySeq introduce Nil/Cons  $introduction
      an Array of a concrete element is a sequence                  $arrays
      an Array of a class or of an empty type is a sequence         $classArrays
      an Array of a type parameter needs its ClassTag               $abstractArray
      an Array of a singleton needs its widened type's ClassTag     $singletonArrays
      an override over List keeps its identity                      $fromList
    Excluded and open declarations
      single-pass and equality-built types are outside the model     $excluded
      compile-time reflection is outside the model                  $quoted
      Java collections name their FQN; throwables are data           $open
      a final class the model does not read is not called open      $finalUnmodelled
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
      an unsupplied wildcard refuses every model name               $wildcardRefusal
      a supplied namespace answers instead                          $wildcardSupplied
      a Java collection wildcard shadows List                       $javaWildcard
      listed stdlib namespaces shadow no model name                 $listedNamespaces
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
      Product is an open parent, Serializable a marker              $markers
      IterableOnce declares no type member                          $iterableTypes
      a model parent under an unsupplied wildcard is not the model  $wildcardParent
      a sealed template whose subtypes are fixed is a fixed receiver $sealedParent
      a parent whose standing its own search asks is doubted        $selfReachingParent
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

  def excluded = {
    val code = """import java.nio.ByteBuffer
      |object X {
      |  def it[A](xs: List[A]): Iterator[A] = ???
      |  def m[K, V](k: K): Map[K, V] = ???
      |  def s[A](a: A): Set[A] = ???
      |  def buf(b: ByteBuffer): Boolean = ???
      |}""".stripMargin
    refused(count(code, "X.it"), "outside the counting model: scala.collection.Iterator")
      .and(
        refused(count(code, "X.m"), "outside the counting model: scala.collection.immutable.Map")
      )
      .and(
        refused(count(code, "X.s"), "outside the counting model: scala.collection.immutable.Set")
      )
      .and(refused(count(code, "X.buf"), "outside the counting model: java.nio.ByteBuffer"))
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
      |}""".stripMargin
    // A throwable is built from any message (register D26): `new IllegalArgumentException(s)` for
    // each string s, which `getMessage` tells apart — ω, `java.util`'s `NoSuchElementException` too.
    (count(code, "O.boom") === Count.Countable)
      .and(refused(count(code, "O.f"), "open stdlib type with unmodelled members: java.util.List"))
      .and(count(code, "O.missing") === Count.Countable)
  }

  // `java.lang.Integer` is a final class whose members the table does not hold: unmodelled, not
  // open. `scala.Equals` is an open trait.
  def finalUnmodelled = {
    val code = """object O {
      |  def boxed(i: Integer): Boolean = ???
      |  def eq(e: Equals): Boolean = ???
      |}""".stripMargin
    refused(count(code, "O.boxed"), "stdlib type with unmodelled members: java.lang.Integer")
      .and(clean(count(code, "O.boxed"), "open stdlib type"))
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

  // With nothing in scope that could bind it, and beside a wildcard of a namespace nobody supplied.
  def nowhere =
    (count("object U { def f(x: Frame): Boolean = ??? }", "U.f") ===
      Count.Unresolved(List(s"declaration not supplied: Frame (${StdlibModel.undeclared})")))
      .and(
        refused(
          count("import kyo.*\nobject U { def f(x: Frame): Boolean = ??? }", "U.f"),
          s"declaration not supplied: Frame (${StdlibModel.undeclared}, may be bound through " +
            "kyo, which is not supplied (wildcard import of kyo))"
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

  // A top-level package `util` outside the sources outranks `scala.util` (Scala 3.9.0 fixture:
  // `summon[util.Either[Int, Int] =:= _root_.util.Either[Int, Int]]` compiles beside one), so only
  // `scala.util.Either` and `_root_.scala.util.Either` are the model's: Left(a) or Right(a). A
  // root-anchored simple name is a top-level declaration, never a root import's.
  def relativePaths = {
    val code = """object P {
      |  def rel[A](a: A): util.Either[A, A] = ???
      |  def full[A](a: A): scala.util.Either[A, A] = ???
      |  def rooted[A](a: A): _root_.scala.util.Either[A, A] = ???
      |  def top[A](a: A): _root_.Option[A] = ???
      |}""".stripMargin
    refused(
      count(code, "P.rel"),
      "builtin name util.Either may be shadowed by a top-level package outside the sources: util"
    )
      .and(count(code, "P.full") === Count.Finite(2))
      .and(count(code, "P.rooted") === Count.Finite(2))
      .and(refused(count(code, "P.top"), "declaration not supplied: _root_.Option"))
  }

  // `lib` may declare its own Option, Unit or Int: a wildcard import outranks the root imports.
  def wildcardRefusal = {
    val code =
      "package p\nimport lib.*\nobject T { def f[A](x: A): Option[A] = ???; def u(x: Int): Unit = () }"
    refused(
      count(code, "p.T.f"),
      "builtin name Option may be shadowed through a reference not supplied: lib (wildcard import of lib)"
    ).and(
      refused(
        count(code, "p.T.u"),
        "builtin name Unit may be shadowed through a reference not supplied: lib"
      )
    )
  }

  // Supplied, `lib`'s own Option[A] holds two A's: (x, x) only.
  def wildcardSupplied =
    countIn(
      "Use.scala" -> "package p\nimport lib.*\nobject T { def f[A](x: A): Option[A] = ??? }",
      "Lib.scala" -> "package lib\nfinal case class Option[A](first: A, second: A)"
    )("p.T.f") === Count.Finite(1)

  // `java.util.*` brings `java.util.List`, which outranks `scala.List`.
  def javaWildcard =
    refused(
      count("import java.util.*\nobject W { def l[A](a: A): List[A] = ??? }", "W.l"),
      "builtin name List may be shadowed through a reference not supplied: java.util"
    )

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
  // not rebound by an import. Scala 3.9.0 types `g` below with `lib.quotes.reflect.Int` when `lib`
  // declares it, and `quotes` with no import is a top-level package outside the sources.
  def listedHead = {
    def g(imports: String) = count(s"$imports\nobject T { def g(): Int = ??? }", "T.g")
    def reflected(imports: String) = count(
      s"$imports\nobject T {\n  def m(using scala.quoted.Quotes): Boolean = {\n" +
        "    import quotes.reflect.*\n    def g(): Int = ???\n    true\n  }\n}",
      "T.m.g"
    )
    val shadowed = "builtin name Int may be shadowed through"
    refused(g("import lib.quotes\nimport quotes.reflect.*"), shadowed)
      .and(refused(g("import scala.quoted.*\nimport _root_.quotes.reflect.*"), shadowed))
      .and(refused(g("import quotes.reflect.*"), shadowed))
      .and(refused(g("import lib.scala\nimport scala.quoted.*"), shadowed))
      .and(refused(g("import lib.scala\nimport scala.jdk.CollectionConverters.*"), shadowed))
      .and(refused(reflected("import lib.quotes"), shadowed))
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
      "case class K[A](a: A) extends Base { def g(x: A): A = ??? }\n" +
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
        "import lib.Base\nclass Outer extends Base { object In { def h[A](a: A): A = ??? } }",
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

  def markers =
    refused(
      count("abstract class C extends Product { def f(b: Boolean): Boolean = ??? }", "C.f"),
      "open stdlib parent with unmodelled members: scala.Product"
    ).and(
      count("class C extends Serializable { def f(b: Boolean): Boolean = ??? }", "C.f") ===
        Count.Finite(4)
    )

  // Inside a template extending IterableOnce, `Int` is scala.Int and `Mine` the package's alias (a
  // 3.9.0 fixture checks `summon[Mine =:= Boolean]` and `summon[Int =:= scala.Int]` there): the
  // parent declares no type member.
  def iterableTypes =
    count(
      "type Mine = Boolean\nfinal class T extends IterableOnce[Int] {\n" +
        "  def iterator: Iterator[Int] = ???\n  def f(m: Mine): Int = ???\n}",
      "T.f"
    ) === Count.Finite(BigInt(2).pow(64))

  // `lib` may declare its own `Serializable` or `IterableOnce` (with `type Int = Boolean`, Scala
  // 3.9.0 compiles `def g(): Int = true` in `O`): such a parent is neither the model's marker nor
  // its summary — absent where the open owner is measured, an unsupplied parent where `Int` is
  // read.
  def wildcardParent = {
    val parent = "inherited parent declaration not supplied: Serializable (may be bound through lib"
    refused(
      count("import lib.*\ntrait T extends Serializable { def f[A](a: A): A = ??? }", "T.f"),
      parent
    ).and(
      refused(
        countIn(
          "A.scala" -> "package r\nimport lib.*\ntrait T extends IterableOnce[String]",
          "B.scala" -> "package r\nobject O extends T { def g(): Int = ??? }"
        )("r.O.g"),
        "inherited type member not resolved: Int (parent IterableOnce[String], which may be bound " +
          "through lib, which is not supplied"
      )
    )
  }

  // P's instances are D1's, a final class: no subclass overrides `stepper`, so the receiver's own
  // family is what stays unread. With a non-final child, a subclass may.
  def sealedParent = {
    val body = "{\n  def iterator: Iterator[Int] = ???\n  override def knownSize: Int = 0\n" +
      "  def g(b: Boolean): String = ???\n}"
    val closed = s"sealed trait P extends IterableOnce[Int] $body\nfinal class D1 extends P"
    val open = s"sealed trait P extends IterableOnce[Int] $body\nclass D1 extends P"
    val overridable = "inherited stdlib member a subclass may override"
    refused(count(closed, "P.g"), "receiver is a sealed family this measurement does not split: P")
      .and(clean(count(closed, "P.g"), overridable))
      .and(refused(count(open, "P.g"), overridable))
  }

  // Whether `Serializable` in `T`'s parents is the marker depends on what `import O.*` brings, and
  // `O` inherits `T`'s parents: the search for that channel meets itself. It is doubted, not taken
  // as answered (Scala 3.9.0 compiles this fixture with `scala.Serializable`; true counts 4, 2^(2^33)
  // and 4) — the doubt is sound and says why. `import O.*` brings what `O` inherits, so the doubt
  // reaches `Boolean` and `Int` through `O`'s ancestry. Each lookup recomputes it, so the later ones
  // in the same scope (`T.h`'s parent first, then `U.f` and `U.g` reaching it) are refused the same
  // way: a result computed while that search was under way is never cached as the scope's own
  // reading.
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
    val route =
      "through Serializable, which is not resolved (wildcard import of O, in a search that reaches " +
        "itself)"
    (counts.map(_._1).filter(Set("T.h", "U.f", "U.g")) === List("T.h", "U.f", "U.g"))
      .and(
        refused(
          found("T.h"),
          "inherited type member not resolved: Boolean (parent Serializable, whose declaration"
        )
      )
      .and(refused(found("U.f"), s"Boolean (parent Serializable, which may be bound $route)"))
      .and(refused(found("U.g"), s"Boolean (parent Serializable, which may be bound $route)"))
      .and(refused(found("U.g"), s"Int (parent Serializable, which may be bound $route)"))
  }

}
