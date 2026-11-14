package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** Type parameters of the compiled front end beyond atoms and rigid constructors: a type
  * constructor written as a class's argument (`Shapes.argument`), and a bounded proper parameter,
  * whose bound is an ambient proof read from its symbol, as the old front end reads it from syntax
  * (`Bounds`). Also the types written otherwise than as a class: literal, singleton, union and
  * intersection types. Every expected count is derived in its example's comment, and the scalameta
  * front end is asked for the same rows.
  */
class TypeParametersSpec extends Specification {

  def is = s2"""
  A written type constructor is applied where its parameter is
    a closure is read with the binders of both sites           $closures
    a constructor row reads a rigid constructor's positions    $constructors
    a capability's own type parameters are read as before      $polymorphic
    a class's parameter bound to a value is not a type          $rebound
    a lambda over a symbol bound again is read where written   $lambdas
    a bounded or variant constructor is refused where it is used $kinds
    a class whose value ignores an unread argument is counted    $unused
  A bound of a proper type parameter is a proof beside it
    an upper bound views the values, a lower bound builds them  $proofs
    a bound the model does not read is refused, as before       $refused
    a top inside a bound is later                               $later
  A literal or singleton type has one value
    a literal is a constant, a key by its class and value       $literals
    a parameter's, an object's or a dependency's path            $singletons
  A union is a tagged sum, an intersection its narrowest part
    alternatives told apart by class or constant are summed      $unions
    a part above another is dropped; distinct objects are empty  $intersections
  """

  private val text =
    """package bnd
      |
      |trait Show { def show: Boolean }
      |sealed trait Two
      |case object One extends Two
      |case object Other extends Two
      |object Use {
      |  def up[A <: Boolean](a: A): Boolean = a
      |  def keep[A <: Boolean](a: A): A = a
      |  def low[A >: Boolean](a: A): A = a
      |  def ref[A <: AnyRef](a: A): A = a
      |  def chain[A <: B, B](a: A): B = a
      |  def fam[A <: Two](a: A): Two = a
      |  def open[A <: Show](a: A): Boolean = a.show
      |  def rec[A <: Option[A]](a: A): Boolean = true
      |  def same[A, B >: A <: A](x: B, y: B): A = x
      |  def top[A >: AnyRef](): A = ???
      |  def fn[A <: (Boolean => Any)](a: A): Boolean = true
      |}
      |final class Box[A <: Boolean](val a: A) { def get: Boolean = a }
      |final case class Wrap[F[_], A](v: F[A]) {
      |  object Ops { def back(w: Wrap[[X] =>> (A, X), Boolean]): A = w.v._1 }
      |}
      |trait Fk[F[_], G[_]] {
      |  def apply[A](fa: F[A]): G[A]
      |  def or[H[_]](h: Fk[H, G]): Fk[[X] =>> Either[F[X], H[X]], G] = ???
      |}
      |type Same[X] = X
      |final case class Kl[F[_], A, B](run: A => F[B]) { def plain: Kl[Same, A, F[B]] = Kl[Same, A, F[B]](run) }
      |final class Hold[F[_]](val b: Boolean)
      |final class Held[F[_], A](val fa: F[A])
      |final class Co[+F[_]](val a: F[Option[Int]], val b: F[Some[Int]])
      |trait Lift { def lift[M](m: M): M; def get: Boolean = true }
      |trait Pick { def pick[M](x: M, y: M): M }
      |object Poly { def tag(p: Pick, a: Boolean): Boolean = a }
      |trait Narrow { def narrow[M <: String](m: M): M; def get: Boolean = true }
      |trait Apply { def lift[G[_]](g: G[Int]): G[Int]; def get: Boolean = true }
      |object Token
      |object Full { def m: Int = 1 }
      |case object Green
      |final class Tok(val t: Token.type)
      |final class Dep { def f(x: Boolean, y: x.type): Boolean = x }
      |object Lit {
      |  val depth: 128 = 128
      |  def flag(x: true): Boolean = x
      |  def same[F[_]](x: F[1]): F[1] = x
      |  def other[F[_]](x: F[1]): F[2] = ???
      |}
      |object Sing {
      |  def pick(x: Boolean, y: x.type): x.type = y
      |  def tok(): Token.type = Token
      |  def green(): Green.type = Green
      |  def full(): Full.type = Full
      |  def either(e: Either.type, b: Boolean): Boolean = b
      |}
      |object Mark
      |object Un {
      |  def lits(x: 1 | 2): 1 | 2 = x
      |  def same(b: Boolean | Boolean): Boolean = b
      |  def tagged(x: Boolean | Token.type): Boolean | Token.type = x
      |  def top(x: Int | AnyRef): Boolean = true
      |  def empty(x: Nothing | Null): Boolean = true
      |  def alone[A](x: A | Nothing): A = x
      |  def overlap[A](x: A | Boolean): Boolean = true
      |  def boxed(x: Int | java.lang.Integer): Boolean = true
      |}
      |object Meet {
      |  def keep(x: Boolean & Any): Boolean = x
      |  def path[A](x: A, y: A & x.type): A = y
      |  def none(x: Token.type & Mark.type): Boolean = true
      |  def open(x: Show & Two): Boolean = true
      |  def single(x: Boolean & Singleton): Boolean = x
      |}
      |trait Vary { def g[G[+_]](x: G[Int]): G[Int]; def get: Boolean = true }
      |object Kind {
      |  def bounded[S[T] <: Show](s: S[Int]): S[Int] = s
      |  def variant[F[+_]](x: F[Int]): F[Int] = x
      |  def unused[F[+_]](b: Boolean): Boolean = b
      |}
      |final class Cache[K, V] { private val m: Map[K, V] = Map.empty; def get(k: K): Option[V] = m.get(k) }
      |final class Box1[A](val a: A)
      |class Graph { final class Node(val b: Boolean); val cache: Cache[String, List[Node]] = new Cache; def box: Box1[Node] = ??? }
      |object Hk {
      |  def wrap(w: Wrap[[X] =>> Either[Boolean, X], Boolean]): Boolean = true
      |  def held[G[_]](w: Wrap[G, Int]): G[Int] = w.v
      |}
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("TypeParameters.scala" -> text)
  private lazy val rows = TastyFrontEnd.rows(input)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name.endsWith(name))
      .get
      .count

  // `wrap`'s argument holds an `Either[Boolean, Boolean]`, 4 values: it is one of 2^4.
  private def closures = count("Hk.wrap") must_== Count.Finite(16)

  // `Hold`'s constructor takes a Boolean, 2, whatever F is. `Held`'s one position is a rigid
  // application: the shape model counts what the caller supplies, `fa` alone, 1.
  // `Co`'s F is covariant: `F[Some[Int]]` is an `F[Option[Int]]` too, which a rigid application
  // does not say, so its row is not read.
  private def constructors =
    (List("Hold.<init>", "Held.<init>").map(count) must_== List(2, 1).map(Count.Finite(_)))
      .and(
        uncovered("Co.<init>") must startWith(
          "a type parameter of another kind than an invariant F[_]"
        )
      )

  // `lift: ∀M. M => M` is the identity, which tells nothing: `Lift.get` is one of 2 Booleans.
  // `pick: ∀M. (M, M) => M` picks its first or second argument, so a `Pick` has 2 values; with
  // `a`, `tag` has 4 inputs and is one of 2^4 functions to Boolean. A bounded binder the type
  // meets, and one the result applies, are not read, as before.
  private def polymorphic =
    (List("Lift.get", "Poly.tag").map(count) must_== List(2, 16).map(Count.Finite(_)))
      .and(uncovered("Narrow.get") must contain("polymorphic capability binder: M (bounded)"))
      .and(uncovered("Apply.get") must contain("the result instantiates the binder"))

  // In `held`, the field `v: F[A]` is `G` applied to `A`, an uninterpreted application whose
  // argument is read as a type, but `A` is bound to the values of `Int`: a class's parameter bound
  // to a value is not read as a type. It is uncovered.
  private def rebound =
    uncovered("Hk.held") must startWith("a class's parameter bound to a value")

  // A class's parameter passed back to the class: in `Fk.or` the written `Either[F[X], H[X]]` means
  // the `F` of the enclosing `Fk`, which reading the result `Fk[..., G]` binds to the lambda. The
  // lambda's body is read where it was written, `X` being the argument `apply` gives it: the goal
  // is `∀A. Either[F[A], H[A]] => G[A]`, and each side has one way to a `G[A]`, `this` or `h`: 1.
  // `Kl.plain` is `A => Same[F[B]]`, `A => F[B]`, which only `run` gives: 1. `Wrap.Ops.back`'s `w`
  // holds an `(A, Boolean)` whose first part is the one `A` the body has (the enclosing `Wrap`'s
  // `v` is a rigid `F[A]`): 1.
  private def lambdas =
    List("Fk.or", "Kl.plain", "Wrap.Ops.back").map(count) must_== List.fill(3)(Count.Finite(1))

  // A constructor with a bound has its bound's members, and one with a variant parameter relates
  // its applications by subtyping: neither is read, as before, where a type names it. `unused`
  // names no such parameter: one of the 2^2 functions of `b`. A capability's own constructor the
  // old front end reads as rigid whatever it declares (`MemberSyntax`): `Vary.get` is not read yet.
  private def kinds =
    (count("Kind.bounded").toString must contain("bounded higher-kinded parameter"))
      .and(count("Kind.variant").toString must contain("variant higher-kinded parameter"))
      .and(count("Kind.unused") must_== Count.Finite(4))
      .and(uncovered("Vary.get") must startWith("a type parameter of another kind"))

  // `Node` is a class nested in a class: not read. `Cache`'s one public constructor takes nothing,
  // so it builds one value whatever its arguments are (rule B): `cache` is 1. `Box1`'s field is
  // its argument: `box` keeps the argument's reason, as before.
  private def unused =
    (count("Graph.cache") must_== Count.Finite(1))
      .and(uncovered("Graph.box") must startWith("a class nested in a class or a body"))

  // A row's reason when it is uncovered, without the prefix that says so.
  private def uncovered(name: String): String = count(name) match {
    case Count.Unresolved(List(reason)) if reason.startsWith(s"${TastyFrontEnd.Uncovered}: ") =>
      reason.stripPrefix(s"${TastyFrontEnd.Uncovered}: ")
    case other => other.toString
  }

  // `up`: `a` read as a Boolean is 2 values, so 2^2 functions to Boolean. `keep`: an A is never
  // built from a Boolean, so `a` alone, 1. `low`: an A is `a`, `true` or `false`, 3. `ref`: an
  // `AnyRef` bound says nothing, 1. `chain`: `a` upcast, the only B, 1. `fam`: `a` read as a Two
  // is one of 2 cases, and the result is either case: 2^2. `open`: `a` read as a Show, a
  // capability record of one Boolean member, 2^2. `Box.get`: its field read as a Boolean, 2^2.
  private def proofs =
    List("Use.up", "Use.keep", "Use.low", "Use.ref", "Use.chain", "Use.fam", "Use.open", "Box.get")
      .map(count) must_== List(4, 1, 3, 1, 1, 4, 4, 4).map(Count.Finite(_))

  // `A <: Option[A]` describes a type by a fixpoint: the model's refusal. `same`'s B lies between
  // A and A, no cycle of upper bounds: x or y upcast, 2.
  private def refused =
    (count("Use.rec").toString must contain("recursive bound not read"))
      .and(count("Use.same") must_== Count.Finite(2))

  // A top in a bound is read by the place the bound puts it, which a proof does not carry: `A >:
  // AnyRef` would build one value where any object is a result (ω), and `A <: (Boolean => Any)`
  // would hide the output top of the function a is read as. (`AnyVal` above `Boolean` below is a
  // no-op: `TypeFormsSpec`.)
  private def later =
    List("Use.top", "Use.fn").map(uncovered) must_== List(
      "a bound that holds a top type: A",
      "a bound that holds a top type: A"
    )

  // `depth` has the one value 128. `flag`'s input has one value, so its count is the Booleans,
  // 2. `F[1]` and `F[2]` are uninterpreted applications to two types: `same` returns `x`, 1;
  // nothing builds an `F[2]`, 0.
  private def literals =
    List("Lit.depth", "Lit.flag", "Lit.same", "Lit.other").map(count) must_==
      List(1, 2, 1, 0).map(Count.Finite(_))

  // A singleton goal is terminal: `pick`, `tok` and `green`, 1 each (a case object's parents
  // and members are the compiler's: no member a body reads). `Tok` holds the one value of
  // `Token.type`: one Tok. `Either.type` is one value, so `either` is one of 2^2 functions of
  // `b`. `Dep.f`'s `y` is `x` itself, nothing more: the 2^2 functions of `x`. An object with
  // members is not read as one value, as before.
  private def singletons =
    (List("Sing.pick", "Sing.tok", "Sing.green", "Tok.<init>", "Sing.either", "Dep.f").map(
      count
    ) must_== List(1, 1, 1, 1, 4, 4).map(Count.Finite(_)))
      .and(count("Sing.full").toString must contain("singleton module member environment"))

  // `lits`: two constants in, two out, 2^2. `same`: one Boolean, 2^2. `tagged`: a Boolean or the
  // object, 3 values in and out, 3^3. `top`: `AnyRef` reads as `Any`, which takes `Int`: one
  // input value, 2 Booleans. `empty`: no input value, one function. `alone`: `x` is the one A,
  // 1. `overlap`: an `A` may be a
  // Boolean, and an Int is an Integer at runtime: neither is told apart, refused as before.
  private def unions =
    (List("Un.lits", "Un.same", "Un.tagged", "Un.top", "Un.empty", "Un.alone").map(count) must_==
      List(4, 4, 27, 2, 1, 1).map(Count.Finite(_)))
      .and(
        List("Un.overlap", "Un.boxed")
          .map(count(_).toString)
          .must(contain(contain("overlap and discrimination proof")).forall)
      )

  // `keep`: `Boolean & Any` is Boolean, 2^2. `path`: `y` is `x` itself, the one A in reach, 1.
  // `none`: two objects share no value, so no input and one function. `open`: `Two` is sealed and
  // neither of its objects is a `Show`, so no value is both (the closed world): one function. A
  // Boolean claimed a singleton is refused.
  private def intersections =
    (List("Meet.keep", "Meet.path", "Meet.none", "Meet.open").map(count) must_==
      List(4, 1, 1, 1).map(Count.Finite(_)))
      .and(count("Meet.single").toString must contain("requires a proven singleton identity"))

}
