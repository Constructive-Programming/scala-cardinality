package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** Forms of types and type parameters in the compiled front end (group h3,
  * docs/plans/compiled-own-types.md): named tuples, wildcard arguments, class bounds beside lower
  * bounds, a constructor's bounded parameters, constructors of constructors, aliases a member's
  * bounds pin and type lambdas applied. Every expected count is derived in its example's comment.
  */
class TypeFormsSpec extends Specification {

  def is = s2"""
  Forms of types and type parameters
    a named tuple is its fields, on both sides              $namedTuples
    a match on a named tuple is not reduced                 $namedStuck
    a wildcard argument is the witness of an input package  $wildcards
    a bounded wildcard, or one returned, is no number       $wildcardsRefused
    a class bound every lower bound is an instance of       $classBounds
    a class bound a lower bound is no instance of           $classBoundsUnread
    a constructor's bounded type parameter gives its proofs $boundedConstructors
    a constructor of constructors is a rigid head          $nestedKinds
    an export's alias, a fixed projection, a lambda applied $aliases
    a sealed case's invariant parameter in a variant slot   $variance
  """

  private val text =
    """package tf
      |
      |type Fst[T] = T match { case (a, b) => a }
      |type Flags = (x: Boolean, y: Boolean)
      |type G[I] = [X, A] =>> (context: X, values: I => A)
      |object N {
      |  def pair[A, B](a: A, b: B): (x: A, y: B) = (x = a, y = b)
      |  def bool(p: (x: Boolean, y: Boolean)): Boolean = p.x
      |  def one[A](p: (x: A)): A = p.x
      |  def nest[A](p: (x: (a: A, b: A), y: Unit)): A = p.x.a
      |  def map[I, X, A, B](fa: G[I][X, A], f: A => B): G[I][X, B] = ???
      |  def stuck(p: (x: Boolean, y: Int)): Fst[(x: Boolean, y: Int)] = ???
      |}
      |final case class Packed[A, R](value: A, consume: A => R)
      |final case class Mixed[A, B, R](value: A, consume: B => R)
      |final case class Twice[A, R](left: A, right: A, consume: A => R)
      |final case class Steps[A, R](seed: A, step: A => A, consume: A => R)
      |final case class Loop[A, R](step: A => A, consume: A => R)
      |final case class Box[A](value: A)
      |type Hidden[R] = Packed[?, R]
      |final class Env[R](p: Packed[?, R]) { def run: R = ??? }
      |object W {
      |  def one[R](p: Packed[?, R]): R = ???
      |  def two[R](p: Packed[?, R], q: Packed[?, R]): R = ???
      |  def mixed[R](p: Mixed[?, ?, R]): R = ???
      |  def twice[R](p: Twice[?, R]): R = ???
      |  def alias[R](p: Hidden[R], q: Hidden[R]): R = ???
      |  def arrow[R]: Packed[?, R] => R = ???
      |  def same[R](p: Packed[?, R])(q: p.type): R = ???
      |  def box[A](box: Box[?]): A = ???
      |  def consume[R](consume: Function1[?, R]): R = ???
      |  def steps[R](p: Steps[?, R]): R = ???
      |  def loop[R](p: Loop[?, R]): R = ???
      |  def bounded[A](p: Option[? <: A]): Boolean = true
      |  def result(p: Option[?]): Option[?] = p
      |  def boundedParameter(b: Flagged[?]): Boolean = ???
      |}
      |trait Flag1 { def on: Boolean }
      |final case class Flagged[A <: Flag1](a: A)
      |sealed trait Flag
      |case object On extends Flag
      |case object Off extends Flag
      |sealed trait Sub extends Flag
      |sealed trait U0 extends Any
      |sealed trait U1 extends U0
      |case object U1a extends U1
      |sealed trait Explicit extends AnyRef
      |case object E1 extends Explicit
      |object K {
      |  def ok[A >: Flag <: AnyRef](x: A): A = x
      |  def obj[A >: Flag <: Object](x: A): A = x
      |  def boolVal[A >: Boolean <: AnyVal](x: A): A = x
      |  def matchable[A >: Flag <: Matchable](x: A): A = x
      |  def withParents[A >: Sub <: AnyRef](x: A): A = x
      |  def explicitRef[A >: Explicit <: AnyRef](x: A): A = x
      |  def anyval[A >: Flag <: AnyVal](x: A): A = x
      |  def anyref[A >: Boolean <: AnyRef](x: A): A = x
      |  def universal[A >: U1 <: AnyRef](x: A): A = x
      |  def cross[A <: AnyVal, B >: Flag <: A](x: B): A = x
      |}
      |final case class C1[A <: Int](a: A)
      |class Other { type X = Boolean }
      |final class Holder2[Prism <: Other](val p: Prism) { type Y = p.X }
      |final case class D1[A <: B, B <: C, C](a: A)
      |final case class D2[A <: B, B](a: A, b: B)
      |object O2 { type X = Nothing; final case class C[A <: X](a: A) }
      |object T {
      |  def eta[F[_[_]], G[_]](x: F[G]): F[[y] =>> G[y]] = x
      |  def named[F[_[_]]](x: F[Option]): F[[y] =>> Option[y]] = x
      |  def other[F[_[_]]](x: F[Option]): F[[y] =>> Either[Unit, y]] = ???
      |  def same[F[_[_, _]]](x: F[Either]): F[[a, b] =>> Either[a, b]] = x
      |  def swapped[F[_[_, _]]](x: F[Either]): F[[a, b] =>> Either[b, a]] = ???
      |}
      |object Types { type Option[A] = (A, A); type Pair[A] = (A, A) }
      |object Re { export Types.* }
      |object UseRe { import Re.*; def pick[A](x: Option[A]): A = x._1 }
      |object InRe { export Types.*; def pick[A](x: Option[A]): A = x._1 }
      |object Re2 { export Types.Pair }
      |object UseRe2 { def pick[A](x: Re2.Pair[A]): A = x._1 }
      |type R1[A] = { type X = A }
      |type R3 = { type X >: Unit <: Unit }
      |type R4 = { type X <: Nothing }
      |object P {
      |  def equal[A](x: R1[A]#X, y: A): A = y
      |  def exact(): R3#X = ()
      |  def bottom(): R4#X = ???
      |  def same[F[_], A](x: F[A | A]): F[A] = x
      |}
      |final case class Pair[A](x: R1[A]#X, y: A)
      |final case class Lam[A](x: ([x] =>> x)[A], y: A)
      |final case class Holder[A](f: ([X] =>> Function1[A, X])[A])
      |sealed trait VCo[+A]
      |final case class VCoK[A](a: A, b: () => A) extends VCo[A]
      |case object VCoN extends VCo[Nothing]
      |sealed trait VTwice[+A]
      |final case class VTwiceK[A](g: (A => Boolean) => Boolean) extends VTwice[A]
      |sealed trait VCon[-A]
      |final case class VConK[A](f: A => Boolean) extends VCon[A]
      |sealed trait VFn[+A]
      |final case class VFnK[A](f: A => Boolean) extends VFn[A]
      |type VFlip[X, Y] = Y => X
      |sealed trait VAl[+A]
      |final case class VAlK[A](f: VFlip[Boolean, A]) extends VAl[A]
      |sealed trait VTop[+A]
      |sealed trait VMid[A] extends VTop[A]
      |final case class VMidK[A](f: A => Boolean) extends VMid[A]
      |final case class VRef[A](a: A)
      |sealed trait VIn[+A]
      |final case class VInK[A](r: VRef[A]) extends VIn[A]
      |sealed trait VConA[-A]
      |final case class VConAK[A](a: A) extends VConA[A]
      |final class VHK[+F[_]](val f: F[Int])
      |sealed trait VLam[+A]
      |final case class VLamK[A](h: VHK[[X] =>> A => X]) extends VLam[A]
      |sealed trait VW[A <: AnyVal]
      |final case class VWK[A <: AnyVal](a: A) extends VW[A]
      |sealed trait VQ[A <: B, B]
      |final case class VQK[A <: B, B](a: A, b: B) extends VQ[A, B]
      |sealed trait VBare
      |final class VBareK extends VBare
      |case object VBareO extends VBare
      |final case class VUCo(x: VCo[Boolean])
      |final case class VUTwice(x: VTwice[Boolean])
      |final case class VUCon(x: VCon[Boolean])
      |final case class VUFn(x: VFn[Boolean])
      |final case class VUAl(x: VAl[Boolean])
      |final case class VUTop(x: VTop[Boolean])
      |final case class VUIn(x: VIn[Boolean])
      |final case class VUConA(x: VConA[Boolean])
      |final case class VUW(x: VW[Boolean])
      |final case class VUQ(x: VQ[Boolean, Boolean])
      |final case class VUBare(x: VBare)
      |final case class VULam(x: VLam[Boolean])
      |trait FT[F[_, _], C[_[_]]] {
      |  def traverse[X, A, B, G[_]: C](fa: F[X, A], f: A => G[B]): G[F[X, B]]
      |}
      |""".stripMargin

  private lazy val input = TastyFrontEndSpec.compiled("Forms.scala" -> text)
  private lazy val rows = TastyFrontEnd.rows(input)

  private def count(name: String): Count =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.methods))
      .find(_.name == s"tf.$name")
      .get
      .count

  private def unresolved(names: String*) =
    names.map(count).must(contain(beLike[Count] { case Count.Unresolved(_) => ok }).forall)

  private def size(name: String): Option[String] =
    rows.sources
      .flatMap(_.toOption.toList.flatMap(_.definitions))
      .find(_.name == s"tf.$name")
      .get
      .size
      .map(_.render)

  // pair: each field from its one input, 1; bool: every function of two Booleans, 2^4 = 16; one:
  // the single field, 1; nest: p.x.a or p.x.b, 2; map: the context from fa's, each index's value
  // f of fa's at that index, 1 (eo's `GlassF`). `Flags` stores 2 × 2 = 4.
  private def namedTuples =
    (List("N.pair", "N.bool", "N.one", "N.nest", "N.map").map(count) must_== List(
      Count.Finite(1),
      Count.Finite(16),
      Count.Finite(1),
      Count.Finite(2),
      Count.Finite(1)
    )).and(size("Flags") must_== Some("4"))

  // Scala reduces no case of `Fst` on a named tuple (it is no tuple to a match type), though its
  // fields are ground: the row is no number, never the projection's Boolean.
  private def namedStuck = unresolved("N.stuck")

  // Each input package is opened on its own witness W (the old `ExistentialInputSpec`): `one` has
  // consume(value), 1; `two` one per package, 2; `mixed` holds no B, 0; `twice` consume of left
  // or right, 2; `alias` two packages through an alias, 2; `arrow` opens the function's argument,
  // 1; `same` reads q as p, 1; `box` and `consume` have no A or R, 0; `steps` applies step any
  // number of times to the seed, ω; `loop` has no W to start from, 0. `Env.run` opens its
  // constructor's package, 1.
  private def wildcards =
    (List("W.one", "W.two", "W.mixed", "W.twice", "W.alias", "W.arrow", "W.same", "W.box")
      .map(count) must_== List(1, 2, 0, 2, 2, 1, 1, 0).map(Count.Finite(_)))
      .and(
        List("W.consume", "W.loop", "Env.run").map(count) must_== List(0, 0, 1).map(Count.Finite(_))
      )
      .and(count("W.steps") must_== Count.Countable)

  // A bounded wildcard, and one for a bounded parameter (whose witness has `on` to read), are not
  // read; a package built as a result is refused by the solver.
  private def wildcardsRefused = unresolved("W.bounded", "W.boundedParameter", "W.result")

  // A lower bound that is an instance of the class bound leaves it a no-op (the old
  // `BinderBoundsSpec`): x, On or Off, 3 (`ok`, `obj`, `matchable`); x, true or false, 3
  // (`boolVal`); `Sub` has no case, x alone, 1; `Explicit` x or E1, 2. A trait extending a
  // universal trait is no universal trait (only one written `extends Any` is): `U1` is an `AnyRef`
  // (3.9.0 accepts `summon[U1 <:< AnyRef]` and `universal[U1](U1a)`), x or U1a, 2; the old front
  // end refuses it.
  private def classBounds =
    List(
      "K.ok",
      "K.obj",
      "K.boolVal",
      "K.matchable",
      "K.withParents",
      "K.explicitRef",
      "K.universal"
    )
      .map(count) must_== List(3, 3, 3, 3, 1, 2, 2).map(Count.Finite(_))

  // `Flag` is no `AnyVal`, `Boolean` no `AnyRef`, and `Flag` lies below `A <: AnyVal` through
  // `B`: Scala rejects every call (E057), and no row is a number.
  private def classBoundsUnread =
    unresolved("K.anyval", "K.anyref", "K.cross")

  // A constructor builds its fields from its arguments; a bound converts a value up, never down
  // (the old `StoredValueSoundnessSpec` rows): `C1`, `Holder2`, `D1` and `O2.C` have their one
  // argument, 1 each; `D2` has a for its A, and a or b for its B, 1 × 2 = 2.
  private def boundedConstructors =
    List("C1.<init>", "Holder2.<init>", "D1.<init>", "D2.<init>", "O2.C.<init>")
      .map(count) must_== List(1, 1, 1, 2, 1).map(Count.Finite(_))

  // `G` is `[y] =>> G[y]`, `Option` is `[y] =>> Option[y]` and `Either` is `[a, b] =>> Either[a,
  // b]` (the old `HigherKindedBindingSpec`): `eta`, `named` and `same` have x, 1 each. `traverse`
  // has no G[F[X, B]]: its evidence `C[G]` is a rigid application nothing eliminates, 0. A lambda
  // of another body or order (`other`, `swapped`) is no number yet.
  private def nestedKinds =
    (List("T.eta", "T.named", "T.same", "FT.traverse").map(count) must_== List(1, 1, 1, 0).map(
      Count.Finite(_)
    )).and(unresolved("T.other", "T.swapped"))

  // `Option` is `Types.Option = (A, A)` through each export (the old
  // `EnvironmentCompletenessSpec`): x._1 or x._2, 2 each. A projection of a member a refinement
  // fixes is that type (the old `MethodRefinementSpec`): `equal` has x or y, 2; `exact` the one
  // Unit, 1; `bottom` no Nothing, 0; `Pair` stores two As
  // from two, 2 × 2 = 4. `A | A` is A: `same` has x, 1. `([x] =>> x)[A]` is A: `Lam` 2 × 2 = 4;
  // `Holder` holds an `A => A`, built from its own f any number of times, ω.
  private def aliases =
    (List("UseRe.pick", "InRe.pick", "UseRe2.pick", "P.equal", "P.exact", "P.bottom")
      .map(count) must_== List(2, 2, 2, 2, 1, 0).map(Count.Finite(_)))
      .and(
        List("P.same", "Pair.<init>", "Lam.<init>").map(count) must_== List(1, 4, 4).map(
          Count.Finite(_)
        )
      )
      .and(count("Holder.<init>") must_== Count.Countable)

  // A case's invariant parameter in a variant slot (`Sums.carried`): `P[T]` holds `C[S]` for each
  // `S` the slot admits, which adds nothing to `C[T]` where every field takes the parameter at the
  // slot's polarity. Each `VU*` constructor takes one family, its count the family's values.
  // Read: `VCo` = `VCoN` + `VCoK(a, b)` = 1 + 2 × 2 = 5; `VTwice` (A twice left of an arrow,
  // positive) 2^(2^2) = 16; `VCon` (A left of an arrow beside a contravariant slot) 2^2 = 4; `VW`
  // and `VQ`, bounded as their slots are, 2 and 2 × 2 = 4; `VBare` 2 (`VBareK` has no field).
  // Refused: `VFn` (A left of an arrow beside a covariant slot: functions on an `S <: Boolean` a
  // holder cannot apply), `VAl` (the same through the alias `VFlip[Boolean, A] = A => Boolean`,
  // whose arguments are not `Function1`'s in order), `VTop` (an invariant sealed level below,
  // whose case takes A left of an arrow), `VIn` (A in an invariant slot of `VRef`), `VConA` (a
  // field `A` beside a contravariant slot holds values of supertypes of Boolean), `VLam` (A left
  // of an arrow inside a type lambda argument).
  private def variance = {
    val read = List("Co" -> 5, "Twice" -> 16, "Con" -> 4, "W" -> 2, "Q" -> 4, "Bare" -> 2)
    (read.map((n, _) => count(s"VU$n.<init>")) must_== read.map((_, c) => Count.Finite(c)))
      .and(unresolved(List("Fn", "Al", "Top", "In", "ConA", "Lam").map(n => s"VU$n.<init>")*))
  }

}
