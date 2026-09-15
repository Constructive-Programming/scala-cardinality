package cardinality

import scala.meta.*

import org.specs2.Specification

// Expected values are the true cardinalities, not what Counter returns today.
// A source's cardinality is the sum over the concrete data types it defines:
// abstract traits/classes and type aliases add nothing on their own.
class CardinalitySpec extends Specification {

  private def tpe(code: String): Size = Counter.`type`(dialects.Scala3(code).parse[Type].get)
  private def src(code: String): Size = Counter.source(dialects.Scala3(code).parse[Source].get)

  def is = s2"""
  Primitives
    Nothing                                  ${tpe("Nothing") === NothingSize}
    Unit                                     ${tpe("Unit") === UnitSize}
    Boolean                                  ${tpe("Boolean") === BooleanSize}
    Byte                                     ${tpe("Byte") === ByteSize}
    Short                                    ${tpe("Short") === ShortSize}
    Char                                     ${tpe("Char") === CharSize}
    Int                                      ${tpe("Int") === IntSize}
    Long                                     ${tpe("Long") === LongSize}
    Float                                    ${tpe("Float") === FloatSize}
    Double                                   ${tpe("Double") === DoubleSize}
    String                                   ${tpe("String") === EffectiveOmega}
    BigInt                                   ${tpe("BigInt") === EffectiveOmega}
    scala.Boolean (qualified)                ${tpe("scala.Boolean") === BooleanSize}

  Literal and singleton types
    true                                     ${tpe("true") === UnitSize}
    42                                       ${tpe("42") === UnitSize}
    'a'                                      ${tpe("'a'") === UnitSize}
    None.type                                ${tpe("None.type") === UnitSize}

  Products
    (Boolean, Boolean)                       ${tpe("(Boolean, Boolean)") === TinySize(4)}
    (Byte, Boolean)                          ${tpe("(Byte, Boolean)") === FiniteSize(9)}
    (Boolean, Nothing)                       ${tpe("(Boolean, Nothing)") === NothingSize}
    EmptyTuple                               ${tpe("EmptyTuple") === UnitSize}
    Boolean *: Boolean *: EmptyTuple         ${tpe("Boolean *: Boolean *: EmptyTuple") === TinySize(
      4
    )}
    (a: Boolean, b: Boolean) named tuple     ${tpe("(a: Boolean, b: Boolean)") === TinySize(4)}

  Sums
    Option[Boolean]                          ${tpe("Option[Boolean]") === TinySize(3)}
    Option[Nothing]                          ${tpe("Option[Nothing]") === UnitSize}
    Option[Byte]                             ${tpe("Option[Byte]") === FiniteSize(9)}
    Either[Boolean, Unit]                    ${tpe("Either[Boolean, Unit]") === TinySize(3)}
    Either[Int, Int]                         ${tpe("Either[Int, Int]") === FiniteSize(33)}
    Boolean | Unit                           ${tpe("Boolean | Unit") === TinySize(3)}
    true | false                             ${tpe("true | false") === BooleanSize}
    Boolean | Boolean (unions overlap)       ${tpe("Boolean | Boolean") === BooleanSize}
    Boolean & true                           ${tpe("Boolean & true") === UnitSize}
    true & Boolean (min is the other side)   ${tpe("true & Boolean") === UnitSize}

  Exponentials
    Boolean => Boolean                       ${tpe("Boolean => Boolean") === TinySize(4)}
    Boolean => Unit                          ${tpe("Boolean => Unit") === UnitSize}
    Unit => Boolean                          ${tpe("Unit => Boolean") === BooleanSize}
    Nothing => Boolean                       ${tpe("Nothing => Boolean") === NothingSize}
    Boolean => Nothing                       ${tpe("Boolean => Nothing") === NothingSize}
    (Boolean, Boolean) => Boolean            ${tpe("(Boolean, Boolean) => Boolean") === TinySize(
      16
    )}
    Boolean => Boolean => Boolean            ${tpe("Boolean => Boolean => Boolean") === TinySize(
      16
    )}
    Boolean => Int                           ${tpe("Boolean => Int") === FiniteSize(64)}
    Int => Boolean                           ${tpe("Int => Boolean") === FiniteSize(
      BigInt(1) << 32
    )}
    Boolean ?=> Boolean                      ${tpe("Boolean ?=> Boolean") === TinySize(4)}
    Int => String                            ${tpe("Int => String") === EffectiveOmega}
    String => Boolean                        ${tpe("String => Boolean") === EffectiveOmega}
    Set[Boolean]                             ${tpe("Set[Boolean]") === TinySize(4)}
    Set[Byte]                                ${tpe("Set[Byte]") === FiniteSize(256)}
    Map[Boolean, Boolean]                    ${tpe("Map[Boolean, Boolean]") === TinySize(9)}
    PartialFunction[Boolean, Boolean]        ${tpe(
      "PartialFunction[Boolean, Boolean]"
    ) === TinySize(9)}

  Unbounded collections
    List[Boolean]                            ${tpe("List[Boolean]") === EffectiveOmega}
    List[Nothing] (only Nil)                 ${tpe("List[Nothing]") === UnitSize}
    Vector[Nothing] (only empty)             ${tpe("Vector[Nothing]") === UnitSize}
    Seq[Nothing] (only empty)                ${tpe("Seq[Nothing]") === UnitSize}
    IndexedSeq[Nothing] (only empty)         ${tpe("IndexedSeq[Nothing]") === UnitSize}
    Array[Nothing] (only empty)              ${tpe("Array[Nothing]") === UnitSize}
    Vector[Unit]                             ${tpe("Vector[Unit]") === EffectiveOmega}
    Array[Byte]                              ${tpe("Array[Byte]") === EffectiveOmega}
    Set[String] (finite subsets)             ${tpe("Set[String]") === EffectiveOmega}
    Stream[Boolean] (countable)              ${tpe(
      "Stream[Boolean]"
    ) === EffectiveOmega}
    Stream[String] (infinite streams)        ${tpe(
      "Stream[String]"
    ) === EffectiveEpsilon0}

  Classes and objects
    case class                               ${src(
      "case class Pair(a: Boolean, b: Boolean)"
    ) === TinySize(4)}
    plain class                              ${src("class Plain(a: Boolean)") === BooleanSize}
    empty case class                         ${src("case class Empty()") === UnitSize}
    multiple parameter lists                 ${src(
      "case class Curried(a: Boolean)(b: Boolean)"
    ) === TinySize(4)}
    value class                              ${src(
      "case class Meters(value: Int) extends AnyVal"
    ) === IntSize}
    by-name parameter                        ${src("class Lazy(b: => Boolean)") === BooleanSize}
    repeated parameter                       ${src("class Many(bs: Boolean*)") === EffectiveOmega}
    literal-typed fields                     ${src("case class Lit(a: true, b: 1)") === UnitSize}
    composite field types                    ${src(
      "case class Nested(p: (Boolean, Boolean), o: Option[Unit])"
    ) === TinySize(8)}
    derived body vals add nothing            ${src(
      "class Derived(a: Boolean) { val b: Boolean = !a }"
    ) === BooleanSize}
    case object                              ${src("case object Singleton") === UnitSize}
    object                                   ${src("object Module") === UnitSize}

  Algebraic data types
    sealed trait of case objects             ${src(
      "sealed trait Light; case object Red extends Light; case object Amber extends Light; case object Green extends Light"
    ) === TinySize(3)}
    sealed trait of mixed cases              ${src(
      "sealed trait Shape; case class Dot(on: Boolean) extends Shape; case object Blank extends Shape"
    ) === TinySize(3)}
    sealed abstract class with fixed args    ${src(
      "sealed abstract class Polygon(sides: Byte); case object Tri extends Polygon(3); case object Quad extends Polygon(4)"
    ) === BooleanSize}
    enum of singletons                       ${src(
      "enum Color { case Red, Green, Blue }"
    ) === TinySize(3)}
    enum with parameterised cases            ${src(
      "enum Opt { case Some(b: Boolean); case None }"
    ) === TinySize(3)}
    enum with constructor arguments          ${src(
      "enum Planet(mass: Double) { case Earth extends Planet(1.0); case Mars extends Planet(0.1) }"
    ) === BooleanSize}
    recursive ADT                            ${src(
      "sealed trait Nat; case object Zero extends Nat; case class Succ(n: Nat) extends Nat"
    ) === EffectiveOmega + UnitSize}
    abstract class recursion stays unknown   ${src(
      "abstract class Abs(n: Abs); case class Uses(a: Abs)"
    ) === EffectiveOmega}
    unsealed trait is not a sum              ${src(
      "trait C2; case object R2 extends C2; case class P2(c: C2)"
    ) === EffectiveOmega + UnitSize}
    unsealed abstract parent is not a sum    ${src(
      "abstract class B6(y: Boolean); case class S6(z: Boolean) extends B6(z); case class R6(b: B6)"
    ) === EffectiveOmega + BooleanSize}
    sealed parent with abstract child        ${src(
      "sealed trait Q; abstract class Qa(x: Boolean) extends Q; case class Qb(y: Boolean) extends Q; case class Uq(q: Q)"
    ) === EffectiveOmega + BooleanSize}
    growth revival after settling is finite  ${src(
      "case class X(o: Option[G], h: H); type G = A1; type A1 = A2; type A2 = A3; type A3 = A4; type A4 = Boolean; type H = Boolean"
    ) === TinySize(6)} (grows twice, never in consecutive rounds)
    revival pinning is observable by a consumer  ${src(
      "case class W2(x: X); case class X(o: Option[G], h: H); type G = A1; type A1 = A2; type A2 = A3; type A3 = A4; type A4 = Boolean; type H = Boolean"
    ) === TinySize(12)} (W2 6 + X 6; the grewEver window must not pin revival growth)
    abstract lazy arm gets no cycle          ${src(
      "abstract class A9(n: => A9); case class Uses9(a: A9)"
    ) === EffectiveOmega}
    field of a type defined in the source    ${src(
      "enum Color { case Red, Green, Blue }; case class Pixel(c: Color, on: Boolean)"
    ) === TinySize(9)} (Color 3 + Pixel 6)

  Recursive types (solved as least fixed points)
    degenerate self-recursion (μX.X, no base)  ${src("case class Loop(next: Loop)") === NothingSize}
    mutual recursion without base              ${src(
      "case class A(b: B); case class B(a: A)"
    ) === NothingSize}
    productive self-recursion (Option tail)    ${src(
      "case class Q(b: Boolean, opt: Option[Q])"
    ) === EffectiveOmega}
    recursive enum                             ${src(
      "enum Chain { case Link(next: Chain); case Stop }"
    ) === EffectiveOmega}
    mutual sealed ADTs                         ${src(
      "sealed trait L; case object L0 extends L; case class L1(r: R) extends L; sealed trait R; case object R0 extends R; case class R1(l: L) extends R"
    ) === EffectiveOmega + EffectiveOmega + BooleanSize}
    recursion in a function domain                   ${src(
      "case class P(b: Boolean, t: P => P)"
    ) === NothingSize} (F[Nothing] is uninhabited, so μX.F is — section 8 of docs/type-arithmetic.md)
    sealed parent without concrete subtypes    ${src(
      "sealed trait Open; trait Aux extends Open; case class Ref(o: Open)"
    ) === EffectiveOmega} (hierarchy open elsewhere)

  Lazy recursion (solved as greatest fixed points)
    lazy wrapper: the one infinite tower       ${src("case class Loop(next: => Loop)") === UnitSize}
    conaturals collapse after completion       ${src(
      "case class CoNat(pred: => Option[CoNat])"
    ) === EffectiveOmega}
    endless Boolean stream, program-countable  ${src(
      "case class St(head: Boolean, tail: => St)"
    ) === EffectiveOmega}
    thunk tail `() => X`                       ${src(
      "case class T2(head: Boolean, next: () => T2)"
    ) === EffectiveOmega}
    cycle through a sealed parent              ${src(
      "sealed trait Lz; case class Node(h: Boolean, next: => Lz) extends Lz"
    ) === EffectiveOmega}
    consumers see the ν count                  ${src(
      "case class Inf(next: => Inf); case class Use(i: Inf)"
    ) === TinySize(2)} (1 finite + 1 infinite each)
    mutual lazy streams                        ${src(
      "case class A(h: Boolean, b: => B); case class B(x: Int, a: => A)"
    ) === EffectiveOmega + EffectiveOmega}
    branching holes saturate at ℵ₀            ${src(
      "case class R(l: => R, r: => R)"
    ) === EffectiveOmega} (computably infinite binary trees)
    holes with different successors       ${src(
      "case class B7(x: => B7, y: => C7); case class C7(z: Boolean)"
    ) === EffectiveOmega + BooleanSize} (the external tail labels each node; the path stays deterministic)
    enum arms to different successors     ${src(
      "enum B8 { case X(t: => B8); case Y(t: => C8) }; case class C8(b: Boolean)"
    ) === EffectiveOmega + BooleanSize} (B8 collapses to ω; the separate C8 definition still adds 2)
    only recognized holes continue the cycle  ${src(
      "sealed trait E; case class One(e: => E) extends E; case class Void(n: Nothing) extends E"
    ) === UnitSize} (the single One-tower; a Void sibling continues nothing)
    function fields continue too          ${src(
      "sealed trait E2; case class One(e: => E2) extends E2; case class Two(i: Int => E2) extends E2"
    ) === EffectiveOmega + EffectiveOmega} (One(Two(_ => e)) unfolds through Two; branchy ⇒ ℵ₀)
    pure function-field cycle             ${src("case class F10(k: Int => F10)") === EffectiveOmega}
    a singleton domain is a thunk in disguise  ${src(
      "case class S10(k: Unit => S10)"
    ) === UnitSize}
    deterministic mutual towers, one each  ${src(
      "case class A11(b: => B11); case class B11(a: => A11)"
    ) === TinySize(2)}
    strict self-argument blocks coiteration  ${src(
      "case class K9(k: K9, m: => K9)"
    ) === NothingSize} (no knot without a lazy tie)
    deep mentions block it too            ${src(
      "case class H9(m: => H9, s: Set[H9])"
    ) === NothingSize}
    lazy cycle through a sealed abstract  ${src(
      "sealed abstract class Nxt(v: Boolean); case class Go(next: => Nxt) extends Nxt(true)"
    ) === UnitSize}
    a tail into another's cycle is no cycle  ${src(
      "case class Wrap(w: => Loop2); case class Loop2(next: => Loop2)"
    ) === TinySize(2)} (Wrap = Loop2 = 1 each)
    strict stream still has no base            ${src(
      "case class S2(head: Boolean, tail: S2)"
    ) === NothingSize}
    LazyList over a countable alphabet         ${tpe(
      "LazyList[String]"
    ) === EffectiveEpsilon0}
    LazyList of finitely-producible values     ${tpe(
      "LazyList[Boolean]"
    ) === EffectiveOmega}
    LazyList[Nothing] is only empty            ${tpe("LazyList[Nothing]") === UnitSize}

  Forward references
    field of a type defined later              ${src(
      "case class Use(d: Def); case class Def(x: Boolean)"
    ) === TinySize(4)} (Use 2 + Def 2)
    alias chain defined bottom-up              ${src(
      "case class Uses(a: A); type A = B; type B = C; type C = D; type D = E; type E = F; type F = G; type G = Boolean"
    ) === BooleanSize}

  Aliases
    type alias                               ${src(
      "type Flag = Boolean; case class F(f: Flag)"
    ) === BooleanSize}
    opaque type hides cardinality           ${src(
      "opaque type Id = Byte; case class User(id: Id)"
    ) === UnitSize}

  Polynomial sums (issue #12)
    either of two countable types keeps both   ${tpe(
      "Either[String, String]"
    ) === EffectiveOmega + EffectiveOmega} (ω + ω, not one absorbed ω)
    a countable alternative keeps its one      ${tpe(
      "Option[String]"
    ) === EffectiveOmega + UnitSize}
    a finite alternative adds its count        ${tpe(
      "Either[String, Boolean]"
    ) === EffectiveOmega + BooleanSize}
    a capacity alternative keeps its width     ${tpe(
      "Either[String, Int]"
    ) === EffectiveOmega + IntSize}
    a function space beside a countable type   ${tpe(
      "Either[String => String, String]"
    ) === EffectiveEpsilon0 + EffectiveOmega}
    two countable fields multiply coarsely     ${tpe(
      "(String, String)"
    ) === EffectiveOmega} (ω * ω = ω: the documented product loss)
    a finite factor does not scale a product   ${tpe("(Boolean, String)") === EffectiveOmega}
    a function between countable types         ${tpe(
      "String => String"
    ) === EffectiveEpsilon0} (ω^ω, capped at the ε₀ tier)
    a predicate over a countable domain        ${tpe("String => Boolean") === EffectiveOmega}
    a result of Unit collapses any domain      ${tpe("String => Unit") === UnitSize}
    an empty domain stays empty                ${tpe("Nothing => String") === NothingSize}
    a field of a two-way sum keeps its count   ${src(
      "type A = Either[String, String]; case class C(a: A)"
    ) === EffectiveOmega + EffectiveOmega} (the alias adds nothing; the field carries 2ω)

  Polynomial recursion (issue #12)
    an empty label space keeps a cycle empty   ${src(
      "case class Dead(n: Nothing, left: => Dead, right: => Dead)"
    ) === NothingSize} (branching cannot conjure values from an empty per-lap space)
    a chain of consumers carries the nu count  ${src(
      "case class Inf(next: => Inf); case class Use(i: Inf); case class Use2(u: Use)"
    ) === TinySize(3)} (one tower each: every consumer sees the settled value)
    a definition agrees with its reference     ${src(
      "case class Q(next: Option[Q]); case class Use(q: Q)"
    ) === Size.tiers(0, 2)} (Q collapses to ω, Use sees ω, and the source still adds both)
    a custom lazy list matches the built in    ${src(
      "enum U { case End; case More(head: Boolean, tail: => U) }"
    ) === EffectiveOmega} (completed lazy type, exactly LazyList[Boolean])
    a recursive epsilon payload keeps its tier ${src(
      "enum High { case Seed(f: String => String); case Next(high: High) }"
    ) === EffectiveEpsilon0} (widening never demotes ε₀ to ω)
    a sealed family sums its definitions       ${src(
      "sealed trait Nat; case object Zero extends Nat; case class Succ(n: Nat) extends Nat"
    ) === EffectiveOmega + UnitSize} (Zero 1 + Succ ω; a reference to the parent is ω)
    an enum is one definition                  ${src(
      "enum Nat2 { case Zero; case Succ(n: Nat2) }"
    ) === EffectiveOmega} (same shape, counted once)

  Completed lazy types collapse before enclosing sums
    LazyList[Unit] collapses finite depths and the tower ${tpe("LazyList[Unit]") === EffectiveOmega}
    Stream[Unit] uses the same completion rule ${tpe("Stream[Unit]") === EffectiveOmega}
    Stream[Nothing] keeps its single empty value ${tpe("Stream[Nothing]") === UnitSize}
    an outer sum keeps two high-tier lazy values ${tpe(
      "Either[LazyList[String], LazyList[String]]"
    ) === Size.tiers(2, 0)}
    an outer sum keeps two countable lazy values ${tpe(
      "Either[LazyList[Unit], Stream[Boolean]]"
    ) === Size.tiers(0, 2)}
    an outer option keeps its finite alternative ${tpe(
      "Option[LazyList[String]]"
    ) === EffectiveEpsilon0 + UnitSize}
    nested lazy collections complete at each layer ${tpe(
      "LazyList[LazyList[Unit]]"
    ) === EffectiveEpsilon0}
    a custom unit list collapses like the built in ${src(
      "enum Units { case End; case More(head: Unit, tail: => Units) }"
    ) === EffectiveOmega}
    a custom string list retains the highest tier ${src(
      "enum Strings { case End; case More(head: String, tail: => Strings) }"
    ) === EffectiveEpsilon0}
    an empty alphabet leaves only a finite base ${src(
      "enum Empty { case End; case More(head: Nothing, tail: => Empty) }"
    ) === UnitSize}
    consumers of a lazy type may add finite terms ${src(
      "case class CoNat(pred: => Option[CoNat]); case class Use(value: Option[CoNat])"
    ) === Size.tiers(0, 2) + UnitSize} (CoNat ω + Use (ω + 1), not a collapsed source total)
  """

}
