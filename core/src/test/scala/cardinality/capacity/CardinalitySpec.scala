package cardinality.capacity

import cardinality.Pending
import org.specs2.Specification

// Expected values are the true cardinalities, not what Counter returns today.
// A source's cardinality is the sum over the concrete data types it defines:
// abstract traits/classes and type aliases add nothing on their own.
class CardinalitySpec extends Specification {

  // A type is read from the alias a compiled source gives it, as every fixture is compiled. A
  // lazy field is a plain class's by-name parameter (`final class St(head: Boolean, tail: => St)`):
  // a case class's parameters are `val`s, which scalac does not take by name. An enum case's are
  // `val`s too: its lazy field is a thunk (`tail: () => U`), the same lazy position
  // (docs/type-arithmetic.md §8).
  private def tpe(code: String): Size = Rows.typeSize(code)

  private def src(code: String): Size = Rows.total(code)

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
    class with a companion object            ${src(
      "case class Companion(a: Boolean)\nobject Companion"
    ) === TinySize(3)} (the class 2 + the module 1; the type keeps the name)
    case object                              ${src("case object Singleton") === UnitSize}
    object                                   ${src("object Module") === UnitSize}

  Algebraic data types
    sealed trait of case objects             ${src(
      "sealed trait Light; case object Red extends Light; case object Amber extends Light; case object Green extends Light"
    ) === TinySize(3)}
    sealed trait with nested cases           ${src(
      "sealed trait Light { case object Red extends Light; case object Green extends Light }"
    ) === BooleanSize}
    object with a nested case class          ${src(
      "object Wrapper { case class Pair(a: Boolean, b: Boolean) }"
    ) === TinySize(5)} (the module 1 + Pair 4)
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
    sealed parent with abstract child        ${Pending.knownLoss(
      src(
        "sealed trait Q; abstract class Qa(x: Boolean) extends Q; case class Qb(y: Boolean) extends Q; case class Uq(q: Q)"
      ) === EffectiveOmega + BooleanSize
    )}
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
    recursion in a function domain has no count      ${Rows.rows(
      "case class P(b: Boolean, t: P => P)"
    )(("P", Definition.Kind.Class)) must startWith(
      "unresolved["
    )} (not 0: P(true, x => x) is a closed value; §8 covers covariant recursion only)
    and its row says so, not the placeholder ω  ${Pending.knownLoss(
      cardinality.Compiled
        .definitions("case class P(b: Boolean, t: P => P)")
        .find(_.name == "P")
        .map(_.unbound.map(_.render)) === Some(List("recursion through a function's domain: P"))
    )}
    sealed parent without concrete subtypes    ${Pending.knownLoss(
      src(
        "sealed trait Open; trait Aux extends Open; case class Ref(o: Open)"
      ) === EffectiveOmega
    )} (hierarchy open elsewhere)

  Lazy recursion (solved as greatest fixed points)
    lazy wrapper: the one infinite tower       ${src(
      "final class Loop(next: => Loop)"
    ) === UnitSize}
    conaturals collapse after completion       ${src(
      "final class CoNat(pred: => Option[CoNat])"
    ) === EffectiveOmega}
    endless Boolean stream, program-countable  ${src(
      "final class St(head: Boolean, tail: => St)"
    ) === EffectiveOmega}
    thunk tail `() => X`                       ${src(
      "case class T2(head: Boolean, next: () => T2)"
    ) === EffectiveOmega}
    cycle through a sealed parent              ${src(
      "sealed trait Lz; final class Node(h: Boolean, next: => Lz) extends Lz"
    ) === EffectiveOmega}
    consumers see the ν count                  ${src(
      "final class Inf(next: => Inf); case class Use(i: Inf)"
    ) === TinySize(2)} (1 finite + 1 infinite each)
    mutual lazy streams                        ${src(
      "final class A(h: Boolean, b: => B); final class B(x: Int, a: => A)"
    ) === EffectiveOmega + EffectiveOmega}
    holes alone are no choice                  ${src(
      "final class R(l: => R, r: => R)"
    ) === UnitSize} (νX.X·X: the one knot, which read ω)
    labelled holes branch                      ${src(
      "final class Rb(b: Boolean, l: => Rb, r: => Rb)"
    ) === EffectiveEpsilon0} (r holds any Rb, ω of them, each lap: ε₀, which read ω)
    holes with different successors       ${src(
      "final class B7(x: => B7, y: => C7); case class C7(z: Boolean)"
    ) === EffectiveOmega + BooleanSize} (the external tail labels each node; the path stays deterministic)
    enum arms to different successors     ${src(
      "enum B8 { case X(t: () => B8); case Y(t: () => C8) }; case class C8(b: Boolean)"
    ) === EffectiveOmega + BooleanSize} (B8 collapses to ω; the separate C8 definition still adds 2)
    only recognized holes continue the cycle  ${src(
      "sealed trait E; final class One(e: => E) extends E; case class Void(n: Nothing) extends E"
    ) === UnitSize} (the single One-tower; a Void sibling continues nothing)
    function fields continue too          ${src(
      "sealed trait E2; final class One(e: => E2) extends E2; case class Two(i: Int => E2) extends E2"
    ) === EffectiveEpsilon0 + EffectiveEpsilon0} (Two continues through one input and holds any E2, ω of them, in the others: ε₀ each, which read ω)
    pure function-field cycle             ${src(
      "case class F10(k: Int => F10)"
    ) === UnitSize} (the one knot, which read ω)
    a singleton domain is a thunk in disguise  ${src(
      "case class S10(k: Unit => S10)"
    ) === UnitSize}
    deterministic mutual towers, one each  ${src(
      "final class A11(b: => B11); final class B11(a: => A11)"
    ) === TinySize(2)}
    strict self-argument blocks coiteration  ${src(
      "final class K9(k: K9, m: => K9)"
    ) === NothingSize} (no knot without a lazy tie)
    a strict set beside a lazy field      ${src(
      "final class H9(m: => H9, s: Set[H9])"
    ) === EffectiveEpsilon0} (`new H9(h, Set())` ties it; s holds any set of H9s beside the tail: ε₀, which read ω)
    lazy cycle through a sealed abstract  ${src(
      "sealed abstract class Nxt(v: Boolean); final class Go(next: => Nxt) extends Nxt(true)"
    ) === UnitSize}
    a tail into another's cycle is no cycle  ${src(
      "final class Wrap(w: => Loop2); final class Loop2(next: => Loop2)"
    ) === TinySize(2)} (Wrap = Loop2 = 1 each)
    a thunk inside a tuple defers the cycle    ${src(
      "case class Ltd(p: (Boolean, () => Ltd))"
    ) === EffectiveOmega} (νX. 2·X: an endless Boolean stream)
    a unit label beside it is the one knot     ${src(
      "case class Lu(p: (Unit, () => Lu))"
    ) === UnitSize} (νX. X)
    two thunks in one field are one knot       ${src(
      "case class Lw(p: (() => Lw, () => Lw))"
    ) === UnitSize} (νX. X·X: the one knot, which read ω)
    a strict field inside the cycle            ${src(
      "case class Out(p: In); case class In(b: Boolean, f: () => Out)"
    ) === EffectiveOmega + EffectiveOmega} (each lap 1 × 2: ω each)
    a sealed parent passes through its children  ${src(
      "sealed trait T; case class A(t: () => T) extends T; case class B(b: Boolean, a: A) extends T"
    ) === EffectiveOmega + EffectiveOmega} (A's thunk returns A or B(b, A): ω each)
    a lazy child outside the parent's cycle    ${src(
      "sealed trait Pt; final class Pa(t: => Pt) extends Pt; final class Pb(h: Boolean, t: => Pb) extends Pt"
    ) === EffectiveOmega + EffectiveOmega} (Pb = 2^ω; Pa = Pt = Pa + Pb: ω, where the one Pa tower read 1)
    a lazy child under a nested sum           ${src(
      "sealed trait Ut; sealed trait Ui extends Ut; final class Ua(t: => Ut) extends Ut; final class Ub(h: Boolean, t: => Ub) extends Ui"
    ) === EffectiveOmega + EffectiveOmega} (Ui = Ub = ω; Ua = Ut = Ua + Ui: ω, where it read 1)
    a field over a lazy cycle outside it      ${src(
      "final class Ra(t: => Ra, c: Rc); case class Rc(b: Rb); final class Rb(h: Boolean, t: => Rb)"
    ) === EffectiveEpsilon0 + EffectiveOmega + EffectiveOmega} (Rc = Rb = ω; Ra streams ω labels: ε₀, where it read 0)
    a held parent holds its children outside   ${src(
      "sealed trait Bo; case class Bo1(s: String) extends Bo; final class Bo2(t: => Bo2, b: Bo) extends Bo"
    ) === EffectiveOmega + EffectiveEpsilon0} (Bo1 = ω; Bo2 streams Bo1 or Bo2 per lap: ε₀, where it read ω)
    a second hole holds a value outside        ${src(
      "enum En { case A(s: String); case B(t: () => En, e: En) }"
    ) === EffectiveEpsilon0} (e may be any A(s) each lap: ε₀, which read ω)
    a nested parent's outside child            ${src(
      "sealed trait Np; sealed trait Nq extends Np; case class Nqo(s: String) extends Nq; final class Nc(t: => Nc, p: Np) extends Nq"
    ) === EffectiveOmega + EffectiveEpsilon0} (Nqo ω; Nc streams Nqo or Nc per lap: ε₀, which read ω)
    a one-hole arm ends at a value outside     ${src(
      "sealed trait Cq; case class Cq1(s: String) extends Cq; case class Sh(c: Cq) extends Cq; final class Lh(t: => Cq) extends Cq"
    ) === EffectiveOmega + EffectiveOmega + EffectiveOmega} (Lh/Sh words ending in a Cq1, or endless: ω each)
    a capacity label beside one hole           ${src(
      "case class Ck(p: (Int, () => Ck))"
    ) === EffectiveOmega} (νX. 2^32·X: one hole, 2^32 labels per lap)
    a list of thunks has infinite arities      ${src(
      "case class Lst(p: List[() => Lst])"
    ) === EffectiveEpsilon0} (each node chooses its arity in ω: ε₀)
    a strict list beside a thunk               ${src(
      "case class Kx(kids: List[Kx], f: () => Kx)"
    ) === EffectiveEpsilon0} (Kx(Nil, () => k) ties it; ω arities per node: ε₀)
    an alias under a thunk's result is expanded  ${src(
      "type T = (Boolean, Xa); final case class Xa(f: () => T)"
    ) === EffectiveOmega} (Xa = νX. 2·X, ω; it read 0; the alias adds nothing)
    an alias under a by-name type is expanded  ${src(
      "type T2 = (Boolean, Ya); final class Ya(f: => T2)"
    ) === EffectiveOmega} (Ya = νX. 2·X, ω)
    a name held strictly and lazily in one field  ${src(
      "case class Ks(p: (Ks, () => Ks))"
    ) === NothingSize} (its μ value; the row is `?`)
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
    opaque type is read only in its scope    ${src(
      "opaque type Id = Byte; case class User(id: Id)"
    ) === TinySize(1)} (a top-level class is outside, where Id is one value)
    opaque type inside its scope            ${src(
      "object Ids { opaque type Id = Boolean; case class User(id: Id) }"
    ) === TinySize(3)} (Ids 1 + User 2)

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
      "final class Dead(n: Nothing, left: => Dead, right: => Dead)"
    ) === NothingSize} (branching cannot conjure values from an empty per-lap space)
    a chain of consumers carries the nu count  ${src(
      "final class Inf(next: => Inf); case class Use(i: Inf); case class Use2(u: Use)"
    ) === TinySize(3)} (one tower each: every consumer sees the settled value)
    a definition agrees with its reference     ${src(
      "case class Q(next: Option[Q]); case class Use(q: Q)"
    ) === Size(
      0,
      2,
      FinitePart.Zero
    )} (Q collapses to ω, Use sees ω, and the source still adds both)
    a custom lazy list matches the built in    ${src(
      "enum U { case End; case More(head: Boolean, tail: () => U) }"
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
    ) === Size(2, 0, FinitePart.Zero)}
    an outer sum keeps two countable lazy values ${tpe(
      "Either[LazyList[Unit], Stream[Boolean]]"
    ) === Size(0, 2, FinitePart.Zero)}
    an outer option keeps its finite alternative ${tpe(
      "Option[LazyList[String]]"
    ) === EffectiveEpsilon0 + UnitSize}
    nested lazy collections complete at each layer ${tpe(
      "LazyList[LazyList[Unit]]"
    ) === EffectiveEpsilon0}
    a custom unit list collapses like the built in ${src(
      "enum Units { case End; case More(head: Unit, tail: () => Units) }"
    ) === EffectiveOmega}
    a custom string list retains the highest tier ${src(
      "enum Strings { case End; case More(head: String, tail: () => Strings) }"
    ) === EffectiveEpsilon0}
    an empty alphabet leaves only a finite base ${src(
      "enum Empty { case End; case More(head: Nothing, tail: () => Empty) }"
    ) === UnitSize}
    consumers of a lazy type may add finite terms ${src(
      "final class CoNat(pred: => Option[CoNat]); case class Use(value: Option[CoNat])"
    ) === Size(
      0,
      2,
      FinitePart.Zero
    ) + UnitSize} (CoNat ω + Use (ω + 1), not a collapsed source total)
  """

}
