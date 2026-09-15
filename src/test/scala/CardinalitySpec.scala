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

  Exponentials
    Boolean => Boolean                       ${tpe("Boolean => Boolean") === TinySize(4)}
    Boolean => Unit                          ${tpe("Boolean => Unit") === UnitSize}
    Unit => Boolean                          ${tpe("Unit => Boolean") === BooleanSize}
    Nothing => Boolean                       ${tpe("Nothing => Boolean") === UnitSize}
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
    Vector[Unit]                             ${tpe("Vector[Unit]") === EffectiveOmega}
    Array[Byte]                              ${tpe("Array[Byte]") === EffectiveOmega}
    Set[String] (finite subsets)             ${tpe("Set[String]") === EffectiveOmega}

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
    ) === EffectiveOmega}
    field of a type defined in the source    ${src(
      "enum Color { case Red, Green, Blue }; case class Pixel(c: Color, on: Boolean)"
    ) === TinySize(9)} (Color 3 + Pixel 6)

  Aliases
    type alias                               ${src(
      "type Flag = Boolean; case class F(f: Flag)"
    ) === BooleanSize}
    opaque type hides cardinality           ${src(
      "opaque type Id = Byte; case class User(id: Id)"
    ) === UnitSize}
  """

}
