import scala.meta.*

import org.specs2.Specification

// The member signature reading: what a definition declares, summed rather than multiplied. Every
// typed field, term and method contributes the size of its declared type, so a module with two
// methods from String to String and one String field is 2ε₀ + ω, while the value-space reading
// (Counter.source) still multiplies the fields of a constructor. The two readings answer
// different questions about the same source, which is why both survive.
class SignatureSpec extends Specification {

  private def sig(code: String): Size =
    Counter.sourceSignature(dialects.Scala3(code).parse[Source].get)

  private def value(code: String): Size = Counter.source(dialects.Scala3(code).parse[Source].get)

  private def defnOf(code: String): Size =
    Counter.defnSignature(
      dialects.Scala3(code).parse[Source].get.stats.collectFirst { case d: Defn => d }.get
    )

  private val service =
    "object Service { def parse(s: String): String = s; def render(s: String): String = s; val name: String = x }"

  private val module =
    "object M { def f(a: Boolean): Boolean = true }"

  private val intMethod =
    "object M { def f(a: Int): Boolean = true }"

  private val serviceOne =
    "object Service { def parse(s: String): String = s; val name: String = x }"

  def is = s2"""
    two endofunctions and a String field       ${sig(
      service
    ) === EffectiveEpsilon0 + EffectiveEpsilon0 + EffectiveOmega} (2ε₀ + ω)
    the same module as a value is one          ${value(
      service
    ) === UnitSize} (an object is a single value)
    a module with one endofunction is smaller  ${sig(
      serviceOne
    ) === EffectiveEpsilon0 + EffectiveOmega}
    constructor fields add, not multiply       ${sig(
      "case class Flags(a: Boolean, b: Boolean, c: Boolean)"
    ) === TinySize(6)}
    while the value space still multiplies     ${value(
      "case class Flags(a: Boolean, b: Boolean, c: Boolean)"
    ) === TinySize(8)}
    a case class parameter is a member         ${sig(
      "case class Pair(a: Boolean, b: Int)"
    ) === FiniteSize(33)}
    a plain parameter is not a member          ${sig("class Plain(a: Boolean)") === NothingSize}
    a val parameter is a member                ${sig("class Kept(val a: Boolean)") === BooleanSize}
    a var parameter is a member                ${sig(
      "class Flagged(var a: Boolean)"
    ) === BooleanSize}
    a capacity field keeps its width           ${sig(
      "object Sizes { val small: Byte = 1; val big: Long = 2 }"
    ) === FiniteSize(65)}
    a Unit field is one value                  ${sig(
      "object O { val u: Unit = (); val t: true = true }"
    ) === TinySize(2)}
    an empty object declares nothing           ${sig("object Empty") === NothingSize}
    abstract declarations count too            ${sig(
      "trait T { def size: Int; val name: String; var ok: Boolean }"
    ) === EffectiveOmega + FiniteSize(32) + BooleanSize}
    an abstract class counts its own members   ${sig(
      "abstract class A { def f(b: Boolean): Boolean }"
    ) === TinySize(4)}
    a method space is codomain over domain     ${sig(module) === TinySize(4)} (2^2)
    a wide domain stays a finite capacity      ${sig(intMethod) === FiniteSize(
      BigInt(1) << 32
    )} (2^(2^32))
    a curried method multiplies its clauses    ${sig(
      "object C { def g(a: Boolean)(b: Boolean): Boolean = true }"
    ) === TinySize(16)}
    a nullary method is its result             ${sig(
      "object N { def size: Byte = 1 }"
    ) === ByteSize}
    an undeclared result type is unknown       ${sig(
      "object U { def f(s: String) = s }"
    ) === EffectiveEpsilon0}
    an undeclared value type is unknown        ${sig("object V { val n = 1 }") === EffectiveOmega}
    multiple patterns declare one each         ${sig(
      "object P { val a, b: Byte = 1 }"
    ) === FiniteSize(9)}
    enum case parameters are members           ${sig(
      "enum Opt { case Some(b: Boolean); case None }"
    ) === BooleanSize}
    an enum own parameter reaches its cases    ${sig(
      "enum Planet(m: Double) { case Earth extends Planet(1.0) }"
    ) === DoubleSize}
    nested definitions contribute once         ${sig(
      "object Outer { object Inner { val flag: Boolean = true }; val name: String = x }"
    ) === EffectiveOmega + BooleanSize}
    a recursive member keeps its settled size  ${sig(
      "sealed trait Nat; case object Zero extends Nat; case class Succ(n: Nat) extends Nat"
    ) === EffectiveOmega}
    packages are summed the same way           ${sig(
      "package a { object One { val b: Boolean = true } }; package b { object Two { val s: String = x } }"
    ) === EffectiveOmega + BooleanSize}
    a single definition reports its own        ${defnOf(module) === TinySize(4)}
    completed lazy fields remain separate contributions ${sig(
      "object S { val first: LazyList[String] = LazyList.empty; val second: LazyList[String] = LazyList.empty }"
    ) === Size.tiers(2, 0)}
    a sum inside a lazy-valued member retains its coefficient ${sig(
      "object S { val both: Either[LazyList[String], LazyList[String]] = Left(LazyList.empty) }"
    ) === Size.tiers(2, 0)}
  """

}
