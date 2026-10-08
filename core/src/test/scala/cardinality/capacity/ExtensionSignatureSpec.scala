package cardinality.capacity

import scala.meta.*

import org.specs2.Specification

class ExtensionSignatureSpec extends Specification {

  private def sig(code: String): Size =
    Counter.sourceSignature(dialects.Scala3(code).parse[Source].get)

  private def definition(code: String): Size =
    Counter.defnSignature(
      dialects.Scala3(code).parse[Source].get.stats.collectFirst { case d: Defn => d }.get
    )

  private val single = "extension (b: Boolean) def flip: Boolean = !b"

  def is = s2"""
    a single extension includes its receiver ${sig(single) === TinySize(4)}
    the definition entry point counts extensions ${definition(single) === TinySize(4)}
    method arguments multiply the receiver domain ${sig(
      "extension (b: Boolean) def choose(x: Boolean)(y: Boolean): Boolean = x"
    ) === FiniteSize(8)}
    group context parameters belong to every method ${sig(
      "extension (b: Boolean)(using enabled: Boolean) { def first: Boolean = b; def second: Boolean = enabled }"
    ) === TinySize(32)}
    method context parameters also belong to the domain ${sig(
      "extension (b: Boolean)(using enabled: Boolean) def choose(x: Boolean)(using flag: Boolean): Boolean = x"
    ) === FiniteSize(16)}
    context clauses before the receiver are included ${sig(
      "extension (using enabled: Boolean)(b: Boolean) def flip: Boolean = !b"
    ) === TinySize(16)}
    a block sums methods without counting the receiver separately ${sig(
      "extension (b: Boolean) { def flip: Boolean = !b; def same: Boolean = b }"
    ) === TinySize(8)}
    abstract extension declarations count too ${sig(
      "trait T { extension (b: Boolean) { def flip: Boolean; def choose(x: Boolean): Boolean } }"
    ) === TinySize(20)}
    indentation syntax counts like an ordinary receiver method ${sig(
      """object O:
        |  extension (b: Boolean)(using enabled: Boolean)
        |    inline def choose(x: Boolean): Boolean = x
        |    inline def same: Boolean = b
        |""".stripMargin
    ) === FiniteSize(9)}
    a receiver with an empty type makes the method empty ${sig(
      "extension (n: Nothing) def impossible: Boolean = true"
    ) === NothingSize}
    an inferred result keeps the ordinary method fallback ${sig(
      "extension (s: String) def same = s"
    ) === EffectiveEpsilon0}
    generic extensions keep the ordinary method approximation ${sig(
      "extension [A](a: A) def same: A = a"
    ) === EffectiveEpsilon0}
    the receiver resolves types in its enclosing scope ${sig(
      "object O { type Flag = Boolean; extension (b: Flag) def flip: Boolean = true }"
    ) === TinySize(4)}
    methods inside implementation bodies are not counted again ${sig(
      "extension (b: Boolean) def flip: Boolean = { def helper(x: Boolean): Boolean = x; helper(b) }"
    ) === TinySize(4)}
    packages and objects aggregate extensions with ordinary members ${sig(
      "package p { object O { val flag: Boolean = true; def ordinary(b: Boolean): Boolean = b; extension (b: Boolean) { def flip: Boolean = !b; def same: Boolean = b } } }"
    ) === TinySize(14)}
  """

}
