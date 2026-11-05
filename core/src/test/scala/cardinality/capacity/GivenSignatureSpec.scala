package cardinality.capacity

import scala.meta.*

import org.specs2.Specification

// The signature inventory counts each supplied instance once. Its implementation is not another
// public module, and its inputs follow the same function-space arithmetic as ordinary methods.
class GivenSignatureSpec extends Specification {

  private def sig(code: String): Size =
    Counter.sourceSignature(dialects.Scala3(code).parse[Source].get)

  private def definition(code: String): Size =
    Counter.defnSignature(
      dialects.Scala3(code).parse[Source].get.stats.collectFirst { case d: Defn => d }.get
    )

  def is = s2"""
    a named alias supplies its declared type ${sig(
      "given flag: Boolean = true"
    ) === BooleanSize}
    an anonymous alias supplies its declared type ${sig(
      "given Boolean = true"
    ) === BooleanSize}
    the definition entry point counts a given alias ${definition(
      "given flag: Boolean = true"
    ) === BooleanSize}
    a given with a unit result contributes one ${sig(
      "given Unit = ()"
    ) === UnitSize}
    context parameters form a factory domain ${sig(
      "given flag(using enabled: Boolean): Boolean = enabled"
    ) === TinySize(4)}
    curried context clauses multiply ${sig(
      "given flag(using a: Boolean)(using b: Boolean): Boolean = a"
    ) === TinySize(16)}
    an empty factory domain contributes nothing ${sig(
      "given impossible(using n: Nothing): Boolean = true"
    ) === NothingSize}
    a generic given retains the ordinary method approximation ${sig(
      "given same[A](using a: A): A = a"
    ) === EffectiveEpsilon0}
    abstract named givens supply their declared type ${sig(
      "trait T { given flag: Boolean }"
    ) === BooleanSize}
    abstract anonymous givens supply their declared type ${sig(
      "trait T { given Boolean }"
    ) === BooleanSize}
    abstract factories include context parameters ${sig(
      "trait T { given flag(using enabled: Boolean): Boolean }"
    ) === TinySize(4)}
    a template supplies its parent type without counting its methods ${sig(
      "class Flag(val value: Boolean); given flag: Flag(true) with { def helper(s: String): String = s }"
    ) === TinySize(4)}
    an anonymous template supplies its parent type ${sig(
      "class Flag(val value: Boolean); given Flag(true) with {}"
    ) === TinySize(4)}
    template factories include context parameters ${sig(
      "class Flag(val value: Boolean); given flag(using enabled: Boolean): Flag(enabled) with {}"
    ) === TinySize(6)}
    multiple template parents use the intersection rule ${sig(
      "class Flag(val value: Boolean); trait Marker; given flag: Flag(true) with Marker with {}"
    ) === TinySize(4)}
    aliases do not count implementation-local methods ${sig(
      "given flag: Boolean = { def helper(s: String): String = s; true }"
    ) === BooleanSize}
    an unresolved instance type retains the existing fallback ${sig(
      "given ordering: Ordering[Boolean] = ???"
    ) === EffectiveOmega}
    package and object inventories sum each given once ${sig(
      "package p { object O { given Boolean = true; given Unit = (); def ordinary(b: Boolean): Boolean = b; extension (b: Boolean) def flip: Boolean = !b } }"
    ) === TinySize(11)}
  """

}
