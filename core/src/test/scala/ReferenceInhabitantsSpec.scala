package cardinality

import org.specs2.Specification

/** Executable examples from Alex Knvl, "Counting type inhabitants", 24 November 2018.
  *
  * Source and assumptions: docs/counting-type-inhabitants.md. Atoms stand for separately
  * universally quantified variables, not for concrete types of unknown finite cardinality. Equality
  * is observational: renaming, beta/eta expansion and redundant case analysis must not manufacture
  * inhabitants.
  */
class ReferenceInhabitantsSpec extends Specification {
  import Inhabitation.{Count, Shape}
  import Shape.*

  private val a = Atom("a")
  private val b = Atom("b")
  private val c = Atom("c")
  private val zero = Sum(Nil)
  private val one = Product(Nil)
  private val bool = Sum(List(one, one))
  private def option(t: Shape): Shape = Sum(List(one, t))
  private def pair(x: Shape, y: Shape): Shape = Product(List(x, y))
  private def fn(args: Shape*)(result: Shape): Shape = Function(args.toList, result)
  private def count(shape: Shape): Count = Inhabitation.count(Nil, shape)

  def is = s2"""
    Finite products, sums and functions (Combining it all together)
      Either Bool (Maybe Bool) has five inhabitants     ${count(
      Sum(List(bool, option(bool)))
    ) === Count.Finite(5)}
      Bool -> Bool has four inhabitants                 ${count(fn(bool)(bool)) === Count.Finite(4)}
      Maybe Bool -> Bool has eight inhabitants          ${count(fn(option(bool))(bool)) === Count
      .Finite(8)}
      Bool -> Maybe Bool has nine inhabitants           ${count(fn(bool)(option(bool))) === Count
      .Finite(9)}
      A triple of Bool -> Unit has one inhabitant       ${count(
      fn(Product(List.fill(3)(bool)))(one)
    ) === Count.Finite(1)}
      Unit -> Bool has two inhabitants                  ${count(fn(one)(bool)) === Count.Finite(2)}
      Nothing -> Bool has one inhabitant                ${count(fn(zero)(bool)) === Count.Finite(1)}
      Bool -> Nothing has no inhabitants                ${count(fn(bool)(zero)) === Count.Finite(0)}
    Rank-1 simple-kinded types (Simple examples)
      forall a. a -> a is identity                       ${count(fn(a)(a)) === Count.Finite(1)}
      forall a b. (a,b) -> a is projection               ${count(fn(pair(a, b))(a)) === Count
      .Finite(1)}
      forall a. (a,a) -> a has two selectors              ${count(fn(pair(a, a))(a)) === Count
      .Finite(2)}
      forall a. (a,a) -> (a,a) has four constructions     ${count(
      fn(pair(a, a))(pair(a, a))
    ) === Count.Finite(4)}
      An irrelevant argument does not add a choice       ${count(fn(fn(a)(c), b, a)(c)) === Count
      .Finite(1)}
      Polymorphic composition is unique                 ${count(
      fn(fn(a)(b), fn(b)(c), a)(c)
    ) === Count.Finite(1)}
      Applying a function or taking the supplied b       ${count(fn(fn(a)(b), b, a)(b)) === Count
      .Finite(2)}
      An independent result type cannot be invented      ${count(fn(fn(a)(b), b, a)(c)) === Count
      .Finite(0)}
      An empty-domain function supplies no a             ${count(fn(fn(zero)(a), a)(zero)) === Count
      .Finite(0)}
      Mapping Option permits erasure or mapping          ${count(
      fn(fn(a)(b), option(a))(option(b))
    ) === Count.Finite(2)}
      Option natural transformations have two choices    ${count(fn(option(a))(option(a))) === Count
      .Finite(2)}
    Recursive types
      Seeded iteration is the natural numbers            ${count(
      fn(fn(a)(a), a)(a)
    ) === Count.Countable}
      Returning a repeatedly composed endomorphism       ${count(
      fn(fn(a)(a))(fn(a)(a))
    ) === Count.Countable}
      A seeded binary producer is countable              ${count(
      fn(fn(a, b)(a), a, b)(a)
    ) === Count.Countable}
      An endomorphism without a seed supplies no a        ${count(fn(fn(a)(a))(a)) === Count.Finite(
      0
    )}
    Observational equivalence and negation
      No a means an a -> Nothing need not be called       ${count(
      fn(pair(fn(a)(zero), b))(pair(bool, b))
    ) === Count.Finite(2)}
      Double-negation elimination without an a is empty  ${count(fn(fn(a)(zero))(zero)) === Count
      .Finite(0)}
      Applying a negation to an a has one inhabitant      ${count(
      fn(fn(a)(zero), a)(zero)
    ) === Count.Finite(1)}
      Two empty-result functions remain indistinguishable ${count(
      fn(pair(fn(a)(zero), fn(a)(zero)))(fn(a)(zero))
    ) === Count.Finite(1)}
    """

}
