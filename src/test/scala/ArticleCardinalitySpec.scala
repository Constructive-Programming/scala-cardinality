import scala.meta.*

import org.specs2.Specification
import org.specs2.execute.Result

// Cases from `docs/type-arithmetic.md`, condensed from Alex Knvl's "Counting type inhabitants".
// Examples assert the count that document derives wherever the calculator already implements the
// rule. The rest are `target`s: an expectation that fails today, which specs2 reports as pending
// and turns into a failure once it starts holding, so support cannot land behind a stale marker.
//
// `TinySize(n)` is the exact count `n`; `FiniteSize(bits)` is a count in `(2^(bits-1), 2^bits]`,
// which is why the larger examples below name a capacity such as 2^16 rather than a number.
class ArticleCardinalitySpec extends Specification {

  private def tpe(code: String): Size = Counter.`type`(dialects.Scala3(code).parse[Type].get)
  private def src(code: String): Size = Counter.source(dialects.Scala3(code).parse[Source].get)

  // What each pending target would need before its rule can be asserted instead.
  private val needsParametricity =
    "a polymorphic count needs parametricity, which the traversal does not model (section 6)"

  private val needsSubtyping =
    "a union or intersection count needs overlap and subtyping information (section 2)"

  // The article's `data Foo = Bar | Baz Bool | Baf Int`: 1 + 2 + 2^32, which the size algebra
  // rounds up to a 33-bit capacity.
  private val adt = "enum Foo { case Bar; case Baz(b: Boolean); case Baf(i: Int) }"

  private def target(count: => Size, expected: Size, reason: String): Result = {
    val actual = count
    pendingUntilFixed(s"$reason: type arithmetic gives $expected, Counter returns $actual")(
      actual === expected
    )
  }

  def is = s2"""
  Sums and products (section 2)
    tagged alternatives do not collapse      ${tpe("Either[Boolean, Boolean]") === TinySize(4)}
    nested sum (2 + (1 + 2))                 ${tpe(
      "Either[Boolean, Option[Boolean]]"
    ) === TinySize(5)}
    a Unit field is a no-op factor           ${tpe("(Boolean, Unit)") === BooleanSize}
    three Boolean fields (2 * 2 * 2)         ${tpe(
      "(Boolean, Boolean, Boolean)"
    ) === TinySize(8)}
    an uninhabited field annihilates         ${tpe("(Nothing, Int)") === NothingSize}
    two 32-bit fields are 2^64               ${tpe("(Int, Int)") === LongSize}
    a * (b + c) distributes                  ${tpe(
      "(Boolean, Either[Boolean, Boolean])"
    ) === TinySize(8)}
    and the expanded sum agrees              ${tpe(
      "Either[(Boolean, Boolean), (Boolean, Boolean)]"
    ) === TinySize(8)}
    ADT sum 1 + 2 + 2^32                     ${src(adt) === FiniteSize(33)} (rounded up)
    a data type with no constructors         ${src("sealed trait Empty") === NothingSize}

  Functions (section 3)
    Boolean => Boolean is 2^2                ${tpe("Boolean => Boolean") === TinySize(4)}
    Option[Boolean] => Boolean is 2^3        ${tpe(
      "Option[Boolean] => Boolean"
    ) === TinySize(8)}
    Boolean => Option[Boolean] is 3^2        ${tpe(
      "Boolean => Option[Boolean]"
    ) === TinySize(9)}
    three Boolean arguments => Unit          ${tpe(
      "(Boolean, Boolean, Boolean) => Unit"
    ) === UnitSize} (1^8)
    two Boolean args, a four-way sum         ${tpe(
      "Boolean => Boolean => Either[Boolean, Boolean] => Boolean"
    ) === FiniteSize(16)} (2^16)
    an empty domain can never be applied     ${tpe("Nothing => Boolean") === NothingSize}
    including into Nothing (0^0 = 0)         ${tpe("Nothing => Nothing") === NothingSize}
    a non-empty domain into Nothing          ${tpe("Int => Nothing") === NothingSize}
    a one-element domain                     ${tpe("Unit => Boolean") === BooleanSize}
    a Unit result collapses any domain       ${tpe("Boolean => Unit") === UnitSize}
    an unknown domain into Unit              ${tpe("String => Unit") === UnitSize}
    an unknown domain into Nothing           ${tpe("String => Nothing") === NothingSize}
    an empty domain into an unknown          ${tpe("Nothing => String") === NothingSize}
    curried matches tupled                   ${tpe(
      "Boolean => Boolean => Boolean"
    ) === TinySize(16)}
    and so does the tupled form              ${tpe(
      "((Boolean, Boolean)) => Boolean"
    ) === TinySize(16)}
    an arrow into a product splits           ${tpe(
      "Boolean => (Boolean, Boolean)"
    ) === TinySize(16)}
    an arrow from a sum splits               ${tpe(
      "Either[Boolean, Boolean] => Boolean"
    ) === TinySize(16)}
    and both equal two separate arrows       ${tpe(
      "(Boolean => Boolean, Boolean => Boolean)"
    ) === TinySize(16)}

  Negation (section 5)
    double negation of Boolean is empty      ${tpe(
      "(Boolean => Nothing) => Nothing"
    ) === NothingSize}
    double negation of Nothing is empty      ${tpe(
      "(Nothing => Nothing) => Nothing"
    ) === NothingSize}

  Sets and lists (section 4)
    Set[Nothing] has only the empty set      ${tpe("Set[Nothing]") === UnitSize}
    Set[Unit] is in or out                   ${tpe("Set[Unit]") === BooleanSize}
    Set[Boolean] is a powerset               ${tpe("Set[Boolean]") === TinySize(4)}
    Set[Option[Boolean]] is 2^3              ${tpe("Set[Option[Boolean]]") === TinySize(8)}
    finite subsets stay countable            ${tpe("Set[String]") === EffectiveOmega}
    a predicate on a countable domain        ${tpe("String => Boolean") === EffectiveOmega} (ℵ₀)
    a function between countable types       ${tpe("String => String") === EffectiveTau} (ℵ₀^ℵ₀)
    List[Nothing] is only Nil                ${tpe("List[Nothing]") === UnitSize}
    List[Unit] is one list per length        ${tpe("List[Unit]") === EffectiveOmega}
    List[Boolean] is countable               ${tpe("List[Boolean]") === EffectiveOmega}

  Targets (section 6): polymorphic counts
    identity has one inhabitant              ${target(
      tpe("[A] => A => A"),
      UnitSize,
      needsParametricity
    )}
    first projection has one inhabitant      ${target(
      tpe("[A, B] => ((A, B)) => A"),
      UnitSize,
      needsParametricity
    )}
    either input may be returned             ${target(
      tpe("[A] => ((A, A)) => A"),
      BooleanSize,
      needsParametricity
    )}
    both outputs chosen independently        ${target(
      tpe("[A] => ((A, A)) => (A, A)"),
      TinySize(4),
      needsParametricity
    )}
    apply the function, ignoring B           ${target(
      tpe("[A, B, C] => (A => C) => B => A => C"),
      UnitSize,
      needsParametricity
    )}
    composition has one inhabitant           ${target(
      tpe("[A, B, C] => (A => B) => (B => C) => A => C"),
      UnitSize,
      needsParametricity
    )}
    apply the function or return B           ${target(
      tpe("[A, B] => (A => B) => B => A => B"),
      BooleanSize,
      needsParametricity
    )}
    C cannot be manufactured                 ${target(
      tpe("[A, B, C] => (A => B) => B => A => C"),
      NothingSize,
      needsParametricity
    )}
    Nothing cannot be manufactured           ${target(
      tpe("[A] => (Nothing => A) => A => Nothing"),
      NothingSize,
      needsParametricity
    )}
    a negated argument empties the domain    ${target(
      tpe("[A, B] => ((A => Nothing, B)) => (Boolean, B)"),
      NothingSize,
      needsParametricity
    )}
    no A is available for every A            ${target(
      tpe("[A] => (A => Nothing) => Nothing"),
      NothingSize,
      needsParametricity
    )}
    a contradiction can never be supplied    ${target(
      tpe("[A] => (A => Nothing) => A => Nothing"),
      NothingSize,
      needsParametricity
    )}
    nor can a pair of them                   ${target(
      tpe("[A] => ((A => Nothing, A => Nothing)) => (A => Nothing)"),
      NothingSize,
      needsParametricity
    )}
    Option's natural transformation          ${target(
      tpe("[A] => Option[A] => Option[A]"),
      BooleanSize,
      needsParametricity
    )}
    the signature of map                     ${target(
      tpe("[A, B] => (A => B) => Option[A] => Option[B]"),
      BooleanSize,
      needsParametricity
    )}

  Recursive types (section 8)
    a wrapper with no base case              ${src(
      "enum Loop { case Next(next: Loop) }"
    ) === NothingSize} (μX.X solved as a least fixed point)
    recursion without a seed                 ${target(
      tpe("[A] => (A => A) => A"),
      NothingSize,
      needsParametricity
    )}
    naturals are countable                   ${src(
      "enum Nat { case Zero; case Succ(n: Nat) }"
    ) === EffectiveOmega} (μX.(1 + X), pinned at ℵ₀ by iteration)
    Church numerals are countable            ${tpe(
      "[A] => (A => A) => A => A"
    ) === EffectiveOmega} (the fallback is the right count for the wrong reason)
    iteration keeps an extra argument        ${tpe(
      "[A, B] => (A => B => A) => A => B => A"
    ) === EffectiveOmega} (the fallback is the right count for the wrong reason)

  Targets (section 2): overlap
    a union with an overlapping member       ${target(
      tpe("Boolean | true"),
      BooleanSize,
      needsSubtyping
    )}
    the intersection of disjoint types       ${target(
      tpe("Byte & Boolean"),
      NothingSize,
      needsSubtyping
    )}
  """

}
