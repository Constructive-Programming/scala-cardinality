package cardinality.analysis

import cardinality.Pending
import cardinality.analysis.inhabitation.*
import cardinality.capacity.*
import org.specs2.Specification

class MethodFeatureIntegrationSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Cross-feature shape traversal
      eliminate repeated inputs behind singleton widening        $singletonSequenceInput
      eliminate repeated inputs inside introduced singleton arrows $singletonSequenceArrow
      construct an accessible sequence singleton uniquely         $singletonSequenceResult
      normalize equality inside sequence introduction            $equalitySequence
      normalize equality inside singleton representations         $equalitySingleton
      read an accessible singleton proof with its provenance      $singletonProof
      validate evidence even for singleton repeated goals         $invalidEvidence
      validate private singleton aliases inside repeated types    ${Pending.rejectedFixture(
      privateRepeatedAlias
    )}
    Syntax-preserving aliases and polymorphic binders
      keep a polymorphic identity binder distinct from alias args $polyAliasShadowing
      substitute free outer types inside polymorphic bodies       ${Pending.knownLoss(
      polyAliasOuter
    )}
      protect nested type-lambda binders during alias substitution $lambdaAliasShadowing
      substitute fixed match patterns for stored-value analysis    $fixedMatchPattern
      reject private singleton leaks through refined members       ${Pending.rejectedFixture(
      privateRefinedMember
    )}
      avoid capture by user binders resembling internal tokens     ${Pending.knownLoss(
      internalTokenShadowing
    )}
  """

  private val a = Atom("A")
  private val b = Atom("B")
  private val unit = Product(Nil)
  private val equality = Evidence(a, b, true)

  // The sequence a singleton widens to is its binding's own: eliminated by length. Empty, it leaves
  // no A (0); beside a seed, one element already gives two answers, so every length does (ω).
  def singletonSequenceInput = {
    val xs = Binding("xs", Singleton("xs", Some(Repeated(a))))
    (Inhabitation.count(List(xs), a) === Count.Finite(0))
      .and(Inhabitation.count(List(xs, Binding("seed", a)), a) === Count.Countable)
  }

  // A goal function's parameters are its body's environment: `(xs: xs.type) => A` is 0, as above.
  def singletonSequenceArrow =
    Inhabitation.count(Nil, Function(List(Singleton("xs", Some(Repeated(a)))), a)) ===
      Count.Finite(0)

  def singletonSequenceResult =
    Inhabitation.count(Nil, Singleton("xs", Some(Repeated(a)))) === Count.Finite(1)

  def equalitySequence =
    Inhabitation.count(
      List(Binding("evidence", equality), Binding("seed", a)),
      Repeated(b)
    ) === Count.Countable

  def equalitySingleton =
    EvidenceAnalysis
      .context(List(equality))
      .flatMap(_.rewrite(Singleton("x", Some(b)))) === Right(Singleton("x", Some(a)))

  def singletonProof =
    Inhabitation.count(
      List(Binding("evidence", Singleton("ev", Some(equality))), Binding("value", a)),
      b
    ) === Count.Finite(1)

  def invalidEvidence =
    Inhabitation.count(
      List(Binding("xs", Repeated(a)), Binding("bad", Evidence(unit, a, true))),
      unit
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("free-atom or rigid-application endpoints")) must beTrue
    }

  def privateRepeatedAlias = {
    val source =
      """object Owner { private object Hidden; type Leak = Hidden.type }
        |def consume[A](xs: Owner.Leak*): Boolean = true
        |""".stripMargin
    cardinality.Compiled
      .analyze(List(cardinality.Compiled.Input("Integration.scala", source)))
      .find(_.name == "consume")
      .get
      .count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("singleton")) must beTrue
    }
  }

  private def methodCount(code: String): Count = {
    cardinality.Compiled
      .analyze(List(cardinality.Compiled.Input("Binders.scala", code)))
      .find(_.name == "f")
      .get
      .count
  }

  def polyAliasShadowing =
    methodCount("type Id[A] = [A] => A => A; def f: Id[(Boolean, Boolean)] = ???") ===
      Count.Finite(1)

  def polyAliasOuter =
    methodCount("type Outer[A] = [B] => A => A; def f[A](x: A): Outer[A] = ???") must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("outside the closed identity fragment")) must beTrue
    }

  def lambdaAliasShadowing =
    methodCount(
      "type Nested[A] = (([A] =>> (A, A))[Boolean], A); def f: Nested[Unit] = ???"
    ) === Count.Finite(4)

  def fixedMatchPattern = {
    val rows = cardinality.Compiled.definitions(
      """type F[A, B] = A match {
        |  case B => Boolean
        |  case _ => Unit
        |}
        |case class Result(value: F[Boolean, Boolean])
        |""".stripMargin
    )
    rows.find(_.name == "Result").get.size === Some(TinySize(2))
  }

  def privateRefinedMember =
    methodCount(
      "object Owner { private object Hidden; type R = Any { type X = Hidden.type } }; " +
        "def f: Owner.R#X = ???"
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("singleton")) must beTrue
    }

  def internalTokenShadowing =
    methodCount(
      "type Outer[A] = [`$cardinalityProjection1`] => A => A; " +
        "def f[A](x: A): Outer[A] = ???"
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("outside the closed identity fragment")) must beTrue
    }

}
