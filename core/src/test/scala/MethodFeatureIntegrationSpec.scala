package cardinality

import scala.meta.*

import org.specs2.Specification

class MethodFeatureIntegrationSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Cross-feature shape traversal
      detect repeated inputs behind singleton widening           $singletonSequenceInput
      detect repeated inputs inside introduced singleton arrows  $singletonSequenceArrow
      construct an accessible sequence singleton uniquely         $singletonSequenceResult
      normalize equality inside sequence introduction            $equalitySequence
      normalize equality inside singleton representations         $equalitySingleton
      read an accessible singleton proof with its provenance      $singletonProof
      validate evidence even for singleton repeated goals         $invalidEvidence
      validate private singleton aliases inside repeated types    $privateRepeatedAlias
    Syntax-preserving aliases and polymorphic binders
      keep a polymorphic identity binder distinct from alias args $polyAliasShadowing
      substitute free outer types inside polymorphic bodies       $polyAliasOuter
      protect nested type-lambda binders during alias substitution $lambdaAliasShadowing
      substitute fixed match patterns for stored-value analysis    $fixedMatchPattern
      reject private singleton leaks through refined members       $privateRefinedMember
      avoid capture by user binders resembling internal tokens     $internalTokenShadowing
      preserve nominal projected member names during substitution $projectedMemberName
      preserve refinement member scope during substitution         $refinementMemberScope
  """

  private val a = Atom("A")
  private val b = Atom("B")
  private val unit = Product(Nil)
  private val equality = Evidence(a, b, true)

  private def sequenceUnsupported(count: Count) =
    count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("Repeated-argument sequence elimination")) must beTrue
    }

  def singletonSequenceInput =
    sequenceUnsupported(
      Inhabitation.count(List(Binding("xs", Singleton("xs", Some(Repeated(a))))), a)
    )

  def singletonSequenceArrow =
    sequenceUnsupported(
      Inhabitation.count(Nil, Function(List(Singleton("xs", Some(Repeated(a)))), a))
    )

  def singletonSequenceResult =
    Inhabitation.count(Nil, Singleton("xs", Some(Repeated(a)))) === Count.Finite(1)

  def equalitySequence =
    Inhabitation.count(
      List(Binding("evidence", equality), Binding("seed", a)),
      Repeated(b)
    ) === Count.Countable

  def equalitySingleton =
    EvidenceAnalysis
      .context(List(equality), Singleton("x", Some(b)))
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
        reasons.exists(_.contains("free-atom endpoints")) must beTrue
    }

  def privateRepeatedAlias = {
    val source = dialects
      .Scala3(
        """object Owner { private object Hidden; type Leak = Hidden.type }
        |def consume[A](xs: Owner.Leak*): Unit = ()
        |""".stripMargin
      )
      .parse[Source]
      .get
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Integration.scala", source)))
      .find(_.name == "consume")
      .get
      .count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("singleton")) must beTrue
    }
  }

  private def methodCount(code: String): Count = {
    val source = dialects.Scala3(code).parse[Source].get
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Binders.scala", source)))
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
    val source = dialects
      .Scala3(
        """type F[A, B] = A match {
        |  case B => Boolean
        |  case _ => Unit
        |}
        |case class Result(value: F[Boolean, Boolean])
        |""".stripMargin
      )
      .parse[Source]
      .get
    Counter.definitions(source).find(_.name == "Result").get.size === Some(TinySize(2))
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

  def projectedMemberName = {
    val source = dialects.Scala3("Owner#B").parse[Type].get
    val argument = dialects.Scala3("(Boolean, Boolean)").parse[Type].get
    MatchTypes.replace(source, Map(TypeName.of("B") -> argument)).structure === source.structure
  }

  def refinementMemberScope = {
    val source = dialects
      .Scala3(
        "Base[A] { type A = Boolean; type X = A; type Y = Outer }"
      )
      .parse[Type]
      .get
    val argument = dialects.Scala3("(Boolean, Boolean)").parse[Type].get
    val unitType = dialects.Scala3("Unit").parse[Type].get
    val expected = dialects
      .Scala3(
        "Base[(Boolean, Boolean)] { type A = Boolean; type X = A; type Y = Unit }"
      )
      .parse[Type]
      .get
    MatchTypes
      .replace(
        source,
        Map(TypeName.of("A") -> argument, TypeName.of("Outer") -> unitType)
      )
      .structure === expected.structure
  }

}
