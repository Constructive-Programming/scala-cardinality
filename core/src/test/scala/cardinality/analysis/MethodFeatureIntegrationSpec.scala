package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.types.*
import org.specs2.Specification

class MethodFeatureIntegrationSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Cross-feature shape traversal
      normalize equality inside sequence introduction            $equalitySequence
      normalize equality inside singleton representations         $equalitySingleton
      validate private singleton aliases inside repeated types    $privateRepeatedAlias
    Syntax-preserving aliases and polymorphic binders
      keep a polymorphic identity binder distinct from alias args $polyAliasShadowing
      protect nested type-lambda binders during alias substitution $lambdaAliasShadowing
      reject private singleton leaks through refined members       $privateRefinedMember
      preserve nominal projected member names during substitution $projectedMemberName
      preserve refinement member scope during substitution         $refinementMemberScope
  """

  private val a = Atom("A")
  private val b = Atom("B")
  private val equality = Evidence(a, b, true)

  def equalitySequence =
    Inhabitation.count(
      List(Binding("evidence", equality), Binding("seed", a)),
      Repeated(b)
    ) === Count.Countable

  def equalitySingleton =
    EvidenceAnalysis
      .context(List(equality))
      .flatMap(_(Singleton("x", Some(b)))) === Right(Singleton("x", Some(a)))

  def privateRepeatedAlias = {
    val source = dialects
      .Scala3(
        """object Owner { private object Hidden; type Leak = Hidden.type }
        |def consume[A](xs: Owner.Leak*): Boolean = true
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

  def lambdaAliasShadowing =
    methodCount(
      "type Nested[A] = (([A] =>> (A, A))[Boolean], A); def f: Nested[Unit] = ???"
    ) === Count.Finite(4)

  def privateRefinedMember =
    methodCount(
      "object Owner { private object Hidden; type R = Any { type X = Hidden.type } }; " +
        "def f: Owner.R#X = ???"
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("singleton")) must beTrue
    }

  def projectedMemberName = {
    val source = dialects.Scala3("Owner#B").parse[Type].get
    val argument = dialects.Scala3("(Boolean, Boolean)").parse[Type].get
    TypeSubstitution.replace(source, Map(TypeName.of("B") -> argument)).structure ===
      source.structure
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
    TypeSubstitution
      .replace(
        source,
        Map(TypeName.of("A") -> argument, TypeName.of("Outer") -> unitType)
      )
      .structure === expected.structure
  }

}
