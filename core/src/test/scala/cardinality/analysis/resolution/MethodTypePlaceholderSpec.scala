package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.types.*
import org.specs2.Specification

class MethodTypePlaceholderSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Placeholder syntax
      distinguishes constructor holes from existential wildcards $ast
      beta applies a partially applied function constructor       $partial
      resolves applied holes nested inside a product              $nested
      normalizes holes in named constructor aliases               $alias
      analyzes constructor inputs through the same hole rule      $constructor
      preserves free binders even with generated-name collisions  $hygiene
      normalizes holes without capturing existing type names      $helperHygiene
      keeps nested placeholder binders separate                   $binders
      keeps existential result construction separate from capture $existential
      retains upper and lower existential bounds in diagnostics   $bounds
      rejects unapplied higher-kinded arguments                   $higherKinded
      rejects placeholder application with the wrong arity        $arity
  """

  def ast = {
    val hole = dialects.Scala3("Function1[A, *]").parse[Type].get
    val unknown = dialects.Scala3("Option[?]").parse[Type].get
    (hole must beAnInstanceOf[Type.AnonymousLambda]).and(
      unknown.collect { case _: Type.Wildcard => true } === List(true)
    )
  }

  private def count(code: String, name: String = "pick"): Count =
    MethodAnalysis
      .analyze(
        List(
          MethodAnalysis.Input(
            "Placeholder.scala",
            dialects.Scala3(code).parse[Source].get
          )
        )
      )
      .find(_.name == name)
      .get
      .count

  private def unresolved(code: String, fragment: String) =
    count(code) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(fragment)) must beTrue
    }

  def partial =
    count("def pick[A](x: A): (Function1[A, *])[A] = a => a") === Count.Finite(2)

  def nested =
    count(
      "case class MultiFocus[A](value: A)\n" +
        "def pick[A](x: A): MultiFocus[(Function1[A, *])[A]] = ???"
    ) === Count.Finite(2)

  def alias =
    count(
      "type FromUnit = Function1[Unit, *]\n" +
        "def pick[A](x: A): FromUnit[A] = ???"
    ) === Count.Finite(1)

  def constructor =
    // The supplied endomorphism is opaque, unlike the closed polymorphic identity: a field
    // can be identity, f, f composed with f, and so on.
    count("case class Holder[A](f: (Function1[A, *])[A])", "Holder.<init>") === Count.Countable

  def hygiene =
    count(
      "def pick[cardinalityHole0, B](x: cardinalityHole0): " +
        "(Function1[cardinalityHole0, *])[B] = ???"
    ) === Count.Finite(0)

  def helperHygiene = {
    val parsed = dialects.Scala3("Either[cardinalityHole0, *]").parse[Type].get
    val normalized = TypePlaceholders.lambda(parsed).get.toOption.get
    val binder = normalized.tparamClause.values.head.name.value
    (binder !== "cardinalityHole0").and(
      normalized.body.asInstanceOf[Type.Apply].argClause.values.map(_.syntax) ===
        List("cardinalityHole0", binder)
    )
  }

  def binders = {
    val parsed = dialects.Scala3("Either[*, Function1[A, *]]").parse[Type].get
    val normalized = TypePlaceholders.lambda(parsed).get.toOption.get
    (normalized.tparamClause.values.size === 1).and(
      normalized.body.collect { case _: Type.AnonymousParam => true } === List(true)
    )
  }

  def existential =
    unresolved("def pick(x: Option[?]): Option[?] = x", "existential wildcard")

  def bounds =
    unresolved("def pick[A](x: Option[? <: A]): A = ???", "? <: A").and(
      unresolved("def pick[A](x: Option[? >: A]): A = ???", "? >: A")
    )

  def higherKinded =
    unresolved(
      "case class MultiFocus[F[_], A](value: F[A])\n" +
        "def pick[A](x: A): MultiFocus[Function1[A, *], A] = ???",
      "unapplied constructor placeholder"
    )

  def arity =
    unresolved("def pick[A](x: A): (Either[*, *])[A] = ???", "arity")

}
