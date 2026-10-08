package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class InheritedOverrideSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Inherited declaration identity
      do not supply a tuple accessor's own inherited declaration $tupleAccessor
      do not supply an Either reverse accessor's own declaration $eitherReverse
      exclude the tuple functor map prototype                    $tupleMap
      exclude the Either functor map prototype                   $eitherMap
      preserve a distinct inherited overload as a capability      $otherOverload
      keep nominal products distinct from tuple syntax            $nominalOverload
      preserve class and method binder shadowing                  $shadowedBinders
      retain a different generic inherited capability             $otherGeneric
      retain an unresolved constructor rather than guessing      $unknownConstructor
  """

  private def count(name: String, code: String): Count = {
    val source = dialects.Scala3(code).parse[Source].get
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Overrides.scala", source)))
      .find(_.name == name)
      .get
      .count
  }

  def tupleAccessor =
    count(
      "Accessor.tupleAccessor.get",
      """trait Accessor[F[_, _]] { def get[X, A](fa: F[X, A]): A }
        |object Accessor {
        |  given tupleAccessor: Accessor[Tuple2] with {
        |    def get[X, A](fa: (X, A)): A = fa._2
        |  }
        |}
        |""".stripMargin
    ) === Count.Finite(1)

  def eitherReverse =
    count(
      "ReverseAccessor.eitherReverse.reverseGet",
      """trait ReverseAccessor[F[_, _]] { def reverseGet[X, A](a: A): F[X, A] }
        |object ReverseAccessor {
        |  given eitherReverse: ReverseAccessor[Either] with {
        |    def reverseGet[X, A](a: A): Either[X, A] = Right(a)
        |  }
        |}
        |""".stripMargin
    ) === Count.Finite(1)

  private def functor(carrier: String, parameter: String, result: String): String =
    s"""trait ForgetfulFunctor[F[_, _]] {
       |  def map[X, A, B](fa: F[X, A], f: A => B): F[X, B]
       |}
       |object ForgetfulFunctor {
       |  given instance: ForgetfulFunctor[$carrier] with {
       |    def map[X, A, B](fa: $parameter, f: A => B): $result = ???
       |  }
       |}
       |""".stripMargin

  def tupleMap =
    count("ForgetfulFunctor.instance.map", functor("Tuple2", "(X, A)", "(X, B)")) === Count.Finite(
      1
    )

  def eitherMap =
    count("ForgetfulFunctor.instance.map", functor("Either", "Either[X, A]", "Either[X, B]")) ===
      Count.Finite(1)

  def otherOverload =
    count(
      "Child.pick",
      """trait Base[A] { def pick(): A; def pick(value: A): A }
        |abstract class Child[A] extends Base[A] { def pick(value: A): A = value }
        |""".stripMargin
    ) === Count.Finite(2)

  def nominalOverload =
    count(
      "Child.pick",
      """case class Pair[A](left: A, right: A)
        |trait Base[A] { def pick(value: Pair[A]): A; def pick(value: (A, A)): A }
        |abstract class Child[A] extends Base[A] { def pick(value: (A, A)): A = value._1 }
        |""".stripMargin
    ) === Count.Countable

  def shadowedBinders =
    count(
      "Child.pick",
      """trait Base[A] { def pick[B](value: B): B }
        |class Child[A](outer: A) extends Base[A] { def pick[A](value: A): A = value }
        |""".stripMargin
    ) === Count.Finite(1)

  def otherGeneric =
    count(
      "Child.pick",
      """trait Base[A] { def pick[B](value: B): B; def create[C](): C }
        |abstract class Child[A] extends Base[A] { def pick[B](value: B): B = value }
        |""".stripMargin
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains("C")) must beTrue
    }

  def unknownConstructor =
    count(
      "Accessor.instance.get",
      """trait Accessor[F[_, _]] { def get[X, A](fa: F[X, A]): A }
        |object Accessor {
        |  given instance: Accessor[ExternalCarrier] with {
        |    def get[X, A](fa: ExternalCarrier[X, A]): A = ???
        |  }
        |}
        |""".stripMargin
    ) must beLike {
      case Count.Unresolved(reasons) => reasons.nonEmpty must beTrue
    }

}
