package cardinality.analysis.resolution

import scala.compiletime.testing.typeChecks
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class OpaqueTypeScopeSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Opaque representation visibility
      resolve generic wraps in the defining object              $objectScope
      retain visibility in nested defining-object scopes        $nestedScope
      resolve top-level definitions in the same source           $topLevelScope
      resolve the named top-level companion scope                $companionScope
      count the six Direct-like wrapper signatures               $directLikeSignatures
      keep unrelated same-file objects opaque                   $unrelatedObject
      keep same-package definitions in other files opaque        $otherFile
      keep public aliases from leaking representations           $exportedAlias
      keep alias chains from changing the observing scope         $aliasChain
      allow external aliases when used from the defining scope   $externalAliasInside
      preserve declaring binders under method shadowing           $shadowing
      count both constructor capture and input without shadowing $nonShadowing
      count just the input when no constructor capture exists     $inputOnly
      keep representations hidden through constructor fields     $productBoundary
      keep inherited declarations hidden from an external caller $inheritedBoundary
      preserve failed inherited opaque argument substitutions    $inheritedArgumentBoundary
      keep type-lambda aliases hidden outside their scope         $lambdaBoundary
      retain higher-kinded/evidence diagnostics                   $higherKindedBoundary
      compile the visibility rules with Scala, not just parsing  $compiledVisibility
  """

  private def inputs(files: (String, String)*): List[MethodAnalysis.Input] =
    files.toList.map { (path, code) =>
      MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)
    }

  private def count(name: String, files: (String, String)*): Count =
    MethodAnalysis.analyze(inputs(files*)).find(_.name == name).get.count

  private def hidden(value: Count) =
    value must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("opaque representation is not visible")) must beTrue
    }

  def objectScope =
    count(
      "Owner.wrap",
      "Owner.scala" ->
        "object Owner { opaque type T[X, A] = A; def wrap[X, A](a: A): T[X, A] = ??? }"
    ) === Count.Finite(1)

  def nestedScope =
    count(
      "Owner.Nested.get",
      "Owner.scala" ->
        "object Owner { opaque type T[A] = A; object Nested { def get[A](a: T[A]): A = ??? } }"
    ) === Count.Finite(1)

  def topLevelScope =
    count(
      "wrap",
      "Top.scala" ->
        "opaque type T[A] = A; def wrap[A](a: A): T[A] = ???"
    ) === Count.Finite(1)

  def companionScope =
    count(
      "T.get",
      "Top.scala" ->
        "opaque type T[A] = A; object T { def get[A](a: T[A]): A = ??? }"
    ) === Count.Finite(1)

  def directLikeSignatures = {
    val source = inputs(
      "Carrier.scala" ->
        """opaque type Carrier[X, A] = A
        |object Carrier {
        |  def wrap[X, A](a: A): Carrier[X, A] = ???
        |  extension [X, A](d: Carrier[X, A]) def value: A = ???
        |  def get[X, A](fa: Carrier[X, A]): A = ???
        |  def reverseGet[X, A](a: A): Carrier[X, A] = ???
        |  def map[X, A, B](fa: Carrier[X, A], f: A => B): Carrier[X, B] = ???
        |  def pure[X, A](a: A): Carrier[X, A] = ???
        |}
        |""".stripMargin
    )
    val methods = MethodAnalysis.analyze(source)
    (methods.size === 6).and(methods.map(_.count) === List.fill(6)(Count.Finite(1)))
  }

  def unrelatedObject =
    hidden(
      count(
        "Other.get",
        "Top.scala" ->
          "opaque type T[A] = A; object Other { def get[A](a: T[A]): A = ??? }"
      )
    )

  def otherFile =
    hidden(
      count(
        "sample.get",
        "Type.scala" -> "package sample; opaque type T[A] = A",
        "Use.scala" -> "package sample; def get[A](a: T[A]): A = ???"
      )
    )

  def exportedAlias =
    hidden(
      count(
        "get",
        "Owner.scala" ->
          ("object Owner { opaque type T[A] = A; type Public[A] = T[A] }; " +
            "def get[A](a: Owner.Public[A]): A = ???")
      )
    )

  def aliasChain =
    hidden(
      count(
        "get",
        "Owner.scala" ->
          ("object Owner { opaque type T[A] = A; type Public[A] = T[A] }; " +
            "type Again[A] = Owner.Public[A]; def get[A](a: Again[A]): A = ???")
      )
    )

  def externalAliasInside =
    count(
      "Owner.get",
      "Owner.scala" ->
        ("object Owner { opaque type T[A] = A; def get[A](a: External[A]): A = ??? }; " +
          "type External[A] = Owner.T[A]")
    ) === Count.Finite(1)

  def shadowing =
    (count(
      "Owner.get",
      "Owner.scala" ->
        "class Owner[A](outer: A) { opaque type T = A; def get[A](a: T): A = ??? }"
    ) === Count.Finite(0)).and(
      typeChecks(
        "class Owner[A](outer: A) { opaque type T = A; def get[A](a: T): A = a }"
      ) must beFalse
    )

  def nonShadowing =
    count(
      "Owner.get",
      "Owner.scala" ->
        "class Owner[A](outer: A) { opaque type T = A; def get(a: T): A = a }"
    ) === Count.Finite(2)

  def inputOnly =
    count(
      "Owner.get",
      "Owner.scala" ->
        "class Owner[A] { opaque type T = A; def get(a: T): A = a }"
    ) === Count.Finite(1)

  def productBoundary =
    hidden(
      count(
        "get",
        "Owner.scala" ->
          ("object Owner { opaque type T[A] = A }; case class Box[A](value: Owner.T[A]); " +
            "def get[A](box: Box[A]): A = ???")
      )
    )

  def inheritedBoundary =
    hidden(
      count(
        "sample.Child.get",
        "Type.scala" ->
          "package sample; opaque type T[A] = A; trait Parent[A] { val value: T[A] }",
        "Use.scala" ->
          "package sample; class Child[A] extends Parent[A] { def get: A = ??? }"
      )
    )

  def inheritedArgumentBoundary =
    hidden(
      count(
        "Child.get",
        "Types.scala" ->
          """opaque type T[A] <: A = A
        |trait Parent[X] { val value: X }
        |abstract class Child[A] extends Parent[T[A]] { def get: A = value }
        |""".stripMargin
      )
    )

  def lambdaBoundary =
    hidden(
      count(
        "get",
        "Owner.scala" ->
          ("object Owner { opaque type T = [A] =>> (A, A) }; " +
            "def get[A](pair: Owner.T[A]): A = ???")
      )
    )

  def higherKindedBoundary =
    count(
      "Owner.get",
      "Owner.scala" ->
        "object Owner { opaque type T[F[_], A] = F[A]; def get[F[_], A](a: T[F, A]): A = ??? }"
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("bounded or higher-kinded")) must beTrue
    }

  def compiledVisibility = {
    import opaquefixture.*
    val wrapped = OpaqueTop[Unit, Int](3)
    (OpaqueTop.value(wrapped) === 3)
      .and(
        OpaqueTop.value(OpaqueTop.Nested.wrap(4)) === 4
      )
      .and(OpaqueUnrelated.representationVisible must beFalse)
      .and(
        typeChecks(
          "val value: Int = cardinality.analysis.resolution.opaquefixture.OpaqueTop[Unit, Int](1)"
        ) must beFalse
      )
      .and(
        typeChecks(
          "val value: Int = cardinality.analysis.resolution.opaquefixture.OpaqueOwner.wrap(1): " +
            "cardinality.analysis.resolution.opaquefixture.OpaqueOwner.Exported[Int]"
        ) must beFalse
      )
  }

}
