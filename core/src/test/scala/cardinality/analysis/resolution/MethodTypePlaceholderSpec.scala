package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class MethodTypePlaceholderSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Placeholder syntax
      distinguishes constructor holes from existential wildcards $ast
      refuses a constructor placeholder                           $placeholder
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

  def placeholder =
    count("def pick[A](x: A): (Function1[A, *])[A] = a => a") must beLike {
      case Count.Unresolved(_) => ok
    }

}
