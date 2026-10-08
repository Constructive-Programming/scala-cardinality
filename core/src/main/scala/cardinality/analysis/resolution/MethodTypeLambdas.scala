package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.*
import cardinality.types.*

import Inhabitation.Shape
import MethodAnalysis.Resolved

/** Hygienic first-order beta application and the closed total polymorphic identity fragment. */
private[cardinality] object MethodTypeLambdas {
  private given Dialect = dialects.Scala3

  def apply(
      lambda: Type.Lambda,
      arguments: List[Shape],
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = {
    val parameters = lambda.tparamClause.values
    if (parameters.exists(TypeApplications.constrained))
      Left(s"constrained type lambda: ${lambda.syntax}")
    else if (parameters.size != arguments.size)
      Left(s"type lambda argument arity: ${lambda.syntax}")
    else
      lambda.body match {
        case body: Type =>
          val substitutions = parameters.zip(arguments).map((p, a) => p.name.value -> Right(a))
          context
            .copy(variables = context.variables ++ substitutions)
            .enter(s"lambda:${lambda.pos.start}:${lambda.syntax}")
            .read(body, resolver)
        case _ => Left("type lambda has a non-type body")
      }
  }

  def resolve(
      poly: Type.PolyFunction,
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved =
    if (poly.tparamClause.values.exists(TypeApplications.constrained))
      Left(s"constrained polymorphic function: ${poly.syntax}")
    else
      readBody(poly, context, resolver).flatMap { _ =>
        if (identity(poly)) Right(Shape.Product(Nil))
        else Left(s"polymorphic function outside the closed identity fragment: ${poly.syntax}")
      }

  // Reading the body first retains dependency/evidence diagnostics; no context evidence executes.
  private def readBody(
      poly: Type.PolyFunction,
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = {
    val binders = poly.tparamClause.values.map { parameter =>
      parameter.name.value ->
        Right(Shape.Atom(s"${context.frame.id}:poly:${poly.pos.start}:${parameter.name.value}"))
    }
    poly.body match {
      case body: Type => context.copy(variables = context.variables ++ binders).read(body, resolver)
      case _          => Left("polymorphic function has a non-type body")
    }
  }

  private def identity(poly: Type.PolyFunction): Boolean =
    (poly.tparamClause.values, poly.body) match {
      case (List(parameter), function: Type.Function) =>
        identityArrow(parameter.name.value, function)
      case _ => false
    }

  private def identityArrow(name: String, function: Type.Function): Boolean =
    (function.paramClause.values, function.res) match {
      case (List(input: Type.Name), output: Type.Name) =>
        input.value == name && output.value == name
      case _ => false
    }

}
