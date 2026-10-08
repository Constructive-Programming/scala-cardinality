package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.types.*

import Inhabitation.Shape
import MethodAnalysis.{Frame, Resolved}

/** Lexical binders keep their owner identity; unsupported bounds never become unconstrained atoms.
  */
private[cardinality] object SourceBindings {
  private given Dialect = dialects.Scala3

  def of(frame: Frame, inspect: String => Unit): Map[String, Resolved] =
    frame.chain.reverse.foldLeft(Map.empty[String, Resolved]) { (env, scope) =>
      inspect("binder scope")
      env ++ scope.typeParams.map { parameter =>
        inspect("binder")
        val resolved =
          if (TypeApplications.constrained(parameter))
            Left(s"bounded or higher-kinded parameter ${parameter.syntax}")
          else Right(Shape.Atom(s"${scope.id}:${parameter.name.value}"))
        parameter.name.value -> resolved
      }
    }

}
