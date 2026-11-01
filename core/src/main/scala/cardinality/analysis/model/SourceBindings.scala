package cardinality.analysis.model

import scala.meta.Type

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.types.*

import Inhabitation.Shape
import MethodAnalysis.{Frame, Resolved}

/** Lexical binders keep their owner identity. A proper binder is an atom — its bounds are read
  * where the atom is met (`BinderBounds`) — and a rigid higher-kinded one a constructor applied as
  * an uninterpreted head; a binder the model cannot read rigidly carries the reason, which counts
  * only where the binder is mentioned.
  */
private[cardinality] object SourceBindings {

  /** The identity of a binder: its declaring scope and its name. */
  def atom(scope: Frame, parameter: Type.Param): String = s"${scope.id}:${parameter.name.value}"

  def of(frame: Frame, inspect: String => Unit, operators: TypeOperators): Map[String, Resolved] =
    frame.chain.reverse.foldLeft(Map.empty[String, Resolved]) { (env, scope) =>
      inspect("binder scope")
      env ++ scope.typeParams.map { parameter =>
        inspect("binder")
        val id = atom(scope, parameter)
        val resolved = Kinds.refusal(parameter).toLeft(Kinds.of(parameter)).map {
          case Kind.Proper => Shape.Atom(id)
          case kind        => operators.rigid(id, kind)
        }
        parameter.name.value -> resolved
      }
    }

}
