package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Binding
import cardinality.types.TypeApplications

import MethodAnalysis.Resolved

/** One scope's value declarations, each resolved to the binding it names.
  *
  * A forward alias chain folds onto the value at its end; a chain that loops, names something
  * invisible, or has a body the model cannot read fails closed. A body this pass cannot follow is a
  * *specific* inhabitant of the value's type, and a total parametric body can only compute what the
  * environment already holds: its value is one of the bindings in scope, so skipping it loses
  * nothing and blocking the count would lose everything. Only an alias is worth following, because
  * it *names* one of those bindings and so raises the count's precision.
  *
  * `visible` gives the binding a name outside the scope has; `declaredType` reads a declared type
  * where the scope is.
  */
final private[cardinality] class CaptureAliases(
    stats: List[Stat],
    visible: String => Option[Binding],
    declaredType: Type => Resolved,
    resolver: Resolver
) {

  /** A concrete value's initializer and declared type. */
  private case class Initialized(body: Term, decltpe: Option[Type])

  // A `var` is read as the value it was initialized with: the model excludes the mutation that
  // could make it anything else.
  private val declared = stats
    .collect {
      case v: Defn.Val => (v.pats, Initialized(v.rhs, v.decltpe))
      case v: Defn.Var => (v.pats, Initialized(v.body, v.decltpe))
    }
    .flatMap((pats, v) => pats.flatMap(TypeApplications.patVarName.getOption(_).map(_ -> v)))
    .toMap

  private val done = mutable.Map.empty[String, Either[String, Binding]]

  /** Each alias the scope declares, with the binding it names, in name order. */
  def bindings: List[(String, Binding)] =
    declared.keys.toList.sorted.flatMap(name => value(name, Set.empty).toOption.map(name -> _))

  private def value(name: String, active: Set[String]): Either[String, Binding] =
    resolver.expanding("capture alias") {
      if (active(name)) Left(s"recursive capture alias: $name")
      else done.getOrElseUpdate(name, source(name, active))
    }

  private def source(name: String, active: Set[String]): Either[String, Binding] = {
    val declaration = declared(name)
    val named = declaration.body match {
      case Term.Name(n) if declared.contains(n) => value(n, active + name)
      case Term.Name(n)                         =>
        visible(n).toRight(s"capture alias not resolved: $name -> $n")
      case _ => Left(s"$name is not an alias")
    }
    named.flatMap(binding => checked(name, binding, declaration))
  }

  // A declared type must agree with the binding the alias names; a mismatch is a conversion the
  // model cannot follow.
  private def checked(
      name: String,
      binding: Binding,
      declaration: Initialized
  ): Either[String, Binding] =
    declaration.decltpe.fold(Right(binding): Either[String, Binding]) { tpe =>
      declaredType(tpe).flatMap { shape =>
        if (binding.shape == shape) Right(binding)
        else Left(s"capture alias type conversion not resolved: $name")
      }
    }

}
