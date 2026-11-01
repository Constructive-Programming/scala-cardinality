package cardinality.analysis.model

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.Resolved
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.types.{Kind, Kinds}

/** What a higher-kinded name is bound to: a rigid binder, or a constructor written at some scope
  * and applied later — a closure over the scope it was written in, so applying it reads the
  * constructor's names where they were written (hygienic β-reduction, no syntactic substitution).
  */
private[cardinality] enum Operator {
  case Rigid(id: String, kind: Kind)
  case Closure(tpe: Type, context: ResolutionContext)
}

/** The operators of one analysis. A binding environment holds `Shape.Constructor(token)` for a
  * higher-kinded name, never a value type; the token names the operator here. Equal closures — the
  * same syntax read at the same reference under the same bindings — share one token. `read`
  * resolves a proper argument where it is written; `kindOf` is the kind an argument's declaration
  * fixes, if it fixes one.
  */
final private[cardinality] class TypeOperators(
    read: (Type, ResolutionContext) => Resolved,
    kindOf: (Type, ResolutionContext) => Option[Kind]
) {

  private val table = mutable.Map.empty[String, Operator]
  private val closures = mutable.Map.empty[ClosureKey, String]

  /** Whether a binding is a rigid binder's constructor: a higher-kinded name read as itself. */
  def isRigid(shape: Shape): Boolean = shape match {
    case Shape.Constructor(token) => table.get(token).exists(_.isInstanceOf[Operator.Rigid])
    case _                        => false
  }

  def rigid(id: String, kind: Kind): Shape = {
    val token = s"rigid:$id"
    if (!table.contains(token)) table.update(token, Operator.Rigid(id, kind))
    Shape.Constructor(token)
  }

  def closure(tpe: Type, context: ResolutionContext): Shape = {
    val key = ClosureKey(
      tpe.syntax,
      tpe.pos.start,
      context.frame.id,
      context.useSite.id,
      context.at,
      context.visiting,
      context.variables
    )
    val token = closures.getOrElseUpdate(
      key, {
        val fresh = s"closure:${closures.size}"
        table(fresh) = Operator.Closure(tpe, context)
        fresh
      }
    )
    Shape.Constructor(token)
  }

  /** An argument written where a constructor of `kind` is expected: a higher-kinded name passes its
    * operator through (a rigid one only at its own kind), anything else is a closure over the
    * argument's scope, read only if it is applied — once its declared kind, where it has one, is
    * the expected one.
    */
  def operand(argument: Type, kind: Kind, context: ResolutionContext): Either[String, Shape] = {
    def mismatch(own: String) =
      Left(s"kind mismatch: ${argument.syntax} has kind $own, expected ${kind.render}")
    argument match {
      case n: Type.Name if context.variables.contains(n.value) =>
        applicable(n, context) match {
          case Right(Operator.Rigid(_, own)) if own != kind => mismatch(own.render)
          case Right(_)                                     => context.variables(n.value)
          case Left(_) => context.variables(n.value).flatMap(_ => mismatch("*"))
        }
      case other =>
        kindOf(other, context) match {
          case Some(own) if own != kind => mismatch(own.render)
          case _                        => Right(closure(other, context))
        }
    }
  }

  /** An argument for a declared type parameter, read by the parameter's kind: a type where it is
    * written, or an operator over the scope it is written in.
    */
  val argument: TypeOperators.Binder = binder(read)

  /** How a declared parameter binds its argument: a proper one to what `proper` reads (a type, or a
    * nominal key), a higher-kinded one to its operator.
    */
  def binder(proper: (Type, ResolutionContext) => Resolved): TypeOperators.Binder =
    (kind, argument, context) =>
      kind match {
        case Kind.Proper => proper(argument, context)
        case arrow       => operand(argument, arrow, context)
      }

  /** Whether resolved arguments have the kinds their declared parameters take: a constructor for a
    * higher-kinded parameter, a type for a proper one.
    */
  def kinded(name: String, parameters: List[Type.Param], args: List[Shape]): Either[String, Unit] =
    parameters
      .zip(args)
      .collectFirst {
        case (parameter, argument)
            if proper(parameter) == argument.isInstanceOf[Shape.Constructor] =>
          s"kind mismatch: ${parameter.name.value} of $name has kind ${kindOf(argument)}, " +
            s"expected ${Kinds.of(parameter).render}"
      }
      .toLeft(())

  private def proper(parameter: Type.Param): Boolean = Kinds.of(parameter) == Kind.Proper

  // A rigid binder's kind is declared; a closure's is the one its position expected, unrecorded.
  private def kindOf(argument: Shape): String = argument match {
    case Shape.Constructor(token) =>
      table(token) match {
        case Operator.Rigid(_, kind) => kind.render
        case _: Operator.Closure     => "(…) → *"
      }
    case _ => Kind.Proper.render
  }

  /** The operator a bound name applies, or why it applies none. */
  def applicable(name: Type.Name, context: ResolutionContext): Either[String, Operator] =
    context.variables(name.value).flatMap {
      case Shape.Constructor(token) => Right(table(token))
      case _ => Left(s"type parameter is not an applicable constructor: ${name.value}")
    }

  // A closure's identity: what it spells, where, and every binding its reading depends on.
  private case class ClosureKey(
      syntax: String,
      position: Int,
      frame: String,
      useSite: String,
      at: Option[(String, Int)],
      visiting: Set[String],
      variables: Map[String, Either[String, Shape]]
  )

}

private[cardinality] object TypeOperators {

  /** How a reading binds one argument of the given kind, written at the given context. */
  type Binder = (Kind, Type, ResolutionContext) => Resolved

}
