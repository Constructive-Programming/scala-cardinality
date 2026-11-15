package cardinality.compiled

import scala.util.Try

import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

/** Whether the implementation of a member may be given a value that holds a capability record
  * (`Reentry`'s narrowing of a site without inputs, `Reentry.opens` before).
  */
private[compiled] object Openings {

  /** Whether an implementation of a member of this type is args a value that may hold a record: one
    * of a value parameter's (`carrier`), or one an abstract member of its result's class (a
    * function's `apply` included) is args, which the implementation builds; one not read may.
    * `args` holds the type arguments of the class whose member is read.
    */
  def opens(
      tpe: TypeOrMethodic,
      args: Map[Symbol, TypeOrWildcard] = Map.empty,
      seen: Set[ClassSymbol] = Set.empty
  )(using Context): Boolean = Try(tpe match {
    case poly: PolyType     => opens(poly.resultType, args, seen)
    case method: MethodType =>
      method.paramTypes.exists(carrier(_, args)) || opens(method.resultType, args, seen)
    case byName: ByNameType => opens(byName.resultType, args, seen)
    case result: Type       =>
      !Reentry.plainType(result) && (World.functional(result) || Reentry.classOf(result).forall {
        k =>
          val arguments: Map[Symbol, TypeOrWildcard] = result.dealias match {
            case applied: AppliedType => k.typeParams.zip(applied.args.map(resolve(_, args))).toMap
            case _                    => Map.empty
          }
          !seen(k) && k.linearization.filterNot(World.universal).flatMap(_.declarations).exists {
            case m: TermSymbol => m.isAbstractMember && opens(m.declaredType, arguments, seen + k)
            case _             => false
          }
      })
  }).getOrElse(true)

  /** Whether a value of this type may hold a record: no plain value (`plainType`), and no type
    * parameter without a bound, which a parametric body cannot use; `args` substitutes a class's
    * type arguments for its parameters.
    */
  private def carrier(tpe: Type, args: Map[Symbol, TypeOrWildcard])(using Context): Boolean =
    resolve(tpe, args) match {
      case ref: TypeRef =>
        ref.optSymbol match {
          case Some(p: (ClassTypeParamSymbol | LocalTypeParamSymbol)) => bounded(p.declaredBounds)
          case _                                                      => !Reentry.plainType(ref)
        }
      case ref: TypeParamRef => bounded(ref.bounds)
      case other: Type       => !Reentry.plainType(other)
      case _                 => true
    }

  // Whether a type parameter's upper bound is no top type.
  private def bounded(bounds: TypeBounds)(using Context): Boolean = !(bounds.high match {
    case lambda: TypeLambda => Reentry.classOf(lambda.resultType)
    case high               => Reentry.classOf(high)
  }).exists(World.universal)

  private def resolve(tpe: TypeOrWildcard, args: Map[Symbol, TypeOrWildcard])(using
      Context
  ): TypeOrWildcard = tpe match {
    case ref: TypeRef => ref.optSymbol.flatMap(args.get).getOrElse(ref)
    case other        => other
  }

}
