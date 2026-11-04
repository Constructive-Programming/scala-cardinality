package cardinality.types

import scala.meta.*

/** Tuple syntax, told apart by whether its elements are named. `(x: A, y: B)` is the opaque
  * `NamedTuple[("x", "y"), (A, B)]`, whose API converts both ways and selects fields: a value of
  * the product of its fields, yet no tuple to a match type (Scala 3.9.0 reduces neither `case (a,
  * b)` nor `case Int` on it, fixtures m1, m4). A `TypedParam` elsewhere is a dependent function's.
  */
private[cardinality] object NamedTuples {

  /** The field names and types of a named tuple type. */
  def unapply(tpe: Type): Option[List[(String, Type)]] = tpe match {
    case Type.Tuple(args) if args.nonEmpty =>
      Traversals.defined(args.map {
        case field: Type.TypedParam if field.mods.isEmpty => Some(field.name.value -> field.typ)
        case _                                            => None
      })
    case _ => None
  }

}

/** A tuple type whose elements are not named: the only tuple a match type reads as one. */
private[cardinality] object PositionalTuple {

  def unapply(tpe: Type): Option[List[Type]] = tpe match {
    case tuple: Type.Tuple if !tuple.args.exists(_.is[Type.TypedParam]) => Some(tuple.args)
    case _                                                              => None
  }

}
