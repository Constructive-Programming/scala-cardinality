package cardinality.types

import scala.meta.*

/** Constructor holes are bound by scalameta's AnonymousLambda, not existential wildcards. Normalize
  * only that binder; beta application remains the resolver's responsibility.
  */
private[cardinality] object TypePlaceholders {
  private given Dialect = dialects.Scala3

  def lambda(tpe: Type): Option[Either[String, Type.Lambda]] = tpe match {
    case anonymous: Type.AnonymousLambda =>
      val used = anonymous.collect { case n: Type.Name => n.value }.toSet
      var names = List.empty[String]
      var failure = Option.empty[String]
      def replace(tree: Type): Type =
        tree match {
          // Stop before descending: nested lambdas own their holes.
          case nested: Type.AnonymousLambda => nested
          case nested: Type.Lambda          => nested
          case hole: Type.AnonymousParam    =>
            if (hole.variant.nonEmpty)
              failure = Some("variant constructor placeholder is unsupported")
            val name = Iterator
              .from(names.size)
              .map(i => s"cardinalityHole$i")
              .find(n => !used(n) && !names.contains(n))
              .get
            names = names :+ name
            Type.Name(name)
          case applied: Type.Apply =>
            applied.copy(
              tpe = replace(applied.tpe),
              argClause = Type.ArgClause(applied.argClause.values.map(replace))
            )
          case Type.Tuple(args) => Type.Tuple(args.map(replace))
          case other            =>
            if (other.collect { case _: Type.AnonymousParam => true }.nonEmpty)
              failure = Some("constructor placeholder body requires unsupported hole traversal")
            other
        }
      val rewritten = replace(anonymous.tpe)
      Some(failure match {
        case Some(reason)          => Left(reason)
        case None if names.isEmpty => Left("constructor placeholder has no local holes")
        case None                  =>
          val params = names.map(name =>
            Type.Param(
              Nil,
              Type.Name(name),
              Type.ParamClause(Nil),
              Type.Bounds(None, None, Nil, Nil)
            )
          )
          Right(Type.Lambda(Type.ParamClause(params), rewritten))
      })
    case _ => None
  }

  def existential(wildcard: Type.Wildcard): String =
    if (wildcard.bounds.lo.nonEmpty || wildcard.bounds.hi.nonEmpty)
      s"bounded existential wildcard requires unknown witness and bounds analysis: ${wildcard.syntax}"
    else s"existential wildcard requires unknown witness analysis: ${wildcard.syntax}"

}
