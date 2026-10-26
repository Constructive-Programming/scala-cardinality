package cardinality.types

import scala.meta.*

import dev.constructive.eo.optics.Prism

/** Constructor holes are bound by scalameta's AnonymousLambda, not existential wildcards. Normalize
  * only that binder; beta application remains the resolver's responsibility.
  */
private[cardinality] object TypePlaceholders {
  private given Dialect = dialects.Scala3

  private val anonymousLambdaPrism = Prism.optional[Type, Type.AnonymousLambda](
    {
      case anonymous: Type.AnonymousLambda => Some(anonymous)
      case _                               => None
    },
    identity
  )

  final private class HoleState(val used: Set[String]) {
    var names: List[String] = Nil
    var failure: Option[String] = None

    def nextHole(hole: Type.AnonymousParam): Type = {
      if (hole.variant.nonEmpty)
        failure = Some("variant constructor placeholder is unsupported")
      val name = Iterator
        .from(names.size)
        .map(i => s"cardinalityHole$i")
        .find(n => !used(n) && !names.contains(n))
        .get
      names = names :+ name
      Type.Name(name)
    }

  }

  private val rewrittenHoles: Type => HoleState => Type =
    TypeSubstitution.localCata[HoleState => Type] { (node, kids, rebuild) => state =>
      node match {
        case nested: Type.AnonymousLambda  => nested
        case nested: Type.Lambda           => nested
        case hole: Type.AnonymousParam     => state.nextHole(hole)
        case _: Type.Apply | _: Type.Tuple =>
          rebuild(kids.map(_(state)))
        case other =>
          if (TypeSubstitution.localUniverse(other).exists(_.isInstanceOf[Type.AnonymousParam]))
            state.failure = Some("constructor placeholder body requires unsupported hole traversal")
          other
      }
    }

  def lambda(tpe: Type): Option[Either[String, Type.Lambda]] =
    anonymousLambdaPrism.getOption(tpe).map { anonymous =>
      val used = TypeSubstitution.universe(anonymous).collect { case n: Type.Name => n.value }.toSet
      val state = new HoleState(used)
      val rewritten = rewrittenHoles(anonymous.tpe)(state)
      state.failure match {
        case Some(reason)                => Left(reason)
        case None if state.names.isEmpty => Left("constructor placeholder has no local holes")
        case None                        =>
          val params = state.names.map(name =>
            Type.Param(
              Nil,
              Type.Name(name),
              Type.ParamClause(Nil),
              Type.Bounds(None, None, Nil, Nil)
            )
          )
          Right(Type.Lambda(Type.ParamClause(params), rewritten))
      }
    }

  def existential(wildcard: Type.Wildcard): String =
    if (wildcard.bounds.lo.nonEmpty || wildcard.bounds.hi.nonEmpty)
      s"bounded existential wildcard requires unknown witness and bounds analysis: ${wildcard.syntax}"
    else s"existential wildcard requires unknown witness analysis: ${wildcard.syntax}"

}
