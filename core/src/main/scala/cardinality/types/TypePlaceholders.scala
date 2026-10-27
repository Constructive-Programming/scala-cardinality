package cardinality.types

import scala.meta.*

/** Constructor holes are bound by scalameta's AnonymousLambda, not existential wildcards. Normalize
  * only that binder; beta application remains the resolver's responsibility.
  */
private[cardinality] object TypePlaceholders {
  private given Dialect = dialects.Scala3

  private val anonymousLambdaPrism = TypeApplications.subPrism[Type, Type.AnonymousLambda]

  final private class HoleState(val used: Set[String]) {
    var names: List[String] = Nil
    var failure: Option[String] = None

    def nextHole(hole: Type.AnonymousParam): Type = {
      if (hole.variant.nonEmpty)
        failure = Some("variant constructor placeholder is unsupported")
      val (name, _) =
        cardinality.analysis.inhabitation.Inhabitation.Shape
          .freshName("cardinalityHole", names.size)(n => used(n) || names.contains(n))
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
          if (
            dev.constructive.eo.optics.Plated
              .universe(other)(using TypeSubstitution.localPlate)
              .exists(_.isInstanceOf[Type.AnonymousParam])
          )
            state.failure = Some("constructor placeholder body requires unsupported hole traversal")
          other
      }
    }

  def lambda(tpe: Type): Option[Either[String, Type.Lambda]] =
    anonymousLambdaPrism.getOption(tpe).map { anonymous =>
      val used = TypeSubstitution.typeNames(anonymous).toSet
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
