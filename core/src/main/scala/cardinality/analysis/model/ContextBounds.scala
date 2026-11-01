package cardinality.analysis.model

import scala.meta.*

/** A context bound is exactly a contextual parameter (hkt R4): `[A: C₁: C₂]` reads as `[A](using
  * C₁[A], C₂[A])`, `[G[_]: C]` as `(using C[G])`, and SIP-64's `A: C as c` names its evidence `c`.
  * The binder stays what it was — an atom or a rigid constructor — and the evidence is one more
  * value in the environment, reached through implicit search like any anonymous `using` parameter
  * (`ContextualEvidence`), or by its name when it has one. A class's bound is also a constructor
  * parameter and a field of its instances.
  */
private[cardinality] object ContextBounds {

  def evidence(parameters: List[Type.Param]): List[Term.Param] =
    parameters.flatMap { parameter =>
      val bound = Type.Name(parameter.name.value)
      parameter.bounds.context.map {
        case named: Type.BoundsAlias => using(Term.Name(named.name.value), named.bounds, bound)
        case other                   => using(Name.Anonymous(), other, bound)
      }
    }

  /** Where a frame parameter's type is written: a bound's evidence, built in no clause, where its
    * binder is declared — inside its class, like the `using` parameter it stands for.
    */
  def written(param: Term.Param, tpe: Type, binders: List[Type.Param]): Tree =
    tpe match {
      case evidence: Type.Apply if param.parent.isEmpty =>
        evidence.argClause.values
          .collectFirst { case Type.Name(binder) => binder }
          .flatMap(binder => binders.find(_.name.value == binder))
          .getOrElse(tpe)
      case _ => tpe
    }

  private def using(name: Name, evidence: Type, bound: Type.Name): Term.Param =
    Term.Param(
      List(Mod.Using()),
      name,
      Some(Type.Apply(evidence, Type.ArgClause(List(bound)))),
      None
    )

}
