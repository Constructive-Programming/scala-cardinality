package cardinality.analysis

import scala.meta.*

import cardinality.analysis.model.ContextBounds
import cardinality.analysis.resolution.InferredResults
import cardinality.types.Spelling

/** What a secondary constructor's row measures. Its self-invocation calls another constructor of
  * the class, and every chain of them ends in the primary one ([30] *Constructor Definitions*): an
  * implementation chooses the primary constructor's arguments over the secondary's parameters, so
  * the row measures their tuple, never a value of the class (an open class's overridable members
  * are no choice a constructor makes).
  */
private[cardinality] object SecondaryConstructors {

  /** `this(ps): C[T]`, each clause spelled on its own. */
  def signature(d: Defn.Class, c: Ctor.Secondary): String =
    "this" + c.paramClauses.map(Spelling.of).mkString + s": ${Spelling.of(classType(d))}"

  /** The arguments a class's primary constructor takes, as one type: their tuple (the type itself
    * for one, `Unit` for none), with the evidence of its bounds last, a by-name parameter's value
    * and a repeated parameter's sequence.
    */
  def primaryArguments(d: Defn.Class): Type =
    arguments(
      d.ctor.paramClauses.toList.flatMap(_.values) ++ ContextBounds.evidence(d.tparamClause.values)
    )

  /** The arguments a list of parameters takes, as one type (`primaryArguments`). */
  def arguments(params: List[Term.Param]): Type =
    params.flatMap(_.decltpe).map {
      case b: Type.ByName   => b.tpe
      case r: Type.Repeated =>
        Type.Apply(
          InferredResults.rooted(List("scala", "collection", "immutable", "Seq")),
          Type.ArgClause(List(r.tpe))
        )
      case other => other
    } match {
      case Nil       => InferredResults.rooted(List("scala", "Unit"))
      case List(one) => one
      case many      => Type.Tuple(many)
    }

  private def classType(d: Defn.Class): Type = d.tparamClause.values match {
    case Nil     => d.name
    case tparams => Type.Apply(d.name, Type.ArgClause(tparams.map(p => Type.Name(p.name.value))))
  }

}
