package cardinality.types

import scala.meta.*

/** A type parameter's kind, read from its declaration: `*` without an own parameter clause,
  * `(k₁, …, kₙ) → *` with one (`F[_, _]`, `C[_[_]]`, `CC[X]`).
  */
private[cardinality] enum Kind {
  case Proper
  case Arrow(override val holes: List[Kind])

  /** The kinds of the arguments a constructor of this kind takes; none for a proper type. */
  def holes: List[Kind] = Nil

  def arity: Int = holes.size

  def render: String = this match {
    case Proper       => "*"
    case Arrow(holes) => holes.map(_.render).mkString("(", ", ", ") → *")
  }

}

/** Which binders the model reads as rigid, and why the others are refused.
  *
  * A higher-kinded binder is rigid when it carries no subtype bound and every hole is unbounded,
  * recursively: a bound gives `F` its bound's members. A context bound is evidence beside the
  * binder, not structure on it, so it never refuses one.
  */
private[cardinality] object Kinds {

  def of(parameter: Type.Param): Kind =
    parameter.tparamClause.values match {
      case Nil   => Kind.Proper
      case holes => Kind.Arrow(holes.map(of))
    }

  /** Why a binder cannot be read as a rigid atom or constructor; None when it can. */
  def refusal(parameter: Type.Param): Option[String] =
    Option.when(
      parameter.tparamClause.values.nonEmpty &&
        (subtypeBounded(parameter) || parameter.tparamClause.values.exists(boundedHole))
    )(
      s"bounded higher-kinded parameter requires its bound's capabilities: ${Spelling.of(parameter)}"
    )

  /** Why a type parameter clause is no Scala 3 program: never, as read here. */
  def invalid(clause: List[Type.Param]): List[String] = Nil

  /** A declared parameter a source type can bind an argument to unchanged: no bound of any kind,
    * and a higher-kinded one rigid. A context bound adds a hidden evidence field to the
    * representation, a subtype bound an obligation the reading does not check.
    */
  def plain(parameter: Type.Param): Boolean =
    !subtypeBounded(parameter) && parameter.bounds.context.isEmpty && refusal(parameter).isEmpty

  def subtypeBounded(parameter: Type.Param): Boolean =
    parameter.bounds.lo.nonEmpty || parameter.bounds.hi.nonEmpty

  private def boundedHole(hole: Type.Param): Boolean =
    constrained(hole) || hole.tparamClause.values.exists(boundedHole)

  // A parameter that carries a bound of any form: subtype, context or view.
  private def constrained(parameter: Type.Param): Boolean =
    subtypeBounded(parameter) || (parameter.bounds.context ++ parameter.bounds.view).nonEmpty

}
