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
  * A higher-kinded binder is rigid when it carries no subtype or view bound and every hole is
  * plain: no variance, no bound, no context bound, recursively. A variant hole makes `F[A]` a
  * subtype of `F[B]` whenever `A <: B` — subtyping between applications this reading does not model
  * — and a bound gives `F` its bound's members. A context bound is evidence beside the binder, not
  * structure on it, so it never refuses one.
  */
private[cardinality] object Kinds {

  def of(parameter: Type.Param): Kind =
    parameter.tparamClause.values match {
      case Nil   => Kind.Proper
      case holes => Kind.Arrow(holes.map(of))
    }

  /** Why a binder cannot be read as a rigid atom or constructor; None when it can. */
  def refusal(parameter: Type.Param): Option[String] =
    if (parameter.bounds.view.nonEmpty) Some(viewBound(parameter))
    else if (parameter.tparamClause.values.isEmpty) None
    else if (subtypeBounded(parameter) || parameter.tparamClause.values.exists(boundedHole))
      Some(
        s"bounded higher-kinded parameter requires its bound's capabilities: ${Spelling.of(parameter)}"
      )
    else if (parameter.tparamClause.values.exists(variantHole))
      Some(
        s"variant higher-kinded parameter requires subtyping between applications: ${Spelling.of(parameter)}"
      )
    else None

  /** Why a type parameter clause is no Scala 3 program, if it is not: a view bound (removed in
    * Scala 3), or a bound that refers back to its binder through bounds alone (3.9.0: E140). The
    * compiler follows a binder an upper bound names — itself or as a component of a union or an
    * intersection — and every binder a lower bound mentions; inside an upper bound's applied type
    * it stops, so an F-bound (`A <: P[A]`) is legal, and so is `[B, A >: B <: B]`, which refers
    * from A to B twice and never back. Such a clause makes the whole signature invalid, whether or
    * not its binders are read.
    */
  def invalid(clause: List[Type.Param]): List[String] = {
    val names = clause.map(_.name.value).toSet
    val cyclic = Graphs.cyclic(clause.map(p => p.name.value -> referenced(p, names)).toMap)
    clause.flatMap { parameter =>
      val view = Option.when(parameter.bounds.view.nonEmpty)(viewBound(parameter))
      val cycle = Option.when(cyclic(parameter.name.value))(
        s"cyclic type bound (an invalid program): ${Spelling.of(parameter)}"
      )
      view.toList ++ cycle
    }
  }

  private def viewBound(parameter: Type.Param): String =
    s"view bound is not Scala 3 (an invalid program): ${Spelling.of(parameter)}"

  // The clause's binders the compiler reads while checking one binder's bounds for cycles.
  private def referenced(parameter: Type.Param, names: Set[String]): Set[String] = {
    val upper = parameter.bounds.hi.toList.flatMap(components).collect {
      case Type.Name(name) if names(name) => name
    }
    val lower = parameter.bounds.lo.toList.flatMap(mentioned).filter(names)
    (upper ++ lower).toSet
  }

  private def components(tpe: Type): List[Type] = tpe match {
    case Type.Or(left, right)   => components(left) ++ components(right)
    case Type.And(left, right)  => components(left) ++ components(right)
    case Type.With(left, right) => components(left) ++ components(right)
    case Type.ApplyInfix(left, op, right) if Set("|", "&")(op.value) =>
      components(left) ++ components(right)
    case other => List(other)
  }

  // The simple type names a type mentions; a path's member is not a binder.
  private def mentioned(tree: Tree): List[String] = tree match {
    case Type.Name(name)       => List(name)
    case _: Type.Select        => Nil
    case project: Type.Project => mentioned(project.qual)
    case other                 => other.children.flatMap(mentioned)
  }

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

  private def variantHole(hole: Type.Param): Boolean =
    hole.mods.exists(m => m.is[Mod.Covariant] || m.is[Mod.Contravariant]) ||
      hole.tparamClause.values.exists(variantHole)

}
