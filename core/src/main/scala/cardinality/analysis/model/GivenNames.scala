package cardinality.analysis.model

import scala.meta.*

/** The name Scala 3 gives an anonymous given: `given_` and the names its type spells. A member of
  * that name is the given, so it implements, overrides or is implemented by a declaration of it
  * like a named one (`given A = a` in a class is `given_A`, the declaration `given given_A: A` of a
  * parent).
  *
  * The type contributes its simple name; an applied type its constructor's and those of its
  * arguments' outermost constructors (`Ord[List[T]]` is `Ord_List`); a tuple its elements' at any
  * depth (`Ord[(Int, (Long, Short))]` is `Ord_Int_Long_Short`); a function `A_to_B`, or `Function`
  * as an argument; an infix operator (`&`, `|`, `A Two B`) its own name before its operands'; a
  * singleton `x_type`; a wildcard its bounds' or nothing. A structural given joins its parents'.
  * Every rule is 3.9.0's, read off `-Xprint:typer` (compiler fixtures `$S/probes/b5-fix2/gname`).
  */
private[cardinality] object GivenNames {

  /** A given definition of any form. */
  type Given = Defn.GivenAlias | Decl.Given | Defn.Given

  /** The synthesized name of an anonymous given whose type the parser kept whole; none for a named
    * one, or for one whose name is not known (`unknown`).
    */
  def of(definition: Given): Option[String] =
    Option.when(definition.name.value.isEmpty && !unknown(definition))(definition match {
      case g: Defn.GivenAlias => "given_" + top(g.decltpe)
      case g: Decl.Given      => "given_" + top(g.decltpe)
      case g: Defn.Given      => "given_" + g.templ.inits.map(init => top(init.tpe)).mkString("_")
    })

  /** Whether an anonymous given's name is not known: scalameta 4.17 reads a bounded wildcard of a
    * given alias's type (`given Ord[? <: String]`, which 3.9.0 names `given_Ord_String`) as a bare
    * `?`, dropping the bound the name is made from. Such a given may be any member `given_…`, and
    * is a member of the name `Unknown`, which no identifier spells unquoted.
    */
  def unknown(definition: Tree): Boolean = definition match {
    case g: Defn.GivenAlias if g.name.value.isEmpty =>
      wildcard(g.decltpe) && Set("<:", ">:").exists(g.pos.text.contains)
    case _ => false
  }

  val Unknown = "<given>"

  /** Whether a member of a name may be the given whose name is not known. */
  def mayBe(definition: Tree, name: String): Boolean =
    unknown(definition) && name.startsWith("given_")

  private def wildcard(tpe: Type): Boolean =
    tpe.collect { case Type.Name("?" | "_") => () }.nonEmpty

  private def top(tpe: Type): String = spelled(tpe, arguments = true)

  // An argument is named by its outermost constructor only, a tuple by all of its elements.
  private def argument(tpe: Type): String = spelled(tpe, arguments = false)

  // A wildcard is a placeholder, which scalameta may make a name (`?`): it is read first.
  private def spelled(tpe: Type, arguments: Boolean): String = tpe match {
    case w: Type.Wildcard       => bounds(w.bounds, arguments)
    case Type.Name("?" | "_")   => ""
    case _: Type.AnonymousParam => ""
    case n: Type.Name           => n.value
    case s: Type.Select         => s.name.value
    case p: Type.Project        => p.name.value
    case s: Type.Singleton      => singleton(s.ref)
    case a: Type.Apply          =>
      val constructor = spelled(a.tpe, arguments)
      if (arguments && a.argClause.values.nonEmpty)
        s"${constructor}_${a.argClause.values.map(argument).mkString("_")}"
      else constructor
    case t: Type.Tuple        => t.args.map(argument).mkString("_")
    case f: Type.FunctionType => function(f.paramClause.values, f.res, arguments)
    case i: Type.ApplyInfix   => infix(i.op.value, i.lhs, i.rhs, arguments)
    case b: Type.Bounds       => bounds(b, arguments)
    case a: Type.Annotate     => spelled(a.tpe, arguments)
    case r: Type.Refine       => r.tpe.fold("")(spelled(_, arguments))
    case l: Type.Lambda       => spelled(l.tpe, arguments)
    case other                => first(other.children, arguments)
  }

  private def function(params: List[Type], result: Type, arguments: Boolean): String =
    if (params.isEmpty) spelled(result, arguments)
    else if (arguments) s"${params.map(argument).mkString("_")}_to_${top(result)}"
    else "Function"

  private def infix(op: String, lhs: Type, rhs: Type, arguments: Boolean): String =
    if (arguments) s"${op}_${argument(lhs)}_${argument(rhs)}" else op

  private def bounds(b: Type.Bounds, arguments: Boolean): String =
    first(b.lo.toList ++ b.hi.toList, arguments)

  // The first name a tree's parts spell, in order, as the compiler's fold over them takes.
  private def first(parts: List[Tree], arguments: Boolean): String =
    parts.iterator
      .collect { case t: Type => spelled(t, arguments) }
      .find(_.nonEmpty)
      .getOrElse("")

  private def singleton(ref: Term.Ref): String = ref match {
    case n: Term.Name   => s"${n.value}_type"
    case s: Term.Select => s"${s.name.value}_type"
    case _              => ""
  }

}
