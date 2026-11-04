package cardinality.capacity

import scala.meta.*

import cardinality.types.Trees

/** A definition's name as a reason spells it: its package and enclosing definitions, dotted. */
private[capacity] object Qualified {

  def of(member: Member): String =
    (Trees.ancestors(member).reverse.flatMap {
      case p: Pkg                                 => List(p.ref.syntax)
      case p: Pkg.Object                          => List(p.name.value)
      case d: Defn if Walk.templateOf(d).nonEmpty => List(Walk.named(d).value)
      case _                                      => Nil
    } :+ member.name.value).mkString(".")

}
