package cardinality.analysis.expansion

import scala.meta.*

/** A `val` or `var`, defined or declared: its patterns, its declared type, and its right-hand side
  * (none for a declaration). Under the model a `var` holds the value it was given (D26), so both
  * read alike.
  */
private[expansion] object Valued {

  def unapply(stat: Stat): Option[(List[Pat], Option[Type], Option[Term])] = stat match {
    case v: Defn.Val => Some((v.pats, v.decltpe, Some(v.rhs)))
    case v: Defn.Var => Some((v.pats, v.decltpe, Some(v.body)))
    case v: Decl.Val => Some((v.pats, Some(v.decltpe), None))
    case v: Decl.Var => Some((v.pats, Some(v.decltpe), None))
    case _           => None
  }

}
