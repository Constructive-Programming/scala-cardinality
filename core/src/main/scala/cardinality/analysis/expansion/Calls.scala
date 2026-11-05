package cardinality.analysis.expansion

import scala.annotation.tailrec
import scala.meta.*

/** `f[T](a)(using b)` flattened: the head, its type arguments, its argument terms, and each
  * argument clause flagged when it is written `using`.
  */
final private[expansion] case class Chain(
    head: Term,
    targs: List[Type],
    args: List[Term],
    clauses: List[Boolean]
)

private[expansion] object Chain {

  def of(term: Term): Chain = flatten(term, Chain(term, Nil, Nil, Nil))

  @tailrec
  private def flatten(term: Term, found: Chain): Chain = term match {
    case a: Term.Apply =>
      flatten(
        a.fun,
        found.copy(
          args = a.argClause.values ++ found.args,
          clauses = a.argClause.mod.exists(_.is[Mod.Using]) :: found.clauses
        )
      )
    case a: Term.ApplyType =>
      flatten(a.fun, found.copy(targs = a.targClause.values ++ found.targs))
    case head => found.copy(head = head)
  }

}

/** How written argument clauses meet a callee's declared ones, each flagged contextual (`true`). */
private[expansion] object Clauses {

  /** Whether a contextual clause is left to implicit search after the written clauses fill the
    * others: one the written clauses skip, or one left over once they run out (a call applied to
    * fewer clauses is eta-expanded, its contextual clauses filled where it is written — fixture
    * review-A6 h7).
    */
  @tailrec
  def searched(declared: List[Boolean], written: List[Boolean]): Boolean =
    (declared, written) match {
      case (rest, Nil)                  => rest.contains(true)
      case (Nil, _)                     => false
      case (true :: rest, true :: more) => searched(rest, more)
      case (true :: _, false :: _)      => true
      case (false :: rest, _ :: more)   => searched(rest, more)
    }

  /** The written clauses the declared ones leave over, which apply the callee's result; none when
    * the callee is partly applied.
    */
  def rest(declared: List[Boolean], written: List[Boolean]): List[Boolean] =
    remaining(declared, written).getOrElse(Nil)

  /** The same, None when a clause that is not contextual is left unwritten (a contextual clause the
    * written ones skip is filled by search).
    */
  @tailrec
  def remaining(declared: List[Boolean], written: List[Boolean]): Option[List[Boolean]] =
    (declared, written) match {
      case (Nil, more)                => Some(more)
      case (true :: left, Nil)        => remaining(left, Nil)
      case (false :: _, Nil)          => None
      case (true :: left, false :: _) => remaining(left, written)
      case (_ :: left, _ :: more)     => remaining(left, more)
    }

}
