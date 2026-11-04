package cardinality.analysis.capability

import scala.meta.Tree

import cardinality.analysis.MethodAnalysis.Resolved
import cardinality.analysis.Resolver
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.model.MemberSyntax

import Inhabitation.{Count, Shape}

/** A deferred (abstract `inline`) method or value of a receiver or a capability record. Only an
  * inline expansion may call one — scalac rejects a call from any other body ("Deferred inline
  * method … cannot be invoked") — and each expansion calls the override of the receiver's static
  * type. So a body that is not inline has no term of it, but a receiver, or a record built as a
  * goal, exists only if some instance implements it. A type built from nothing constrains neither
  * and is dropped; any other might empty the environment or the goal with the rest (`inline def
  * neg: A => Nothing` beside an `a: A`), which is not decided here, so it is refused.
  */
private[cardinality] object DeferredInline {

  /** Why a deferred inline member of type `shape`, seen from `useSite`, is refused; none: dropped.
    */
  def refusal(
      shape: Shape,
      name: String,
      useSite: Option[Tree],
      resolver: Resolver
  ): Option[String] =
    if (useSite.exists(MemberSyntax.inlined))
      Some(s"deferred inline member resolved at each expansion: $name")
    else
      Option.unless(inhabited(shape, resolver))(
        s"deferred inline member whose type may be empty: $name"
      )

  /** As `refusal`, for a type that may not have been read. */
  def read(shape: Resolved, name: String, useSite: Tree, resolver: Resolver): Option[String] =
    shape.fold(Some(_), refusal(_, name, Some(useSite), resolver))

  // Built from no values, its atoms rigid: inhabited whatever the atoms are.
  private def inhabited(shape: Shape, resolver: Resolver): Boolean =
    Inhabitation.count(
      Nil,
      shape,
      resolver.limits.maxStates,
      resolver.inspect,
      resolver.solverDepth
    ) match {
      case Count.Finite(value)                    => value > 0
      case Count.Symbolic(_) | Count.Countable    => true
      case Count.Bounded(_) | Count.Unresolved(_) => false
    }

}
