package cardinality.analysis.capability

import scala.meta.Tree

import cardinality.analysis.MethodAnalysis.Resolved
import cardinality.analysis.Resolver
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.model.MemberSyntax

import Inhabitation.Shape

/** A deferred (abstract `inline`) method or value of a receiver or a capability record. Only an
  * inline expansion may call one — scalac rejects a call from any other body ("Deferred inline
  * method … cannot be invoked") — and each expansion calls the override of the receiver's static
  * type. An inline body counts it as it would an abstract `def` (`supplied`): a capability. A body
  * that is not inline has no term of it, but a receiver, or a record built as a goal, exists only
  * if some instance implements it. A type built from nothing constrains neither and is dropped; any
  * other might empty the environment or the goal with the rest (`inline def neg: A => Nothing`
  * beside an `a: A`), which is not decided here, so it is refused.
  */
private[cardinality] object DeferredInline {

  /** Whether a body read at `useSite` may call a deferred inline member: an inline one may. */
  def supplied(useSite: Option[Tree]): Boolean = useSite.exists(MemberSyntax.inlined)

  /** Why a deferred inline member of type `shape`, which `useSite` may not call, is refused; none:
    * dropped.
    */
  def refusal(shape: Shape, name: String, resolver: Resolver): Option[String] =
    Option.unless(inhabited(shape, resolver))(
      s"deferred inline member whose type may be empty: $name"
    )

  /** As `refusal`, for a type that may not have been read. */
  def read(shape: Resolved, name: String, resolver: Resolver): Option[String] =
    shape.fold(Some(_), refusal(_, name, resolver))

  // Built from no values, its atoms rigid: inhabited whatever the atoms are.
  private def inhabited(shape: Shape, resolver: Resolver): Boolean =
    Inhabitation
      .count(
        Nil,
        shape,
        resolver.limits.maxStates,
        resolver.inspect,
        resolver.solverDepth
      )
      .atLeast(1)

}
