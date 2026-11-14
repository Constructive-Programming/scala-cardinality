package cardinality

import org.specs2.execute.PendingUntilFixed.*
import org.specs2.execute.{AsResult, Result}

/** An example the TASTy front end does not meet yet, by its class in the plan's spec inventory
  * (docs/plans/compiled-own-types.md, "Switch-over: spec inventory"). It still runs, reads as
  * pending while it fails, and fails once it passes, so no marker outlives what it waits for.
  */
object Pending {

  /** (a) It pins the old front end's behavior or spelling: a row's name, a signature, a reason. */
  def oldBehavior[R: AsResult](body: => R): Result = waiting("(a) pins old behavior")(body)

  /** (b) It pins a refusal the compiled tree resolves: the new count is to be derived. */
  def resolvedRefusal[R: AsResult](body: => R): Result =
    waiting("(b) pins a refusal the compiled tree resolves")(body)

  /** (c) The TASTy front end refuses where the old one counted: not at parity yet. */
  def knownLoss[R: AsResult](body: => R): Result = waiting("(c) known loss")(body)

  /** Its fixture is one scalac rejects: it comes back with uncompiled support. */
  def rejectedFixture[R: AsResult](body: => R): Result =
    waiting("rejected fixture, uncompiled support")(body)

  /** It reads sources with no compiled classes: it comes back with uncompiled support. */
  def uncompiled[R: AsResult](body: => R): Result =
    waiting("uncompiled support: reads sources with no compiled classes")(body)

  /** Both front ends give a number, and they differ: to derive, a correction or a bug. */
  def countDiffers[R: AsResult](body: => R): Result =
    waiting("count differs: to derive, a correction or a bug")(body)

  private def waiting[R: AsResult](reason: String)(body: => R): Result =
    body.pendingUntilFixed(reason)

}
