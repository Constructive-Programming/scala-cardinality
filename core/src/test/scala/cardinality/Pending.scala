package cardinality

import org.specs2.execute.PendingUntilFixed.*
import org.specs2.execute.{AsResult, Result}

/** An example the TASTy front end does not meet yet, by its class in the plan's spec inventory
  * (docs/plans/compiled-own-types.md, "Switch-over: spec inventory"). It still runs, reads as
  * pending while it fails, and fails once it passes, so no marker outlives what it waits for.
  */
object Pending {

  /** (c) The TASTy front end refuses where the old one counted: not at parity yet. */
  def knownLoss[R: AsResult](body: => R): Result = waiting("(c) known loss")(body)

  /** Its fixture is one scalac rejects: it comes back with uncompiled support. */
  def rejectedFixture[R: AsResult](body: => R): Result =
    waiting("rejected fixture, uncompiled support")(body)

  /** It reads sources with no compiled classes: it comes back with uncompiled support. */
  def uncompiled[R: AsResult](body: => R): Result =
    waiting("uncompiled support: reads sources with no compiled classes")(body)

  private def waiting[R: AsResult](reason: String)(body: => R): Result =
    body.pendingUntilFixed(reason)

}
