package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// A throwable on the stored side (register D26): an open class, whose values are any subclass's,
// so the estimate leaves it to its implementations rather than invent a size — the class every
// other open abstraction has. Fixtures checked with scalac 3.9.0 (`scala.IllegalStateException`
// excepted: the `scala` package does not alias it, which is the point).
class ThrowableSlotsSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    A throwable slot is open to implementations, never a size     $openSlots
    every root-import spelling of a throwable is the open class    $spellings
    a constructor over a throwable slot says why it has no number  $constructorSlot
    a source class named Throwable is that class                   $shadowed
    a throwable the scala package does not alias is not one there  $unaliased
  """

  private val failing: String =
    "package f\nenum Failure { case Missing(at: Int); case Failed(cause: Throwable) }\n" +
      "final class FailureException(val failure: Failure) extends RuntimeException(\"f\")"

  // Each read `unresolved[…]` before.
  def openSlots = {
    val found = rowsOf(
      "a/A.scala" -> ("package a\nfinal case class K1(e: Throwable)\n" +
        "final case class K2(e: Exception, i: IllegalArgumentException)\n" +
        "type K4 = Either[Throwable, Int]"),
      "f/F.scala" -> failing
    )
    (found(("a.K1", Class)) === "open[Throwable]")
      .and(found(("a.K2", Class)) === "open[Exception, IllegalArgumentException]")
      .and(found(("a.K4", Alias)) === "open[Throwable]")
      .and(found(("f.Failure", Enum)) === "open[Throwable]")
      .and(found(("f.FailureException", Class)) === "open[Throwable]")
  }

  def spellings =
    rowsOf(
      "a/A.scala" -> ("package a\nfinal case class K3(e: java.lang.Throwable, " +
        "f: scala.RuntimeException, g: java.util.NoSuchElementException)")
    )(("a.K3", Class)) ===
      "open[java.lang.Throwable, scala.RuntimeException, java.util.NoSuchElementException]"

  def constructorSlot =
    constructor("f.FailureException.<init>", "F.scala" -> failing) ===
      Count.Unresolved(List("open to implementations: Throwable"))

  // One Boolean field: 2.
  def shadowed =
    rowsOf(
      "a/A.scala" -> "package a\nfinal class Throwable(val b: Boolean)\nfinal case class K5(t: Throwable)"
    )(("a.K5", Class)) === "2"

  def unaliased =
    rowsOf("a/A.scala" -> "package a\nfinal case class K6(e: scala.IllegalStateException)")(
      ("a.K6", Class)
    ) === "unresolved[scala.IllegalStateException]"

}
