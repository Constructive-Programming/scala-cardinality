package cardinality.capacity

import org.specs2.Specification

// A throwable on the stored side (register D26): an open class, whose values are any subclass's,
// so the estimate leaves it to its implementations rather than invent a size — the class every
// other open abstraction has. Fixtures checked with scalac 3.9.0 (`scala.IllegalStateException`
// excepted: the `scala` package does not alias it, which is the point).
class ThrowableSlotsSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    every root-import spelling of a throwable is the open class    $spellings
    a source class named Throwable is that class                   $shadowed
    a throwable the scala package does not alias is not one there  $unaliased
  """

  def spellings =
    rowsOf(
      "a/A.scala" -> ("package a\nfinal case class K3(e: java.lang.Throwable, " +
        "f: scala.RuntimeException, g: java.util.NoSuchElementException)")
    )(("a.K3", Class)) ===
      "open[java.lang.Throwable, scala.RuntimeException, java.util.NoSuchElementException]"

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
