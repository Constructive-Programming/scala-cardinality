package tastyfixtures

// Compiled declarations TastyStubsSpec reads back from this file's TASTy. Each one is a shape the
// stubs must spell faithfully; the spec compares counts read through the stubs with counts read
// through this source.

/** An abstract endomorphism: a capability a target may be handed. */
trait Endo[A] {
  def run(a: A): A
}

/** A concrete identity: an implementation, not a freely supplied callable. */
object Concrete {
  def identity[A](a: A): A = a
  val alias: Boolean = true
}

final case class Pair(left: Boolean, right: Boolean = false)

sealed trait Light

object Light {
  case object Red extends Light
  case object Green extends Light
  final case class Blink(on: Boolean) extends Light
}

enum Suit {
  case Hearts, Spades
}

opaque type Flag = Boolean

object Flag {
  def of(b: Boolean): Flag = b
  extension (f: Flag) def value: Boolean = f
}

trait Show[A] {
  def show(a: A): Boolean
}

object Show {
  given Show[Boolean] = new Show[Boolean] { def show(a: Boolean): Boolean = a }
}

object Inlines {
  inline def pick[A](x: A, @scala.annotation.unused y: A): A = x
}

abstract class Base(val seed: Int)
final class Built() extends Base(0)
