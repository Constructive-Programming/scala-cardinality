package cardinality.analysis.resolution

/** Compiled witnesses: matching aliases and a genuine equal-shaped nominal overload. */
private[cardinality] object AliasOverrideExamples {
  type Id[A] = A
  type Pair[A] = (A, A)

  trait Base {
    def get[A](a: Id[A]): Id[A]
  }

  object Renamed extends Base {
    def get[B](b: B): B = b
  }

  trait Capturing[T] {
    type Captured = T
    def get[T](a: Captured, b: T): Captured
  }

  abstract class Shadowed[A] extends Capturing[A] {
    def get[B](a: A, @scala.annotation.unused b: B): A = a
  }

  object Library {
    case class Token[A](value: A)
    type Alias[A] = Token[A]
  }

  trait Qualified[A] {
    def get(a: Library.Alias[A]): A
  }

  abstract class Matching[A] extends Qualified[A] {
    def get(a: Library.Token[A]): A = a.value
  }

  abstract class Overloaded[A] extends Qualified[A] {
    case class Token[B](value: B)
    def get(a: Token[A]): A = a.value
  }

}
