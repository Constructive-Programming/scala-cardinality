package cardinality.analysis.resolution.opaquefixture

import scala.compiletime.testing.typeChecks

opaque type OpaqueTop[X, A] = A

object OpaqueTop {
  def apply[X, A](value: A): OpaqueTop[X, A] = value
  def value[X, A](wrapped: OpaqueTop[X, A]): A = wrapped

  object Nested {
    def wrap[A](value: A): OpaqueTop[Unit, A] = value
  }

}

def topWrap[X, A](value: A): OpaqueTop[X, A] = value

opaque type Bounded[A] <: A = A

trait Parent[X] { val value: X }

abstract class Child[A] extends Parent[Bounded[A]] {
  def get: A = value
}

class NonShadowingOwner[A](outer: A) {
  opaque type T = A
  def get(a: T): A = a
  def getOuter(a: T): A = outer
}

class InputOnlyOwner[A] {
  opaque type T = A
  def get(a: T): A = a
}

object OpaqueUnrelated {

  val representationVisible: Boolean = typeChecks(
    "val wrapped: cardinality.analysis.resolution.opaquefixture.OpaqueTop[Unit, Int] = 1"
  )

}

object OpaqueOwner {
  opaque type Hidden[A] = A
  type Exported[A] = Hidden[A]

  def wrap[A](value: A): Hidden[A] = value
  def unwrap[A](value: Hidden[A]): A = value
}
