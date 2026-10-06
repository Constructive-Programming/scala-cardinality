package cardinality.terminalexamples

/** Compiler-checked witnesses; the source-to-report fixture keeps all four binders distinct. */
trait CanModifyP[S, T, A, B] {
  def modify(f: A => B): S => T
  def replace(b: B): S => T = modify(_ => b)
}

final class Modify[S, T, A, B](val modifyFn: (A => B) => S => T)
