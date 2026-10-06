package cardinality

import scala.meta.*

/** Parsed declarations have object identity even when two inputs use identical source offsets. */
private[cardinality] object DeclarationIdentity {
  def same(left: Tree, right: Tree): Boolean = left eq right
}
