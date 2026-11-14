package cardinality.capacity

/** Why the stored-value fixed point (`Fixpoint`, read by `compiled.Stored`) gives a definition on a
  * cycle no count: a lazy cycle the ν completion did not finish (its μ value counts the finite
  * values only), or a recursion through a type member read off a value (`x: q.X`), which is read
  * over an earlier solve and never iterated.
  */
private[cardinality] enum Blocked(val reason: String) {
  case Lazy extends Blocked("lazy cycle not completed")
  case Unseen extends Blocked("recursion through a type member of a value")

  def about(name: String): String = s"$reason: $name"
}
