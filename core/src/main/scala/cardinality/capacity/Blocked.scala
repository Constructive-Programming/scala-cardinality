package cardinality.capacity

/** Why the stored-value fixed point (`Fixpoint`) gives a definition on a cycle no count: a lazy
  * cycle the ν completion did not finish, or a recursion through a type member read off a value.
  */
private[cardinality] enum Blocked(val reason: String) {
  case Lazy extends Blocked("lazy cycle not completed")
  case Unseen extends Blocked("recursion through a type member of a value")

  def about(name: String): String = s"$reason: $name"
}
