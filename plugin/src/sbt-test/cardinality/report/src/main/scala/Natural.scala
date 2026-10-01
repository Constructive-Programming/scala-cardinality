package example

// A recursive family in a file of its own: a report reads the project's sources as one set, so
// `Light.scala`'s consumer resolves `Nat` here instead of counting it as an unknown name.
sealed trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat
