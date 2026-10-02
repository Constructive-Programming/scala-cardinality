package example

sealed trait Light
case object Red extends Light
case class Custom(on: Boolean) extends Light

enum Mode {
  case Fast, Slow
}

case class Holder[A](value: A, count: Int)

// What the branch gained since PR #10 was cut: recursion is solved as a fixed point, a lazy hole
// counts its infinite values, and a function space over a countable type reaches the ε₀ tier.
// `Nat` lives in `Natural.scala`, so this consumer also reads across files.
case class Cursor(at: Nat, open: Boolean)

case class Timeline(head: Boolean, tail: => Timeline)

case class Reducer(run: String => String)
