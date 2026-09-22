package example

sealed trait Light
case object Red extends Light
case class Custom(on: Boolean) extends Light

enum Mode {
  case Fast, Slow
}

case class Holder[A](value: A, count: Int)
