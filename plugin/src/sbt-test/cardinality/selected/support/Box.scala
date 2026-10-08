package example

case class Box[A](value: A)
def supportOnly[A](a: A): A = a
