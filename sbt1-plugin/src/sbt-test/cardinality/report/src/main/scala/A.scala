import cats.data.NonEmptyList

final case class P(a: Boolean, b: Option[Boolean])

def head[A](xs: NonEmptyList[A]): A = xs.head
