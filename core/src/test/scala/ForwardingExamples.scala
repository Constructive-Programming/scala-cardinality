package cardinality

/** Reduced compiled signatures, not vendored optic implementations. No optic laws are assumed. */
object ForwardingExamples {
  case class PickFold[S, A](pick: S => Option[A])
  case class MendTearPrism[S, T, A, B](tear: S => Either[T, A], mend: B => T)
  case class PickMendPrism[S, A, B](pick: S => Option[A], mend: B => S)
  case class Optional[S, T, A, B](getOrModify: S => Either[T, A], reverseGet: (S, B) => T)

  def forward[S, A](input: PickFold[S, A]): PickFold[S, A] =
    PickFold(s => input.pick(s).fold[Option[A]](None)(Some(_)))

  @scala.annotation.nowarn("msg=unused explicit parameter")
  def discard[S, A](input: PickFold[S, A]): PickFold[S, A] =
    PickFold(_ => None)

  def forwardPrism[S, T, A, B](input: MendTearPrism[S, T, A, B]): MendTearPrism[S, T, A, B] =
    MendTearPrism(s => input.tear(s).fold(Left(_), Right(_)), b => input.mend(b))

  def reuseLeft[S, T, A, B](input: Optional[S, T, A, B]): (S, B) => T =
    (s, b) => input.getOrModify(s).fold(identity, _ => input.reverseGet(s, b))

}
