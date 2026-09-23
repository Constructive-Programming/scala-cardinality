package optics

sealed trait Optic
case object Id extends Optic
case class Composed[A](left: A, right: A) extends Optic
