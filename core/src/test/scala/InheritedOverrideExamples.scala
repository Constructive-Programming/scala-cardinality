package cardinality.overridefixture

trait Accessor[F[_, _]] {
  def get[X, A](fa: F[X, A]): A
}

object Accessor {

  given tuple: Accessor[Tuple2] with {
    def get[X, A](fa: (X, A)): A = fa._2
  }

}

trait ReverseAccessor[F[_, _]] {
  def reverseGet[X, A](a: A): F[X, A]
}

object ReverseAccessor {

  given either: ReverseAccessor[Either] with {
    def reverseGet[X, A](a: A): Either[X, A] = Right(a)
  }

}

trait Mapper[F[_, _]] {
  def map[X, A, B](fa: F[X, A], f: A => B): F[X, B]
}

object Mapper {

  given tuple: Mapper[Tuple2] with {
    def map[X, A, B](fa: (X, A), f: A => B): (X, B) = (fa._1, f(fa._2))
  }

  given either: Mapper[Either] with {
    def map[X, A, B](fa: Either[X, A], f: A => B): Either[X, B] = fa.map(f)
  }

}

trait AnnotatedBase[A] {
  def get(a: A): A
}

abstract class AnnotatedInstance[A] extends AnnotatedBase[A] {
  def get(@scala.annotation.unused a: A): A = a
}
