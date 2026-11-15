package cardinality.compiled

import cardinality.capacity.{Blocked, NothingSize, Size, Store}
import tastyquery.Symbols.ClassSymbol

import Stored.Held

/** The system of equations the stored side solves where a definition reaches itself (`Stored`,
  * `Fixpoint`): its variables, one per class of the module with its type parameters bound, and the
  * values they hold so far.
  */
private[compiled] object Equations {

  // A class of the module with its type parameters bound: a definition of a system.
  final case class Instance(cls: ClassSymbol, arguments: List[Held])

  // A system's values so far, and the classes the fixed point left blocked.
  final case class Values(
      sizes: Map[Instance, Size] = Map.empty,
      blocked: Map[Instance, Blocked] = Map.empty
  )

  given Store[Instance, Values] with {

    extension (values: Values) {
      def value(name: Instance): Size = values.sizes.getOrElse(name, NothingSize)

      def withValues(more: Iterable[(Instance, Size)]): Values =
        values.copy(sizes = values.sizes ++ more)

      def withBlocked(more: Iterable[(Instance, Blocked)]): Values =
        values.copy(blocked = values.blocked ++ more)

    }

  }

  // How many argument lists a system reads one class with: past them it would not end.
  val Instantiations = 8

  // A system's classes in an order that does not depend on the run.
  def ordered(keys: Iterable[Instance]): List[Instance] =
    keys.toList.sortBy(k => (k.cls.displayFullName, k.arguments.toString))

  // A class of the module inside a system: its value in `values`, a variable of the equation.
  def variable(c: ClassSymbol, arguments: List[Held])(values: Values): Either[String, Held] =
    if (arguments.exists(_.refs.nonEmpty))
      Left(s"a class a system reads through a type argument: ${c.displayFullName}")
    else {
      val key = Instance(c, arguments)
      Right(Held(values.value(key), refs = Set(key)))
    }

  // A reading inside a system after `Stored.discovered` has read every equation over empty values:
  // what a reading may refuse does not depend on the values, and `Stored.system` reads every
  // equation again over the solved ones, so a refusal cannot first appear here.
  def settled[A, B](read: Either[String, A], none: B)(f: A => B): B =
    read.fold(_ => none, f)

}
