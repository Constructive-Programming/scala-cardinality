package cardinality.analysis.inhabitation

import Inhabitation.{Binding, Count, Shape}
import Shape.*

/** Repeated arguments are finite, possibly empty sequences, never a supplied element.
  *
  * The introduction fragment has Nil and Cons. Elimination of an arbitrary-length supplied sequence
  * (including length tests, folds and selection) is not in the finite-context calculus. Reject that
  * fragment unless the result has a unique inhabitant. A sequence of an empty element type has only
  * Nil and can instead be normalized to Unit.
  */
private[cardinality] object RepeatedArguments {

  private val elimination =
    "Repeated-argument sequence elimination is unsupported (length tests, selection and folds)"

  def prepare(bindings: List[Binding], result: Shape): Either[Count, (List[Binding], Shape)] = {
    val normalized = bindings.map(b => b.copy(shape = normalize(b.shape)))
    val target = normalize(result)
    val hasSequence = bindings.exists(b => contains(b.shape)) || contains(result)
    // Leave even singleton goals to the solver, so evidence validation cannot be bypassed.
    if (hasSequence && singleton(target)) Right(normalized -> target)
    else if (normalized.exists(b => contains(b.shape)) || negativeSequence(target))
      Left(Count.Unresolved(List(elimination)))
    else Right(normalized -> target)
  }

  private def normalize(shape: Shape): Shape = shape match {
    case Repeated(element) if empty(element) => Product(Nil)
    case Repeated(element)                   => Repeated(normalize(element))
    case Product(fields)                     => Product(fields.map(normalize))
    case Sum(alternatives)                   => Sum(alternatives.map(normalize))
    case Function(parameters, result) => Function(parameters.map(normalize), normalize(result))
    case Singleton(id, underlying)    => Singleton(id, underlying.map(normalize))
    case Existential(witnesses, body) => Existential(witnesses, normalize(body))
    case other                        => other
  }

  // Only structural emptiness is used as a proof; opaque atoms and functions are not guessed.
  private def empty(shape: Shape): Boolean = shape match {
    case Sum(alternatives) => alternatives.forall(empty)
    case Product(fields)   => fields.exists(empty)
    case _                 => false
  }

  private def singleton(shape: Shape): Boolean = shape match {
    case Product(fields)        => fields.forall(singleton)
    case Sum(List(alternative)) => singleton(alternative)
    case Function(_, result)    => singleton(result)
    case Singleton(_, _)        => true
    case _                      => false
  }

  private def contains(shape: Shape): Boolean = shape match {
    case Repeated(_)                  => true
    case Product(fields)              => fields.exists(contains)
    case Sum(alternatives)            => alternatives.exists(contains)
    case Function(parameters, result) => parameters.exists(contains) || contains(result)
    case Singleton(_, underlying)     => underlying.exists(contains)
    case Existential(_, body)         => contains(body)
    case _                            => false
  }

  // Arrow introduction can supply sequences even when the outer lexical environment has none.
  private def negativeSequence(shape: Shape): Boolean = shape match {
    case Function(parameters, result) =>
      parameters.exists(contains) || negativeSequence(result)
    case Product(fields)          => fields.exists(negativeSequence)
    case Sum(alternatives)        => alternatives.exists(negativeSequence)
    case Repeated(element)        => negativeSequence(element)
    case Singleton(_, underlying) => underlying.exists(negativeSequence)
    case Existential(_, body)     => negativeSequence(body)
    case _                        => false
  }

}
