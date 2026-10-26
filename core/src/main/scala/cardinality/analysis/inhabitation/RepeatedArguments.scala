package cardinality.analysis.inhabitation

import dev.constructive.eo.generics.lens

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

  private val bindingShapeLens = lens[Binding](_.shape)

  // One bottom-up pass that normalizes empty-element sequences to Unit and proves structural
  // emptiness as it goes; opaque atoms and functions are not guessed.
  private val normalized =
    Shape.cata[(Shape, Boolean)] { (node, kids) =>
      val rebuilt = Shape.rebuild(node, kids)(_._1)
      node match {
        case Repeated(_) if kids.head._2 => Product(Nil) -> false
        case Sum(_)                      => rebuilt -> kids.forall(_._2)
        case Product(_)                  => rebuilt -> kids.exists(_._2)
        case proof: Evidence             => proof -> false
        case _                           => rebuilt -> false
      }
    }

  private val singletonCheck =
    Shape.cata[Boolean] { (node, kids) =>
      node match {
        case Product(_)      => kids.forall(identity)
        case Sum(_)          => kids.size == 1 && kids.head
        case Function(_, _)  => kids.last
        case Singleton(_, _) => true
        case _               => false
      }
    }

  def prepare(bindings: List[Binding], result: Shape): Either[Count, (List[Binding], Shape)] = {
    val prepared = bindings.map(bindingShapeLens.modify(normalize))
    val target = normalize(result)
    val hasSequence = bindings.exists(b => contains(b.shape)) || contains(result)
    // Leave even singleton goals to the solver, so evidence validation cannot be bypassed.
    if (hasSequence && singleton(target)) Right(prepared -> target)
    else if (prepared.exists(b => contains(b.shape)) || negativeSequence(target))
      Left(Count.Unresolved(List(elimination)))
    else Right(prepared -> target)
  }

  private def normalize(shape: Shape): Shape = normalized.get(shape)._1

  private def singleton(shape: Shape): Boolean = singletonCheck.get(shape)

  private def contains(shape: Shape): Boolean =
    Shape.exists(shape)(_.isInstanceOf[Repeated])

  // Arrow introduction can supply sequences even when the outer lexical environment has none.
  private def negativeSequence(shape: Shape): Boolean =
    Shape.exists(shape) {
      case Function(parameters, _) => parameters.exists(contains)
      case _                       => false
    }

}
