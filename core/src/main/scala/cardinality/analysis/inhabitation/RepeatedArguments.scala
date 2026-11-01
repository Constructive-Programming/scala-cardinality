package cardinality.analysis.inhabitation

import dev.constructive.eo.generics.lens

import Inhabitation.{Binding, Count, Shape}
import Shape.*

/** Sequences — repeated arguments and the stdlib model's finite sequences — before the solver sees
  * them.
  *
  * The introduction fragment has Nil and Cons, in the solver. A sequence of an empty element type
  * has only Nil and is normalized to Unit. A sequence of closed elements is transparent data
  * (`ClosedSpaces`): the solver splits it like any other closed value. A supplied sequence of
  * opaque elements is eliminated by `SequenceElimination` when it is a value of the environment
  * itself (a field of one included); the goal's own function parameters are introduced first, so
  * they are such values too. Anywhere else an opaque sequence would be eliminated — under a sum, as
  * a callable's result, as a parameter of a function the goal or a callable's argument builds — the
  * fragment is refused, unless the goal has one inhabitant whatever the sequence holds.
  */
private[cardinality] object RepeatedArguments {

  /** What the solver is handed: one environment, or one per length of each opaque sequence. */
  enum Prepared {
    case Direct(bindings: List[Binding], target: Shape)
    case Eliminate(rest: List[Binding], sequences: List[Sequence], target: Shape)
  }

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

  def prepare(bindings: List[Binding], result: Shape): Either[Count, Prepared] = {
    val prepared = bindings.map(bindingShapeLens.modify(normalized.get(_)._1))
    val target = normalized.get(result)._1
    val hasSequence = bindings.exists(b => contains(b.shape)) || contains(result)
    // Leave even singleton goals to the solver, so evidence validation cannot be bypassed.
    if (hasSequence && singletonCheck.get(target)) Right(Prepared.Direct(prepared, target))
    else {
      val (introduced, goal) = introduce(prepared, target)
      val split = introduced.map(binding => extract(binding.id, binding.shape))
      val stranded = split.flatMap(_.stranded) ++ unsplit(goal, read = false)
      stranded.headOption match {
        case Some(place) => Left(Count.Unresolved(List(s"$Refused $place")))
        case None        =>
          val rest = introduced.zip(split).map((binding, part) => binding.copy(shape = part.kept))
          split.flatMap(_.sequences).distinctBy(_.id) match {
            case Nil       => Right(Prepared.Direct(rest, goal))
            case sequences => Right(Prepared.Eliminate(rest, sequences, goal))
          }
      }
    }
  }

  /** One environment value split for elimination: the value with Unit in place of each opaque
    * sequence it holds, those sequences as values of their own, and the places of any other.
    */
  final case class Extracted(kept: Shape, sequences: List[Sequence], stranded: List[String])

  /** An opaque sequence the environment holds, keyed by the provenance the solver gives it. */
  final case class Sequence(id: String, shape: Repeated)

  private val Refused = "Repeated-argument sequence elimination is unsupported:"

  private def contains(shape: Shape): Boolean =
    Shape.exists(shape)(Shape.repeatedPrism.getOption(_).nonEmpty)

  // A sequence whose elements hold a type parameter: the one the solver cannot split.
  private def opaque(shape: Shape): Boolean = shape match {
    case sequence: Repeated => !ClosedSpaces.closed(sequence)
    case _                  => false
  }

  // A goal function's parameters are the environment of its body (arrow introduction is unique), so
  // a curried goal is its result under its parameters as values. Done only where a parameter holds
  // an opaque sequence, so every other goal reaches the solver as it is written.
  private def introduce(bindings: List[Binding], target: Shape): (List[Binding], Shape) =
    target match {
      case Function(parameters, result) if parameters.exists(Shape.exists(_)(opaque)) =>
        val named =
          parameters.zipWithIndex.map((shape, i) => Binding(s"<goal-${bindings.size}#$i>", shape))
        introduce(bindings ++ named, result)
      case _ => (bindings, target)
    }

  // A value's opaque sequences that are values of the environment themselves — the value, or a
  // field of it — and the places any other opaque sequence sits in. A singleton stays the alias it
  // is: `q: p.type` is `p` (two aliases are one value), so its sequences are `p`'s own.
  private def extract(id: String, shape: Shape): Extracted =
    shape match {
      case sequence: Repeated if opaque(sequence) =>
        Extracted(Product(Nil), List(Sequence(id, sequence)), Nil)
      case Singleton(reference, Some(underlying)) =>
        val inner = extract(reference, underlying)
        inner.copy(kept = Singleton(reference, Some(inner.kept)))
      case Product(fields) =>
        val parts = fields.zipWithIndex.map((field, i) => extract(s"$id.$i", field))
        Extracted(
          Product(parts.map(_.kept)),
          parts.flatMap(_.sequences),
          parts.flatMap(_.stranded)
        )
      case other => Extracted(other, Nil, unsplit(other, read = true))
    }

  // Where an opaque sequence sits that no rule eliminates — one the program would read anywhere
  // but at the top of the environment, which `extract` took — named at the boundary that makes it
  // read: the alternatives of a supplied sum, the result of a supplied callable, the parameters of
  // a function the program builds, an existential or a proof. `read`: whether the program reads
  // this shape (it is supplied) rather than builds it; a sequence it builds is introduced by Nil
  // and Cons, its elements built the same way.
  private def unsplit(shape: Shape, read: Boolean): List[String] =
    shape match {
      case Sum(alternatives) =>
        crossing(read, alternatives, "a sequence inside a supplied sum") ++
          alternatives.flatMap(unsplit(_, read))
      case Function(parameters, result) =>
        crossing(!read, parameters, "a sequence parameter of a built function") ++
          crossing(read, List(result), "a sequence a supplied callable returns") ++
          parameters.flatMap(unsplit(_, !read)) ++ unsplit(result, read)
      case _: Existential | _: Evidence =>
        Option
          .when(Shape.exists(shape)(opaque))("a sequence under an existential or a proof")
          .toList
      case other => Shape.children(other).flatMap(unsplit(_, read))
    }

  // The place, when the program reads parts holding an opaque sequence below no other boundary.
  private def crossing(read: Boolean, parts: List[Shape], place: String): List[String] =
    Option.when(read && parts.exists(exposed))(place).toList

  private def exposed(shape: Shape): Boolean = shape match {
    case sequence: Repeated if opaque(sequence)              => true
    case _: Sum | _: Function | _: Existential | _: Evidence => false
    case other => Shape.children(other).exists(exposed)
  }

}
