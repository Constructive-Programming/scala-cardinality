package cardinality.analysis.resolution

import scala.meta.{Type, TypeCase}

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.SourceBindings
import cardinality.types.Universe

/** Stuck tuple projections (members MT3).
  *
  * Scala reduces `T match { case (f, s) => f }` only once `T` is known to be a pair, so on an
  * abstract `T` it stays stuck: a value of it comes only from a value of the same stuck type, and
  * nothing destructures a `T`. Read as the atom `proj:n:i:(α)`, its terms are an atom's rigid
  * terms, as distinct as those because one instantiation `α = (β1, …, βn)` reduces every projection
  * of the signature to an independent variable at once. That needs one arity per scrutinee
  * (`Fst[T]` beside a three-way `Fst3[T]` has no common instantiation, so every implementation is
  * vacuously equal), no bound on α, and no evidence about α, which `substituteCo[Fst]` rewrites
  * inside the stuck type where the atom hides α. Otherwise the signature is refused.
  */
private[cardinality] object StuckProjections {

  /** The atom of the `index`-th (1-based) component of an `arity`-tuple scrutinee. */
  object Projection {

    private val Spelled = """proj:(\d+):(\d+):\((.*)\)""".r

    def apply(arity: Int, index: Int, scrutinee: String): String =
      s"proj:$arity:$index:($scrutinee)"

    /** (arity, index, scrutinee) of a projection atom. */
    def unapply(atom: String): Option[(Int, Int, String)] = atom match {
      case Spelled(arity, index, scrutinee) => Some((arity.toInt, index.toInt, scrutinee))
      case _                                => None
    }

  }

  /** The component a single-case projection takes, as (arity, 1-based index): one tuple pattern of
    * distinct binders whose body is one of them.
    */
  def projection(cases: List[TypeCase]): Option[(Int, Int)] = cases match {
    case List(single) =>
      (single.pat, single.body) match {
        case (Type.Tuple(parts), Type.Name(body)) =>
          val binders = parts.collect {
            case Type.Name(name) if name.headOption.exists(_.isLower) => name
          }
          Option
            .when(binders.size == parts.size && binders.distinct.size == binders.size)(
              binders.indexOf(body)
            )
            .filter(_ >= 0)
            .map(index => parts.size -> (index + 1))
        case _ => None
      }
    case _ => None
  }

  /** Whether an atom may stand for any type a caller picks: a projection atom does not (nested
    * stuck projections are not read), and a bounded binder's choices are constrained.
    */
  def instantiable(id: String, bounded: String => Boolean): Boolean =
    Projection.unapply(id).isEmpty && !bounded(id)

  /** Why the projections of one signature's shapes have no common reducing instantiation, if they
    * have none, given the proofs of its binders' bounds.
    */
  def refusal(shapes: List[Shape], proofs: List[Shape]): Option[String] = {
    val projected = shapes.flatMap(Universe.select(_) {
      case Shape.Atom(Projection(arity, _, scrutinee)) => Some(scrutinee -> arity)
      case _                                           => None
    })
    val bound = atoms(proofs)
    val equated = atoms(
      shapes.flatMap(Universe.select(_)(Shape.evidencePrism.getOption))
    )
    projected.groupMap(_._1)(_._2).toList.sortBy(_._1).collectFirst {
      case (scrutinee, arities) if arities.distinct.size > 1 =>
        s"stuck match types on ${SourceBindings.label(scrutinee)} have no common reducing " +
          s"instantiation: arities ${arities.distinct.sorted.mkString(", ")}"
      case (scrutinee, _) if bound(scrutinee) =>
        s"stuck match types on ${SourceBindings.label(scrutinee)} meet a bound that mentions it"
      case (scrutinee, _) if equated(scrutinee) =>
        s"stuck match types on ${SourceBindings.label(scrutinee)} meet type evidence that mentions it"
    }
  }

  private def atoms(shapes: List[Shape]): Set[String] =
    shapes
      .flatMap(Universe.select(_)(Shape.atomPrism.getOption(_).map(_.id)))
      .toSet

}
