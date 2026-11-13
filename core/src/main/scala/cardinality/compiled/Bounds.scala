package cardinality.compiled

import cardinality.analysis.inhabitation.EvidenceAnalysis
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.{StuckProjections, TopTypes}
import cardinality.types.{Graphs, Traversals}
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

/** The bounds of a proper type parameter or abstract type member, read from its symbol, as the old
  * front end's `BinderBounds` reads them from syntax (hkt R5): a subtype bound is an ambient proof,
  * never a value. `A <: U` is `A <:< U` and `A >: L` is `L <:< A`; `A <: Any` and `A >: Nothing`
  * say nothing, and `AnyRef` (`Object`), `Matchable` or `AnyVal` above says nothing of the values.
  * Only a bound that is another type parameter, a rigid application (an endpoint) or a closed type
  * is a proof; a trait or class of the module is closed where its shape reads (a capability
  * record's values are read through its members). A bound to a structural type that is none of them
  * needs coercion analysis: the model's refusal, as before. As there, a bound is read only for a
  * parameter the shapes mention, and for the parameters its bounds mention in turn.
  *
  * A class bound (`Object`, `AnyVal`) admits no type unless every structural lower bound below it
  * is an instance of that class (`[A >: Flag <: AnyVal]`: Scala rejects every call, E057). Which
  * parameter lies below which is not read: where each structural lower bound is a subtype of every
  * class bound of the environment, the class bounds are no-ops (`[A >: Flag <: AnyRef]`), as
  * `Matchable` always is; otherwise the row is not read yet.
  */
final private[compiled] class Bounds(shapes: Shapes)(using Context) {
  import Unread.{Refused, Uncovered}

  /** The count of a method from `values` to `result` under the proofs of the bounds they mention,
    * refused where stuck projections of one parameter no single instantiation reduces.
    */
  def count(values: List[Binding], result: Shape, env: Shapes.Env): Either[Unread, Count] =
    for {
      proofs <- proofs(result :: values.map(_.shape), env)
      _ <- StuckProjections
        .refusal(result :: values.map(_.shape), proofs.map(_.shape))
        .map(Refused(_))
        .toLeft(())
    } yield Readings.count(values ++ proofs, result)

  /** The proofs of the bounds of the bounded proper parameters and abstract type members the shapes
    * mention, as values.
    */
  def proofs(mentioned: List[Shape], env: Shapes.Env): Either[Unread, List[Binding]] = {
    val bounded = env.binders.toList
      .flatMap {
        case (parameter: TypeParamSymbol, atom: Shape.Atom) =>
          Some((atom, (parameter.declaredBounds, parameter.name.toString)))
        case (member: TypeMemberSymbol, atom: Shape.Atom) =>
          Shapes.bounds(member).map(bounds => (atom, (bounds, member.name.toString)))
        case _ => None
      }
      .filter { case (_, (bounds, _)) => !Shapes.unbounded(bounds) }
      .toMap
    def atoms(shape: Shape): List[Shape.Atom] = shape match {
      case atom: Shape.Atom => List(atom).filter(bounded.contains)
      case other            => Shape.children(other).flatMap(atoms)
    }
    // Each parameter's proofs, read for the parameters mentioned and, in turn, for those their
    // bounds mention.
    def reach(
        pending: List[Shape.Atom],
        done: Map[Shape.Atom, List[Shape]]
    ): Either[Unread, Map[Shape.Atom, List[Shape]]] = pending match {
      case Nil                                 => Right(done)
      case atom :: rest if done.contains(atom) => reach(rest, done)
      case atom :: rest                        =>
        read(atom, bounded(atom)._1, bounded(atom)._2, env).flatMap(found =>
          reach(rest ++ found.flatMap(atoms), done + (atom -> found))
        )
    }
    reach(mentioned.flatMap(atoms), Map.empty).flatMap { found =>
      val proofs = found.toList.sortBy(_._1.id).flatMap(_._2)
      // An upper bound whose values hold its parameter again (`A <: P[A]`), directly or through
      // other upper bounds, describes a type by a fixpoint: whether any value exists is not read.
      val recursive = Graphs.cyclic(found.map {
        case (atom, proofs) =>
          atom.id -> proofs
            .collect {
              case Shape.Evidence(`atom`, bound, _) => EvidenceAnalysis.valueAtoms(bound)
            }
            .flatten
            .collect { case Shape.Atom(held) => held }
            .toSet
      })
      // A structural lower bound that is no instance of a class bound (the class doc).
      val classes = bounded.values.map(_._1).filter(Bounds.restricting).map(_.high)
      val lowerBesideClass = proofs.exists {
        case Shape.Evidence(lower, atom: Shape.Atom, _) if !EvidenceAnalysis.endpoint(lower) =>
          classes.exists(!bounded(atom)._1.low.isSubType(_))
        case _ => false
      }
      if (recursive.nonEmpty)
        Left(Refused(s"recursive bound not read: ${recursive.toList.sorted.mkString(", ")}"))
      else if (lowerBesideClass) Left(Uncovered("a structural lower bound beside a class bound"))
      else Right(proofs.zipWithIndex.map((proof, index) => Binding(s"<bound#$index>", proof)))
    }
  }

  // A parameter's proofs.
  private def read(
      atom: Shape,
      bounds: TypeBounds,
      name: String,
      env: Shapes.Env
  ): Either[Unread, List[Shape]] = {
    val upper =
      Option.when(!bounds.high.isSameType(defn.AnyType) && Bounds.classBound(bounds).isEmpty)(
        structural(bounds.high, name, env).map(Shape.Evidence(atom, _, false))
      )
    val lower = Option.when(!bounds.low.isSameType(defn.NothingType))(
      structural(bounds.low, name, env).map(Shape.Evidence(_, atom, false))
    )
    Traversals.sequence(upper.toList ++ lower.toList)
  }

  // A bound's shape where it is a proof. A top inside a bound is read by the place the bound puts
  // it, which a proof does not carry: not read yet.
  private def structural(
      bound: Type,
      name: String,
      env: Shapes.Env
  ): Either[Unread, Shape] =
    shapes.of(bound, env).flatMap {
      case shape if TopTypes.holds(shape) =>
        Left(Uncovered(s"a bound that holds a top type: $name"))
      case shape if EvidenceAnalysis.endpoint(shape) || EvidenceAnalysis.closed(shape) =>
        Right(shape)
      case _ =>
        Left(Refused(s"bound to a structural type requires coercion analysis: $name"))
    }

}

private object Bounds {

  // The upper bounds that say nothing of the values (`AnyRef` is an alias of `Object`).
  private val ClassBounds = Set("java.lang.Object", "scala.Matchable", "scala.AnyVal")

  /** The class of an upper bound that says nothing of the values. */
  private def classBound(bounds: TypeBounds)(using Context): Option[Symbol] =
    bounds.high.dealias match {
      case ref: TypeRef => ref.optSymbol.filter(s => ClassBounds(s.displayFullName))
      case _            => None
    }

  /** Whether the upper bound is a class not every type is an instance of: `Object` or `AnyVal`
    * (every class is `Matchable`).
    */
  private def restricting(bounds: TypeBounds)(using Context): Boolean =
    classBound(bounds).exists(_ != defn.MatchableClass)

}
