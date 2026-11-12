package cardinality.compiled

import cardinality.analysis.inhabitation.EvidenceAnalysis
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Shape}
import cardinality.analysis.resolution.TopTypes
import cardinality.types.{Graphs, Traversals}
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

/** The bounds of a proper type parameter, read from its symbol, as the old front end's
  * `BinderBounds` reads them from syntax (hkt R5): a subtype bound is an ambient proof, never a
  * value. `A <: U` is `A <:< U` and `A >: L` is `L <:< A`; `A <: Any` and `A >: Nothing` say
  * nothing, and `AnyRef` (`Object`), `Matchable` or `AnyVal` above says nothing of the values. Only
  * a bound that is another type parameter, a rigid application (an endpoint) or a closed type is a
  * proof; a trait or class of the module is closed where its shape reads (a capability record's
  * values are read through its members). A bound to a structural type that is none of them needs
  * coercion analysis: the model's refusal, as before. As there, a bound is read only for a
  * parameter the shapes mention, and for the parameters its bounds mention in turn.
  */
final private[compiled] class Bounds(shapes: Shapes)(using Context) {
  import Unread.{Refused, Uncovered}

  /** The proofs of the bounds of the bounded proper parameters the shapes mention, as values. */
  def proofs(mentioned: List[Shape], env: Shapes.Env): Either[Unread, List[Binding]] = {
    val bounded = env.binders.toList.collect {
      case (parameter: TypeParamSymbol, atom: Shape.Atom)
          if !Shapes.unbounded(parameter.declaredBounds) =>
        atom -> parameter
    }.toMap
    def atoms(shape: Shape): List[Shape.Atom] = shape match {
      case atom: Shape.Atom => List(atom).filter(bounded.contains)
      case other            => Shape.children(other).flatMap(atoms)
    }
    // Each parameter's proofs and whether a class bound is above it, read for the parameters
    // mentioned and, in turn, for those their bounds mention.
    def reach(
        pending: List[Shape.Atom],
        done: Map[Shape.Atom, (List[Shape], Boolean)]
    ): Either[Unread, Map[Shape.Atom, (List[Shape], Boolean)]] = pending match {
      case Nil                                 => Right(done)
      case atom :: rest if done.contains(atom) => reach(rest, done)
      case atom :: rest                        =>
        read(atom, bounded(atom), env).flatMap(found =>
          reach(rest ++ found._1.flatMap(atoms), done + (atom -> found))
        )
    }
    reach(mentioned.flatMap(atoms), Map.empty).flatMap { found =>
      val read = found.toList.sortBy(_._1.id).map(_._2)
      val proofs = read.flatMap(_._1)
      // An upper bound whose values hold its parameter again (`A <: P[A]`), directly or through
      // other upper bounds, describes a type by a fixpoint: whether any value exists is not read.
      val recursive = Graphs.cyclic(found.map {
        case (atom, (proofs, _)) =>
          atom.id -> proofs
            .collect {
              case Shape.Evidence(`atom`, bound, _) => EvidenceAnalysis.valueAtoms(bound)
            }
            .flatten
            .collect { case Shape.Atom(held) => held }
            .toSet
      })
      // A class bound admits no type unless every structural lower bound below it is an
      // instance of that class. Neither which class a lower bound's values are instances of nor
      // which parameters lie below which is read: a structural lower bound anywhere beside a
      // class bound is not read yet.
      val lowerBesideClass = read.exists(_._2) && proofs.exists {
        case Shape.Evidence(lower, _: Shape.Atom, _) => !EvidenceAnalysis.endpoint(lower)
        case _                                       => false
      }
      if (recursive.nonEmpty)
        Left(Refused(s"recursive bound not read: ${recursive.toList.sorted.mkString(", ")}"))
      else if (lowerBesideClass) Left(Uncovered("a structural lower bound beside a class bound"))
      else Right(proofs.zipWithIndex.map((proof, index) => Binding(s"<bound#$index>", proof)))
    }
  }

  // A parameter's proofs, and whether a class bound is above it.
  private def read(
      atom: Shape,
      parameter: TypeParamSymbol,
      env: Shapes.Env
  ): Either[Unread, (List[Shape], Boolean)] = {
    val bounds = parameter.declaredBounds
    val classBound = bounds.high.dealias match {
      case ref: TypeRef => ref.optSymbol.exists(s => Bounds.ClassBounds(s.displayFullName))
      case _            => false
    }
    val upper = Option.when(!bounds.high.isSameType(defn.AnyType) && !classBound)(
      structural(bounds.high, parameter, env).map(Shape.Evidence(atom, _, false))
    )
    val lower = Option.when(!bounds.low.isSameType(defn.NothingType))(
      structural(bounds.low, parameter, env).map(Shape.Evidence(_, atom, false))
    )
    Traversals.sequence(upper.toList ++ lower.toList).map(_ -> classBound)
  }

  // A bound's shape where it is a proof. A top inside a bound is read by the place the bound puts
  // it, which a proof does not carry: not read yet.
  private def structural(
      bound: Type,
      parameter: TypeParamSymbol,
      env: Shapes.Env
  ): Either[Unread, Shape] =
    shapes.of(bound, env).flatMap {
      case shape if TopTypes.holds(shape) =>
        Left(Uncovered(s"a bound that holds a top type: ${parameter.name}"))
      case shape if EvidenceAnalysis.endpoint(shape) || EvidenceAnalysis.closed(shape) =>
        Right(shape)
      case _ =>
        Left(Refused(s"bound to a structural type requires coercion analysis: ${parameter.name}"))
    }

}

private object Bounds {

  // The upper bounds that say nothing of the values (`AnyRef` is an alias of `Object`).
  private val ClassBounds = Set("java.lang.Object", "scala.Matchable", "scala.AnyVal")

}
