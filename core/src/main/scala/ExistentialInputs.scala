package cardinality

import scala.meta.*

import Inhabitation.Shape
import Inhabitation.Shape.*
import MethodAnalysis.{Frame, Resolved}

/** Existentials are packages with locally bound witness names, not caller-instantiable types.
  * Opening an input replaces those names with rigid atoms owned by that value's provenance.
  */
private[cardinality] object ExistentialInputs {

  /** Capture names describe a type template. Runtime input identity comes from the solver's binding
    * provenance, so two inputs of the same alias do not share their hidden witnesses. Re-resolving
    * a stable input's annotation must retain its template names.
    */
  final class Captures {

    private val templates =
      scala.collection.mutable.Map.empty[(String, String, Int, Int), String]

    def arguments(
        callee: Type,
        types: List[Type],
        frame: Frame,
        resolve: Type => Resolved
    ): Either[String, (List[String], List[Shape])] = {
      val witnesses = List.newBuilder[String]
      val prepared = types.zipWithIndex.map { (tpe, index) =>
        tpe match {
          case wildcard: Type.Wildcard
              if wildcard.bounds.lo.nonEmpty || wildcard.bounds.hi.nonEmpty =>
            Left(TypePlaceholders.existential(wildcard))
          case wildcard: Type.Wildcard =>
            val key = (frame.id, TypeApplications.name(callee), wildcard.pos.start, index)
            val name = templates.getOrElseUpdate(key, s"$$existentialTemplate${templates.size}")
            witnesses += name
            Right(Atom(name))
          case other => resolve(other)
        }
      }
      MethodAnalysis.sequence(prepared).map(shapes => witnesses.result() -> shapes)
    }

  }

  val resultBoundary =
    "existential wildcard result construction and repackaging requires witness-choice analysis"

  val callableBoundary =
    "existential callable result elimination requires scoped witness-generation analysis"

  /** Capture-avoiding rewriting of free atoms. Renaming is deterministic and scoped. */
  def mapAtoms(shape: Shape)(rewrite: Atom => Shape): Shape = {
    val replacements = freeAtoms(shape).map(id => id -> rewrite(Atom(id))).toMap
    val inserted = replacements.values.flatMap(freeAtoms).toSet
    val reserved = scala.collection.mutable.Set.from(
      names(shape) ++ replacements.values.flatMap(names)
    )
    var alphaNumber = 0
    def freshName(): String = {
      var name = s"$$existentialAlpha$alphaNumber"
      while (reserved(name)) {
        alphaNumber += 1
        name = s"$$existentialAlpha$alphaNumber"
      }
      alphaNumber += 1
      reserved += name
      name
    }
    def loop(current: Shape, bound: Map[String, String]): Shape = current match {
      case atom @ Atom(id) =>
        bound.get(id).map(Atom(_)).getOrElse(replacements.getOrElse(id, atom))
      case Singleton(id, underlying)    => Singleton(id, underlying.map(loop(_, bound)))
      case Product(fields)              => Product(fields.map(loop(_, bound)))
      case Sum(cases)                   => Sum(cases.map(loop(_, bound)))
      case Function(parameters, result) =>
        Function(parameters.map(loop(_, bound)), loop(result, bound))
      case Repeated(element)               => Repeated(loop(element, bound))
      case Evidence(left, right, equality) =>
        Evidence(loop(left, bound), loop(right, bound), equality)
      case Existential(witnesses, body) =>
        val renamings = witnesses.map(id => id -> (if (inserted(id)) freshName() else id))
        Existential(renamings.map(_._2), loop(body, bound ++ renamings))
    }
    loop(shape, Map.empty)
  }

  def freeAtoms(shape: Shape): Set[String] = collectNames(shape, includeBound = false)

  /** Fresh allocation must avoid binders as well as free atoms. */
  def names(shape: Shape): Set[String] = collectNames(shape, includeBound = true)

  private def collectNames(shape: Shape, includeBound: Boolean): Set[String] = {
    def loop(current: Shape, bound: Set[String]): Set[String] = current match {
      case Atom(id)                     => if (!includeBound && bound(id)) Set.empty else Set(id)
      case Singleton(_, underlying)     => underlying.toSet.flatMap(loop(_, bound))
      case Product(fields)              => fields.flatMap(loop(_, bound)).toSet
      case Sum(cases)                   => cases.flatMap(loop(_, bound)).toSet
      case Function(parameters, result) =>
        parameters.flatMap(loop(_, bound)).toSet ++ loop(result, bound)
      case Repeated(element)            => loop(element, bound)
      case Evidence(left, right, _)     => loop(left, bound) ++ loop(right, bound)
      case Existential(witnesses, body) =>
        (if (includeBound) witnesses.toSet else Set.empty[String]) ++
          loop(body, bound ++ witnesses)
    }
    loop(shape, Set.empty)
  }

}
