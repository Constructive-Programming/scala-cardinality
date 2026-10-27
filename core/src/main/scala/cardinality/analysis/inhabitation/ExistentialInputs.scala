package cardinality.analysis.inhabitation

import scala.meta.*

import cardinality.analysis.*
import cardinality.types.*

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

  final private case class RewriteEnv(
      replacements: Map[String, Shape],
      inserted: Set[String],
      freshName: () => String
  )

  private val mappedAtoms =
    Shape.cata[(RewriteEnv, Map[String, String]) => Shape] { (node, kids) => (env, bound) =>
      node match {
        case atom @ Atom(id) =>
          bound.get(id).map(Atom(_)).getOrElse(env.replacements.getOrElse(id, atom))
        case Existential(witnesses, _) =>
          val renamings =
            witnesses.map(id => id -> (if (env.inserted(id)) env.freshName() else id))
          Existential(renamings.map(_._2), kids.head(env, bound ++ renamings))
        case _ =>
          Shape.rebuild(node, kids)(_(env, bound))
      }
    }

  private val collectedNames =
    Shape.cata[(Boolean, Set[String]) => Set[String]] { (node, kids) => (includeBound, bound) =>
      node match {
        case Atom(id) =>
          if (!includeBound && bound(id)) Set.empty else Set(id)
        case Existential(witnesses, _) =>
          (if (includeBound) witnesses.toSet else Set.empty[String]) ++
            kids.head(includeBound, bound ++ witnesses)
        case _ =>
          kids.foldLeft(Set.empty[String])((acc, kid) => acc ++ kid(includeBound, bound))
      }
    }

  /** Capture-avoiding rewriting of free atoms. Renaming is deterministic and scoped. */
  def mapAtoms(shape: Shape)(rewrite: Atom => Shape): Shape = {
    val replacements = freeAtoms(shape).map(id => id -> rewrite(Atom(id))).toMap
    val inserted = replacements.values.flatMap(freeAtoms).toSet
    val reserved = scala.collection.mutable.Set.from(
      names(shape) ++ replacements.values.flatMap(names)
    )
    var alphaNumber = 0
    def freshName(): String = {
      val (name, next) = Shape.freshName("$existentialAlpha", alphaNumber)(reserved)
      alphaNumber = next
      reserved += name
      name
    }
    mappedAtoms.get(shape)(RewriteEnv(replacements, inserted, () => freshName()), Map.empty)
  }

  def freeAtoms(shape: Shape): Set[String] = collectedNames.get(shape)(false, Set.empty)

  /** Fresh allocation must avoid binders as well as free atoms. */
  def names(shape: Shape): Set[String] = collectedNames.get(shape)(true, Set.empty)

}
