package cardinality.analysis.inhabitation

import cardinality.analysis.*

import Inhabitation.Shape
import Inhabitation.Shape.*

/** Existentials are packages with locally bound witness names, not caller-instantiable types.
  * Opening an input replaces those names with rigid atoms owned by that value's provenance.
  */
private[cardinality] object ExistentialInputs {

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
        // A member's binders are unique to it, so they are bound without renaming.
        case Poly(binders, _) =>
          Poly(binders, kids.head(env, bound ++ binders.map(b => b.id -> b.id)))
        case _ =>
          Shape.rebuild(node, kids)(_(env, bound))
      }
    }

  private val collectedNames =
    Shape.cata[(Boolean, Set[String]) => Set[String]] { (node, kids) => (includeBound, bound) =>
      node match {
        case Atom(id) =>
          if (!includeBound && bound(id)) Set.empty else Set(id)
        case Binders(ids) =>
          (if (includeBound) ids.toSet else Set.empty[String]) ++
            kids.head(includeBound, bound ++ ids)
        case _ =>
          kids.foldLeft(Set.empty[String])((acc, kid) => acc ++ kid(includeBound, bound))
      }
    }

  // The atoms a node binds over its body: an existential's witnesses, a member's own binders.
  private object Binders {

    def unapply(node: Shape): Option[List[String]] = node match {
      case Existential(witnesses, _) => Some(witnesses)
      case Poly(binders, _)          => Some(binders.map(_.id))
      case _                         => None
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
