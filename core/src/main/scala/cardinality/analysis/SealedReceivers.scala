package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation, Natural}

import Inhabitation.{Count, Shape}
import MethodAnalysis.{Frame, Target}

/** A member of an enum or a sealed template reads `this`, which is one of the family's cases: a
  * body may match on it, so its implementations are functions of the case, and an enum's class
  * parameters are fixed by the case rather than supplied. The measurement binds neither the
  * receiver nor that dependency, so its count is an answer only where the receiver cannot change
  * it:
  *
  *   - a terminal goal (one value) has one implementation in every environment;
  *   - a goal with infinitely many literals (`String`) has ω implementations once the context is
  *     not absurd: the solver's ω already proves the rest of the environment is not, and a receiver
  *     of an enum with a case the empty environment builds keeps it so (finite programs bound it at
  *     ω).
  *
  * Anything else is refused until the receiver is split.
  */
final private[cardinality] class SealedReceivers(target: Target, resolver: Resolver) {

  private val families: List[Frame] =
    if (target.constructor) Nil else target.frame.chain.filter(SealedReceivers.family)

  def certify(goal: Shape, count: Count): Count = families.headOption match {
    case Some(family) if !stands(goal, count) =>
      val name = family.path.last
      Count.Unresolved(
        List(s"receiver of sealed family $name is a sum this measurement does not split")
      )
    case _ => count
  }

  // A reason stands as it is; a number stands for a terminal goal, or as ω for a goal of
  // infinitely many literals over inhabited enums.
  private def stands(goal: Shape, count: Count): Boolean = count match {
    case Count.Unresolved(_) => true
    case _                   =>
      values(goal).exists { space =>
        space.isOne ||
        (space == Card.Omega && count == Count.Countable && families.forall(inhabited))
      }
  }

  private def values(goal: Shape): Option[Card] =
    Option
      .when(ClosedSpaces.closed(goal))(Natural.attempt(ClosedSpaces.space(goal)).toOption)
      .flatten

  // An enum is inhabited when one of its cases is built from nothing: a singleton case, or one
  // whose fields are closed and nonempty.
  private def inhabited(family: Frame): Boolean = family.tree.exists {
    case d: Defn.Enum =>
      d.templ.body.stats.exists {
        case _: Defn.RepeatedEnumCase => true
        case c: Defn.EnumCase         => c.ctor.paramClauses.flatMap(_.values).forall(built(family))
        case _                        => false
      }
    case _ => false
  }

  private def built(family: Frame)(field: Term.Param): Boolean =
    field.decltpe
      .flatMap(tpe => resolver.resolve(tpe, family, resolver.typeParameters(family)).toOption)
      .flatMap(values)
      .exists(!_.isZero)

}

private[cardinality] object SealedReceivers {

  private def family(frame: Frame): Boolean = frame.tree.exists(sealedTree)

  /** An enum, or a sealed trait or class: a template whose cases are all in its file. */
  def sealedTree(tree: Tree): Boolean = tree match {
    case _: Defn.Enum  => true
    case d: Defn.Trait => d.mods.exists(_.is[Mod.Sealed])
    case d: Defn.Class => d.mods.exists(_.is[Mod.Sealed])
    case _             => false
  }

}
