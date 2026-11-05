package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.*
import cardinality.analysis.Resolver
import cardinality.analysis.model.*
import cardinality.types.*

/** The stable paths a scope declares; a member selected on one, or on a projected type, is refused.
  */
private[cardinality] object RefinedMembers {

  /** A member of a qualifier type, or of a type projection `T#N`: not read, so refused. */
  def resolve(
      qualifier: Type,
      member: String,
      @annotation.unused context: ResolutionContext,
      @annotation.unused resolver: Resolver,
      @annotation.unused projection: Boolean = false
  ): Resolved = Left(s"unavailable refined member: ${Spelling.of(qualifier)}#$member")

  def pathType(path: Term.Ref, frame: Frame): Option[Either[String, (Type, Frame)]] = path match {
    case name: Term.Name =>
      frame.chain.iterator.map(declaredPath(name.value, _)).find(_.nonEmpty).flatten
    case _ => None
  }

  /** Whether a type selects a member of a value's path (`p.T`, `p` a parameter or value). */
  def throughValue(tpe: Type, frame: Frame): Boolean = tpe match {
    case s: Type.Select => pathType(s.qual, frame).nonEmpty
    case _              => false
  }

  /** The innermost scope of a frame's chain that binds a term name as a parameter or value. */
  def declaring(name: String, frame: Frame): Option[Frame] =
    frame.chain.find(declaresValue(name, _))

  /** Whether a scope binds a term name as a parameter or value. */
  def declaresValue(name: String, owner: Frame): Boolean = declaredPath(name, owner).nonEmpty

  /** The type a scope declares a term name with, when it binds the name: why it is no stable path
    * otherwise.
    */
  def declaredValue(name: String, owner: Frame): Option[Either[String, Type]] =
    declaredPath(name, owner).map(_.map(_._1))

  private def declaredPath(name: String, owner: Frame): Option[Either[String, (Type, Frame)]] = {
    val parameter = owner.params.find(_.name.value == name).map(parameterType)
    val binding = owner.stats.collectFirst(Function.unlift(bindingType(name, _)))
    parameter.orElse(binding).map(_.map(_ -> owner))
  }

  // A by-name parameter is no stable path: each read evaluates its argument anew. Neither is a
  // `var`, by Scala's typing rule (E083, "not an immutable path"), though the model reads it as the
  // value it holds.
  private def parameterType(parameter: Term.Param): Either[String, Type] =
    if (parameter.mods.exists(_.is[Mod.VarParam]))
      Left(s"not a stable path: ${parameter.name.value} is a var")
    else
      parameter.decltpe.filterNot(_.is[Type.ByName]).toRight(unavailable(parameter.name.value))

  private def bindingType(name: String, stat: Stat): Option[Either[String, Type]] = stat match {
    case d: Defn.Val if TypeApplications.binds(name, d.pats) =>
      Some(d.decltpe.toRight(unavailable(name)))
    case d: Decl.Val if TypeApplications.binds(name, d.pats) => Some(Right(d.decltpe))
    case d: (Defn.Def | Decl.Def) if d.name.value == name    => Some(Left(unavailable(name)))
    case _                                                   => None
  }

  private def unavailable(name: String): String = s"unavailable stable path type: $name"

}
