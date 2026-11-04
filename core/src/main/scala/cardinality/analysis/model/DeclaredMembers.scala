package cardinality.analysis.model

import scala.collection.mutable
import scala.meta.Tree

import cardinality.analysis.inhabitation.*

import Inhabitation.Shape

/** Binding keys for declared callables, so an overload is a second capability rather than a
  * replacement of the first.
  *
  * One declaration reached from two receivers is two slots. Within one linearization, two
  * declarations of a name at one level overload; at different levels, parameter types that resolve
  * to the same declarations restate the member — the template's own declaration giving its result,
  * while a refinement met only between two inherited levels is refused. Parameter types that read
  * differently, or resolve to different declarations, overload; equal shapes whose declarations are
  * not known are refused.
  */
final private[cardinality] class DeclaredMembers {
  import DeclaredMembers.Declared

  private val seen = mutable.Map.empty[String, List[(Tree, String, Declared)]]

  /** `Some(key)` binds the declaration, `None` is a member already bound, `Left` an identity this
    * reading cannot settle. A declaration read twice for one template keeps its first key.
    */
  def key(name: String, declaration: Tree, declared: Declared): Either[String, Option[String]] = {
    val prior = seen.getOrElse(name, Nil)
    prior.find((tree, _, p) => (tree eq declaration) && p.template == declared.template) match {
      case Some((_, key, _)) => Right(Some(key))
      case None              =>
        val alike = prior
          .map(_._3)
          .filter(p =>
            p.template == declared.template && p.level != declared.level &&
              arguments(p.shape) == arguments(declared.shape)
          )
        alike.find(p => known(p, declared).forall(_.contains(true))) match {
          case Some(p) if p.shape == declared.shape || p.level == p.template => Right(None)
          case Some(_) => Left(s"refined inherited declaration not resolved: $name")
          case None if alike.exists(p => !known(p, declared).exists(_.contains(false))) =>
            Left(s"overloaded declaration identity not resolved: $name")
          case None =>
            val key = if (prior.isEmpty) name else s"$name#${prior.size + 1}"
            seen.update(name, prior :+ (declaration, key, declared))
            Right(Some(key))
        }
    }
  }

  // Per parameter: whether the two identities are equal, where both are known.
  private def known(left: Declared, right: Declared): List[Option[Boolean]] =
    if (left.parameters.size != right.parameters.size) List(Some(false))
    else left.parameters.lazyZip(right.parameters).map((l, r) => l.zip(r).map(_ == _))

  private def arguments(shape: Shape): List[Shape] = shape match {
    case Shape.Function(parameters, _) => parameters
    case _                             => Nil
  }

}

private[cardinality] object DeclaredMembers {

  /** One declaration: the template whose member it is, the frame that declares it, its callable
    * shape and its parameter types' identities (`TypeIdentities`), None where not known.
    */
  final case class Declared(
      template: String,
      level: String,
      shape: Shape,
      parameters: List[Option[String]]
  )

}
