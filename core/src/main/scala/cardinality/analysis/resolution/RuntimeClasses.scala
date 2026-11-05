package cardinality.analysis.resolution

import scala.annotation.unused
import scala.meta.*

import cardinality.analysis.model.{AliasReading, ResolutionContext}
import cardinality.analysis.{MethodAnalysis, Resolver, StdlibModel}
import cardinality.capacity.Counter
import cardinality.types.{Spelling, TypeApplications}

import MethodAnalysis.{Frame, TypeEntry}
import RuntimeClasses.*

/** The class a union alternative's values carry at runtime, and when two classes provably share no
  * value.
  *
  * A tuple's class is its arity's, and a supplied class, trait or enum is its own declaration. Two
  * distinct tuple classes share no value, and no source class extends one. Every other pair, and
  * every sealed family, is refused: no corpus needs their ancestry.
  */
final private[cardinality] class RuntimeClasses(
    resolver: Resolver,
    @unused types: List[TypeEntry],
    @unused sources: Map[String, Source],
    @unused supplied: String => Boolean
) {

  /** The runtime class of one alternative where it is written, or why it has none. */
  def alternative(tpe: Type, context: ResolutionContext): Either[String, Alternative] =
    tpe match {
      case a: Type.Apply                 => headed(a.tpe, context).map(_.copy(applied = true))
      case Type.Tuple(args)              => Right(Alternative(Builtin(tuple(args.size)), true))
      case _: Type.Name | _: Type.Select => headed(tpe, context)
      case other                         => Left(s"${Spelling.of(other)} has no runtime class")
    }

  /** Why two runtime classes may share a value, if they may. */
  def disjoint(a: Runtime, b: Runtime): Either[String, Unit] = (a, b) match {
    case _ if a == b => Left(s"two alternatives share runtime class ${a.label}")
    // A tuple class is final: no source class extends one, and two distinct ones share no instance.
    case (_: Builtin, _) | (_, _: Builtin) => Right(())
    case _ => Left(s"${a.label} and ${b.label} may overlap (ancestry not read)")
  }

  /** Why a sealed family's values may come from a subclass its shape does not count, if they may. */
  def enumerable(runtime: Runtime): Either[String, Unit] = runtime match {
    case family: Declared =>
      family.tree match {
        case t: (Defn.Trait | Defn.Class) if Counter.isSealed(t) =>
          Left(s"sealed family ${family.label} is not enumerated")
        case _ => Right(())
      }
    case _ => Right(())
  }

  private def headed(head: Type, context: ResolutionContext) = {
    val name = TypeApplications.name(head)
    Found.named(resolver, head, name, context) match {
      case Some(Found.Entry(entry))   => declaration(entry, name, context)
      case Some(failed: Found.Failed) => Left(failed.reason)
      case Some(Found.Absent) if StdlibModel.entry(name).isEmpty =>
        Left(s"$name has no runtime class (declaration not supplied)")
      case Some(Found.Absent) => Left(s"$name has no runtime class this reading tells apart (stdlib model)")
      case _                  => Left(s"${Spelling.of(head)} has no runtime class")
    }
  }

  private def declaration(entry: TypeEntry, name: String, context: ResolutionContext) =
    entry.tree match {
      case _: Defn.Class | _: Defn.Trait | _: Defn.Enum =>
        Right(Alternative(Declared(s"${entry.owner.input}:${entry.tree.pos.start}", entry.name)(entry.tree, entry.owner), false))
      case alias: Defn.Type =>
        context
          .enterAlias(entry, resolver.typeParameters(entry.owner), AliasReading.Runtime)
          .toRight(s"recursive alias: $name")
          .flatMap(alternative(alias.body, _))
      case _ => Left(s"$name has no runtime class")
    }

}

private[cardinality] object RuntimeClasses {

  /** A runtime class: a declaration of the sources or a tuple class. */
  sealed trait Runtime {
    def label: String
  }

  final case class Declared(id: String, label: String)(val tree: Tree, val owner: Frame)
      extends Runtime

  final case class Builtin(label: String) extends Runtime

  /** The first refusal among checks, if any. */
  def all(checks: List[Either[String, Unit]]): Either[String, Unit] =
    checks.collectFirst { case Left(reason) => reason }.toLeft(())

  /** One alternative's class, and whether the alternative applies it to type arguments. */
  final case class Alternative(runtime: Runtime, applied: Boolean)

  // Arities past 22 share one runtime class.
  private def tuple(arity: Int): String =
    if (arity <= 22) s"scala.Tuple$arity" else "scala.runtime.TupleXXL"

}
