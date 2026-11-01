package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{BuiltinTypes, MethodAnalysis, Resolver}
import cardinality.types.Traversals

import MethodAnalysis.TypeEntry

/** A type's identity as the declarations its names resolve to, for the questions a structural shape
  * cannot answer: `final case class P1[A](a: A)` and `P2[A](a: A)` share a shape and are unrelated
  * types, and two classes spelled `P` in two objects are two types.
  */
private[cardinality] object TypeIdentities {

  /** A canonical spelling of `tpe`: each name the declaration it resolves to, each binder its own
    * identity. None when a name is an abstract member, a parameterized alias or unresolved, or a
    * binder stands for an instantiation — an identity this reading does not know.
    */
  def of(tpe: Type, context: ResolutionContext, resolver: Resolver): Option[String] = tpe match {
    case n: Type.Name =>
      context.variables.get(n.value) match {
        case Some(Right(Shape.Atom(id)))                         => Some(s"binder $id")
        case Some(Right(constructor @ Shape.Constructor(token))) =>
          Option.when(resolver.operators.isRigid(constructor))(token)
        case Some(_) => None
        case None    => named(tpe, n.value, context, resolver)
      }
    case s: Type.Select     => named(tpe, s.syntax, context, resolver)
    case a: Type.Apply      => applied(a.tpe, a.argClause.values, context, resolver)
    case a: Type.ApplyInfix =>
      applied(a.op, List(a.lhs, a.rhs), context, resolver)
    // Sugar is the type it spells: `(A, B)` is `Tuple2[A, B]`, `A => B` is `Function1[A, B]`.
    case Type.Tuple(args) =>
      all(args, context, resolver).map(_.mkString(s"scala.Tuple${args.size}[", ", ", "]"))
    case f: Type.Function =>
      all(f.paramClause.values :+ f.res, context, resolver)
        .map(_.mkString(s"scala.Function${f.paramClause.values.size}[", ", ", "]"))
    case _ => None
  }

  /** The declaration of a final concrete class a type applies, when it is one: no other type is a
    * subtype of it, so two distinct ones are unrelated whatever their arguments.
    */
  def finalClass(tpe: Type, context: ResolutionContext, resolver: Resolver): Option[String] = {
    val callee = tpe match {
      case a: Type.Apply => a.tpe
      case other         => other
    }
    callee match {
      case n: Type.Name if context.variables.contains(n.value) => None
      case _: (Type.Name | Type.Select)                        =>
        resolver.declaration(callee, callee.syntax, context) match {
          case Found.Entry(entry)
              if entry.tree.is[Defn.Class] && FinalImplementations.isFinal(entry.tree) =>
            Some(key(entry))
          case _ => None
        }
      case _ => None
    }
  }

  private def applied(
      callee: Type,
      args: List[Type],
      context: ResolutionContext,
      resolver: Resolver
  ): Option[String] =
    for {
      head <- of(callee, context, resolver)
      parts <- all(args, context, resolver)
    } yield parts.mkString(s"$head[", ", ", "]")

  private def all(
      types: List[Type],
      context: ResolutionContext,
      resolver: Resolver
  ): Option[List[String]] =
    Traversals.defined(types.map(of(_, context, resolver)))

  private def named(
      tpe: Type,
      name: String,
      context: ResolutionContext,
      resolver: Resolver
  ): Option[String] =
    resolver.declaration(tpe, name, context) match {
      case Found.Entry(entry) => declared(entry, context, resolver)
      case Found.Absent       => BuiltinTypes.canonical(name).map(plain => s"scala.$plain")
      case _                  => None
    }

  // A class, trait, enum or opaque type is its own identity; a plain alias is the type it names,
  // read where it is declared; an abstract member is not known here.
  private def declared(
      entry: TypeEntry,
      context: ResolutionContext,
      resolver: Resolver
  ): Option[String] = entry.tree match {
    case d: Defn.Type if !d.mods.exists(_.is[Mod.Opaque]) =>
      val k = key(entry)
      Option
        .when(d.tparamClause.values.isEmpty && !context.visiting(k))(
          context.inScope(entry.owner, resolver.typeParameters(entry.owner)).enter(k)
        )
        .flatMap(of(d.body, _, resolver))
    case _: Decl.Type => None
    case _            => Some(key(entry))
  }

  /** A declaration's identity: its declaring scope and its name. */
  def key(entry: TypeEntry): String = s"${entry.owner.id}#${entry.name}"

}
