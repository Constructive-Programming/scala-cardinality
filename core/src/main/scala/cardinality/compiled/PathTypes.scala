package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.resolution.TopTypes
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*

/** Types selected on paths and refined, as the method side reads them (`Shapes` asks): a member
  * alias as its prefix sees it (`follow`), an abstract member of a parameter as the path's own atom
  * (`member`, members M0), a refinement of type members over a type whose values nothing tells
  * apart (`refinement`), and the polymorphic identity (`polyIdentity`).
  */
final private[compiled] class PathTypes(of: (Type, Shapes.Env) => Either[Unread, Shape])(using
    Context
) {
  import Shapes.Env
  import Unread.Uncovered

  // A refinement of type members (`R { type X = A }`, `{ type X <: Tuple }`) over a type whose values
  // nothing tells apart (only type members, or a top type) is that type, where it can be satisfied:
  // the member is new or abstract and unbounded in a trait or abstract class (or `Object`), and the
  // refinement fixes it or bounds it on one side. Over any other type, what the members fixed
  // change of what its members supply is not read yet.
  def refinement(refined: TypeRefinement, env: Env): Either[Unread, Shape] = {
    val bounds = refined.refinedBounds
    val name = refined.refinedName
    def free(parent: Type): Boolean = parent match {
      case inner: TypeRefinement => inner.refinedName != name && free(inner.parent)
      case _                     =>
        World.classNamed(parent).exists { c =>
          (c == defn.ObjectClass || c.isTrait || c.isAbstractClass) &&
          c.linearization.flatMap(_.getDecl(name)).forall {
            case member: TypeMemberSymbol =>
              member.typeDef match {
                case TypeMemberDefinition.AbstractType(declared) => Shapes.unbounded(declared)
                case _                                           => false
              }
            case _ => false
          }
        }
    }
    val satisfiable = free(refined.parent) && (bounds.isInstanceOf[TypeAlias] ||
      bounds.low.isSameType(defn.NothingType) || bounds.high.isSameType(defn.AnyType))
    if (!satisfiable) Left(Uncovered(s"a type: ${refined.showBasic}"))
    else
      of(refined.parent, env)
        .filterOrElse(PathTypes.featureless, Uncovered(s"a type: ${refined.showBasic}"))
  }

  // An abstract type member selected on a parameter of the measured definition or of a method
  // around it (`p.X`), unbounded as the path sees it: the witness of the package the parameter's
  // value opens, an atom of the path's own, so two paths never share it and `q: p.type` is p's path
  // (members M0, the old `TypeMembers`). Read only where the parameter's type tells none of its
  // values apart (only type members, or a top type), so nothing else it holds supplies the member.
  // Another method's parameter (a capability's, a constructor's read as a field) is one path per
  // call: not read.
  def member(ref: TypeRef, env: Env): Either[Unread, Shape] = {
    def path(prefix: Prefix): Option[(TermSymbol, Type)] = prefix match {
      case term: TermRef =>
        term.optSymbol
          .collect {
            case p: TermSymbol
                if PathTypes.methodParameter(p) && World.owners(env.site).contains(p.owner) =>
              p
          }
          .flatMap(p =>
            p.declaredType match {
              case singleton: TermRef => path(singleton)
              case _: ByNameType      => None
              case tpe: Type          => Some(p -> tpe)
              case _                  => None
            }
          )
      case _ => None
    }
    val atom = for {
      (p, declared) <- path(ref.prefix)
      if ref.optAliasedType.isEmpty && Shapes.unbounded(ref.bounds)
      read <- of(declared, env).toOption
      if PathTypes.featureless(read)
    } yield Shape.Atom(s"path:${p.owner.displayFullName}.${p.name}#${ref.name}")
    atom.toRight(Uncovered(s"a type: ${ref.showBasic}"))
  }

}

private[compiled] object PathTypes {

  /** Whether a polymorphic function type is the identity's, `[A] => A => A` with `A` unbounded: its
    * one implementation (the closed identity fragment of the old `MethodTypeLambdas`). Any other is
    * not read yet.
    */
  def polyIdentity(refined: TermRefinement)(using Context): Boolean =
    (World.classNamed(refined.parent).map(_.displayFullName), refined.refinedType) match {
      case (Some("scala.PolyFunction"), poly: PolyType) =>
        def own(tpe: TypeOrMethodic) = tpe match {
          case ref: TypeParamRef => ref.binder eq poly
          case _                 => false
        }
        poly.paramNames.sizeIs == 1 && Shapes.unbounded(poly.paramTypeBounds.head) &&
        (poly.resultType match {
          case method: MethodType =>
            !method.isImplicitOrContextual && method.paramTypes.sizeIs == 1 &&
            own(method.paramTypes.head) && own(method.resultType)
          case _ => false
        })
      case _ => false
    }

  /** A type with its aliases followed as the compiler sees them: a member alias through the prefix
    * it is selected on (`p.X` for a `p: R { type X = A }` is `A`; `Y` of a parent seen from a
    * subclass, its arguments substituted), which `dealias` does not do. An opaque alias stays.
    */
  def follow(tpe: Type)(using Context): Type = tpe match {
    case ref: TypeRef if ref.optSymbol.forall {
          case member: TypeMemberSymbol => !member.isOpaqueTypeAlias
          case _                        => true
        } =>
      ref.optAliasedType.fold(tpe.dealias)(follow)
    case _ => tpe.dealias
  }

  /** Whether a shape tells none of a type's values apart: a type of type members alone, or a top.
    */
  def featureless(shape: Shape): Boolean =
    shape == Shape.Product(Nil) || shape == TopTypes.terminal

  /** Whether a term is a value parameter of a method. */
  def methodParameter(term: TermSymbol): Boolean = term.owner match {
    case method: TermSymbol => method.paramSymss.exists(_.left.exists(_.contains(term)))
    case _                  => false
  }

}
