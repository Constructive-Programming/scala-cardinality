package cardinality.compiled

import cardinality.analysis.BuiltinTypes
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Names.typeName
import tastyquery.Symbols.*
import tastyquery.Types.*

/** Types selected on paths and refined, as the method side reads them (`Shapes` asks): a member
  * alias as its prefix sees it (`follow`), an abstract member of a parameter as the path's own atom
  * (`member`, members M0), a refinement of type members over a type whose values nothing tells
  * apart (`refinement`), the polymorphic identity (`polyIdentity`), the standard library's opaque
  * aliases (`standard`) and a class applied to wildcards (`existential`); and which match types
  * scalac reduces (`trusted`).
  */
final private[compiled] class PathTypes(of: Shapes.Reading)(using Context) {
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
    def path(prefix: Prefix): Option[(String, Type)] = prefix match {
      case term: TermRef =>
        term.optSymbol
          .collect {
            case p: TermSymbol
                if PathTypes.methodParameter(p) && World.owners(env.site).contains(p.owner) =>
              s"${p.owner.displayFullName}.${p.name}" -> p
          }
          .flatMap((id, p) =>
            p.declaredType match {
              case singleton: TermRef => path(singleton)
              case _: ByNameType      => None
              case tpe: Type          => Some(id -> tpe)
              case _                  => None
            }
          )
      case _ => None
    }
    // A member of kind `F[_]`, invariant and rigid as the path sees it, is the path's own rigid
    // constructor (`Shapes.free`), applied as a type parameter of that kind is.
    val constructor = ref.optSymbol.exists {
      case member: TypeMemberSymbol =>
        Shapes.free(member) && Shapes.rigidBounds(ref.bounds) &&
        tastyquery.Variances.invariant(ref.bounds.high)
      case _ => false
    }
    val atom = for {
      (where, declared) <- path(ref.prefix)
      if ref.optAliasedType.isEmpty && (Shapes.unbounded(ref.bounds) || constructor)
      read <- of(declared, env).toOption
      if PathTypes.featureless(read)
      id = s"path:$where#${ref.name}"
    } yield if (constructor) Shape.Constructor(id) else Shape.Atom(id)
    atom.toRight(Uncovered(s"a type: ${ref.showBasic}"))
  }

  /** An opaque alias of the standard library, read by the standard model under the name it spells
    * (a named tuple is its fields, `StdlibModel`); not read otherwise.
    */
  def standard(alias: TypeMemberSymbol, arguments: List[Type], env: Env): Either[Unread, Shape] =
    Traversals.sequence(arguments.map(of(_, env))).flatMap { read =>
      BuiltinTypes
        .known(alias.displayFullName.replace("$.", "."), read)
        .map(_.left.map(Unread.Refused(_)))
        .getOrElse(
          Left(Uncovered(s"an opaque type of the standard library: ${alias.displayFullName}"))
        )
    }

  // The type lambdas `existential` builds, each parameter a witness. A witness's atom only needs to
  // differ from every other atom of the count (the solver renames an input package's witnesses per
  // value), so the order the lambdas are built in changes no count.
  private val witnesses = scala.collection.mutable.ListBuffer.empty[TypeLambda]

  private def witnessId(lambda: Int, parameter: Int) = s"wildcard:$lambda:$parameter"

  /** A class applied to wildcards (`Packed[?, R]`) is a package whose witnesses are the wildcards,
    * one each: `∃W. Packed[W, R]`. The solver opens an input package per value and refuses to build
    * one as a result (the old `ExistentialInputs`). Read where each wildcard and the parameter it
    * stands for are unbounded and take types: a bound would give the witness proofs, which are not
    * read. None otherwise (`World.proper` refuses the wildcard).
    */
  def existential(
      ref: TypeRef,
      arguments: List[TypeOrWildcard],
      env: Env
  ): Option[Either[Unread, Shape]] = {
    val parameters = ref.optSymbol.collect { case c: ClassSymbol => c.typeParams }.toList.flatten
    val wildcards = arguments.collect { case wildcard: WildcardTypeArg => wildcard }
    val read = wildcards.nonEmpty && parameters.sizeIs == arguments.size &&
      arguments.lazyZip(parameters).forall {
        case (wildcard: WildcardTypeArg, parameter) =>
          Shapes.unbounded(wildcard.bounds) && Shapes.unbounded(parameter.declaredBounds)
        case _ => true
      }
    Option.when(read) {
      val lambda = TypeLambda(wildcards.indices.map(i => typeName(s"W$i")).toList)(
        _ => wildcards.map(_.bounds),
        lambda => {
          val refs = lambda.paramRefs.iterator
          AppliedType(
            ref,
            arguments.map {
              case _: WildcardTypeArg => refs.next()
              case argument           => argument
            }
          )
        }
      )
      witnesses += lambda
      val ids = wildcards.indices.map(witnessId(witnesses.size, _)).toList
      of(lambda.resultType, env).map(Shape.Existential(ids, _))
    }
  }

  /** A wildcard's witness (`existential`), as its atom. */
  def witness(ref: TypeParamRef): Either[Unread, Shape] =
    witnesses.indexWhere(_ eq ref.binder) match {
      case -1    => Left(Uncovered(s"a type: ${ref.showBasic}"))
      case index => Right(Shape.Atom(witnessId(index + 1, ref.paramNum)))
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

  /** A type lambda that applies one constructor to its parameters in their order is that
    * constructor (`[y] =>> G[y]` is `G`, `[a, b] =>> Either[a, b]` is `Either`).
    */
  object Eta {

    def unapply(lambda: TypeLambda): Option[TypeRef] = lambda.resultType match {
      case applied: AppliedType =>
        applied.tycon match {
          case constructor: TypeRef if applied.args.corresponds(lambda.paramRefs) {
                case (argument: TypeParamRef, own) =>
                  (argument.binder eq lambda) && argument.paramNum == own.paramNum
                case _ => false
              } =>
            Some(constructor)
          case _ => None
        }
      case _ => None
    }

  }

  /** A type with its aliases followed as the compiler sees them: a member alias through the prefix
    * it is selected on (`p.X` for a `p: R { type X = A }` is `A`; `Y` of a parent seen from a
    * subclass, its arguments substituted), which `dealias` does not do, and a member its bounds pin
    * to one type (`pinned`), applied or not. An opaque alias stays.
    */
  def follow(tpe: Type)(using Context): Type = tpe match {
    case ref: TypeRef if ref.optSymbol.forall {
          case member: TypeMemberSymbol => !member.isOpaqueTypeAlias
          case _                        => true
        } =>
      ref.optAliasedType.map(own(ref.prefix, _)).orElse(pinned(ref)).fold(tpe.dealias)(follow)
    case World.Named(ref, arguments) =>
      pinned(ref).fold(tpe.dealias)(body => follow(body.appliedTo(arguments)))
    case _ => tpe.dealias
  }

  // A member of a refinement's own value (`T { type X = this.Y }`, or a sibling `{ type X = Y }`),
  // which tasty-query leaves unsubstituted in the alias it gives a member selected on the
  // refinement (`p.X`, `R[A, B]#X`), is that member of what it was selected on (`p.Y`, `R[A, B]#Y`).
  private def own(prefix: Prefix, body: Type)(using Context): Type = (prefix, body) match {
    case (on: Type, member: TypeRef) =>
      // A `this` directly in the member's alias is the refinement declaring the member, the one
      // it was selected on (a fresh copy, once an alias's arguments are substituted).
      val refined = on match {
        case path: TermRef => path.underlying
        case other         => other.dealias
      }
      (member.prefix, refined) match {
        case (_: RecThis, _: RecType) => TypeRef(on, member.name)
        case _                        => body
      }
    case _ => body
  }

  /** An abstract type member bounded by one type on both sides, as its prefix sees it: that type.
    * An export's alias is one (`export Types.Pair`), and so is a member a refinement fixes (`R#X`
    * for `R = { type X = Unit }` or `{ type X <: Nothing }`).
    */
  def pinned(ref: TypeRef)(using Context): Option[Type] =
    ref.optSymbol
      .fold(Some(ref.bounds)) {
        case member: TypeMemberSymbol =>
          Option.when(member.typeDef.isInstanceOf[TypeMemberDefinition.AbstractType])(ref.bounds)
        case _ => None
      }
      .collect { case bounds if exported(bounds) => bounds.high }

  /** Bounds that meet, one type on both sides (or bounds written so, which only that type meets):
    * an export's alias, a member a refinement fixes.
    */
  def exported(bounds: TypeBounds)(using Context): Boolean = bounds.low.isSameType(bounds.high)

  /** Whether a shape tells none of a type's values apart: a type of type members alone, or a top.
    */
  def featureless(shape: Shape): Boolean =
    shape == Shape.Product(Nil) || shape == TopTypes.terminal

  /** Whether a term is a value parameter of a method. */
  def methodParameter(term: TermSymbol): Boolean = term.owner match {
    case method: TermSymbol => method.paramSymss.exists(_.left.exists(_.contains(term)))
    case _                  => false
  }

  /** What a match type reduces to where scalac reduces it too: tasty-query reduces on an abstract
    * scrutinee where scalac does not (`Fst[T]` for a `T <: (Int, String)` is `Int` there, stuck in
    * scalac), so its reduction is trusted on a ground scrutinee, and where the first case's pattern
    * matches whatever the scrutinee's abstract parts are (`covers`).
    */
  def trusted(matched: MatchType)(using Context): Option[Type] =
    matched.reduced.filter(_ =>
      ground(matched.scrutinee) || matched.cases.headOption.exists { first =>
        // `Any`, a binder of the case met once, or a tuple of such patterns over a tuple of the
        // scrutinee's arity: the subtype test scalac makes succeeds whatever the parts are.
        def binder(tpe: TypeOrWildcard) = tpe match {
          case ref: TypeParamRef if ref.binder eq first => List(ref.paramNum)
          case _                                        => Nil
        }
        def binders(pattern: TypeOrWildcard): List[Int] = pattern match {
          case applied: AppliedType => applied.args.flatMap(binders)
          case other                => binder(other)
        }
        def covers(pattern: TypeOrWildcard, scrutinee: Type): Boolean = (
          pattern,
          scrutinee.dealias
        ) match {
          case (any: Type, _) if any.isSameType(defn.AnyType) => true
          case (ref, _) if binder(ref).nonEmpty               => true
          case (tuple: AppliedType, applied: AppliedType)     =>
            World.classNamed(tuple).exists(_.displayFullName == s"scala.Tuple${tuple.args.size}") &&
            World.classNamed(applied) == World.classNamed(tuple) &&
            tuple.args.lazyZip(applied.args).forall {
              case (part, written: Type) => covers(part, written)
              case _                     => false
            }
          case _ => false
        }
        val bound = binders(first.pattern)
        bound.distinct == bound && covers(first.pattern, matched.scrutinee)
      }
    )

  /** A type built of static classes and constants alone: no type parameter, abstract member, path
    * or other form a reduction would have to reason about.
    */
  private def ground(tpe: TypeOrWildcard)(using Context): Boolean = tpe match {
    case ref: TypeRef if ref.dealias ne ref => ground(ref.dealias)
    case ref: TypeRef                       =>
      ref.optSymbol.exists {
        // `Null` is below every nullable class: tasty-query reduces on it where scalac does not.
        case c: ClassSymbol => World.inObjects(c) && c != defn.NullClass
        case _              => false
      }
    case applied: AppliedType if applied.dealias ne applied => ground(applied.dealias)
    case applied: AppliedType => ground(applied.tycon) && applied.args.forall(ground)
    case _: ConstantType      => true
    case _                    => false
  }

}
