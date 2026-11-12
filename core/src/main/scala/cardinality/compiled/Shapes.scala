package cardinality.compiled

import cardinality.analysis.BuiltinTypes
import cardinality.analysis.inhabitation.Inhabitation.{Head, Shape}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Modifiers.OpenLevel
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

/** Why a type gives no shape: no slice reads it yet, or the counting model refuses it. */
private[compiled] enum Unread {
  case Uncovered(what: String)
  case Refused(reason: String)
}

/** Types as the method side reads them (slice S1; docs/plans/compiled-own-types.md, "Front end:
  * design and status"). A class the standard model holds is read by it, under its fully qualified
  * name (the compiler bound it, decision D3). A class of the module, and one of a dependency with
  * TASTy outside the standard library and the JDK (`World.readByDeclaration`; its bodies are not
  * read), is read by its declaration (v1.md §2):
  *
  *   - a case class whose one parameter list is its whole product (no written parent, no member but
  *     a secondary constructor, no restricted field or constructor) is the product of its fields;
  *   - an enum and a sealed family are the sum of their cases (`Sums`);
  *   - a trait or class with only type members is one value;
  *   - any other class or trait is a capability record (`record`): the tuple of its public fields
  *     and of what its abstract members supply;
  *   - an opaque alias is one value outside the scope its representation is visible in (rule F),
  *     which every dependency's is, the standard library's aside.
  *
  * A dependency's class without TASTy (Java, Scala 2) is read from its symbols
  * (`DependencyClasses`): the sum of its constants or children, or the product of its primary
  * constructor's parameters; a generic one and a record of abstract members are not read yet.
  *
  * A type that reaches itself and an enum's class case as a type are refused (`recursive type not
  * read`, `enum case type not read`). What the reading does not cover yet stays `Uncovered`, each
  * by its reason.
  */
final private[compiled] class Shapes(world: World)(using Context) {
  import Shapes.Env
  import Unread.{Refused, Uncovered}

  private val sums = new Sums(world, this)

  /** The shape of `tpe` read from `env`'s site, its type parameters bound as `env` says. */
  def of(tpe: Type, env: Env): Either[Unread, Shape] = tpe.dealias match {
    case by: ByNameType => of(by.resultType, env)
    // `Nothing` is a type of its own in the tree, with no class to name.
    case _: NothingType              => standard("scala.Nothing", Nil).getOrElse(uncovered(tpe))
    case repeated: RepeatedType      => of(repeated.elemType, env).map(Shape.Repeated(_))
    case World.Named(ref, arguments) => applied(arguments).flatMap(named(ref, _, env))
    case other                       => uncovered(other)
  }

  private def applied(arguments: List[TypeOrWildcard]): Either[Unread, List[Type]] =
    World.proper(arguments).left.map(Uncovered(_))

  // A type as an argument of an uninterpreted application: its identity, not its values. A
  // binder is its atom, its constructor or an application of one; one standing for a written
  // constructor is the type written. A class or an opaque alias is its name over its arguments'
  // keys (an alias is followed first: `Id[A]` is `A`). A binder bound to a value's shape, and any
  // other form, is not read.
  private def key(tpe: Type, env: Env): Either[Unread, Shape] = tpe.dealias match {
    case World.Named(ref, arguments) =>
      applied(arguments).flatMap { proper =>
        ref.optSymbol match {
          case Some(binder) if Shapes.binds(ref, binder, env) =>
            (env.binders(binder), proper) match {
              // A written constructor used whole is the type written, in its own scope.
              case (Shape.Constructor(id), Nil) if closures.contains(id) =>
                val (written, outer) = closures(id)
                key(written, outer)
              // A binder's own atom or constructor, or an application of one, is its identity. A
              // class's parameter bound to another shape holds the values of its argument, and
              // two types with one shape are two types: not read as a key.
              case (
                    shape @ (Shape.Atom(_) | Shape.Constructor(_) |
                    Shape.Applied(Head.Rigid(_), _)),
                    _
                  ) =>
                bound(ref, shape, proper, env, key)
              case _ =>
                Left(
                  Uncovered(s"a class's parameter bound to a value, as a type: ${ref.showBasic}")
                )
            }
          // An opaque alias seen inside its scope is its representation (rule F).
          case Some(alias: TypeMemberSymbol) if World.owners(env.site).contains(alias.owner) =>
            alias.typeDef match {
              case TypeMemberDefinition.OpaqueTypeAlias(_, body) => key(body.appliedTo(proper), env)
              case _                                             => uncovered(tpe)
            }
          // A static class or opaque alias is the same type at every use; one reached through a
          // value's path, and an abstract type member, may differ per path: not read.
          case Some(named @ (_: ClassSymbol | _: TypeMemberSymbol))
              if World.inObjects(named) && (named match {
                case member: TypeMemberSymbol => member.isOpaqueTypeAlias
                case _                        => true
              }) =>
            Traversals
              .sequence(proper.map(key(_, env)))
              .map(Shape.Applied(Head.Nominal(named.displayFullName), _))
          case _ => uncovered(tpe)
        }
      }
    case other => uncovered(other)
  }

  private def uncovered(tpe: Type): Either[Unread, Shape] =
    Left(Uncovered(s"a type: ${tpe.showBasic}"))

  // What the standard model says of a name it holds: a refusal of its own is for good, not left to
  // a later slice.
  private def standard(name: String, arguments: List[Shape]): Option[Either[Unread, Shape]] =
    BuiltinTypes.known(name, arguments).map(_.left.map(Refused(_)))

  private def named(ref: TypeRef, arguments: List[Type], env: Env): Either[Unread, Shape] =
    ref.optSymbol match {
      case Some(binder) if Shapes.binds(ref, binder, env) =>
        bound(ref, env.binders(binder), arguments, env, of)
      // An evidence type compares its arguments as types, and two types of the program with one
      // shape are two types: their shapes are not read there.
      case Some(symbol) if env.nominal && world.shaped(symbol) =>
        Left(Uncovered(s"a type of the module in an evidence type: ${symbol.displayFullName}"))
      case Some(c: ClassSymbol) =>
        // A context function is a function: only implicit search tells its argument apart.
        val library =
          world.library(c).map(_.replaceFirst("^scala\\.ContextFunction", "scala.Function"))
        val inside = if (library.exists(Shapes.Evidence)) env.copy(nominal = true) else env
        val higher = c.typeParams.map(_.declaredBounds.high.isInstanceOf[TypeLambda])
        val shapes =
          arguments.zipWithIndex.map((a, i) => argument(a, higher.lift(i).contains(true), inside))
        Traversals.sequence(shapes).flatMap { read =>
          library.flatMap(standard(_, read)) match {
            case Some(known)                        => known
            case None if world.readByDeclaration(c) => declared(c, read, env)
            case None if World.standard(c)          =>
              Left(Uncovered(s"a class of a dependency: ${c.displayFullName}"))
            case None => withoutTasty(c, env)
          }
        }
      case Some(alias: TypeMemberSymbol) =>
        alias.typeDef match {
          case TypeMemberDefinition.OpaqueTypeAlias(_, body) => opaque(alias, body, arguments, env)
          case _                                             => uncovered(ref)
        }
      case _ => uncovered(ref)
    }

  // The type constructors written as type arguments (`Functor[[X] =>> Either[E, X]]`,
  // `Functor[List]`), each kept with the environment it was read in, by the token that stands for
  // it. A token is never a key: a key reads the written type instead.
  private val closures = scala.collection.mutable.Map.empty[String, (Type, Env)]

  // A class's type argument is read as a type's shape. For a parameter that takes a type
  // constructor, it is that constructor instead: a binder's own if it reads as one, otherwise the
  // written type kept as a closure.
  private def argument(tpe: Type, higher: Boolean, env: Env): Either[Unread, Shape] =
    of(tpe, env) match {
      case read @ Right(Shape.Constructor(_)) => read
      case read if !higher                    => read
      case _                                  =>
        val token = s"closure:${closures.size}"
        closures(token) = (tpe, env)
        Right(Shape.Constructor(token))
    }

  // A binder applied to arguments: what it stands for, read as a value (`of`) or as a type (`key`).
  private def bound(
      ref: TypeRef,
      shape: Shape,
      arguments: List[Type],
      env: Env,
      read: (Type, Env) => Either[Unread, Shape]
  ): Either[Unread, Shape] =
    (shape, arguments) match {
      case (_, Nil) => Right(shape)
      // A binder that stands for a written constructor (`[X] =>> Either[E, X]`, `List`) is that
      // constructor applied to the arguments, read with the binders of both the site that wrote
      // it and the site that applies it. One symbol bound differently at the two sites (a class's
      // parameter passed back to the class) would take the applying site's meaning: refused.
      case (Shape.Constructor(id), applied) if closures.contains(id) =>
        val (written, outer) = closures(id)
        val rebound =
          outer.binders.exists((symbol, meaning) => env.binders.get(symbol).exists(_ != meaning))
        if (rebound)
          Left(Uncovered(s"a written constructor over a parameter bound again: ${ref.showBasic}"))
        else read(written.appliedTo(applied), env.copy(binders = outer.binders ++ env.binders))
      // A higher-kinded binder applied is an uninterpreted application over its arguments,
      // each read as the type it is (`key`), not as its values.
      case (Shape.Constructor(id), applied) =>
        Traversals.sequence(applied.map(key(_, env))).map(Shape.rigid(id, _))
      case _ =>
        Left(
          Uncovered(s"an applied type parameter not read as a constructor: ${ref.showBasic}")
        )
    }

  // Rule F: outside the scope its representation is visible in (its owner), an opaque alias is one
  // value; inside, it is its representation. No measured site is inside a dependency's scope.
  // The standard library's are left to their own readings (a named tuple is its fields).
  private def opaque(
      alias: TypeMemberSymbol,
      body: Type,
      arguments: List[Type],
      env: Env
  ): Either[Unread, Shape] =
    if (World.standard(alias))
      Left(Uncovered(s"an opaque type of the standard library: ${alias.displayFullName}"))
    else if (!World.owners(env.site).contains(alias.owner)) Right(Shape.Product(Nil))
    // The compiler refuses an alias that refers to itself: a cycle passes through a class.
    else of(body.appliedTo(arguments), env)

  // A dependency's class without TASTy (Java, Scala 2), read from its symbols: a sum of its
  // constants or its children, the product of its primary constructor's parameters. A generic
  // one, a record of abstract members and what the symbols do not say are not read yet.
  private def withoutTasty(c: ClassSymbol, env: Env): Either[Unread, Shape] = {
    import DependencyClasses.Declared
    def field(symbol: TermSymbol) = symbol.declaredType match {
      case tpe: Type => of(tpe, env.copy(visiting = env.visiting + c))
      case _         => Left(Uncovered(s"a parameter of a method type: ${symbol.name}"))
    }
    if (env.visiting(c)) recursive(c)
    else if (c.typeParams.nonEmpty)
      Left(Uncovered(s"a generic class without TASTy: ${c.displayFullName}"))
    else
      world.withoutTasty(c).left.map(why => Uncovered(s"a class of a dependency: $why")).flatMap {
        case Declared.Sum(cases) =>
          Traversals
            .sequence(cases.map {
              case child: ClassSymbol if !child.isModuleClass =>
                withoutTasty(child, env.copy(visiting = env.visiting + c))
              case _ => Right(Shape.Product(Nil))
            })
            .map(Shape.Sum(_))
        case Declared.Fields(_, fields) =>
          Traversals.sequence(fields.map(field)).map(Shape.Product(_))
        case Declared.Record(_, _) =>
          Left(
            Uncovered(
              s"a class that is no product, sum or type-only template: ${c.displayFullName}"
            )
          )
      }
  }

  private def recursive(c: ClassSymbol): Either[Unread, Shape] =
    Left(Refused(s"recursive type not read: ${c.displayFullName}"))

  // ---- classes of the module ----------------------------------------------------------------

  private def declared(c: ClassSymbol, arguments: List[Shape], env: Env): Either[Unread, Shape] =
    Shapes.tree(c).flatMap { tree =>
      val inside = env.copy(visiting = env.visiting + c)
      // A member of a class nested in a class or a body may read what is around it.
      if (!World.inObjects(c))
        Left(Uncovered(s"a class nested in a class or a body: ${c.displayFullName}"))
      else if (env.visiting(c)) recursive(c)
      else if (c.typeParams.sizeIs != arguments.size)
        Left(Uncovered(s"a type constructor: ${c.displayFullName}"))
      else if (c.isEnum && c.isCaseClass)
        Left(Refused(s"enum case type not read: ${c.displayFullName}"))
      else if (c.isEnum || Shapes.isFamily(c)) sums.of(tree, arguments, inside)
      else if (typesOnly(tree)) Right(Shape.Product(Nil))
      else if (plainCase(tree)) fields(tree, arguments, inside).map(Shape.Product(_))
      else record(tree, arguments, inside)
    }

  // A capability record: a class or trait with members, named by a type (v1.md §2). Every
  // constructor parameter must be a public field, which a body may read. A trait or abstract
  // class is any implementation of its abstract members: its value is the tuple of its fields and
  // of what those members supply (`member`). A concrete class nothing extends is final: its value
  // is the tuple of its fields, from which every member computes. A template whose construction
  // always throws, or a class written `final abstract`, has no value at all: the empty sum, as the
  // old front end reads a record (a sealed case's construction that throws is S6's still).
  // Left for later steps, each named: parents (what they declare, the index's overriders),
  // overriders tasty-query does not read, a construction that binds a pattern, an abstract member
  // hidden at the use site, a sealed class, a self type narrower than the class, an abstract type member, a concrete
  // member something overrides, a member named like the measured definition (it may be that one
  // on another receiver), a restricted constructor and a parameter that is no public field (rule
  // B), a class something extends, and what `member` leaves.
  private def record(template: ClassDef, bound: List[Shape], env: Env): Either[Unread, Shape] = {
    val (c, name) = (template.symbol, template.symbol.displayFullName)
    val parameters = Shapes.parameters(template.rhs.constr)
    val fieldNames = template.rhs.body.collect {
      case v: ValDef if v.symbol.isParamAccessor && v.symbol.isPublic => v.name
    }.toSet
    val members = template.rhs.body.collect { case member: ValOrDefDef => member }
    val supplied = members.filter { member =>
      Receivers.open(member) && !member.symbol.isSynthetic &&
      !member.name.toString.endsWith("_=") &&
      // A member restating one of `Any`'s is outside the model.
      member.symbol.allOverriddenSymbols.isEmpty && !universal(member)
    }
    // How a class of the program may override each concrete member (the closed world).
    val overriding = members.collect {
      case member if !member.symbol.isAbstractMember => world.overriding(c, member.symbol)
    }
    // A member named like the measured definition, which may be that one on another receiver; a
    // constructor row measures no member.
    val measured = env.site.name.toString
    val clash = !World.isConstructor(env.site) && members.exists { member =>
      !member.symbol.isSynthetic && member.name.toString == measured
    }
    def refused(left: Boolean, what: String) =
      Either.cond(!left, (), Uncovered(s"a capability record $what: $name"))
    val read = env.copy(binders =
      env.binders ++
        (Shapes.typeParameters(template.rhs.constr) ++ c.typeParams).zip(bound ++ bound).toMap
    )
    // A template whose construction always throws, or a class written `final abstract`, has no
    // value: the empty sum.
    if ((Shapes.isFinal(c) && c.isAbstractClass) || Bodies.alwaysThrows(template))
      Right(Shape.Sum(Nil))
    else
      for {
        _ <- refused(World.ancestors(c).nonEmpty, "with parents")
        _ <- refused(
          overriding.contains(ClosedProgram.Overriding.Unknown),
          "whose overriders are not read"
        )
        _ <- refused(world.unbuilt(template), "whose construction binds a pattern")
        _ <- refused(supplied.exists(!_.symbol.isPublic), "with a member hidden at the use site")
        _ <- refused(c.openLevel == OpenLevel.Sealed, "of a sealed class")
        _ <- refused(!World.ownSelf(c), "with a self type")
        _ <- refused(Shapes.abstractTypeMember(template), "with an abstract type member")
        _ <- refused(
          overriding.contains(ClosedProgram.Overriding.Supplied),
          "with a member something overrides"
        )
        _ <- refused(clash, "with a member named like the measured definition")
        _ <- refused(!template.rhs.constr.symbol.isPublic, "with a restricted constructor")
        _ <- refused(
          !parameters.forall(p => fieldNames(p.name)),
          "with a parameter that is no public field"
        )
        _ <- refused(
          !c.isTrait && !c.isAbstractClass && !Shapes.isFinal(c) && world.extended(c),
          "of a class something extends"
        )
        fields <- inputs(parameters, read)
        held <- Traversals.sequence(supplied.map(member(_, "a capability record", read)))
      } yield Shape.Product(fields ++ held.flatten)
  }

  // Whether a member restates one of `Any`'s (`toString`, `hashCode`, `equals(that: Any)`), which
  // the model excludes: by its name and parameters, since a trait with no parent overrides
  // nothing. Any other member counts, an overload of such a name and `canEqual` included.
  private def universal(member: ValOrDefDef): Boolean = member match {
    case method: DefDef =>
      val taken = Shapes.parameters(method).map(_.tpt.toType)
      method.name.toString match {
        case "toString" | "hashCode" => taken.isEmpty
        case "equals"                => taken.sizeIs == 1 && taken.head.isSameType(defn.AnyType)
        case _                       => false
      }
    case _: ValDef => false
  }

  /** A class of the module as the receiver of a member written in it: its type parameters are the
    * atoms `env` binds them to.
    */
  def receiver(c: ClassSymbol, env: Env): Either[Unread, Shape] =
    Traversals
      .sequence(c.typeParams.map { binder =>
        env.binders.get(binder).toRight(Uncovered(s"a type parameter not bound: ${binder.name}"))
      })
      .flatMap(declared(c, _, env))

  /** What a member a subclass supplies gives a body that holds the instance (`where` names that
    * instance in a reason): a value of its type, or one function from all of a method's parameters
    * to its result; an abstract given with a `using` clause is such a method (the old front end
    * read it as a value of its result type: a listed correction). A one-valued result (`Unit`, an
    * opaque type outside its scope) supplies nothing: every body builds it without the member.
    */
  def member(member: ValOrDefDef, where: String, env: Env): Either[Unread, Option[Shape]] = {
    val symbol = member.symbol
    val inlineSite = env.site match {
      case term: TermSymbol => term.isInline
      case _                => false
    }
    // An extension method is no member of its receiver's instances, and a given without a name is
    // found by search alone: neither is read as a capability yet.
    val unread =
      if (symbol.isExtensionMethod) Left("an extension method")
      else if (Readings.contextual(symbol) && member.name.toString.startsWith("given_"))
        Left("an anonymous given")
      else Readings.later(symbol)
    (unread, member) match {
      case (Left(what), _) => Left(Uncovered(s"$what among the members of $where: ${member.name}"))
      // Each implementation of a deferred `inline val` is a constant of its own.
      case (_, value: ValDef) if symbol.isInline =>
        Left(Uncovered(s"a deferred inline value of $where: ${value.name}"))
      // A deferred inline method is called from inline code alone: the compiler rejects a call of
      // it from any other body, to which it therefore supplies nothing. One over a type parameter
      // may have no implementation at some instance, which says something of the instance: not
      // read. (Its signature may name one through an alias too, so it is not searched.) The
      // templates around it, and no measured site, have the type parameters its signature may name, and
      // the environment the abstract type members.
      case (_, method: DefDef) if symbol.isInline && !inlineSite =>
        Either.cond(
          Shapes.typeParameters(method).isEmpty && World.owners(symbol.owner).forall {
            case owner: ClassSymbol => owner.typeParams.isEmpty
            case _: PackageSymbol   => true
            case _                  => false
          } && !env.binders.keys.exists(_.isInstanceOf[TypeMemberSymbol]),
          None,
          Uncovered(s"a deferred inline method over a type parameter: ${method.name}")
        )
      case (_, value: ValDef)  => of(value.tpt.toType, env).map(Some(_))
      case (_, method: DefDef) =>
        val parameters = Shapes.parameters(method)
        if (Shapes.typeParameters(method).nonEmpty)
          Left(Uncovered(s"a capability with type parameters of its own: ${method.name}"))
        else
          for {
            _ <- Readings.plainInputs(parameters)
            result <- of(method.resultTpt.toType, env)
            read <- inputs(parameters, env)
          } yield Option.when(result != Shape.Product(Nil))(Shape.Function(read, result))
    }
  }

  /** A class's constructor parameters, read with its type parameters bound to `arguments`: the
    * constructor's own copies of them, which its parameter types name, and the class's.
    */
  def inputs(parameters: List[ValDef], env: Env): Either[Unread, List[Shape]] =
    Traversals.sequence(parameters.map(parameter => of(parameter.tpt.toType, env)))

  def fields(tree: ClassDef, arguments: List[Shape], env: Env): Either[Unread, List[Shape]] = {
    val constructor = tree.rhs.constr
    val copies = Shapes.typeParameters(constructor)
    val bound = (copies.zip(arguments) ++ tree.symbol.typeParams.zip(arguments)).toMap
    Traversals.sequence(
      Shapes
        .parameters(constructor)
        .map(p => of(p.tpt.toType, env.copy(binders = env.binders ++ bound)))
    )
  }

  private def plainCase(tree: ClassDef): Boolean =
    tree.symbol.isCaseClass && !tree.symbol.isAbstractClass && Shapes.bare(tree) &&
      tree.rhs.constr.paramLists.count(_.isLeft) == 1 &&
      tree.rhs.body.forall {
        case v: ValDef    => v.symbol.isParamAccessor && v.symbol.isPublic
        case d: DefDef    => d.symbol.isSynthetic || World.isConstructor(d.symbol)
        case _: TypeParam => true
        case _            => false
      }

  // A trait or plain class whose only members are types: no observation tells two of its values
  // apart, and it has a value where each abstract member can be given a type (one bound at most).
  private def typesOnly(tree: ClassDef): Boolean = {
    val c = tree.symbol
    c.openLevel != OpenLevel.Sealed && !c.isCaseClass && Shapes.bare(tree) &&
    Shapes
      .parameters(tree.rhs.constr)
      .isEmpty && World.ownSelf(c) && !(Shapes.isFinal(c) && c.isAbstractClass) &&
    tree.rhs.body.forall {
      case member: TypeMember =>
        member.symbol.typeDef match {
          case TypeMemberDefinition.AbstractType(bounds) =>
            bounds.low.isSameType(defn.NothingType) || bounds.high.isSameType(defn.AnyType)
          case _: TypeMemberDefinition.TypeAlias => true
          case _                                 => false
        }
      case _: TypeParam => true
      case _            => false
    }
  }

}

private[compiled] object Shapes {

  /** What a type is read in: the shapes its type parameters stand for, the definition measured
    * (what it may see decides an opaque type), the classes being read (a cycle is refused), and
    * whether the type is compared with another as a type (`nominal`), where a type of the module is
    * not its shape.
    */
  final case class Env(
      binders: Map[Symbol, Shape],
      site: Symbol,
      visiting: Set[ClassSymbol] = Set.empty,
      nominal: Boolean = false
  )

  // The types whose arguments are compared as types.
  private val Evidence = Set("scala.<:<", "scala.=:=")

  /** What a type parameter of a measured definition or a receiver stands for, its identity being
    * its owner's name and its own: one atom where any type instantiates it; a rigid constructor
    * where any invariant type constructor of proper parameters does (`F[_]`), applied as an
    * uninterpreted head (`key`). A bound, a variance, or a constructor of constructors is not read
    * yet.
    */
  def binder(owner: Symbol, parameter: TypeParamSymbol, copies: List[Symbol] = Nil)(using
      Context
  ): Either[Unread, Shape] = {
    val bounds = parameter.declaredBounds
    val id = s"${owner.displayFullName}[${parameter.name}]"
    if (unbounded(bounds)) Right(Shape.Atom(id))
    // A class's parameter keeps its variance on the constructor's copy of it.
    else if (constructor(parameter) && !copies.exists(Shapes.variant)) Right(Shape.Constructor(id))
    else
      Left(
        Unread.Uncovered(
          s"a bounded type parameter, or one of another kind than F[_]: ${parameter.name}"
        )
      )
  }

  /** What an abstract type member of a receiver stands for on its instance, as a type parameter
    * would (`binder`): one atom, or a rigid constructor for `F[_]`. A bound is not read yet.
    */
  def typeMember(owner: ClassSymbol, member: TypeMemberSymbol)(using
      Context
  ): Either[Unread, Shape] = {
    val id = s"${owner.displayFullName}#${member.name}"
    member.typeDef match {
      case TypeMemberDefinition.AbstractType(bounds) if unbounded(bounds)   => Right(Shape.Atom(id))
      case TypeMemberDefinition.AbstractType(bounds) if constructor(bounds) =>
        Right(Shape.Constructor(id))
      case _ =>
        Left(
          Unread.Uncovered(
            s"a bounded abstract type member, or one of another kind than F[_]: ${member.name}"
          )
        )
    }
  }

  // Whether a reference names what the environment binds: a type parameter, or an abstract type
  // member on the instance of the receiver that holds it (`this.T`, `typeMember`'s identity), never
  // one reached through another path or another instance.
  private def binds(ref: TypeRef, binder: Symbol, env: Env)(using Context): Boolean =
    env.binders.get(binder).exists { shape =>
      (binder, ref.prefix) match {
        case (_: TypeMemberSymbol, self: ThisType) =>
          val id = s"${self.cls.displayFullName}#${binder.name}"
          shape == Shape.Atom(id) || shape == Shape.Constructor(id)
        case (_: TypeMemberSymbol, _) => false
        case _                        => true
      }
    }

  private def constructor(parameter: TypeParamSymbol)(using Context): Boolean =
    constructor(parameter.declaredBounds) && !Shapes.variant(parameter)

  // Bounds any type constructor of proper, unbounded parameters meets: `[X] =>> Any`
  // above, nothing below (written as a lambda too, on a method's own parameter).
  private def constructor(bounds: TypeBounds)(using Context): Boolean = {
    val bottom = bounds.low match {
      case lambda: TypeLambda => lambda.resultType.isSameType(defn.NothingType)
      case low                => low.isSameType(defn.NothingType)
    }
    bottom && (bounds.high match {
      case lambda: TypeLambda =>
        lambda.paramTypeBounds.forall(unbounded) && lambda.resultType.isSameType(defn.AnyType)
      case _ => false
    })
  }

  /** Whether a type parameter declares a variance on a parameter of its own (`F[+_]`). */
  def variant(parameter: Symbol): Boolean = parameter match {
    case declared: TypeParamSymbol => tastyquery.Variances.variant(declared)
    case _                         => false
  }

  /** Bounds any type meets. */
  def unbounded(bounds: TypeBounds)(using Context): Boolean =
    bounds.low.isSameType(defn.NothingType) && bounds.high.isSameType(defn.AnyType)

  /** A constructor's or method's own type parameters: a constructor's are copies of its class's.
    */
  def typeParameters(method: DefDef): List[Symbol] =
    method.paramLists.flatMap(_.toOption.toList.flatten).map(_.symbol)

  /** A constructor's or method's value parameters, clause after clause. */
  def parameters(method: DefDef): List[ValDef] =
    method.paramLists.flatMap(_.left.toOption.toList.flatten)

  /** A class written `final`: the closed world's finality is `World.extended`'s to say. */
  def isFinal(c: ClassSymbol): Boolean = c.openLevel == OpenLevel.Final

  /** A sealed trait or abstract class, which is the sum of its subtypes; an enum is read by its own
    * rule.
    */
  def isFamily(c: ClassSymbol): Boolean =
    c.openLevel == OpenLevel.Sealed && c.isAbstractClass && !c.isEnum

  /** Whether a template declares an abstract type member. */
  def abstractTypeMember(tree: ClassDef): Boolean = tree.rhs.body.exists {
    case member: TypeMember => member.symbol.isAbstractMember
    case _                  => false
  }

  /** A class's tree, or what it needs that is not read. */
  def tree(c: ClassSymbol): Either[Unread, ClassDef] = World.tree(c).left.map(Unread.Uncovered(_))

  // A class written without a parent (the compiler's own parents have no extent) whose
  // constructor is public.
  private def bare(tree: ClassDef): Boolean =
    tree.rhs.parents.forall(_.pos.isZeroExtent) && tree.rhs.constr.symbol.isPublic

}
