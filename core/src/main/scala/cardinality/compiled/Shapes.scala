package cardinality.compiled

import cardinality.analysis.BuiltinTypes
import cardinality.analysis.inhabitation.Inhabitation.{Binder, Head, Shape}
import cardinality.analysis.inhabitation.PolymorphicMembers
import cardinality.analysis.resolution.{LiteralTypes, StuckProjections}
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
  *     and of what its abstract members supply, a package over its unbounded abstract type members
  *     (`∃X. tuple`);
  *   - an opaque alias is one value outside the scope its representation is visible in (rule F),
  *     which every dependency's is, the standard library's aside.
  *
  * A dependency's class without TASTy (Java, Scala 2) is read from its symbols
  * (`DependencyClasses`): the sum of its constants or children, the product of its primary
  * constructor's parameters, or a capability record (`Receivers.dependency`); a generic one is not
  * read yet.
  *
  * A literal type is its one constant, and a written `p.type` the one value p (`singleton`). A
  * union is the tagged sum of its alternatives where their values are told apart
  * (`Relations.union`), an intersection its narrowest part (`Relations.intersection`), a match type
  * what the compiler reduces it to or a stuck projection's atom (`matchType`).
  *
  * A type that reaches itself and an enum's class case as a type are refused (`recursive type not
  * read`, `enum case type not read`). What the reading does not cover yet stays `Uncovered`, each
  * by its reason.
  */
final private[compiled] class Shapes(world: World, receivers: => Receivers)(using Context) {
  import Shapes.Env
  import Unread.{Refused, Uncovered}

  private val sums = new Sums(world, this)
  private val relations = new Relations(world, of)

  /** The shape of `tpe` read from `env`'s site, its type parameters bound as `env` says. */
  // A value `preceding` is the one a class written as the whole type names (the old front end
  // anchors it at that type): what it is read through or holds is not.
  def of(tpe: Type, env: Env): Either[Unread, Shape] = {
    val parts = env.copy(preceding = false)
    tpe.dealias match {
      case by: ByNameType => of(by.resultType, parts)
      // `Nothing` is a type of its own in the tree, with no class to name.
      case _: NothingType              => standard("scala.Nothing", Nil).getOrElse(uncovered(tpe))
      case repeated: RepeatedType      => of(repeated.elemType, parts).map(Shape.Repeated(_))
      case World.Named(ref, arguments) => applied(arguments).flatMap(named(ref, _, env))
      case constant: ConstantType      => literal(constant).map(Shape.Singleton(_, None))
      case path: TermRef               => singleton(path, parts)
      case or: OrType                  => relations.union(Relations.alternatives(or), parts)
      case and: AndType                => relations.intersection(Relations.conjuncts(and), parts)
      case matched: MatchType          => matchType(matched, parts)
      case other                       => uncovered(other)
    }
  }

  // A match type is what the compiler reduces it to; one stuck on an unbounded type parameter under
  // a single tuple projection (`A match { case (f, s) => f }`), without a bound, is the projection's
  // atom (`StuckProjections`, members MT3), and `Readings` refuses arities no instantiation meets.
  private def matchType(matched: MatchType, env: Env): Either[Unread, Shape] =
    matched.reduced match {
      // tasty-query reduces on an abstract scrutinee where scalac does not (`Fst[T]` for a
      // `T <: (Int, String)` is `Int` there, stuck in scalac): trusted on a ground one only.
      case Some(reduced) if Shapes.ground(matched.scrutinee) => of(reduced, env)
      case Some(_)                                           => uncovered(matched)
      case None                                              =>
        val stuck = for {
          (arity, index) <- Shapes.projection(matched.cases)
          if matched.bound.isSameType(defn.AnyType)
          case ref: TypeRef <- Some(matched.scrutinee.dealias)
          parameter <- Shapes.rigidParameter(ref)
          if Shapes.binds(ref, parameter, env)
          id <- env.binding(parameter).toOption.collect { case Shape.Atom(id) => id }
        } yield Shape.Atom(StuckProjections.Projection(arity, index, id))
        stuck.toRight(Uncovered(s"a type: ${matched.showBasic}"))
    }

  // A literal type's one value, named as the old front end names it (`LiteralTypes`); a floating
  // zero is read too, since the compiled constant keeps its sign.
  private def literal(constant: ConstantType): Either[Unread, String] =
    LiteralTypes.identity(constant.value.value).toRight(Uncovered(s"a type: ${constant.showBasic}"))

  // A written `p.type` has the one value p (v1.md §2): its goal is terminal. A method's parameter
  // is that value of its declared type, under the identity its binding has (`Readings`). A static
  // object read by its declaration is one value only where it is written without a parent and
  // declares types alone, as the old front end reads it (the compiler's own members and parents
  // aside). Any other static path (an object or value of the standard library, an enum's value
  // case) is a value of its own name. A constructor's `val`, a local value and a path through an
  // instance are not read yet.
  private def singleton(path: TermRef, env: Env): Either[Unread, Shape] =
    path.optSymbol match {
      case Some(term: TermSymbol) if term.owner.isTerm =>
        (term.owner, term.declaredType) match {
          case (method: TermSymbol, declared: Type)
              if method.paramSymss.exists(_.left.exists(_.contains(term))) =>
            of(declared, env).map(read =>
              Shape.Singleton(s"${method.displayFullName}.${term.name}", Some(read))
            )
          case _ => uncovered(path)
        }
      case Some(term: TermSymbol) if World.inObjects(term) =>
        val typesAlone =
          term.moduleClass.filter(world.readByDeclaration).flatMap(_.tree).forall { template =>
            Shapes.parentless(template) && template.rhs.body.forall {
              case _: TypeMember       => true
              case member: ValOrDefDef => member.symbol.isSynthetic
              case _                   => false
            }
          }
        Either.cond(
          typesAlone,
          Shape.Singleton(s"path:${term.displayFullName}", None),
          Refused(s"singleton module member environment not resolved: ${term.name}")
        )
      case _ => uncovered(path)
    }

  private def applied(arguments: List[TypeOrWildcard]): Either[Unread, List[Type]] =
    World.proper(arguments).left.map(Uncovered(_))

  // A type as an argument of an uninterpreted application: its identity, not its values. A
  // binder is its atom, its constructor or an application of one; one standing for a written
  // constructor is the type written. A class or an opaque alias is its name over its arguments'
  // keys (an alias is followed first: `Id[A]` is `A`); a literal type, its constant. A binder bound
  // to a value's shape, and any other form, is not read.
  private def key(tpe: Type, env: Env): Either[Unread, Shape] = tpe.dealias match {
    case constant: ConstantType      => literal(constant).map(LiteralTypes.nominal)
    case World.Named(ref, arguments) =>
      applied(arguments).flatMap { proper =>
        ref.optSymbol match {
          case Some(binder) if Shapes.binds(ref, binder, env) =>
            env
              .binding(binder)
              .flatMap((_, proper) match {
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
              })
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
      case Some(parameter) if Shapes.binds(ref, parameter, env) =>
        env.binding(parameter).flatMap(bound(ref, _, arguments, env, of))
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
          arguments.zipWithIndex.map((a, i) =>
            argument(a, higher.lift(i).contains(true), inside.copy(preceding = false))
          )
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
        else
          read(
            written.appliedTo(applied),
            env.copy(binders = outer.binders ++ env.binders, unread = outer.unread ++ env.unread)
          )
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
  // constants or its children, the product of its primary constructor's parameters, or a record
  // of what its members supply (`Receivers.dependency`). A generic one and what the symbols do not
  // say are not read yet.
  private def withoutTasty(c: ClassSymbol, env: Env): Either[Unread, Shape] = {
    import DependencyClasses.Declared
    // What a field or member of the value holds may be built after it: only the value itself is
    // `preceding`, as in a record of the module.
    val inside = env.copy(visiting = env.visiting + c, preceding = false)
    def field(symbol: TermSymbol) = symbol.declaredType match {
      case tpe: Type => of(tpe, inside)
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
                withoutTasty(child, inside)
              case _ => Right(Shape.Product(Nil))
            })
            .map(Shape.Sum(_))
        case Declared.Fields(_, fields) =>
          Traversals.sequence(fields.map(field)).map(Shape.Product(_))
        case Declared.Record(abstracts, concrete) =>
          val refused = Shapes.unread(c.displayFullName)
          for {
            _ <- refused(!World.ownSelf(c), "with a self type")
            _ <- refused(
              Shapes.abstractType((c :: World.ancestors(c)).flatMap(_.declarations)),
              "with an abstract type member"
            )
            _ <- refused(
              Shapes.clash(c, env.site),
              "with a member named like the measured definition"
            )
            site <- Shapes.siteMember(env.site, c.displayFullName)
            // A class whose constructor the site may not call holds what its scope builds.
            _ <- refused(
              c.declarations.exists {
                case k: TermSymbol => World.isConstructor(k) && !World.callable(k, site)
                case _             => false
              },
              "with a restricted constructor"
            )
            held <- receivers.dependency(c, abstracts, concrete, site, inside)
          } yield Shape.Product(held)
      }
  }

  private def recursive(c: ClassSymbol): Either[Unread, Shape] =
    Left(Refused(Shapes.Recursive + c.displayFullName))

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

  // A capability record: a class or trait with members, named by a type (v1.md §2). A trait or
  // abstract class is any implementation of the members a subclass supplies: its value is the
  // tuple of its fields, each a public one, and of what those members supply, own or inherited,
  // read as a receiver reads them (`Receivers.record`). A concrete class nothing extends is final,
  // and rule B reads it: where the site may call its primary constructor, its value is the tuple
  // of its fields, each one the site observes as itself (`exposed`), from which every member
  // computes; where it may not, it is one of what the site may call to build one (`Factories`). A
  // template whose construction always throws, or a class written `final abstract`, has no value
  // at all: the empty sum, as the old front end reads a record (a sealed case's construction that
  // throws is S6's still). A member that may be the measured definition on another receiver is
  // refused (`Relations.clash`). Left for later steps, each named: a construction that binds a
  // pattern, a sealed class, a self type narrower than the class, a field
  // the site does not observe as itself, a restricted constructor of a class something may extend,
  // a class something extends, and what `Receivers.record` and `Factories` leave.
  private def record(template: ClassDef, bound: List[Shape], env: Env): Either[Unread, Shape] = {
    val (c, name) = (template.symbol, template.symbol.displayFullName)
    val parameters = Shapes.parameters(template.rhs.constr)
    val fieldNames = template.rhs.body.collect {
      case v: ValDef if v.symbol.isParamAccessor && v.symbol.isPublic => v.name
    }.toSet
    val fixed = !c.isTrait && !c.isAbstractClass && (Shapes.isFinal(c) || !world.extended(c))
    val clash = relations.clash(c, env)
    val refused = Shapes.unread(name)
    // What a member of the value returns may be built after it, the receiver among them: only
    // the value itself is `preceding`.
    val read = env.copy(
      binders = env.binders ++
        (Shapes.typeParameters(template.rhs.constr) ++ c.typeParams).zip(bound ++ bound).toMap,
      preceding = false
    )
    // A template whose construction always throws, or a class written `final abstract`, has no
    // value: the empty sum.
    if ((Shapes.isFinal(c) && c.isAbstractClass) || Bodies.alwaysThrows(template))
      Right(Shape.Sum(Nil))
    else
      for {
        _ <- refused(world.unbuilt(template), "whose construction binds a pattern")
        _ <- refused(c.openLevel == OpenLevel.Sealed, "of a sealed class")
        _ <- refused(!World.ownSelf(c), "with a self type")
        _ <- Either.cond(
          !clash,
          (),
          Refused(
            s"capability record member may be the measured declaration on its receiver: $name.${env.site.name}"
          )
        )
        _ <- refused(!c.isTrait && !c.isAbstractClass && !fixed, "of a class something extends")
        site <- Shapes.siteMember(env.site, name)
        public = World.callable(template.rhs.constr.symbol, site)
        _ <- refused(!fixed && !public, "with a restricted constructor")
        _ <- refused(
          public &&
            !parameters.forall(p => fieldNames(p.name) || fixed && exposed(template, p, site)),
          "with a parameter that is no public field"
        )
        // A final class no constructor callable from the site builds is one of what the site may
        // call to build one, over the fields it observes (rule B).
        shape <-
          if (fixed && !public)
            factories.space(template, site, read).map(Shape.Ground(name, _))
          else receivers.record(template, site, read)
      } yield shape
  }

  /** A class of the module as the receiver of a member written in it: its type parameters are the
    * atoms `env` binds them to.
    */
  def receiver(c: ClassSymbol, env: Env): Either[Unread, Shape] =
    Traversals
      .sequence(c.typeParams.map { binder =>
        if (env.binds(binder)) env.binding(binder)
        else Left(Uncovered(s"a type parameter not bound: ${binder.name}"))
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
        for {
          _ <- Readings.plainInputs(parameters)
          own <- polymorphic(symbol.displayFullName, method)
          inside = env.copy(binders = env.binders ++ own.map((p, shape, _) => p -> shape))
          result <- of(method.resultTpt.toType, inside)
          read <- inputs(parameters, inside)
          // A member's own type parameters are read as the scalameta front end reads them
          // (`PolymorphicMembers.close`): exact where a reading is, kept for the solver otherwise.
          closed <- PolymorphicMembers
            .close(symbol.displayFullName, own.map(_._3), Shape.Function(read, result))
            .left
            .map(Uncovered(_))
        } yield Option.when(result != Shape.Product(Nil))(closed)
    }
  }

  // A capability's own type parameters, each under the identity `PolymorphicMembers.close` looks
  // for: the member's own atom, or a rigid constructor for an `F[_]`, with the bounds it declares.
  private def polymorphic(member: String, method: DefDef) =
    Traversals.sequence(Shapes.typeParameters(method).map { symbol =>
      val parameter = symbol.asInstanceOf[TypeParamSymbol]
      val (name, bounds) = (parameter.name.toString, parameter.declaredBounds)
      // The old front end reads any constructor here as rigid (`MemberSyntax`): a bound or a
      // variance it does not refuse is not read yet.
      val read = Shapes.binder(method.symbol, parameter).left.map {
        case Unread.Refused(_) =>
          Uncovered(s"a type parameter of another kind than an invariant F[_]: $name")
        case uncovered => uncovered
      }
      read.map { read =>
        // A rigid constructor's bounds are a type lambda's, which `Shapes.binder` checked.
        val proper = !read.isInstanceOf[Shape.Constructor]
        val declared = PolymorphicMembers.Declared(
          name,
          lower = proper && !bounds.low.isSameType(defn.NothingType),
          upper = proper && !bounds.high.isSameType(defn.AnyType),
          view = false
        )
        val id = Binder(member, name).id
        (symbol, if (proper) Shape.Atom(id) else Shape.Constructor(id), declared)
      }
    })

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

  // Whether the site observes a final class's constructor parameter as itself: a field it may
  // select on a value it is given, a member it may call without arguments whose body is that
  // field (an alias), or, for a field it may select on `this` alone (a receiver is read by
  // `Receivers.own`), one a member it may call reads (rule B's observed fields, `Factories`). A
  // constructor's position of its own class keeps every such field: the row reads it with the
  // stored side's estimate, whose value holds every parameter (`Constructors`).
  private def exposed(template: ClassDef, parameter: ValDef, site: TermSymbol): Boolean =
    template.symbol.declarations
      .collectFirst {
        case field: TermSymbol if field.isParamAccessor && field.name == parameter.name => field
      }
      .exists { field =>
        val own = World.isConstructor(site) && site.owner == template.symbol
        World.selectable(field, site) || World.callable(field, site) &&
        (own || factories.observed(template, site).contains(parameter)) ||
        template.rhs.body.exists {
          case alias: ValOrDefDef
              if alias.symbol != field && !alias.symbol.isParamAccessor &&
                !alias.symbol.isSynthetic && World.selectable(alias.symbol, site) &&
                World.declared(alias).isSameType(parameter.tpt.toType) && (alias match {
                  case d: DefDef => d.paramLists.isEmpty
                  case _: ValDef => true
                }) =>
            World.rhs(alias).exists(Shapes.names(_, field))
          case _ => false
        }
      }

  private lazy val factories = new Factories(world, this)

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

  /** How the method side refuses a class it is reading again. */
  val Recursive = "recursive type not read: "

  /** What a type is read in: the shapes its type parameters stand for, the definition measured
    * (what it may see decides an opaque type), the classes being read (a cycle is refused), and
    * whether the type is compared with another as a type (`nominal`), where a type of the module is
    * not its shape. A type parameter `unread` is the reason of a type that names it, and of nothing
    * else: an unused parameter's kind tells nothing. A value `preceding` is built before the
    * measured definition's receiver, which it therefore never is.
    */
  final case class Env(
      binders: Map[Symbol, Shape],
      site: Symbol,
      visiting: Set[ClassSymbol] = Set.empty,
      nominal: Boolean = false,
      unread: Map[Symbol, Unread] = Map.empty,
      preceding: Boolean = false
  ) {

    /** Whether a type parameter is one of the environment's, read or not. */
    def binds(parameter: Symbol): Boolean =
      binders.contains(parameter) || unread.contains(parameter)

    /** What a type parameter of the environment stands for, or why it is not read. */
    def binding(parameter: Symbol): Either[Unread, Shape] =
      unread.get(parameter).toLeft(binders(parameter))

  }

  object Env {

    /** An environment of type parameters each read or not (`Shapes.binder`). */
    def of(read: Iterable[(Symbol, Either[Unread, Shape])], site: Symbol): Env =
      Env(
        read.collect { case (symbol, Right(shape)) => symbol -> shape }.toMap,
        site,
        unread = read.collect { case (symbol, Left(why)) => symbol -> why }.toMap
      )

  }

  // The types whose arguments are compared as types.
  private val Evidence = Set("scala.<:<", "scala.=:=")

  /** What a type parameter of a measured definition or a receiver stands for, its identity being
    * its owner's name and its own: one atom where any type instantiates it; a rigid constructor
    * where any invariant type constructor of proper parameters does (`F[_]`), applied as an
    * uninterpreted head (`key`). A proper parameter with bounds is an atom too, its bounds being
    * proofs beside it (`Bounds`). A constructor with a bound, or with a variance on a parameter of
    * its own, is refused, as the old front end refuses it (`Kinds.refusal` of `types`; its view
    * bound is no Scala 3); one declared variant itself, or a constructor of constructors, is not
    * read yet.
    */
  def binder(owner: Symbol, parameter: TypeParamSymbol, copies: List[Symbol] = Nil)(using
      Context
  ): Either[Unread, Shape] = {
    val bounds = parameter.declaredBounds
    val id = s"${owner.displayFullName}[${parameter.name}]"
    def refused(why: String) = Left(Unread.Refused(s"$why: ${parameter.name}"))
    if (proper(bounds)) Right(Shape.Atom(id))
    else if (!plain(bounds))
      refused("bounded higher-kinded parameter requires its bound's capabilities")
    // A class's parameter keeps its variance on the constructor's copy of it.
    else if ((parameter :: copies).exists(Shapes.variant))
      refused("variant higher-kinded parameter requires subtyping between applications")
    else if (constructor(parameter)) Right(Shape.Constructor(id))
    else
      Left(
        Unread.Uncovered(
          s"a type parameter of another kind than an invariant F[_]: ${parameter.name}"
        )
      )
  }

  // Bounds that say nothing: `Nothing` below and `Any` above, or for a constructor a type lambda
  // over parameters whose bounds say nothing in turn. Read from the bounds, not from what is
  // written: `F[_] <: Any`, which the old front end refused, says nothing.
  private def plain(bounds: TypeBounds)(using Context): Boolean = {
    def top(tpe: Type): Boolean = tpe match {
      case lambda: TypeLambda => lambda.paramTypeBounds.forall(plain) && top(lambda.resultType)
      case other              => other.isSameType(defn.AnyType)
    }
    def bottom(tpe: Type): Boolean = tpe match {
      case lambda: TypeLambda => bottom(lambda.resultType)
      case other              => other.isSameType(defn.NothingType)
    }
    top(bounds.high) && bottom(bounds.low)
  }

  /** What an abstract type member of a receiver stands for on its instance, as a type parameter
    * would (`binder`): one atom, bounded or not (`Bounds` reads a bound's proofs), or a rigid
    * constructor for an invariant, unbounded `F[_]`.
    */
  def typeMember(owner: ClassSymbol, member: TypeMemberSymbol)(using
      Context
  ): Either[Unread, Shape] = {
    val id = s"${owner.displayFullName}#${member.name}"
    member.typeDef match {
      // Bounds that meet name one type (an exported alias): not an atom.
      case TypeMemberDefinition.AbstractType(bounds)
          if proper(bounds) && !bounds.low.isSameType(bounds.high) =>
        Right(Shape.Atom(id))
      // A parameter declared `+X` or `-X` relates the member's applications by subtyping.
      case TypeMemberDefinition.AbstractType(bounds) if free(member) && !unbounded(bounds) =>
        Right(Shape.Constructor(id))
      case _ =>
        Left(
          Unread.Uncovered(
            s"an abstract type member that is an alias, variant, or of another kind: ${member.name}"
          )
        )
    }
  }

  // Whether a reference names what the environment binds: a type parameter, or an abstract type
  // member on the instance of the receiver that holds it (`this.T`, `typeMember`'s identity), never
  // one reached through another path or another instance.
  private def binds(ref: TypeRef, binder: Symbol, env: Env)(using Context): Boolean =
    (env.unread.contains(binder) && !binder.isInstanceOf[TypeMemberSymbol]) ||
      env.binders.get(binder).exists { shape =>
        (binder, ref.prefix) match {
          case (_: TypeMemberSymbol, self: ThisType) =>
            val id = s"${self.cls.displayFullName}#${binder.name}"
            shape == Shape.Atom(id) || shape == Shape.Constructor(id)
          case (_: TypeMemberSymbol, _) => false
          case _                        => true
        }
      }

  // Whether a parameter whose bounds say nothing and which declares no variance on a parameter of
  // its own is a constructor of proper parameters, and not itself declared `+F[_]` or `-F[_]` (a
  // class's: its applications are related by subtyping).
  private def constructor(parameter: TypeParamSymbol): Boolean = {
    val invariant = parameter match {
      case declared: ClassTypeParamSymbol =>
        declared.declaredVariance == tastyquery.Modifiers.Variance.Invariant
      case _ => true
    }
    invariant && (parameter.declaredBounds.high match {
      case lambda: TypeLambda => lambda.paramTypeBounds.forall(!_.high.isInstanceOf[TypeLambda])
      case _                  => false
    })
  }

  // Bounds any type constructor of proper, unbounded parameters meets: `[X] =>> Any`
  // above, nothing below (written as a lambda too, on a method's own parameter).
  private def rigidBounds(bounds: TypeBounds)(using Context): Boolean = {
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

  // The bounds of a proper type parameter: no type lambda above or below.
  private def proper(bounds: TypeBounds): Boolean =
    !bounds.high.isInstanceOf[TypeLambda] && !bounds.low.isInstanceOf[TypeLambda]

  /** The bounds of an abstract type member; none for any other. */
  def bounds(member: TypeMemberSymbol): Option[TypeBounds] = member.typeDef match {
    case TypeMemberDefinition.AbstractType(bounds) => Some(bounds)
    case _                                         => None
  }

  /** Whether an abstract type member is bounded (its bound is a proof, `Bounds`). */
  def bounded(member: TypeMemberSymbol)(using Context): Boolean =
    bounds(member).exists(!unbounded(_))

  /** Whether an abstract type member says nothing of what it stands for: unbounded, or an invariant
    * `F[_]` of unbounded parameters.
    */
  def free(member: TypeMemberSymbol)(using Context): Boolean = bounds(member).exists { b =>
    // Without a tree a declared variance is not seen: such a member is not free.
    unbounded(b) || rigidBounds(b) && tastyquery.Variances.invariant(b.high) &&
    member.tree.nonEmpty && !tastyquery.Variances.variant(member)
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

  /** Whether a record has a member named like the measured definition, own or inherited, which may
    * be that one on another receiver; a constructor row measures no member.
    */
  private def clash(c: ClassSymbol, site: Symbol)(using Context): Boolean =
    !World.isConstructor(site) &&
      (c.declarations.collect { case term: TermSymbol => term } ++
        World.ancestors(c).flatMap(_.declarations)).exists { member =>
        !member.isSynthetic && member.name.toString == site.name.toString
      }

  /** The measured member a record of `name` is read for, or why a site that is none leaves it. */
  private def siteMember(site: Symbol, name: String): Either[Unread, TermSymbol] = site match {
    case site: TermSymbol => Right(site)
    case _ => Left(Unread.Uncovered(s"a capability record read outside a member: $name"))
  }

  /** Whether one of `members` is an abstract type member. */
  def abstractType(members: List[Symbol]): Boolean = members.exists {
    case member: TypeMemberSymbol => member.isAbstractMember
    case _                        => false
  }

  /** A record of `name` left where it has `what` (`left`). */
  private def unread(name: String)(left: Boolean, what: String): Either[Unread, Unit] =
    Either.cond(!left, (), Unread.Uncovered(s"a capability record $what: $name"))

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

  /** The unbounded type parameter `ref` names where it is held rigid: a method's, or a class's own
    * through `this` (not one selected on another instance, whose bounds a prefix may change).
    */
  def rigidParameter(ref: TypeRef)(using Context): Option[TypeParamSymbol] =
    ref.optSymbol.collect {
      case p: TypeParamSymbol if Shapes.unbounded(p.declaredBounds) && (ref.prefix match {
            case NoPrefix       => true
            case self: ThisType => self.cls == p.owner
            case _              => false
          }) =>
        p
    }

  /** The component a match type's single case projects, as (arity, 1-based index): a pattern
    * `TupleN` over distinct binders of the case, whose result is one of them (the TASTy counterpart
    * of the old `MethodProjections.projection`; `Tuple1` is no tuple syntax there).
    */
  private def projection(cases: List[MatchTypeCase])(using Context): Option[(Int, Int)] =
    cases match {
      case List(single) =>
        def binder(tpe: Type) = tpe match {
          case ref: TypeParamRef if ref.binder eq single => Some(ref.paramNum)
          case _                                         => None
        }
        single.pattern match {
          case pattern: AppliedType
              if pattern.args.sizeIs >= 2 &&
                World
                  .classNamed(pattern)
                  .exists(_.displayFullName == s"scala.Tuple${pattern.args.size}") =>
            StuckProjections.component(
              pattern.args.map {
                case argument: Type => binder(argument)
                case _              => None
              },
              binder(single.result)
            )
          case _ => None
        }
      case _ => None
    }

  /** A sealed trait or abstract class, which is the sum of its subtypes; an enum is read by its own
    * rule.
    */
  def isFamily(c: ClassSymbol): Boolean =
    c.openLevel == OpenLevel.Sealed && c.isAbstractClass && !c.isEnum

  /** Whether a term is a reference to `field` on the instance, through ascriptions. */
  private def names(term: TermTree, field: TermSymbol)(using Context): Boolean = term match {
    case Typed(inner, _)             => names(inner, field)
    case Inlined(inner, _, _)        => names(inner, field)
    case Block(Nil, inner)           => names(inner, field)
    case ident: Ident                => World.named(ident).contains(field)
    case select @ Select(_: This, _) => World.named(select).contains(field)
    case _                           => false
  }

  /** Whether a template declares an abstract type member. */
  def abstractTypeMember(tree: ClassDef): Boolean =
    abstractType(tree.rhs.body.collect { case member: TypeMember => member.symbol })

  /** A class's tree, or what it needs that is not read. */
  def tree(c: ClassSymbol): Either[Unread, ClassDef] = World.tree(c).left.map(Unread.Uncovered(_))

  // A class written without a parent whose constructor is public.
  private def bare(tree: ClassDef): Boolean = parentless(tree) && tree.rhs.constr.symbol.isPublic

  // A template written without a parent: the compiler's own parents have no extent.
  def parentless(tree: ClassDef): Boolean = tree.rhs.parents.forall(_.pos.isZeroExtent)

}
