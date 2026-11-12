package cardinality.compiled

import cardinality.analysis.BuiltinTypes
import cardinality.analysis.inhabitation.Inhabitation.Shape
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
  * name (the compiler bound it, decision D3). A class of the module is read by its declaration
  * (v1.md §2):
  *
  *   - a case class whose one parameter list is its whole product (no written parent, no member but
  *     a secondary constructor, no restricted field or constructor) is the product of its fields;
  *   - an enum and a sealed family are the sum of their cases (`Sums`);
  *   - a trait or class with only type members is one value;
  *   - any other class or trait is a capability record (`record`): the tuple of its public fields
  *     and of what its abstract members supply;
  *   - an opaque alias is one value outside the scope its representation is visible in (rule F).
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
    case World.Named(ref, arguments) =>
      World.proper(arguments).left.map(Uncovered(_)).flatMap(named(ref, _, env))
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
      case Some(binder) if env.binders.contains(binder) =>
        if (arguments.isEmpty) Right(env.binders(binder))
        else Left(Uncovered(s"a type parameter applied: ${ref.showBasic}"))
      // An evidence type compares its arguments as types, and two types of the module with one
      // shape are two types: their shapes are not read there.
      case Some(symbol) if env.nominal && world.ofModule(symbol) =>
        Left(Uncovered(s"a type of the module in an evidence type: ${symbol.displayFullName}"))
      case Some(c: ClassSymbol) =>
        // A context function is a function: only implicit search tells its argument apart.
        val library =
          world.library(c).map(_.replaceFirst("^scala\\.ContextFunction", "scala.Function"))
        val inside = if (library.exists(Shapes.Evidence)) env.copy(nominal = true) else env
        Traversals.sequence(arguments.map(of(_, inside))).flatMap { read =>
          library.flatMap(standard(_, read)) match {
            case Some(known)               => known
            case None if world.ofModule(c) => declared(c, read, env)
            case None => Left(Uncovered(s"a class of a dependency: ${c.displayFullName}"))
          }
        }
      case Some(alias: TypeMemberSymbol) =>
        alias.typeDef match {
          case TypeMemberDefinition.OpaqueTypeAlias(_, body) => opaque(alias, body, arguments, env)
          case _                                             => uncovered(ref)
        }
      case _ => uncovered(ref)
    }

  // Rule F: outside the scope its representation is visible in (its owner), an opaque alias of the
  // module is one value; inside, it is its representation. A dependency's is left to its slice:
  // the standard library's have readings of their own (a named tuple is its fields).
  private def opaque(
      alias: TypeMemberSymbol,
      body: Type,
      arguments: List[Type],
      env: Env
  ): Either[Unread, Shape] =
    if (!world.ofModule(alias))
      Left(Uncovered(s"an opaque type of a dependency: ${alias.displayFullName}"))
    else if (!World.owners(env.site).contains(alias.owner)) Right(Shape.Product(Nil))
    // The compiler refuses an alias that refers to itself: a cycle passes through a class.
    else of(body.appliedTo(arguments), env)

  // ---- classes of the module ----------------------------------------------------------------

  private def declared(c: ClassSymbol, arguments: List[Shape], env: Env): Either[Unread, Shape] =
    Shapes.tree(c).flatMap { tree =>
      val inside = env.copy(visiting = env.visiting + c)
      // A member of a class nested in a class or a body may read what is around it.
      if (!World.inObjects(c))
        Left(Uncovered(s"a class nested in a class or a body: ${c.displayFullName}"))
      else if (env.visiting(c)) Left(Refused(s"recursive type not read: ${c.displayFullName}"))
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
  // hidden at the use site, a sealed class, a self type, an abstract type member, a concrete
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
    val overridden = members.exists { member =>
      !member.symbol.isAbstractMember && world.overridden(member.symbol)
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
      (Shapes.typeParameters(template.rhs.constr) ++ c.typeParams).zip(bound ++ bound).toMap
    )
    // A template whose construction always throws, or a class written `final abstract`, has no
    // value: the empty sum.
    if ((Shapes.isFinal(c) && c.isAbstractClass) || Bodies.alwaysThrows(template))
      Right(Shape.Sum(Nil))
    else
      for {
        _ <- refused(World.ancestors(c).nonEmpty, "with parents")
        _ <- refused(world.unjudged(c), "whose overriders are not read")
        _ <- refused(world.unbuilt(template), "whose construction binds a pattern")
        _ <- refused(supplied.exists(!_.symbol.isPublic), "with a member hidden at the use site")
        _ <- refused(c.openLevel == OpenLevel.Sealed, "of a sealed class")
        _ <- refused(c.givenSelfType.nonEmpty, "with a self type")
        _ <- refused(Shapes.abstractTypeMember(template), "with an abstract type member")
        _ <- refused(overridden, "with a member something overrides")
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
      // read. (Its signature may name one through an alias too, so it is not searched.)
      case (_, method: DefDef) if symbol.isInline && !inlineSite =>
        Either.cond(
          Shapes.typeParameters(method).isEmpty && env.binders.isEmpty,
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
      Shapes.parameters(constructor).map(p => of(p.tpt.toType, env.copy(binders = bound)))
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
      .isEmpty && c.givenSelfType.isEmpty && !(Shapes.isFinal(c) && c.isAbstractClass) &&
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
