package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.{Binding, Shape}
import cardinality.types.Traversals
import tastyquery.Contexts.Context
import tastyquery.Modifiers.OpenLevel
import tastyquery.Symbols.*
import tastyquery.Trees.*

/** What the classes around a member supply to its body (slice S4, v1.md §2): a member of a class
  * runs on an instance, so it reads what the instance holds.
  *
  *   - Every parameter of the class's primary constructor is a value of the environment, `val` or
  *     not, whatever its visibility: the member is inside the class. (Its primary constructor is
  *     callable there, rule B's site: the old front end reads its own receivers so.)
  *   - Every abstract member of the class is a capability: a value of its type, or for a method one
  *     function from all its parameters to its result. One with a one-valued result supplies
  *     nothing: every body builds that value without it.
  *   - A concrete member supplies nothing: it computes from what the environment already holds.
  *     That holds where no class of the module overrides it (the closed world).
  *   - The class's type parameters are atoms.
  *   - A class that inherits is read by its own state alone where every member it inherits is
  *     implemented and none can be overridden: a concrete class that is final or that nothing
  *     extends. An object is such a template with no state.
  *
  * A class around the class, and one around an object the member is in, supplies the same: an inner
  * instance holds its outer one.
  *
  * Not read yet, each a `Left` naming it: a sealed class or an enum (the receiver splits into its
  * cases); a class that inherits and is abstract, open or extended (what it inherits is a
  * capability); a parent of the module with constructor parameters, in a class or a body (its
  * members may read state the class's own code may not); a member something overrides (a
  * capability), or overriders the classes read do not settle; a self type; a bounded or
  * higher-kinded type parameter; an abstract type member; a capability with type parameters of its
  * own; a deferred `inline val`, or a deferred inline method over a type parameter; a construction
  * that always throws or binds a pattern (S6); and what implicit search chooses (S5's remainder): a
  * contextual parameter without a name, an abstract given without a name, an abstract extension
  * method. A given written `= deferred` is a capability like an abstract member.
  */
final private[compiled] class Receivers(world: World, shapes: Shapes)(using Context) {
  import Unread.Uncovered

  /** The classes around a member, outermost first. Every other owner up to the package is an
    * object, which holds no state of its own.
    */
  def around(owner: Symbol, inner: Boolean = false): Either[String, List[ClassDef]] = owner match {
    case _: PackageSymbol                  => Right(Nil)
    case c: ClassSymbol if c.isModuleClass =>
      for {
        tree <- World.tree(c)
        _ <- inherits(tree)
        outer <- around(c.owner, inner)
        // An object in a class is built with an instance of that class, one around a class before
        // any instance of it, and one with parents runs their constructors: where its
        // construction always throws, no member in it is ever called (S6). (An object without
        // parents, in no class and around none, is `Readings.built`'s.)
        unbuilt = (inner || outer.nonEmpty || World.ancestors(c).nonEmpty) && world.unbuilt(tree)
        _ <- Either.cond(
          !unbuilt,
          (),
          s"an object whose construction always throws or binds a pattern: ${c.name}"
        )
      } yield outer
    case c: ClassSymbol =>
      for {
        tree <- World.tree(c)
        _ <- covered(tree)
        outer <- around(c.owner, inner = true)
      } yield outer :+ tree
    case other => Left(s"an enclosing definition: ${other.name}")
  }

  // What a template that inherits may still be read as: its own state alone. That holds where
  // every member it inherits is implemented and none can be overridden (an object, a concrete
  // class that is final or that nothing extends: the closed world's final), so that each computes
  // from what the template holds, and where no parent of the module takes constructor parameters,
  // which a parent's member may read and the template's own code may not.
  private def inherits(tree: ClassDef): Either[String, Unit] = {
    val c = tree.symbol
    val parents = World.ancestors(c)
    // `abstract final` is legal and implements nothing.
    def fixed = c.isModuleClass || (!c.isAbstractClass && !c.isTrait &&
      (Shapes.isFinal(c) || (c.openLevel == OpenLevel.Closed && !world.extended(c))))
    // A parent in a class holds that class's instance too, reached through whatever path names it.
    // One without a tree (compiled from Java) is not read.
    def stateful = parents.filter(world.ofModule).find { parent =>
      !World.inObjects(parent) ||
      parent.tree.forall(template => Shapes.parameters(template.rhs.constr).nonEmpty)
    }
    if (parents.isEmpty) Right(())
    else if (!fixed)
      Left(s"an enclosing class that inherits and is abstract or may be extended: ${c.name}")
    else
      stateful
        .map(parent => s"a parent with constructor parameters or in a class: ${parent.name}")
        .toLeft(())
  }

  private def covered(tree: ClassDef): Either[String, Unit] = {
    val c = tree.symbol
    def refused(left: Boolean, what: String) = Either.cond(!left, (), s"$what: ${c.name}")
    // A concrete member a class of the module overrides is a capability of this class.
    val overridden = tree.rhs.body.exists {
      case member: ValOrDefDef =>
        !Receivers.open(member) && world.overridden(member.symbol)
      case _ => false
    }
    for {
      _ <- refused(
        c.openLevel == OpenLevel.Sealed || c.isEnum || c.parentClasses.exists(_.isEnum),
        "an enclosing sealed class or enum"
      )
      _ <- inherits(tree)
      _ <- refused(world.unjudged(c), "an enclosing class whose overriders are not read")
      _ <- refused(overridden, "an enclosing class with a member something overrides")
      _ <- refused(c.givenSelfType.nonEmpty, "an enclosing class with a self type")
      _ <- refused(
        c.typeParams.exists(binder => !Shapes.unbounded(binder.declaredBounds)),
        "an enclosing class with a bounded or higher-kinded type parameter"
      )
      _ <- refused(
        tree.rhs.body.exists {
          case member: TypeMember => member.symbol.isAbstractMember
          case _                  => false
        },
        "an enclosing class with an abstract type member"
      )
      _ <- refused(
        world.unbuilt(tree),
        "an enclosing class whose construction always throws or binds a pattern"
      )
    } yield ()
  }

  /** The atoms of the receivers' type parameters, and of the copies their constructors hold. */
  def atoms(receivers: List[ClassDef]): Map[Symbol, Shape] = receivers.flatMap { tree =>
    val c = tree.symbol
    val named = c.typeParams.map(binder => Shape.Atom(s"${c.displayFullName}[${binder.name}]"))
    c.typeParams.zip(named) ++ Shapes.typeParameters(tree.rhs.constr).zip(named)
  }.toMap

  /** What the receivers supply to `measured`: their constructors' parameters and their abstract
    * members, `measured` aside (a member is no capability of its own).
    */
  def bindings(
      receivers: List[ClassDef],
      measured: TermSymbol,
      env: Shapes.Env
  ): Either[Unread, List[Binding]] =
    Traversals.sequence(receivers.map(supplied(_, measured, env))).map(_.flatten)

  /** The names the receivers give a member's captures, as the report lists them: each constructor
    * parameter and each member a subclass supplies, the measured one aside. (The old front end also
    * lists a value that is an alias of one of those, and a later overload as `name#2`: the gate
    * classes both differences.)
    */
  def names(receivers: List[ClassDef], measured: TermSymbol): List[String] =
    receivers.flatMap { tree =>
      Shapes.parameters(tree.rhs.constr).map(_.name.toString) ++ tree.rhs.body.collect {
        case member: ValOrDefDef
            if Receivers.open(member) && !member.symbol.isExtensionMethod &&
              member.symbol != measured && !member.name.toString.endsWith("_=") &&
              // A deferred inline method is supplied to inline code alone.
              (measured.isInline || !(member.symbol.isInline && member.isInstanceOf[DefDef])) =>
          member.name.toString
      }
    }

  private def supplied(
      tree: ClassDef,
      measured: TermSymbol,
      env: Shapes.Env
  ): Either[Unread, List[Binding]] = {
    val self = tree.symbol.displayFullName
    def held(value: ValDef) =
      shapes
        .of(value.tpt.toType, env)
        .map(Binding(s"$self.${value.name}", _, Some(value.name.toString)))
    val parameters = Shapes.parameters(tree.rhs.constr)
    val capabilities = tree.rhs.body.collect {
      case member: ValOrDefDef if Receivers.open(member) && member.symbol != measured =>
        member
    }
    for {
      _ <- Readings.plainInputs(parameters)
      fields <- Traversals.sequence(parameters.map(held))
      members <- Traversals.sequence(capabilities.map { member =>
        // An extension method is no member of its receiver's instances, and a given without a
        // name is found by search alone: neither is read as a capability yet.
        val unread =
          if (member.symbol.isExtensionMethod) Left("an extension method")
          else if (Readings.contextual(member.symbol) && member.name.toString.startsWith("given_"))
            Left("an anonymous given")
          else Readings.later(member.symbol)
        unread match {
          case Left(what) =>
            Left(Uncovered(s"$what among the members of an enclosing class: ${member.name}"))
          case Right(()) =>
            member match {
              // Each implementation of a deferred `inline val` is a constant of its own.
              case value: ValDef if value.symbol.isInline =>
                Left(Uncovered(s"a deferred inline value of an enclosing class: ${value.name}"))
              // A deferred inline method is called from inline code alone: the compiler rejects
              // a call of it from any other body, to which it therefore supplies nothing. One over
              // a type parameter may have no implementation at some instance, which says
              // something of the receiver: not read.
              case method: DefDef if method.symbol.isInline && !measured.isInline =>
                Either.cond(
                  !generic(method, env),
                  None,
                  Uncovered(s"a deferred inline method over a type parameter: ${method.name}")
                )
              case value: ValDef  => held(value).map(Some(_))
              case method: DefDef => capability(self, method, env)
            }
        }
      })
    } yield fields ++ members.flatten
  }

  // Whether a method's signature may name a type parameter: it has its own, or one is in scope
  // (through an alias too, so the signature is not searched).
  private def generic(method: DefDef, env: Shapes.Env): Boolean =
    Shapes.typeParameters(method).nonEmpty || env.binders.nonEmpty

  // An abstract method is one function, from the parameters of all its clauses to its result; an
  // abstract given with a `using` clause is such a method (the old front end read it as a value
  // of its result type: a listed correction). A
  // one-valued result (`Unit`, an opaque type outside its scope) is built without it.
  private def capability(
      self: String,
      member: DefDef,
      env: Shapes.Env
  ): Either[Unread, Option[Binding]] = {
    val parameters = Shapes.parameters(member)
    if (Shapes.typeParameters(member).nonEmpty)
      Left(Uncovered(s"a capability with type parameters of its own: ${member.name}"))
    else
      for {
        _ <- Readings.plainInputs(parameters)
        result <- shapes.of(member.resultTpt.toType, env)
        inputs <- shapes.inputs(parameters, env)
      } yield Option.when(result != Shape.Product(Nil))(
        Binding(s"$self.${member.name}", Shape.Function(inputs, result), Some(member.name.toString))
      )
  }

}

private[compiled] object Receivers {

  /** A member a subclass supplies: an abstract one, or a given written `= deferred`
    * (`scala.compiletime.deferred`, as the compiler resolved the name), which each class extending
    * the trait gives.
    */
  def open(member: ValOrDefDef)(using Context): Boolean = {
    val body = member match {
      case method: DefDef => method.rhs
      case value: ValDef  => value.rhs
    }
    member.symbol.isAbstractMember || (member.symbol.isGivenOrUsing && body.exists {
      case reference: TermReferenceTree =>
        val named = reference.symbol
        named.name.toString == "deferred" && World.under(named, "scala.compiletime")
      case _ => false
    })
  }

}
