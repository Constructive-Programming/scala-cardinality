package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.{Binding, Shape}
import cardinality.types.Traversals
import tastyquery.Contexts.Context
import tastyquery.Modifiers.OpenLevel
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

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
  *     That holds where no class of the program overrides it (the closed world, `ClosedProgram`).
  *   - The class's type parameters are atoms.
  *   - A class that inherits is read by its own state alone where every member it inherits is
  *     implemented and none can be overridden: a concrete class that is final or that nothing
  *     extends. An object is such a template with no state.
  *   - A class that inherits and is abstract or may be extended reads what it inherits as it
  *     resolves it: an abstract member is a capability, read from its signature as the class sees
  *     it, and a concrete one supplies nothing where no class of the program overrides it.
  *
  * A class around the class, and one around an object the member is in, supplies the same: an inner
  * instance holds its outer one.
  *
  * Not read yet, each a `Left` naming it: a sealed class or an enum (the receiver splits into its
  * cases); a class that reaches one parent through two of its parents, or whose inherited members
  * are not read; a parent with constructor parameters, in a class or a body (its members may read
  * state the class's own code may not), and for a class that is not fixed a parent without TASTy
  * that is no trait; a member something overrides (a capability), or overriders the classes read do
  * not settle; a self type; a bounded or higher-kinded type parameter; an abstract type member; a
  * capability with type parameters of its own; a deferred `inline val`, or a deferred inline method
  * over a type parameter; a construction that always throws or binds a pattern (S6); and what
  * implicit search chooses (S5's remainder): a contextual parameter without a name, an abstract
  * given without a name, an abstract extension method. A given written `= deferred` is a capability
  * like an abstract member.
  */
final private[compiled] class Receivers(world: World, shapes: Shapes)(using Context) {
  import Unread.Uncovered

  /** The classes around a member, outermost first. An object holds no state of its own; a
    * definition a class is written in is skipped (`Readings.enclosing` reads its methods); an
    * anonymous class is read as the final template it is, a local class or object is not.
    */
  def around(owner: Symbol, inner: Boolean = false): Either[String, List[ClassDef]] = owner match {
    case _: PackageSymbol => Right(Nil)
    // A definition a class is written in: `Readings` reads what its methods supply.
    case term: TermSymbol => around(term.owner, inner)
    // A local class or object holds what the methods around it take by rules of its own (a
    // subclass in the same body may override it): not read. An anonymous class is final.
    case c: ClassSymbol if !c.owner.isClass && !c.owner.isPackage && !Receivers.anonymous(c) =>
      Left(s"a local class or object: ${c.name}")
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
  //
  // A template that may be extended, or is abstract, reads what it inherits as it resolves it
  // (`inherited`): an abstract member is a capability, a concrete one computes from the instance
  // where nothing overrides it. That holds where no parent holds state: none takes constructor
  // parameters (which the class that finally extends it passes), none is in a class, and none is
  // a class without TASTy, whose constructors set state no signature shows.
  private def inherits(tree: ClassDef): Either[String, Unit] = {
    val c = tree.symbol
    val parents = World.ancestors(c)
    // A parent in a class holds that class's instance too, reached through whatever path names it.
    // A fixed template reads its module's parents alone; one of them without a tree (compiled from
    // Java) is not read. A template that is not fixed reads every parent, and one without a tree
    // that is no trait sets state in constructors no signature shows.
    val whole = fixed(c)
    def why(parent: ClassSymbol): Option[String] =
      if (!World.inObjects(parent)) Some("a parent with constructor parameters or in a class")
      else
        parent.tree match {
          case Some(t) =>
            Option.when(Shapes.parameters(t.rhs.constr).nonEmpty)(
              "a parent with constructor parameters or in a class"
            )
          case None if whole           => Some("a parent compiled from Java")
          case None if !parent.isTrait => Some("a parent without TASTy that is no trait")
          case None                    => None
        }
    parents
      .filter(parent => !whole || world.ofModule(parent))
      .flatMap(parent => why(parent).map(reason => s"$reason: ${parent.name}"))
      .headOption
      .toLeft(())
  }

  // Whether the template, or a class it extends, reaches one parent through two of its parents.
  private def reachesTwice(c: ClassSymbol): Boolean = {
    val ancestors = World.ancestors(c)
    (c :: ancestors).exists { k =>
      val parents = k.parentClasses.filter(ancestors.contains)
      ancestors.exists(ancestor => parents.count(_.isSubClass(ancestor)) > 1)
    }
  }

  // The member `c` has where `member` is declared: the first one along its linearization that
  // matches it, unless that one is abstract and a concrete one matches in a parent it does not
  // extend (mixed in beside it), which implements it. An abstract declaration in a class that
  // extends the concrete one's makes the member abstract again.
  private def resolved(
      member: TermOrTypeSymbol,
      line: List[ClassSymbol],
      c: ClassSymbol
  ): Option[TermOrTypeSymbol] = {
    val matching = line.flatMap(member.matchingSymbol(_, c))
    matching.headOption.map { first =>
      if (!abstractLike(first)) first
      else
        matching
          .find { other =>
            !abstractLike(other) && !first.owner.asClass.isSubClass(other.owner.asClass)
          }
          .getOrElse(first)
    }
  }

  // A member each class extending the template gives: an abstract one, or a deferred given.
  private def abstractLike(member: TermOrTypeSymbol): Boolean = member match {
    case term: TermSymbol => term.isAbstractMember || Receivers.deferred(term)
    case other            => other.isAbstractMember
  }

  // `abstract final` is legal and implements nothing.
  private def fixed(c: ClassSymbol): Boolean =
    c.isModuleClass || (!c.isAbstractClass && !c.isTrait && world.finalClass(c))

  // The members a template that is not fixed inherits as it resolves them: each ancestor's member
  // no class nearer the template replaces (a universal marker's aside). None for a fixed one,
  // which implements all it inherits, and a `Left` where a signature is not read.
  private def inherited(c: ClassSymbol): Either[String, List[TermOrTypeSymbol]] =
    inheritance.getOrElseUpdate(c, inheritedOf(c))

  private val inheritance =
    scala.collection.mutable.Map.empty[ClassSymbol, Either[String, List[TermOrTypeSymbol]]]

  private def inheritedOf(c: ClassSymbol): Either[String, List[TermOrTypeSymbol]] =
    if (fixed(c)) Right(Nil)
    // A parent two of its parents extend is read once by the linearization, each with its own
    // type arguments: not read yet.
    else if (reachesTwice(c))
      Left(s"an enclosing class that reaches a parent through two parents: ${c.name}")
    else
      try {
        val line = c.linearization
        Right(
          World
            .ancestors(c)
            .flatMap(_.declarations)
            .filter {
              case _: ClassSymbol | _: TypeParamSymbol                                    => false
              case member: TermSymbol if member.isModuleVal || Receivers.accessor(member) => false
              case member                                                                 =>
                !member.isPrivate && !member.isSynthetic && !World.isConstructor(member) &&
                resolved(member, line, c).contains(member)
            }
            .distinct
        )
      } catch {
        case scala.util.control.NonFatal(_) =>
          Left(s"an enclosing class whose inherited members are not read: ${c.name}")
      }

  private def covered(tree: ClassDef): Either[String, Unit] = {
    val c = tree.symbol
    def refused(left: Boolean, what: String) = Either.cond(!left, (), s"$what: ${c.name}")
    // A concrete member a class of the program overrides is a capability of this class; one a
    // class that is not read may override is not known to be either.
    val inheriting = inherited(c)
    val overriding = tree.rhs.body.collect {
      case member: ValOrDefDef if !Receivers.open(member) => world.overriding(c, member.symbol)
    } ++ inheriting.getOrElse(Nil).collect {
      case member: TermSymbol if !abstractLike(member) => world.overriding(c, member)
    }
    for {
      _ <- refused(
        c.openLevel == OpenLevel.Sealed || c.isEnum || c.parentClasses.exists(_.isEnum),
        "an enclosing sealed class or enum"
      )
      _ <- inherits(tree)
      _ <- inheriting
      _ <- refused(
        overriding.contains(ClosedProgram.Overriding.Unknown),
        "an enclosing class whose overriders are not read"
      )
      _ <- refused(
        overriding.contains(ClosedProgram.Overriding.Supplied),
        "an enclosing class with a member something overrides"
      )
      _ <- refused(c.givenSelfType.nonEmpty, "an enclosing class with a self type")
      _ <- refused(
        c.typeParams.exists(binder => !Shapes.unbounded(binder.declaredBounds)),
        "an enclosing class with a bounded or higher-kinded type parameter"
      )
      _ <- refused(
        tree.rhs.body.exists {
          case member: TypeMember => member.symbol.isAbstractMember
          case _                  => false
        } || inheriting.getOrElse(Nil).exists {
          case member: TypeMemberSymbol => member.isAbstractMember
          case _                        => false
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
      } ++ inheritedOpen(tree.symbol)
        .filter(member => member != measured && !member.name.toString.endsWith("_="))
        .map(_.name.toString)
    }

  // The abstract members a template that is not fixed inherits: capabilities, as its own are.
  private def inheritedOpen(c: ClassSymbol): List[TermSymbol] =
    inherited(c).getOrElse(Nil).collect {
      case member: TermSymbol if abstractLike(member) => member
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
      more <- Traversals.sequence(
        inheritedOpen(tree.symbol)
          .filter(_ != measured)
          .map(inheritedCapability(tree.symbol, _, env))
      )
    } yield fields ++ members.flatten ++ more.flatten
  }

  // An inherited abstract member as a capability, read from its signature as the template sees
  // it: a value of its type, or one function from all its parameters to its result.
  private def inheritedCapability(
      c: ClassSymbol,
      member: TermSymbol,
      env: Shapes.Env
  ): Either[Unread, Option[Binding]] = {
    val name = member.name.toString
    // Overloads an ancestor declares are capabilities of their own: each is named with its
    // signature.
    val identity = s"${c.displayFullName}.${member.signedName}"
    val unread =
      if (member.isExtensionMethod) Left("an extension method")
      else if (Readings.contextual(member) && name.startsWith("given_")) Left("an anonymous given")
      else if (member.isInline) Left("a deferred inline member")
      else Readings.later(member)
    def clauses(tpe: TypeOrMethodic, read: List[Type]): Either[Unread, (List[Type], Type)] =
      tpe match {
        case _: PolyType =>
          Left(Uncovered(s"a capability with type parameters of its own: $name"))
        case method: MethodType
            if method.isImplicitOrContextual && method.paramNames.exists(
              _.toString.contains("$")
            ) =>
          Left(Uncovered(s"an anonymous contextual parameter of: $name"))
        case method: MethodType => clauses(method.resultType, read ++ method.paramTypes)
        // A result that names a parameter has no shape: `shapes.of` leaves it uncovered.
        case result: Type => Right((read, result))
      }
    for {
      _ <- unread.left.map(what =>
        Uncovered(s"$what among the members of an enclosing class: $name")
      )
      seen = member.typeAsSeenFrom(c.thisType)
      binding <-
        if (!member.isMethod)
          seen match {
            case tpe: Type =>
              shapes.of(tpe, env).map(s => Some(Binding(identity, s, Some(name))))
            case _ => Left(Uncovered(s"a value of a method type: $name"))
          }
        else
          clauses(seen, Nil).flatMap { (parameters, result) =>
            for {
              inputs <- Traversals.sequence(parameters.map(shapes.of(_, env)))
              read <- shapes.of(result, env)
            } yield Option.when(read != Shape.Product(Nil))(
              Binding(identity, Shape.Function(inputs, read), Some(name))
            )
          }
    } yield binding
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

  /** An anonymous class (`new T { … }`), which nothing can extend. */
  def anonymous(c: ClassSymbol): Boolean = c.name.toString == "$anon"

  /** A member the compiler adds to a trait for a `super` call (`superT$$m`): abstract, and
    * implemented by every class mixing the trait in as the call of the parent's member, which is
    * concrete code.
    */
  def accessor(member: TermSymbol): Boolean = member.name.toString.contains("$$")

  /** A member a subclass supplies: an abstract one, or a given written `= deferred`
    * (`scala.compiletime.deferred`, as the compiler resolved the name), which each class extending
    * the trait gives.
    */
  def open(member: ValOrDefDef)(using Context): Boolean =
    !accessor(member.symbol) && (member.symbol.isAbstractMember || deferred(member.symbol))

  /** A given written `= deferred`, which each class extending the trait gives. */
  def deferred(member: TermSymbol)(using Context): Boolean =
    member.isGivenOrUsing && member.tree.exists {
      case definition: ValOrDefDef =>
        val body = definition match {
          case method: DefDef => method.rhs
          case value: ValDef  => value.rhs
        }
        body.exists {
          case reference: TermReferenceTree =>
            val named = reference.symbol
            named.name.toString == "deferred" && World.under(named, "scala.compiletime")
          case _ => false
        }
      case _ => false
    }

}
