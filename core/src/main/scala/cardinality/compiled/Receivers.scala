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
  *   - The class's type parameters are atoms, or rigid constructors for `F[_]` (`Shapes.binder`).
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
  * A sealed family or an enum is one value of its sum as a receiver: which case, and what the case
  * holds (`split`). Not read yet, each a `Left` naming it: a concrete sealed class or an enum
  * case's own class; a sum this reading does not take; a class that reaches one parent through two
  * of its parents, or whose inherited members are not read; a parent with constructor parameters,
  * in a class or a body (its members may read state the class's own code may not), and for a class
  * that is not fixed a parent without TASTy that is no trait; a member something overrides (a
  * capability), or overriders the classes read do not settle; a self type; a bounded type
  * parameter, or one of another kind than `F[_]`; an abstract type member; a capability with type
  * parameters of its own; a deferred `inline val`, or a deferred inline method over a type
  * parameter; a construction that always throws or binds a pattern (S6); and what implicit search
  * chooses (S5's remainder): a contextual parameter without a name, an abstract given without a
  * name, an abstract extension method. A given written `= deferred` is a capability like an
  * abstract member.
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
    val sum = Receivers.split(c)
    for {
      // A sealed family or an enum is the sum of its cases (`split`); a concrete sealed class or
      // an enum case's own class is not read yet.
      _ <- refused(
        !sum && (c.openLevel == OpenLevel.Sealed || c.parentClasses.exists(_.isEnum)),
        "an enclosing concrete sealed class or enum case"
      )
      // What a sum receiver inherits, or a case overrides, computes from the case it is, which
      // the sum holds.
      _ <-
        if (sum) Right(())
        else
          for {
            _ <- inherits(tree)
            _ <- inheriting
            _ <- refused(
              overriding.contains(ClosedProgram.Overriding.Supplied),
              "an enclosing class with a member something overrides"
            )
          } yield ()
      _ <- refused(
        overriding.contains(ClosedProgram.Overriding.Unknown),
        "an enclosing class whose overriders are not read"
      )
      _ <- refused(c.givenSelfType.nonEmpty, "an enclosing class with a self type")
      _ <- refused(
        Shapes.abstractTypeMember(tree) || (!sum && inheriting.getOrElse(Nil).exists {
          case member: TypeMemberSymbol => member.isAbstractMember
          case _                        => false
        }),
        "an enclosing class with an abstract type member"
      )
      _ <- refused(
        world.unbuilt(tree),
        "an enclosing class whose construction always throws or binds a pattern"
      )
    } yield ()
  }

  /** What the receivers' type parameters stand for (`Shapes.binder`), and the copies their
    * constructors hold.
    */
  def atoms(receivers: List[ClassDef]): Either[Unread, Map[Symbol, Shape]] =
    Traversals
      .sequence(receivers.map { tree =>
        val c = tree.symbol
        Traversals
          .sequence(c.typeParams.map(Shapes.binder(c, _, Shapes.typeParameters(tree.rhs.constr))))
          .map(named =>
            c.typeParams.zip(named) ++ Shapes.typeParameters(tree.rhs.constr).zip(named)
          )
      })
      .map(_.flatten.toMap)

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
      if (Receivers.split(tree.symbol)) List(Receivers.whole(tree.symbol))
      else
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
  ): Either[Unread, List[Binding]] =
    if (Receivers.split(tree.symbol)) {
      // The receiver is one value of its family's sum: which case, and what that case holds.
      val label = Receivers.whole(tree.symbol)
      // A sum this reading does not take leaves the row uncovered, not refused: the old front end
      // still counts a row whose goal stands without the split (an infinite one).
      shapes
        .receiver(tree.symbol, env)
        .map(sum => List(Binding(label, sum, Some(label))))
        .left
        .map {
          case Unread.Refused(reason) =>
            Unread.Uncovered(s"an enclosing sum not read: $reason")
          case uncovered => uncovered
        }
    } else fields(tree, measured, env)

  // What a receiver that is no sum supplies: its constructor's parameters and its members a
  // subclass supplies, `measured` aside.
  private def fields(template: ClassDef, measured: TermSymbol, env: Shapes.Env) = {
    val self = template.symbol.displayFullName
    def held(of: ValOrDefDef, shape: Shape) =
      Binding(s"$self.${of.name}", shape, Some(of.name.toString))
    val parameters = Shapes.parameters(template.rhs.constr)
    val capabilities = template.rhs.body.collect {
      case member: ValOrDefDef if Receivers.open(member) && member.symbol != measured =>
        member
    }
    for {
      _ <- Readings.plainInputs(parameters)
      fields <- shapes.inputs(parameters, env)
      members <- Traversals.sequence(capabilities.map { member =>
        shapes.member(member, "an enclosing class", env).map(_.map(held(member, _)))
      })
      more <- Traversals.sequence(
        inheritedOpen(template.symbol)
          .filter(_ != measured)
          .map(inheritedCapability(template.symbol, _, env))
      )
    } yield parameters.lazyZip(fields).map(held) ++ members.flatten ++ more.flatten
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

}

private[compiled] object Receivers {

  /** A receiver read as one value of its sum: a sealed family or an enum. */
  def split(c: ClassSymbol): Boolean = Shapes.isFamily(c) || c.isEnum

  /** The name a sum receiver is bound and listed under, as the old front end names it. */
  def whole(c: ClassSymbol): String = s"<receiver ${c.displayFullName.replace("$.", ".")}>"

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
