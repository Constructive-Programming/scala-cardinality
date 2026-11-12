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
  * A sealed family or an enum is one value of its sum as a receiver: which case, and what the case
  * holds (`split`). Not read yet, each a `Left` naming it: a concrete sealed class or an enum
  * case's own class; a sum this reading does not take; a class that inherits and is abstract, open
  * or extended (what it inherits is a capability); a parent of the module with constructor
  * parameters, in a class or a body (its members may read state the class's own code may not); a
  * member something overrides (a capability), or overriders the classes read do not settle; a self
  * type; a bounded or higher-kinded type parameter; an abstract type member; a capability with type
  * parameters of its own; a deferred `inline val`, or a deferred inline method over a type
  * parameter; a construction that always throws or binds a pattern (S6); and what implicit search
  * chooses (S5's remainder): a contextual parameter without a name, an abstract given without a
  * name, an abstract extension method. A given written `= deferred` is a capability like an
  * abstract member.
  */
final private[compiled] class Receivers(world: World, shapes: Shapes)(using Context) {

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
            _ <- refused(overridden, "an enclosing class with a member something overrides")
          } yield ()
      _ <- refused(world.unjudged(c), "an enclosing class whose overriders are not read")
      _ <- refused(c.givenSelfType.nonEmpty, "an enclosing class with a self type")
      _ <- refused(
        c.typeParams.exists(binder => !Shapes.unbounded(binder.declaredBounds)),
        "an enclosing class with a bounded or higher-kinded type parameter"
      )
      _ <- refused(
        Shapes.abstractTypeMember(tree),
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
      if (Receivers.split(tree.symbol)) List(Receivers.whole(tree.symbol))
      else
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
    } yield parameters.lazyZip(fields).map(held) ++ members.flatten
  }

}

private[compiled] object Receivers {

  /** A receiver read as one value of its sum: a sealed family or an enum. */
  def split(c: ClassSymbol): Boolean = Shapes.isFamily(c) || c.isEnum

  /** The name a sum receiver is bound and listed under, as the old front end names it. */
  def whole(c: ClassSymbol): String = s"<receiver ${c.displayFullName.replace("$.", ".")}>"

  /** An anonymous class (`new T { … }`), which nothing can extend. */
  def anonymous(c: ClassSymbol): Boolean = c.name.toString == "$anon"

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
