package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.{Binding, Shape}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Modifiers.{OpenLevel, Visibility}
import tastyquery.SourceLanguage
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

/** What the classes around a member supply to its body (slice S4, v1.md §2): a member of a class
  * runs on an instance, so it reads what the instance holds. A value of a capability record is read
  * the same way, as what its template supplies as a receiver (`record`), and one of a dependency's
  * record without TASTy as what its members supply (`dependency`).
  *
  *   - Every parameter of the class's primary constructor is a value of the environment, `val` or
  *     not, whatever its visibility: the member is inside the class. (Its primary constructor is
  *     callable there, rule B's site: the old front end reads its own receivers so.)
  *   - Every abstract member of the class is a capability: a value of its type, or for a method one
  *     function from all its parameters to its result. One with a one-valued result supplies
  *     nothing: every body builds that value without it.
  *   - A concrete member supplies nothing: it computes from what the environment already holds.
  *     That holds where no class of the program overrides it (the closed world, `ClosedProgram`);
  *     one a class of the program overrides varies across instances, and is a capability as an
  *     abstract member is, its own body one of its behaviors.
  *   - The class's type parameters are atoms, or rigid constructors for `F[_]` (`Shapes.binder`),
  *     and so are its abstract type members, declared or inherited, on its own instance
  *     (`Shapes.typeMember`).
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
  * of its parents, or whose inherited members are not read; a parent in a class or a body, a parent
  * whose private constructor parameters a subclass passes, a constructor field something overrides,
  * and for a class that is not fixed a parent without TASTy that is no trait; an overridden member
  * a behavior of which may call the measured one (`Reentry`), or overriders the classes read do not
  * settle; a self type narrower than the class (`World.ownSelf`); a type parameter of another kind
  * than `F[_]` that a type read names; an abstract type member of a sum, a bounded one or one of
  * another kind than `F[_]`, one two enclosing instances hold, one of a class no class written
  * elsewhere extends, one another declaration of its name bounds or hides; a capability with type
  * parameters of its own; a deferred `inline val`, or a deferred inline method over a type
  * parameter; and what implicit search chooses (S5's remainder): a contextual parameter without a
  * name, an abstract given without a name, an abstract extension method. A given written
  * `= deferred` is a capability like an abstract member.
  */
final private[compiled] class Receivers(world: World, shapes: Shapes)(using Context) {
  import Unread.Uncovered

  private val reentry = new Reentry(world)

  /** The classes around a member, outermost first. An object holds no state of its own; a
    * definition a class is written in is skipped (`Readings.enclosing` reads its methods); an
    * anonymous class is read as the final template it is, a local class or object is not.
    */
  def around(owner: Symbol): Either[String, List[ClassDef]] = owner match {
    case _: PackageSymbol => Right(Nil)
    // A definition a class is written in: `Readings` reads what its methods supply.
    case term: TermSymbol => around(term.owner)
    // A local class or object holds what the methods around it take by rules of its own (a
    // subclass in the same body may override it): not read. An anonymous class is final.
    case c: ClassSymbol if !c.owner.isClass && !c.owner.isPackage && !Receivers.anonymous(c) =>
      Left(s"a local class or object: ${c.name}")
    case c: ClassSymbol if c.isModuleClass =>
      for {
        tree <- World.tree(c)
        _ <- inherits(tree)
        outer <- around(c.owner)
      } yield outer
    case c: ClassSymbol =>
      for {
        tree <- World.tree(c)
        _ <- covered(tree)
        outer <- around(c.owner)
      } yield outer :+ tree
    case other => Left(s"an enclosing definition: ${other.name}")
  }

  // What a template that inherits may still be read as: its own state alone. That holds where
  // every member it inherits is implemented and none can be overridden (an object, a concrete
  // class that is final or that nothing extends: the closed world's final), so that each computes
  // from what the template holds.
  //
  // A template that may be extended, or is abstract, reads what it inherits as it resolves it
  // (`inherited`): an abstract member is a capability, a concrete one computes from the instance
  // where nothing overrides it.
  //
  // A parent's constructor parameters are state its members may read: a function of what the
  // template holds where the template, or a class it extends, passes their arguments; where a
  // subclass passes them, each is a free value, which a public field of the parent observes (a
  // capability, `free`) and a private one, or a field the line overrides, hides (not read). A
  // constructor field something overrides is two values, what the parent was given and what its
  // getter answers (not read). A parent in a class holds that class's instance too, reached through
  // whatever path names it (not read). A fixed template reads its module's parents alone; one of
  // them without a tree (compiled from Java) is not read. A template that is not fixed reads every
  // parent, and one without a tree that is no trait sets state in constructors no signature shows.
  private def inherits(tree: ClassDef): Either[String, Unit] = {
    val c = tree.symbol
    val parents = World.ancestors(c)
    val whole = fixed(c)
    def fields(k: ClassSymbol) = k.declarations.collect {
      case field: TermSymbol if field.isParamAccessor => field
    }
    def why(parent: ClassSymbol): Option[String] =
      if (!World.inObjects(parent)) Some("a parent with constructor parameters or in a class")
      else
        parent.tree match {
          case Some(_) =>
            // A private field, or one the line answers otherwise than the parameter it was given.
            val hidden = fields(parent).exists { field =>
              !field.isPublic ||
              scala.util
                .Try(resolved(field, c.linearization, c))
                .toOption
                .flatten
                .exists(_ != field)
            }
            Option.when(!passed(c, parent) && hidden)(
              "a parent with constructor parameters or in a class"
            )
          case None if whole           => Some("a parent compiled from Java")
          case None if !parent.isTrait => Some("a parent without TASTy that is no trait")
          case None                    => None
        }
    val overriddenField = (c :: parents).flatMap(fields).find { field =>
      world.overriding(c, field) == ClosedProgram.Overriding.Supplied
    }
    overriddenField
      .map(field =>
        s"an enclosing class with a constructor field something overrides: ${field.name}"
      )
      .orElse(
        parents
          .filter(parent => !whole || world.ofModule(parent))
          .flatMap(parent => why(parent).map(reason => s"$reason: ${parent.name}"))
          .headOption
      )
      .toLeft(())
  }

  // Whether the template, or a class it extends, passes the arguments of `parent`'s constructor.
  private def passed(c: ClassSymbol, parent: ClassSymbol): Boolean =
    (c :: World.ancestors(c)).exists { k =>
      k != parent && k.tree.exists(_.rhs.parents.exists {
        case term: TermTree =>
          term.tpe match {
            case tpe: Type => World.classNamed(tpe).contains(parent)
            case _         => false
          }
        case _ => false
      })
    }

  // A parent's field whose argument a subclass passes: a value each instance chooses (a private
  // one refuses the template first, in `inherits`).
  private def free(c: ClassSymbol, member: TermSymbol): Boolean =
    member.isParamAccessor && member.owner != c && !passed(c, member.owner.asClass)

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
      _ <- if (sum) Right(()) else inherits(tree).flatMap(_ => inheriting)
      _ <- refused(unsettled(tree), "an enclosing class whose overriders are not read")
      _ <- refused(!World.ownSelf(c), "an enclosing class with a self type")
      // A class's abstract type members are atoms (`atoms`); a sum's are not read yet.
      _ <- refused(
        sum && (Shapes.abstractTypeMember(tree) || Shapes.abstractType(inheriting.getOrElse(Nil))),
        "an enclosing class with an abstract type member"
      )
    } yield ()
  }

  // Whether a class that is not read may override a concrete member of the template, own or
  // inherited, which is then not known to be a capability or to compute from the instance.
  private def unsettled(tree: ClassDef): Boolean =
    unsettled(
      tree.symbol,
      tree.rhs.body.collect {
        case member: ValOrDefDef if !Receivers.open(member) => member.symbol
      } ++ inherited(tree.symbol).getOrElse(Nil).collect {
        case member: TermSymbol if !abstractLike(member) => member
      }
    )

  private def unsettled(c: ClassSymbol, concrete: List[TermSymbol]): Boolean =
    concrete.exists(world.overriding(c, _) == ClosedProgram.Overriding.Unknown)

  /** What a value of a capability record holds as the site observes it (v1.md §2), read as the
    * template's receiver reading reads it (`own`): its constructor's fields and the members a
    * subclass supplies, own or inherited, an overridden one included unless a behavior of it may
    * call the site (`Reentry`). Synthetic members, setters and members restating one every value
    * has (`universal`) observe nothing of the value and are set aside. Its unbounded abstract type
    * members, declared or inherited, are the witnesses of the package each value carries (`∃X.
    * members over X`, as the old front end reads it): each an atom on the record's instance, which
    * the solver opens per value. Refused: a parent this reading does not take (`inherits`,
    * `inherited`), an abstract type member that is bounded, of another kind than a type, one
    * `atoms` would not read or one a receiver around the site holds too, a member the site may not
    * call, and overriders the classes read do not settle.
    */
  def record(template: ClassDef, site: TermSymbol, env: Shapes.Env): Either[Unread, Shape] = {
    val c = template.symbol
    val where = Receivers.Record
    val hidden = (supplies(template, site).map(_.symbol) ++ inheritedOpen(c, site))
      .find(member => Receivers.observes(member) && !member.isPublic)
    for {
      inheriting <- inherits(template)
        .flatMap(_ => inherited(c))
        .left
        .map(reason =>
          Uncovered(
            s"$where whose parents are not read: ${reason.replace("an enclosing class", "a class")}"
          )
        )
      witnesses <- typeMembers(template, inheriting)
      _ <- witnesses
        .collectFirst { case (member, _) if env.binders.contains(member) => member }
        .map(member =>
          Uncovered(s"$where with an abstract type member a receiver holds: ${member.name}")
        )
        .toLeft(())
      // A witness's bound is a proof about each record value's own type member: not read yet.
      ids <- Traversals.sequence(witnesses.map {
        case (member, _: Shape.Atom) if Shapes.bounded(member) =>
          Left(Uncovered(s"$where with a bounded abstract type member: ${member.name}"))
        case (_, Shape.Atom(id)) => Right(id)
        case (member, _)         =>
          Left(Uncovered(s"$where with an abstract type member of another kind: ${member.name}"))
      })
      _ <- hidden
        .map(member => Uncovered(s"$where with a member hidden at the use site: ${member.name}"))
        .toLeft(())
      _ <- Either.cond(
        !unsettled(template),
        (),
        Uncovered(s"$where whose overriders are not read: ${c.displayFullName}")
      )
      held <- own(
        template,
        site,
        env.copy(binders = env.binders ++ witnesses),
        where,
        Receivers.observes
      )
    } yield {
      val product = Shape.Product(held.map(_.shape))
      if (ids.isEmpty) product else Shape.Existential(ids, product)
    }
  }

  /** What a value of a dependency's record without TASTy holds as the site observes it
    * (`DependencyClasses.Declared.Record`), read as `record` reads what a template inherits: each
    * abstract member, and each concrete one a class of the program overrides, is a capability read
    * from its signature; a concrete one nothing overrides is code that is not supplied, which
    * computes from the others. Code that is not supplied (the record's, or other code in the
    * member's scope) may read such a member the site may not call: then the sources do not say what
    * a value is, which the scalameta reading refuses in the same words. Not read yet: a parent
    * reached through two parents or holding state, and overriders the classes read do not settle.
    */
  def dependency(
      c: ClassSymbol,
      abstracts: List[TermSymbol],
      concrete: List[TermSymbol],
      site: TermSymbol,
      env: Shapes.Env
  ): Either[Unread, List[Shape]] = {
    val name = c.displayFullName
    val open = (abstracts ++ concrete.filter(overridden(c, _))).filter(Receivers.observes)
    def unread(left: Boolean, why: Unread) = Either.cond(!left, (), why)
    for {
      _ <- unread(
        !open.forall(World.callable(_, site)),
        Unread.Refused(s"capability record with members hidden at the use site: $name")
      )
      // A parent class given arguments, or keeping Java state, holds what each instance chooses.
      _ <- unread(
        reachesTwice(c) || World.ancestors(c).exists { k =>
          !k.isTrait && (k.sourceLanguage == SourceLanguage.Java || k.declarations.exists {
            case t: TermSymbol =>
              World.isConstructor(t) && t.paramSymss.exists(_.left.exists(_.nonEmpty))
            case _ => false
          })
        },
        Uncovered(s"${Receivers.Record} whose parents are not read: $name")
      )
      _ <- unread(
        unsettled(c, concrete),
        Uncovered(s"${Receivers.Record} whose overriders are not read: $name")
      )
      held <- Traversals.sequence(open.map(inheritedCapability(c, _, site, env, value = true)))
    } yield held.flatten.map(_.shape)
  }

  /** What the receivers' type parameters stand for (`Shapes.binder`), each read or not, and the
    * copies their constructors hold; and what their abstract type members, declared or inherited,
    * stand for (`Shapes.typeMember`), on the instance alone (`this.T`; `Shapes` reads no other path
    * to one).
    */
  def atoms(receivers: List[ClassDef]): Either[Unread, List[(Symbol, Either[Unread, Shape])]] =
    Traversals
      .sequence(receivers.map { tree =>
        val c = tree.symbol
        for {
          named = c.typeParams.map(Shapes.binder(c, _, Shapes.typeParameters(tree.rhs.constr)))
          held <- typeMembers(tree, inherited(c).getOrElse(Nil))
        } yield c.typeParams.zip(named) ++ Shapes.typeParameters(tree.rhs.constr).zip(named) ++
          held.map((member, shape) => member -> Right(shape))
      })
      .flatMap { pairs =>
        // Two instances around the member each hold their own type of one declaration.
        val members = pairs.flatten.map(_._1).collect { case m: TypeMemberSymbol => m }
        members.diff(members.distinct).headOption match {
          case Some(twice) =>
            Left(Uncovered(s"an abstract type member two enclosing instances hold: ${twice.name}"))
          case None => Right(pairs.flatten)
        }
      }

  // What the abstract type members of a template, declared or among those it inherits, stand for
  // on its instance (`Shapes.typeMember`), unless the program's classes pick them (`pinned`) or
  // another declaration bounds or hides one (`alike`).
  private def typeMembers(
      tree: ClassDef,
      inheriting: List[TermOrTypeSymbol]
  ): Either[Unread, List[(TypeMemberSymbol, Shape)]] = {
    val c = tree.symbol
    val members = (tree.rhs.body.collect { case member: TypeMember => member.symbol } ++
      inheriting).collect { case member: TypeMemberSymbol if member.isAbstractMember => member }
    members.headOption
      .flatMap(_ => pinned(c))
      .orElse(members.view.flatMap(alike(c, _)).headOption)
      .map(Uncovered(_))
      .toLeft(())
      .flatMap(_ => Traversals.sequence(members.map(Shapes.typeMember(c, _))))
      .map(members.zip(_))
  }

  // Why the types an abstract type member of `c` stands for are only those the program's classes
  // pick, as the old front end reads it: it counts one as any type only where a class written
  // anywhere may extend `c`, which is not so where `c` is never extended (final, `final abstract`,
  // a value class), where a restricted constructor of it or of a class it extends is callable only
  // in its scope, or where `c` or a class around it is private to a template.
  private def pinned(c: ClassSymbol): Option[String] = {
    def confined(s: TermOrTypeSymbol, toPackage: Boolean) = s.visibility match {
      case Visibility.Private | Visibility.PrivateThis => true
      case Visibility.ScopedPrivate(scope)             => toPackage || !scope.isPackage
      case _                                           => false
    }
    (if (Shapes.isFinal(c) || c.isSubClass(defn.AnyValClass)) Some(s"${c.name} is never extended")
     else
       (c :: World.ancestors(c))
         .find(_.tree.exists(k => confined(k.rhs.constr.symbol, toPackage = true)))
         .map(k => s"the constructor of ${k.name} is not public")
         .orElse(World.owners(c).collectFirst {
           case k: ClassSymbol if confined(k, toPackage = false) => s"${k.name} is private"
         })).map(why =>
      s"an abstract type member of a class no class written elsewhere extends: $why"
    )
  }

  // Another declaration of an abstract type member's name along `c`'s parents: a private one,
  // which tasty-query takes for the member when it matches members, or one that bounds it, which
  // the member's own declaration does not show.
  private def alike(c: ClassSymbol, member: TypeMemberSymbol): Option[String] =
    World.ancestors(c).flatMap(_.declarations).collectFirst {
      case other: TypeMemberSymbol
          if other != member && other.name == member.name &&
            (other.isPrivate || !Shapes.free(other)) =>
        s"an abstract type member another declaration of its name bounds or hides: ${member.name}"
    }

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
        Shapes.parameters(tree.rhs.constr).map(_.name.toString) ++ supplies(tree, measured)
          .collect {
            case member
                if !member.symbol.isExtensionMethod && !member.name.toString.endsWith("_=") &&
                  // A deferred inline method is supplied to inline code alone.
                  (measured.isInline || !(member.symbol.isInline && member.isInstanceOf[DefDef])) =>
              member.name.toString
          } ++ inheritedOpen(tree.symbol, measured)
          .filter(!_.name.toString.endsWith("_="))
          .map(_.name.toString)
    }

  // The members a template that is not fixed inherits and a subclass may supply: capabilities, as
  // its own are, the measured one aside.
  private def inheritedOpen(c: ClassSymbol, measured: TermSymbol): List[TermSymbol] =
    inherited(c).getOrElse(Nil).collect {
      case member: TermSymbol
          if (abstractLike(member) || overridden(c, member) || free(c, member)) &&
            member != measured &&
            !reentry.definite(member, measured) =>
        member
    }

  // Whether an overridden member may be a behavior of the measured one's own (`Reentry`).
  private def reenters(
      c: ClassSymbol,
      member: TermSymbol,
      measured: TermSymbol,
      value: Boolean
  ): Boolean =
    overridden(c, member) && reentry.may(c, member, measured, instance = !value)

  // The members of a template's own body a subclass supplies, the measured one aside.
  private def supplies(tree: ClassDef, measured: TermSymbol): List[ValOrDefDef] =
    tree.rhs.body.collect {
      case member: ValOrDefDef
          if (Receivers.open(member) || overridden(tree.symbol, member.symbol)) &&
            member.symbol != measured && !reentry.definite(member.symbol, measured) =>
        member
    }

  // A concrete member a class of the program overrides varies across instances as an abstract
  // one does: it is a capability of the template, its own body one of its behaviors.
  private def overridden(c: ClassSymbol, member: TermSymbol): Boolean =
    // A constructor field something overrides refuses the template (`inherits`).
    !abstractLike(member) && !member.isParamAccessor &&
      world.overriding(c, member) == ClosedProgram.Overriding.Supplied

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
    } else own(tree, measured, env, "an enclosing class", _ => true, receiving = true)

  // What a receiver that is no sum supplies: its constructor's parameters and its members a
  // subclass supplies, `measured` aside, the ones `kept` keeps; `where` names the template in a
  // reason.
  private def own(
      template: ClassDef,
      measured: TermSymbol,
      env: Shapes.Env,
      where: String,
      kept: TermSymbol => Boolean,
      receiving: Boolean = false
  ) = {
    val value = !receiving
    val self = template.symbol.displayFullName
    def held(of: ValOrDefDef, shape: Shape) =
      Binding(s"$self.${of.name}", shape, Some(of.name.toString))
    val parameters = Shapes.parameters(template.rhs.constr)
    val capabilities = supplies(template, measured).filter(member => kept(member.symbol))
    for {
      _ <- Readings.plainInputs(parameters)
      // A receiver's constructor parameter was built before it, and is never the receiver: one
      // whose field no class of the program overrides (with the receiver, say), and not inline
      // (by-name is read past its arrow, as nothing preceding), as the old front end's
      // `builtBefore`.
      fields <- Traversals.sequence(parameters.map { parameter =>
        val fixed = template.rhs.body.exists {
          case field: ValDef =>
            field.name == parameter.name && field.symbol.isParamAccessor &&
            world.overriding(template.symbol, field.symbol) == ClosedProgram.Overriding.No
          case _ => false
        }
        val before = receiving && fixed && !parameter.symbol.isInline
        shapes.of(parameter.tpt.toType, env.copy(preceding = before))
      })
      members <- Traversals.sequence(capabilities.map { member =>
        if (reenters(template.symbol, member.symbol, measured, value))
          Left(
            Uncovered(
              s"${Receivers.Reenters} among the members of $where: ${member.name}"
            )
          )
        else shapes.member(member, where, env).map(_.map(held(member, _)))
      })
      more <- Traversals.sequence(
        inheritedOpen(template.symbol, measured)
          .filter(kept)
          .map(inheritedCapability(template.symbol, _, measured, env, value))
      )
    } yield parameters.lazyZip(fields).map(held) ++ members.flatten ++ more.flatten
  }

  // An inherited member a subclass supplies (abstract, overridden, or a free parent field) as a
  // capability, read from its signature as the template sees it: a value of its type, or one
  // function from all its parameters to its result.
  private def inheritedCapability(
      c: ClassSymbol,
      member: TermSymbol,
      measured: TermSymbol,
      env: Shapes.Env,
      value: Boolean
  ): Either[Unread, Option[Binding]] = {
    val name = member.name.toString
    val where = if (value) Receivers.Record else "an enclosing class"
    // Overloads an ancestor declares are capabilities of their own: each is named with its
    // signature.
    val identity = s"${c.displayFullName}.${member.signedName}"
    val unread =
      if (reenters(c, member, measured, value)) Left(Receivers.Reenters)
      else if (member.isExtensionMethod) Left("an extension method")
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
      _ <- unread.left.map(what => Uncovered(s"$what among the members of $where: $name"))
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

  private val Record = "a capability record"

  /** Whether a member observes a value of a capability record: synthetic members, setters and
    * members restating one every value has (`World.restates`) do not.
    */
  private def observes(member: TermSymbol)(using Context): Boolean =
    !member.isSynthetic && !member.name.toString.endsWith("_=") && !World.restates(member)

  /** An anonymous class (`new T { … }`), which nothing can extend. */
  def anonymous(c: ClassSymbol): Boolean = c.name.toString == "$anon"

  /** A member the compiler adds to a trait for a `super` call (`superT$$m`): abstract, and
    * implemented by every class mixing the trait in as the call of the parent's member, which is
    * concrete code.
    */
  def accessor(member: TermSymbol): Boolean = member.name.toString.contains("$$")

  /** Why an overridden member a behavior of which may call the measured one is not read. */
  val Reenters = "an overridden member that may call it"

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
