package cardinality.compiled

import cardinality.analysis.AnalysisWork
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.{StuckProjections, TopTypes}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Traversers
import tastyquery.Trees.*
import tastyquery.Types.{AppliedType, Type, TypeRef}

/** What the compiled front end measures so far, read from symbols and types
  * (docs/plans/compiled-own-types.md, "Front end: design and status", lists the slices): a `Left`
  * names what a row needs that no slice reads yet, and is never a count. A type the counting model
  * refuses is the row's `?` with the model's reason (`Unread.Refused`).
  *
  * Method rows: a method or value whose environment is its own parameters, those of the methods a
  * local definition is written in (`local` says what of them is not read yet), and what the classes
  * around it hold (`Receivers`; a concrete member around it computes from what the environment
  * already holds, and an object holds nothing), over the types `Shapes` reads and the type
  * parameters as `Shapes.binder` reads them.
  *
  * An inline method counts as a method unless an expansion of it is decided at a call site, which
  * `Inlines` says: its refusal is the row's reason, the model's own.
  *
  * A class's row is its primary constructor's, read by `Constructors` where the class is declared:
  * a class in a class is built on an instance of each class around it, whose captures the row
  * lists. Definition rows are measured by `Stored`.
  */
final private[compiled] class Readings(world: World, stored: Stored, inlines: Inlines)(using
    Context
) {
  import Readings.{Held, Local}
  import Unread.Uncovered

  // A capability record is read as what its template supplies as a receiver: `Shapes` asks
  // `Receivers`, which reads types with `Shapes`.
  private val shapes: Shapes = new Shapes(world, receivers)
  private val bounds = new Bounds(shapes)
  private lazy val receivers: Receivers = new Receivers(world, shapes)
  private val constructors = new Constructors(shapes, stored, receivers)

  // ---- method rows --------------------------------------------------------------------------

  /** The count of a method row, and its captures: the names of what the methods and classes around
    * it supply. A class's row measures its primary constructor.
    */
  def method(tree: ValOrDefDef): Either[String, (Count, List[String])] = {
    val primary = Readings.primary(tree.symbol)
    // A constructor runs where its class is declared: the instance it builds does not exist yet.
    val owner = if (primary) tree.symbol.owner.owner else tree.symbol.owner
    for {
      _ <- if (primary) Right(()) else Readings.later(tree.symbol)
      // A constructor row reads the classes around its class; a member reads its receivers.
      // A local definition reads the methods it is written in, then the classes around them.
      (methods, top) <- if (primary) Right(Nil -> owner) else enclosing(owner, tree.symbol)
      scope <- if (primary) Right(Local.Plain) else local(tree, methods)
      around <-
        if (!primary) receivers.around(top)
        // A class in a class is built on an instance of the class around it, whose state its
        // constructor's positions may take; an object holds no such state.
        else
          inTemplates(owner).flatMap(_ =>
            if (World.inObjects(tree.symbol.owner)) Right(Nil) else receivers.around(owner)
          )
      _ <- inherited(methods, top)
      _ <- plain(top)
      count <-
        // A call of an inline method is its expansion, which builds no object: the old front
        // end counts one in an object that is never built like any other.
        if (
          (tree :: methods).exists(of => inlines.inlineMethod(of.symbol)) ||
          built(top, methods.headOption.getOrElse(tree))
        )
          (tree match {
            case constructor: DefDef if primary =>
              constructorSearch(constructor).flatMap(_ => constructors.count(constructor, around))
            case _ => counted(tree, around, methods, scope)
          }).map(_ -> captures(tree, around, methods)).left.flatMap {
            // A row the model refuses lists no captures, as the old front end's does not.
            case Unread.Refused(reason) => Right(Count.Unresolved(List(reason)) -> Nil)
            case Uncovered(what)        => Left(what)
          }
        else if (primary) Left("a constructor in an object that is never built")
        // No call of a member of an object that is never built returns: one implementation.
        else Right(Count.Finite(1) -> captures(tree, around, methods))
    } yield count
  }

  // A `transparent inline` result is refused first (rule X: callers see the expansion's type).
  // A goal with one value has one implementation whatever the environment holds (v1.md §2), so
  // the result is read next, before what an inline body does, as the old front end reads it;
  // else the count is the solver's over the parameters and what the receivers hold.
  private def counted(
      tree: ValOrDefDef,
      around: List[ClassDef],
      methods: List[DefDef],
      scope: Local
  ): Either[Unread, Count] = {
    val (_, parameters, declared) = signature(tree)
    // A value of the environment: a parameter of the member or of a method it is written in.
    def bound(of: ValOrDefDef)(parameter: ValDef, shape: Shape) =
      Binding(
        s"${of.symbol.displayFullName}.${parameter.name}",
        shape,
        Some(parameter.name.toString)
      )
    for {
      _ <- inlines.narrowed(tree.symbol).map(Unread.Refused(_)).toLeft(())
      env <- environment(tree, around, methods)
      result <- shapes.of(declared.toType, env)
      count <-
        if (TopTypes.terminalGoal(TopTypes.polarized(result, true))) Right(Count.Finite(1))
        else
          for {
            // After the one-valued result: such a result needs nothing of the environment.
            _ <- scope.blocked.map(Uncovered(_)).toLeft(())
            _ <- inlines.refusal(tree.symbol).map(Unread.Refused(_)).toLeft(())
            _ <- searched(declared.toType, parameters)
            held <- receivers.bindings(around, tree.symbol, env)
            captured <- Traversals.sequence(methods.map { method =>
              val taken = Readings.taken(method).filterNot(p => scope.hidden(p.symbol))
              Readings
                .plainInputs(taken)
                // A method around the definition took them before its receiver was built (a
                // by-name type is read past its arrow, as nothing preceding); an inline one is
                // evaluated where it is read, and may be the receiver.
                .flatMap(_ =>
                  Traversals.sequence(taken.map { parameter =>
                    val before = env.copy(preceding = !parameter.symbol.isInline)
                    shapes.of(parameter.tpt.toType, before)
                  })
                )
                .map(taken.lazyZip(_).map(bound(method)))
            })
            inputs <- shapes.inputs(parameters, env)
            values = held ++ captured.flatten ++ parameters.lazyZip(inputs).map(bound(tree))
            // The proofs of the bounds of the parameters the result or a value mentions (`Bounds`).
            proofs <- bounds.proofs(result :: values.map(_.shape), env)
            // Stuck projections of one parameter that no single instantiation reduces.
            _ <- StuckProjections
              .refusal(result :: values.map(_.shape), proofs.map(_.shape))
              .map(Unread.Refused(_))
              .toLeft(())
          } yield Readings.count(values ++ proofs, result)
    } yield count
  }

  // An anonymous contextual parameter of the measured method is found by implicit search alone.
  // A body is counted, not searched, and what one declares tells nothing of the others; what
  // encloses the method is shallower and loses (`ContextualScope` of the old front end). So its
  // competitors are the method's other contextual parameters: one of its very type makes it
  // ambiguous (refused); each other one must be of a different class, read by its declaration or
  // final (the old reading knows only those classes' bases) and not `Null`, neither class
  // extending the other. Otherwise, and beside a context-function result, which puts binders
  // around every body, it is not read yet.
  private def searched(result: Type, parameters: List[ValDef]): Either[Unread, Unit] = {
    val contextual = parameters.filter(p => Readings.contextual(p.symbol))
    def known(c: ClassSymbol) =
      c != defn.NullClass && (world.readByDeclaration(c) || Shapes.isFinal(c))
    val verdicts = contextual.filter(Readings.anonymous).flatMap { p =>
      contextual.filter(_ != p).map { q =>
        val (mine, theirs) = (p.tpt.toType, q.tpt.toType)
        (World.classNamed(mine), World.classNamed(theirs)) match {
          case _ if mine.isSameType(theirs) =>
            Left(Unread.Refused(s"ambiguous anonymous evidence: ${mine.showBasic}"))
          case (Some(a), Some(b))
              if a != b && known(a) && known(b) && !a.isSubClass(b) && !b.isSubClass(a) =>
            Right(())
          case (Some(a), Some(b)) if a == b && Readings.unrelated(mine, theirs) => Right(())
          case _                                                                =>
            Left(
              Uncovered(s"an anonymous contextual parameter search may not tell apart: ${p.name}")
            )
        }
      }
    }
    if (!contextual.exists(Readings.anonymous)) Right(())
    else if (World.classNamed(result).exists(_.displayFullName.startsWith("scala.ContextFunction")))
      Left(Uncovered("an anonymous contextual parameter beside a context-function result"))
    else
      // A refusal of the model first, as the old front end reports every one.
      verdicts
        .collectFirst { case refused @ Left(_: Unread.Refused) => refused }
        .orElse(verdicts.find(_.isLeft))
        .getOrElse(Right(()))
  }

  // A constructor's anonymous contextual parameter is searched for in its class's body, where the
  // class's own and inherited contextual definitions and the body's imports compete with it (the
  // old `ContextualScope` at the template): it is read as the measured method's is (`searched`) only
  // where the class is written without parents or a self type and its body imports nothing and
  // declares nothing contextual but its own parameters.
  private def constructorSearch(constructor: DefDef): Either[Unread, Unit] = {
    val parameters = Shapes.parameters(constructor)
    val owner = constructor.symbol.owner.asClass
    val quiet = owner.tree.exists { cls =>
      Shapes.parentless(cls) && World.ownSelf(owner) && cls.rhs.body.forall {
        case _: Import           => false
        case member: ValOrDefDef =>
          member.symbol.isParamAccessor || !Readings.contextual(member.symbol)
        case _ => true
      }
    }
    if (!parameters.exists(Readings.anonymous)) Right(())
    else if (!quiet)
      Left(
        Uncovered(
          s"an anonymous contextual parameter beside the class's contextual scope: ${owner.name}"
        )
      )
    else searched(owner.appliedRefInsideThis, parameters)
  }

  // A member's type parameters, value parameters and declared result.
  private def signature(tree: ValOrDefDef) = tree match {
    case v: ValDef => (Nil, Nil, v.tpt)
    case d: DefDef =>
      (d.paramLists.flatMap(_.toOption.toList.flatten), Shapes.parameters(d), d.resultTpt)
  }

  // The type parameters of the measured method and of the methods around it (`Shapes.binder`),
  // beside its receivers', and the method as the site rule F reads an opaque type from.
  private def environment(
      tree: ValOrDefDef,
      around: List[ClassDef],
      methods: List[DefDef]
  ): Either[Unread, Shapes.Env] = {
    val own = (methods :+ tree).flatMap { of =>
      signature(of)._1.map { binder =>
        binder.symbol -> Shapes.binder(of.symbol, binder.symbol.asInstanceOf[TypeParamSymbol])
      }
    }
    receivers.atoms(around).map(receiving => Shapes.Env.of(receiving ++ own, tree.symbol))
  }

  // The methods a definition is written in, outermost first, at whatever depth (a member of a
  // class in a body, or of a template inside such a class, is written in them too), and the
  // innermost owner that is none: a class, an object or a package. A value a definition is
  // written in adds nothing: its right-hand side runs before the value exists.
  private def enclosing(
      owner: Symbol,
      measured: Symbol
  ): Either[String, (List[DefDef], Symbol)] = {
    val owners = World.owners(owner)
    val innermost = owners.find(!_.isTerm).getOrElse(owner)
    Traversals
      .sequence(owners.collect { case term: TermSymbol => term }.map { term =>
        term.tree match {
          // A class-body statement is the primary constructor's: a definition in one reads the
          // instance as a member does, its constructor parameters among what the class holds (the
          // old front end walks it in the template). The parents' arguments, which the primary
          // constructor owns too, are evaluated before the instance exists. A secondary
          // constructor's body reads the instance its first call built, whose fields that call's
          // arguments fix: not read yet.
          case Some(_: DefDef) if Readings.primary(term) && term.owner.tree.exists {
                case template: ClassDef =>
                  !template.rhs.parents.exists(Readings.declares(_, measured))
                case _ => false
              } =>
            Right(None)
          case Some(constructor: DefDef) if World.isConstructor(constructor.symbol) =>
            Left("a definition in a constructor's body")
          case Some(method: DefDef) => Right(Some(method))
          // A member value's initializer runs while its template is built: where that always
          // throws, whether the definition was called first is the body reading's to say (S6).
          case Some(_: ValDef) if term.owner.tree.exists {
                case template: ClassDef => Bodies.alwaysThrows(template)
                case _                  => false
              } =>
            Left("a definition in the initializer of a template that is never built")
          case Some(_: ValDef) => Right(None)
          case _               => Left(s"an enclosing definition: ${term.name}")
        }
      })
      .map(found => found.flatten.reverse -> innermost)
  }

  // A class written in a method inherits members whose names come before that method's
  // parameters: a parameter of such a name cannot be named in the class at all.
  private def inherited(methods: List[DefDef], top: Symbol): Either[String, Unit] = {
    val taken = methods.flatMap(Readings.taken).map(_.name.toString).toSet
    // Every class or object around the member that is itself inside a method.
    val members = World
      .owners(top)
      .collect { case c: ClassSymbol if World.owners(c.owner).exists(_.isTerm) => c }
      .flatMap(_.linearization)
      .flatMap(_.declarations)
      .map(_.name.toString)
    members.find(taken).map(name => s"a capture an inherited member hides: $name").toLeft(())
  }

  // What a local definition reads of the methods around it: each of their parameters, by its
  // name, and the parameters a scope between that method and the definition binds again (a
  // block's definition, an inner method's parameter, a class's member or self alias, the
  // definition's own parameter), which it cannot name: those are left out of its environment
  // (`BodyScopes` and `HeldCaptures` of the old front end), unless other code around the definition
  // holds one for it (`holding`). A contextual one stays within implicit search's reach, which a
  // name does not end: not read. A definition elsewhere in the method that shares the name hides
  // nothing. A case clause around the definition is read where its pattern binds nothing and adds
  // no type equation (`patterned`), and a `for` comprehension's generator not at all. The scopes
  // are read from the outermost code the definition is written in: the outermost method, value or
  // class-body statement (whose class is walked).
  private def local(tree: ValOrDefDef, methods: List[DefDef]): Either[String, Local] =
    World.owners(tree.symbol.owner).collect { case term: TermSymbol => term }.lastOption.flatMap {
      outermost => if (Readings.primary(outermost)) outermost.owner.tree else outermost.tree
    } match {
      case None       => Right(Local.Plain)
      case Some(root) =>
        // The scopes open at each point, innermost first: the definition a scope is of, if any,
        // and the names it binds. A block's names count over the whole block.
        type Scope = (Option[Symbol], Set[String])
        var scopes = List.empty[Scope]
        var cases = List.empty[CaseDef]
        var seen = Option.empty[(List[Scope], List[CaseDef])]
        var generators = Set.empty[Symbol]
        def within(scope: Scope)(body: => Unit): Unit = {
          val saved = scopes
          scopes = scope :: scopes
          body
          scopes = saved
        }
        def named(stats: List[Tree]) = stats.collect {
          case d: ValOrDefDef => d.name.toString
        }.toSet
        new Traversers.TreeTraverser {
          override def traverse(node: Tree): Unit = node match {
            case clause: CaseDef =>
              cases = clause :: cases
              super.traverse(clause)
              cases = cases.tail
            case call: Apply =>
              generators ++= Readings.generators(call)
              super.traverse(call)
            case block: Block       => within(None -> named(block.stats))(super.traverse(block))
            case template: ClassDef =>
              // Its members, and the alias it may give itself (`new T { self => … }`).
              val alias = template.rhs.self.map(_.name.toString).toSet
              within(None -> (named(template.rhs.body) ++ alias))(super.traverse(template))
            case definition: ValOrDefDef =>
              val own = definition match {
                case method: DefDef => Shapes.parameters(method).map(_.name.toString).toSet
                case _: ValDef      => Set.empty[String]
              }
              within(Some(definition.symbol) -> own) {
                if (definition.symbol == tree.symbol) seen = Some(scopes -> cases)
                super.traverse(definition)
              }
            case other => super.traverse(other)
          }
        }.traverse(root)
        seen match {
          case None                 => Left("a definition not found in the method around it")
          case Some((open, around)) =>
            val hidden = methods.flatMap { method =>
              val nearer = open.takeWhile(!_._1.contains(method.symbol)).flatMap(_._2).toSet
              // An anonymous contextual parameter has no name to hide.
              Readings
                .taken(method)
                .filter(p => !Readings.anonymous(p) && nearer(p.name.toString))
            }
            val held = hidden.map(p => p -> holding(p, root, tree.symbol))
            val blocked = Option
              .when(methods.exists(m => generators(m.symbol)))(
                "a binder of an enclosing for-comprehension"
              )
              .orElse(around.flatMap(patterned(_, tree.symbol)).headOption)
              .orElse(hidden.find(p => Readings.contextual(p.symbol)).map { p =>
                s"a contextual capture a nearer scope binds again: ${p.name}"
              })
              .orElse(held.collectFirst {
                case (p, Held.Otherwise) =>
                  s"a capture a nearer scope binds again, held by other code: ${p.name}"
              })
            Right(Local(held.collect { case (p, Held.Not) => p.symbol }.toSet, blocked))
        }
    }

  // A case clause's pattern changes the context in two ways not read (the old `BodyScopes`): a
  // binder may be the scrutinee or a part of a value the environment has, and where a type
  // parameter is in scope a constructor, typed or stable-identifier pattern may add a type
  // equation.
  private def patterned(clause: CaseDef, measured: Symbol): Option[String] = {
    def refining(pattern: PatternTree): Boolean = pattern match {
      case _: WildcardPattern                      => false
      case ExprPattern(_: Literal)                 => false
      case Bind(_, body, _)                        => refining(body)
      case Alternative(trees)                      => trees.exists(refining)
      case Unapply(fun, _, patterns) if tuple(fun) => patterns.exists(refining)
      case _                                       => true
    }
    def tuple(fun: TermTree): Boolean = fun match {
      case TypeApply(inner, _) => tuple(inner)
      case select: Select      => select.symbol.owner.displayFullName.startsWith("scala.Tuple")
      case _                   => false
    }
    var binder = Option.empty[Bind]
    new Traversers.TreeTraverser {
      override def traverse(node: Tree): Unit = node match {
        case bind: Bind => binder = binder.orElse(Some(bind))
        case other      => super.traverse(other)
      }
    }.traverse(clause.pattern)
    val generic = World.owners(measured.owner).exists {
      case c: ClassSymbol => c.typeParams.nonEmpty
      case t: TermSymbol  =>
        t.tree.exists {
          case d: DefDef => Shapes.typeParameters(d).nonEmpty
          case _         => false
        }
      case _ => false
    }
    binder
      .map(bind => s"a binder of an enclosing case clause: ${bind.name}")
      .orElse(
        Option.when(generic && refining(clause.pattern))(
          "a pattern of an enclosing case clause that may refine a type"
        )
      )
  }

  // What code around `measured` other than the definitions it is written in makes of a hidden
  // parameter (the old `HeldCaptures`). Code names it in a definition beside it, a class beside
  // it, or the parents of a class it is written in, whose fields the arguments fill. A holder that
  // is exactly the parameter (`def seed: Boolean = b`, a `lazy val`, a given) shows it at its
  // declared type: at the parameter's own, reading it is reading the parameter, which stays an
  // input (`Exactly`); at a top type it shows nothing (D-top). Any other holder (a strict value
  // computed from it included) is `Otherwise`. A throw supplies nothing; a function literal that
  // is no such definition's is code that binds nothing the definition sees.
  private def holding(parameter: ValDef, root: Tree, measured: Symbol): Held = {
    val path = World.owners(measured).toSet
    def mentions(tree: Tree) = Reentry.search(tree) {
      case _: Throw                                                => Some(false)
      case id: Ident if World.named(id).contains(parameter.symbol) => Some(true)
      case _                                                       => None
    }
    def alias(holder: ValOrDefDef): Held = {
      val at = World.declared(holder)
      val exact = (holder match {
        case d: DefDef => d.paramLists.isEmpty
        case _: ValDef => true
      }) && World.rhs(holder).exists(Shapes.names(_, parameter.symbol))
      if (exact && at.isSameType(parameter.tpt.toType)) Held.Exactly
      else if (exact && Relations.top(at)) Held.Not
      else Held.Otherwise
    }
    val verdicts = List.newBuilder[Held]
    new Traversers.TreeTraverser {
      override def traverse(node: Tree): Unit = node match {
        case c: ClassDef if path(c.symbol) =>
          if (c.rhs.parents.exists(mentions)) verdicts += Held.Otherwise
          super.traverse(c)
        case Targets.Scope.Lambda(function)    => function.rhs.foreach(traverse)
        case d: ValOrDefDef if !path(d.symbol) => if (mentions(d)) verdicts += alias(d)
        case c: ClassDef if mentions(c)        => verdicts += Held.Otherwise
        case other                             => super.traverse(other)
      }
    }.traverse(root)
    verdicts.result().maxOption.getOrElse(Held.Not)
  }

  // A row's captures, as the report lists them: the names its receivers supply, the parameters
  // of the methods a local definition is written in (one a nearer scope hides among them, as the
  // old front end lists it), and for an
  // extension method the parameters of its `extension` clause, written before the method.
  private def captures(
      tree: ValOrDefDef,
      around: List[ClassDef],
      methods: List[DefDef]
  ): List[String] = {
    val group = tree match {
      case method: DefDef if method.symbol.isExtensionMethod && !method.pos.isUnknown =>
        Shapes
          .parameters(method)
          .filter(p => !p.pos.isUnknown && p.pos.startOffset < method.pos.startOffset)
          .map(_.name.toString)
      case _ => Nil
    }
    val taken = methods.flatMap(Readings.taken).map(_.name.toString)
    (receivers.names(around, tree.symbol) ++ taken ++ group).distinct.sorted
  }

  // Whether every owner of a constructor up to the package is a class or an object.
  private def inTemplates(owner: Symbol): Either[String, Unit] = owner match {
    case _: PackageSymbol => Right(())
    case c: ClassSymbol   => inTemplates(c.owner)
    case other            => Left(s"an enclosing definition: ${other.name}")
  }

  // Which values of a pattern or multi-name definition the constructor evaluates, and which is the
  // measured choice, is the body reading's to say (slice S6).
  private def plain(owner: Symbol): Either[String, Unit] = owner.tree match {
    case Some(c: ClassDef) if Bodies.destructures(c) =>
      Left("an enclosing object with a pattern or multi-name value definition")
    case _ => Right(())
  }

  // Whether the object a member runs in is ever built (register D26): a constructor that always
  // throws leaves no instance.
  private def built(owner: Symbol, measured: Tree): Boolean =
    owner.tree.forall {
      case c: ClassDef => !Bodies.alwaysThrows(c, Some(measured))
      case _           => true
    }

}

private[compiled] object Readings {

  /** What a local definition's scopes leave of the methods around it: the parameters it cannot
    * reach, and what of the scopes is not read yet. That refuses the row only where its result has
    * more than one value, which needs nothing of the environment.
    */
  final case class Local(hidden: Set[Symbol], blocked: Option[String])

  object Local {
    val Plain: Local = Local(Set.empty, None)
  }

  /** What code around a definition makes of a parameter the definition cannot name, in order of
    * precedence: none holds it, or each at a type with one value (`Not`); one is exactly it
    * (`Exactly`); one reads it otherwise.
    */
  enum Held {
    case Not, Exactly, Otherwise
  }

  given Ordering[Held] = Ordering.by(_.ordinal)

  /** Whether a symbol is its class's primary constructor. */
  def primary(symbol: Symbol): Boolean = symbol.owner match {
    case c: ClassSymbol => c.tree.exists(_.rhs.constr.symbol == symbol)
    case _              => false
  }

  /** Whether a tree declares `symbol`, at any depth. */
  def declares(tree: Tree, symbol: Symbol): Boolean = Reentry.search(tree) {
    case d: ValOrDefDef if d.symbol == symbol => Some(true)
    case _                                    => None
  }

  /** The function literals a call passes that a `for` comprehension's generators made: the
    * parameter is written before the collection, the receiver of the call (of an extension method,
    * its first argument, whatever clauses follow it). Its binder is an element of a value the
    * environment may have (the old front end refuses it). An unknown position counts as a
    * generator's.
    */
  def generators(call: Apply)(using Context): List[Symbol] = {
    def receiver(fun: TermTree): Option[TermTree] = fun match {
      case TypeApply(inner, _)                       => receiver(inner)
      case Select(of, _)                             => Some(of)
      case Apply(inner, List(of)) if extended(inner) => Some(of)
      // A later clause of the call (a `using` one after an extension's receiver).
      case Apply(inner, _) => receiver(inner)
      case _               => None
    }
    def extended(fun: TermTree): Boolean = fun match {
      case TypeApply(inner, _) => extended(inner)
      case other               => World.named(other).exists(_.isExtensionMethod)
    }
    def literal(argument: TermTree): Option[DefDef] = argument match {
      case Typed(inner, _)                => literal(inner)
      case Inlined(inner, _, _)           => literal(inner)
      case Targets.Scope.Lambda(function) => Some(function)
      case _                              => None
    }
    for {
      of <- receiver(call.fun).toList
      function <- call.args.flatMap(literal)
      if Shapes
        .parameters(function)
        .exists(p => p.pos.isUnknown || of.pos.isUnknown || p.pos.startOffset < of.pos.startOffset)
    } yield function.symbol
  }

  /** A proper type parameter any type instantiates (a constructor row's: its positions are read
    * without the proofs a bound would give).
    */
  def unbounded(binder: TypeParam)(using Context): Either[Unread, Unit] =
    if (Shapes.unbounded(binder.symbol.declaredBounds)) Right(())
    else Left(Unread.Uncovered(s"a bounded type parameter: ${binder.name}"))

  /** Whether two applications of one class are never subtypes of each other in a body: their
    * arguments are unbounded type parameters held rigid there (a method's, or the class's own
    * through `this`), distinct at some place, whatever the variance (`[A: Show, B: Show]`; the old
    * `ContextualEvidence.unrelated`, distinct atoms).
    */
  private def unrelated(left: Type, right: Type)(using Context): Boolean = {
    def parameters(tpe: Type) = tpe.dealias match {
      case applied: AppliedType =>
        val rigid = applied.args.collect { case ref: TypeRef => Shapes.rigidParameter(ref) }.flatten
        Option.when(rigid.sizeIs == applied.args.size)(rigid)
      case _ => None
    }
    parameters(left).zip(parameters(right)).exists(_.lazyZip(_).exists(_ != _))
  }

  /** Parameters the reading takes as inputs. A contextual one with a name is a value like any other
    * (slice S5): only implicit search tells it apart, and a body is counted, not searched. One
    * without a name (`anonymous`) is found by search alone, among whatever else is in scope: not
    * read yet here (the measured method's or constructor's own is read where nothing competes with
    * it: `searched`).
    */
  def plainInputs(parameters: List[ValDef]): Either[Unread, Unit] =
    parameters
      .find(anonymous)
      .map(p => Unread.Uncovered(s"an anonymous contextual parameter: ${p.name}"))
      .toLeft(())

  def contextual(symbol: TermSymbol): Boolean = symbol.isGivenOrUsing || symbol.isImplicit

  /** The parameters of a method, or of a function literal, that a body inside it may name: a
    * function literal's wildcard parameter (`_ => …`, a `{ case … }`'s own) is no value it can, and
    * an anonymous contextual one is left to implicit search (`plainInputs`).
    */
  def taken(method: DefDef): List[ValDef] =
    Shapes.parameters(method).filter(p => contextual(p.symbol) || !p.name.toString.contains("$"))

  /** A contextual parameter without a name (`using T`, a context bound's evidence): the compiler
    * names it `x$N` or `evidence$N`, and a constructor's is not flagged synthetic.
    */
  def anonymous(parameter: ValDef): Boolean =
    contextual(parameter.symbol) && parameter.name.toString.contains("$")

  /** What a member's kind leaves to slices not read yet. A given alias, an implicit definition and
    * an extension method count as the methods they are (S5); a given instance is a class.
    */
  def later(symbol: TermSymbol)(using Context): Either[String, Unit] =
    if (World.isConstructor(symbol)) Left("a secondary constructor")
    else if (contextual(symbol) && instance(symbol)) Left("a given instance")
    else Right(())

  // A given instance's value or method: it builds the class the compiler names like it.
  private def instance(symbol: TermSymbol)(using Context): Boolean =
    symbol.isModuleVal || symbol.tree.exists {
      case member: ValOrDefDef =>
        val declared = member match {
          case method: DefDef => method.resultTpt
          case value: ValDef  => value.tpt
        }
        World
          .classNamed(declared.toType)
          .exists { c =>
            // Made with the given, at its own place (`Targets.instances`): a class written apart
            // that the given shares its name with is an alias's type.
            c.owner == symbol.owner && c.name.toString == symbol.name.toString &&
            c.tree.forall { made =>
              made.pos.isUnknown || member.pos.isUnknown ||
              made.pos.startOffset == member.pos.startOffset
            }
          }
      case _ => false
    }

  /** The solver's depth budget, the same for every row; its state budget is its own default. */
  val Depth = 64

  /** The solver's step budget for one row: ten times the old front end's for one target
    * (`Measuring.TargetWork`), which refuses rows this front end measured unbudgeted; with none, a
    * few rows run for many minutes. The old reading's work counter is reused so a refusal reads as
    * its own does ("target budget exhausted at …").
    */
  val Work = 3000000L

  /** The solver over a row's bindings within `Work` steps: a row that spends them is refused with
    * the budget named.
    */
  def count(bindings: List[Binding], result: Shape): Count = {
    val work = new AnalysisWork(0, Int.MaxValue, counting = false)
    work.target(Work)
    try Inhabitation.count(bindings, result, inspect = work.step, maxDepth = Depth)
    catch { case stop: AnalysisWork.Exhausted => Count.Unresolved(List(stop.getMessage)) }
  }

}
