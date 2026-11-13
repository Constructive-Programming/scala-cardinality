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
  * local definition is written in (`hiddenCaptures` says which of them it cannot read), and what
  * the classes around it hold (`Receivers`; a concrete member around it computes from what the
  * environment already holds, and an object holds nothing), over the types `Shapes` reads and the
  * type parameters as `Shapes.binder` reads them.
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
    val primary = tree.symbol.owner match {
      case c: ClassSymbol => c.tree.exists(_.rhs.constr.symbol == tree.symbol)
      case _              => false
    }
    // A constructor runs where its class is declared: the instance it builds does not exist yet.
    val owner = if (primary) tree.symbol.owner.owner else tree.symbol.owner
    for {
      _ <- if (primary) Right(()) else Readings.later(tree.symbol)
      // A constructor row reads the classes around its class; a member reads its receivers.
      // A local definition reads the methods it is written in, then the classes around them.
      (methods, top) <- if (primary) Right(Nil -> owner) else enclosing(owner)
      hidden <- hiddenCaptures(tree, methods)
      around <-
        if (!primary) receivers.around(top)
        // A class in a class is built on an instance of the class around it, whose state its
        // constructor's positions may take; an object holds no such state.
        else
          inTemplates(owner).flatMap(_ =>
            if (World.inObjects(tree.symbol.owner)) Right(Nil) else receivers.around(owner)
          )
      _ <- inherited(methods, hidden, top)
      count <-
        // A constructor builds its instance: its row reads its parameters only.
        if (
          primary || built(
            top,
            // The member of `top` the definition is: itself or the method it is written in.
            (methods :+ tree).find(_.symbol.owner == top).getOrElse(tree).symbol,
            inlined = (tree :: methods).exists(of => inlines.inlineMethod(of.symbol))
          )
        )
          (tree match {
            case constructor: DefDef if primary =>
              constructorSearch(constructor).flatMap(_ => constructors.count(constructor, around))
            case _ => counted(tree, around, methods, hidden)
          }).map(_ -> captures(tree, around, methods)).left.flatMap {
            // A row the model refuses lists no captures, as the old front end's does not.
            case Unread.Refused(reason) => Right(Count.Unresolved(List(reason)) -> Nil)
            case Uncovered(what)        => Left(what)
          }
        // No call of a member of a template that is never built returns: one implementation.
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
      hidden: Set[Symbol]
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
            _ <- inlines.refusal(tree.symbol).map(Unread.Refused(_)).toLeft(())
            _ <- searched(declared.toType, parameters)
            held <- receivers.bindings(around, tree.symbol, env)
            captured <- Traversals.sequence(methods.map { method =>
              val taken = Shapes.parameters(method).filterNot(p => hidden(p.symbol))
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
  private def enclosing(owner: Symbol): Either[String, (List[DefDef], Symbol)] = {
    val owners = World.owners(owner)
    val innermost = owners.find(!_.isTerm).getOrElse(owner)
    Traversals
      .sequence(owners.collect { case term: TermSymbol => term }.map { term =>
        term.tree match {
          // A function literal's parameter may be a generator's binder, which is not read.
          case Some(literal: DefDef) if literal.name.toString.startsWith("$anonfun") =>
            Left("a definition in a function literal")
          // A constructor's body runs before the instance holds what its members read.
          case Some(constructor: DefDef) if World.isConstructor(constructor.symbol) =>
            Left("a definition in a constructor's body")
          case Some(method: DefDef) => Right(Some(method))
          // A member value's initializer runs while its template is built: where that always
          // throws, whether the definition was called first is the body reading's to say.
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
  // A parameter a nearer scope hides already (`hiddenCaptures`) is not read there either.
  private def inherited(
      methods: List[DefDef],
      hidden: Set[Symbol],
      top: Symbol
  ): Either[String, Unit] = {
    val taken =
      methods.flatMap(Shapes.parameters).filterNot(p => hidden(p.symbol)).map(_.name.toString).toSet
    // Every class or object around the member that is itself inside a method.
    val members = World
      .owners(top)
      .collect { case c: ClassSymbol if World.owners(c.owner).exists(_.isTerm) => c }
      .flatMap(_.linearization)
      .flatMap(_.declarations)
      .map(_.name.toString)
    members.find(taken).map(name => s"a capture an inherited member hides: $name").toLeft(())
  }

  // The parameters of the methods around a local definition that it cannot read, which the
  // environment drops: those a scope between their method and the definition binds again (a
  // block's definition, an inner method's parameter, a class's member, the definition's own
  // parameter: the definition sees the inner name) and that no definition it sees keeps
  // (`kept`). An anonymous contextual parameter is found by search, not by name. Not read yet: a
  // parameter a self alias between may hide (the old front end reads the outer one there), and a
  // definition in a case clause, whose pattern may bind names.
  private def hiddenCaptures(
      tree: ValOrDefDef,
      methods: List[DefDef]
  ): Either[String, Set[Symbol]] =
    methods.headOption match {
      case None            => Right(Set.empty)
      case Some(outermost) =>
        // A scope open at a point: the definition it is of, if any, the names it binds, the self
        // alias it gives, and what it holds that the definition may read through. A block's
        // names count over the whole block.
        final case class Scope(
            of: Option[Symbol],
            names: Set[String],
            holders: List[Tree] = Nil,
            alias: Option[String] = None,
            // What else may bind a holder's name there: a member a template inherits, or an
            // import of the scope.
            shadows: String => Boolean = _ => false
        )
        var scopes = List.empty[Scope]
        var cases = 0
        var seen = Option.empty[(List[Scope], Boolean)]
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
              cases += 1
              super.traverse(clause)
              cases -= 1
            case block: Block =>
              val imports = block.stats.exists(_.isInstanceOf[Import])
              within(Scope(None, named(block.stats), block.stats, shadows = _ => imports))(
                super.traverse(block)
              )
            case template: ClassDef =>
              val body = template.rhs.body
              val alias = template.rhs.self.map(_.name.toString)
              val imports = body.exists(_.isInstanceOf[Import])
              val inherited =
                template.symbol.linearization.tail.flatMap(_.declarations).map(_.name.toString)
              val shadows = (name: String) => imports || inherited.contains(name)
              within(Scope(None, named(body), template.rhs.parents ++ body, alias, shadows))(
                super.traverse(template)
              )
            case definition: ValOrDefDef =>
              val own = definition match {
                case method: DefDef => Shapes.parameters(method).map(_.name.toString).toSet
                case _: ValDef      => Set.empty[String]
              }
              within(Scope(Some(definition.symbol), own)) {
                if (definition.symbol == tree.symbol) seen = Some(scopes -> (cases > 0))
                super.traverse(definition)
              }
            case other => super.traverse(other)
          }
        }.traverse(outermost)
        // A template the definition is in holds it: its members are read one by one.
        val around = World.owners(tree.symbol).toSet
        seen match {
          case None                => Left("a definition not found in the method around it")
          case Some((_, true))     => Left("a definition in a case clause")
          case Some((open, false)) =>
            Traversals
              .sequence(methods.flatMap { method =>
                val nearer = open.takeWhile(!_.of.contains(method.symbol))
                val names = nearer.flatMap(_.names).toSet
                val aliases = nearer.flatMap(_.alias).toSet
                // Each holder, and whether the definition can name it: no scope nearer than its
                // own binds its name again or may.
                val holders = nearer.zipWithIndex.flatMap { (scope, i) =>
                  def again(name: String) = nearer.take(i).exists { s =>
                    s.names(name) || s.alias.contains(name) || s.shadows(name)
                  }
                  scope.holders.collect {
                    case d: ValOrDefDef                         => d -> !again(d.name.toString)
                    case c: ClassDef if !around(c.symbol)       => c -> false
                    case other if !other.isInstanceOf[ClassDef] => other -> false
                  }
                }
                Shapes.parameters(method).filterNot(Readings.anonymous).collect {
                  case p if aliases(p.name.toString) =>
                    Left(s"a capture a self alias may hide: ${p.name}")
                  // Implicit search reaches a contextual one whatever hides its name.
                  case p if names(p.name.toString) && Readings.contextual(p.symbol) =>
                    Left(s"a hidden contextual parameter: ${p.name}")
                  case p if names(p.name.toString) =>
                    kept(p, holders, tree.symbol).map(Option.unless(_)(p.symbol))
                }
              })
              .map(_.flatten.toSet)
        }
    }

  // Whether a hidden parameter is still a value where the definition `site` is (the old
  // `HeldCaptures`): concrete code adds no choice only where it computes from what the
  // environment holds, and a hidden parameter is not among that. A holder that reads it and is
  // the parameter itself at its type, and that the definition can name, keeps it; at a type with
  // one value it shows nothing; any other reading is refused, a method the definition is in
  // included (the definition may call it). A `throw`'s operand reads nothing a result is built
  // from, unless a `try` around it may catch what it throws.
  private def kept(
      parameter: ValDef,
      holders: List[(Tree, Boolean)],
      site: Symbol
  ): Either[String, Boolean] = {
    def reads(holder: Tree, caught: Boolean): Boolean =
      Reentry.search(holder) {
        case t: Try if !caught                                    => Some(reads(t, caught = true))
        case _: Throw if !caught                                  => Some(false)
        case d: ValOrDefDef if d.symbol == site                   => Some(false)
        case node if World.named(node).contains(parameter.symbol) => Some(true)
        case _                                                    => None
      }
    def oneValued(at: Type) = shapes.of(at, Shapes.Env.of(Nil, site)).exists {
      case _: Shape.Singleton => false
      case shape              => TopTypes.terminalGoal(TopTypes.polarized(shape, false))
    }
    Traversals
      .sequence(holders.filter((holder, _) => reads(holder, caught = false)).map {
        case (Readings.Alias(at, of), true)
            if of == parameter.symbol && at.isSameType(parameter.tpt.toType) =>
          Right(true)
        case (Readings.Alias(at, of), _) if of == parameter.symbol && oneValued(at) => Right(false)
        case _ => Left(s"a hidden capture a definition reads: ${parameter.name}")
      })
      .map(_.contains(true))
  }

  // A row's captures, as the report lists them: the names its receivers supply, the parameters
  // of the methods a local definition is written in (a hidden one too, as the old front end lists
  // it), and for an extension method the parameters of its `extension` clause, written before the
  // method.
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
    val taken = methods.flatMap(Shapes.parameters).map(_.name.toString)
    (receivers.names(around, tree.symbol) ++ taken ++ group).distinct.sorted
  }

  // Whether every owner of a constructor up to the package is a class or an object.
  private def inTemplates(owner: Symbol): Either[String, Unit] = owner match {
    case _: PackageSymbol => Right(())
    case c: ClassSymbol   => inTemplates(c.owner)
    case other            => Left(s"an enclosing definition: ${other.name}")
  }

  // Whether every template a member runs in is ever built (register D26, the old
  // `ReceiverCapabilities.construction`): its own and each one its instances carry, an inner
  // class its outer instance, an anonymous class the receiver of the code it is made in, a member
  // object the instance it belongs to; a template a static object encloses is built without the
  // object. A template whose construction, or an ancestor's of the module, always throws, the
  // measured member's own initializer aside, leaves no instance. An inline member is expanded
  // where it is called, which builds no static object: such an owner does not stop the call.
  private def built(owner: Symbol, measured: Symbol, inlined: Boolean): Boolean = {
    def static(c: ClassSymbol) = c.isModuleClass && World.inObjects(c)
    // A static object holds a template written as its member without building it; a method of
    // it that the template is made in runs only once the object is built.
    def carried(c: ClassSymbol): List[ClassSymbol] =
      c :: World
        .owners(c.owner)
        .collectFirst { case o: ClassSymbol => o }
        .filterNot(o => static(o) && (inlined || c.owner == o))
        .toList
        .flatMap(carried)
    owner match {
      case c: ClassSymbol if !(inlined && static(c)) =>
        !carried(c).exists(world.unbuilt(_, Some(measured)))
      case _ => true
    }
  }

}

private[compiled] object Readings {

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

  /** A definition that is a parameter and nothing else: a value, or a method without clauses, whose
    * body is the parameter's name, with its declared type.
    */
  private object Alias {

    def unapply(tree: Tree)(using Context): Option[(Type, Symbol)] = tree match {
      case d: ValOrDefDef if d match {
            case m: DefDef => m.paramLists.isEmpty
            case _: ValDef => true
          } =>
        World.rhs(d).flatMap(Shapes.named).map(World.declared(d) -> _)
      case _ => None
    }

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
