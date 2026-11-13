package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Traversers
import tastyquery.Trees.*

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
      _ <- local(tree, methods)
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
            case constructor: DefDef if primary => constructors.count(constructor, around)
            case _                              => counted(tree, around, methods)
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
      methods: List[DefDef]
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
            _ <- Readings.plainInputs(parameters)
            held <- receivers.bindings(around, tree.symbol, env)
            captured <- Traversals.sequence(methods.map { method =>
              val taken = Shapes.parameters(method)
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
          } yield Inhabitation.count(
            values ++ proofs,
            result,
            maxDepth = Readings.Depth
          )
    } yield count
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
    val taken = methods.flatMap(Shapes.parameters).map(_.name.toString).toSet
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
  // name. Left to later steps: a parameter's name that a scope between that method and the
  // definition binds again (a block's definition, an inner method's parameter, a class's
  // member, the definition's own parameter: the definition sees the inner name), and a
  // definition in a case clause, whose pattern may bind names. A definition elsewhere in the
  // method that shares the name hides nothing here.
  private def local(tree: ValOrDefDef, methods: List[DefDef]): Either[String, Unit] =
    methods.headOption match {
      case None            => Right(())
      case Some(outermost) =>
        // The scopes open at each point, innermost first: the definition a scope is of, if any,
        // and the names it binds. A block's names count over the whole block.
        type Scope = (Option[Symbol], Set[String])
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
                if (definition.symbol == tree.symbol) seen = Some(scopes -> (cases > 0))
                super.traverse(definition)
              }
            case other => super.traverse(other)
          }
        }.traverse(outermost)
        seen match {
          case None                => Left("a definition not found in the method around it")
          case Some((_, true))     => Left("a definition in a case clause")
          case Some((open, false)) =>
            methods.iterator
              .flatMap { method =>
                val nearer = open.takeWhile(!_._1.contains(method.symbol)).flatMap(_._2).toSet
                Shapes.parameters(method).map(_.name.toString).filter(nearer)
              }
              .nextOption()
              .map(name => s"a capture a nearer scope binds again: $name")
              .toLeft(())
        }
    }

  // A row's captures, as the report lists them: the names its receivers supply, the parameters
  // of the methods a local definition is written in, and for an
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
    val taken = methods.flatMap(Shapes.parameters).map(_.name.toString)
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

  /** A proper type parameter any type instantiates (a constructor row's: its positions are read
    * without the proofs a bound would give).
    */
  def unbounded(binder: TypeParam)(using Context): Either[Unread, Unit] =
    if (Shapes.unbounded(binder.symbol.declaredBounds)) Right(())
    else Left(Unread.Uncovered(s"a bounded type parameter: ${binder.name}"))

  /** Parameters the reading takes as inputs. A contextual one with a name is a value like any other
    * (slice S5): only implicit search tells it apart, and a body is counted, not searched. One
    * without a name (`using T`, a context bound's evidence) is found by search alone, among
    * whatever else is in scope: not read yet.
    */
  def plainInputs(parameters: List[ValDef]): Either[Unread, Unit] =
    parameters
      // The compiler names it `x$N` or `evidence$N`; a constructor's is not flagged synthetic.
      .find(p => contextual(p.symbol) && p.name.toString.contains("$"))
      .map(p => Unread.Uncovered(s"an anonymous contextual parameter: ${p.name}"))
      .toLeft(())

  def contextual(symbol: TermSymbol): Boolean = symbol.isGivenOrUsing || symbol.isImplicit

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

}
