package cardinality.compiled

import cardinality.analysis.AnalysisWork
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Modifiers.Visibility
import tastyquery.Symbols.*
import tastyquery.Traversers
import tastyquery.Trees.*
import tastyquery.Types.{Type, TypeRef}

/** What the compiled front end measures so far, read from symbols and types
  * (docs/plans/compiled-own-types.md, "Front end: design and status", lists the slices): a `Left`
  * names what a row needs that no slice reads yet, and is never a count. A type the counting model
  * refuses is the row's `?` with the model's reason (`Unread.Refused`).
  *
  * Method rows: a method or value whose environment is its own parameters, those of the methods a
  * local definition is written in (`local` says which of them it cannot read), and what the classes
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
  import Readings.Local
  import Unread.Uncovered

  // A capability record is read as what its template supplies as a receiver: `Shapes` asks
  // `Receivers`, which reads types with `Shapes`.
  private val shapes: Shapes = new Shapes(world, receivers)
  private val bounds = new Bounds(shapes)
  private lazy val receivers: Receivers = new Receivers(world, shapes)
  private lazy val including: Receivers = new Receivers(world, shapes, including = true)
  private val constructors = new Constructors(shapes, stored, receivers)
  private val evidence = new Evidence(world)

  // ---- method rows --------------------------------------------------------------------------

  /** The count of a method row, and its captures: the names of what the methods and classes around
    * it supply. A class's row measures its primary constructor.
    */
  def method(tree: ValOrDefDef): Either[String, (Count, List[String])] = {
    // A constructor, primary or secondary, runs where its class is declared: the instance it
    // builds does not exist yet.
    val constructs = World.isConstructor(tree.symbol)
    val owner = if (constructs) tree.symbol.owner.owner else tree.symbol.owner
    for {
      _ <- if (constructs) Right(()) else Readings.later(tree.symbol)
      // A constructor row reads the classes around its class; a member reads its receivers.
      // A local definition, or a class written in a body, reads the methods it is written in,
      // then the classes around them.
      (methods, top) <- enclosing(owner, tree.symbol)
      scope <- local(tree, methods)
      around <-
        if (!constructs) receivers.around(top)
        // A class in a class is built on an instance of the class around it, whose state its
        // constructor's positions may take; an object holds no such state.
        else if (World.inObjects(tree.symbol.owner)) Right(Nil)
        else receivers.around(owner)
      count <-
        // A constructor builds its instance: its row reads its parameters only.
        if (
          constructs || built(
            top,
            // The member of `top` the definition is: itself or the method it is written in.
            (methods :+ tree).find(_.symbol.owner == top).getOrElse(tree).symbol,
            inlined = (tree :: methods).exists(of => inlines.inlineMethod(of.symbol))
          )
        )
          (tree match {
            case constructor: DefDef if constructs =>
              // Its body is the class's, which another implementation need not share, and no
              // instance exists yet whose members or parents could offer a given.
              val parameters = Shapes.parameters(constructor)
              for {
                _ <- evidence.searched(parameters, parameters, Some(Nil))
                // A class written in a body captures the parameters of the methods around it; a
                // secondary constructor's self-invocation sees other scopes: not read yet.
                _ <- Either.cond(
                  methods.isEmpty || Readings.primary(tree.symbol),
                  (),
                  Uncovered(s"a secondary constructor of a class written in a body: ${owner.name}")
                )
                _ <- scope.blocked.map(Uncovered(_)).toLeft(())
                env <- environment(constructor, around, methods)
                taken <- captured(methods, scope, env, parameters, Some(Nil))
                count <- constructors.count(constructor, around, typeBinders(methods), taken)
              } yield count
            case _ => bounded(counted(tree, around, methods, scope, _))
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
      scope: Local,
      receivers: Receivers
  ): Either[Unread, Count] = {
    val (_, parameters, declared) = signature(tree)
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
            wrapping = evidence.binders(tree, declared.toType)
            _ <- evidence.searched(parameters, parameters, wrapping)
            // For a receiver's evidence, code between a local definition and its class (a method,
            // a value's initializer, a class-body statement) may declare givens or import, and a
            // context-function result's binders are not compared with it: not read yet.
            found <-
              if (
                !World.owners(tree.symbol.owner).takeWhile(!_.isClass).exists(_.isTerm) &&
                !evidence.functionResult(declared.toType)
              )
                evidence.receiverSearch(around, tree.symbol, parameters)
              else Right(Set.empty[Symbol])
            held <- receivers.bindings(around, tree.symbol, env, found)
            taken <- captured(methods, scope, env, parameters, wrapping)
            inputs <- shapes.inputs(parameters, env)
            // Under the proofs of the bounds of the parameters the result or a value mentions.
            count <- bounds.count(
              held ++ taken ++ parameters.lazyZip(inputs).map(Readings.bound(tree)),
              result,
              env
            )
          } yield count
    } yield count
  }

  // The parameters of the methods a local definition is written in that it may name, as values of
  // its environment. Their anonymous ones compete with the contextual parameters of the methods
  // inside them and the definition's own (`parameters`, under a result's `wrapping` binders);
  // `local` has read the scopes between. A function literal's `_` the source writes
  // (`(_: A) ?=> …`) is keyed by a literal the report names otherwise: not read yet.
  private def captured(
      methods: List[DefDef],
      scope: Local,
      env: Shapes.Env,
      parameters: List[ValDef],
      wrapping: Option[List[Type]]
  ): Either[Unread, List[Binding]] =
    Traversals
      .sequence(methods.tails.collect {
        case method :: inner =>
          val taken = Readings.taken(method).filterNot(p => scope.hidden(p.symbol))
          val deeper = (method :: inner).flatMap(Shapes.parameters) ++ parameters
          val written =
            if (!Readings.literal(method.symbol)) Nil
            else taken.filter(p => Readings.anonymous(p) && !p.pos.isZeroExtent)
          Readings
            .plainInputs(written)
            .flatMap(_ => evidence.searched(taken, deeper, wrapping))
            // A method around the definition took them before its receiver was built (a by-name
            // type is read past its arrow, as nothing preceding); an inline one is evaluated where
            // it is read, and may be the receiver.
            .flatMap(_ =>
              Traversals.sequence(taken.map { parameter =>
                val before = env.copy(preceding = !parameter.symbol.isInline)
                shapes.of(parameter.tpt.toType, before)
              })
            )
            .map(taken.lazyZip(_).map(Readings.bound(method)))
      }.toList)
      .map(_.flatten)

  // Members of a receiver that may call the measured one leave the row. Taken for free capabilities
  // they bound the count from above (register D31): a count of 0 then is the count.
  private def bounded(count: Receivers => Either[Unread, Count]): Either[Unread, Count] =
    count(receivers).left.flatMap {
      case unread @ Uncovered(why) if why.startsWith(Receivers.Reenters) =>
        count(including).toOption.filter(_.isZero).toRight(unread)
      case unread => Left(unread)
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
    receivers
      .atoms(around)
      .map(receiving => Shapes.Env.of(receiving ++ typeBinders(methods :+ tree), tree.symbol))
  }

  // The type parameters of definitions (`Shapes.binder`).
  private def typeBinders(of: List[ValOrDefDef]) = of.flatMap { definition =>
    signature(definition)._1.map { binder =>
      binder.symbol -> Shapes.binder(definition.symbol, binder.symbol.asInstanceOf[TypeParamSymbol])
    }
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

  // What a local definition reads of the methods around it: each of their parameters, by its
  // name, and the parameters a scope between that method and the definition binds again (a
  // block's definition, an inner method's parameter, a class's member or self alias, the
  // definition's own parameter), which it cannot name: those are left out of its environment
  // (`BodyScopes` and `HeldCaptures` of the old front end), unless other code around the definition
  // holds one for it (`kept`). A contextual one stays within implicit search's reach, which a
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
        // A scope open at a point: the definition it is of, if any, the names it binds (a self
        // alias among them), and what it holds that the definition may read through. A block's
        // names count over the whole block.
        final case class Scope(
            of: Option[Symbol],
            names: Set[String],
            holders: List[Tree] = Nil,
            // What else may bind a holder's name there: a member a template inherits, or an
            // import of the scope.
            shadows: String => Boolean = _ => false,
            // Whether a search from inside it may meet a given it declares, imports or inherits.
            offers: Boolean = false,
            // The names an import of the scope may bind that are not read (`importing`).
            unsure: String => Boolean = _ => false,
            // Whether it is a template's body, whose parents' fields `this` selects.
            template: Boolean = false
        )
        val primary = Readings.primary(tree.symbol)
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
        // The term names the imports among statements written before the definition bind, and
        // the names that is not read for: every name where a position is not known or a
        // wildcard's qualifier's class is not, else a wildcard's members of restricted access,
        // imported only where accessible. A wildcard binds the public members that are no given
        // and that no other selector of its import names. A capture of such a name cannot be
        // named past the import (Scala refers to neither, an ambiguity, where both bind it).
        def importing(stats: List[Tree]): (Set[String], String => Boolean) = {
          def qualifier(i: Import) = i.expr.tpe.widenTermRef match {
            case tpe: Type => World.classNamed(tpe)
            case _         => None
          }
          def terms(c: ClassSymbol) = c.linearization.flatMap(_.declarations).collect {
            case m: TermSymbol => m
          }
          def wildcard(i: Import) = {
            val named = i.selectors.filterNot(_.isWildcard).map(_.name.toString).toSet
            qualifier(i).map { c =>
              val members =
                terms(c).filter(m => !m.isGivenOrUsing && !named(m.name.toString))
              // A private member is imported inside its class or companion, where it is accessible.
              val inside =
                World.owners(tree.symbol).exists(o => o == c || c.companionClass.contains(o))
              val (public, other) = members.partition(_.isPublic)
              val (secret, restricted) = other.partition(m =>
                m.visibility == Visibility.Private || m.visibility == Visibility.PrivateThis
              )
              val bound = if (inside) public ++ secret else public
              (bound.map(_.name.toString).toSet, restricted.map(_.name.toString).toSet)
            }
          }
          // A selector binds a term only where its qualifier has one of its name (a type of that
          // name leaves the term to an outer scope); unsure where the qualifier is not read.
          def explicit(i: Import, name: String, rename: String) = qualifier(i) match {
            case Some(c) if terms(c).exists(_.name.toString == name) =>
              Some(Set(rename) -> Set.empty)
            case Some(_) => Some(Set.empty[String] -> Set.empty[String])
            case None    => Some(Set.empty[String] -> Set(rename))
          }
          val read = stats.collect { case i: Import => i }.map { i =>
            if (i.pos.isUnknown || tree.pos.isUnknown) None
            else if (i.pos.startOffset > tree.pos.startOffset) Some(Set.empty[String] -> Set.empty)
            else
              i.selectors
                // A wildcard is spelled `_` too; a selector renamed to `_` binds nothing.
                .filterNot(s => s.isGiven || !s.isWildcard && s.rename.toString == "_")
                .foldLeft(Option(Set.empty[String] -> Set.empty[String])) { (sofar, s) =>
                  sofar
                    .zip(
                      if (s.isWildcard) wildcard(i)
                      else explicit(i, s.name.toString, s.rename.toString)
                    )
                    .map { case ((a, b), (c, d)) => (a ++ c, b ++ d) }
                }
          }
          val names = read.flatten.flatMap(_._1).toSet
          val unsure = read.flatten.flatMap(_._2).toSet
          (names, name => read.contains(None) || unsure(name))
        }
        def opened(
            names: Set[String],
            stats: List[Tree],
            holders: List[Tree],
            shadows: String => Boolean,
            template: Boolean,
            offers: Boolean
        ) = {
          val (imported, unsure) = importing(stats)
          Scope(None, names ++ imported, holders, shadows, offers, unsure, template)
        }
        // What of a scope's statements may hold a value: its definitions. An expression statement
        // has no effect and no name, so nothing reads what it computes.
        def defined(stats: List[Tree]) = stats.filter {
          case _: (ValOrDefDef | ClassDef) => true
          case _                           => false
        }
        new Traversers.TreeTraverser {
          override def traverse(node: Tree): Unit = node match {
            case clause: CaseDef =>
              cases = clause :: cases
              super.traverse(clause)
              cases = cases.tail
            case call: Apply =>
              generators ++= Readings.generators(call)
              super.traverse(call)
            case block: Block =>
              val imports = block.stats.exists(_.isInstanceOf[Import])
              val givens = block.stats.exists {
                case d: ValOrDefDef => Readings.contextual(d.symbol)
                case _              => false
              }
              within(
                opened(
                  named(block.stats),
                  block.stats,
                  defined(block.stats),
                  _ => imports,
                  false,
                  imports || givens
                )
              )(super.traverse(block))
            case template: ClassDef =>
              // A constructor is called where its class is written, with its own parameters: its
              // positions' values are the caller's, not what the class body names.
              if (template.symbol == tree.symbol.owner && primary) {
                val own = Scope(Some(tree.symbol), named(Shapes.parameters(template.rhs.constr)))
                seen = Some((own :: scopes) -> cases)
              }
              val body = template.rhs.body
              val alias = template.rhs.self.map(_.name.toString)
              val imports = body.exists(_.isInstanceOf[Import])
              val inherited = template.symbol.linearization.tail.flatMap(_.declarations)
              val names = inherited.map(_.name.toString).toSet
              val shadows = (name: String) => imports || names(name)
              // An inherited member hides a name of a scope around the template.
              val members = inherited.collect {
                case member: TermSymbol if !member.isPrivate && !World.isConstructor(member) =>
                  member.name.toString
              }
              val givens = template.symbol.linearization.exists(_.declarations.exists {
                case term: TermSymbol => Readings.contextual(term)
                case _                => false
              })
              val holders = template.rhs.parents ++ defined(body)
              val offers = imports || givens || !World.ownSelf(template.symbol)
              within(opened(named(body) ++ alias ++ members, body, holders, shadows, true, offers))(
                super.traverse(template)
              )
            case definition: ValOrDefDef =>
              val own = definition match {
                case method: DefDef => Shapes.parameters(method).map(_.name.toString).toSet
                case _: ValDef      => Set.empty[String]
              }
              within(Scope(Some(definition.symbol), own)) {
                if (definition.symbol == tree.symbol && !primary) seen = Some(scopes -> cases)
                super.traverse(definition)
              }
            case other => super.traverse(other)
          }
        }.traverse(root)
        seen match {
          case None                 => Left("a definition not found in the method around it")
          case Some((open, around)) =>
            // A template the definition is in holds it: its members are read one by one.
            val path = World.owners(tree.symbol).toSet
            def inside(method: DefDef) = open.takeWhile(!_.of.contains(method.symbol))
            val held = methods.flatMap { method =>
              val nearer = inside(method)
              val names = nearer.flatMap(_.names).toSet
              // Each holder, and whether the definition can name it: no scope nearer than its own
              // binds its name again or may.
              val holders = nearer.zipWithIndex.flatMap { (scope, i) =>
                def again(name: String) =
                  nearer.take(i).exists(s => s.names(name) || s.shadows(name))
                // A function literal the definition is in has no name: the definition cannot call
                // it, so what else it reads holds nothing for it.
                scope.holders
                  .filterNot {
                    case d: DefDef => path(d.symbol) && d.name.toString.startsWith("$anonfun")
                    case _         => false
                  }
                  .collect {
                    case d: ValOrDefDef                 => d -> !again(d.name.toString)
                    case c: ClassDef if !path(c.symbol) => c -> false
                    // A parent call holds what it passes in fields `this` selects where no
                    // template is nearer.
                    case other if !other.isInstanceOf[ClassDef] =>
                      other -> (scope.template && !nearer.take(i).exists(_.template))
                  }
              }
              // An anonymous contextual parameter has no name to hide.
              Readings
                .taken(method)
                .filter(p => !Readings.anonymous(p) && names(p.name.toString))
                .map(p => p -> kept(p, holders, tree.symbol))
            }
            // A search from the definition meets a given a scope between it and a method around it
            // offers before that method's anonymous parameter.
            val offered = methods.collectFirst {
              case m
                  if Readings.taken(m).exists(Readings.anonymous) &&
                    inside(m).exists(_.offers) =>
                s"an anonymous contextual parameter beside a scope that may offer a given: ${m.name}"
            }
            val unknown = methods.exists { method =>
              Readings.taken(method).exists(p => inside(method).exists(_.unsure(p.name.toString)))
            }
            val blocked = Option
              .when(methods.exists(m => generators(m.symbol)))(
                "a binder of an enclosing for-comprehension"
              )
              .orElse(offered)
              .orElse(Option.when(unknown)("a capture an import not read may hide"))
              .orElse(around.flatMap(patterned(_, tree.symbol)).headOption)
              .orElse(held.collectFirst {
                case (p, _) if Readings.contextual(p.symbol) =>
                  s"a contextual capture a nearer scope binds again: ${p.name}"
              })
              .orElse(held.collectFirst {
                case (p, None) =>
                  s"a capture a nearer scope binds again, held by other code: ${p.name}"
              })
            Right(Local(held.collect { case (p, Some(false)) => p.symbol }.toSet, blocked))
        }
    }

  // Whether a parent's constructor call reads a parameter only as the argument of a public field
  // of the parameter's type that nothing overrides, which then holds it for the template's members.
  private def passes(parent: Apply, parameter: ValDef): Boolean = {
    def field(p: TermSymbol) = p.owner.owner.asClass.getDecl(p.name).exists { f =>
      f.isParamAccessor && f.isPublic &&
      world.overriding(f.owner.asClass, f) == ClosedProgram.Overriding.No && (f.declaredType match {
        case tpe: Type => tpe.isSameType(parameter.tpt.toType)
        case _         => false
      })
    }
    val (head, args) = Reentry.unapplied(parent)
    // A secondary constructor's parameter is not the field of its name.
    val held = World.named(head).filter(Readings.primary).toList.flatMap { c =>
      c.paramSymss.flatMap(_.left.toSeq).lazyZip(args).flatMap(_.lazyZip(_)).collect {
        case (p, arg) if World.named(arg).contains(parameter.symbol) && field(p) => arg
      }
    }
    held.nonEmpty && !Reentry.search(parent) { node =>
      if (held.exists(_ eq node)) Some(false)
      else Option.when(World.named(node).contains(parameter.symbol))(true)
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

  // Whether a hidden parameter is still a value where the definition `site` is (the old
  // `HeldCaptures`), `None` where that is not read: concrete code adds no choice only where it
  // computes from what the environment holds, and a hidden parameter is not among that. A holder
  // that reads it and is the parameter itself at its type (`def seed: Boolean = b`, a `lazy val`,
  // a given), and that the definition can name, keeps it; at a type with one value it shows
  // nothing; any other reading is not read, a method the definition is in included (the
  // definition may call it). A `throw`'s operand reads nothing a result is built from, unless a
  // `try` around it may catch what it throws.
  private def kept(
      parameter: ValDef,
      holders: List[(Tree, Boolean)],
      site: Symbol
  ): Option[Boolean] = {
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
        case (parent: Apply, true) if passes(parent, parameter)                     => Right(true)
        case _                                                                      => Left(())
      })
      .toOption
      .map(_.contains(true))
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
    val taken = methods.flatMap(method => Readings.taken(method).map(captureName(method, _)))
    (receivers.names(around, tree.symbol) ++ taken ++ group).distinct.sorted
  }

  // A parameter of a method around a definition by the name the report gives it: an anonymous one
  // by its owner and position, as the old front end keys it (`ContextualEvidence.key`,
  // `ContextBinders`); one of a function literal the compiler made around a context-function body
  // by the definition whose result that is, counted over such literals from it inward.
  private def captureName(method: DefDef, parameter: ValDef): String = {
    def made(symbol: Symbol) = Readings.literal(symbol) && symbol.tree.exists {
      case function: DefDef => Targets.Scope.contextual(function)
      case _                => false
    }
    def index = Shapes.parameters(method).indexOf(parameter)
    if (!Readings.anonymous(parameter)) parameter.name.toString
    else if (!made(method.symbol)) s"<using#$index@${Targets.pathOf(method.symbol)}>"
    else {
      // `made` holds of the method itself, so `literals` is never empty, nor `owner` (a package).
      val (literals, owner) = World.owners(method.symbol).span(made)
      val outer = literals.tail.flatMap(_.tree).collect { case d: DefDef => Shapes.parameters(d) }
      s"<context#${outer.flatten.size + index}@${Targets.pathOf(owner.head)}>"
    }
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

  // A value of the environment: a parameter of a definition, of the measured one or of a method it
  // is written in.
  private def bound(of: ValOrDefDef)(parameter: ValDef, shape: Shape) =
    Binding(s"${of.symbol.displayFullName}.${parameter.name}", shape, Some(parameter.name.toString))

  /** What a local definition's scopes leave of the methods around it: the parameters it cannot
    * reach, and what of the scopes is not read yet. That refuses the row only where its result has
    * more than one value, which needs nothing of the environment.
    */
  final case class Local(hidden: Set[Symbol], blocked: Option[String])

  object Local {
    val Plain: Local = Local(Set.empty, None)
  }

  /** Whether a class is a context function's, which also answers searches for its result. */
  def function(c: ClassSymbol): Boolean = c.displayFullName.startsWith("scala.ContextFunction")

  /** Whether a method is one the compiler made of a function literal. */
  def literal(symbol: Symbol): Boolean = symbol.name.toString.startsWith("$anonfun")

  /** The unbounded type parameter held rigid in a body that a type is, if it is one. */
  def rigid(tpe: Type)(using Context): Option[TypeParamSymbol] = tpe.dealias match {
    case ref: TypeRef => Shapes.rigidParameter(ref)
    case _            => None
  }

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

  /** Parameters the reading takes as inputs. A contextual one with a name is a value like any other
    * (slice S5): only implicit search tells it apart, and a body is counted, not searched. One
    * without a name (`anonymous`) is found by search alone, among whatever else is in scope: read
    * where `searched` tells it apart from its competitors; here, a receiver's that search was not
    * shown to find, and a function literal's `_` the source writes, are not read yet.
    */
  def plainInputs(parameters: List[ValDef]): Either[Unread, Unit] =
    parameters
      .find(anonymous)
      .map(p => Unread.Uncovered(s"an anonymous contextual parameter: ${p.name}"))
      .toLeft(())

  def contextual(symbol: TermSymbol): Boolean = symbol.isGivenOrUsing || symbol.isImplicit

  /** The parameters of a method, or of a function literal, that a body inside it may name: a
    * function literal's wildcard parameter (`_ => …`, a `{ case … }`'s own) is no value it can, and
    * an anonymous contextual one is a value where implicit search finds it (`searched`).
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
