package cardinality.compiled

import cardinality.analysis.AnalysisWork
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.inhabitation.{ClosedSpaces, Inhabitation}
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
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
  import Unread.Uncovered

  // A capability record is read as what its template supplies as a receiver: `Shapes` asks
  // `Receivers`, which reads types with `Shapes`.
  private val shapes: Shapes = new Shapes(world, receivers)
  private val bounds = new Bounds(shapes)
  private lazy val receivers: Receivers = new Receivers(world, shapes)
  private lazy val including: Receivers = new Receivers(world, shapes, including = true)
  private val constructors = new Constructors(shapes, stored, receivers)
  private val evidence = new Evidence(world)
  private val scopes = new Scopes(world, shapes)

  // ---- method rows --------------------------------------------------------------------------

  /** The count of a method row, and its captures: the names of what the methods and classes around
    * it supply. A class's row measures its primary constructor.
    */
  def method(tree: ValOrDefDef): Either[String, (Count, List[String])] =
    measured(tree).left.flatMap { why =>
      // Its captures: what the classes and methods around it supply, as for any member row.
      lazy val around = World
        .owners(tree.symbol.owner)
        .collect { case c: ClassSymbol if !c.isModuleClass => c.tree }
        .flatten
        .reverse
      lazy val methods = enclosing(tree.symbol.owner, tree.symbol).fold(_ => Nil, _._1)
      Either.cond(singleGoal(tree), Count.Finite(1) -> captures(tree, around, methods), why)
    }

  // The fallback where `measured` leaves a row for any reason: a member whose goal has one value
  // has one implementation whatever its receivers, scopes or the methods around it hold, as
  // `counted` reads it first. Read only for a result that names no type parameter of the member
  // or of a definition around it, and no type member of a class around it (or of what that class
  // extends), whose meaning only the reading left gives. A constructor, a row `Readings.later`
  // leaves and a `transparent` result (rule X, `inlines.narrowed`) keep their reason.
  private def singleGoal(tree: ValOrDefDef): Boolean =
    !World.isConstructor(tree.symbol) && Readings.later(tree.symbol).isRight &&
      inlines.narrowed(tree.symbol).isEmpty && {
        val scoped = World.owners(tree.symbol).flatMap {
          case c: ClassSymbol =>
            (c :: World.ancestors(c)).flatMap { k =>
              k.typeParams ++ k.declarations.collect { case t: TypeMemberSymbol => t }
            }
          case term: TermSymbol =>
            term.tree.toList.collect { case d: DefDef => Shapes.typeParameters(d) }.flatten
          case _ => Nil
        }
        val result = signature(tree)._3.toType
        !World.mentions(result, scoped.toSet) && shapes
          .of(result, Shapes.Env.of(Nil, tree.symbol))
          .exists(goal => TopTypes.terminalGoal(TopTypes.polarized(goal, true)))
      }

  private def measured(tree: ValOrDefDef): Either[String, (Count, List[String])] = {
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
      scope <- scopes.local(tree, methods)
      around <- aroundOf(tree, owner, top)
      count <-
        // A constructor builds its instance: its row reads its parameters only.
        if (
          constructs || built(
            top,
            // The member of `top` the definition is: itself or the method it is written in.
            (methods :+ tree).find(_.symbol.owner == top).getOrElse(tree).symbol,
            inlined = (tree :: methods).exists(of => inlines.inlineMethod(of.symbol))
          )
        ) row(tree, around, Readings.Written(methods, scope))
        // No call of a member of a template that is never built returns: one implementation.
        else Right(Count.Finite(1) -> captures(tree, around, methods))
    } yield count
  }

  // The classes around a row that hold its environment.
  private def aroundOf(
      tree: ValOrDefDef,
      owner: Symbol,
      top: Symbol
  ): Either[String, List[ClassDef]] =
    if (!World.isConstructor(tree.symbol)) receivers.around(top)
    // A class in a class is built on an instance of the class around it, whose state its
    // constructor's positions may take; an object holds no such state.
    else if (World.inObjects(tree.symbol.owner)) Right(Nil)
    else receivers.around(owner)

  // The count of a row whose templates are built, and its captures.
  private def row(
      tree: ValOrDefDef,
      around: List[ClassDef],
      written: Readings.Written
  ): Either[String, (Count, List[String])] =
    (tree match {
      case constructor: DefDef if World.isConstructor(tree.symbol) =>
        constructed(constructor, around, written)
      case _ => bounded(counted(tree, around, written, _))
    }).map(_ -> captures(tree, around, written.methods)).left.flatMap {
      // A row the model refuses (`?`) lists no captures: it has no count to attribute
      // them to.
      case Unread.Refused(reason) => Right(Count.Unresolved(List(reason)) -> Nil)
      case Uncovered(what)        => Left(what)
    }

  // A constructor's count. Its body is the class's, which another implementation need not share,
  // and no instance exists yet whose members or parents could offer a given.
  private def constructed(
      constructor: DefDef,
      around: List[ClassDef],
      written: Readings.Written
  ): Either[Unread, Count] = {
    val parameters = Shapes.parameters(constructor)
    val owner = constructor.symbol.owner.owner
    for {
      _ <- evidence.searched(parameters, parameters, Some(Nil))
      // A class written in a body captures the parameters of the methods around it; a
      // secondary constructor's self-invocation sees other scopes: not read yet.
      _ <- Either.cond(
        written.methods.isEmpty || Readings.primary(constructor.symbol),
        (),
        Uncovered(s"a secondary constructor of a class written in a body: ${owner.name}")
      )
      _ <- written.scope.blocked.map(Uncovered(_)).toLeft(())
      env <- environment(constructor, around, written.methods)
      taken <- captured(written, env, parameters, Some(Nil))
      count <- constructors.count(constructor, around, typeBinders(written.methods), taken)
    } yield count
  }

  // A `transparent inline` result is refused first (rule X: callers see the expansion's type).
  // A goal with one value has one implementation whatever the environment holds (v1.md §2), so
  // the result is read next, before what an inline body does (a one-valued goal settles it);
  // else the count is the solver's over the parameters and what the receivers hold.
  private def counted(
      tree: ValOrDefDef,
      around: List[ClassDef],
      written: Readings.Written,
      receivers: Receivers
  ): Either[Unread, Count] = {
    val (_, parameters, declared) = signature(tree)
    // The count over what the receivers `kept` hold.
    def solved(env: Shapes.Env, result: Shape, kept: List[ClassDef]) =
      for {
        // After the one-valued result: such a result needs nothing of the environment.
        _ <- written.scope.blocked.map(Uncovered(_)).toLeft(())
        _ <- inlines.refusal(tree.symbol).map(Unread.Refused(_)).toLeft(())
        wrapping = evidence.binders(tree, declared.toType)
        _ <- evidence.searched(parameters, parameters, wrapping)
        found <- receiverEvidence(tree, around, parameters)
        held <- receivers.bindings(kept, tree.symbol, env, found)
        taken <- captured(written, env, parameters, wrapping)
        inputs <- shapes.inputs(parameters, env)
        // Under the proofs of the bounds of the parameters the result or a value mentions.
        count <- bounds.count(
          held ++ taken ++ parameters.lazyZip(inputs).map(Readings.bound(tree)),
          result,
          env
        )
      } yield count
    for {
      _ <- inlines.narrowed(tree.symbol).map(Unread.Refused(_)).toLeft(())
      env <- environment(tree, around, written.methods)
      result <- shapes.of(declared.toType, env)
      count <-
        if (TopTypes.terminalGoal(TopTypes.polarized(result, true))) Right(Count.Finite(1))
        else
          solved(env, result, around).left.flatMap {
            // A sum receiver not read cannot change a goal of infinitely many literals once the
            // context is not absurd: the solver's ω over the rest proves the rest is not, and an
            // enum with a case built from nothing keeps it so (finite programs bound it at ω): a
            // sum receiver adds choices only to a goal already countable.
            case unread @ Uncovered(why)
                if why.startsWith(Receivers.Unsplit) &&
                  ClosedSpaces.count(result).map(Count.of).contains(Count.Countable) =>
              val unsplit = unsplitReceivers(around, env)
              if (unsplit.forall(r => builtFromNothing(r.symbol, tree.symbol)))
                solved(env, result, around.diff(unsplit)).toOption
                  .filter(_ == Count.Countable)
                  .toRight(unread)
              else Left(unread)
            case unread => Left(unread)
          }
    } yield count
  }

  // For a receiver's evidence, code between a local definition and its class (a method, a
  // value's initializer, a class-body statement) may declare givens or import, and a
  // context-function result's binders are not compared with it: not read yet.
  private def receiverEvidence(
      tree: ValOrDefDef,
      around: List[ClassDef],
      parameters: List[ValDef]
  ): Either[Unread, Set[Symbol]] =
    if (
      !World.owners(tree.symbol.owner).takeWhile(!_.isClass).exists(_.isTerm) &&
      !evidence.functionResult(signature(tree)._3.toType)
    )
      evidence.receiverSearch(around, tree.symbol, parameters)
    else Right(Set.empty[Symbol])

  // The receivers `Receivers.supplied` words as sums not read.
  private def unsplitReceivers(around: List[ClassDef], env: Shapes.Env): List[ClassDef] =
    around.filter { r =>
      Receivers.split(r.symbol) && (shapes.receiver(r.symbol, env) match {
        case Left(_: Unread.Refused) => true
        case _                       => false
      })
    }

  // The parameters of the methods a local definition is written in that it may name, as values of
  // its environment. Their anonymous ones compete with the contextual parameters of the methods
  // inside them and the definition's own (`parameters`, under a result's `wrapping` binders);
  // `Scopes.local` has read the scopes between. A function literal's `_` the source writes
  // (`(_: A) ?=> …`) is keyed by a literal the report names otherwise: not read yet.
  private def captured(
      written: Readings.Written,
      env: Shapes.Env,
      parameters: List[ValDef],
      wrapping: Option[List[Type]]
  ): Either[Unread, List[Binding]] =
    Traversals
      .sequence(written.methods.tails.collect {
        case method :: inner =>
          val taken = Readings.taken(method).filterNot(p => written.scope.hidden(p.symbol))
          val deeper = (method :: inner).flatMap(Shapes.parameters) ++ parameters
          val spelled =
            if (!Readings.literal(method.symbol)) Nil
            else taken.filter(p => Readings.anonymous(p) && !p.pos.isZeroExtent)
          Readings
            .plainInputs(spelled)
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

  // An enum without type parameters (a case of a generic one may be of one instantiation alone)
  // one of whose cases the reading shows built from nothing: a value case, or a class case, each
  // parameter of it and of the enum of a closed nonempty space, its templates ever built. A sealed
  // trait is not taken (not read yet); a recursive field is not shown nonempty.
  private def builtFromNothing(c: ClassSymbol, site: Symbol): Boolean = {
    def ground(k: ClassSymbol) = k.tree.exists(t =>
      Shapes.parameters(t.rhs.constr).forall { p =>
        shapes
          .of(p.tpt.toType, Shapes.Env(Map.empty, site))
          .toOption
          .flatMap(ClosedSpaces.count)
          .exists(!_.isZero)
      }
    )
    // Every template that builds the case is built (register D26): a value case's companion and
    // anonymous class, a class case's own.
    def built(k: ClassSymbol) = !world.unbuilt(k, Some(site))
    c.isEnum && c.typeParams.isEmpty && ground(c) && built(c) && c.sealedChildren.exists {
      case _: TermSymbol  => Sums.singletons(c).forall(t => built(t.symbol))
      case k: ClassSymbol => ground(k) && built(k)
    }
  }

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
          // instance as a member does, its constructor parameters among what the class holds (it
          // runs inside the template). The parents' arguments, which the primary
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

  // A row's captures, as the report lists them: the names its receivers supply, the parameters
  // of the methods a local definition is written in (one a nearer scope hides among them: the
  // report names what the code around it binds), and for an extension method the parameters of
  // its `extension` clause, written before the method.
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
  // by its owner and position (`<using#i@method>`); one of a function literal the compiler
  // made around a context-function body by the definition whose result that is
  // (`<context#i@owner>`), counted over such literals from it inward.
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

  // Whether every template a member runs in is ever built (register D26: code of a template
  // never built runs in no program): its own and each one its instances carry, an inner
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

  // The methods a local definition is written in, outermost first, and what their scopes leave
  // of them (`Scopes.local`).
  final private case class Written(methods: List[DefDef], scope: Scopes.Local)

  // A value of the environment: a parameter of a definition, of the measured one or of a method it
  // is written in.
  private def bound(of: ValOrDefDef)(parameter: ValDef, shape: Shape) =
    Binding(s"${of.symbol.displayFullName}.${parameter.name}", shape, Some(parameter.name.toString))

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

  /** The solver's depth budget, the same for every row; its state budget is its own default. */
  val Depth = 64

  /** The solver's step budget for one row; with none, a few rows run for many minutes. A row that
    * spends it reads "target budget exhausted at …".
    */
  val Work = 3000000L

  /** The solver over a row's bindings within `Work` steps: a row that spends them is refused with
    * the budget named.
    */
  def count(bindings: List[Binding], result: Shape): Count = {
    val work = new AnalysisWork(Work)
    try Inhabitation.count(bindings, result, inspect = work.step, maxDepth = Depth)
    catch { case stop: AnalysisWork.Exhausted => Count.Unresolved(List(stop.getMessage)) }
  }

}
