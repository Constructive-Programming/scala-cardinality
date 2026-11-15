package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Modifiers.Visibility
import tastyquery.Symbols.*
import tastyquery.Traversers
import tastyquery.Trees.*
import tastyquery.Types.Type

/** What a local definition reads of the methods it is written in (`Readings`): the parameters the
  * scopes between hide from it, and what of those scopes is not read yet.
  */
final private[compiled] class Scopes(world: World, shapes: Shapes)(using Context) {
  import Scopes.*

  // What a local definition reads of the methods around it: each of their parameters, by its
  // name, and the parameters a scope between that method and the definition binds again (a
  // block's definition, an inner method's parameter, a class's member or self alias, the
  // definition's own parameter), which it cannot name: those are left out of its environment
  // (a name it cannot write is no capture), unless other code around the definition
  // holds one for it (`kept`). A contextual one stays within implicit search's reach, which a
  // name does not end: not read. A definition elsewhere in the method that shares the name hides
  // nothing. A case clause around the definition is read where its pattern binds nothing and adds
  // no type equation (`patterned`), and a `for` comprehension's generator not at all. The scopes
  // are read from the outermost code the definition is written in: the outermost method, value or
  // class-body statement (whose class is walked).
  def local(tree: ValOrDefDef, methods: List[DefDef]): Either[String, Local] =
    World.owners(tree.symbol.owner).collect { case term: TermSymbol => term }.lastOption.flatMap {
      outermost => if (Readings.primary(outermost)) outermost.owner.tree else outermost.tree
    } match {
      case None       => Right(Local.Plain)
      case Some(root) =>
        Scopes
          .scopesAt(tree, root)
          .map(leftBy(tree, methods, _))
          .toRight("a definition not found in the method around it")
    }

  // What the scopes `found` at the definition leave of the methods it is written in (`local`).
  private def leftBy(tree: ValOrDefDef, methods: List[DefDef], found: Found): Local = {
    val held = methods.flatMap(heldIn(tree, found.open, _))
    Local(
      held.collect { case (p, Some(false)) => p.symbol }.toSet,
      blocked(tree, methods, found, held)
    )
  }

  // The parameters of a method around the definition a nearer scope binds again, each with
  // whether other code still holds it for the definition (`kept`).
  private def heldIn(
      tree: ValOrDefDef,
      open: List[Scope],
      method: DefDef
  ): List[(ValDef, Option[Boolean])] = {
    val nearer = inside(open, method)
    val names = nearer.flatMap(_.names).toSet
    // A template the definition is in holds it: its members are read one by one.
    val path = World.owners(tree.symbol).toSet
    val holders = Scopes.holders(nearer, path)
    // An anonymous contextual parameter has no name to hide.
    Readings
      .taken(method)
      .filter(p => !Readings.anonymous(p) && names(p.name.toString))
      .map(p => p -> kept(p, holders, tree.symbol))
  }

  // What of the scopes around the definition is not read yet, if any.
  private def blocked(
      tree: ValOrDefDef,
      methods: List[DefDef],
      found: Found,
      held: List[(ValDef, Option[Boolean])]
  ): Option[String] = {
    // A search from the definition meets a given a scope between it and a method around it
    // offers before that method's anonymous parameter.
    val offered = methods.collectFirst {
      case m
          if Readings.taken(m).exists(Readings.anonymous) &&
            inside(found.open, m).exists(_.offers) =>
        s"an anonymous contextual parameter beside a scope that may offer a given: ${m.name}"
    }
    val unknown = methods.exists { method =>
      Readings
        .taken(method)
        .exists(p => inside(found.open, method).exists(_.unsure(p.name.toString)))
    }
    Option
      .when(methods.exists(m => found.generators(m.symbol)))(
        "a binder of an enclosing for-comprehension"
      )
      .orElse(offered)
      .orElse(Option.when(unknown)("a capture an import not read may hide"))
      .orElse(found.around.flatMap(patterned(_, tree.symbol)).headOption)
      .orElse(held.collectFirst {
        case (p, _) if Readings.contextual(p.symbol) =>
          s"a contextual capture a nearer scope binds again: ${p.name}"
      })
      .orElse(held.collectFirst {
        case (p, None) =>
          s"a capture a nearer scope binds again, held by other code: ${p.name}"
      })
  }

  // Whether a parent's constructor call reads a parameter only as the argument of a public field
  // that nothing overrides, whose type as the call sees it `holds`: the parameter's type, where the
  // field then holds it for the template's members, or a top type, where it shows nothing of it.
  private def passes(parent: Apply, parameter: ValDef)(holds: Type => Boolean): Boolean = {
    def field(p: TermSymbol) = p.owner.owner.asClass.getDecl(p.name).exists { f =>
      f.isParamAccessor && f.isPublic &&
      world.overriding(f.owner.asClass, f) == ClosedProgram.Overriding.No &&
      seen(f.asTerm, parent).exists(holds)
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

  // Whether a hidden parameter is still a value where the definition `site` is (a held
  // capture), `None` where that is not read: concrete code adds no choice only where it
  // computes from what the environment holds, and a hidden parameter is not among that. A holder
  // that reads it and is the parameter itself at its type (`def seed: Boolean = b`, a `lazy val`, a
  // given), and that the definition can name, keeps it; at a type with one value it shows nothing;
  // a parent call passing it to a field is read the same way (`passes`); any other reading is not
  // read, a method the definition is in included (the definition may call it). A `throw`'s operand
  // reads nothing a result is built from, unless a `try` around it may catch what it throws.
  private def kept(
      parameter: ValDef,
      holders: List[(Tree, Boolean)],
      site: Symbol
  ): Option[Boolean] =
    Traversals
      .sequence(
        holders
          .filter((holder, _) => reads(parameter, site)(holder, caught = false))
          .map(keeps(parameter, site))
      )
      .toOption
      .map(_.contains(true))

  // What one holder that reads the parameter, and whether the definition can name it, says of it.
  private def keeps(parameter: ValDef, site: Symbol)(held: (Tree, Boolean)): Either[Unit, Boolean] =
    held match {
      case (Alias(at, of), nameable) if of == parameter.symbol =>
        keptAs(nameable, at.isSameType(parameter.tpt.toType), oneValued(site)(at))
      case (parent: Apply, nameable) =>
        keptAs(
          nameable,
          passes(parent, parameter)(_.isSameType(parameter.tpt.toType)),
          passes(parent, parameter)(oneValued(site))
        )
      case _ => Left(())
    }

  private def oneValued(site: Symbol)(at: Type): Boolean =
    shapes.of(at, Shapes.Env.of(Nil, site)).exists {
      case _: Inhabitation.Shape.Singleton => false
      case shape => TopTypes.terminalGoal(TopTypes.polarized(shape, false))
    }

}

private[compiled] object Scopes {

  /** What a local definition's scopes leave of the methods around it: the parameters it cannot
    * reach, and what of the scopes is not read yet. That refuses the row only where its result has
    * more than one value, which needs nothing of the environment.
    */
  final case class Local(hidden: Set[Symbol], blocked: Option[String])

  object Local {
    val Plain: Local = Local(Set.empty, None)
  }

  // A scope open at a point (`scopesAt`): the definition it is of, if any, the names it binds (a
  // self alias among them), and what it holds that the definition may read through. A block's
  // names count over the whole block.
  final private case class Scope(
      of: Option[Symbol],
      names: Set[String],
      holders: List[Tree] = Nil,
      // What else may bind a holder's name there: a member a template inherits, or an import of
      // the scope.
      shadows: String => Boolean = _ => false,
      // Whether a search from inside it may meet a given it declares, imports or inherits.
      offers: Boolean = false,
      // The names an import of the scope may bind that are not read (`importing`).
      unsure: String => Boolean = _ => false,
      // Whether it is a template's body, whose parents' fields `this` selects.
      template: Boolean = false
  )

  // What `scopesAt` finds at the definition: the scopes `open` there, innermost first, the case
  // clauses `around` it and the function literals a `for` comprehension's generators made.
  final private case class Found(
      open: List[Scope],
      around: List[CaseDef],
      generators: Set[Symbol]
  )

  // Walks `root` to the definition: the scopes open there, innermost first, the case clauses
  // around it and the function literals a `for` comprehension's generators made anywhere in
  // `root`; none where the definition is not found.
  private def scopesAt(tree: ValOrDefDef, root: Tree)(using Context): Option[Found] = {
    val walk = new Walk(tree)
    walk.traverse(root)
    walk.found
  }

  final private class Walk(tree: ValOrDefDef)(using Context) extends Traversers.TreeTraverser {
    private val primary = Readings.primary(tree.symbol)
    private var scopes = List.empty[Scope]
    private var cases = List.empty[CaseDef]
    private var seen = Option.empty[(List[Scope], List[CaseDef])]
    private var generators = Set.empty[Symbol]

    def found: Option[Found] = seen.map((open, around) => Found(open, around, generators))

    override def traverse(node: Tree): Unit = node match {
      case clause: CaseDef =>
        cases = clause :: cases
        super.traverse(clause)
        cases = cases.tail
      case call: Apply =>
        generators ++= Scopes.generators(call)
        super.traverse(call)
      case block: Block       => within(blockScope(block))(super.traverse(block))
      case template: ClassDef =>
        constructed(template)
        within(templateScope(template))(super.traverse(template))
      case definition: ValOrDefDef => enter(definition)
      case other                   => super.traverse(other)
    }

    private def within(scope: Scope)(body: => Unit): Unit = {
      val saved = scopes
      scopes = scope :: scopes
      body
      scopes = saved
    }

    private def opened(stats: List[Tree], scope: Scope): Scope = {
      val (imported, unsure) = importing(tree, stats)
      scope.copy(names = scope.names ++ imported, unsure = unsure)
    }

    private def blockScope(block: Block): Scope = {
      val imports = block.stats.exists(_.isInstanceOf[Import])
      val givens = block.stats.exists {
        case d: ValOrDefDef => Readings.contextual(d.symbol)
        case _              => false
      }
      opened(
        block.stats,
        Scope(None, named(block.stats), defined(block.stats), _ => imports, imports || givens)
      )
    }

    // A constructor is called where its class is written, with its own parameters: its
    // positions' values are the caller's, not what the class body names.
    private def constructed(template: ClassDef): Unit =
      if (template.symbol == tree.symbol.owner && primary) {
        val own = Scope(Some(tree.symbol), named(Shapes.parameters(template.rhs.constr)))
        seen = Some((own :: scopes) -> cases)
      }

    private def templateScope(template: ClassDef): Scope = {
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
      val givens = inheritsGiven(template.symbol)
      val holders = template.rhs.parents ++ defined(body)
      val offers = imports || givens || !World.ownSelf(template.symbol)
      opened(
        body,
        Scope(None, named(body) ++ alias ++ members, holders, shadows, offers, template = true)
      )
    }

    private def enter(definition: ValOrDefDef): Unit = {
      val own = definition match {
        case method: DefDef => Shapes.parameters(method).map(_.name.toString).toSet
        case _: ValDef      => Set.empty[String]
      }
      within(Scope(Some(definition.symbol), own)) {
        if (definition.symbol == tree.symbol && !primary) seen = Some(scopes -> cases)
        super.traverse(definition)
      }
    }

  }

  // Whether a template declares or inherits a contextual member.
  private def inheritsGiven(template: ClassSymbol)(using Context): Boolean =
    template.linearization.exists(_.declarations.exists {
      case term: TermSymbol => Readings.contextual(term)
      case _                => false
    })

  private def named(stats: List[Tree]) = stats.collect {
    case d: ValOrDefDef =>
      d.name.toString
  }.toSet

  // What of a scope's statements may hold a value: its definitions. An expression statement
  // has no effect and no name, so nothing reads what it computes.
  private def defined(stats: List[Tree]) = stats.filter {
    case _: (ValOrDefDef | ClassDef) => true
    case _                           => false
  }

  // The term names the imports among statements written before the definition bind, and
  // the names that is not read for: every name where a position is not known or a
  // wildcard's qualifier's class is not, else a wildcard's members of restricted access,
  // imported only where accessible. A wildcard binds the public members that are no given
  // and that no other selector of its import names. A capture of such a name cannot be
  // named past the import (Scala refers to neither, an ambiguity, where both bind it).
  private def importing(tree: ValOrDefDef, stats: List[Tree])(using
      Context
  ): (Set[String], String => Boolean) = {
    val read = stats.collect { case i: Import => i }.map(imported(tree, _))
    val names = read.flatten.flatMap(_._1).toSet
    val unsure = read.flatten.flatMap(_._2).toSet
    (names, name => read.contains(None) || unsure(name))
  }

  // What one import binds and leaves unsure; none where a position is not known.
  private def imported(tree: ValOrDefDef, i: Import)(using
      Context
  ): Option[(Set[String], Set[String])] =
    if (i.pos.isUnknown || tree.pos.isUnknown) None
    else if (i.pos.startOffset > tree.pos.startOffset) Some(Set.empty[String] -> Set.empty)
    else
      i.selectors
        // A wildcard is spelled `_` too; a selector renamed to `_` binds nothing.
        .filterNot(s => s.isGiven || !s.isWildcard && s.rename.toString == "_")
        .foldLeft(Option(Set.empty[String] -> Set.empty[String])) { (sofar, s) =>
          sofar
            .zip(
              if (s.isWildcard) wildcard(tree, i)
              else explicit(i, s.name.toString, s.rename.toString)
            )
            .map { case ((a, b), (c, d)) => (a ++ c, b ++ d) }
        }

  private def qualifier(i: Import)(using Context) = i.expr.tpe.widenTermRef match {
    case tpe: Type => World.classNamed(tpe)
    case _         => None
  }

  private def terms(c: ClassSymbol)(using Context) =
    c.linearization.flatMap(_.declarations).collect { case m: TermSymbol => m }

  private def wildcard(tree: ValOrDefDef, i: Import)(using Context) = {
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
  private def explicit(i: Import, name: String, rename: String)(using Context) =
    qualifier(i) match {
      case Some(c) if terms(c).exists(_.name.toString == name) =>
        Some(Set(rename) -> Set.empty)
      case Some(_) => Some(Set.empty[String] -> Set.empty[String])
      case None    => Some(Set.empty[String] -> Set(rename))
    }

  // The scopes open at the definition that are nearer than a method around it.
  private def inside(open: List[Scope], method: DefDef) =
    open.takeWhile(!_.of.contains(method.symbol))

  // Each holder of the `nearer` scopes, and whether the definition can name it: no scope nearer
  // than its own binds its name again or may.
  private def holders(nearer: List[Scope], path: Set[Symbol]): List[(Tree, Boolean)] =
    nearer.zipWithIndex.flatMap((scope, i) => holding(scope, nearer.take(i), path))

  private def holding(
      scope: Scope,
      inner: List[Scope],
      path: Set[Symbol]
  ): List[(Tree, Boolean)] = {
    def again(name: String) = inner.exists(s => s.names(name) || s.shadows(name))
    scope.holders
      .filterNot(enclosingLiteral(path))
      .collect {
        case d: ValOrDefDef                 => d -> !again(d.name.toString)
        case c: ClassDef if !path(c.symbol) => c -> false
        // A parent call holds what it passes in fields `this` selects where no
        // template is nearer.
        case other if !other.isInstanceOf[ClassDef] =>
          other -> (scope.template && !inner.exists(_.template))
      }
  }

  // A function literal the definition is in has no name: the definition cannot call
  // it, so what else it reads holds nothing for it.
  private def enclosingLiteral(path: Set[Symbol])(holder: Tree): Boolean = holder match {
    case d: DefDef => path(d.symbol) && d.name.toString.startsWith("$anonfun")
    case _         => false
  }

  // Whether a holder reads the parameter in a value a result may be built from (`kept`).
  private def reads(parameter: ValDef, site: Symbol)(holder: Tree, caught: Boolean)(using
      Context
  ): Boolean =
    Reentry.search(holder) {
      case t: Try if !caught                  => Some(reads(parameter, site)(t, caught = true))
      case _: Throw if !caught                => Some(false)
      case d: ValOrDefDef if d.symbol == site => Some(false)
      case node if World.named(node).contains(parameter.symbol) => Some(true)
      case _                                                    => None
    }

  // A holder that the definition can name (`nameable`) and holds the parameter at its type
  // (`same`) keeps it; one at a type with one value (`oneValued`) shows nothing of it; else not read.
  private def keptAs(
      nameable: Boolean,
      same: => Boolean,
      oneValued: => Boolean
  ): Either[Unit, Boolean] =
    if (nameable && same) Right(true) else if (oneValued) Right(false) else Left(())

  // A parent's field type as the parent call sees it: a type parameter of the parent is the
  // argument the call gives it (`new F[A](a)`'s `f: X` is an `A`), through an alias too.
  private def seen(field: TermSymbol, parent: Apply)(using Context): Option[Type] =
    parent.tpe match {
      case call: Type =>
        field.typeAsSeenFrom(call) match {
          case tpe: Type => Some(tpe)
          case _         => None
        }
      case _ => None
    }

  // A case clause's pattern changes the context in two ways not read yet: a
  // binder may be the scrutinee or a part of a value the environment has, and where a type
  // parameter is in scope a constructor, typed or stable-identifier pattern may add a type
  // equation.
  private def patterned(clause: CaseDef, measured: Symbol)(using Context): Option[String] = {
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

  /** The function literals a call passes that a `for` comprehension's generators made: the
    * parameter is written before the collection, the receiver of the call (of an extension method,
    * its first argument, whatever clauses follow it). Its binder is an element of a value the
    * environment may have, which is not read yet. An unknown position counts as a generator's.
    */
  private def generators(call: Apply)(using Context): List[Symbol] =
    for {
      of <- receiver(call.fun).toList
      function <- call.args.flatMap(literal)
      if Shapes
        .parameters(function)
        .exists(p => p.pos.isUnknown || of.pos.isUnknown || p.pos.startOffset < of.pos.startOffset)
    } yield function.symbol

  private def receiver(fun: TermTree)(using Context): Option[TermTree] = fun match {
    case TypeApply(inner, _)                       => receiver(inner)
    case Select(of, _)                             => Some(of)
    case Apply(inner, List(of)) if extended(inner) => Some(of)
    // A later clause of the call (a `using` one after an extension's receiver).
    case Apply(inner, _) => receiver(inner)
    case _               => None
  }

  private def extended(fun: TermTree)(using Context): Boolean = fun match {
    case TypeApply(inner, _) => extended(inner)
    case other               => World.named(other).exists(_.isExtensionMethod)
  }

  private def literal(argument: TermTree)(using Context): Option[DefDef] = argument match {
    case Typed(inner, _)                => literal(inner)
    case Inlined(inner, _, _)           => literal(inner)
    case Targets.Scope.Lambda(function) => Some(function)
    case _                              => None
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

}
