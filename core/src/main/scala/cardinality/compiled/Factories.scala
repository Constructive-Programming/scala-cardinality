package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Natural}
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*

/** Rule B for a final class no constructor callable from the site builds (v1.md §2, register D42),
  * as the scalameta front end reads it: its value is one of what the site may call to build one,
  * over the fields the site observes.
  *
  *   - A builder is a secondary constructor callable from the site, or a factory: a method or a
  *     single `val` of a template of the module callable there (no method's local definition),
  *     other than the site and the definitions around it, whose result (through blocks, local
  *     `val`s, branches and ascriptions) constructs the class (`new C(…)`, a case class's `apply`),
  *     or calls a method that does, followed through two calls. One that declares the class as its
  *     result and is read as no construction is a factory too.
  *   - Each builder builds at most its parameters' space where it is closed: a method of an object
  *     or a package, read as a construction; one value for a `val` of one and for a definition read
  *     as no construction; no known bound for a definition of a class or trait (its receiver is an
  *     input too) or whose body is not supplied.
  *   - The observed fields are the constructor parameters the site may select, or that a member it
  *     may call reads, directly or through members it may not call; synthetic members and those
  *     restating one every value has observe nothing. None observed is one value.
  *   - The class's space is the largest of min(builder, observed fields): the fields' own as soon
  *     as one builder reaches it, else the largest where every bound is known and comparable.
  */
final private[compiled] class Factories(world: World)(using Context) {
  import Unread.Uncovered
  import Factories.Read

  /** The space of a final class's values as the site may build and observe them. */
  def space(
      tree: ClassDef,
      site: TermSymbol,
      env: Shapes.Env,
      shapes: Shapes
  ): Either[Unread, Card] = {
    val name = tree.symbol.displayFullName
    def closed(parameters: List[ValDef]) =
      shapes
        .inputs(parameters, env)
        .toOption
        .flatMap(read => ClosedSpaces.count(Shape.Product(read)))
    val made = builders(tree, site)
    if (made.isEmpty)
      Left(
        Uncovered(s"a capability record with no constructor or factory the site may call: $name")
      )
    else
      closed(observed(tree, site))
        .toRight(Uncovered(s"a capability record whose observed fields' space is not read: $name"))
        .flatMap(fields =>
          Factories
            .largest(fields, made.map(_.flatMap(closed)))
            .toRight(
              Uncovered(s"a capability record whose builders' spaces cannot be compared: $name")
            )
        )
  }

  /** What the site may call to build a class, one entry per builder: `Some(parameters)`, the
    * parameters whose space bounds what it builds (a secondary constructor, a factory of an object
    * or a package); `Some(Nil)`, one value (a `val` of one, a definition read as no construction);
    * `None`, no known bound (a factory without a body, with type parameters of its own, or on a
    * varying receiver).
    */
  def builders(tree: ClassDef, site: Symbol): List[Option[List[ValDef]]] = {
    val secondaries = tree.rhs.body.collect {
      case d: DefDef
          if World.isConstructor(d.symbol) && d.symbol != tree.rhs.constr.symbol &&
            World.callable(d.symbol, site) =>
        Some(Shapes.parameters(d))
    }
    val factories = built(tree.symbol).collect {
      case (definition, read)
          if !World.owners(site).contains(definition.symbol) &&
            reached(definition.symbol, tree.symbol, site) =>
        (read, definition) match {
          case (Read.Unsupplied, _)                                => None
          case (Read.Other, _)                                     => Some(Nil)
          case (_, d) if !World.inObjects(d.symbol)                => None
          case (_, d: DefDef) if Shapes.typeParameters(d).nonEmpty => None
          case (_, d: DefDef)                                      => Some(Shapes.parameters(d))
          case (_, _: ValDef)                                      => Some(Nil)
        }
    }
    secondaries ++ factories
  }

  // Whether the site may call a definition and reach every template around it; a protected one
  // from a subclass too. A restriction on the class built or on a template around it is
  // disregarded (rule B).
  private def reached(symbol: Symbol, c: ClassSymbol, site: Symbol): Boolean =
    World.owners(symbol).filterNot(World.owners(c).contains).forall {
      case s: TermOrTypeSymbol =>
        World.callable(s, site) || World.owners(site).exists {
          case k: ClassSymbol => s.owner.isClass && k.isSubClass(s.owner.asClass)
          case _              => false
        }
      case _ => true
    }

  /** The constructor parameters the site observes on a value it is given. With `outside`, for a row
    * with no measuring site, also those code of the module outside the class's body may read
    * (`mentionedOutside`), and every field of a case class, which a pattern may extract.
    */
  def observed(tree: ClassDef, site: Symbol, outside: Boolean = false): List[ValDef] = {
    val members = tree.rhs.body.collect { case m: ValOrDefDef => m }
    val bySymbol = members.map(m => (m.symbol: Symbol) -> m).toMap
    @annotation.tailrec
    def read(found: Set[Symbol], pending: List[Tree], seen: Set[Symbol]): Set[Symbol] =
      pending match {
        case Nil       => found
        case m :: rest =>
          val next = Factories.mentioned(m).filterNot(seen)
          read(found ++ next, rest ++ next.toList.flatMap(bySymbol.get), seen ++ next)
      }
    val open = members.filter { m =>
      !m.symbol.isParamAccessor && !m.symbol.isSynthetic && !World.restates(m.symbol) &&
      World.selectable(m.symbol, site)
    }
    // A class or object in the class the site may reach reads what its members read.
    val nested = tree.rhs.body.collect {
      case k: ClassDef if World.selectable(k.symbol, site) => k
    }
    // What code outside the class names, a private member of it included, reads in turn.
    val elsewhere = if (outside) mentionedOutside(tree.symbol) else Set.empty[Symbol]
    val names = read(
      elsewhere,
      open ++ nested ++ elsewhere.flatMap(bySymbol.get),
      open.map(m => m.symbol: Symbol).toSet ++ elsewhere
    )
    val accessors = tree.symbol.declarations.collect {
      case f: TermSymbol if f.isParamAccessor => f.name -> f
    }.toMap
    // A field is read where it, or a member it implements, is named.
    Shapes.parameters(tree.rhs.constr).filter { p =>
      outside && tree.symbol.isCaseClass || accessors.get(p.name).exists { f =>
        names(f) || f.allOverriddenSymbols.exists(names) || World.selectable(f, site)
      }
    }
  }

  // The symbols the module's code outside a class's body names: its companion's, another class's,
  // a default argument's, whether or not public code reaches the mention.
  private def mentionedOutside(c: ClassSymbol): Set[Symbol] = mentions.getOrElseUpdate(
    c, {
      val found = Set.newBuilder[Symbol]
      val outside = new TreeTraverser {
        override def traverse(tree: Tree): Unit = tree match {
          case k: ClassDef if k.symbol == c => ()
          case other                        =>
            found ++= World.named(other)
            super.traverse(other)
        }
      }
      world.module.filter(_.owner.isPackage).flatMap(_.tree).foreach(outside.traverse)
      found.result()
    }
  )

  private val mentions = scala.collection.mutable.Map.empty[ClassSymbol, Set[Symbol]]

  // The definitions of the module that build `c`, each with how its body is read.
  private def built(c: ClassSymbol): List[(ValOrDefDef, Read)] =
    cache.getOrElseUpdate(
      c,
      definitions.flatMap { definition =>
        World.rhs(definition) match {
          case Some(body) if Factories.constructs(body, c, depth = 2) =>
            Some(definition -> Read.Construction)
          case body if World.classNamed(World.declared(definition)).contains(c) =>
            Some(definition -> (if (body.nonEmpty) Read.Other else Read.Unsupplied))
          case _ => None
        }
      }
    )

  private val cache = scala.collection.mutable.Map.empty[ClassSymbol, List[(ValOrDefDef, Read)]]

  // Every method and `val` member of a template of the module that no method holds: constructors,
  // accessors and synthetic members aside.
  private lazy val definitions: List[ValOrDefDef] =
    world.module.toList
      .flatMap(_.tree)
      .flatMap(World.classes)
      .filter(k => World.owners(k.symbol).forall(o => o.isClass || o.isPackage))
      .flatMap(_.rhs.body.collect {
        case m: ValOrDefDef
            if !World.isConstructor(m.symbol) && !m.symbol.isParamAccessor &&
              !m.symbol.isSynthetic =>
          m
      })

}

private[compiled] object Factories {

  /** How a definition's body is read: as a construction, as none, or not at all (not supplied). */
  private enum Read {
    case Construction, Other, Unsupplied
  }

  // The symbols a tree names.
  private def mentioned(tree: Tree)(using Context): Set[Symbol] = {
    val found = Set.newBuilder[Symbol]
    new TreeTraverser {
      override def traverse(tree: Tree): Unit = {
        found ++= World.named(tree)
        super.traverse(tree)
      }
    }.traverse(tree)
    found.result()
  }

  // Whether a body's result constructs `c`, through at most `depth` delegating calls.
  private def constructs(body: TermTree, c: ClassSymbol, depth: Int)(using Context): Boolean =
    results(body, Map.empty).exists { result =>
      builds(result, c) || depth > 0 && callee(result).exists(_.tree.exists {
        case d: DefDef => d.rhs.exists(constructs(_, c, depth - 1))
        case _         => false
      })
    }

  // A direct construction: `new C(…)` (applied to types and clauses), or a case class's `apply`.
  private def builds(term: TermTree, c: ClassSymbol)(using Context): Boolean = {
    def creator(fun: TermTree): Boolean = fun match {
      case Apply(inner, _)     => creator(inner)
      case TypeApply(inner, _) => creator(inner)
      case Select(New(tpt), _) => World.classNamed(tpt.toType).contains(c)
      case _                   => false
    }
    term match {
      case _: Apply => creator(term) || callee(term).exists(applies(_, c))
      case _        => false
    }
  }

  // A case class's synthetic `apply` in its companion.
  private def applies(method: TermSymbol, c: ClassSymbol)(using Context): Boolean =
    c.isCaseClass && method.name.toString == "apply" && method.isSynthetic &&
      c.companionClass.contains(method.owner)

  // The method a call runs, where the tree names it.
  private def callee(term: TermTree)(using Context): Option[TermSymbol] = term match {
    case Apply(fun, _)                => callee(fun)
    case TypeApply(fun, _)            => callee(fun)
    case reference: TermReferenceTree => World.named(reference).filter(_.isMethod)
    case _                            => None
  }

  // The terms a body may evaluate to: its result through blocks, local `val`s and branches.
  private def results(term: TermTree, locals: Map[Symbol, TermTree])(using
      Context
  ): List[TermTree] = term match {
    case Block(stats, expr) =>
      val vals = stats.collect {
        case v: ValDef if v.rhs.nonEmpty => (v.symbol: Symbol) -> v.rhs.get
      }
      results(expr, locals ++ vals)
    case If(_, yes, no)       => results(yes, locals) ++ results(no, locals)
    case Match(_, cases)      => cases.flatMap(c => results(c.body, locals))
    case Typed(inner, _)      => results(inner, locals)
    case Inlined(inner, _, _) => results(inner, locals)
    case ident: Ident         =>
      World.named(ident).filter(locals.contains) match {
        case Some(local) => results(locals(local), locals - local)
        case None        => List(ident)
      }
    case other => List(other)
  }

  // The largest of min(a builder's space, the fields'): the fields' own as soon as one reaches it,
  // else the largest where each is known and comparable.
  private def largest(observed: Card, builders: List[Option[Card]]): Option[Card] = {
    val capped =
      builders.map(_.flatMap(space => below(space, observed).map(if (_) space else observed)))
    if (capped.contains(Some(observed))) Some(observed)
    else if (capped.contains(None)) None
    else
      capped.flatten.map(Option(_)).reduce { (left, right) =>
        for {
          a <- left
          b <- right
          smaller <- below(a, b)
        } yield if (smaller) b else a
      }
  }

  // Whether one count is below another, where their magnitudes compare: ω is above every exact
  // count, a literal below every symbolic one, and two symbolic ones compare only when equal.
  private def below(a: Card, b: Card): Option[Boolean] =
    if (a == b) Some(false)
    else
      (rank(a), rank(b)) match {
        case (Some(x), Some(y))      => Some(x._1 < y._1 || x._1 == y._1 && x._2 < y._2)
        case (Some((tier, _)), None) => Some(tier == 0)
        case (None, Some((tier, _))) => Some(tier != 0)
        case _                       => None
      }

  // A literal's tier and value, or ω's tier; none for a symbolic count.
  private def rank(card: Card): Option[(Int, BigInt)] = card match {
    case Card.Omega                 => Some(2 -> BigInt(0))
    case Card.Exact(Natural.Lit(n)) => Some(0 -> n)
    case _                          => None
  }

}
