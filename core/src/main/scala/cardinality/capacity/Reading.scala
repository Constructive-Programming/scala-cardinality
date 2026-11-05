package cardinality.capacity

import scala.meta.*

import cardinality.types.Trees

/** The estimate's reading of one source, which `MethodAnalysis` reads a constructor's parameter
  * types through, so the two halves of a report answer with one number. A parameter type is read
  * where its constructor is written — inside the package clauses, objects, classes, methods and
  * blocks around it, its type parameters and its sibling parameters bound — as its row reads it;
  * anywhere else it would borrow whatever another package defines under the same name.
  */
final class Reading private (private val top: Scope) {

  // Each enclosing body is solved once, however many constructors it holds, and its clauses read
  // once.
  private val bodies = new java.util.IdentityHashMap[Tree, Scope]
  private val clauses = new java.util.IdentityHashMap[Tree, Stat => Imports]

  /** What a signature type is worth in this source, read where `written` sits: the size, when the
    * scope answers the type whole — every name it reads, transitively, bounded — and otherwise the
    * reasons the estimate could not bound it for, which the shape model must not count past. A type
    * rebuilt from a parsed one has no position of its own, so it reads in the scope of the tree it
    * was rebuilt from.
    */
  def read(tpe: Type, written: Tree): Either[List[Definition.Unbound], Size] = {
    // The method side counts a sequence of any non-empty element as ω (its closed algebra), so the
    // tier a finite collection keeps from its element is the stored-value side's alone.
    val probe = Scope.saturatingLens.replace(true)(at(written).measured)
    val size = Counter.typeIn(probe)(tpe)
    if (probe.unbound.isEmpty) Right(size) else Left(probe.unbound)
  }

  // The scope a type reads in at its position: the bodies around its owner, outermost first, each
  // with the clauses written before the statement the type sits in, then the owner's binders and
  // parameters. A type with no position (built rather than parsed) reads at the top.
  private def at(tpe: Tree): Scope = {
    val ancestors = Trees.ancestors(tpe)
    val inside = Trees.identitySet(ancestors)
    val owners = ancestors.takeWhile(!Reading.container(_)) ++
      ancestors.dropWhile(!Reading.container(_)).take(1)
    val binders = owners.collect { case d: Defn => Reading.binders(d) }.flatten
    val parameters = owners.collect { case d: Defn => Walk.parameters(d) }.flatten
    val around = ancestors.drop(owners.size).filter(Reading.container).reverse
    val source = ancestors.lastOption.filter(_.is[Source])
    val body = around
      .foldLeft((source.fold(top)(positioned(top, _, inside.contains)), List.empty[String])) {
        case ((scope, path), container) =>
          val (entered, inner) = enter(scope, path, container)
          (positioned(entered, container, inside.contains), inner)
      }
      ._1
    // An enum case's parameters read in the enum's companion (the case's enum owns them).
    val read = owners
      .collectFirst { case e: Defn.Enum if owners.exists(_.is[Defn.EnumCase]) => e }
      .fold(body)(EnumCompanions.caseScope(body, _))
    Scope.termsLens.modify(_ ++ parameters)(Scope.bindersLens.modify(binders ++ _)(read))
  }

  // A body's scope at the statement of `container` that holds the type: with the clauses written
  // before it.
  private def positioned(scope: Scope, container: Tree, holds: Tree => Boolean): Scope = {
    val stats = Reading.statements(container)
    stats.find(holds).fold(scope) { st =>
      val before = clauses.synchronized {
        Option(clauses.get(container)).getOrElse {
          val read = Imports.before(stats)
          clauses.put(container, read)
          read
        }
      }
      scope.within(Layer(imports = before(st)))
    }
  }

  // One step into an enclosing body, as the walk takes it: a method binds its type parameters and
  // its parameters, a block solves its local definitions.
  private def enter(scope: Scope, path: List[String], container: Tree): (Scope, List[String]) =
    Reading.packageBody(path, container) match {
      case Some((inner, stats)) => (solve(container, stats, Walk.opening(scope, inner)), inner)
      case None                 =>
        container match {
          // A method's parameters are terms nearer than anything a template around inherits.
          case m: Defn.Def =>
            val bound = Scope.bindersLens.modify(Counter.binders(m) ++ _)(scope)
            val params = Walk.parameters(m)
            val nearer =
              Scope.templatesLens.modify(_.map(_.defining(Set.empty, params.map(_._1).toSet)))
            (Scope.termsLens.modify(_ ++ params)(nearer(bound)), path)
          case b: Term.Block => (solve(b, b.stats, scope), path)
          case d: Defn       => (Walk.bodyScope(scope, d)(solve(d, _, _)), path)
          case _             => (scope, path)
        }
    }

  private def solve(tree: Tree, stats: List[Stat], scope: Scope): Scope =
    bodies.synchronized {
      Option(bodies.get(tree)).getOrElse {
        val solved = Walk.solved(stats, scope)
        bodies.put(tree, solved)
        solved
      }
    }

}

object Reading {

  // A definition's binders around a position: an enum case's own and a given instance's clauses'
  // too, whose constructors the method side reads positions of.
  private def binders(d: Defn): List[Binder] = d match {
    case c: Defn.EnumCase => Counter.binders(c.tparamClause.values)
    case g: Defn.Given    => Counter.binders(g.paramClauseGroups.flatMap(_.tparamClause.values))
    case other            => Counter.binders(other)
  }

  def of(s: Source, library: Library = Library.empty): Reading =
    new Reading(Walk.solved(s.stats, Scope.libraryLens.replace(library)(Scope.empty)))

  /** A reading with nothing in scope but the base types: the fallback for a source the index does
    * not hold.
    */
  val bare: Reading = new Reading(Scope.empty)

  private def packageBody(path: List[String], tree: Tree): Option[(List[String], List[Stat])] =
    tree match {
      case st: Stat => Walk.packageBody(path, st)
      case _        => None
    }

  // The statements of a body, where its clauses are written.
  private def statements(tree: Tree): List[Stat] = tree match {
    case s: Source                                                           => s.stats
    case p: Pkg                                                              => p.body.stats
    case d: (Pkg.Object | Defn.Object | Defn.Class | Defn.Trait | Defn.Enum) => d.templ.body.stats
    case b: Term.Block                                                       => b.stats
    case _                                                                   => Nil
  }

  // The trees whose bodies a definition sits in; the nearest one around a parameter type owns it.
  private def container(tree: Tree): Boolean = tree match {
    case _: (Pkg | Pkg.Object | Defn.Object | Defn.Class | Defn.Trait | Defn.Enum) => true
    case _: (Defn.Def | Term.Block)                                                => true
    case _                                                                         => false
  }

}
