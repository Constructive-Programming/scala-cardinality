package cardinality.capacity

import scala.meta.*

import cardinality.types.*

/** The definitions the other supplied sources introduce, so a reference can leave its file.
  *
  * A name no body around the read binds resolves as Scala binds it (`Scope.binding`): an explicit
  * clause, which is not followed; a wildcard over a supplied namespace that defines it, or over one
  * no source supplies, which may; then the packages the read's package clauses open, innermost
  * first, which this library holds. A classpath-only name is unresolved. Each package's definitions
  * solve as one system, each reading with what its own file adds (clauses and imports) and with the
  * library behind it, so a read can cross packages. Packages are solved in the order their reads
  * need: a package after every package it reads, and the packages that read each other in a cycle
  * together, in rounds; at the bottom of the rounds, a member of a package of the cycle not solved
  * yet is `pending` — a reason of its own.
  */
final class Library private[cardinality] (
    private val packages: Map[List[TypeName], Scope],
    private val declared: Map[List[TypeName], Set[TypeName]],
    private val unsolved: Set[List[TypeName]],
) {

  // Every package path the source set opens or nests inside: `a.b.c` makes `a` and `a.b` packages.
  private val paths: Set[List[TypeName]] = declared.keySet.flatMap(_.inits)

  /** The package scope a bare name the read's package clauses open resolves to. */
  private[cardinality] def frame(name: TypeName, world: World): Option[Scope] =
    opened(world.opened).flatMap(packages.get).find(_.defines(name))

  /** The reason a bare name the read's package clauses open has no value yet: a package of a cycle
    * the library is still solving defines it.
    */
  private[cardinality] def pending(name: TypeName, world: World): Option[Definition.Unbound] =
    opened(world.opened).iterator.flatMap(pendingMember(_, name)).nextOption()

  /** The same for a member of one package. */
  private[cardinality] def pendingMember(
      path: List[TypeName],
      name: TypeName
  ): Option[Definition.Unbound] =
    Option.when(unsolved(path) && declared.get(path).exists(_(name)))(
      Definition.Unbound.Syntax(
        s"cross-package cycle not solved: ${(path :+ name).map(_.value).mkString(".")}"
      )
    )

  private[cardinality] def isPackage(path: List[TypeName]): Boolean = paths(path)

  private[cardinality] def pkg(path: List[TypeName]): Option[Scope] = packages.get(path)

  // The opened packages, innermost first; a source with no package clause reads in the empty
  // package, whose members are in scope.
  private def opened(opened: List[List[TypeName]]): List[List[TypeName]] =
    if (opened.isEmpty) List(Nil) else opened

}

object Library {

  val empty: Library = new Library(Map.empty, Map.empty, Set.empty)

  /** Every source's top-level definitions, solved package by package. */
  def of(sources: List[scala.meta.Source]): Library = {
    val parts = sources.zipWithIndex
      .flatMap((source, unit) => Walk.parts(source.stats, unit))
      .groupBy(_.path)
    val declared = parts.view.mapValues(_.flatMap(p => Walk.typeNames(p.stats)).toSet).toMap
    val reads = parts.map((path, found) => path -> Reads.of(path, found, parts.keySet))
    Reads
      .components(parts.keySet.toList.sortBy(_.mkString(".")), reads)
      .foldLeft(
        new Library(Map.empty, declared, Set.empty)
      ) { (library, component) =>
        val cyclic = component.size > 1 || component.exists(p => reads(p)(p))
        solve(parts, component, if (cyclic) component.size + 3 else 1, library)
      }
  }

  // A cycle of packages (or a package that reads itself by a qualified path) is solved in rounds,
  // each against the round before; any other package once. A read down a chain of the cycle longer
  // than the rounds stops at a pending member, which says so — never a guess.
  private def solve(
      parts: Map[List[TypeName], List[Walk.Part]],
      component: List[List[TypeName]],
      rounds: Int,
      done: Library
  ): Library = {
    val start = new Library(done.packages, done.declared, component.toSet)
    Iterator
      .iterate(start) { behind =>
        new Library(
          done.packages ++ component.map { path =>
            val base =
              Scope.openedLens.replace(List(path))(Scope.libraryLens.replace(behind)(Scope.empty))
            path -> Walk.solvedParts(parts(path), base)
          },
          done.declared,
          Set.empty
        )
      }
      .drop(rounds)
      .next()
  }

}

// The packages a package's sources may read, itself included when it names itself: those its
// clauses open, those its wildcards and qualified paths name — relative to an opened package or
// from the root — and their enclosing packages. An over-approximation: an extra edge only orders,
// or joins a cycle, needlessly.
private object Reads {

  def of(
      path: List[TypeName],
      found: List[Walk.Part],
      packages: Set[List[TypeName]]
  ): Set[List[TypeName]] = {
    val opened = found.flatMap(_.layer.opened.toList.flatten).distinct
    val written = (found.flatMap(p => p.stats.flatMap(paths)) ++
      found.flatMap(_.layer.imports.wildcards.map(_.path))).map(_.filterNot(_.value == "_root_"))
    val bases = (Nil :: opened).distinct
    val named = for {
      names <- written
      base <- bases
      prefix <- names.inits.toList if prefix.nonEmpty
      candidate = base ++ prefix
      if packages(candidate)
    } yield candidate
    (opened.filter(p => packages(p) && p != path) ++ named).toSet
  }

  // Every stable path the statements write: qualifiers of type references and import clauses.
  private def paths(tree: Tree): List[List[TypeName]] =
    tree.collect {
      case Type.Select(qualifier, _) => Namespaces.path(qualifier).toList
      case importer: Importer        => Namespaces.path(importer.ref).toList
    }.flatten

  /** The strongly connected components of the read graph, each after every component it reads
    * (Tarjan): a package is solved after the packages it reads, a cycle of packages together.
    */
  def components(
      nodes: List[List[TypeName]],
      edges: Map[List[TypeName], Set[List[TypeName]]]
  ): List[List[List[TypeName]]] = {
    val index = scala.collection.mutable.Map.empty[List[TypeName], Int]
    val low = scala.collection.mutable.Map.empty[List[TypeName], Int]
    val stack = scala.collection.mutable.Stack.empty[List[TypeName]]
    val onStack = scala.collection.mutable.Set.empty[List[TypeName]]
    val result = scala.collection.mutable.ListBuffer.empty[List[List[TypeName]]]
    def visit(v: List[TypeName]): Unit = {
      index(v) = index.size
      low(v) = index(v)
      stack.push(v)
      onStack += v
      edges.getOrElse(v, Set.empty).toList.sortBy(_.mkString(".")).foreach { w =>
        if (!index.contains(w)) {
          visit(w)
          low(v) = low(v).min(low(w))
        } else if (onStack(w)) low(v) = low(v).min(index(w))
      }
      if (low(v) == index(v)) {
        val component = List.unfold(true)(more =>
          Option.when(more) {
            val w = stack.pop()
            onStack -= w
            (w, w != v)
          }
        )
        result += component
      }
    }
    nodes.foreach(v => if (!index.contains(v)) visit(v))
    result.toList
  }

}
