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
    private val table: Library.Packages,
    private val visible: Int,
    private val overlay: Map[List[TypeName], Scope],
    private val declared: Map[List[TypeName], Set[TypeName]],
    private val unsolved: Set[List[TypeName]],
    private[cardinality] val synonyms: SealedFamilies.Synonyms = SealedFamilies.Synonyms.none,
    private val filled: Set[List[TypeName]] = Set.empty,
    private val packageMembers: Map[List[TypeName], PackageMembers] = Map.empty,
) {

  // A package's solved scope, as the eager fold would hold it here: the round before of the cycle
  // being solved, or a component before the `visible` one, solved on first use. Handed out detached:
  // a reading notes into its own copy, never into a scope another reading (or worker) shares.
  private def solvedAt(path: List[TypeName]): Option[Scope] =
    overlay.get(path).orElse(table.solved(path, visible)).map(_.detached)

  // Every package path the source set opens or nests inside: `a.b.c` makes `a` and `a.b` packages.
  private val paths: Set[List[TypeName]] = declared.keySet.flatMap(_.inits)

  /** Whether a supplied file declares a member of the package: it is then read as complete (the
    * closed-package assumption). One the sources spell only as the prefix of a deeper package holds
    * whatever a dependency declares there.
    */
  private[cardinality] def complete(path: List[TypeName]): Boolean = filled(path)

  /** What a clause over the package imports of the top-level members its files define. */
  private[cardinality] def exposure(path: List[TypeName]): Exposure =
    packageMembers.get(path).fold(Exposure.none)(_.exposure)

  /** How a bare name binds as a member of a package the read's clauses open, innermost first: a
    * definition of its files, under its own name in the package's scope; the reason a package of a
    * cycle the library is still solving gives; or the reason what the package holds beyond its
    * files' own definitions (`PackageMembers`) may bind the name.
    */
  private[cardinality] def member(name: TypeName, world: World): Option[Binding] =
    opened(world.opened).iterator
      .flatMap { path =>
        solvedAt(path)
          .filter(_.defines(name))
          .map(Binding.Elsewhere(_, name))
          .orElse(pendingMember(path, name).map(Binding.Clause(_)))
          .orElse(
            Option
              .unless(declared.get(path).exists(_(name)))(packageMembers.get(path))
              .flatten
              .flatMap(_.bind(name, path))
          )
      }
      .nextOption()

  /** What a path's first name the package's files define no term or subpackage of names as a member
    * of the package — through its export clauses or its package object's parents (see
    * `PackageMembers.head`).
    */
  private[cardinality] def head(path: List[TypeName]): Option[Namespaces.Target] =
    packageMembers.get(path).flatMap(_.head(path))

  /** The reason a member of one package has no value yet: a package of a cycle the library is still
    * solving defines it.
    */
  private[cardinality] def pendingMember(
      path: List[TypeName],
      name: TypeName
  ): Option[Definition.Unbound] =
    Option.when(unsolved(path) && declared.get(path).exists(_(name)))(
      Definition.Unbound.Refusal(
        s"cross-package cycle not solved: ${(path :+ name).map(_.value).mkString(".")}"
      )
    )

  private[cardinality] def isPackage(path: List[TypeName]): Boolean = paths(path)

  private[cardinality] def pkg(path: List[TypeName]): Option[Scope] = solvedAt(path)

  // The same library with other packages in view (`Packages.solve`).
  private def view(
      visible: Int,
      overlay: Map[List[TypeName], Scope],
      unsolved: Set[List[TypeName]]
  ) =
    new Library(table, visible, overlay, declared, unsolved, synonyms, filled, packageMembers)

  // The opened packages, innermost first; a source with no package clause reads in the empty
  // package, whose members are in scope.
  private def opened(opened: List[List[TypeName]]): List[List[TypeName]] =
    if (opened.isEmpty) List(Nil) else opened

}

object Library {

  val empty: Library =
    new Library(
      new Packages(Map.empty, Vector.empty, Map.empty),
      0,
      Map.empty,
      Map.empty,
      Set.empty
    )

  /** Every source's top-level definitions, solved package by package as reads need them. */
  def of(sources: List[scala.meta.Source]): Library = {
    val parts = sources.zipWithIndex
      .flatMap((source, unit) => Walk.parts(source.stats, unit))
      .groupBy(_.path)
    val declared = parts.view.mapValues(_.flatMap(p => Walk.typeNames(p.stats)).toSet).toMap
    val filled = parts.collect {
      case (path, found)
          if found.exists(p => Trees.declaresPackageMember(p.stats, p.packageObject.nonEmpty)) =>
        path
    }.toSet
    // A package object's parents and export clauses add members to its package.
    val packageMembers = parts.view.mapValues(PackageMembers.of).toMap
    val reads = parts.map((path, found) => path -> Reads.of(path, found, parts.keySet))
    // Every alias and renaming the sources write: one may name a sealed family a subtype extends
    // through it.
    val synonyms = SealedFamilies.Synonyms.of(sources)
    val components = Reads.components(parts.keySet.toList.sortBy(_.mkString(".")), reads).toVector
    val table = new Packages(parts, components, reads)
    val library =
      new Library(
        table,
        components.size,
        Map.empty,
        declared,
        Set.empty,
        synonyms,
        filled,
        packageMembers
      )
    table.bind(library)
    library
  }

  /** The packages' solutions, each component of the read graph solved once, on first use, against
    * the components before it in the order the eager fold solved them (a package after every
    * package it reads, a cycle together): what it reads is solved first, on demand, and a component
    * after it is not in view - as in the fold. Shared: a component's solution is a lazy value
    * (thread-safe), and solving one reads only components before it, so no two wait on each other.
    */
  final private[cardinality] class Packages(
      parts: Map[List[TypeName], List[Walk.Part]],
      components: Vector[List[List[TypeName]]],
      reads: Map[List[TypeName], Set[List[TypeName]]]
  ) {

    private val componentOf: Map[List[TypeName], Int] =
      components.zipWithIndex.flatMap((component, i) => component.map(_ -> i)).toMap

    @volatile private var whole: Library = scala.compiletime.uninitialized

    private[Library] def bind(library: Library): Unit = whole = library

    final private class Cell(i: Int) {
      lazy val solution: Map[List[TypeName], Scope] = solve(i)
    }

    private val cells = Vector.tabulate(components.size)(new Cell(_))

    def solved(path: List[TypeName], visible: Int): Option[Scope] =
      componentOf.get(path).filter(_ < visible).flatMap(i => cells(i).solution.get(path))

    // A cycle of packages (or a package that reads itself by a qualified path) is solved in rounds,
    // each against the round before; any other package once. A read down a chain of the cycle
    // longer than the rounds stops at a pending member, which says so — never a guess.
    private def solve(i: Int): Map[List[TypeName], Scope] = {
      val component = components(i)
      val cyclic = component.size > 1 || component.exists(p => reads(p)(p))
      val rounds = if (cyclic) component.size + 3 else 1
      val start = whole.view(i, Map.empty, component.toSet)
      Iterator
        .iterate(start) { behind =>
          whole.view(
            i,
            component.map { path =>
              val base =
                Scope.openedLens.replace(List(path))(Scope.libraryLens.replace(behind)(Scope.empty))
              path -> Walk.solvedParts(parts(path), base)
            }.toMap,
            Set.empty
          )
        }
        .drop(rounds)
        .next()
        .overlay
    }

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
