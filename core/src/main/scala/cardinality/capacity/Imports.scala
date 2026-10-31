package cardinality.capacity

import scala.meta.*

import cardinality.types.*

// Import and export clauses as Scala scopes them (docs/research/stored-value-names.md). An import
// binds the statements after it in its block or template, never the ones before. An export defines
// members of its template, so it binds the whole body. A definition of the body itself outranks
// both — Scala reports a name bound by a definition and an import of one scope only when the
// definition is in an outer scope, so a program that compiles never means the import there.

/** A wildcard clause `import a.b.*` or `export a.b.*`: the path it names (empty when it is not a
  * stable path), as written, the names its importer excludes (`import a.{W => _, *}`) — for an
  * export, also every name its body or a body inside defines — and the body it is written in.
  */
final private[cardinality] case class Wildcard(
    path: List[TypeName],
    written: String,
    excluded: Set[TypeName],
    body: Option[Tree] = None,
    exported: Boolean = false,
) {

  /** Whether two wildcards are written in one body, where both binding a name is an ambiguity. */
  def beside(other: Wildcard): Boolean = body.exists(b => other.body.exists(_ eq b))

  /** Whether two wildcards are the same clause. */
  def same(other: Wildcard): Boolean = written == other.written && beside(other)

}

/** The clauses in scope: the names explicit clauses bind, with where they come from; the wildcards,
  * latest first; and, for a file read with the other files of its package, the names only those
  * files define — which a wildcard of this file outranks, being a binding of higher precedence than
  * another compilation unit's package member.
  */
final private[cardinality] case class Imports(
    named: Map[TypeName, String] = Map.empty,
    wildcards: List[Wildcard] = Nil,
    foreign: Set[TypeName] = Set.empty,
) {

  /** The same clauses inside a body that defines `names`: an export over them no longer binds them.
    */
  def definingInside(names: Set[TypeName]): Imports =
    if (names.isEmpty) this
    else
      copy(wildcards =
        wildcards.map(w => if (w.exported) w.copy(excluded = w.excluded ++ names) else w)
      )

  /** These clauses with `inner`'s, which are nested in them or written after them; a clause already
    * in scope (a view applied again over the scope it built) is not repeated.
    */
  def ++(inner: Imports): Imports =
    Imports(
      named ++ inner.named,
      inner.wildcards.filterNot(w => wildcards.exists(_.same(w))) ++ wildcards,
      foreign ++ inner.foreign
    )

}

private[cardinality] object Imports {

  val none: Imports = Imports()

  /** The clauses each statement of a body reads with: the body's exports, and the imports written
    * before it — none of them binding a name the body itself defines.
    */
  def before(stats: List[Stat]): Stat => Imports = {
    val own = Walk.typeNames(stats) ++ Walk.termsOf(stats).keySet
    val exported = clauses(stats.collect { case e: Export => e.importers }.flatten, "exported")
      .definingInside(own)
    val positions = new java.util.IdentityHashMap[Stat, Imports]
    val _ = stats.foldLeft(exported) { (acc, st) =>
      positions.put(st, acc)
      st match {
        case i: Import => acc ++ clauses(i.importers, "imported")
        case _         => acc
      }
    }
    st => {
      val at = Option(positions.get(st)).getOrElse(exported)
      at.copy(named = at.named -- own)
    }
  }

  // What the importers of one clause bind: each selected name with its provenance, and one wildcard
  // per importer that has one, minus the names that importer hides.
  private def clauses(importers: List[Importer], verb: String): Imports =
    importers.foldLeft(none) { (acc, importer) =>
      val ref = importer.ref.syntax
      val named = importer.importees.collect {
        case Importee.Name(name) => TypeName.of(name.value) -> s"$verb from $ref.${name.value}"
        case Importee.Rename(from, to) =>
          TypeName.of(to.value) -> s"$verb from $ref.${from.value}"
      }
      val hidden = importer.importees.collect { case Importee.Unimport(n) => TypeName.of(n.value) }
      val wildcard = importer.importees.collectFirst {
        case _: Importee.Wildcard =>
          Wildcard(
            Namespaces.path(importer.ref).getOrElse(Nil),
            s"$ref.*",
            hidden.toSet,
            importer.parent.flatMap(_.parent),
            exported = verb == "exported"
          )
      }
      acc ++ Imports(
        named.map { case (n, from) => n -> s"$from, not followed" }.toMap,
        wildcard.toList,
      )
    }

}
