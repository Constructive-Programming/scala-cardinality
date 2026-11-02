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
  * export, also every name its body or a body inside defines — the body it is written in, and its
  * kind. A `given` selector (`import a.given`, `import a.{given T}`) is one that binds only the
  * given instances of its namespace: terms, under their own names.
  */
final private[cardinality] case class Wildcard(
    path: List[TypeName],
    written: String,
    excluded: Set[TypeName],
    body: Option[Tree] = None,
    kind: Wildcard.Kind = Wildcard.Kind.Import,
) {

  def exported: Boolean = kind == Wildcard.Kind.Export

  def givens: Boolean = kind == Wildcard.Kind.Given

  /** Whether two wildcards are written in one body, where both binding a name is an ambiguity. */
  def beside(other: Wildcard): Boolean = body.exists(b => other.body.exists(_ eq b))

  /** Whether two wildcards are the same clause. */
  def same(other: Wildcard): Boolean = written == other.written && beside(other)

}

private[cardinality] object Wildcard {

  /** An import's `*`, an import's `given` selector, or an export's `*`. */
  enum Kind {
    case Import, Given, Export
  }

}

/** An explicit clause's selector, `import a.b.N` or `{N as M}` (or an export): the stable path it
  * reads (empty when it is not one), the member it names, how it reads (`imported from a.b.N`), the
  * clauses its path resolves with — those of its body written before it (`local`), behind those of
  * the bodies around (`outer`) — and the namespaces it binds there: a body's own definition of the
  * name outranks it in that namespace only (`val W` leaves `import a.W` the type `W`).
  */
final private[cardinality] case class Selector(
    path: List[TypeName],
    member: TypeName,
    provenance: String,
    local: Imports,
    outer: Imports = Imports.none,
    types: Boolean = true,
    terms: Boolean = true,
) {

  /** The clauses the selector's path is read with. */
  def prior: Imports = outer ++ local

  /** The selector's provenance, with why the estimate has no declaration behind it. */
  def because(why: String): String = s"$provenance, $why"
}

/** The clauses in scope: the names explicit clauses bind, with their selectors; the wildcards,
  * latest first; and, for a file read with the other files of its package, the names only those
  * files define — which a wildcard of this file outranks, being a binding of higher precedence than
  * another compilation unit's package member.
  */
final private[cardinality] case class Imports(
    named: Map[TypeName, Selector] = Map.empty,
    wildcards: List[Wildcard] = Nil,
    foreign: Set[TypeName] = Set.empty,
) {

  /** The explicit selectors that bind a type name. */
  def types: Map[TypeName, Selector] = named.filter(_._2.types)

  /** The explicit selectors that bind a term name. */
  def terms: Map[TypeName, Selector] = named.filter(_._2.terms)

  /** The same clauses inside a body that defines `names`: an export over them no longer binds them.
    */
  def definingInside(names: Set[TypeName]): Imports =
    if (names.isEmpty) this
    else
      copy(wildcards =
        wildcards.map(w => if (w.exported) w.copy(excluded = w.excluded ++ names) else w)
      )

  /** These clauses with `inner`'s, which are nested in them or written after them; a clause already
    * in scope (a view applied again over the scope it built) is not repeated. An inner selector's
    * path reads with these clauses behind its own body's.
    */
  def ++(inner: Imports): Imports =
    if (inner eq Imports.none) this
    else
      Imports(
        named ++ inner.named.map((n, s) => n -> s.copy(outer = this ++ s.outer)),
        inner.wildcards.filterNot(w => wildcards.exists(_.same(w))) ++ wildcards,
        foreign ++ inner.foreign
      )

}

private[cardinality] object Imports {

  val none: Imports = Imports()

  /** The clauses each statement of a body reads with: the body's exports, which define members and
    * so outrank its imports, and the imports written before it — none binding a name the body
    * itself defines in that namespace. An export's path reads with the imports written before it.
    */
  def before(stats: List[Stat]): Stat => Imports = {
    val types = Walk.typeNames(stats)
    val terms = Walk.termsOf(stats).keySet
    val imported = positions(stats, none)
    val exported = stats
      .foldLeft(none) {
        case (acc, e: Export) => acc ++ clauses(e.importers, "exported", imported(e))
        case (acc, _)         => acc
      }
      .definingInside(types ++ terms)
    val at = positions(stats, exported)
    st => {
      val here = at(st)
      here.copy(named = (here.named ++ exported.named).flatMap { (n, selector) =>
        val kept =
          selector.copy(types = selector.types && !types(n), terms = selector.terms && !terms(n))
        Option.when(kept.types || kept.terms)(n -> kept)
      })
    }
  }

  // The clauses each statement of a body reads with when only its imports count, from `start`.
  private def positions(stats: List[Stat], start: Imports): Stat => Imports = {
    val at = new java.util.IdentityHashMap[Stat, Imports]
    val _ = stats.foldLeft(start) { (acc, st) =>
      at.put(st, acc)
      st match {
        case i: Import => acc ++ clauses(i.importers, "imported", acc)
        case _         => acc
      }
    }
    st => Option(at.get(st)).getOrElse(start)
  }

  // What the importers of one clause bind: each selected name with its selector, read with the
  // clauses written before the clause (`before`), and one wildcard per importer that has one, minus
  // the names that importer hides.
  private def clauses(importers: List[Importer], verb: String, before: Imports): Imports =
    importers.foldLeft(none) { (acc, importer) =>
      val ref = importer.ref.syntax
      val path = Namespaces.path(importer.ref).getOrElse(Nil)
      def selector(member: Name): Selector =
        Selector(path, TypeName.of(member.value), s"$verb from $ref.${member.value}", before)
      val named = importer.importees.collect {
        case Importee.Name(name)       => TypeName.of(name.value) -> selector(name)
        case Importee.Rename(from, to) => TypeName.of(to.value) -> selector(from)
      }
      // A renamed member is no longer importable under its own name, by the wildcard beside it too.
      val hidden = importer.importees.collect {
        case Importee.Unimport(n)     => TypeName.of(n.value)
        case Importee.Rename(from, _) => TypeName.of(from.value)
      }
      def wildcard(written: String, kind: Wildcard.Kind) = Wildcard(
        Namespaces.path(importer.ref).getOrElse(Nil),
        written,
        hidden.toSet,
        importer.parent.flatMap(_.parent),
        kind
      )
      val wildcards = importer.importees.collectFirst {
        case _: Importee.Wildcard =>
          wildcard(
            s"$ref.*",
            if (verb == "exported") Wildcard.Kind.Export else Wildcard.Kind.Import
          )
      } ++ importer.importees.collectFirst {
        case _: (Importee.Given | Importee.GivenAll) => wildcard(s"$ref.given", Wildcard.Kind.Given)
      }
      acc ++ Imports(named.toMap, wildcards.toList)
    }

}
