package cardinality.analysis

import scala.meta.*

import cardinality.analysis.model.ScopeTerms
import cardinality.types.Spelling

/** One importer of an import or export clause: where it stands, the names its explicit selectors
  * bind with the path each reaches, the prefixes its wildcard opens, and the names that wildcard
  * leaves out — an unimport (`N as _`) and the original of a rename, which Scala binds only under
  * its new name. A `given` selector (`p.given`, `p.{given T}`) binds the given instances of its
  * prefix, terms of any name (`givens`). An importer whose prefix is no path (`import
  * Outer.this.o.*`) is not indexed, so `unfollowed` stays empty.
  */
final private[cardinality] case class ImportBinding(
    start: Int,
    named: List[(String, List[String])],
    wildcards: List[List[String]],
    hidden: Set[String] = Set.empty,
    givens: List[List[String]] = Nil,
    unfollowed: Option[ImportBinding.Unfollowed] = None
) {

  /** The prefixes this importer's wildcard opens for `name`: none when the name is left out. */
  def opening(name: String): List[List[String]] = if (hidden(name)) Nil else wildcards

}

private[cardinality] object ImportBinding {

  /** What one importer binds: named and renamed selectors their members, a wildcard every member,
    * and a `given` selector the prefix's given instances — terms, under their own names, whatever
    * type `import p.{given T}` selects by (Scala 3.9.0 binds `compose` through `import zzz.given`
    * over another file's package `compose`: probes/review-A7/r11).
    */
  def of(importer: Importer, path: List[String]): ImportBinding = {
    val named = importer.importees.collect {
      case e: Importee.Name   => e.name.value -> (path :+ e.name.value)
      case e: Importee.Rename => e.rename.value -> (path :+ e.name.value)
    }
    val wildcards = importer.importees.collect { case _: Importee.Wildcard => path }
    val givens = Option.when(ScopeTerms.opensGivens(importer))(path).toList
    apply(importer.pos.start, named, wildcards, SourceIndex.hiddenNames(importer), givens)
  }

  /** An importer whose prefix is no path, as written: the names its explicit selectors bind, and
    * whether a wildcard (`any`) or a `given` selector (`givens`, terms only) may bind others.
    */
  final case class Unfollowed(written: String, names: Set[String], any: Boolean, givens: Boolean) {

    /** The wildcard as written, when it may bind the name. */
    def wildcard(name: String, hidden: Set[String]): Option[String] =
      Option.when(any && !hidden(name))(s"$written.*")

    /** The `given` selector as written, when the importer has one. */
    def givenSelector: Option[String] = Option.when(givens)(s"$written.given")

  }

}

/** One importer of an export clause, with what it binds when its prefix is a pure path. */
final private[cardinality] case class ExportClause(
    importer: Importer,
    binding: Option[ImportBinding]
) {

  /** The prefix as written. */
  def prefix: String = Spelling.of(importer.ref)

}

private[cardinality] object ExportClause {

  /** Whether an exporter may define a member under the name whatever its prefix holds: a selector
    * spelling it, or a wildcard that does not leave it out.
    */
  def mayDefine(importer: Importer, name: String): Boolean =
    importer.importees.exists {
      case _: Importee.Wildcard => !SourceIndex.hiddenNames(importer)(name)
      case selector             => SourceIndex.boundName(selector).contains(name)
    }

}
