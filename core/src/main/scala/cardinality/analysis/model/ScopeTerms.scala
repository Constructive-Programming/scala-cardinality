package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.SourceIndex
import cardinality.types.TypeApplications

/** The term names one scope's own statements define — every variable of a pattern definition (`val
  * (seed, z) = …`) and every extension method included — which hide an outer binding of the name
  * for every reference inside the scope (Scala specification, chapter 2; an export defines members
  * too, register D11). An outer binding a qualified path reaches (`Outer.this.seed`) stays
  * readable; an inner import or inherited member binding the name makes it ambiguous (E049).
  */
private[cardinality] object ScopeTerms {

  /** The names the statements define by declaration or definition. */
  def defined(stats: List[Stat]): Set[String] = definedBy(stats, _ => true)

  /** The names the statements define as values: every definition but an object's, a class's or an
    * enum's, whose name a path may also select a member of.
    */
  def values(stats: List[Stat]): Set[String] = definedBy(stats, !namespace(_))

  private def namespace(stat: Stat): Boolean = stat match {
    case _: (Defn.Object | Defn.Class | Defn.Enum) => true
    case _                                         => false
  }

  /** The names a template's statements pass on to its subclasses: every definition but a `private`
    * or `private[this]` one (a qualified `private[p]` member is inherited).
    */
  def inherited(stats: List[Stat]): Set[String] = definedBy(stats, !privateMember(_))

  private def definedBy(stats: List[Stat], kept: Stat => Boolean): Set[String] =
    definitions(stats).collect { case (name, stat) if kept(stat) => name }.toSet

  private def definitions(stats: List[Stat]): List[(String, Stat)] = stats.flatMap { stat =>
    (stat match {
      case d: Defn.Def                                    => List(d.name.value)
      case d: Decl.Def                                    => List(d.name.value)
      case v: Defn.Val                                    => names(v.pats)
      case v: Defn.Var                                    => names(v.pats)
      case v: Decl.Val                                    => names(v.pats)
      case v: Decl.Var                                    => names(v.pats)
      case o: Defn.Object                                 => List(o.name.value)
      case g: Defn.Given                                  => named(g.name)
      case g: Defn.GivenAlias                             => named(g.name)
      case g: Decl.Given                                  => named(g.name)
      case c: Defn.Class if c.mods.exists(_.is[Mod.Case]) => List(c.name.value)
      case e: Defn.Enum                                   => List(e.name.value)
      case e: Defn.ExtensionGroup                         => extensionMethods(e).map(_.name.value)
      case _                                              => Nil
    }).map(_ -> stat)
  }

  /** The methods an extension group defines. */
  def extensionMethods(group: Defn.ExtensionGroup): List[Defn.Def | Decl.Def] =
    (group.body match {
      case block: Term.Block => block.stats
      case single            => List(single)
    }).collect {
      case d: Defn.Def => d
      case d: Decl.Def => d
    }

  /** Whether a member is invisible to subclasses: `private` or `private[this]`, not `private[p]`.
    */
  def privateMember(stat: Stat): Boolean = stat match {
    case member: Stat.WithMods => unqualifiedPrivate(member.mods)
    case _                     => false
  }

  /** Whether modifiers make a member `private` or `private[this]`. */
  def unqualifiedPrivate(mods: List[Mod]): Boolean = mods.exists {
    case Mod.Private(within) => within.is[Name.Anonymous] || within.is[Term.This]
    case _                   => false
  }

  /** What the statements' export clauses define as terms: the names their explicit selectors spell,
    * or None where a wildcard, a `given` selector (a named given is exported under its name) or a
    * prefix that is no path may define any name.
    */
  def exported(stats: List[Stat]): Option[Set[String]] =
    spelled(stats.collect { case e: Export => e.importers }.flatten)

  /** The same for import clauses: the names their explicit selectors bind, or None where one may
    * bind any name.
    */
  def imported(stats: List[Stat]): Option[Set[String]] =
    spelled(stats.collect { case i: Import => i.importers }.flatten)

  private def spelled(importers: List[Importer]): Option[Set[String]] = {
    val open = importers.exists(importer =>
      SourceIndex.importPath(importer.ref).isEmpty ||
        importer.importees.exists(e =>
          e.is[Importee.Wildcard] || e.is[Importee.Given] || e.is[Importee.GivenAll]
        )
    )
    Option.unless(open)(importers.flatMap(_.importees).flatMap(SourceIndex.boundName).toSet)
  }

  private def names(pats: List[Pat]): List[String] = TypeApplications.boundNames(pats)

  private def named(name: Name): List[String] = Option(name.value).filter(_.nonEmpty).toList

}
