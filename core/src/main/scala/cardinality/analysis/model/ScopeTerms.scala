package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.SourceIndex
import cardinality.types.*
import cardinality.types.Trees.hasCase

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

  /** An object, or the companion Scala gives a case class or an enum: a path selects through it. */
  def namespace(stat: Stat): Boolean = stat match {
    case _: (Defn.Object | Defn.Class | Defn.Enum) => true
    case _                                         => false
  }

  def defining(stats: List[Stat], name: String): List[Stat] =
    definitions(stats).collect { case (`name`, stat) => stat }

  /** Whether a clause importing from a definition's scope binds it: a wildcard every definition but
    * a given, a `given` selector a given or an implicit one (Scala 3 reference, given imports).
    */
  def imports(stat: Stat, givens: Boolean): Boolean = stat match {
    case _: (Defn.Given | Defn.GivenAlias | Decl.Given) => givens
    case member: Stat.WithMods if givens => MemberSyntax.implicitModifier(member.mods)
    case _                               => !givens
  }

  /** The values the statements pass on that such a clause over a subclass's object binds. */
  def importable(stats: List[Stat], givens: Boolean): Set[String] =
    definedBy(stats, stat => !namespace(stat) && !privateMember(stat) && imports(stat, givens))

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
      case g: (Defn.Given | Defn.GivenAlias | Decl.Given) => MemberSyntax.names(g)
      case c: Defn.Class if c.mods.hasCase                => List(c.name.value)
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
    case member: Stat.WithMods => Overrides.unqualifiedPrivate(member.mods)
    case _                     => false
  }

  def protectedMember(tree: Tree): Boolean = tree match {
    case member: Stat.WithMods => member.mods.exists(_.is[Mod.Protected])
    case _                     => false
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
      SourceIndex.importPath(importer.ref).isEmpty || opensAll(importer)
    )
    Option.unless(open)(importers.flatMap(_.importees).flatMap(SourceIndex.boundName).toSet)
  }

  /** Whether an importer's selectors may bind names it does not spell: a wildcard or a `given`
    * selector.
    */
  def opensAll(importer: Importer): Boolean =
    importer.importees.exists(e =>
      e.is[Importee.Wildcard] || e.is[Importee.Given] || e.is[Importee.GivenAll]
    )

  def opensGivens(importer: Importer): Boolean =
    importer.importees.exists(e => e.is[Importee.Given] || e.is[Importee.GivenAll])

  // The class cases of an enum, each a type `import E.*` binds: a case with neither a parameter
  // list nor type parameters is a value (Scala 3 reference, enums).
  def classCases(d: Defn.Enum): List[String] = d.templ.body.stats.collect {
    case c: Defn.EnumCase if c.ctor.paramClauses.nonEmpty || c.tparamClause.values.nonEmpty =>
      c.name.value
  }

  /** The cases an enum's body declares, by name. */
  def enumCases(d: Defn.Enum): List[String] = d.templ.body.stats.flatMap {
    case c: Defn.EnumCase         => List(c.name.value)
    case r: Defn.RepeatedEnumCase => r.cases.map(_.value)
    case _                        => Nil
  }

  private def names(pats: List[Pat]): List[String] = TypeApplications.boundNames(pats)

}
