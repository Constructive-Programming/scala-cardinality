package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.model.MemberSyntax

/** The source declarations through which the compiler may run code around an expression without
  * evaluating it first (capabilities C6): an implicit conversion or extension whose receiver is by
  * name, a method overloading an operator with a by-name parameter, and the type aliases that name
  * a context function (an argument whose expected type is one is wrapped in a lambda).
  *
  * A Scala 3 conversion is a `Conversion` instance, whose `apply` takes its argument by value; only
  * a Scala 2 `implicit def` or `implicit class` can take it by name. Declarations outside the
  * sources reach a body through a channel `Resolver.unreadChannel` reports (an unsupplied wildcard
  * import, parent or self type) or a named import, which the view reading checks where it is used.
  */
final private[cardinality] case class Insertions(
    conversion: Boolean,
    extensions: Set[String],
    byName: Set[String],
    contextAliases: Set[String]
) {

  /** Whether a parameter type defers its argument: by name, a context function or an alias of one
    * (by its name, wherever the alias is declared).
    */
  def deferring(tpe: Type): Boolean = tpe match {
    case _: (Type.ByName | Type.ContextFunction) => true
    case Type.Repeated(element)                  => deferring(element)
    case Type.Apply.After_4_6_0(callee, _)       => deferring(callee)
    case Type.Name(name)                         => contextAliases(name)
    case selected: Type.Select                   => contextAliases(selected.name.value)
    case _                                       => false
  }

}

private[cardinality] object Insertions {

  /** What the supplied sources declare. */
  def of(sources: List[Tree]): Insertions = {
    val trees = sources.flatMap(_.collect { case stat: Stat => stat })
    val aliases = contextAliases(trees.collect { case d: Defn.Type => d })
    val syntax = Insertions(conversion = false, Set.empty, Set.empty, aliases)
    trees.foldLeft(syntax) { (found, stat) =>
      stat match {
        case d: Defn.Def if implicitConversion(d.mods, firstClause(d.paramClauseGroups), found) =>
          found.copy(conversion = true, byName = found.byName + d.name.value)
        case c: Defn.Class
            if implicitConversion(c.mods, c.ctor.paramClauses.headOption.toList, found) =>
          found.copy(conversion = true)
        case e: Defn.ExtensionGroup if receiverDeferred(e, found) =>
          found.copy(extensions = found.extensions ++ extensionNames(e))
        case d: (Defn.Def | Decl.Def) if deferringParameter(d, found) =>
          found.copy(byName = found.byName + MemberSyntax.names(d).mkString)
        case _ => found
      }
    }
  }

  private def firstClause(groups: List[Member.ParamClauseGroup]): List[Term.ParamClause] =
    groups.flatMap(_.paramClauses).filterNot(MemberSyntax.contextual).take(1)

  // An `implicit def` or `implicit class` whose converted parameter is deferred.
  private def implicitConversion(
      mods: List[Mod],
      clause: List[Term.ParamClause],
      syntax: Insertions
  ): Boolean =
    mods.exists(_.is[Mod.Implicit]) &&
      clause.flatMap(_.values).exists(_.decltpe.exists(syntax.deferring))

  private def receiverDeferred(group: Defn.ExtensionGroup, syntax: Insertions): Boolean =
    group.paramClauseGroup.toList
      .flatMap(_.paramClauses)
      .flatMap(_.values)
      .exists(_.decltpe.exists(syntax.deferring))

  private def extensionNames(group: Defn.ExtensionGroup): List[String] =
    group.body.collect { case d: Defn.Def => d.name.value }

  private def deferringParameter(member: Stat, syntax: Insertions): Boolean =
    MemberSyntax.parameters(MemberSyntax.groups(member)).exists(_.decltpe.exists(syntax.deferring))

  // The aliases whose body is a context function, directly or through another such alias (by
  // name; a cycle is no alias Scala accepts).
  private def contextAliases(aliases: List[Defn.Type]): Set[String] = {
    val bodies = aliases.map(alias => alias.name.value -> alias.body).toMap
    def wraps(tpe: Type, seen: Set[String]): Boolean = tpe match {
      case _: Type.ContextFunction           => true
      case Type.Apply.After_4_6_0(callee, _) => wraps(callee, seen)
      case Type.Name(name)                   => expands(name, seen)
      case selected: Type.Select             => expands(selected.name.value, seen)
      case _                                 => false
    }
    def expands(name: String, seen: Set[String]): Boolean =
      !seen(name) && bodies.get(name).exists(wraps(_, seen + name))
    bodies.keySet.filter(expands(_, Set.empty))
  }

}
