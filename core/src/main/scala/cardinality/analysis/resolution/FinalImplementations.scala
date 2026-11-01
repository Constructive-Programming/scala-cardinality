package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.SourceIndex
import cardinality.capacity.Counter
import cardinality.types.TypeApplications

/** Inherited declarations a final template implements in its own body.
  *
  * No subclass can override a member of a final concrete class, an object or a given instance, so
  * where such a template implements an inherited declaration itself, the member *is* that concrete
  * body, not a free capability a caller fills. The match is by name and clause shape; a
  * parameterless declaration is also implemented by a `val` or a constructor `val`. A same-shaped
  * overload leaves the declaration implemented by other concrete code: a concrete final template
  * implements every inherited declaration somewhere. An `abstract final` class implements nothing
  * by being final, and an implementation inherited from an intermediate parent is not followed here
  * (members gate G1).
  */
private[cardinality] object FinalImplementations {

  /** The member of a final template's own body that implements an inherited declaration. */
  def implementation(template: Frame, declaration: Stat): List[(String, Tree)] =
    if (!template.tree.exists(isFinal)) Nil
    else
      declared(declaration).flatMap { (name, groups) =>
        template.stats
          .find(implements(_, name, groups))
          .orElse(
            Option.when(groups.isEmpty)(template.params.find(field(_, name, template))).flatten
          )
          .map(name -> _)
      }

  /** The level of a final root's linearization that implements an inherited declaration, if one
    * does: a concrete member of its name and clause shape, or for a parameterless one a field or a
    * `var` (members gate G1). An open root implements nothing for good: a subclass may override.
    */
  def implementer(root: Frame, linearization: List[Frame], declaration: Stat): Option[Frame] =
    if (!root.tree.exists(isFinal)) None
    else linearization.find(implementedIn(_, declaration))

  private def implementedIn(template: Frame, declaration: Stat): Boolean =
    declared(declaration).exists { (name, groups) =>
      template.stats.exists(implements(_, name, groups)) ||
      (groups.isEmpty && template.params.exists(field(_, name, template)))
    }

  /** Why an implementation G1 drops cannot be read, if it cannot: a local template's member may
    * read a capture the target hides, and no prefix names it.
    */
  def localImplementer(level: Frame, declaration: Stat): Option[String] =
    Option.when(level.chain.exists(_.local))(
      s"inherited declaration ${names(declaration).mkString(", ")} " +
        s"is implemented in local ${level.path.last}, whose captures are not read"
    )

  /** The names an inherited declaration introduces. */
  def names(declaration: Stat): List[String] = declared(declaration).map(_._1)

  // The names an inherited declaration introduces, each with its clauses.
  private def declared(declaration: Stat): List[(String, List[Member.ParamClauseGroup])] =
    declaration match {
      case d: Decl.Def   => List(d.name.value -> d.paramClauseGroups)
      case v: Decl.Val   => v.pats.flatMap(TypeApplications.patVarName.getOption).map(_ -> Nil)
      case g: Decl.Given =>
        Option.when(g.name.value.nonEmpty)(g.name.value -> g.paramClauseGroups).toList
      case _ => Nil
    }

  /** Whether every instance of a template is of a supplied declaration no subclass extends: a final
    * class, an object, a given, or a sealed template whose instances are all named subtypes in its
    * file, each such in turn. Its members are then the supplied ones, or what it inherits.
    */
  def fixed(frame: Frame, index: SourceIndex): Boolean =
    frame.tree.exists(
      fixed(_, frame.chain.last.id, frame.path.lastOption.getOrElse(""), Set.empty, index)
    )

  private def fixed(
      tree: Tree,
      file: String,
      name: String,
      seen: Set[String],
      index: SourceIndex
  ): Boolean =
    tree match {
      case t if isFinal(t)                                                    => true
      case d: (Defn.Class | Defn.Trait) if !seen(name) && Counter.isSealed(d) =>
        index.typesInFile(file, name).size == 1 && index.unnamedInstances(file, name).isEmpty &&
        index.subtypes
          .getOrElse((file, name), Nil)
          .forall(child => fixed(child.tree, file, child.name, seen + name, index))
      case _ => false
    }

  /** A final concrete class, an object or a given instance: no subclass overrides its members. */
  def isFinal(tree: Tree): Boolean = tree match {
    case c: Defn.Class => c.mods.exists(_.is[Mod.Final]) && !c.mods.exists(_.is[Mod.Abstract])
    case _: Defn.Object | _: Defn.Given => true
    case _                              => false
  }

  private def implements(stat: Stat, name: String, groups: List[Member.ParamClauseGroup]): Boolean =
    stat match {
      case d: Defn.Def        => d.name.value == name && shape(d.paramClauseGroups) == shape(groups)
      case g: Defn.GivenAlias => g.name.value == name && shape(g.paramClauseGroups) == shape(groups)
      case g: Defn.Given      => g.name.value == name && shape(g.paramClauseGroups) == shape(groups)
      // Mutation is excluded, so a `var` holds its initializer: a parameterless member's value.
      case v: Defn.Val => groups.isEmpty && TypeApplications.binds(name, v.pats)
      case v: Defn.Var => groups.isEmpty && TypeApplications.binds(name, v.pats)
      case _           => false
    }

  private def field(param: Term.Param, name: String, template: Frame): Boolean =
    param.name.value == name &&
      (param.mods.exists(_.is[Mod.ValParam]) || template.tree.exists {
        case c: Defn.Class => c.mods.exists(_.is[Mod.Case])
        case _             => false
      })

  // Type parameter count, then each term clause's size: what tells an override from an overload
  // without reading a type.
  private def shape(groups: List[Member.ParamClauseGroup]): List[Int] =
    groups.flatMap(group => group.tparamClause.values.size :: group.paramClauses.map(_.values.size))

  /** The term names templates declare or take as parameters: inside an implementation they mean the
    * template's own members, which every receiver path reaches.
    */
  def memberNames(templates: List[Frame]): Set[String] =
    templates.flatMap { frame =>
      frame.params.map(_.name.value) ++ frame.stats.flatMap {
        case d: Defn.Def        => List(d.name.value)
        case d: Decl.Def        => List(d.name.value)
        case v: Defn.Val        => v.pats.flatMap(TypeApplications.patVarName.getOption)
        case v: Decl.Val        => v.pats.flatMap(TypeApplications.patVarName.getOption)
        case v: Defn.Var        => v.pats.flatMap(TypeApplications.patVarName.getOption)
        case o: Defn.Object     => List(o.name.value)
        case g: Defn.GivenAlias => List(g.name.value)
        case g: Defn.Given      => List(g.name.value)
        case g: Decl.Given      => List(g.name.value)
        case _                  => Nil
      }
    }.toSet

  /** The names an implementation's body reads that its own parameters do not bind, and whether the
    * body is that one name (`def seed: A = a1`): a member that is an alias of a capture.
    */
  def reads(implementation: Tree): (Set[String], Option[String]) = {
    val (params, body) = implementation match {
      case d: Defn.Def        => (d.paramClauseGroups, Some(d.body))
      case g: Defn.GivenAlias => (g.paramClauseGroups, Some(g.body))
      case v: Defn.Val        => (Nil, Some(v.rhs))
      case g: Defn.Given      => (g.paramClauseGroups, Some(g.templ))
      case _                  => (Nil, None)
    }
    val own = params.flatMap(_.paramClauses.flatMap(_.values.map(_.name.value))).toSet
    val names = body.toList.flatMap(_.collect { case n: Term.Name => n.value }).toSet -- own
    val alias = body.collect { case Term.Name(n) if params.isEmpty && !own(n) => n }
    (names, alias)
  }

}
