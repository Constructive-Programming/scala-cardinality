package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Frame
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

  // The names an inherited declaration introduces, each with its clauses.
  private def declared(declaration: Stat): List[(String, List[Member.ParamClauseGroup])] =
    declaration match {
      case d: Decl.Def   => List(d.name.value -> d.paramClauseGroups)
      case v: Decl.Val   => v.pats.flatMap(TypeApplications.patVarName.getOption).map(_ -> Nil)
      case g: Decl.Given =>
        Option.when(g.name.value.nonEmpty)(g.name.value -> g.paramClauseGroups).toList
      case _ => Nil
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
      case v: Defn.Val        =>
        groups.isEmpty && v.pats.exists(TypeApplications.patVarName.getOption(_).contains(name))
      case _ => false
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
