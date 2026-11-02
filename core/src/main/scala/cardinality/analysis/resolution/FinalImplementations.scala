package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.SourceIndex
import cardinality.analysis.model.{MemberSyntax, ScopeTerms}
import cardinality.capacity.Counter
import cardinality.types.TypeApplications

/** Inherited declarations a final template implements in its own body.
  *
  * No subclass can override a member of a final concrete class, an object or a given instance, nor
  * a `var` of any template (E164), so where such a member implements an inherited declaration, the
  * member *is* that concrete body, not a free capability a caller fills. The match is by name and
  * clause shape; a parameterless declaration is also implemented by a `val` or a constructor `val`.
  * A same-shaped overload leaves the declaration implemented by other concrete code: a concrete
  * final template implements every inherited declaration somewhere. An `abstract final` class
  * implements nothing by being final, and an implementation inherited from an intermediate parent
  * is not followed here (members gate G1).
  */
private[cardinality] object FinalImplementations {

  /** The member of a template's own body that implements an inherited declaration for every
    * instance: any member of a final template, and a `var` of any template.
    */
  def implementation(template: Frame, declaration: Stat): List[(String, Tree)] = {
    val closed = template.tree.exists(isFinal)
    declared(declaration).flatMap { (name, groups) =>
      implementing(template, name, groups).filter(closed || variable(_)).map(name -> _)
    }
  }

  private def implementing(
      template: Frame,
      name: String,
      groups: List[Member.ParamClauseGroup]
  ): Option[Tree] =
    template.stats
      .find(implements(_, name, groups))
      .orElse(Option.when(groups.isEmpty)(template.params.find(field(_, name, template))).flatten)

  // The model reads a `var` as the value it was initialized with, and no member overrides it.
  private def variable(member: Tree): Boolean = member match {
    case _: Defn.Var   => true
    case p: Term.Param => p.mods.exists(_.is[Mod.VarParam])
    case _             => false
  }

  /** The level of a final root's linearization that implements an inherited declaration, if one
    * does: a concrete member of its name and clause shape, or for a parameterless one a field or a
    * `var` (members gate G1). An open root implements a declaration for good only by a `var`: any
    * other member a subclass may override.
    */
  def implementer(root: Frame, linearization: List[Frame], declaration: Stat): Option[Frame] = {
    val nearest = linearization.find(implementedIn(_, declaration))
    if (root.tree.exists(isFinal)) nearest
    else
      nearest.filter(level =>
        declared(declaration).exists(implementing(level, _, _).exists(variable))
      )
  }

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
      case v: Decl.Var   => v.pats.flatMap(TypeApplications.patVarName.getOption).map(_ -> Nil)
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
      case d: (Defn.Def | Defn.GivenAlias | Defn.Given) =>
        MemberSyntax.names(d).contains(name) &&
        MemberSyntax.shape(MemberSyntax.groups(d)) == MemberSyntax.shape(groups)
      // Mutation is excluded, so a `var` holds its initializer: a parameterless member's value.
      case v: Defn.Val => groups.isEmpty && TypeApplications.boundNames(v.pats).contains(name)
      case v: Defn.Var => groups.isEmpty && TypeApplications.boundNames(v.pats).contains(name)
      case _           => false
    }

  private def readNames(tree: Tree): List[String] = tree match {
    case _: Term.Throw   => Nil
    case Term.Name(name) => List(name)
    case other           => other.children.flatMap(readNames)
  }

  /** Whether a class parameter of the name is a field: a `val` or `var` one, or any of a case
    * class.
    */
  def field(param: Term.Param, name: String, template: Frame): Boolean =
    param.name.value == name &&
      (MemberSyntax.keptParameter(param) || template.tree.exists {
        case c: Defn.Class => c.mods.exists(_.is[Mod.Case])
        case _             => false
      })

  /** The term names templates declare or take as parameters: inside an implementation they mean the
    * template's own members, which every receiver path reaches.
    */
  def memberNames(templates: List[Frame]): Set[String] =
    templates
      .flatMap(frame => frame.params.map(_.name.value) ++ ScopeTerms.defined(frame.stats))
      .toSet

  /** The names an implementation's body reads that its own parameters do not bind, and whether the
    * body is that one name (`def seed: A = a1`): a member that is an alias of a capture. A
    * `throw`'s operand reads nothing a result is built from: that path never returns.
    */
  def reads(implementation: Tree): (Set[String], Option[String]) = {
    val (params, body) = implementation match {
      case d: Defn.Def        => (d.paramClauseGroups, Some(d.body))
      case g: Defn.GivenAlias => (g.paramClauseGroups, Some(g.body))
      case v: Defn.Val        => (Nil, Some(v.rhs))
      case v: Defn.Var        => (Nil, Some(v.body))
      case g: Defn.Given      => (g.paramClauseGroups, Some(g.templ))
      case _                  => (Nil, None)
    }
    val own = MemberSyntax.parameterNames(params)
    val names = body.toList.flatMap(readNames).toSet -- own
    val alias = body.collect { case Term.Name(n) if params.isEmpty && !own(n) => n }
    (names, alias)
  }

}
