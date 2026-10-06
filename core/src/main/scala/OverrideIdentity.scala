package cardinality

import scala.meta.*

import MethodAnalysis.{Frame, Target}

/** Override identity is nominal type identity, not equality of inhabitant representations.
  * Unsupported signatures deliberately leave an obligation instead of guessing an override.
  */
final private[cardinality] class OverrideIdentity(target: Target, resolver: Resolver) {
  private given Dialect = dialects.Scala3

  private type Key = Option[String]
  private type Bindings = Map[String, Key]

  def matches(declaration: Decl.Def, parent: Init, from: Frame, owner: Frame): Option[Boolean] =
    target.tree match {
      case method: (Defn.Def | Decl.Def) if method.name.value == declaration.name.value =>
        directMatch(declaration, method, parent -> from, owner)
      case _ => Some(false)
    }

  private def directMatch(
      declaration: Decl.Def,
      method: Tree,
      inheritance: (Init, Frame),
      owner: Frame
  ): Option[Boolean] =
    if (
      !target.frame.parent.exists(_.id == inheritance._2.id) ||
      declaration.mods.exists(_.is[Mod.Private])
    ) None
    else
      parentIdentity(inheritance).flatMap { _ =>
        compare(
          declaration,
          method,
          parentBindings(inheritance._1, inheritance._2, owner),
          owner
        )
      }

  private def parentIdentity(inheritance: (Init, Frame)): Key = {
    val context =
      ResolutionContext(inheritance._2, resolver.typeParameters(inheritance._2), Set.empty)
    key(inheritance._1.tpe, context, lexical(inheritance._2))
  }

  private def compare(
      declaration: Decl.Def,
      method: Tree,
      bindings: Bindings,
      owner: Frame
  ): Option[Boolean] = {
    val inherited = signature(declaration.paramClauseGroups, declaration.decltpe, owner, bindings)
    val current = method match {
      case d: Defn.Def =>
        d.decltpe.flatMap(signature(d.paramClauseGroups, _, target.frame, lexical(target.frame)))
      case d: Decl.Def =>
        signature(d.paramClauseGroups, d.decltpe, target.frame, lexical(target.frame))
      case _ => None
    }
    for {
      left <- inherited
      right <- current
      matched <- sameSignature(left, right)
    } yield matched
  }

  private def sameSignature(left: (String, String), right: (String, String)): Option[Boolean] =
    if (left._1 != right._1) Some(false)
    // A covariant return can implement the same slot; unequal returns are not proof of an overload.
    else Option.when(left._2 == right._2)(true)

  private def parentBindings(parent: Init, from: Frame, owner: Frame): Bindings = {
    val arguments = parent.tpe match {
      case applied: Type.Apply => applied.argClause.values
      case _                   => Nil
    }
    val caller = ResolutionContext(from, resolver.typeParameters(from), Set.empty)
    lexical(owner) ++ owner.typeParams
      .map(_.name.value)
      .zip(
        arguments.map(key(_, caller, lexical(from)))
      )
  }

  private def lexical(frame: Frame): Bindings =
    frame.chain.reverse.foldLeft(Map.empty[String, Key]) { (bindings, scope) =>
      bindings ++ scope.typeParams.map(p =>
        p.name.value -> Some(s"binder:${scope.id}:${p.name.value}")
      )
    }

  private def signature(
      groups: List[Member.ParamClauseGroup],
      result: Type,
      frame: Frame,
      outer: Bindings
  ): Option[(String, String)] = {
    val parameters = groups.flatMap(_.tparamClause.values)
    val method = parameters.zipWithIndex.map { (p, index) =>
      p.name.value -> Some(s"method:$index")
    }.toMap
    val context = ResolutionContext(frame, resolver.typeParameters(frame), Set.empty)
    val bindings = outer ++ method
    val clauses = groups.map(groupKey(_, context, bindings))
    if (groups.drop(1).exists(_.tparamClause.values.nonEmpty) || dependent(groups, result)) None
    else
      for {
        params <- combine(clauses)
        res <- key(result, context, bindings)
      } yield params -> res
  }

  private def dependent(groups: List[Member.ParamClauseGroup], result: Type): Boolean = {
    val terms = groups.flatMap(_.paramClauses).flatMap(_.values).map(_.name.value).toSet
    val trees: List[Tree] = result :: groups
    trees.exists(_.collect {
      case selected: Type.Select => selected.qual.syntax.takeWhile(_ != '.')
    }.exists(terms))
  }

  private def groupKey(
      group: Member.ParamClauseGroup,
      context: ResolutionContext,
      bindings: Bindings
  ): Key = {
    val types = group.tparamClause.values.map(parameterKey(_, context, bindings))
    val clauses = group.paramClauses.map { clause =>
      val params = clause.values.map { p =>
        for {
          _ <- plainModifiers(p.mods)
          tpe <- p.decltpe.flatMap(key(_, context, bindings))
        } yield tpe
      }
      if (clause.mod.nonEmpty) None
      else combine(params).map(k => s"${clause.copy(values = Nil).structure}:$k")
    }
    combine(List(combine(types), combine(clauses)))
  }

  private def parameterKey(
      parameter: Type.Param,
      context: ResolutionContext,
      bindings: Bindings
  ): Key =
    if (parameter.tparamClause.values.nonEmpty) None
    else {
      val bounds = parameter.bounds
      val keys = List(bounds.lo.toList, bounds.hi.toList, bounds.context, bounds.view)
        .map(types => combine(types.map(key(_, context, bindings))))
      plainModifiers(parameter.mods).flatMap(_ => combine(keys))
    }

  // An annotation does not distinguish an overload's parameter type or calling convention.
  // Other modifiers and contextual clauses need semantic normalization before we can compare.
  private def plainModifiers(mods: List[Mod]): Option[Unit] =
    Option.when(mods.forall(_.is[Mod.Annot]))(())

  private def key(tpe: Type, context: ResolutionContext, bindings: Bindings): Key = tpe match {
    case n: Type.Name  => bindings.getOrElse(n.value, constructor(n.value, context))
    case t: Type.Apply =>
      application(key(t.tpe, context, bindings), t.argClause.values, context, bindings)
    case Type.Tuple(args) =>
      application(Some(s"builtin:Tuple${args.size}"), args, context, bindings)
    case f: Type.Function =>
      combine(f.paramClause.values.map(key(_, context, bindings)) :+ key(f.res, context, bindings))
        .map(k => s"function:$k")
    case other => decoratedKey(other, context, bindings)
  }

  private def decoratedKey(tpe: Type, context: ResolutionContext, bindings: Bindings): Key =
    tpe match {
      case selected: Type.Select => selectedConstructor(selected, context)
      case Type.ByName(value)    => key(value, context, bindings).map(k => s"byname:$k")
      case Type.Repeated(value)  => key(value, context, bindings).map(k => s"repeated:$k")
      case _                     => None
    }

  private def selectedConstructor(tpe: Type.Select, context: ResolutionContext): Key = {
    val name = tpe.syntax.stripPrefix("_root_.")
    val builtin = name.stripPrefix("scala.")
    if (termQualifier(tpe, context.frame)) None
    else if (resolver.lookup(name, context.frame).nonEmpty) constructor(name, context)
    else Option.when(name.startsWith("scala.") && builtins(builtin))(s"builtin:$builtin")
  }

  private def termQualifier(tpe: Type.Select, frame: Frame): Boolean = {
    val root = tpe.qual.syntax.takeWhile(_ != '.')
    frame.chain.exists { scope =>
      scope.params.exists(_.name.value == root) || scope.stats.exists {
        case v: (Defn.Val | Decl.Val) =>
          v.pats.exists(_.collect { case Pat.Var(n) => n.value }.contains(root))
        case _ => false
      }
    }
  }

  private def application(
      constructor: Key,
      args: List[Type],
      context: ResolutionContext,
      bindings: Bindings
  ): Key =
    for {
      name <- constructor
      arguments <- combine(args.map(key(_, context, bindings)))
    } yield s"apply:$name:$arguments"

  private val builtins = Set(
    "Nothing",
    "Any",
    "AnyVal",
    "AnyRef",
    "Unit",
    "Boolean",
    "Byte",
    "Short",
    "Int",
    "Long",
    "Float",
    "Double",
    "Char",
    "String",
    "Option",
    "Either",
    "Tuple1",
    "Tuple2",
    "Tuple3",
    "Tuple4",
    "EmptyTuple"
  )

  private def constructor(name: String, context: ResolutionContext): Key =
    if (imported(name, context.frame)) None
    else
      resolver.lookup(name, context.frame) match {
        // A transparent alias is not a nominal constructor; equality needs scoped expansion.
        case List(entry) if entry.tree.is[Defn.Type] || entry.tree.is[Decl.Type] => None
        case List(entry)           => Some(s"source:${entry.owner.id}:${entry.tree.pos.start}")
        case Nil if builtins(name) => Some(s"builtin:$name")
        case _                     => None
      }

  private def imported(name: String, frame: Frame): Boolean =
    frame.chain.flatMap(_.stats).exists {
      case i: Import =>
        val tokens = i.syntax.split("[^\\p{L}\\p{N}_$*]+").toSet
        name.split('.').exists(tokens) || tokens("*") || tokens("_")
      case _: Export => true
      case _         => false
    }

  private def combine(keys: List[Key]): Key =
    keys
      .foldRight(Option(List.empty[String])) { (key, result) =>
        for {
          head <- key
          tail <- result
        } yield head :: tail
      }
      .map(_.map(k => s"${k.length}:$k").mkString("[", ",", "]"))

}
