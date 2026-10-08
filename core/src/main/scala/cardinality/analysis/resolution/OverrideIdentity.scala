package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.model.*

import MethodAnalysis.{Frame, Target, TypeEntry}

/** Override identity is nominal type identity, not equality of inhabitant representations.
  * Unsupported signatures deliberately leave an obligation instead of guessing an override.
  */
final private[cardinality] class OverrideIdentity(target: Target, resolver: Resolver) {
  private given Dialect = dialects.Scala3

  private type Key = Option[String]

  // Immediate method/alias formals take precedence; enclosing binders compete with declarations
  // in lexical order. Scoped identities retain receiver substitutions across shadowing.
  private case class Bindings(
      locals: Map[String, Key],
      scoped: Map[(String, String), Key],
      expansionDepth: Int = 0
  )

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
    val currentGroups = method match {
      case d: (Defn.Def | Decl.Def) => d.paramClauseGroups
      case _                        => Nil
    }
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
      inheritedConvention <- conventions(declaration.paramClauseGroups)
      currentConvention <- conventions(currentGroups)
      matched <- sameSignature(left, right, inheritedConvention == currentConvention)
    } yield matched
  }

  private def sameSignature(
      left: (String, String),
      right: (String, String),
      sameConvention: Boolean
  ): Option[Boolean] =
    if (left._1 != right._1) Some(false)
    else if (!sameConvention) None
    // A covariant return can implement the same slot; unequal returns are not proof of an overload.
    else Option.when(left._2 == right._2)(true)

  private def parentBindings(parent: Init, from: Frame, owner: Frame): Bindings = {
    val arguments = parent.tpe match {
      case applied: Type.Apply => applied.argClause.values
      case _                   => Nil
    }
    val caller = ResolutionContext(from, resolver.typeParameters(from), Set.empty)
    val replacements = owner.typeParams
      .map(p => (owner.id, p.name.value))
      .zip(arguments.map(key(_, caller, lexical(from))))
      .toMap
    lexical(owner, replacements)
  }

  private def lexical(
      frame: Frame,
      replacements: Map[(String, String), Key] = Map.empty
  ): Bindings = {
    val scoped = frame.chain
      .flatMap(scope =>
        scope.typeParams.map(p => {
          val id = scope.id -> p.name.value
          id -> replacements.getOrElse(id, Some(s"binder:${scope.id}:${p.name.value}"))
        })
      )
      .toMap
    Bindings(Map.empty, scoped)
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
    val bindings = outer.copy(locals = method)
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
          // Scalameta repeats the clause's contextual modifier on each parameter.
          _ <- plainModifiers(
            p.mods.filterNot(mod => clause.mod.exists(_.structure == mod.structure))
          )
          tpe <- p.decltpe.flatMap(key(_, context, bindings))
        } yield tpe
      }
      for {
        _ <- clauseKind(clause.mod)
        types <- combine(params)
      } yield types
    }
    combine(List(combine(types), combine(clauses)))
  }

  // `implicit` and `using` are two spellings of a contextual clause. A different calling
  // convention is not by itself proof of a separate inherited slot, so compare leaves it unknown.
  private def conventions(groups: List[Member.ParamClauseGroup]): Key =
    combine(groups.flatMap(_.paramClauses).map(clause => clauseKind(clause.mod)))

  private def clauseKind(mod: Option[Mod]): Key = mod match {
    case None                                 => Some("ordinary")
    case Some(_: Mod.Using | _: Mod.Implicit) => Some("contextual")
    case _                                    => None
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
  // Other parameter modifiers need semantic normalization before we can compare.
  private def plainModifiers(mods: List[Mod]): Option[Unit] =
    Option.when(mods.forall(_.is[Mod.Annot]))(())

  private def key(tpe: Type, context: ResolutionContext, bindings: Bindings): Key = tpe match {
    case n: Type.Name  => named(n.value, Nil, context, bindings)
    case t: Type.Apply =>
      t.tpe match {
        case n: Type.Name => named(n.value, t.argClause.values, context, bindings)
        case s: Type.Select if !termQualifier(s, context.frame) =>
          named(
            s.syntax.stripPrefix("_root_."),
            t.argClause.values,
            context,
            bindings,
            qualified = true
          )
        case _ => application(key(t.tpe, context, bindings), t.argClause.values, context, bindings)
      }
    case Type.Tuple(args) =>
      application(Some(s"builtin:Tuple${args.size}"), args, context, bindings)
    case f: Type.Function =>
      combine(f.paramClause.values.map(key(_, context, bindings)) :+ key(f.res, context, bindings))
        .map(k => s"function:$k")
    case other => decoratedKey(other, context, bindings)
  }

  private def decoratedKey(tpe: Type, context: ResolutionContext, bindings: Bindings): Key =
    tpe match {
      case selected: Type.Select =>
        if (termQualifier(selected, context.frame)) None
        else named(selected.syntax.stripPrefix("_root_."), Nil, context, bindings, qualified = true)
      case Type.ByName(value)   => key(value, context, bindings).map(k => s"byname:$k")
      case Type.Repeated(value) => key(value, context, bindings).map(k => s"repeated:$k")
      case _                    => None
    }

  private def named(
      name: String,
      args: List[Type],
      context: ResolutionContext,
      bindings: Bindings,
      qualified: Boolean = false
  ): Key = {
    val entries = resolver.lookup(name, context.frame)
    val enclosing = context.frame.chain
      .find(scope => bindings.scoped.contains(scope.id -> name))
      .filterNot { binder =>
        val nearer = context.frame.chain.takeWhile(_.id != binder.id)
        entries.exists(entry => nearer.exists(_.id == entry.owner.id)) || imports(name, nearer)
      }
      .map(scope => bindings.scoped(scope.id -> name))
    val bound = if (qualified) None else bindings.locals.get(name).orElse(enclosing)
    bound match {
      case Some(binding)                         => applied(binding, args, context, bindings)
      case None if imported(name, context.frame) => None
      case None                                  =>
        entries match {
          case List(entry) if entry.tree.is[Defn.Type] =>
            alias(entry, args, context, bindings)
          case _ => applied(constructor(name, context), args, context, bindings)
        }
    }
  }

  private def applied(
      constructor: Key,
      args: List[Type],
      context: ResolutionContext,
      bindings: Bindings
  ): Key =
    if (args.isEmpty) constructor else application(constructor, args, context, bindings)

  private def alias(
      entry: TypeEntry,
      args: List[Type],
      context: ResolutionContext,
      bindings: Bindings
  ): Key = {
    val declaration = entry.tree.asInstanceOf[Defn.Type]
    val parameters = declaration.tparamClause.values
    val id = s"override-alias:${entry.owner.id}:${declaration.pos.start}"
    if (
      declaration.mods.exists(_.is[Mod.Opaque]) ||
      parameters.size != args.size ||
      parameters.exists(p =>
        p.tparamClause.values.nonEmpty || p.mods.nonEmpty ||
          p.bounds.lo.nonEmpty || p.bounds.hi.nonEmpty ||
          p.bounds.context.nonEmpty || p.bounds.view.nonEmpty
      ) ||
      context.visiting(id) || bindings.expansionDepth >= resolver.limits.maxTypeDepth
    ) None
    else {
      val depth = bindings.expansionDepth + 1
      val arguments = args.map(key(_, context, bindings.copy(expansionDepth = depth)))
      val lexicalBindings = lexical(entry.owner, bindings.scoped)
      val replacements = parameters.map(_.name.value).zip(arguments).toMap
      val scoped = context.inScope(entry.owner, resolver.typeParameters(entry.owner)).enter(id)
      key(
        declaration.body,
        scoped,
        lexicalBindings.copy(locals = replacements, expansionDepth = depth)
      )
    }
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
        // Aliases are expanded by `named`; abstract members remain unsupported.
        case List(entry) if entry.tree.is[Defn.Type] || entry.tree.is[Decl.Type] => None
        case List(entry)           => Some(s"source:${entry.owner.id}:${entry.tree.pos.start}")
        case Nil if builtins(name) => Some(s"builtin:$name")
        case Nil if name.startsWith("scala.") && builtins(name.stripPrefix("scala.")) =>
          Some(s"builtin:${name.stripPrefix("scala.")}")
        case _ => None
      }

  private def imported(name: String, frame: Frame): Boolean =
    imports(name, frame.chain)

  private def imports(name: String, scopes: List[Frame]): Boolean =
    scopes.flatMap(_.stats).exists {
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
