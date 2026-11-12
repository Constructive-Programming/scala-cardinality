package cardinality.compiled

import scala.collection.mutable

import cardinality.capacity.Definition
import cardinality.capacity.Definition.Kind
import tastyquery.Contexts.*
import tastyquery.Exceptions.MemberNotFoundException
import tastyquery.Names.{nme, SimpleName}
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*

/** The rows of one compiled source: every definition the report lists, found in the typed trees of
  * the classes compiled from it and named as the report names it (docs/plans/compiled-own-types.md,
  * "Front end: design and status", row identity).
  *
  * A row's name is its owners' path. A package, a class, an object, a method and a value give their
  * own name; a constructor is `<init>`. What has no name gets the one the source reading gave it:
  * an anonymous given `<given@line>`, an extension's group `<extension@line>`, and a scope of a
  * body that holds a definition `<kind#n>`, n its ordinal among its owner's scopes of that kind
  * (`<block#1>`, `<new#1>`, `<lambda#1>`, `<case#1>`, a secondary constructor's body `<init#1>`).
  * Compiler-made members (default getters, setters, case-class and enum members, export forwarders,
  * the method beside a given or implicit class, companions nobody wrote, a `@main` method's class)
  * are no rows.
  */
final private class Targets(source: Source, trees: List[ClassDef])(using Context) {
  import Targets.*

  private val methods = List.newBuilder[Method]
  private val definitions = List.newBuilder[Defined]
  // The definitions whose name the source does not spell where the classes say it is.
  private val unspelled = List.newBuilder[String]
  // How many scopes of each kind a path has opened: the ordinal names the next.
  private val ordinals = mutable.Map.empty[(List[String], String), Int]

  // Every definition and every tree with one below it: the scopes a walk must open.
  private val holders = java.util.Collections.newSetFromMap(
    new java.util.IdentityHashMap[Tree, java.lang.Boolean]()
  )

  // The classes of the source's given instances: each sits beside the given value or method that
  // builds it, under its name, and the flag is on that value or method only.
  private val givens = mutable.Set.empty[Symbol]
  // The names of the source's `@main` methods: the compiler makes a class of each.
  private val mains = mutable.Set.empty[String]

  private def mark(root: Tree): Unit = new TreeTraverser {
    private var above = List.empty[Tree]
    override def traverse(tree: Tree): Unit = {
      tree match {
        case Template(_, _, _, body)           => instances(body)
        case Block(stats, _)                   => instances(stats)
        case d: DefDef if entryPoint(d.symbol) => mains += d.name.toString
        case _                                 => ()
      }
      if (definition(tree)) (tree :: above).foreach(holders.add)
      above ::= tree
      super.traverse(tree)
      above = above.tail
    }
  }.traverse(root)

  private def instances(stats: List[Tree]): Unit = {
    // A given instance's class is made with the given, at the given's own place: a class written
    // apart that a given merely shares its name with (`class s; given s: s = new s`) is an
    // ordinary class.
    val built = stats.collect {
      case d: ValOrDefDef if d.symbol.isGivenOrUsing => d.name.toString -> d.pos
    }
    stats.foreach {
      case c: ClassDef if built.exists { (name, at) =>
            name == nameOf(c.symbol) &&
            (at.isUnknown || c.pos.isUnknown || at.startOffset == c.pos.startOffset)
          } =>
        givens += c.symbol
      case _ => ()
    }
  }

  // An annotation whose class the classpath does not hold (a compile-only dependency's) is not
  // `scala.main`.
  private def entryPoint(symbol: TermSymbol): Boolean =
    symbol.annotations.exists { annotation =>
      try annotation.symbol.displayFullName == "scala.main"
      catch { case _: MemberNotFoundException => false }
    }

  private def definition(tree: Tree): Boolean = tree match {
    case _: ClassDef => true
    case d: DefDef   => written(d)
    case _           => false
  }

  // A definition somebody wrote: the compiler's own carry the flag (a function literal's method
  // among them), a name no source spells (a default getter's), a setter's role or no position of
  // their own.
  private def written(tree: ValOrDefDef): Boolean = {
    val symbol = tree.symbol
    tree.name.isInstanceOf[SimpleName] && !symbol.isSynthetic && !symbol.isExport &&
    !symbol.isSetter && !tree.pos.isUnknown && !tree.pos.isSynthetic
  }

  // ---- method rows --------------------------------------------------------------------------

  private def classDef(c: ClassDef, path: List[String]): Unit = {
    val symbol = c.symbol
    val name = nameOf(symbol)
    if (holder(symbol)) template(c, path)
    // A companion nobody wrote still holds an enum's cases; its own members are no rows.
    else if (symbol.isModuleClass && symbol.isSynthetic) template(c, path :+ name)
    else if (symbol.isModuleClass) template(c, path :+ segment(c, isGiven(c), name))
    else if (!entryClass(c)) {
      val inside = path :+ segment(c, isGiven(c), name)
      val clauses = c.rhs.constr.paramLists
      // A class has a constructor row; a trait where it is written with a parameter clause; a
      // given instance where it has clauses of any kind; an enum none (its cases have theirs).
      val constructed =
        if (isGiven(c)) clauses.exists(_.merge.nonEmpty)
        else if (symbol.isTrait) source.parenthesized(c.pos.pointOffset, name)
        else !enumClass(symbol)
      if (constructed) methods += Method(inside :+ Init, c, c.rhs.constr, "constructor")
      template(c, inside)
    }
  }

  // A definition's own name where the source spells it; an anonymous given's is its line. Any
  // other name the source does not spell there is recorded: the classes are of another text.
  private def segment(tree: Tree, anonymous: Boolean, name: String): String =
    if (source.spells(name, tree.pos.pointOffset) && source.placed(tree.pos)) name
    else if (anonymous) s"<given@${tree.pos.startLine + 1}>"
    else {
      unspelled += name
      name
    }

  private def isGiven(c: ClassDef): Boolean = givens(c.symbol)

  // The class the compiler makes of a `@main` method: it has the method's name, sits at the
  // annotation and holds nothing written.
  private def entryClass(c: ClassDef): Boolean = {
    val name = nameOf(c.symbol)
    mains(name) && !source.spells(name, c.pos.pointOffset)
  }

  private def template(c: ClassDef, path: List[String]): Unit =
    members(c.rhs.body, path, local = false)

  private def members(stats: List[StatementTree], path: List[String], local: Boolean): Unit = {
    // A given or implicit class comes with the method that builds it, at the class's own name.
    val classes = stats.collect { case c: ClassDef => c.pos.pointOffset }.toSet
    stats.foreach {
      case d: DefDef if classes(d.pos.pointOffset) => ()
      case stat                                    => statement(stat, path, local)
    }
  }

  private def statement(stat: StatementTree, path: List[String], local: Boolean): Unit =
    stat match {
      case c: ClassDef => classDef(c, path)
      // A given alias without clauses is a value (an `inline` one a method without clauses): in a
      // block, a step like a local value.
      case d: DefDef if local && d.symbol.isGivenOrUsing && d.paramLists.isEmpty =>
        d.rhs.foreach(walk(_, path))
      case d: DefDef if written(d) => method(d, path)
      case v: ValDef if member(v)  =>
        val inside = path :+ segment(v, v.symbol.isGivenOrUsing, v.name.toString)
        methods += Method(inside, v, v, if (v.rhs.isEmpty) "declaration" else "method")
        v.rhs.foreach(body(_, inside))
      // Any other value with an initializer is a step of what is around it: a block's value, or
      // one the compiler made (a pattern definition's holder).
      case v: ValDef if local || !written(v) => v.rhs.foreach(walk(_, path))
      case term: TermTree                    => walk(term, path)
      case _                                 => ()
    }

  // A value of a template somebody wrote: not an object's own value, an enum's case or a
  // constructor's parameter.
  private def member(v: ValDef): Boolean = {
    val symbol = v.symbol
    written(v) && !symbol.isModuleVal && !symbol.isEnumCase && !symbol.isParamAccessor &&
    symbol.owner.isClass
  }

  private def method(d: DefDef, path: List[String]): Unit =
    if (d.name == nme.Constructor) {
      // A secondary constructor: a row named like the class's own, its body's definitions under
      // `<init#k>`.
      methods += Method(path :+ Init, d, d, "method")
      d.rhs.foreach(body(_, path :+ opened(path, "init")))
    } else {
      val group =
        if (d.symbol.isExtensionMethod) List(s"<extension@${source.extensionLine(d)}>") else Nil
      val inside = path ++ group :+ segment(d, d.symbol.isGivenOrUsing, d.name.toString)
      methods += Method(inside, d, d, if (d.rhs.isEmpty) "declaration" else "method")
      // The compiler ascribes an inline method's body with its declared type.
      d.rhs.foreach {
        case Typed(ascribed, _) if d.symbol.isInline => body(ascribed, inside)
        case rhs                                     => body(rhs, inside)
      }
    }

  // A definition's right-hand side: its own block is its body, any other expression is walked.
  // So is the block of a method whose result is a context function, which the compiler puts in a
  // function literal of its own.
  private def body(term: TermTree, path: List[String]): Unit = term match {
    case Scope.Plain(block)                           => statements(block, path)
    case Scope.Lambda(made) if Scope.contextual(made) => made.rhs.foreach(body(_, path))
    case other                                        => walk(other, path)
  }

  private def statements(block: Block, path: List[String]): Unit = {
    members(block.stats, path, local = true)
    walk(block.expr, path)
  }

  // Every scope under `term` that holds a definition, opened under `path`.
  private def walk(term: Tree, path: List[String]): Unit =
    new TreeTraverser {
      override def traverse(tree: Tree): Unit =
        if (holders.contains(tree)) tree match {
          case Scope.Anonymous(c) =>
            // The parents' arguments are evaluated outside the instance.
            traverse(c.rhs.parents)
            template(c, path :+ opened(path, "new"))
          // A pattern-matching function literal is its cases.
          case Scope.Lambda(Scope.Cases(cases)) => traverse(cases)
          // A function literal the compiler made around a context function's body is no scope
          // the source has: what it holds is walked in place. A block in it is still the block
          // somebody wrote (an argument `run { … }`), where a definition's own (`body`) is not.
          case Scope.Lambda(made) if Scope.contextual(made) => traverse(made.rhs)
          case Scope.Lambda(function)                       =>
            function.rhs.foreach(body(_, path :+ opened(path, "lambda")))
          case Scope.Plain(block) => statements(block, path :+ opened(path, "block"))
          case c: CaseDef         =>
            val inside = path :+ opened(path, "case")
            body(c.body, inside)
            c.guard.foreach(walk(_, inside))
          // A `transparent inline` call's expansion is the callee's code, written elsewhere. The call's own
          // arguments sit in it too: a definition inside one is not found (the plan's design
          // section, "What it does not undo").
          case _: Inlined => ()
          case other      => super.traverse(other)
        }
    }.traverse(term)

  private def opened(path: List[String], kind: String): String = {
    val next = ordinals.getOrElse((path, kind), 0) + 1
    ordinals.update((path, kind), next)
    s"<$kind#$next>"
  }

  // ---- definition rows ----------------------------------------------------------------------

  // The definitions a template or package holds, named under `prefix`; a body's are none.
  private def defined(c: ClassDef, prefix: List[String], top: Boolean): Unit = {
    val symbol = c.symbol
    val name = nameOf(symbol)
    def inside(under: List[String], top: Boolean) = {
      c.rhs.body.foreach {
        case nested: ClassDef                       => defined(nested, under, top = false)
        case t: TypeMember if !t.symbol.isSynthetic => alias(t, under)
        case _                                      => ()
      }
      // A top-level value is a definition row, one per name it binds; a given is a method row only.
      if (top) c.rhs.body.foreach {
        case v: ValDef if member(v) && !v.symbol.isGivenOrUsing =>
          val bound = Definition.qualified(under :+ v.name.toString)
          definitions += Defined(bound, Kind.Value, Nil, v)
        case _ => ()
      }
    }
    if (holder(symbol)) inside(prefix, top)
    else if (symbol.isModuleClass) {
      if (!symbol.isSynthetic && !isGiven(c)) {
        definitions += Defined(Definition.qualified(prefix :+ name), Kind.Object, Nil, c)
        inside(prefix :+ name, top = false)
      }
    } else if (!isGiven(c) && !(symbol.isEnum && symbol.isCaseClass) && !entryClass(c)) {
      val kind =
        if (enumClass(symbol)) Kind.Enum
        else if (symbol.isAbstractClass) Kind.Abstract
        else Kind.Class
      val params = symbol.typeParams.map(_.name.toString)
      definitions += Defined(Definition.qualified(prefix :+ name), kind, params, c)
      inside(prefix :+ name :+ Definition.Instance, top = false)
    }
  }

  // A type alias: `type F[X] = …` has its parameters, `type F = [X] =>> …` is a constructor, and
  // so is one whose body is a type lambda again (`type F[X] = [Y] =>> …`). The compiled tree is the
  // same for the first two, so the source tells them apart. An abstract type member is declared,
  // not defined.
  private def alias(t: TypeMember, prefix: List[String]): Unit = {
    val (binders, body) = t.rhs match {
      case PolyTypeDefinitionTree(tparams, body) => (tparams, body)
      case other                                 => (Nil, other)
    }
    val applied = source.spells("[", t.pos.pointOffset + t.name.toString.length)
    val lambda = body match {
      case _: PolyTypeDefinitionTree | TypeAliasDefinitionTree(_: TypeLambdaTree) => true
      case _                                                                      => false
    }
    val kind = t.symbol.typeDef match {
      case _: TypeMemberDefinition.OpaqueTypeAlias => Some(Kind.Opaque)
      case _: TypeMemberDefinition.TypeAlias       =>
        Some(if (lambda || (binders.nonEmpty && !applied)) Kind.Constructor else Kind.Alias)
      case _: TypeMemberDefinition.AbstractType => None
    }
    val params = if (applied) binders.map(_.name.toString) else Nil
    kind.foreach(
      definitions += Defined(Definition.qualified(prefix :+ t.name.toString), _, params, t)
    )
  }

  // The walk, after every field above is set: the scopes and givens first, then the rows.
  trees.foreach(mark)
  trees.sortBy(_.pos.startOffset).foreach { tree =>
    val prefix = packageOf(tree.symbol)
    classDef(tree, prefix)
    defined(tree, prefix, top = true)
  }

}

private[compiled] object Targets {

  val Init = "<init>"

  /** A method row: its owners' path, the tree its line and signature are read from, and the
    * definition measured (a class's row measures its primary constructor).
    */
  final case class Method(
      path: List[String],
      shown: ValOrDefDef | ClassDef,
      measured: ValOrDefDef,
      kind: String
  ) {
    def name: String = path.mkString(".")
  }

  /** A definition row: its name, kind and type parameters, and its tree. */
  final case class Defined(name: String, kind: Kind, params: List[String], tree: Tree)

  /** The rows of the classes `trees`, the top-level classes compiled from `source`: its method
    * rows, its definition rows, and the names that show the classes are of another text.
    */
  def of(source: Source, trees: List[ClassDef])(using
      Context
  ): (List[Method], List[Defined], List[String]) = {
    val targets = new Targets(source, trees)
    (targets.methods.result(), targets.definitions.result(), targets.unspelled.result().distinct)
  }

  // The names of the packages around a top-level class.
  private def packageOf(symbol: Symbol): List[String] = symbol.owner match {
    case p: PackageSymbol => p.fullName.path.map(_.toString)
    case _                => Nil
  }

  // A class's name as its source spells it: an object's class is named after the object.
  private def nameOf(symbol: ClassSymbol): String =
    if (symbol.isModuleClass) symbol.name.sourceObjectName.toString else symbol.name.toString

  // The object that holds a package's top-level definitions, or a package object: its members are
  // the package's.
  private def holder(symbol: ClassSymbol): Boolean =
    symbol.isModuleClass && (nameOf(symbol).endsWith("$package") || nameOf(symbol) == "package")

  // An enum is a sealed class; its cases are the classes and values of its companion.
  private def enumClass(symbol: ClassSymbol): Boolean = symbol.isEnum && !symbol.isCaseClass

  /** The scopes a body opens, as the typed tree holds them. */
  private object Scope {

    /** `new T { … }`: the anonymous class. */
    object Anonymous {

      def unapply(tree: Tree): Option[ClassDef] = tree match {
        case Block(List(c: ClassDef), _) if c.name.toString == "$anon" => Some(c)
        case _                                                         => None
      }

    }

    /** A function literal: the method the compiler made of it, which the block's result refers to.
      */
    object Lambda {

      def unapply(tree: Tree)(using Context): Option[DefDef] = tree match {
        case Block(List(d: DefDef), made: tastyquery.Trees.Lambda)
            if made.meth.symbol == d.symbol =>
          Some(d)
        case _ => None
      }

    }

    /** A function literal written as cases, `{ case … => … }`: the compiler matches on the one
      * parameter it made.
      */
    object Cases {

      def unapply(function: DefDef)(using Context): Option[List[CaseDef]] =
        (function.paramLists, function.rhs) match {
          case (List(Left(List(made))), Some(Match(selector: Ident, cases)))
              if made.symbol.isSynthetic && selector.symbol == made.symbol =>
            Some(cases)
          case _ => None
        }

    }

    /** Whether the compiler made a function literal for a context function nobody wrote as one:
      * every parameter is a given under a name no source spells.
      */
    def contextual(function: DefDef): Boolean = {
      val parameters = function.paramLists.flatMap(_.left.toOption.toList.flatten)
      parameters.nonEmpty && parameters.forall { p =>
        p.symbol.isGivenOrUsing && (p.symbol.isSynthetic || !p.name.isInstanceOf[SimpleName])
      }
    }

    /** Any other block: one somebody wrote, as far as the tree tells. */
    object Plain {

      def unapply(tree: Tree)(using Context): Option[Block] = tree match {
        case Anonymous(_) | Lambda(_) => None
        case block: Block             => Some(block)
        case _                        => None
      }

    }

  }

}
