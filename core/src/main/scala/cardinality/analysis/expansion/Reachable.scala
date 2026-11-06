package cardinality.analysis.expansion

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.model.{MemberSyntax, ScopeTerms}
import cardinality.analysis.{Resolver, SourceIndex}
import cardinality.types.Trees.hasOpaque

/** The implicit scope of the types an inline body can reach (register D43, v1.md §2): a conversion
  * or extension outside the body's lexical scope is a candidate only when its object is in that
  * implicit scope.
  *
  * The types a body can reach are those written in it, those of the signatures around it, and the
  * signatures of every declaration it references (each name its walk resolved, a class it
  * constructs, a member a name it selects may denote) or given it may summon (an implicit of a
  * scope found lexically or in the implicit scope so far), repeated until nothing new is added. In
  * well-typed code every inferred type is built from those signatures: a least upper bound is a
  * supertype of the branch types, whose companions are reached through their base classes.
  *
  * A type is read by the names it spells, resolved by scope without typing (`occurrence`), each
  * standing for every supplied declaration the scoping admits: a superset of what Scala resolves,
  * so the scope found is one too. A name no supplied declaration may be is a library type, which
  * anchors no supplied scope. A template brings its parents, its companion and the objects it is a
  * member of (and what those extend); an alias its body; an opaque or abstract type its body or
  * bounds and the object of its name. A declaration outside the body with no declared type, or a
  * name the walk did not resolve, leaves the scope unknown, and every candidate is kept.
  */
final private[expansion] class Reachable(resolver: Resolver, conversions: Conversions) {

  private val known = mutable.Map.empty[String, Option[Set[String]]]

  /** The scopes in the implicit scope of what a definition's body can reach, given the meanings its
    * walk resolved; None when that is not known.
    */
  def scopes(definition: Definition, heard: => List[Meaning]): Option[Set[String]] =
    known.getOrElseUpdate(definition.frame.id, new Closure(definition).of(heard))

  private lazy val all: List[Frame] = resolver.memberScopes

  private lazy val templates: Map[String, List[Frame]] =
    all.filter(_.isTemplate).groupBy(_.name.getOrElse(""))

  private lazy val typeMembers: Map[String, List[(Stat, Frame)]] =
    all
      .flatMap(scope =>
        scope.stats.collect {
          case d: Defn.Type => d.name.value -> (d, scope)
          case d: Decl.Type => d.name.value -> (d, scope)
        }
      )
      .groupMap(_._1)(_._2)

  // The values a member of each name is declared by, for a singleton type that spells one.
  private lazy val valueMembers: Map[String, List[Stat]] =
    all
      .flatMap(
        _.stats
          .collect {
            case stat @ (_: Defn.Val | _: Decl.Val | _: Defn.Var | _: Decl.Var | _: Defn.Def |
                _: Decl.Def) =>
              MemberSyntax.defined(stat).map(_ -> stat)
          }
          .flatten
      )
      .groupMap(_._1)(_._2)

  // The names a renaming import or export gives a declaration, to the names it renames.
  private lazy val renamings: Map[String, Set[String]] =
    all
      .flatMap(_.stats.flatMap(_.collect { case r: Importee.Rename => r }))
      .groupMap(_.rename.value)(_.name.value)
      .view
      .mapValues(_.toSet)
      .toMap

  // Each scope's frame, by where its tree is written, and by its path.
  private lazy val positioned: Map[(Input, Int), Frame] =
    all.flatMap(f => f.tree.map(t => (t.pos.input -> t.pos.start) -> f)).toMap

  private lazy val byPath: Map[List[String], List[Frame]] = all.groupBy(_.path)

  // A member's signature, without its body.
  private def signature(tree: Tree): List[Tree] = tree match {
    case d: Defn.Def            => d.paramClauseGroups ++ d.decltpe
    case d: Decl.Def            => d.paramClauseGroups :+ d.decltpe
    case g: Defn.GivenAlias     => g.paramClauseGroups :+ g.decltpe
    case g: Decl.Given          => g.paramClauseGroups :+ g.decltpe
    case g: Defn.Given          => g.paramClauseGroups ++ g.templ.inits
    case e: Defn.ExtensionGroup => e.paramClauseGroup.toList
    case v: Defn.Val            => v.decltpe.toList
    case v: Defn.Var            => v.decltpe.toList
    case v: Decl.Val            => List(v.decltpe)
    case v: Decl.Var            => List(v.decltpe)
    case c: Defn.Class          => c.ctor :: c.tparamClause :: c.templ.inits
    case t: Defn.Trait          => t.ctor :: t.tparamClause :: t.templ.inits
    case e: Defn.Enum           => e.ctor :: e.tparamClause :: e.templ.inits
    case o: Defn.Object         => o.templ.inits
    case _                      => Nil
  }

  // A definition whose type Scala infers from its right-hand side.
  private def untyped(tree: Tree): Boolean = tree match {
    case d: Defn.Def => d.decltpe.isEmpty
    case v: Defn.Val => v.decltpe.isEmpty
    case v: Defn.Var => v.decltpe.isEmpty
    case _           => false
  }

  private def implicitly(stat: Stat): Boolean = stat match {
    case _: (Defn.Given | Defn.GivenAlias | Decl.Given) => true
    case member: Stat.WithMods => MemberSyntax.implicitModifier(member.mods)
    case _                     => false
  }

  /** One body's closure, grown from what it spells and references. */
  final private class Closure(definition: Definition) {

    private val body = definition.tree
    private val frame = definition.frame

    private val names = mutable.Set.empty[String]
    private val values = mutable.Set.empty[String]
    private val frames = mutable.Map.empty[String, Frame]
    private val read = mutable.Set.empty[String]
    private val pendingNames = mutable.Queue.empty[String]
    private val pendingValues = mutable.Queue.empty[String]
    private val pendingFrames = mutable.Queue.empty[Frame]
    // Whether a declaration this reading cannot type was met: every candidate is kept.
    private var unknown = false

    // The names the body selects: a member a conversion's result has may answer one.
    private val selected: Set[String] =
      body.collect {
        case s: Term.Select        => s.name.value
        case o: Term.ApplyInfix    => o.op.value
        case u: Term.ApplyUnary    => u.op.value
        case p: Term.SelectPostfix => p.name.value
      }.toSet

    def of(heard: List[Meaning]): Option[Set[String]] = {
      spell(body)
      frame.chain.drop(1).foreach { around =>
        if (around.isTemplate) reach(around)
        around.tree.foreach(t => signature(t).foreach(spell))
      }
      constructed()
      heard.foreach(meant)
      grow()
      Option.unless(unknown)(frames.keySet.toSet)
    }

    private def inBody(tree: Tree): Boolean =
      tree.pos.input == body.pos.input && tree.pos.start >= body.pos.start &&
        tree.pos.end <= body.pos.end

    // The type names a tree spells, and the values whose singleton types it spells.
    private def spell(tree: Tree): Unit = {
      tree.collect { case n: Type.Name if !introduced(n) => n }.foreach(occurrence)
      tree
        .collect {
          case Type.Singleton(ref: Term.Name)   => ref.value
          case Type.Singleton(ref: Term.Select) => ref.name.value
        }
        .foreach(v => if (values.add(v)) pendingValues += v)
    }

    // A selected path is what ends with it; a simple name a type parameter around it binds brings
    // that parameter's bounds, one a type member of a template or refinement around it declares
    // that member, another what its scopes bring (`lexical`).
    private def occurrence(n: Type.Name): Unit =
      selectedPath(n) match {
        case Some(full) => path(full)
        case None       =>
          ancestors(n).iterator.flatMap(scoped(_, n.value)).nextOption() match {
            case Some(Left(param)) =>
              param.foreach(p => if (bounded.add(p.pos.input -> p.pos.start)) spell(p.bounds))
            case Some(Right(member)) => typeMember(member, None)
            case None                => lexical(n)
          }
      }

    // A simple name no type parameter or type member around it declares: the templates the scopes
    // around it declare or inherit, what an import or export around it brings, the members of the
    // packages it is written in; otherwise a root import's, a library type. A tree no source holds,
    // or an import this reading cannot follow, may bring any declaration of the name.
    private def lexical(n: Type.Name): Unit = {
      val around = ancestors(n)
      val v = n.value
      val packages = around.reverse
        .collect { case p: Pkg => SourceIndex.importPath(p.ref).getOrElse(Nil) }
        .scanLeft(List.empty[String])(_ ++ _)
        .drop(1)
      val clauses =
        around
          .flatMap(statsOf)
          .collect {
            case i: Import => i.importers
            case e: Export => e.importers
          }
          .flatten ++ packages.flatMap(byPath.getOrElse(_, Nil)).flatMap(_.exported)
      val enclosing = around.collect { case t: Template => t }
      if (!around.lastOption.exists(_.is[Source])) name(v)
      else {
        enclosing.flatMap(_.body.stats).foreach(declares(_, v))
        enclosing.flatMap(inherited).foreach { (stat, owner) =>
          declares(stat, v)
          stat match {
            case d: (Defn.Type | Decl.Type) if d.name.value == v => typeMember(d, Some(owner))
            case _                                               => ()
          }
        }
        packages.flatMap(p => byPath.getOrElse(p :+ v, Nil)).foreach(reach)
        typeMembers
          .getOrElse(v, Nil)
          .filter((_, owner) => packages.contains(owner.path))
          .foreach((stat, owner) => typeMember(stat, Some(owner)))
        clauses.foreach(imported(_, v))
      }
    }

    private def statsOf(tree: Tree): List[Stat] = tree match {
      case t: Template   => t.body.stats
      case p: Pkg        => p.body.stats
      case b: Term.Block => b.stats
      case s: Source     => s.stats
      case _             => Nil
    }

    // A template a statement declares under the name.
    private def declares(stat: Stat, v: String): Unit =
      if (MemberSyntax.templateName(stat).contains(v))
        positioned.get(stat.pos.input -> stat.pos.start).foreach(reach)

    // The statements of the templates a template's parents and self type name, by their simple
    // names, each with its template.
    private def inherited(t: Template): List[(Stat, Frame)] = {
      val seen = mutable.Set.empty[String]
      def spelled(trees: List[Tree]): List[String] =
        trees.flatMap(_.collect { case n: Type.Name => n.value })
      def up(names: List[String]): List[Frame] =
        names
          .filter(seen.add)
          .flatMap(templates.getOrElse(_, Nil))
          .flatMap(f => f :: up(spelled(f.parents.map(_.tpe))))
      up(spelled(t.inits.map(_.tpe) ++ t.body.selfOpt.flatMap(_.decltpe))).flatMap(f =>
        f.stats.map(_ -> f)
      )
    }

    // What an import or export clause brings under a name: a selector of it (or renaming to it),
    // or a wildcard over a scope that declares it; any declaration of it, when the clause names
    // no supplied scope or a path this reading cannot spell.
    private def imported(importer: Importer, v: String): Unit =
      SourceIndex.importPath(importer.ref).map(_.filterNot(_ == SourceIndex.Root)) match {
        case None      => name(v)
        case Some(ref) =>
          importer.importees.foreach {
            case Importee.Name(named) if named.value == v    => path(ref :+ v)
            case Importee.Rename(named, to) if to.value == v => path(ref :+ named.value)
            case _: (Importee.Wildcard | Importee.Given | Importee.GivenAll) =>
              if (all.exists(_.path.endsWith(ref))) matching(ref :+ v): Unit
              else if (!standard(ref)) name(v)
            case _ => ()
          }
      }

    private def standard(full: List[String]): Boolean =
      full.headOption.exists(List("scala", "java", "javax").contains)

    // The path a type name is selected by when its prefix is a path of names (`_root_` dropped).
    private def selectedPath(n: Type.Name): Option[List[String]] = n.parent.flatMap {
      case Type.Select(qual, named) if named eq n =>
        SourceIndex.importPath(qual).map(_.filterNot(_ == SourceIndex.Root) :+ n.value)
      case _ => None
    }

    // A selected type is the templates and type members whose path ends with the one it spells; a
    // path no supplied declaration ends with selects through a value (any member of the name) or
    // names a class of the Scala or Java standard library (none supplied).
    private def path(full: List[String]): Unit =
      if (!matching(full) && !standard(full)) throughValue(full.last)

    // A type selected through a value: a type member of that name, or a class or trait nested in
    // a class or trait (an object or package on the path would have matched it).
    private def throughValue(v: String): Unit = {
      typeMembers.getOrElse(v, Nil).foreach((stat, owner) => typeMember(stat, Some(owner)))
      templates
        .getOrElse(v, Nil)
        .filter(_.parent.exists(o => o.isTemplate && !o.isObject))
        .foreach(reach)
    }

    // The templates and type members whose path ends with the one given; whether there are any.
    private def matching(full: List[String]): Boolean = {
      val n = full.last
      val found = templates.getOrElse(n, Nil).filter(_.path.endsWith(full))
      val declared =
        typeMembers.getOrElse(n, Nil).filter((_, owner) => (owner.path :+ n).endsWith(full))
      found.foreach(reach)
      declared.foreach((stat, owner) => typeMember(stat, Some(owner)))
      found.nonEmpty || declared.nonEmpty
    }

    private val bounded = mutable.Set.empty[(Input, Int)]

    private def ancestors(tree: Tree): List[Tree] =
      tree.parent.fold(List.empty[Tree])(p => p :: ancestors(p))

    // What a tree around a name declares under it: a type parameter (none for a match type's type
    // variable), or a type member.
    private def scoped(around: Tree, n: String): Option[Either[Option[Type.Param], Stat]] = {
      val params = around match {
        case w: Tree.WithTParamClause => w.tparamClause.values
        case d: Defn.Def              => d.paramClauseGroups.flatMap(_.tparamClause.values)
        case d: Decl.Def              => d.paramClauseGroups.flatMap(_.tparamClause.values)
        case g: Defn.GivenAlias       => g.paramClauseGroups.flatMap(_.tparamClause.values)
        case g: Decl.Given            => g.paramClauseGroups.flatMap(_.tparamClause.values)
        case g: Defn.Given            => g.paramClauseGroups.flatMap(_.tparamClause.values)
        case e: Defn.ExtensionGroup   => e.paramClauseGroup.toList.flatMap(_.tparamClause.values)
        case _                        => Nil
      }
      val variables = around match {
        case c: TypeCase => c.pat.collect { case v: Type.Var => v.name.value }
        case _           => Nil
      }
      val members = around match {
        case t: Template    => t.body.stats
        case r: Type.Refine => r.body.stats
        case b: Term.Block  => b.stats
        case _              => Nil
      }
      params
        .find(_.name.value == n)
        .map(p => Left(Some(p)))
        .orElse(Option.when(variables.contains(n))(Left(None)))
        .orElse(
          members
            .find {
              case d: Defn.Type => d.name.value == n
              case d: Decl.Type => d.name.value == n
              case _            => false
            }
            .map(Right(_))
        )
    }

    private def introduced(n: Type.Name): Boolean = n.parent.exists {
      case p: Type.Param => p.name eq n
      case d: Defn.Type  => d.name eq n
      case d: Decl.Type  => d.name eq n
      case _: Type.Var   => true
      case _             => false
    }

    private def name(n: String): Unit = if (names.add(n)) pendingNames += n

    private def reach(found: Frame): Unit =
      if (!frames.contains(found.id)) {
        frames(found.id) = found
        pendingFrames += found
      }

    // A declaration's signature; one outside the body with no declared type is not known.
    private def declared(tree: Tree): Unit =
      if (untyped(tree) && !inBody(tree)) unknown = true
      else {
        signature(tree).foreach(spell)
        positioned.get(tree.pos.input -> tree.pos.start).foreach(reach)
      }

    // The constructor parameters of a class a `new` in the body builds.
    private def constructed(): Unit =
      body
        .collect { case i: Init => i.tpe.collect { case n: Type.Name => n.value } }
        .flatten
        .flatMap(templates.getOrElse(_, Nil))
        .flatMap(_.tree)
        .foreach {
          case c: Defn.Class => spell(c.ctor)
          case _             => ()
        }

    private def meant(meaning: Meaning): Unit = meaning match {
      case Meaning.Value(Some(tpe), _, _) => spell(tpe)
      case Meaning.Value(None, _, _)      => unknown = true
      case Meaning.Instance(receiver)     => received(receiver)
      case Meaning.Call(_, result, _)     => received(result)
      case Meaning.Methods(definitions)   =>
        definitions.foreach { d =>
          d.frame.parent.flatMap(_.tree).foreach {
            case e: Defn.ExtensionGroup => signature(e).foreach(spell)
            case _                      => ()
          }
          declared(d.tree)
        }
      case Meaning.Module(module)                     => reach(module)
      case Meaning.Companion(tree)                    => declared(tree)
      case _: (Meaning.Package | Meaning.Library)     => ()
      case _: (Meaning.CompileTime | Meaning.Refused) => unknown = true
    }

    private def received(receiver: Receiver): Unit = receiver match {
      case Receiver.Template(t)    => reach(t)
      case Receiver.Open(inner, _) => received(inner)
      case _                       => ()
    }

    // Until nothing new is added: each name's declarations, each template's parents, companion
    // and owners, then the implicits and selected members of every scope found so far.
    @annotation.tailrec
    private def grow(): Unit = {
      while (pending) step()
      if (!unknown) {
        val ids = frames.keySet.toSet
        val found =
          all.filter(s =>
            !read(s.id) && (ids(s.id) || conversions.visible(s, frame, Some(ids), givens = false))
          )
        found.foreach { scope =>
          read += scope.id
          scope.stats.foreach(member)
          exported(scope)
        }
        if (found.nonEmpty) grow()
      }
    }

    private def pending: Boolean =
      !unknown && (pendingNames.nonEmpty || pendingValues.nonEmpty || pendingFrames.nonEmpty)

    private def step(): Unit =
      if (pendingNames.nonEmpty) declarations(pendingNames.dequeue())
      else if (pendingValues.nonEmpty) valued(pendingValues.dequeue())
      else template(pendingFrames.dequeue())

    private def declarations(spelled: String): Unit =
      (renamings.getOrElse(spelled, Set.empty) + spelled).foreach { n =>
        templates.getOrElse(n, Nil).foreach(reach)
        typeMembers.getOrElse(n, Nil).foreach((stat, owner) => typeMember(stat, Some(owner)))
      }

    // An alias brings its body; an opaque or abstract type also the object of its name in its
    // scope (any of that name, when the scope is not a supplied one), and its bounds.
    private def typeMember(stat: Stat, owner: Option[Frame]): Unit =
      if (members.add(stat.pos.input -> stat.pos.start)) stat match {
        case alias: Defn.Type =>
          spell(alias.body)
          if (alias.mods.hasOpaque) sameNamed(owner, alias.name.value)
        case abstr: Decl.Type =>
          spell(abstr.bounds)
          sameNamed(owner, abstr.name.value)
        case _ => ()
      }

    private val members = mutable.Set.empty[(Input, Int)]

    private def sameNamed(owner: Option[Frame], n: String): Unit =
      owner.fold(templates.getOrElse(n, Nil).filter(_.isObject).foreach(reach))(sameNamed(_, n))

    private def sameNamed(owner: Frame, n: String): Unit =
      conversions.implicitScope.objects.getOrElse(owner.path :+ n, Nil).foreach(reach)

    // A value whose singleton type the closure spells: an object of that name, or the declared
    // type of a member value of it (a parameter's type is spelled in its own signature).
    private def valued(v: String): Unit = {
      templates.getOrElse(v, Nil).filter(_.isObject).foreach(reach)
      valueMembers.getOrElse(v, Nil).foreach(declared)
    }

    private def template(found: Frame): Unit = {
      found.parents.foreach(init => spell(init.tpe))
      found.typeParams.foreach(p => spell(p.bounds))
      conversions.implicitScope.of(found).foreach(reach)
    }

    // An implicit of a scope found is a given the body may summon; a member of a name it selects
    // may answer through a conversion's result, an extension method of that name likewise.
    private def member(stat: Stat): Unit = stat match {
      case e: Defn.ExtensionGroup =>
        val methods = ScopeTerms.extensionMethods(e).filter(m => selected(m.name.value))
        if (methods.nonEmpty) {
          signature(e).foreach(spell)
          methods.foreach(declared)
        }
      case _ if implicitly(stat) || MemberSyntax.names(stat).exists(selected) => declared(stat)
      case _                                                                  => ()
    }

    // The scopes an export clause of a scope found defines members of again.
    private def exported(scope: Frame): Unit =
      scope.exported.foreach(importer =>
        SourceIndex.importPath(importer.ref) match {
          case Some(path) =>
            all.filter(s => path.lastOption == s.path.lastOption).foreach(reach)
          case None => unknown = true
        }
      )

  }

}
