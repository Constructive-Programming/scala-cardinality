package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, Input, Target, TypeEntry}
import cardinality.analysis.SourceIndex.{givenName, Scope, Written}
import cardinality.analysis.model.{ContextBounds, MemberSyntax, SourceBindings}
import cardinality.types.Trees.{ClassDefn, TemplateDefn}
import cardinality.types.{Kinds, Memo, Spelling, TypeApplications, TypeName}

final private[cardinality] class SourceIndex(inputs: List[Input], work: Option[AnalysisWork]) {
  private given scala3: Dialect = dialects.Scala3

  private[cardinality] val types = mutable.ListBuffer.empty[TypeEntry]
  private[cardinality] val targets = mutable.ListBuffer.empty[Target]
  private[cardinality] val packageFrames = mutable.ListBuffer.empty[Frame]
  private[cardinality] val moduleFrames = mutable.ListBuffer.empty[Frame]
  private[cardinality] val givenFrames = mutable.ListBuffer.empty[Frame]
  private val givens = mutable.Set.empty[(String, String)]
  // Import clauses bind simple names in the block that holds them, from their own position
  // onward. The binding records what each introduces: explicit names with the qualified key
  // they reach, and the prefixes a wildcard opens.
  private[cardinality] val imports = mutable.ListBuffer.empty[(String, ImportBinding)]
  // Export clauses define members of the template that holds them, so they bind across the whole
  // body, not from a position on: indexed apart from imports, with the importers whose prefix is
  // no pure path (`export f().x.*`) kept as written, since any name may stand behind them.
  private[cardinality] val exports = mutable.ListBuffer.empty[(String, ExportClause)]
  // Every proper binder that carries a subtype bound, by its atom, with the frame declaring it: the
  // bound is read wherever the measurement meets the atom (`BinderBounds`).
  private[cardinality] val boundedBinders = mutable.LinkedHashMap.empty[String, (Frame, Type.Param)]
  // How many scopes of each kind a frame path has opened in each file: the ordinal names the next.
  private val ordinals = mutable.Map.empty[(String, List[String], String), Int]

  private val scopes = new BodyScopes(
    BodyScopes.holders(inputs.map(_.tree)),
    (input, owner, opening) => opened(input, owner, opening),
    (input, frame) => index(input, frame)
  )

  inputs.sortBy(_.path).foreach { input =>
    val root = Frame(input.path, Nil, None, input.tree.stats)
    packageFrames += root
    index(input.path, root)
  }

  // `sealed` is the compiler's promise that every direct subtype of a trait or abstract class
  // is declared in the same source file. The registry keeps that promise checkable: subtypes
  // are indexed by the file of their declaration and the simple name their `extends` clauses
  // spell, so a sealed family's enumeration is a lookup, not a guess.
  private[cardinality] val subtypes: Map[(String, String), List[TypeEntry]] =
    (typeSubtypes.iterator ++ objectSubtypes.iterator).toList
      .groupBy(_._1)
      .map { (key, found) => key -> found.map(_._2) }

  private def typeSubtypes: List[((String, String), TypeEntry)] =
    types.iterator.flatMap { entry =>
      initNames(entry.tree).map(parent => ((entry.owner.chain.last.id, parent), entry))
    }.toList

  // Companion objects are not type declarations — the name index keeps terms and types apart —
  // but a `case object` is exactly a value a sealed family counts, so the subtype registry
  // reads them from the module frames as entries of their own.
  private def objectSubtypes: List[((String, String), TypeEntry)] =
    moduleFrames.iterator.flatMap { module =>
      module.tree.toList.flatMap { tree =>
        initNames(tree).map(parent =>
          (
            (module.chain.last.id, parent),
            TypeEntry(module.path.last, tree, module.parent.getOrElse(module))
          )
        )
      }
    }.toList

  // The parents a `given` instantiation extends: a given is a value, and sealed says nothing
  // about it being one of the enumerated cases, so a family a given reaches into is not one
  // this fragment can read whole.
  private[cardinality] val givenTargets: Set[(String, String)] = givens.toSet

  // The parents an anonymous class instance (`new P {…}`) extends, by file: such a value belongs to
  // a sealed family without being one of its named subtypes, and `sealed` lets it appear only in
  // the family's own file, so the file is the whole search.
  private[cardinality] val anonymousTargets: Set[(String, String)] =
    inputs.iterator.flatMap { input =>
      input.tree
        .collect { case n: Term.NewAnonymous => n.templ.inits.map(TypeApplications.parentName) }
        .flatten
        .map(input.path -> _)
    }.toSet

  // Named classes, traits, objects and enums nested anywhere but in packages and objects — in a
  // class, trait or given body, or in any block (a method's, a val's, a lambda's, an if arm's) — by
  // file and the parent name their clauses spell: each is a class per enclosing instance or
  // evaluation, so a sealed family one of them extends is not the sum of its static children.
  private[cardinality] val innerChildren: Map[(String, String), List[String]] =
    inputs.iterator
      .flatMap { input =>
        input.tree.collect {
          case d: TemplateDefn if !static(d) =>
            d.templ.inits.map(init =>
              (input.path, TypeApplications.parentName(init)) -> d.name.value
            )
        }.flatten
      }
      .toList
      .groupMap(_._1)(_._2)

  private def static(tree: Tree): Boolean =
    Iterator.iterate(tree.parent)(_.flatMap(_.parent)).takeWhile(_.nonEmpty).flatten.forall {
      case _: Source | _: Pkg | _: Pkg.Body | _: Pkg.Object | _: Defn.Object | _: Template.Body =>
        true
      case t: Template => t.parent.exists(p => p.is[Defn.Object] || p.is[Pkg.Object])
      case _           => false
    }

  // The type aliases that may denote a family: those whose body mentions its name or another such
  // alias, by simple name — a superset, so a family is refused rather than enumerated short.
  private[cardinality] def aliasesOf(family: String): Set[String] =
    aliases(family)(closeAliases(Set(family)) - family)

  private val aliases = new Memo[String, Set[String]]

  private lazy val aliasBodies: List[(String, Set[String])] =
    types.iterator.collect {
      case TypeEntry(name, d: Defn.Type, _, _) =>
        name -> TypeName.words(d.body).toSet
    }.toList

  @scala.annotation.tailrec
  private def closeAliases(found: Set[String]): Set[String] = {
    val next = found ++ aliasBodies.collect { case (name, words) if words.exists(found) => name }
    if (next == found) found else closeAliases(next)
  }

  /** Why a sealed family's named subtypes in its file are not all of its instances, if they are
    * not: a `given` instance, an anonymous class, a parent clause through an alias, or a class
    * nested in an instance or a block is a value of the family no named subtype enumerates.
    */
  private[cardinality] def unnamedInstances(file: String, family: String): Option[String] =
    if (givenTargets.contains((file, family)))
      Some(s"sealed family has a given instance outside the fragment: $family")
    else if (anonymousTargets.contains((file, family)))
      Some(s"sealed family has anonymous instances in its file: $family")
    else if (throughAlias(file, family))
      Some(s"sealed family is extended through an alias: $family")
    else
      innerChildren
        .get((file, family))
        .map(children => s"sealed subtype is an inner or local class: ${children.head}")

  // Children are registered by the parent name their clause spells, so a child or an anonymous
  // instance written against an alias of the family (`type SA = S; new SA {}`) is not among them.
  // Any alias whose body mentions the family, or such an alias, used as a parent in the family's
  // file makes the enumeration incomplete.
  private def throughAlias(file: String, family: String): Boolean =
    aliasesOf(family).exists(alias =>
      subtypes.contains((file, alias)) ||
        innerChildren.contains((file, alias)) ||
        anonymousTargets((file, alias)) ||
        givenTargets((file, alias))
    )

  // The declarations of a name in one file: uniqueness here is what makes a `sealed` parent's
  // simple name in a child's `extends` clause denote exactly the family being enumerated.
  private[cardinality] def typesInFile(file: String, name: String): List[TypeEntry] =
    typesByFile.getOrElse((file, name), Nil)

  // Read only once indexing is done (measurements), so built then, in declaration order.
  private lazy val typesByFile: Map[(String, String), List[TypeEntry]] =
    types.toList
      .filter(entry => !entry.tree.is[Defn.Object])
      .groupBy(entry => (entry.owner.chain.last.id, entry.name))

  private def initNames(tree: Tree): List[String] = tree match {
    case d: TemplateDefn =>
      d.templ.inits.map(TypeApplications.parentName)
    case _ => Nil
  }

  private def inspect(operation: String): Unit = work.foreach(_.step(operation))

  // A frame `node` declares under `owner`. Its path is the owner's extended by its name except where
  // the two differ: an enum case is a member of the enum's companion but typed where the enum is
  // declared, a secondary constructor typed outside the class it is a member of.
  private def child(input: String, node: Tree, path: List[String], owner: Frame, scope: Scope) =
    registered(SourceIndex.declared(input, node, path, owner, scope))

  // A frame the index opens records its subtype-bounded binders.
  private def registered(frame: Frame): Frame = {
    frame.typeParams
      .filter(p => p.tparamClause.values.isEmpty && Kinds.subtypeBounded(p))
      .foreach(p => boundedBinders.update(SourceBindings.atom(frame, p), frame -> p))
    frame
  }

  // A scope a body opens, named by its kind and ordinal under its owner's path in its file. Its id
  // carries the name too: a braceless block starts where its first definition does.
  private def opened(input: String, owner: Frame, opening: BodyScopes.Opening): Frame = {
    val name = s"<${opening.kind}#${ordinal(input, owner.path, opening.kind)}>"
    registered(
      Frame(
        s"$input:${opening.node.pos.start}:$name",
        owner.path :+ name,
        Some(owner),
        opening.stats,
        typeParams = opening.tparams,
        params = opening.params,
        local = opening.local,
        parents = opening.parents,
        tree = Some(opening.node),
        refusals = opening.refusals
      )
    )
  }

  private def ordinal(input: String, path: List[String], kind: String): Int = {
    val next = ordinals.getOrElse((input, path, kind), 0) + 1
    ordinals.update((input, path, kind), next)
    next
  }

  // One pass over a frame's statements, naming what each declaration introduces for the second
  // pass to resolve. Each kind gets its own method: what differs between them is the body a
  // definition opens, its binders, and the parameters it takes.
  private def index(input: String, frame: Frame): Unit =
    frame.stats.foreach { stat =>
      inspect("index statement")
      indexStat(input, frame, stat)
    }

  private def indexStat(input: String, frame: Frame, stat: Stat): Unit = {
    val _ = stat match {
      case p: Pkg         => indexPackage(input, frame, p)
      case p: Pkg.Object  => indexPackageObject(input, frame, p)
      case i: Import      => indexImport(frame, i)
      case e: Export      => indexExport(frame, e)
      case d: Defn.Type   => named(d.name.value, d, frame)
      case d: Decl.Type   => named(d.name.value, d, frame)
      case d: Defn.Class  => indexClass(input, frame, d)
      case d: Defn.Trait  => indexClass(input, frame, d)
      case d: Defn.Object => indexObject(input, frame, d)
      case d: Defn.Enum   => indexEnum(input, frame, d)
      // NOTE both kind methods above register the name: an enum's cases and a trait's members
      // are resolved through the declaration this puts in `types`.
      case d: Defn.Given =>
        d.templ.inits.foreach(init =>
          givens += frame.chain.last.id -> TypeApplications.parentName(init)
        )
        indexGiven(input, frame, d)
      case d: Defn.ExtensionGroup => indexExtension(input, frame, d)
      case d: Defn.Def            =>
        method(input, d, Written.of(d.name, d.paramClauseGroups, d.decltpe), Some(d.body), frame)
      // A nullary given alias is a lazy given value (3.9.0): in a block, a step like a local value.
      case d: Defn.GivenAlias if frame.local && d.paramClauseGroups.isEmpty =>
        scopes.walk(input, d.body, frame)
      case d: Defn.GivenAlias         => method(input, d, givenWritten(d), Some(d.body), frame)
      case d: (Decl.Def | Decl.Given) => abstractMethod(input, frame, d)
      case d: (Defn.Val | Defn.Var | Decl.Val | Decl.Var) => values(input, frame, d)
      case c: Ctor.Secondary                              => secondary(input, frame, c)
      // A template's initializer statements and a block's expressions may open scopes holding
      // definitions.
      case t: Term => scopes.walk(input, t, frame)
      case _       => ()
    }
  }

  private def named(name: String, tree: Stat, frame: Frame): Unit =
    types += TypeEntry(name, tree, frame)

  private def indexImport(frame: Frame, clause: Import): Unit =
    clause.importers.foreach { importer =>
      val binding = SourceIndex
        .importPath(importer.ref)
        .fold(ImportBinding.unfollowed(importer))(
          ImportBinding.of(importer, _)
        )
      if (
        binding.named.nonEmpty || binding.wildcards.nonEmpty || binding.givens.nonEmpty ||
        binding.unfollowed.nonEmpty
      )
        imports += ((frame.id, binding))
    }

  private def indexExport(frame: Frame, clause: Export): Unit =
    clause.importers.foreach { importer =>
      val binding = SourceIndex.importPath(importer.ref).map(ImportBinding.of(importer, _))
      exports += ((frame.id, ExportClause(importer, binding)))
    }

  private def indexPackage(input: String, frame: Frame, pkg: Pkg): Unit = {
    val nested = frame.copy(
      id = s"$input:${pkg.pos.start}:package",
      path = frame.path ++ pkg.ref.syntax.split('.'),
      parent = Some(frame),
      stats = pkg.body.stats
    )
    packageFrames += nested
    index(input, nested)
  }

  // A package object's parents pass their members on to its package.
  private def indexPackageObject(input: String, frame: Frame, pkg: Pkg.Object): Unit = {
    val nested = registered(
      SourceIndex
        .frame(input, pkg, pkg.name.value, frame, pkg.templ.body.stats, Nil, Nil)
        .copy(parents = pkg.templ.inits)
    )
    packageFrames += nested
    index(input, nested)
  }

  // Every class has a primary constructor — an abstract class's is the one its subclasses call —
  // and a trait with a parameter clause one its first implementing class calls.
  private def indexClass(input: String, frame: Frame, d: Defn.Class | Defn.Trait): Unit = {
    val nested = indexTemplate(input, frame, d)
    if (d.is[Defn.Class] || d.ctor.paramClauses.nonEmpty)
      constructor(input, d, nested, d.name.syntax, d.tparamClause :: d.ctor.paramClauses.toList)
  }

  // An enum's parameterized cases are classes of its companion, each with its constructor. Their
  // signatures are typed where the enum is declared, with the enum's type parameters when the case
  // declares none, whether or not it has a parent of its own: 3.9.0 gives `case D(y: A) extends
  // E[Int]` all of `E[A, B]`'s binders (`case class D[A, B](y: A)`, `-Vprint:typer`), and one whose
  // parameters mention none of them no binder they could see. It finds neither the enum's members
  // (E006) nor its companion's ("illegal reference … from enum case") there.
  private def indexEnum(input: String, frame: Frame, d: Defn.Enum): Unit = {
    val nested = indexTemplate(input, frame, d)
    d.templ.body.stats.foreach {
      case c: Defn.EnumCase if c.ctor.paramClauses.nonEmpty =>
        val own = c.tparamClause.values
        val tparams = if (own.nonEmpty) own else d.tparamClause.values
        val params = c.ctor.paramClauses.toList.flatMap(_.values)
        val scope = child(input, c, nested.path :+ c.name.value, frame, Scope(Nil, tparams, params))
        constructor(input, c, scope, c.name.syntax, c.tparamClause :: c.ctor.paramClauses.toList)
      case _ => ()
    }
  }

  // A constructor's inputs are available when choosing its output fields. The instance and its
  // members do not exist yet, so they are not constructor captures.
  private def constructor(
      input: String,
      node: Tree,
      template: Frame,
      head: String,
      clauses: List[Tree]
  ): Unit =
    targets += Target(
      input,
      node,
      (template.path :+ "<init>").mkString("."),
      head + clauses.map(Spelling.of).mkString,
      template.copy(stats = Nil, parents = Nil),
      None,
      constructor = true
    )

  // A secondary constructor's signature and self-invocation are typed where its class is declared,
  // with the class's binders and its bounds' evidence ([30] *Constructor Definitions*): its row
  // measures the primary constructor's arguments over its parameters (`SecondaryConstructors`).
  // The rest of the body is typed as a function body in the class: its definitions see the class's
  // members and the secondary's parameters, under `<init#k>`, k its ordinal among the class's.
  private def secondary(input: String, template: Frame, c: Ctor.Secondary): Unit =
    template.tree match {
      case Some(d: Defn.Class) =>
        val params = c.paramClauses.toList.flatMap(_.values)
        val frame = secondaryTarget(input, template, d, c, params)
        c.body.init.argClauses.foreach(scopes.walk(input, _, frame))
        secondaryBody(input, template, c, params)
      case _ => ()
    }

  private def secondaryTarget(
      input: String,
      template: Frame,
      d: Defn.Class,
      c: Ctor.Secondary,
      params: List[Term.Param]
  ): Frame = {
    val path = template.path :+ "<init>"
    val scope = Scope(Nil, d.tparamClause.values, params, local = true)
    val frame = child(input, c, path, template.parent.getOrElse(template), scope)
    targets += Target(
      input,
      c,
      path.mkString("."),
      SecondaryConstructors.signature(d, c),
      frame,
      Some(SecondaryConstructors.primaryArguments(d)),
      constructor = false
    )
    frame
  }

  private def secondaryBody(
      input: String,
      template: Frame,
      c: Ctor.Secondary,
      params: List[Term.Param]
  ): Unit = {
    val name = s"<init#${ordinal(input, template.path, "init")}>"
    val body = Frame(
      s"$input:${c.body.pos.start}:$name",
      template.path :+ name,
      Some(template),
      c.body.stats,
      params = params,
      local = true,
      tree = Some(c.body)
    )
    index(input, body)
  }

  private def indexTemplate(
      input: String,
      frame: Frame,
      d: ClassDefn
  ): Frame = {
    val nested = registered(SourceIndex.templateFrame(frame, d))
    // Named in the enclosing frame, so a sibling reference resolves as before; its own frame
    // travels with it, because a member of it — a declared field a subclass inherits — reads in
    // the scope that declares it.
    types += TypeEntry(d.name.value, d, frame, Some(nested))
    index(input, nested)
    nested
  }

  private def indexObject(input: String, frame: Frame, d: Defn.Object): Unit = {
    val module = registered(SourceIndex.templateFrame(frame, d))
    moduleFrames += module
    index(input, module)
  }

  // A conditional given instance is a class whose constructor takes its clauses; an unconditional
  // one is an object.
  private def indexGiven(input: String, frame: Frame, d: Defn.Given): Unit = {
    val nested = registered(SourceIndex.givenFrame(frame, d))
    givenFrames += nested
    index(input, nested)
    if (d.paramClauseGroups.nonEmpty) {
      val head = if (d.name.value.isEmpty) "given" else d.name.syntax
      constructor(input, d, nested, head, d.paramClauseGroups.flatMap(clauses))
    }
  }

  // A given alias is a method of its clauses, spelled as one: `given name[A](using B): T`.
  private def givenWritten(d: Defn.GivenAlias | Decl.Given): Written = {
    val (groups, result) = d match {
      case g: Defn.GivenAlias => (g.paramClauseGroups, g.decltpe)
      case g: Decl.Given      => (g.paramClauseGroups, g.decltpe)
    }
    val head =
      if (d.name.value.isEmpty) "given"
      else "given " + Spelling.declared(d.name, beforeColon = groups.isEmpty)
    Written(givenName(d), head, groups, Some(result))
  }

  private def abstractMethod(input: String, frame: Frame, d: Decl.Def | Decl.Given): Unit = {
    val written = d match {
      case m: Decl.Def   => Written.of(m.name, m.paramClauseGroups, Some(m.decltpe))
      case g: Decl.Given => givenWritten(g)
    }
    method(input, d, written, None, frame, declaration = true)
  }

  // A member value is a definition of its own, a nullary signature of its type; a block's local
  // value is a step of the definition around it, which that definition's row counts, so only the
  // scopes its right-hand side opens are indexed. A declared type types each name only of plain
  // names: `val (a, b): (A, B)` types the pattern. A shared right-hand side is walked once.
  private def values(
      input: String,
      frame: Frame,
      d: Defn.Val | Defn.Var | Decl.Val | Decl.Var
  ): Unit = {
    val (keyword, pats, declared, rhs) = d match {
      case v: Defn.Val => ("val", v.pats, v.decltpe, Some(v.rhs))
      case v: Defn.Var => ("var", v.pats, v.decltpe, Some(v.body))
      case v: Decl.Val => ("val", v.pats, Some(v.decltpe), None)
      case v: Decl.Var => ("var", v.pats, Some(v.decltpe), None)
    }
    val names = pats.flatMap(_.collect { case Pat.Var(name) => name })
    if (frame.local || names.isEmpty) rhs.foreach(scopes.walk(input, _, frame))
    else {
      val result = declared.filter(_ => pats.forall(_.is[Pat.Var]))
      names.zipWithIndex.foreach { (name, at) =>
        val head = s"$keyword ${Spelling.declared(name, beforeColon = true)}"
        val body = rhs.filter(_ => at == 0)
        val written = Written(name.value, head, Nil, result)
        method(input, d, written, body, frame, declaration = rhs.isEmpty)
      }
    }
  }

  private def indexExtension(input: String, frame: Frame, d: Defn.ExtensionGroup): Unit =
    index(input, registered(SourceIndex.extensionFrame(d, frame)))

  private def method(
      input: String,
      node: Tree,
      written: Written,
      body: Option[Tree],
      owner: Frame,
      declaration: Boolean = false
  ): Unit = {
    val groups = written.groups
    val frame = registered(SourceIndex.methodFrame(node, written.name, groups, owner))
    // Each clause is spelled on its own, so curried clauses read `(x: A)(y: A)` however they were
    // laid out; `<inferred>` is no Scala the tokenizer can read back.
    val signature =
      written.head + groups.flatMap(clauses).map(Spelling.of).mkString +
        written.result.fold(": <inferred>")(t => s": ${Spelling.of(t)}")
    targets += Target(
      input,
      node,
      frame.path.mkString("."),
      signature,
      frame,
      written.result,
      constructor = false,
      declaration = declaration
    )
    // Local methods capture their enclosing parameters and preceding local values. We do not
    // count the analysed method's own implementation locals as inputs to its signature.
    body.foreach(scopes.body(input, frame, _))
  }

  private def clauses(group: Member.ParamClauseGroup): List[Tree] =
    group.tparamClause :: group.paramClauses

}

object SourceIndex {

  /** What a frame binds: its statements, type and term parameters, a template's parents, and
    * whether its statements are positional (a block's are; a template's are not).
    */
  final private case class Scope(
      stats: List[Stat],
      tparams: List[Type.Param] = Nil,
      params: List[Term.Param] = Nil,
      parents: List[Init] = Nil,
      local: Boolean = false
  )

  /** How a target is written: its name, the head its signature is spelled from (a method's name,
    * `given name`, `val name`), its clauses and its declared result.
    */
  final private[cardinality] case class Written(
      name: String,
      head: String,
      groups: List[Member.ParamClauseGroup],
      result: Option[Type]
  )

  private[cardinality] object Written {

    def of(name: Term.Name, groups: List[Member.ParamClauseGroup], result: Option[Type]): Written =
      Written(name.value, Spelling.declared(name, beforeColon = groups.isEmpty), groups, result)

  }

  /** The frame a declaration opens: its id is its position in its source, the key every reading of
    * it shares (an inline body's callees are judged under the frame the index gave them).
    */
  private[cardinality] def frame(
      input: String,
      node: Tree,
      name: String,
      owner: Frame,
      stats: List[Stat],
      tparams: List[Type.Param],
      params: List[Term.Param],
      local: Boolean = false,
      parents: List[Init] = Nil
  ): Frame =
    declared(input, node, owner.path :+ name, owner, Scope(stats, tparams, params, parents, local))

  /** A frame under `owner` at `path`, which need not extend the owner's (the index's `child`). */
  private def declared(
      input: String,
      node: Tree,
      path: List[String],
      owner: Frame,
      scope: Scope
  ): Frame =
    Frame(
      s"$input:${node.pos.start}",
      path,
      Some(owner),
      scope.stats,
      typeParams = scope.tparams,
      params = scope.params ++ ContextBounds.evidence(scope.tparams),
      local = scope.local,
      parents = scope.parents,
      tree = Some(node)
    )

  /** The frame of a definition's clauses: their type and term parameters, read in `owner`. */
  private[cardinality] def clausesFrame(
      node: Tree,
      name: String,
      owner: Frame,
      stats: List[Stat],
      groups: List[Member.ParamClauseGroup],
      local: Boolean = true
  ): Frame =
    frame(
      owner.input,
      node,
      name,
      owner,
      stats,
      groups.flatMap(_.tparamClause.values),
      groups.flatMap(_.paramClauses).flatMap(_.values),
      local
    )

  /** The frame of a method's own parameters, as the index opens it. */
  private[cardinality] def methodFrame(
      node: Tree,
      name: String,
      groups: List[Member.ParamClauseGroup],
      owner: Frame
  ): Frame = clausesFrame(node, name, owner, Nil, groups)

  /** A method's or given alias's frame, under its first name and its own clauses. */
  private[cardinality] def methodFrame(d: Defn.Def | Defn.GivenAlias, owner: Frame): Frame =
    methodFrame(d, MemberSyntax.names(d).headOption.getOrElse(""), MemberSyntax.groups(d), owner)

  /** A named template's frame: its body, its binders and constructor parameters, its parents. */
  private[cardinality] def templateFrame(
      owner: Frame,
      d: TemplateDefn
  ): Frame = d match {
    case o: Defn.Object =>
      frame(
        owner.input,
        o,
        o.name.value,
        owner,
        o.templ.body.stats,
        Nil,
        Nil,
        parents = o.templ.inits
      )
    case c: ClassDefn =>
      frame(
        owner.input,
        c,
        c.name.value,
        owner,
        c.templ.body.stats,
        c.tparamClause.values,
        c.ctor.paramClauses.toList.flatMap(_.values),
        parents = c.templ.inits
      )
  }

  /** A given instance's frame: its clauses, its body, its parents. */
  private[cardinality] def givenFrame(owner: Frame, d: Defn.Given): Frame =
    clausesFrame(d, givenName(d), owner, d.templ.body.stats, d.paramClauseGroups, local = false)
      .copy(parents = d.templ.inits)

  /** A given's path segment: its name, or its line when it has none. */
  private[cardinality] def givenName(d: Defn.Given | Defn.GivenAlias | Decl.Given): String =
    if (d.name.value.isEmpty) s"<given@${d.pos.startLine + 1}>" else d.name.value

  /** An anonymous class's frame: its body and its parents. */
  private[cardinality] def anonymousFrame(owner: Frame, n: Term.NewAnonymous): Frame =
    frame(
      owner.input,
      n,
      s"<anonymous@${n.pos.startLine + 1}>",
      owner,
      n.templ.body.stats,
      Nil,
      Nil,
      parents = n.templ.inits
    )

  /** The frame of an extension group, whose receiver clauses its methods share. */
  private[cardinality] def extensionFrame(group: Defn.ExtensionGroup, owner: Frame): Frame =
    clausesFrame(
      group,
      extensionName(group),
      owner,
      extensionStats(group),
      group.paramClauseGroup.toList
    )

  /** An extension group has no name of its own; its path segment is its line. */
  private def extensionName(group: Defn.ExtensionGroup): String =
    s"<extension@${group.pos.startLine + 1}>"

  private def extensionStats(group: Defn.ExtensionGroup): List[Stat] = group.body match {
    case b: Term.Block => b.stats
    case s             => List(s)
  }

  /** A pure dotted term path; anything deeper (a `this`, a term prefix) names no package the model
    * can reach and binds nothing. A leading `_root_` stays as the path's first segment: it anchors
    * the path at the root package instead of at the enclosing packages, so it is kept for lookup to
    * read rather than stripped — and only there, since the segment names the root nowhere else.
    */
  def importPath(ref: Term): Option[List[String]] =
    segments(ref).filterNot(_.drop(1).contains(Root))

  /** The anchor segment of an absolute path. */
  val Root = "_root_"

  /** The name an explicit selector binds: its own, or a rename's new one. */
  def boundName(importee: Importee): Option[String] = importee match {
    case e: Importee.Name   => Some(e.name.value)
    case e: Importee.Rename => Some(e.rename.value)
    case _                  => None
  }

  /** The names an importer's wildcard leaves out: an unimport (`N as _`) and the original of a
    * rename, which Scala binds only under its new name.
    */
  def hiddenNames(importer: Importer): Set[String] =
    importer.importees.collect {
      case e: Importee.Unimport => e.name.value
      case e: Importee.Rename   => e.name.value
    }.toSet

  private def segments(ref: Term): Option[List[String]] = ref match {
    case n: Term.Name   => Some(List(n.value))
    case s: Term.Select => segments(s.qual).map(_ :+ s.name.value)
    case _              => None
  }

}
