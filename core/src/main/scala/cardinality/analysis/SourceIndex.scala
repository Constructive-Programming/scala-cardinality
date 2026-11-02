package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, Input, Target, TypeEntry}
import cardinality.analysis.model.{ContextBounds, SourceBindings}
import cardinality.types.{Kinds, Spelling, TypeApplications, TypeName}

/** One importer of an import or export clause: where it stands, the names its explicit selectors
  * bind with the path each reaches, the prefixes its wildcard opens, and the names that wildcard
  * leaves out — an unimport (`N as _`) and the original of a rename, which Scala binds only under
  * its new name.
  */
final private[cardinality] case class ImportBinding(
    start: Int,
    named: List[(String, List[String])],
    wildcards: List[List[String]],
    hidden: Set[String] = Set.empty
) {

  /** The prefixes this importer's wildcard opens for `name`: none when the name is left out. */
  def opening(name: String): List[List[String]] = if (hidden(name)) Nil else wildcards

}

/** One importer of an export clause, with what it binds when its prefix is a pure path. */
final private[cardinality] case class ExportClause(
    importer: Importer,
    binding: Option[ImportBinding]
) {

  /** The prefix as written. */
  def prefix: String = Spelling.of(importer.ref)

}

private[cardinality] object ExportClause {

  /** Whether an exporter may define a member under the name whatever its prefix holds: a selector
    * spelling it, or a wildcard that does not leave it out.
    */
  def mayDefine(importer: Importer, name: String): Boolean =
    importer.importees.exists {
      case _: Importee.Wildcard => !SourceIndex.hiddenNames(importer)(name)
      case selector             => SourceIndex.boundName(selector).contains(name)
    }

}

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
        .collect { case n: Term.NewAnonymous => n.templ.inits.map(initParentName) }
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
          case d: (Defn.Class | Defn.Trait | Defn.Object | Defn.Enum) if !static(d) =>
            d.templ.inits.map(init => (input.path, initParentName(init)) -> d.name.value)
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
    aliases.getOrElseUpdate(family, closeAliases(Set(family)) - family)

  private val aliases = mutable.Map.empty[String, Set[String]]

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
    types.iterator
      .filter(entry => entry.owner.chain.last.id == file && entry.name == name)
      .filter(entry => !entry.tree.is[Defn.Object])
      .toList

  private def initNames(tree: Tree): List[String] = tree match {
    case d: (Defn.Class | Defn.Trait | Defn.Enum | Defn.Object) => d.templ.inits.map(initParentName)
    case _                                                      => Nil
  }

  /** The simple name a parent of an `extends` clause spells. */
  private[cardinality] def initParentName(init: Init): String =
    TypeApplications.name(init.tpe).takeWhile(_ != '[').split('.').last

  private def inspect(operation: String): Unit = work.foreach(_.step(operation))

  // A frame the index opens records its subtype-bounded binders.
  private def opened(frame: Frame): Frame = {
    frame.typeParams
      .filter(p => p.tparamClause.values.isEmpty && Kinds.subtypeBounded(p))
      .foreach(p => boundedBinders.update(SourceBindings.atom(frame, p), frame -> p))
    frame
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
      case d: Defn.Trait  => indexTemplate(input, frame, d)
      case d: Defn.Object => indexObject(input, frame, d)
      case d: Defn.Enum   => indexTemplate(input, frame, d)
      // NOTE both kind methods above register the name: an enum's cases and a trait's members
      // are resolved through the declaration this puts in `types`.
      case d: Defn.Given =>
        d.templ.inits.foreach(init => givens += frame.chain.last.id -> initParentName(init))
        indexGiven(input, frame, d)
      case d: Defn.ExtensionGroup => indexExtension(input, frame, d)
      case d: Defn.Def            =>
        method(input, d, d.name, d.paramClauseGroups, d.decltpe, Some(d.body), frame)
      case d: Decl.Def =>
        method(
          input,
          d,
          d.name,
          d.paramClauseGroups,
          Some(d.decltpe),
          None,
          frame,
          declaration = true
        )
      case _ => ()
    }
  }

  private def named(name: String, tree: Stat, frame: Frame): Unit =
    types += TypeEntry(name, tree, frame)

  private def indexImport(frame: Frame, clause: Import): Unit =
    clause.importers.foreach { importer =>
      importPath(importer.ref).foreach { path =>
        val binding = SourceIndex.binding(importer, path)
        if (binding.named.nonEmpty || binding.wildcards.nonEmpty)
          imports += ((frame.id, binding))
      }
    }

  private def indexExport(frame: Frame, clause: Export): Unit =
    clause.importers.foreach { importer =>
      val binding = importPath(importer.ref).map(SourceIndex.binding(importer, _))
      exports += ((frame.id, ExportClause(importer, binding)))
    }

  // A pure dotted term path, a leading `_root_` kept as the anchor; anything deeper (a `this`, a
  // term prefix) names no package the model can reach and binds nothing.
  private def importPath(ref: Term): Option[List[String]] = SourceIndex.importPath(ref)

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

  private def indexPackageObject(input: String, frame: Frame, pkg: Pkg.Object): Unit = {
    val nested =
      opened(SourceIndex.frame(input, pkg, pkg.name.value, frame, pkg.templ.body.stats, Nil, Nil))
    packageFrames += nested
    index(input, nested)
  }

  private def indexClass(input: String, frame: Frame, d: Defn.Class): Unit = {
    val nested = indexTemplate(input, frame, d)
    if (!d.mods.exists(_.is[Mod.Abstract]))
      // A constructor's inputs are available when choosing its output fields. The instance and
      // its members do not exist yet, so they are not constructor captures.
      targets += Target(
        input,
        d,
        (nested.path :+ "<init>").mkString("."),
        d.name.syntax + Spelling.of(d.tparamClause) + d.ctor.paramClauses.map(Spelling.of).mkString,
        nested.copy(stats = Nil, parents = Nil),
        None,
        constructor = true
      )
  }

  private def indexTemplate(
      input: String,
      frame: Frame,
      d: Defn.Class | Defn.Trait | Defn.Enum
  ): Frame = {
    val nested = opened(SourceIndex.templateFrame(frame, d))
    // Named in the enclosing frame, so a sibling reference resolves as before; its own frame
    // travels with it, because a member of it — a declared field a subclass inherits — reads in
    // the scope that declares it.
    types += TypeEntry(d.name.value, d, frame, Some(nested))
    index(input, nested)
    nested
  }

  private def indexObject(input: String, frame: Frame, d: Defn.Object): Unit = {
    val module = opened(SourceIndex.templateFrame(frame, d))
    moduleFrames += module
    index(input, module)
  }

  private def indexGiven(input: String, frame: Frame, d: Defn.Given): Unit = {
    val instance = opened(SourceIndex.givenFrame(frame, d))
    givenFrames += instance
    index(input, instance)
  }

  private def indexExtension(input: String, frame: Frame, d: Defn.ExtensionGroup): Unit =
    index(input, opened(SourceIndex.extensionFrame(d, frame)))

  private def method(
      input: String,
      node: Tree,
      name: Term.Name,
      groups: List[Member.ParamClauseGroup],
      result: Option[Type],
      body: Option[Term],
      owner: Frame,
      declaration: Boolean = false
  ): Unit = {
    val frame = opened(SourceIndex.methodFrame(node, name.value, groups, owner))
    // Each clause is spelled on its own, so curried clauses read `(x: A)(y: A)` however they were
    // laid out; `<inferred>` is no Scala the tokenizer can read back.
    val clauses = groups.flatMap(group => group.tparamClause :: group.paramClauses)
    val signature =
      Spelling.declared(name, beforeColon = groups.isEmpty) + clauses.map(Spelling.of).mkString +
        result.fold(": <inferred>")(t => s": ${Spelling.of(t)}")
    targets += Target(
      input,
      node,
      frame.path.mkString("."),
      signature,
      frame,
      result,
      constructor = false,
      declaration = declaration
    )
    // Local methods capture their enclosing parameters and preceding local values. We do not
    // count the analysed method's own implementation locals as inputs to its signature.
    body.foreach {
      case block: Term.Block =>
        index(
          input,
          frame.copy(
            id = frame.id + ":body",
            parent = Some(frame),
            stats = block.stats,
            params = Nil,
            typeParams = Nil,
            tree = Some(block)
          )
        )
      case _ => ()
    }
  }

}

object SourceIndex {

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
    Frame(
      s"$input:${node.pos.start}",
      owner.path :+ name,
      Some(owner),
      stats,
      tparams,
      params ++ ContextBounds.evidence(tparams),
      local,
      parents,
      Some(node)
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

  /** A named template's frame: its body, its binders and constructor parameters, its parents. */
  private[cardinality] def templateFrame(
      owner: Frame,
      d: Defn.Class | Defn.Trait | Defn.Enum | Defn.Object
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
    case c: (Defn.Class | Defn.Trait | Defn.Enum) =>
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
  private[cardinality] def givenFrame(owner: Frame, d: Defn.Given): Frame = {
    val name = if (d.name.value.isEmpty) s"<given@${d.pos.startLine + 1}>" else d.name.value
    clausesFrame(d, name, owner, d.templ.body.stats, d.paramClauseGroups, local = false)
      .copy(parents = d.templ.inits)
  }

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

  /** What one importer binds: `given` selectors bring contextual values and no name (`import
    * p.{given T}` selects by type), so only named, renamed and wildcard selectors appear.
    */
  def binding(importer: Importer, path: List[String]): ImportBinding = {
    val named = importer.importees.collect {
      case e: Importee.Name   => e.name.value -> (path :+ e.name.value)
      case e: Importee.Rename => e.rename.value -> (path :+ e.name.value)
    }
    val wildcards = importer.importees.collect { case _: Importee.Wildcard => path }
    ImportBinding(importer.pos.start, named, wildcards, hiddenNames(importer))
  }

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
