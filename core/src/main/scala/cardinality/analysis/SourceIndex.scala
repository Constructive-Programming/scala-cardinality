package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, Input, Target, TypeEntry}

final private[cardinality] class SourceIndex(inputs: List[Input], work: Option[AnalysisWork]) {
  private given scala3: Dialect = dialects.Scala3

  private[cardinality] val types = mutable.ListBuffer.empty[TypeEntry]
  private[cardinality] val targets = mutable.ListBuffer.empty[Target]
  private[cardinality] val packageFrames = mutable.ListBuffer.empty[Frame]
  private[cardinality] val moduleFrames = mutable.ListBuffer.empty[Frame]

  inputs.sortBy(_.path).foreach { input =>
    val root = Frame(input.path, Nil, None, input.tree.stats)
    packageFrames += root
    index(input.path, root)
  }

  private def inspect(operation: String): Unit = work.foreach(_.step(operation))

  private def child(
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
      params,
      local,
      parents,
      Some(node)
    )

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
      case d: Defn.Type   => named(d.name.value, d, frame)
      case d: Decl.Type   => named(d.name.value, d, frame)
      case d: Defn.Class  => indexClass(input, frame, d)
      case d: Defn.Trait  => indexTemplate(input, frame, templateOf(d))
      case d: Defn.Object => indexObject(input, frame, d)
      case d: Defn.Enum   => indexTemplate(input, frame, templateOf(d))
      // NOTE both kind methods above register the name: an enum's cases and a trait's members
      // are resolved through the declaration this puts in `types`.
      case d: Defn.Given          => indexGiven(input, frame, d)
      case d: Defn.ExtensionGroup => indexExtension(input, frame, d)
      case d: Defn.Def            =>
        method(input, d, d.name.value, d.paramClauseGroups, d.decltpe, Some(d.body), frame)
      case d: Decl.Def =>
        method(
          input,
          d,
          d.name.value,
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
    val nested = child(input, pkg, pkg.name.value, frame, pkg.templ.body.stats, Nil, Nil)
    packageFrames += nested
    index(input, nested)
  }

  private def indexClass(input: String, frame: Frame, d: Defn.Class): Unit = {
    val nested = indexTemplate(input, frame, templateOf(d))
    if (!d.mods.exists(_.is[Mod.Abstract]))
      // A constructor's inputs are available when choosing its output fields. The instance and
      // its members do not exist yet, so they are not constructor captures.
      targets += Target(
        input,
        d,
        (nested.path :+ "<init>").mkString("."),
        d.name.syntax + d.tparamClause.syntax + d.ctor.paramClauses.map(_.syntax).mkString,
        nested.copy(stats = Nil, parents = Nil),
        None,
        constructor = true
      )
  }

  // A template is the body a definition opens, with its own binders and constructor inputs.
  private case class Template(
      node: Tree,
      name: String,
      stats: List[Stat],
      tparams: List[Type.Param] = Nil,
      params: List[Term.Param] = Nil,
      parents: List[Init] = Nil
  )

  private def indexTemplate(input: String, frame: Frame, template: Template): Frame = {
    val nested = child(
      input,
      template.node,
      template.name,
      frame,
      template.stats,
      template.tparams,
      template.params,
      parents = template.parents
    )
    // Named in the enclosing frame, so a sibling reference resolves as before; its own frame
    // travels with it, because a member of it — a declared field a subclass inherits — reads in
    // the scope that declares it.
    types += TypeEntry(template.name, template.node, frame, Some(nested))
    index(input, nested)
    nested
  }

  private def templateOf(d: Defn.Class | Defn.Trait | Defn.Enum): Template =
    d match {
      case c: Defn.Class =>
        Template(
          c,
          c.name.value,
          c.templ.body.stats,
          c.tparamClause.values,
          c.ctor.paramClauses.toList.flatMap(_.values),
          c.templ.inits
        )
      case t: Defn.Trait =>
        Template(
          t,
          t.name.value,
          t.templ.body.stats,
          t.tparamClause.values,
          t.ctor.paramClauses.toList.flatMap(_.values),
          t.templ.inits
        )
      case e: Defn.Enum =>
        Template(
          e,
          e.name.value,
          e.templ.body.stats,
          e.tparamClause.values,
          e.ctor.paramClauses.toList.flatMap(_.values),
          e.templ.inits
        )
    }

  private def indexObject(input: String, frame: Frame, d: Defn.Object): Unit = {
    val module =
      child(input, d, d.name.value, frame, d.templ.body.stats, Nil, Nil, parents = d.templ.inits)
    moduleFrames += module
    index(input, module)
  }

  private def indexGiven(input: String, frame: Frame, d: Defn.Given): Unit = {
    val name = if (d.name.value.isEmpty) s"<given@${d.pos.startLine + 1}>" else d.name.value
    index(
      input,
      child(
        input,
        d,
        name,
        frame,
        d.templ.body.stats,
        d.paramClauseGroups.flatMap(_.tparamClause.values),
        d.paramClauseGroups.flatMap(_.paramClauses).flatMap(_.values),
        parents = d.templ.inits
      )
    )
  }

  private def indexExtension(input: String, frame: Frame, d: Defn.ExtensionGroup): Unit = {
    val groups = d.paramClauseGroup.toList
    val stats = d.body match {
      case b: Term.Block => b.stats
      case s             => List(s)
    }
    index(
      input,
      child(
        input,
        d,
        s"<extension@${d.pos.startLine + 1}>",
        frame,
        stats,
        groups.flatMap(_.tparamClause.values),
        groups.flatMap(_.paramClauses).flatMap(_.values),
        local = true
      )
    )
  }

  private def method(
      input: String,
      node: Tree,
      name: String,
      groups: List[Member.ParamClauseGroup],
      result: Option[Type],
      body: Option[Term],
      owner: Frame,
      declaration: Boolean = false
  ): Unit = {
    val params = groups.flatMap(_.paramClauses).flatMap(_.values)
    val tparams = groups.flatMap(_.tparamClause.values)
    val frame = child(input, node, name, owner, Nil, tparams, params, local = true)
    val signature =
      name + groups.map(_.syntax).mkString + result.fold(": <inferred>")(t => s": ${t.syntax}")
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
