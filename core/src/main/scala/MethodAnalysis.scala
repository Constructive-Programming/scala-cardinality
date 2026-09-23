package cardinality

import scala.collection.mutable
import scala.meta.*
import scala.util.control.NonFatal

/** Source-set analysis of pure, total parametric implementations, not stored-value cardinality.
  *
  * Pass one indexes declarations and lexical owners without measuring them. Pass two resolves
  * signatures and captures against that index. Unsupported syntax or an incomplete environment is a
  * diagnostic, never evidence of infinity. No source code is executed.
  */
object MethodAnalysis {

  // Trees are parsed as Scala 3; printing one with any other dialect makes scalameta reprint it
  // under Scala 2 rules, which cannot spell Scala 3's modifiers at all.
  private given scala3: Dialect = dialects.Scala3
  import Inhabitation.{Binding, Count, Shape}

  final case class Input(path: String, tree: Source)

  final case class Entry(
      path: String,
      name: String,
      signature: String,
      line: Int,
      kind: String,
      count: Count,
      captures: List[String]
  )

  final case class Limits(maxTypeDepth: Int = 64, maxStates: Int = 256)

  def analyze(inputs: List[Input], limits: Limits = Limits()): List[Entry] =
    new Index(inputs, limits).analyze()

  // The concrete atoms whose count depends on who can produce them. Primitives are opaque to the
  // shape model (a Boolean is one bit, not a case split), so a producer outside the lexical chain
  // could change the answer. `Unit`, `Option` and `Either` are structural — a count of cases —
  // and stay countable wherever they appear.
  private val ConcreteAtoms = Set(
    "Boolean",
    "Byte",
    "Short",
    "Char",
    "Int",
    "Long",
    "Float",
    "Double"
  )

  private type Resolved = Either[String, Shape]

  final private case class Frame(
      id: String,
      path: List[String],
      parent: Option[Frame],
      stats: List[Stat],
      typeParams: List[Type.Param] = Nil,
      params: List[Term.Param] = Nil,
      local: Boolean = false,
      parents: List[Init] = Nil,
      tree: Option[Tree] = None
  ) {
    def chain: List[Frame] = this :: parent.toList.flatMap(_.chain)
  }

  final private case class TypeEntry(name: String, tree: Stat, owner: Frame)

  final private case class Target(
      input: String,
      tree: Tree,
      name: String,
      signature: String,
      frame: Frame,
      result: Option[Type],
      constructor: Boolean,
      declaration: Boolean = false
  )

  final private class Index(inputs: List[Input], limits: Limits) {
    private val types = mutable.ListBuffer.empty[TypeEntry]
    private val targets = mutable.ListBuffer.empty[Target]
    private val packages = mutable.ListBuffer.empty[Frame]
    private val modules = mutable.ListBuffer.empty[Frame]

    inputs.sortBy(_.path).foreach { input =>
      val root = Frame(input.path, Nil, None, input.tree.stats)
      packages += root
      index(input.path, root)
    }

    def analyze(): List[Entry] = targets.toList.map(measure)

    // Measuring one signature must not lose the report: scalameta's printers can throw on syntax
    // they cannot spell, and a signature the analysis cannot read is a diagnostic, not a crash.
    private def measure(target: Target): Entry =
      try measurement(target)
      catch
        case NonFatal(error) =>
          Entry(
            target.input,
            target.name,
            target.signature.replaceAll("\\s+", " "),
            target.tree.pos.startLine + 1,
            kindOf(target),
            Count.Unresolved(List(s"analysis failed: ${error.getClass.getSimpleName}")),
            Nil
          )

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

    private def index(input: String, frame: Frame): Unit =
      frame.stats.foreach {
        case p: Pkg =>
          val nested = frame.copy(
            id = s"$input:${p.pos.start}:package",
            path = frame.path ++ p.ref.syntax.split('.'),
            parent = Some(frame),
            stats = p.body.stats
          )
          packages += nested
          index(input, nested)
        case p: Pkg.Object =>
          val nested = child(input, p, p.name.value, frame, p.templ.body.stats, Nil, Nil)
          packages += nested
          index(input, nested)
        case d: Defn.Type  => types += TypeEntry(d.name.value, d, frame)
        case d: Decl.Type  => types += TypeEntry(d.name.value, d, frame)
        case d: Defn.Class =>
          types += TypeEntry(d.name.value, d, frame)
          val nested = child(
            input,
            d,
            d.name.value,
            frame,
            d.templ.body.stats,
            d.tparamClause.values,
            d.ctor.paramClauses.toList.flatMap(_.values),
            parents = d.templ.inits
          )
          if (!d.mods.exists(_.is[Mod.Abstract])) {
            // A constructor's inputs are available when choosing its output fields. The instance
            // and its members do not exist yet, so they are not constructor captures.
            val construction = nested.copy(stats = Nil, parents = Nil)
            targets += Target(
              input,
              d,
              (nested.path :+ "<init>").mkString("."),
              d.name.syntax + d.tparamClause.syntax + d.ctor.paramClauses.map(_.syntax).mkString,
              construction,
              None,
              constructor = true
            )
          }
          index(input, nested)
        case d: Defn.Trait =>
          types += TypeEntry(d.name.value, d, frame)
          index(
            input,
            child(
              input,
              d,
              d.name.value,
              frame,
              d.templ.body.stats,
              d.tparamClause.values,
              d.ctor.paramClauses.toList.flatMap(_.values),
              parents = d.templ.inits
            )
          )
        case d: Defn.Object =>
          val module = child(
            input,
            d,
            d.name.value,
            frame,
            d.templ.body.stats,
            Nil,
            Nil,
            parents = d.templ.inits
          )
          modules += module
          index(input, module)
        case d: Defn.Enum =>
          types += TypeEntry(d.name.value, d, frame)
          index(
            input,
            child(
              input,
              d,
              d.name.value,
              frame,
              d.templ.body.stats,
              d.tparamClause.values,
              d.ctor.paramClauses.toList.flatMap(_.values),
              parents = d.templ.inits
            )
          )
        case d: Defn.Given =>
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
        case d: Defn.ExtensionGroup =>
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
        case d: Defn.Def =>
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

    private def typeParameters(frame: Frame): Map[String, Resolved] =
      frame.chain.reverse.foldLeft(Map.empty[String, Resolved]) { (env, f) =>
        env ++ f.typeParams.map { p =>
          val constrained = p.bounds.lo.nonEmpty || p.bounds.hi.nonEmpty ||
            p.bounds.context.nonEmpty || p.bounds.view.nonEmpty
          val resolved =
            if (constrained || p.tparamClause.values.nonEmpty)
              Left(s"bounded or higher-kinded parameter ${p.syntax}")
            else Right(Shape.Atom(s"${f.id}:${p.name.value}"))
          p.name.value -> resolved
        }
      }

    private def sequence(values: List[Resolved]): Either[String, List[Shape]] =
      values.foldRight(Right(Nil): Either[String, List[Shape]]) { (head, tail) =>
        for {
          h <- head
          t <- tail
        } yield h :: t
      }

    private def resolve(
        tpe: Type,
        frame: Frame,
        variables: Map[String, Resolved],
        visiting: Set[String] = Set.empty
    ): Resolved =
      if (visiting.size >= limits.maxTypeDepth) Left("type resolution budget exhausted")
      else
        tpe match {
          case n: Type.Name if variables.contains(n.value) => variables(n.value)
          case Type.Tuple(args)                            =>
            sequence(args.map(resolve(_, frame, variables, visiting))).map(Shape.Product(_))
          case f: Type.Function =>
            for {
              args <- sequence(f.paramClause.values.map(resolve(_, frame, variables, visiting)))
              result <- resolve(f.res, frame, variables, visiting)
            } yield Shape.Function(args, result)
          case f: Type.ContextFunction =>
            Left(s"context function requires evidence analysis: ${f.syntax}")
          case t: Type.Apply =>
            for {
              args <- sequence(t.argClause.values.map(resolve(_, frame, variables, visiting)))
              result <- namedType(t.tpe.syntax, args, frame, visiting)
            } yield result
          case n: Type.Name   => namedType(n.value, Nil, frame, visiting)
          case s: Type.Select => namedType(s.syntax.stripPrefix("_root_."), Nil, frame, visiting)
          case _              => Left(s"unsupported type: ${tpe.syntax.replaceAll("\\s+", " ")}")
        }

    private def lookup(name: String, frame: Frame): List[TypeEntry] = {
      val lexical = frame.chain.iterator
        .map { f =>
          types.toList.filter(t => t.owner.id == f.id && t.name == name)
        }
        .find(_.nonEmpty)
      lexical.getOrElse {
        val candidates = frame.chain.map(f => (f.path :+ name).mkString(".")) :+ name
        candidates.iterator
          .map { full =>
            types.toList.filter(t =>
              !t.owner.chain.exists(_.local) &&
                (t.owner.path :+ t.name).mkString(".") == full && accessible(t.tree, t.owner, frame)
            )
          }
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    private def accessible(tree: Tree, owner: Frame, from: Frame): Boolean = tree match {
      case d: Stat.WithMods if d.mods.exists(m => m.is[Mod.Private] || m.is[Mod.Protected]) =>
        from.chain.exists(_.id == owner.id)
      case _ => true
    }

    private def namedType(
        name: String,
        args: List[Shape],
        frame: Frame,
        visiting: Set[String]
    ): Resolved = {
      val entries = lookup(name, frame)
      if (entries.size > 1) Left(s"ambiguous type: $name")
      else
        entries.headOption match {
          case Some(entry) =>
            val key = (entry.owner.path :+ entry.name).mkString(".")
            if (visiting(key)) Left(s"recursive type requires a structural proof: $key")
            else {
              val (parameters, bodies) = entry.tree match {
                case a: Defn.Type if !a.mods.exists(_.is[Mod.Opaque]) =>
                  (a.tparamClause.values, Right(List(a.body)))
                case c: Defn.Class
                    if c.mods.exists(_.is[Mod.Case]) &&
                      !c.mods.exists(_.is[Mod.Abstract]) &&
                      c.ctor.paramClauses.size == 1 && c.ctor.mods.isEmpty &&
                      c.templ.inits.isEmpty && c.templ.body.stats.isEmpty &&
                      !c.ctor.paramClauses
                        .flatMap(_.values)
                        .exists(p =>
                          p.mods.exists(m =>
                            m.is[Mod.Private] || m.is[Mod.Protected] || m.is[Mod.VarParam]
                          )
                        ) =>
                  val fields = c.ctor.paramClauses.toList.flatMap(_.values)
                  val declared = fields.flatMap(_.decltpe)
                  val shape =
                    if (fields.size == declared.size) Right(declared)
                    else Left(s"missing field type: $key")
                  (c.tparamClause.values, shape)
                case _ => (Nil, Left(s"abstract type or method-valued representation: $key"))
              }
              bodies.flatMap { tpes =>
                if (
                  parameters.exists(p =>
                    p.bounds.lo.nonEmpty || p.bounds.hi.nonEmpty ||
                      p.bounds.context.nonEmpty || p.bounds.view.nonEmpty || p.tparamClause.values.nonEmpty
                  )
                )
                  Left(s"constrained type constructor: $name")
                else if (parameters.size != args.size) Left(s"type argument arity: $name")
                else {
                  val replacements = parameters.zip(args).map((p, a) => p.name.value -> Right(a))
                  val env = typeParameters(entry.owner) ++ replacements
                  val fields = sequence(tpes.map(resolve(_, entry.owner, env, visiting + key)))
                  entry.tree match {
                    case _: Defn.Class => fields.map(Shape.Product(_))
                    case _             => fields.map(_.head)
                  }
                }
              }
            }
          case None =>
            (name, args) match {
              case ("Unit" | "scala.Unit", Nil)       => Right(Shape.Product(Nil))
              case ("Nothing" | "scala.Nothing", Nil) => Right(Shape.Sum(Nil))
              case ("Boolean" | "scala.Boolean", Nil) =>
                Right(Shape.Sum(List(Shape.Product(Nil), Shape.Product(Nil))))
              case ("Option" | "scala.Option", List(a)) =>
                Right(Shape.Sum(List(Shape.Product(Nil), a)))
              case ("Either" | "scala.Either" | "scala.util.Either", List(a, b)) =>
                Right(Shape.Sum(List(a, b)))
              case _ => Left(s"unresolved type: $name")
            }
        }
    }

    // The concrete atoms a signature or a tree mentions, written either bare or qualified: a
    // producer of one of these could change the count of a signature that mentions it.
    private def mentionedNames(tree: Tree): Set[String] =
      tree.syntax.split("[^\\p{L}\\p{N}_$]+").iterator.filter(ConcreteAtoms.contains).toSet

    private def mentionedConcrete(target: Target): Set[String] =
      (target.frame.params.flatMap(_.decltpe) ++ target.result).flatMap(mentionedNames).toSet

    private def kindOf(target: Target): String =
      if (target.constructor) "constructor" else if (target.declaration) "declaration" else "method"

    private def measurement(target: Target): Entry = {
      val errors = mutable.ListBuffer.empty[String]
      val values = mutable.LinkedHashMap.empty[String, Binding]
      val qualifiedValues = mutable.LinkedHashMap.empty[String, Binding]
      val captures = mutable.ListBuffer.empty[String]
      val variables = typeParameters(target.frame)
      variables.values.collect { case Left(reason) => reason }.foreach(errors += _)
      val owners = target.frame.chain.reverse
      // Members of a module outside the lexical chain can never hold or produce this method's
      // type parameters — a module's scope is fixed, and no method's binders reach it. They can
      // only matter where a signature mentions a concrete, modelled type (a Boolean, an Option),
      // so those modules, imports and sibling bodies are the ones to report rather than every
      // companion object and every import in the file.
      val mentions = mentionedConcrete(target)

      def bind(name: String, binding: Binding, owner: Frame): Unit = {
        values.update(name, binding)
        if (owner.id != target.frame.id) {
          captures += name
          // Shadowing the short name does not remove this.x / Outer.this.x / package.x.
          if (!owner.local) qualifiedValues.update(s"${owner.id}:$name", binding)
        }
      }

      def add(name: String, tpe: Option[Type], owner: Frame): Unit =
        tpe
          .toRight(s"missing type of accessible value: $name")
          .flatMap(resolve(_, owner, typeParameters(owner))) match {
          case Left(reason) => errors += reason
          case Right(shape) => bind(name, Binding(s"${owner.id}:$name", shape), owner)
        }

      def isOwner(tree: Tree): Boolean =
        (tree eq target.tree) || owners.flatMap(_.tree).exists(_ eq tree)

      def aliases(stats: List[Stat], owner: Frame): Unit = {
        val declared = stats
          .collect { case v: Defn.Val => v }
          .flatMap { v =>
            v.pats.collect { case Pat.Var(n) => n.value -> v }
          }
          .toMap
        val done = mutable.Map.empty[String, Either[String, Binding]]
        def value(name: String, active: Set[String]): Either[String, Binding] =
          if (active(name)) Left(s"recursive capture alias: $name")
          else
            done.getOrElseUpdate(
              name, {
                val v = declared(name)
                val source = v.rhs match {
                  case Term.Name(n) if declared.contains(n) => value(n, active + name)
                  case Term.Name(n)                         =>
                    values.get(n).toRight(s"capture alias not resolved: $name -> $n")
                  case _ => Left(s"accessible value body not normalized: $name")
                }
                source.flatMap { binding =>
                  v.decltpe.fold(Right(binding): Either[String, Binding]) { tpe =>
                    resolve(tpe, owner, typeParameters(owner)).flatMap { shape =>
                      if (binding.shape == shape) Right(binding)
                      else Left(s"capture alias type conversion not resolved: $name")
                    }
                  }
                }
              }
            )
        declared.keys.toList.sorted.foreach { name =>
          value(name, Set.empty) match {
            case Right(binding) => bind(name, binding, owner)
            case Left(reason)   => errors += reason
          }
        }
      }

      owners.foreach { owner =>
        owner.params.foreach(p => add(p.name.value, p.decltpe, owner))
        // Package-level declarations are indexed across all files. Unrelated class parameters
        // are never pooled just because both happen to be named A.
        val peers = packages.filter(_.path == owner.path).toList
        val frames = if (peers.exists(_.id == owner.id)) peers else List(owner)
        frames.foreach { f =>
          val visible = f.stats.filter(s =>
            (!f.local || s.pos.start < target.tree.pos.start) &&
              accessible(s, f, target.frame)
          )
          aliases(visible, f)
          visible.foreach {
            case v: Defn.Val if !v.pats.forall(_.is[Pat.Var]) =>
              errors += "destructured capture not resolved"
            case v: Decl.Val =>
              v.pats.foreach {
                case Pat.Var(name) => add(name.value, Some(v.decltpe), f)
                case _             => errors += "destructured capture not resolved"
              }
            case _: Defn.Var | _: Decl.Var => errors += "mutable capture"
            // A method that is only declared, in this scope or an inherited one, cannot invent a
            // value: a total parametric body builds its result from arguments and captures, and
            // both are already in the environment. An import of a type or a typeclass is the same
            // shape of thing. Only what can carry a *concrete* type this signature mentions is
            // worth a diagnostic, since that is where an outside producer could change the count.
            case d: Defn.Def if !isOwner(d) && mentions.nonEmpty =>
              errors += s"accessible method body not normalized: ${d.name.value}"
            case i: Import if mentions.nonEmpty =>
              errors += s"imported environment not resolved: ${i.syntax}"
            case _: Export                                                                 => ()
            case d: Defn.Given if owners.exists(_.id == s"${target.input}:${d.pos.start}") => ()
            case _: Defn.Given | _: Defn.GivenAlias | _: Decl.GivenLike                    =>
              errors += "given environment not resolved"
            case _ => ()
          }
        }
      }
      val reachable = modules.filterNot(m => owners.exists(_.id == m.id)).filter { module =>
        mentions.nonEmpty && module.stats.exists(s => accessible(s, module, target.frame))
      }
      if (reachable.nonEmpty) {
        val first = reachable.head.path.mkString(".")
        val rest = if (reachable.size == 1) "" else s" and ${reachable.size - 1} more"
        errors += s"qualified member environment not resolved: $first$rest"
      }
      // Parameters of the target hide outer names, but their type binders keep their identities.
      target.frame.params.foreach(p => add(p.name.value, p.decltpe, target.frame))
      val result = if (target.constructor) {
        sequence(
          target.frame.params.map(p =>
            p.decltpe
              .toRight(s"missing parameter type: ${p.name.value}")
              .flatMap(resolve(_, target.frame, variables))
          )
        ).map(Shape.Product(_))
      } else
        target.result
          .toRight("inferred result type not resolved")
          .flatMap(resolve(_, target.frame, variables))
      result.left.foreach(errors += _)
      val count =
        if (errors.nonEmpty) Count.Unresolved(errors.toList.distinct.sorted)
        else
          result.fold(
            reason => Count.Unresolved(List(reason)),
            shape =>
              Inhabitation
                .count((values.values ++ qualifiedValues.values).toList, shape, limits.maxStates)
          )
      Entry(
        target.input,
        target.name,
        target.signature.replaceAll("\\s+", " "),
        target.tree.pos.startLine + 1,
        kindOf(target),
        count,
        captures.toList.distinct.sorted
      )
    }

  }

}
