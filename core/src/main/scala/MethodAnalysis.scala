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

  // A source type's parameters and the types its representation is built from: an alias body, a
  // case class's fields, or a reason the model cannot read the declaration at all.
  private type Representation = (List[Type.Param], Either[String, List[Type]])

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

    // A parameter whose bounds or type constructor make its shape unreadable: a diagnostic wherever
    // it appears, never an atom.
    private def constrained(p: Type.Param): Boolean =
      p.bounds.lo.nonEmpty || p.bounds.hi.nonEmpty ||
        p.bounds.context.nonEmpty || p.bounds.view.nonEmpty || p.tparamClause.values.nonEmpty

    private def typeParameters(frame: Frame): Map[String, Resolved] =
      frame.chain.reverse.foldLeft(Map.empty[String, Resolved]) { (env, f) =>
        env ++ f.typeParams.map { p =>
          val resolved =
            if (constrained(p)) Left(s"bounded or higher-kinded parameter ${p.syntax}")
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
    ): Resolved =
      lookup(name, frame) match {
        case Nil         => builtin(name, args)
        case List(entry) => sourceType(name, args, entry, visiting)
        case _           => Left(s"ambiguous type: $name")
      }

    // The primitives and structural constructors the shape model spells itself, by the arity of
    // their application. `Unit` is one construction, `Nothing` none and `Boolean` one bit;
    // `Option` and `Either` are a count of cases, and stay countable wherever they appear.
    private def builtin(name: String, args: List[Shape]): Resolved = args match {
      case Nil        => nullaryBuiltin(name)
      case List(a)    => unaryBuiltin(name, a)
      case List(a, b) => binaryBuiltin(name, a, b)
      case _          => Left(s"unresolved type: $name")
    }

    private def nullaryBuiltin(name: String): Resolved = name match {
      case "Unit" | "scala.Unit"       => Right(Shape.Product(Nil))
      case "Nothing" | "scala.Nothing" => Right(Shape.Sum(Nil))
      case _                           => booleanBuiltin(name)
    }

    private def booleanBuiltin(name: String): Resolved = name match {
      case "Boolean" | "scala.Boolean" =>
        Right(Shape.Sum(List(Shape.Product(Nil), Shape.Product(Nil))))
      case _ => Left(s"unresolved type: $name")
    }

    private def unaryBuiltin(name: String, arg: Shape): Resolved = name match {
      case "Option" | "scala.Option" => Right(Shape.Sum(List(Shape.Product(Nil), arg)))
      case _                         => Left(s"unresolved type: $name")
    }

    private def binaryBuiltin(name: String, left: Shape, right: Shape): Resolved = name match {
      case "Either" | "scala.Either" | "scala.util.Either" => Right(Shape.Sum(List(left, right)))
      case _                                               => Left(s"unresolved type: $name")
    }

    // A source type resolves through its own declaration: an alias body, a case class's fields, or
    // a representation the model cannot read. Its identity guards against a resolution cycle.
    private def sourceType(
        name: String,
        args: List[Shape],
        entry: TypeEntry,
        visiting: Set[String]
    ): Resolved = {
      val key = (entry.owner.path :+ entry.name).mkString(".")
      if (visiting(key)) Left(s"recursive type requires a structural proof: $key")
      else {
        val (parameters, bodies) = representation(entry, key)
        bodies.flatMap(tpes => parameterized(name, args, parameters, tpes, entry, visiting, key))
      }
    }

    private def representation(entry: TypeEntry, key: String): Representation =
      entry.tree match {
        case a: Defn.Type if !a.mods.exists(_.is[Mod.Opaque]) =>
          (a.tparamClause.values, Right(List(a.body)))
        case c: Defn.Class => caseClassRepresentation(c, key)
        case _             => abstractRepresentation(key)
      }

    private def abstractRepresentation(key: String): Representation =
      (Nil, Left(s"abstract type or method-valued representation: $key"))

    // Only a case class whose single parameter list *is* the product can be projected: no second
    // list, no constructor modifiers, no parents, no members and no hidden parameter.
    private def caseClassRepresentation(c: Defn.Class, key: String): Representation =
      if (!isCaseClass(c) || !isPlainProduct(c)) abstractRepresentation(key)
      else {
        val fields = c.ctor.paramClauses.toList.flatMap(_.values)
        (c.tparamClause.values, declaredFields(fields, key))
      }

    private def isCaseClass(c: Defn.Class): Boolean =
      c.mods.exists(_.is[Mod.Case]) && !c.mods.exists(_.is[Mod.Abstract])

    private def isPlainProduct(c: Defn.Class): Boolean =
      c.ctor.paramClauses.size == 1 &&
        c.ctor.mods.isEmpty &&
        c.templ.inits.isEmpty &&
        c.templ.body.stats.isEmpty &&
        !c.ctor.paramClauses.flatMap(_.values).exists(hiddenParameter)

    private def hiddenParameter(p: Term.Param): Boolean =
      p.mods.exists(m => m.is[Mod.Private] || m.is[Mod.Protected] || m.is[Mod.VarParam])

    // A field without a declared type leaves the product unreadable.
    private def declaredFields(
        fields: List[Term.Param],
        key: String
    ): Either[String, List[Type]] = {
      val declared = fields.flatMap(_.decltpe)
      if (fields.size == declared.size) Right(declared)
      else Left(s"missing field type: $key")
    }

    private def parameterized(
        name: String,
        args: List[Shape],
        parameters: List[Type.Param],
        tpes: List[Type],
        entry: TypeEntry,
        visiting: Set[String],
        key: String
    ): Resolved =
      if (parameters.exists(constrained)) Left(s"constrained type constructor: $name")
      else if (parameters.size != args.size) Left(s"type argument arity: $name")
      else {
        val replacements = parameters.zip(args).map((p, a) => p.name.value -> Right(a))
        val env = typeParameters(entry.owner) ++ replacements
        sequence(tpes.map(resolve(_, entry.owner, env, visiting + key))).map(shapeOf(entry, _))
      }

    // A case class denotes the product of its fields; an alias the single type it names.
    private def shapeOf(entry: TypeEntry, fields: List[Shape]): Shape =
      entry.tree match {
        case _: Defn.Class => Shape.Product(fields)
        case _             => fields.head
      }

    // The concrete atoms a signature or a tree mentions, written either bare or qualified: a
    // producer of one of these could change the count of a signature that mentions it.
    private def mentionedNames(tree: Tree): Set[String] =
      tree.syntax.split("[^\\p{L}\\p{N}_$]+").iterator.filter(ConcreteAtoms.contains).toSet

    private def mentionedConcrete(target: Target): Set[String] =
      (target.frame.params.flatMap(_.decltpe) ++ target.result).flatMap(mentionedNames).toSet

    private def kindOf(target: Target): String =
      if (target.constructor) "constructor" else if (target.declaration) "declaration" else "method"

    private def measurement(target: Target): Entry = new Measurement(target).entry()

    /** One signature's environment: the bindings visible from its frame, the diagnostics that make
      * that environment incomplete, and the count the two admit.
      */
    private class Measurement(target: Target) {
      private val errors = mutable.ListBuffer.empty[String]
      private val values = mutable.LinkedHashMap.empty[String, Binding]
      private val qualifiedValues = mutable.LinkedHashMap.empty[String, Binding]
      private val captures = mutable.ListBuffer.empty[String]
      private val variables = typeParameters(target.frame)
      private val owners = target.frame.chain.reverse
      // Members of a module outside the lexical chain can never hold or produce this method's
      // type parameters — a module's scope is fixed, and no method's binders reach it. They can
      // only matter where a signature mentions a concrete, modelled type (a Boolean, an Option),
      // so those modules, imports and sibling bodies are the ones to report rather than every
      // companion object and every import in the file.
      private val mentions = mentionedConcrete(target)

      def entry(): Entry = {
        variables.values.collect { case Left(reason) => reason }.foreach(errors += _)
        owners.foreach(scan)
        flagReachableModules()
        // Parameters of the target hide outer names, but their type binders keep their identities.
        target.frame.params.foreach(p => add(p.name.value, p.decltpe, target.frame))
        val result = resultShape()
        result.left.foreach(errors += _)
        Entry(
          target.input,
          target.name,
          target.signature.replaceAll("\\s+", " "),
          target.tree.pos.startLine + 1,
          kindOf(target),
          count(result),
          captures.toList.distinct.sorted
        )
      }

      // One owner's bindings: its own parameters, then every scope that shares its path.
      private def scan(owner: Frame): Unit = {
        owner.params.foreach(p => add(p.name.value, p.decltpe, owner))
        frames(owner).foreach(scanFrame)
      }

      // Package-level declarations are indexed across all files. Unrelated class parameters are
      // never pooled just because both happen to be named A.
      private def frames(owner: Frame): List[Frame] = {
        val peers = packages.filter(_.path == owner.path).toList
        if (peers.exists(_.id == owner.id)) peers else List(owner)
      }

      private def scanFrame(scope: Frame): Unit = {
        val visible = scope.stats.filter(visibleIn(scope, _))
        new Aliases(visible, scope).report()
        visible.foreach(flagStat(scope, _))
      }

      private def visibleIn(scope: Frame, stat: Stat): Boolean =
        (!scope.local || stat.pos.start < target.tree.pos.start) &&
          accessible(stat, scope, target.frame)

      private def bind(name: String, binding: Binding, owner: Frame): Unit = {
        values.update(name, binding)
        if (owner.id != target.frame.id) {
          captures += name
          // Shadowing the short name does not remove this.x / Outer.this.x / package.x.
          if (!owner.local) qualifiedValues.update(s"${owner.id}:$name", binding)
        }
      }

      private def add(name: String, tpe: Option[Type], owner: Frame): Unit =
        tpe
          .toRight(s"missing type of accessible value: $name")
          .flatMap(resolve(_, owner, typeParameters(owner))) match {
          case Left(reason) => errors += reason
          case Right(shape) => bind(name, Binding(s"${owner.id}:$name", shape), owner)
        }

      private def isOwner(tree: Tree): Boolean =
        (tree eq target.tree) || owners.flatMap(_.tree).exists(_ eq tree)

      // A scope's declarations by kind: a value binds, a variable or an unreadable given is a
      // diagnostic, and a body only matters where a concrete type is at stake.
      private def flagStat(scope: Frame, stat: Stat): Unit = stat match {
        case v: Defn.Val               => flagVal(v)
        case v: Decl.Val               => addDeclaredValue(scope, v)
        case _: Defn.Var | _: Decl.Var => errors += "mutable capture"
        case other                     => flagEnvironment(other)
      }

      private def flagVal(value: Defn.Val): Unit =
        if (!value.pats.forall(_.is[Pat.Var])) errors += "destructured capture not resolved"

      private def addDeclaredValue(scope: Frame, declaration: Decl.Val): Unit =
        declaration.pats.foreach {
          case Pat.Var(name) => add(name.value, Some(declaration.decltpe), scope)
          case _             => errors += "destructured capture not resolved"
        }

      // A method that is only declared, in this scope or an inherited one, cannot invent a value:
      // a total parametric body builds its result from arguments and captures, and both are
      // already in the environment. An import of a type or a typeclass is the same shape of thing.
      // Only what can carry a *concrete* type this signature mentions is worth a diagnostic, since
      // that is where an outside producer could change the count.
      private def flagEnvironment(stat: Stat): Unit = stat match {
        case d: Defn.Def   => flagMethodBody(d)
        case i: Import     => flagImport(i)
        case d: Defn.Given => flagGiven(d)
        case other         => flagGivenLike(other)
      }

      private def flagMethodBody(definition: Defn.Def): Unit =
        if (!isOwner(definition) && mentions.nonEmpty)
          errors += s"accessible method body not normalized: ${definition.name.value}"

      private def flagImport(imported: Import): Unit =
        if (mentions.nonEmpty) errors += s"imported environment not resolved: ${imported.syntax}"

      private def flagGiven(definition: Defn.Given): Unit =
        if (!owners.exists(_.id == s"${target.input}:${definition.pos.start}"))
          errors += "given environment not resolved"

      private def flagGivenLike(stat: Stat): Unit = stat match {
        case _: Defn.GivenAlias | _: Decl.GivenLike => errors += "given environment not resolved"
        case _                                      => ()
      }

      private def flagReachableModules(): Unit = {
        val reachable = modules.filterNot(inChain).filter(moduleVisible).toList
        if (reachable.nonEmpty) errors += qualifiedMemberMessage(reachable)
      }

      private def inChain(module: Frame): Boolean = owners.exists(_.id == module.id)

      private def moduleVisible(module: Frame): Boolean =
        mentions.nonEmpty && module.stats.exists(accessible(_, module, target.frame))

      private def qualifiedMemberMessage(reachable: List[Frame]): String = {
        val rest = if (reachable.size == 1) "" else s" and ${reachable.size - 1} more"
        s"qualified member environment not resolved: ${reachable.head.path.mkString(".")}$rest"
      }

      private def resultShape(): Resolved =
        if (target.constructor) constructorShape() else declaredShape()

      private def constructorShape(): Resolved =
        sequence(
          target.frame.params.map(p =>
            p.decltpe
              .toRight(s"missing parameter type: ${p.name.value}")
              .flatMap(resolve(_, target.frame, variables))
          )
        ).map(Shape.Product(_))

      private def declaredShape(): Resolved =
        target.result
          .toRight("inferred result type not resolved")
          .flatMap(resolve(_, target.frame, variables))

      private def count(result: Resolved): Count =
        if (errors.nonEmpty) Count.Unresolved(errors.toList.distinct.sorted)
        else result.fold(unresolved, inhabitation)

      private def unresolved(reason: String): Count = Count.Unresolved(List(reason))

      private def inhabitation(shape: Shape): Count =
        Inhabitation.count(
          (values.values ++ qualifiedValues.values).toList,
          shape,
          limits.maxStates
        )

      /** One scope's value declarations, each resolved to the binding it names.
        *
        * A forward alias chain folds onto the value at its end; a chain that loops, names something
        * invisible, or has a body the model cannot read fails closed, as a diagnostic.
        */
      private class Aliases(stats: List[Stat], owner: Frame) {

        private val declared = stats
          .collect { case v: Defn.Val => v }
          .flatMap(v => v.pats.collect { case Pat.Var(n) => n.value -> v })
          .toMap

        private val done = mutable.Map.empty[String, Either[String, Binding]]

        def report(): Unit = declared.keys.toList.sorted.foreach(reportOne)

        private def reportOne(name: String): Unit =
          value(name, Set.empty) match {
            case Right(binding) => bind(name, binding, owner)
            case Left(reason)   => errors += reason
          }

        private def value(name: String, active: Set[String]): Either[String, Binding] =
          if (active(name)) Left(s"recursive capture alias: $name")
          else done.getOrElseUpdate(name, source(name, active))

        private def source(name: String, active: Set[String]): Either[String, Binding] = {
          val declaration = declared(name)
          val named = declaration.rhs match {
            case Term.Name(n) if declared.contains(n) => value(n, active + name)
            case Term.Name(n)                         =>
              values.get(n).toRight(s"capture alias not resolved: $name -> $n")
            case _ => Left(s"accessible value body not normalized: $name")
          }
          named.flatMap(binding => checked(name, binding, declaration))
        }

        // A declared type must agree with the binding the alias names; a mismatch is a conversion
        // the model cannot follow.
        private def checked(
            name: String,
            binding: Binding,
            declaration: Defn.Val
        ): Either[String, Binding] =
          declaration.decltpe.fold(Right(binding): Either[String, Binding]) { tpe =>
            resolve(tpe, owner, typeParameters(owner)).flatMap { shape =>
              if (binding.shape == shape) Right(binding)
              else Left(s"capture alias type conversion not resolved: $name")
            }
          }

      }

    }

  }

}
