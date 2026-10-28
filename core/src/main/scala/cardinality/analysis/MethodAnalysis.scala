package cardinality.analysis

import scala.meta.*
import scala.util.control.NonFatal

import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.*
import cardinality.analysis.resolution.*
import cardinality.capacity.{Counter, Library}
import cardinality.types.*

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
  import Inhabitation.{Count, Shape}

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

  object Entry:

    /** One report row for a target: the signature is flattened to a single line, and scalameta's
      * zero-based line becomes the line an editor shows.
      */
    def of(
        target: Target,
        kind: String,
        count: Count,
        captures: List[String] = Nil
    ): Entry =
      Entry(
        target.input,
        target.name,
        target.signature.replaceAll("\\s+", " "),
        target.tree.pos.startLine + 1,
        kind,
        count,
        captures
      )

  final case class Limits(maxTypeDepth: Int = 64, maxStates: Int = 256)

  def analyze(inputs: List[Input], limits: Limits = Limits()): List[Entry] =
    new Index(inputs, limits).analyze()

  // The names whose count depends on who can produce them: the shape model spells `Boolean` as a
  // case split, so the values behind it could come from anywhere — a producer outside the lexical
  // chain included. The fixed-width numeric family is not one of these: the estimate reads a byte
  // as 2^8 and an `Int` as 2^32 wherever it needs their size, so nothing outside the chain can move
  // the number. `Unit`, `Option` and `Either` are structural too — a count of cases — and stay
  // countable wherever they appear.
  private val ConcreteAtoms = Set("Boolean")

  private[cardinality] type Resolved = Either[String, Shape]

  // A source type's parameters and the cases its representation is built from: one case of one
  // type for an alias, one case of its fields for a case class, one per case for an enum. A
  // declaration the model cannot read carries the reason instead.
  private type Representation = (List[Type.Param], Either[String, List[List[Type]]])

  // Traverse a list of resolutions: the first failure wins, and the successful ones keep their
  // order. (Used both for a case's fields and for the cases of an enum.) It is a free function
  // rather than an index method because the measurement needs it too.
  private[cardinality] def sequence[A](values: List[Either[String, A]]): Either[String, List[A]] =
    Inhabitation.Shape.sequenceEithers(values)

  final private[cardinality] case class Frame(
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

    // The source a frame was indexed from: the file path is the root frame's identity and the
    // head of every nested `input:offset` id, so positions compared against it are positions
    // in the same file.
    def input: String = id.takeWhile(_ != ':')
  }

  final private[cardinality] case class TypeEntry(
      name: String,
      tree: Tree,
      owner: Frame,
      self: Option[Frame] = None
  )

  final private[cardinality] case class Target(
      input: String,
      tree: Tree,
      name: String,
      signature: String,
      frame: Frame,
      result: Option[Type],
      constructor: Boolean,
      declaration: Boolean = false
  )

  final private[cardinality] class Index(
      inputs: List[Input],
      val limits: Limits,
      work: Option[AnalysisWork] = None
  ) extends Resolver {
    private val existentialCaptures = new ExistentialInputs.Captures
    private val sourceNames = inputs.flatMap(_.tree.collect { case name: Name => name.value }).toSet

    private val indexer = new SourceIndex(inputs, work)

    def analyze(): List[Entry] = indexer.targets.toList.map(measure)

    // One reading per source: the estimate's scope, solved with every supplied file behind it, so
    // a constructor's parameter answers `Int` with 2^32 and `Nat` with its sealed sum — the same
    // numbers the definitions' rows carry.
    private lazy val readings: Map[String, Counter.Reading] = {
      val library = Library.of(inputs.map(_.tree))
      inputs.map(input => input.path -> Counter.Reading.of(input.tree, library)).toMap
    }

    def reading(path: String): Counter.Reading = readings.getOrElse(path, Counter.Reading.bare)

    private val lookupIndex = new SourceLookup(
      indexer.types.toList,
      indexer.packageFrames.toList,
      indexer.imports.toList.groupMap(_._1)(_._2)
    )

    def selected(paths: Set[String], names: Set[String]): List[Target] =
      indexer.targets.toList
        .filter(t => paths(t.input) && (names.isEmpty || names(t.name)))
        .sortBy(t => (t.input, t.name, t.signature, t.tree.pos.start))

    def inspect(operation: String): Unit = work.foreach(_.step(operation))

    def expanding[A](operation: String)(read: => A): A =
      work.fold(read)(_.expanding(operation)(read))

    def solverDepth: Int = work.fold(Int.MaxValue)(_ => limits.maxTypeDepth)
    def peers(path: List[String]): List[Frame] = lookupIndex.peers(path)
    def isPackage(frame: Frame): Boolean = lookupIndex.isPackage(frame)

    // The package and module lists the measurement reads as frames an outside member could live
    // in, and the limits its count runs under.
    def packages: List[Frame] = indexer.packageFrames.toList

    def modules: List[Frame] = indexer.moduleFrames.toList

    // Measuring one signature must not lose the report: scalameta's printers can throw on syntax
    // they cannot spell, and a signature the analysis cannot read is a diagnostic, not a crash.
    def measure(target: Target): Entry =
      try measurement(target)
      catch
        case error: AnalysisWork.Exhausted =>
          Entry.of(target, kindOf(target), Count.Unresolved(List(error.getMessage)))
        case NonFatal(error) =>
          Entry.of(
            target,
            kindOf(target),
            Count.Unresolved(List(s"analysis failed: ${error.getClass.getSimpleName}"))
          )

    def typeParameters(frame: Frame): Map[String, Resolved] =
      SourceBindings.of(frame, inspect)

    def resolve(
        tpe: Type,
        frame: Frame,
        bindings: Map[String, Resolved],
        seen: Set[String]
    ): Resolved = resolve(tpe, ResolutionContext(frame, bindings, seen))

    def resolve(
        tpe: Type,
        context: ResolutionContext
    ): Resolved =
      // The reference's own position decides which import clauses have bound their names:
      // a clause binds from its position onward, so a read before it sees only the scope the
      // compiler would give that position. A transparent alias's body, reconstructed under
      // substitution, carries no position: inside the caller's own file the caller's position
      // decides; a reconstructed tree read under another file's scope binds no import.
      expanding("resolve")(readType(tpe, context.copy(at = referenceAt(tpe, context))))

    private def referenceAt(tpe: Type, context: ResolutionContext): Option[(String, Int)] =
      val file = context.frame.input
      if (tpe.pos.start >= 0) Some(file -> tpe.pos.start)
      else
        context.at match
          // Reconstructed inside the caller's own file: the caller's reference decides.
          case Some((callerFile, _)) if callerFile == file => context.at
          // Reconstructed under another file's scope — a transparent alias's body: the scope's
          // own declaration position is the lower bound, so clauses written before it bind.
          case _ => frameAnchor(context.frame, file)

    // Root frames carry the path alone; a nested frame's id is `input:offset[...]`, the
    // declaration point the scope was opened at.
    private def frameAnchor(frame: Frame, file: String): Option[(String, Int)] =
      frame.id.drop(file.length + 1).takeWhile(_.isDigit) match
        case digits if digits.nonEmpty => Some(file -> digits.toInt)
        case _                         => None

    private def readType(tpe: Type, context: ResolutionContext): Resolved = {
      val ResolutionContext(frame, variables, visiting, _, _) = context
      if (visiting.size >= limits.maxTypeDepth) Left("type resolution budget exhausted")
      else
        tpe match {
          case SingletonIntersections(syntax) =>
            SingletonIntersections.resolve(syntax, context, this)
          case n: Type.Name if variables.contains(n.value)     => variables(n.value)
          case union: Type.ApplyInfix if union.op.value == "|" =>
            resolveUnion(union, context)
          case Type.Or(_, _) => resolveUnion(tpe, context)
          case MethodProjections(projected) if MethodProjections.accepts(projected, frame, this) =>
            MethodProjections.resolve(projected, context, this, sourceNames)
          case Type.Tuple(args) =>
            sequence(args.map(context.read(_, this))).map(Shape.Product(_))
          case Type.Repeated(element) =>
            context.read(element, this).map(Shape.Repeated(_))
          case f: Type.Function =>
            for {
              args <- sequence(f.paramClause.values.map(context.read(_, this)))
              result <- context.read(f.res, this)
            } yield Shape.Function(args, result)
          case f: Type.ContextFunction =>
            Left(s"context function requires evidence analysis: ${f.syntax}")
          case wildcard: Type.Wildcard => Left(TypePlaceholders.existential(wildcard))
          case _: Type.AnonymousParam  =>
            Left("constructor placeholder has no enclosing constructor application")
          case _: Type.AnonymousLambda =>
            Left("unapplied constructor placeholder requires a higher-kinded argument environment")
          case f: Type.PolyFunction => MethodTypeLambdas.resolve(f, context, this)
          case _: Type.Lambda       =>
            Left("unapplied type lambda requires higher-kinded analysis")
          case t: Type.Apply =>
            resolveApplication(t.tpe, t.argClause.values, context)
          case t: Type.ApplyInfix =>
            resolveApplication(t.op, List(t.lhs, t.rhs), context)
          case n: Type.Name    => namedType(n.value, Nil, context)
          case p: Type.Project =>
            RefinedMembers
              .resolve(p.qual, p.name.value, context, this)
              .flatMap(SingletonIntersections.validate(_, context.useSite, this))
          case r: Type.Refine =>
            Left(s"refined representation unavailable (constraints not erased): ${r.syntax}")
          case s: Type.Select if RefinedMembers.pathType(s.qual, frame).nonEmpty =>
            RefinedMembers.pathType(s.qual, frame).get.flatMap { (declared, owner) =>
              val declaredContext = context.inScope(owner, typeParameters(owner))
              RefinedMembers
                .resolve(declared, s.name.value, declaredContext, this)
                .flatMap(SingletonIntersections.validate(_, context.useSite, this))
            }
          case s: Type.Select => namedType(s.syntax.stripPrefix("_root_."), Nil, context)
          case _              => Left(s"unsupported type: ${tpe.syntax.replaceAll("\\s+", " ")}")
        }
    }

    // Scala unions carry no constructor tag. Only idempotence and empty alternatives are
    // justified here; equal structural shapes alone do not prove equal nominal types.
    // Null is empty in the null-free implementation model, not an extra Option-like case.
    private def resolveUnion(
        union: Type,
        context: ResolutionContext
    ): Resolved = {
      val unique = TypeSubstitution.unionAlternatives(union).distinctBy(_.syntax)
      sequence(unique.map(context.read(_, this))).left
        .map(reason => s"union ${union.syntax}: $reason")
        .flatMap { shapes =>
          val inhabited = shapes.filterNot(_ == Shape.Sum(Nil))
          inhabited match {
            case Nil          => Right(Shape.Sum(Nil))
            case List(single) => Right(single)
            // Aliases of the same free binder also denote the very same type. Do not extend
            // this equality test to products or sums: unrelated nominal types can share them.
            case (atom: Shape.Atom) :: rest if rest.forall(_ == atom) => Right(atom)
            case _                                                    =>
              Left(
                s"union alternatives require an overlap and discrimination proof: ${union.syntax}"
              )
          }
        }
    }

    // Infix types are binary constructor applications, not term-method calls: even a colon-ending
    // operator keeps lhs before rhs. Associativity is already encoded in the parser's nested AST.
    // Both spellings share argument resolution, qualified lookup and representation/arity checks.
    private def resolveApplication(
        callee: Type,
        arguments: List[Type],
        context: ResolutionContext
    ): Resolved = {
      val name = TypeApplications.name(callee)
      if (projectable(name, arguments, context))
        MethodProjections.resolve(
          Type.Apply(callee, Type.ArgClause(arguments)),
          context,
          this,
          sourceNames
        )
      else
        existentialCaptures
          .arguments(callee, arguments, context.frame, context.read(_, this))
          .flatMap { (witnesses, args) =>
            val resolved = TypeApplications.lambda(callee) match {
              case Some(lambda) => lambda.flatMap(MethodTypeLambdas(_, args, context, this))
              case None if context.variables.contains(name) =>
                context
                  .variables(name)
                  .flatMap(_ => Left(s"type parameter is not an applicable constructor: $name"))
              case None => namedType(name, args, context)
            }
            resolved
              .map(shape => if (witnesses.isEmpty) shape else Shape.Existential(witnesses, shape))
          }
    }

    private def projectable(
        name: String,
        arguments: List[Type],
        context: ResolutionContext
    ): Boolean =
      if (arguments.exists(_.is[Type.Wildcard])) false
      else if (context.variables.contains(name)) false
      else MethodProjections.alias(name, context.frame, this).nonEmpty

    def lookup(name: String, frame: Frame): List[TypeEntry] = {
      inspect("lookup")
      lookupIndex.lookup(name, frame, accessible, inspect)
    }

    def accessible(tree: Tree, owner: Frame, from: Frame): Boolean = {
      inspect("accessibility")
      tree match {
        case d: Stat.WithMods if d.mods.exists(m => m.is[Mod.Private] || m.is[Mod.Protected]) =>
          // A `private[p]` naming an enclosing package publishes the declaration to every
          // scope inside `p`, subpackages included — a sibling file of that package reaches it
          // as its own file does. Any other qualifier (a class, `this`, `super`), a
          // `protected[p]` whose subclass condition this model does not track, and an
          // unqualified top-level `private`, which in Scala 3 means the enclosing file, keep
          // the enclosing-declaration rule: a reference they cannot reach stays a diagnostic
          // instead of becoming invented scope.
          packageGrant(d.mods, owner.path)
            .exists(prefix => from.chain.exists(_.path.startsWith(prefix))) ||
          from.chain.exists(_.id == owner.id)
        case _ => true
      }
    }

    // The package a `private[p]` qualifier names, as a path prefix: the innermost segment of
    // the declaration's lexical chain that the qualifier spells and that some supplied package
    // really is — a same-spelled class is not the grant. With no package at all behind the
    // qualifier (an anonymous unqualified `private`, a `this`, a class-scoped name), there is
    // no grant and the enclosing-declaration rule alone decides.
    private def packageGrant(mods: List[Mod], path: List[String]): Option[List[String]] =
      mods.collectFirst { case m: Mod.Private => m.within } match {
        case Some(name: Name) =>
          path.lastIndexWhere(_ == name.value) match {
            case -1  => None
            case idx => Option.when(packagePrefixes(path.take(idx + 1)))(path.take(idx + 1))
          }
        case _ => None
      }

    // Every prefix of a package path among the indexed sources — `p`, `p.q`, `p.q.r` all count
    // as packages once some file is supplied in `p.q.r`, the way Scala sees them.
    private lazy val packagePrefixes: Set[List[String]] =
      indexer.packageFrames.flatMap(_.path.inits).toSet

    private def namedType(
        name: String,
        args: List[Shape],
        context: ResolutionContext
    ): Resolved =
      lookupIndex.resolveName(name, context.frame, context.at, accessible, inspect) match {
        case Found.Absent          => builtin(name, args)
        case Found.Blocked(reason) => Left(reason)
        case Found.Entry(entry)    =>
          sourceType(name, args, entry, context)
            .flatMap(SingletonIntersections.validate(_, context.useSite, this))
      }

    // The primitives and structural constructors the shape model spells itself, by the arity of
    // their application. `Unit` is one construction, `Nothing` none and `Boolean` one bit;
    // `Option` and `Either` are a count of cases, and stay countable wherever they appear.
    private def builtin(name: String, args: List[Shape]): Resolved =
      (name.stripPrefix("scala."), args) match {
        case ("Unit", Nil)             => Right(Shape.Product(Nil))
        case ("Nothing" | "Null", Nil) => Right(Shape.Sum(Nil))
        case ("Boolean", Nil)          =>
          Right(Shape.Sum(List(Shape.Product(Nil), Shape.Product(Nil))))
        case ("Option", List(a))                    => Right(Shape.Sum(List(Shape.Product(Nil), a)))
        case ("Either" | "util.Either", List(l, r)) => Right(Shape.Sum(List(l, r)))
        case ("=:=" | "<:<", List(l, r))            => EvidenceAnalysis.resolve(name, l, r)
        case ("Function1", List(l, r))              => Right(Shape.Function(List(l), r))
        case _                                      => Left(s"unresolved type: $name")
      }

    // A source type resolves through its own declaration: an alias body, a case class's fields, or
    // a representation the model cannot read. Its identity guards against a resolution cycle.
    private def sourceType(
        name: String,
        args: List[Shape],
        entry: TypeEntry,
        context: ResolutionContext
    ): Resolved = {
      val key = (entry.owner.path :+ entry.name).mkString(".")
      if (context.visiting(key)) Left(s"recursive type requires a structural proof: $key")
      else if (OpaqueTypes.hidden(entry, context.useSite))
        Left(s"opaque representation is not visible at use site: $key")
      else
        entry.tree match {
          case alias: Defn.Type if TypeApplications.lambda(alias.body).nonEmpty =>
            if (alias.tparamClause.values.isEmpty)
              TypeApplications
                .lambda(alias.body)
                .get
                .flatMap(
                  MethodTypeLambdas(
                    _,
                    args,
                    context.inScope(entry.owner, typeParameters(entry.owner)).enter(key),
                    this
                  )
                )
            else
              Left(
                s"parameterized type-lambda alias requires higher-kinded analysis: $name"
              )
          case _ =>
            val (parameters, bodies) = representation(entry, key)
            bodies.flatMap(cases =>
              parameterized(Applied(name, args, parameters, cases, entry, context, key))
            )
        }
    }

    private def representation(entry: TypeEntry, key: String): Representation =
      entry.tree match {
        case a: Defn.Type =>
          (a.tparamClause.values, Right(List(List(a.body))))
        case c: Defn.Class => caseClassRepresentation(c, key)
        case e: Defn.Enum  => enumRepresentation(e, key)
        case _             => abstractRepresentation(key)
      }

    // An enum is the sum of its cases; a repeated case is one constructor per name, and a case
    // whose field type is not declared leaves the representation unreadable.
    private def enumRepresentation(e: Defn.Enum, key: String): Representation =
      if (e.mods.exists(_.is[Mod.Abstract])) abstractRepresentation(key)
      else (e.tparamClause.values, enumCases(e, key))

    private def abstractRepresentation(key: String): Representation =
      (Nil, Left(s"abstract type or method-valued representation: $key"))

    // Only a case class whose single parameter list *is* the product can be projected: no second
    // list, no constructor modifiers, no parents, no members and no hidden parameter.
    private def caseClassRepresentation(c: Defn.Class, key: String): Representation =
      if (!isCaseClass(c) || !isPlainProduct(c)) abstractRepresentation(key)
      else
        (
          c.tparamClause.values,
          declaredFields(c.ctor.paramClauses.toList.flatMap(_.values), key).map(List(_))
        )

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
    ): Either[String, List[Type]] =
      sequence(fields.map(_.decltpe.toRight(s"missing field type: $key")))

    // One application of a source type: what it is applied to, and what its declaration says it
    // is built from.
    private case class Applied(
        name: String,
        args: List[Shape],
        parameters: List[Type.Param],
        cases: List[List[Type]],
        entry: TypeEntry,
        context: ResolutionContext,
        key: String
    )

    private def parameterized(applied: Applied): Resolved = {
      import applied.{args, cases, context, entry, key, name, parameters}
      TypeApplications.checkApplication(name, parameters, args.size).flatMap { _ =>
        val replacements = parameters.zip(args).map((p, a) => p.name.value -> Right(a))
        val env = typeParameters(entry.owner) ++ replacements
        val scoped = context.inScope(entry.owner, env).enter(key)
        sequence(
          cases.map(fields => sequence(fields.map(scoped.read(_, this))))
        )
          .map(shapeOf(entry, _))
      }
    }

    // A case class denotes the product of its fields; an alias the single type it names.
    // A case class and an alias are one case; an enum is as many cases as it declares.
    private def shapeOf(entry: TypeEntry, cases: List[List[Shape]]): Shape =
      entry.tree match {
        case _: Defn.Enum => Shape.Sum(cases.map(Shape.Product(_)))
        case _: Defn.Type => cases.head.head
        case _            => Shape.Product(cases.head)
      }

    private def enumCases(e: Defn.Enum, key: String): Either[String, List[List[Type]]] =
      sequence(
        e.templ.body.stats.flatMap {
          case c: Defn.EnumCase =>
            List(declaredFields(c.ctor.paramClauses.toList.flatMap(_.values), key))
          case r: Defn.RepeatedEnumCase => List.fill(r.cases.size)(Right(Nil))
          case _                        => Nil
        }
      )

    // The concrete atoms a signature or a tree mentions, written either bare or qualified: a
    // producer of one of these could change the count of a signature that mentions it.
    private def mentionedNames(tree: Tree): Set[String] =
      tree.syntax.split("[^\\p{L}\\p{N}_$]+").iterator.filter(ConcreteAtoms.contains).toSet

    def mentionedConcrete(target: Target): Set[String] =
      (target.frame.params.flatMap(_.decltpe) ++ target.result).flatMap(mentionedNames).toSet

    def kindOf(target: Target): String =
      if (target.constructor) "constructor" else if (target.declaration) "declaration" else "method"

    private def measurement(target: Target): Entry = new Measurement(target, this).entry()

  }

}
