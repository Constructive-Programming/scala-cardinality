package cardinality.analysis

import scala.meta.*
import scala.util.control.NonFatal

import cardinality.analysis.capability.{CapabilityRecords, Insertions}
import cardinality.analysis.expansion.InlineBodies
import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.*
import cardinality.analysis.resolution.*
import cardinality.capacity.{Counter, Library, Reading}
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

    /** One report row for a target: the signature (spelled tree by tree where it is built) and
      * every reason are kept on one line, the way a row quotes them (`Spelling`), and scalameta's
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
        Spelling.line(target.signature),
        target.tree.pos.startLine + 1,
        kind,
        count match {
          case Count.Unresolved(reasons) => Count.Unresolved(reasons.map(Spelling.line))
          case solved                    => solved
        },
        captures
      )

  final case class Limits(maxTypeDepth: Int = 64, maxStates: Int = 256)

  def analyze(inputs: List[Input], limits: Limits = Limits()): List[Entry] =
    new Index(inputs, limits).analyze()

  private[cardinality] type Resolved = Either[String, Shape]

  // Traverse a list of resolutions: the first failure wins, and the successful ones keep their
  // order. (Used both for a case's fields and for the cases of an enum.) It is a free function
  // rather than an index method because the measurement needs it too.
  private[cardinality] def sequence[A](values: List[Either[String, A]]): Either[String, List[A]] =
    Traversals.sequence(values)

  final private[cardinality] case class Frame(
      id: String,
      path: List[String],
      parent: Option[Frame],
      stats: List[Stat],
      typeParams: List[Type.Param] = Nil,
      params: List[Term.Param] = Nil,
      local: Boolean = false,
      parents: List[Init] = Nil,
      tree: Option[Tree] = None,
      refusals: List[String] = Nil
  ) {
    def chain: List[Frame] = this :: parent.toList.flatMap(_.chain)

    /** The name the frame's definition or scope has in its owner's path (none for a file root). */
    def name: Option[String] = path.lastOption

    /** Whether the frame is a class, trait, object, enum or given body. */
    def isTemplate: Boolean = tree.exists(TypeApplications.template(_).nonEmpty)

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
  ) {

    /** The declaration's qualified path: its identity among the scopes a reading has entered. */
    def key: String = (owner.path :+ name).mkString(".")

  }

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
    private val sourceNames = inputs.flatMap(_.tree.collect { case name: Name => name.value }).toSet

    private val indexer = new SourceIndex(inputs, work)

    val operators = new TypeOperators(resolve(_, _), constructorKinds.of(_, _))
    val boundedBinders: Map[String, (Frame, Type.Param)] = indexer.boundedBinders.toMap
    private val representations = new Representations(indexer, accessible, packageGrant)
    private lazy val records = new CapabilityRecords(this, new OverrideIdentity(this))

    def analyze(): List[Entry] = indexer.targets.toList.map(measure)

    // One reading per source: the estimate's scope, solved with every supplied file behind it, so
    // a constructor's parameter answers `Int` with 2^32 and `Nat` with its sealed sum — the same
    // numbers the definitions' rows carry.
    private lazy val readings: Map[String, Reading] = {
      val library = Library.of(inputs.map(_.tree))
      inputs.map(input => input.path -> Reading.of(input.tree, library)).toMap
    }

    def reading(path: String): Reading = readings.getOrElse(path, Reading.bare)

    private val lookupIndex: SourceLookup = new SourceLookup(
      indexer.types.toList,
      indexer.packageFrames.toList,
      indexer.moduleFrames.toList,
      indexer.imports.toList.groupMap(_._1)(_._2),
      (prefix, scope, at) => shadowedBuiltins.listed(prefix, scope, at),
      indexer.exports.toList.groupMap(_._1)(_._2)
    )

    private val inheritedMembers = new InheritedTypeMembers(lookupIndex, this)

    private val members = new TypeMembers(lookupIndex, inheritedMembers, this)

    def typeMember(
        tpe: Type,
        context: ResolutionContext
    ): Option[Either[String, TypeMembers.Meaning]] = members.of(tpe, context)

    def stableInput(tpe: Type, context: ResolutionContext): Either[String, Type] =
      members.stableInput(tpe, context)

    def declaresMember(owner: Type, name: String, context: ResolutionContext): Boolean =
      members.declaresMember(owner, name, context)

    def openFamily(template: Frame): Boolean =
      (template.tree, template.parent) match {
        case (Some(tree), Some(owner)) =>
          representations.openSubtype(TypeEntry(template.path.last, tree, owner)).nonEmpty
        case _ => false
      }

    private lazy val constructorKinds = new ConstructorKinds(this)

    private lazy val shadowedBuiltins: ShadowedBuiltins = new ShadowedBuiltins(
      this,
      indexer.types.toList,
      inputs.map(_.tree),
      lookupIndex.clauses,
      lookupIndex.doubtedHeads(accessible, inspect, inheritedMembers)
    )

    private[cardinality] lazy val runtimeClasses = new RuntimeClasses(
      this,
      indexer.types.toList,
      inputs.map(input => input.path -> input.tree).toMap,
      shadowedBuiltins.mayBeSupplied
    )

    private lazy val unions = new UnionSums(this, runtimeClasses)

    def unreadChannel(context: ResolutionContext): Option[String] =
      shadowedBuiltins.unread(context)

    lazy val insertions: Insertions = Insertions.of(inputs.map(_.tree))

    def rootShadow(name: String, context: ResolutionContext): Option[String] =
      shadowedBuiltins.rootShadow(name, context)

    def builtin(written: String, context: ResolutionContext): Either[String, String] =
      shadowedBuiltins.denoted(written, context)

    private lazy val applications = new ConstructorApplications(
      this,
      new NominalKeys(this, shadowedBuiltins),
      shadowedBuiltins,
      namedType,
      sourceNames
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

    def namespace(path: List[String]): Boolean = lookupIndex.packageNamespace(path)

    lazy val inlineBodies: InlineBodies = new InlineBodies(this)

    def modules: List[Frame] = indexer.moduleFrames.toList

    lazy val memberScopes: List[Frame] =
      (indexer.packageFrames ++ indexer.moduleFrames ++ indexer.types.flatMap(_.self) ++
        indexer.givenFrames).toList.filterNot(_.chain.exists(_.local))

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
      SourceBindings.of(frame, inspect, operators)

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
      expanding("resolve")(readType(tpe, context.referencing(tpe)))

    // A simple name the template body inherits (or may inherit from an unread parent) is read as
    // that member before any other reading of the name, an alias projection's included.
    private def readType(tpe: Type, context: ResolutionContext): Resolved =
      if (context.visiting.size >= limits.maxTypeDepth) Left("type resolution budget exhausted")
      else
        typeMember(tpe, context) match {
          case Some(meaning) => meaning.flatMap(members.denoted(_, context))
          case None          =>
            tpe match {
              case n: Type.Name if !context.variables.contains(n.value) =>
                inheritedMembers
                  .find(n.value, context)
                  .fold(readShape(tpe, context))(inheritedType(n.value, Arguments.none, _, context))
              case _ => readShape(tpe, context)
            }
        }

    private def readShape(tpe: Type, context: ResolutionContext): Resolved = {
      val frame = context.frame
      val variables = context.variables
      tpe match {
        case SingletonIntersections(syntax) =>
          SingletonIntersections.resolve(syntax, context, this)
        case n: Type.Name if variables.contains(n.value) => proper(n.value, variables(n.value))
        case TypeSubstitution.Union(_, _)                => unions.resolve(tpe, context)
        case MethodProjections(projected) if MethodProjections.accepts(projected, context, this) =>
          MethodProjections.resolve(projected, context, this, sourceNames)
        // A named tuple is its fields' product; it is still no tuple to a match type.
        case NamedTuples(fields) =>
          sequence(fields.map((_, field) => context.read(field, this))).map(Shape.Product(_))
        case Type.Tuple(args) =>
          sequence(args.map(context.read(_, this))).map(Shape.Product(_))
        case lit: Lit               => LiteralTypes.shape(lit)
        case Type.Repeated(element) =>
          context.read(element, this).map(Shape.Repeated(_))
        // Call-by-name differs from call-by-value only in when and how often the argument is
        // evaluated, which the model's exclusions (effects, divergence) make unobservable.
        case Type.ByName(argument) => context.read(argument, this)
        // A context function is a function whose arguments implicit search may supply
        // (`ContextFunctions`); the binders it puts around a whole body are read with the scope.
        case f: Type.FunctionType =>
          for {
            args <- sequence(f.paramClause.values.map(context.read(_, this)))
            result <- context.read(f.res, this)
          } yield Shape.Function(args, result)
        case wildcard: Type.Wildcard => Left(TypePlaceholders.existential(wildcard))
        case _: Type.AnonymousParam  =>
          Left("constructor placeholder has no enclosing constructor application")
        case _: Type.AnonymousLambda =>
          Left("unapplied constructor placeholder used as a proper type")
        case f: Type.PolyFunction => MethodTypeLambdas.resolve(f, context, this)
        case _: Type.Lambda       =>
          Left("unapplied type lambda used as a proper type")
        case t: Type.Apply if unions.aliased(t, context) => unions.resolve(t, context)
        case t: Type.Apply                               =>
          applications.resolve(t.tpe, t.argClause.values, context)
        case t: Type.ApplyInfix =>
          applications.resolve(t.op, List(t.lhs, t.rhs), context)
        case n: Type.Name    => lexicalType(n.value, Arguments.none, context)
        case p: Type.Project =>
          RefinedMembers
            .resolve(p.qual, p.name.value, context, this, projection = true)
            .flatMap(SingletonIntersections.validate(_, context.useSite, this))
        case r: Type.Refine => members.refinement(r, context)
        case s: Type.Select if RefinedMembers.pathType(s.qual, frame).nonEmpty =>
          RefinedMembers.pathType(s.qual, frame).get.flatMap { (declared, owner) =>
            val declaredContext = context.inScope(owner, typeParameters(owner))
            RefinedMembers
              .resolve(declared, s.name.value, declaredContext, this)
              .flatMap(SingletonIntersections.validate(_, context.useSite, this))
          }
        case s: Type.Select => namedType(s.syntax, Arguments.none, context)
        case _              => Left(s"unsupported type: ${Spelling.of(tpe)}")
      }
    }

    // A higher-kinded name read where a value type is expected is a kind error, not a value.
    private def proper(name: String, bound: Resolved): Resolved = bound.flatMap {
      case _: Shape.Constructor => Left(s"higher-kinded parameter used as a proper type: $name")
      case shape                => Right(shape)
    }

    def inheritsName(tpe: Type, name: String, context: ResolutionContext): Boolean =
      inheritedMembers.find(name, context.referencing(tpe)).nonEmpty

    def parent(init: Init, from: Frame): Either[String, Option[InheritedTypeMembers.Member]] =
      inheritedMembers.parent(init, from)

    def unsuppliedParent(init: Init, from: Frame): Boolean = inheritedMembers.unsupplied(init, from)

    def fixedTemplate(frame: Frame): Boolean = FinalImplementations.fixed(frame, indexer)

    def baseClasses(frame: Frame): (Set[String], Boolean) = inheritedMembers.baseClasses(frame)

    def outsideParent(init: Init, from: Frame): Option[InheritedTypeMembers.Outside] =
      inheritedMembers.outside(init, from)

    def declaration(tpe: Type, name: String, context: ResolutionContext): Found = {
      val at = context.referencing(tpe)
      inheritedMembers.find(name, at) match {
        case Some(found) => found.fold(Found.Blocked(_), (entry, _) => Found.Entry(entry))
        case None        =>
          lookupIndex.resolveName(name, at.frame, at.at, accessible, inspect, inheritedMembers)
      }
    }

    def resolveName(name: String, context: ResolutionContext): Found =
      lookupIndex.resolveName(
        name,
        context.frame,
        context.at,
        accessible,
        inspect,
        inheritedMembers
      )

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

    // A name inside a template body that the template inherits from a supplied parent means that
    // member before any enclosing declaration or import (Scala's precedence: members of the
    // template, inherited ones included, sit in the body's own scope); its declaration is read
    // under the arguments the `extends` chain gives the parent.
    private def namedType(
        name: String,
        arguments: Arguments,
        context: ResolutionContext
    ): Resolved =
      inheritedMembers
        .find(name, context)
        .fold(lexicalType(name, arguments, context))(inheritedType(name, arguments, _, context))

    private def lexicalType(
        name: String,
        arguments: Arguments,
        context: ResolutionContext
    ): Resolved =
      resolveName(name, context) match {
        case Found.Absent => shadowedBuiltins.resolve(name, arguments.shapes, context)
        // An import binds the name to its target outright: a builtin or the model reads it when it
        // holds it.
        case Found.Foreign(path, binding) =>
          shadowedBuiltins.imported(path, binding, arguments.shapes, context)
        case failed: Found.Failed => Left(failed.reason)
        case Found.Entry(entry)   =>
          sourceType(name, arguments, entry, context, typeParameters(entry.owner))
            .flatMap(SingletonIntersections.validate(_, context.useSite, this))
      }

    private def inheritedType(
        name: String,
        arguments: Arguments,
        found: Either[String, InheritedTypeMembers.Member],
        context: ResolutionContext
    ): Resolved =
      found.flatMap { (entry, scope) =>
        sourceType(name, arguments, entry, context, scope)
          .flatMap(SingletonIntersections.validate(_, context.useSite, this))
      }

    // A source type resolves through its own declaration: an alias body, a case class's fields, or
    // a representation the model cannot read, under its declaring scope's binders. Its identity
    // guards against a resolution cycle.
    private def sourceType(
        name: String,
        arguments: Arguments,
        entry: TypeEntry,
        context: ResolutionContext,
        scope: Map[String, Resolved]
    ): Resolved = {
      val key = entry.key
      val args = arguments.shapes
      OpaqueTypes.readable(context, entry).flatMap { _ =>
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
                    context.inScope(entry.owner, scope).enter(key),
                    this
                  )
                )
            else Left(s"type constructor used as a proper type: $name")
          case tree if CapabilityRecords.applies(tree) =>
            records.resolve(name, entry, args, context.inScope(entry.owner, scope))
          case _ =>
            val (parameters, bodies) = representations.of(entry, key, context.useSite)
            bodies.flatMap(cases =>
              parameterized(
                Applied(name, arguments, parameters, cases, entry, context, key, scope)
              )
            )
        }
      }
    }

    // One application of a source type: what it is applied to, and what its declaration says it
    // is built from.
    private case class Applied(
        name: String,
        arguments: Arguments,
        parameters: List[Type.Param],
        cases: List[List[Type]],
        entry: TypeEntry,
        context: ResolutionContext,
        key: String,
        scope: Map[String, Resolved]
    )

    // Arguments were read by the declaration's own kinds: a constructor for a higher-kinded
    // parameter, a type for a proper one.
    private def parameterized(applied: Applied): Resolved = {
      import applied.{cases, context, entry, key, name, parameters, scope}
      val args = applied.arguments.shapes
      operators
        .applied(name, parameters)(args)
        .flatMap { replacements =>
          // Proper parameters remember their arguments as written, for a projection to reread.
          val written = parameters.zip(applied.arguments.written).collect {
            case (p, argument) if Kinds.of(p) == Kind.Proper => p.name.value -> argument
          }
          val scoped =
            context.inScope(entry.owner, scope ++ replacements).written(written).enter(key)
          sequence(cases.map(fields => sequence(fields.map(scoped.read(_, this)))))
            .map(shapeOf(entry, _))
        }
    }

    // A case class denotes the product of its fields; an alias the single type it names.
    // A case class and an alias are one case; an enum is as many cases as it declares; a
    // sealed family reached with readable cases is the sum of them.
    private def shapeOf(entry: TypeEntry, cases: List[List[Shape]]): Shape =
      entry.tree match {
        case _: Defn.Enum                                  => Shape.Sum(cases.map(Shape.Product(_)))
        case t: Defn.Trait if Representations.typesOnly(t) => Shape.Product(Nil)
        case _: Defn.Trait                                 => Shape.Sum(cases.map(Shape.Product(_)))
        case c: Defn.Class if Counter.isSealed(c) && Counter.isAbstractClass(c) =>
          Shape.Sum(cases.map(Shape.Product(_)))
        case _: Defn.Type => cases.head.head
        case _            => Shape.Product(cases.head)
      }

    def kindOf(target: Target): String =
      if (target.constructor) "constructor" else if (target.declaration) "declaration" else "method"

    private def measurement(target: Target): Entry = new Measurement(target, this).entry()

  }

}
