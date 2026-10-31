package cardinality.analysis

import scala.meta.*
import scala.util.control.NonFatal

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
    private val representations = new Representations(indexer, accessible, packageGrant)

    def analyze(): List[Entry] = indexer.targets.toList.map(measure)

    // One reading per source: the estimate's scope, solved with every supplied file behind it, so
    // a constructor's parameter answers `Int` with 2^32 and `Nat` with its sealed sum — the same
    // numbers the definitions' rows carry.
    private lazy val readings: Map[String, Reading] = {
      val library = Library.of(inputs.map(_.tree))
      inputs.map(input => input.path -> Reading.of(input.tree, library)).toMap
    }

    def reading(path: String): Reading = readings.getOrElse(path, Reading.bare)

    private val lookupIndex = new SourceLookup(
      indexer.types.toList,
      indexer.packageFrames.toList,
      indexer.moduleFrames.toList,
      indexer.imports.toList.groupMap(_._1)(_._2)
    )

    private val inheritedMembers = new InheritedTypeMembers(lookupIndex, this)

    private lazy val shadowedBuiltins = new ShadowedBuiltins(
      this,
      indexer.types.toList,
      inputs.map(_.tree),
      lookupIndex.wildcardPrefixes
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
      expanding("resolve")(readType(tpe, context.referencing(tpe)))

    // A simple name the template body inherits (or may inherit from an unread parent) is read as
    // that member before any other reading of the name, an alias projection's included.
    private def readType(tpe: Type, context: ResolutionContext): Resolved =
      if (context.visiting.size >= limits.maxTypeDepth) Left("type resolution budget exhausted")
      else
        tpe match {
          case n: Type.Name if !context.variables.contains(n.value) =>
            inheritedMembers
              .find(n.value, context)
              .fold(readShape(tpe, context))(inheritedType(n.value, Nil, _, context))
          case _ => readShape(tpe, context)
        }

    private def readShape(tpe: Type, context: ResolutionContext): Resolved = {
      val ResolutionContext(frame, variables, _, _, _) = context
      tpe match {
        case SingletonIntersections(syntax) =>
          SingletonIntersections.resolve(syntax, context, this)
        case n: Type.Name if variables.contains(n.value)     => variables(n.value)
        case union: Type.ApplyInfix if union.op.value == "|" =>
          resolveUnion(union, context)
        case Type.Or(_, _) => resolveUnion(tpe, context)
        case MethodProjections(projected) if MethodProjections.accepts(projected, context, this) =>
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
        case n: Type.Name    => lexicalType(n.value, Nil, context)
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
        case s: Type.Select => namedType(s.syntax, Nil, context)
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
      if (projectable(callee, name, arguments, context))
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
              case None => namedType(TypeApplications.spelled(callee), args, context)
            }
            resolved
              .map(shape => if (witnesses.isEmpty) shape else Shape.Existential(witnesses, shape))
          }
    }

    private def projectable(
        callee: Type,
        name: String,
        arguments: List[Type],
        context: ResolutionContext
    ): Boolean =
      if (arguments.exists(_.is[Type.Wildcard])) false
      else if (context.variables.contains(name)) false
      else if (inheritedMembers.find(name, context).nonEmpty) false
      else MethodProjections.alias(callee, context, this).nonEmpty

    def inheritsName(tpe: Type, name: String, context: ResolutionContext): Boolean =
      inheritedMembers.find(name, context.referencing(tpe)).nonEmpty

    def parent(init: Init, from: Frame): Either[String, Option[InheritedTypeMembers.Member]] =
      inheritedMembers.parent(init, from)

    def unsuppliedParent(init: Init, from: Frame): Boolean = inheritedMembers.unsupplied(init, from)

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
        args: List[Shape],
        context: ResolutionContext
    ): Resolved =
      inheritedMembers
        .find(name, context)
        .fold(lexicalType(name, args, context))(inheritedType(name, args, _, context))

    private def lexicalType(name: String, args: List[Shape], context: ResolutionContext): Resolved =
      resolveName(name, context) match {
        case Found.Absent => shadowedBuiltins.resolve(name.stripPrefix("_root_."), args, context)
        case Found.Blocked(reason) => Left(reason)
        case Found.Foreign(reason) => Left(reason)
        case Found.Entry(entry)    =>
          sourceType(name, args, entry, context, typeParameters(entry.owner))
            .flatMap(SingletonIntersections.validate(_, context.useSite, this))
      }

    private def inheritedType(
        name: String,
        args: List[Shape],
        found: Either[String, InheritedTypeMembers.Member],
        context: ResolutionContext
    ): Resolved =
      found.flatMap { (entry, scope) =>
        sourceType(name, args, entry, context, scope)
          .flatMap(SingletonIntersections.validate(_, context.useSite, this))
      }

    // A source type resolves through its own declaration: an alias body, a case class's fields, or
    // a representation the model cannot read, under its declaring scope's binders. Its identity
    // guards against a resolution cycle.
    private def sourceType(
        name: String,
        args: List[Shape],
        entry: TypeEntry,
        context: ResolutionContext,
        scope: Map[String, Resolved]
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
                    context.inScope(entry.owner, scope).enter(key),
                    this
                  )
                )
            else
              Left(
                s"parameterized type-lambda alias requires higher-kinded analysis: $name"
              )
          case _ =>
            val (parameters, bodies) = representations.of(entry, key, context.useSite)
            bodies.flatMap(cases =>
              parameterized(Applied(name, args, parameters, cases, entry, context, key, scope))
            )
        }
    }

    // One application of a source type: what it is applied to, and what its declaration says it
    // is built from.
    private case class Applied(
        name: String,
        args: List[Shape],
        parameters: List[Type.Param],
        cases: List[List[Type]],
        entry: TypeEntry,
        context: ResolutionContext,
        key: String,
        scope: Map[String, Resolved]
    )

    private def parameterized(applied: Applied): Resolved = {
      import applied.{args, cases, context, entry, key, name, parameters, scope}
      TypeApplications.checkApplication(name, parameters, args.size).flatMap { _ =>
        val replacements = parameters.zip(args).map((p, a) => p.name.value -> Right(a))
        val env = scope ++ replacements
        val scoped = context.inScope(entry.owner, env).enter(key)
        sequence(
          cases.map(fields => sequence(fields.map(scoped.read(_, this))))
        )
          .map(shapeOf(entry, _))
      }
    }

    // A case class denotes the product of its fields; an alias the single type it names.
    // A case class and an alias are one case; an enum is as many cases as it declares; a
    // sealed family reached with readable cases is the sum of them.
    private def shapeOf(entry: TypeEntry, cases: List[List[Shape]]): Shape =
      entry.tree match {
        case _: Defn.Enum  => Shape.Sum(cases.map(Shape.Product(_)))
        case _: Defn.Trait => Shape.Sum(cases.map(Shape.Product(_)))
        case c: Defn.Class if Counter.isSealed(c) && Counter.isAbstractClass(c) =>
          Shape.Sum(cases.map(Shape.Product(_)))
        case _: Defn.Type => cases.head.head
        case _            => Shape.Product(cases.head)
      }

    // The concrete atoms a signature or a tree mentions, written either bare or qualified: a
    // producer of one of these could change the count of a signature that mentions it.
    private def mentionedNames(tree: Tree): Set[String] =
      TypeName.words(tree).iterator.filter(ConcreteAtoms.contains).toSet

    def mentionedConcrete(target: Target): Set[String] =
      (target.frame.params.flatMap(_.decltpe) ++ target.result).flatMap(mentionedNames).toSet

    def kindOf(target: Target): String =
      if (target.constructor) "constructor" else if (target.declaration) "declaration" else "method"

    private def measurement(target: Target): Entry = new Measurement(target, this).entry()

  }

}
