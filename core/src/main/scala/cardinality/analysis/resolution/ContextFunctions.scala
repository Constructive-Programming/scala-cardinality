package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Resolved, TypeEntry}
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{AliasReading, Operator, ResolutionContext}
import cardinality.analysis.{Resolver, StdlibModel}
import cardinality.types.TypeSubstitution.{Meet, Union}
import cardinality.types.{Kinds, Spelling, TypeApplications}

/** Context function types, `T ?=> R` (Scala 3 reference, "Context Functions").
  *
  * A value of `T ?=> R` is a function from `T` to `R`. A body applies one to a `T` it passes
  * explicitly (`f(using t)`) or leaves to implicit search, which picks one of the values it could
  * pass anyway, and builds one as a literal `(t: T) ?=> e` whose binder it may name: so the type
  * reads as `T => R`. One thing is not a function's: an expression whose expected type is a context
  * function is wrapped in such a literal, and its binders are givens inside it, nested deeper than
  * anything outside. Where the wrapper encloses part of a body, the body can read outer evidence
  * before it (`val o = summon[T]`; fixture cf3 `hoisted` prints the outer value), so nothing is
  * hidden. A method's own result encloses the whole body — a block, a helper call and a local
  * definition included (fixtures cf, cf2, cf3: `g(using 1)(using 2)` prints 2 in each) — so there
  * its binders hide outer evidence of their types from every search the body makes.
  *
  * Scala wraps exactly where the expected type, dealiased, is a context function class: through
  * aliases and their arguments, applied type lambdas, a match type that reduces to one, a member as
  * seen from its receiver, a projection, a path, a refinement declaring `apply`; never through a
  * union, an intersection, a singleton or an abstract type (3.9.0 fixtures a5-fix1 w1–w4).
  */
private[cardinality] object ContextFunctions {

  /** One binder a context function puts around a body: its type, the scope to read it in, and its
    * clause in the chain — the binders of one clause are given together, each `?=>` nests the next.
    */
  final case class Binder(tpe: Type, context: ResolutionContext, clause: Int)

  /** The binders a result type puts around a whole body: the parameters of the chain
    * `T1 ?=> … ?=> R` it starts with, followed through aliases with their arguments, applied type
    * lambdas, substituted constructors and members as seen from the receiver. A form the reading
    * does not follow and that may be a context function — a match type, a projection, a path, an
    * inherited or partially applied alias, a refinement of one, a parameter an `extends` clause
    * substituted with a function, `ContextFunctionN` by name — stops it with its reason. A name the
    * sources do not supply (nor the stdlib model) ends the chain, for a caller whose own reading of
    * that name already fails. `written` gives binders of the context their arguments as written,
    * each with its scope, so a function argument is read as the type it spells rather than the
    * shape it was bound to.
    */
  def binders(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver,
      written: List[(String, Type, ResolutionContext)] = Nil
  ): Either[String, List[Binder]] = {
    val arguments =
      written.map((name, arg, scope) => name -> Argument(arg, Site(scope, NoArguments)))
    chain(tpe, Site(context, Arguments(arguments.toMap)), new Chain(resolver, strict = false))
  }

  /** The binders as `binders` reads them, where nothing else reads the type: a name not supplied
    * stops the chain with its reason.
    */
  def required(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver
  ): Either[String, List[Binder]] =
    chain(tpe, Site(context, NoArguments), new Chain(resolver, strict = true))

  /** The types besides its own that a value of `tpe`, read as `shape`, answers implicit search for:
    * Scala applies a candidate of type `A ?=> B ?=> C` to the givens it finds where a search wants
    * `B ?=> C` or `C` (fixtures review-A5 r2 cf4, amb2: `using A ?=> B` and `using B` are ambiguous
    * for `summon[B]`). A chain this reading does not follow may be one: then each function the
    * shape starts with is taken as a context function.
    */
  def results(
      tpe: Type,
      shape: Shape,
      context: ResolutionContext,
      resolver: Resolver
  ): List[Shape] =
    shape match {
      case _: Shape.Function =>
        val clauses = required(tpe, context, resolver).toOption
          .map(_.map(_.clause + 1).maxOption.getOrElse(0))
        peeled(shape, clauses)
      case _ => Nil
    }

  // The results under the first `clauses` functions of a shape (under all of them when unknown).
  private def peeled(shape: Shape, clauses: Option[Int]): List[Shape] = shape match {
    case Shape.Function(_, result) if clauses.forall(_ > 0) =>
      result :: peeled(result, clauses.map(_ - 1))
    case _ => Nil
  }

  private def chain(tpe: Type, site: Site, reading: Chain): Either[String, List[Binder]] =
    reading.walk(tpe, site, Rest(tpe, Nil)).map { clauses =>
      clauses.zipWithIndex
        .flatMap((clause, index) => clause.map((binder, scope) => Binder(binder, scope, index)))
    }

  /** Where a type is written: its scope, and each alias or lambda parameter's argument there, as
    * written and where it is written.
    */
  final private case class Site(context: ResolutionContext, written: Arguments)

  final private case class Argument(tpe: Type, site: Site)

  final private case class Arguments(bound: Map[String, Argument])

  private val NoArguments = Arguments(Map.empty)

  private type Clauses = Either[String, List[List[(Type, ResolutionContext)]]]

  /** What a type read is still to be applied to — its argument lists, innermost first:
    * `Alias[F][X, A]` reads `Alias[F]`, then applies what it reduces to to `[X, A]` — and the type
    * as the declaration being read spells it, which a reason names.
    */
  final private case class Rest(spelled: Type, lists: List[List[Argument]]) {
    def next: Rest = copy(lists = lists.drop(1))
    def push(args: List[Argument]): Rest = copy(lists = args :: lists)
  }

  final private class Chain(resolver: Resolver, strict: Boolean) {

    def walk(tpe: Type, site: Site, rest: Rest): Clauses =
      if (site.context.visiting.size >= resolver.limits.maxTypeDepth)
        Left("context function chain budget exhausted")
      else application(tpe, site, rest).getOrElse(leaf(tpe, site, rest))

    // A form that applies a head to arguments, or applies itself to the pending ones.
    private def application(tpe: Type, site: Site, rest: Rest): Option[Clauses] = tpe match {
      case lambda: Type.Lambda if rest.lists.nonEmpty =>
        Some(reduced(lambda, site, rest.lists.head, rest.next))
      case i: Type.ApplyInfix if !Plain.unapply(i) =>
        Some(applied(i.op, List(i.lhs, i.rhs).map(Argument(_, site)), site, rest))
      case a: Type.Apply =>
        val (callee, args) = TypeApplications.headAndArguments(a)
        Some(applied(callee, args.map(Argument(_, site)), site, rest))
      case named @ (_: Type.Name | _: Type.Select) =>
        Some(applied(named, rest.lists.headOption.getOrElse(Nil), site, rest.next))
      case a: Type.Annotate => Some(walk(a.tpe, site, rest))
      case _                => None
    }

    // A form applied to nothing more: a context function adds its clause, a plain type ends.
    private def leaf(tpe: Type, site: Site, rest: Rest): Clauses = tpe match {
      case _ if rest.lists.nonEmpty => Left(behind("an applied type", rest.spelled))
      case f: Type.ContextFunction  =>
        walk(f.res, site, rest).map(f.paramClause.values.map(_ -> site.context) :: _)
      case Plain()              => Right(Nil)
      case refined: Type.Refine => refinement(refined, site, rest)
      case other                => Left(behind(construct(other), rest.spelled))
    }

    // A head and its arguments, each argument read where it is written: an alias or lambda
    // parameter is its argument, a lambda is reduced, an application is read first, a binder of
    // the scope is read by what it is bound to, and a name by its declaration.
    private def applied(callee: Type, args: List[Argument], site: Site, rest: Rest): Clauses =
      callee match {
        case Type.Name(param) if site.written.bound.contains(param) =>
          substituted(site.written.bound(param), args, rest)
        case lambda: Type.Lambda                => reduced(lambda, site, args, rest)
        case _: Type.Apply | _: Type.ApplyInfix => walk(callee, site, rest.push(args))
        case n: Type.Name if site.context.variables.contains(n.value) =>
          if (rest.lists.isEmpty) variable(n, args, site.context, rest)
          else Left(behind("an applied type parameter", rest.spelled))
        case _: Type.Name | _: Type.Select => named(callee, args, site.context, rest)
        case other                         => Left(behind(construct(other), rest.spelled))
      }

    // An argument in a head's place, applied to the head's arguments: a lambda (or placeholder) is
    // reduced, a name or an application is read in the argument's own scope with the arguments
    // kept in theirs.
    private def substituted(argument: Argument, args: List[Argument], rest: Rest): Clauses =
      if (args.isEmpty) walk(argument.tpe, argument.site, rest.copy(spelled = argument.tpe))
      else
        TypeApplications.lambda(argument.tpe) match {
          case Some(Right(lambda)) => reduced(lambda, argument.site, args, rest)
          case Some(Left(reason))  => Left(reason)
          case None                => applied(argument.tpe, args, argument.site, rest)
        }

    // β-reduction: the body read where the lambda is written, each parameter its argument.
    private def reduced(
        lambda: Type.Lambda,
        site: Site,
        args: List[Argument],
        rest: Rest
    ): Clauses = {
      val params = lambda.tparamClause.values
      if (params.size != args.size) Left(behind("a type lambda applied partially", rest.spelled))
      else {
        val scope = site.context.bind(params.zip(args).map((p, a) => p.name.value -> bound(p, a)))
        val names = params.map(_.name.value).zip(args)
        walk(lambda.tpe, Site(scope, Arguments(site.written.bound ++ names)), rest)
      }
    }

    private def bound(parameter: Type.Param, argument: Argument): Resolved =
      resolver.operators.argument(Kinds.of(parameter), argument.tpe, argument.site.context)

    // A binder of the scope, which this reading sees only as what it is bound to: a rigid binder
    // wraps nothing, a constructor an application gave is applied, and a type parameter an
    // `extends` clause substituted is read as a shape that has lost `?=>` — a function there may
    // have been a context function.
    private def variable(
        name: Type.Name,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Clauses =
      if (args.isEmpty)
        context.variables(name.value) match {
          case Right(_: Shape.Function) =>
            Left(behind("a substituted type parameter", rest.spelled))
          case Right(_)     => Right(Nil)
          case Left(reason) => unsupplied(reason)
        }
      else
        resolver.operators.applicable(name, context) match {
          case Right(Operator.Closure(body, scope)) =>
            substituted(Argument(body, Site(scope, NoArguments)), args, rest)
          case Right(_: Operator.Rigid) => Right(Nil)
          case Left(reason)             => unsupplied(reason)
        }

    // A name: a type member as seen from the receiver `this` is read as, a source alias followed
    // with its arguments (an inherited one is read where it is declared, under binders this
    // reading does not substitute); a class or a library type ends the chain.
    private def named(
        callee: Type,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Clauses = {
      val name = TypeApplications.name(callee)
      val at = context.referencing(callee)
      if (RefinedMembers.throughValue(callee, context.frame))
        Left(s"context function result through a path not read: ${Spelling.of(rest.spelled)}")
      else
        member(callee, args, at, rest).getOrElse(
          resolver.declaration(callee, name, at) match {
            case Found.Entry(entry)
                if visibleAlias(entry, context) && resolver.inheritsName(callee, name, at) =>
              Left(behind("an inherited alias", rest.spelled))
            case Found.Entry(entry) => declared(entry, args, context, rest)
            case Found.Absent if ContextFunctionName.matches(name.stripPrefix("scala.")) =>
              Left(s"context function spelled by name: $name")
            case Found.Absent => resolver.builtin(name, at).fold(unsupplied, _ => Right(Nil))
            case Found.Foreign(path, _) if StdlibModel.entry(path).nonEmpty => Right(Nil)
            case failed: Found.Failed => unsupplied(failed.reason)
          }
        )
    }

    // A type member is what its receiver makes it (members M2): the subclass's alias of a member a
    // parent leaves abstract wraps the body of a member the subclass implements (fixture o3 `F6`).
    private def member(
        callee: Type,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Option[Clauses] =
      resolver
        .unappliedMember(callee, args.map(_.tpe), context)
        .map {
          case Right(TypeMembers.Meaning.Alias(body, scope)) =>
            walk(body, Site(scope, NoArguments), rest.copy(spelled = callee))
          case Right(_: TypeMembers.Meaning.Rigid)         => Right(Nil)
          case Right(_: TypeMembers.Meaning.RigidOperator) => Right(Nil)
          case Left(reason)                                => unsupplied(reason)
        }

    // An alias visible as one here: an opaque one hidden here is no context function at this use.
    private def visibleAlias(entry: TypeEntry, context: ResolutionContext): Boolean =
      entry.tree.is[Defn.Type] && !OpaqueTypes.hidden(entry, context.useSite)

    // A transparent alias visible here is followed with its arguments; a partial application is a
    // lambda.
    private def declared(
        entry: TypeEntry,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Clauses =
      entry.tree match {
        case _: Defn.Type if !visibleAlias(entry, context)                   => Right(Nil)
        case alias: Defn.Type if alias.tparamClause.values.size == args.size =>
          val params = alias.tparamClause.values
          context
            .enterAlias(
              entry,
              resolver.typeParameters(entry.owner),
              AliasReading.Transparent,
              params.zip(args).map(bound)
            )
            .toRight(s"recursive alias: ${entry.name}")
            .flatMap(scope =>
              val written = Arguments(params.map(_.name.value).zip(args).toMap)
              walk(alias.body, Site(scope, written), rest)
            )
        case _: Defn.Type => Left(behind("an alias applied partially", rest.spelled))
        case _            => Right(Nil)
      }

    // Scala wraps a refinement of a context function in some forms (one declaring `apply`) and not
    // in others (fixture w3): a refinement of a type that is none wraps nothing.
    private def refinement(refined: Type.Refine, site: Site, rest: Rest): Clauses =
      refined.tpe.fold[Clauses](Right(Nil))(parent =>
        walk(parent, site, rest).flatMap {
          case Nil => Right(Nil)
          case _   => Left(behind("a refinement", rest.spelled))
        }
      )

    private def unsupplied(reason: String): Clauses = if (strict) Left(reason) else Right(Nil)

  }

  // Types whose dealiased form is never a context function class: a function, a tuple, a union or
  // intersection of anything (fixture w2: `K | K`, `K & K` are not wrapped), a literal, a singleton.
  private object Plain {

    def unapply(tpe: Type): Boolean = tpe match {
      case _: Type.Function | _: Type.PolyFunction | _: Type.Tuple | _: Lit | _: Type.Singleton |
          _: Type.ByName | _: Type.Repeated =>
        true
      case Union(_, _) | Meet(_, _) => true
      case _                        => false
    }

  }

  private def behind(what: String, at: Type): String =
    s"context function behind $what not read: ${Spelling.of(at)}"

  private def construct(tpe: Type): String = tpe match {
    case _: Type.Match   => "a match type"
    case _: Type.Project => "a type projection"
    case _               => "a type"
  }

  private val ContextFunctionName = "ContextFunction[0-9]+".r

}
