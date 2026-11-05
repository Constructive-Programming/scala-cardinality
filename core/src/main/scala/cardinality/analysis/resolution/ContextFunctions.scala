package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Resolved, TypeEntry}
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{AliasReading, Operator, ResolutionContext}
import cardinality.analysis.{Resolver, StdlibModel}
import cardinality.types.{Kinds, Spelling, TypeApplications}

/** Context function types, `T ?=> R` (Scala 3 reference, "Context Functions").
  *
  * A value of `T ?=> R` is a function from `T` to `R`. A body applies one to a `T` it passes
  * explicitly (`f(using t)`) or leaves to implicit search, which picks one of the values it could
  * pass anyway, and builds one as a literal `(t: T) ?=> e` whose binder it may name: so the type
  * reads as `T => R`. One thing is not a function's: an expression whose expected type is a context
  * function is wrapped in such a literal, and its binders are givens inside it. Scala wraps exactly
  * where the expected type, dealiased, is a context function class. This reading follows a result
  * type through aliases with their arguments, applied type lambdas, substituted constructors and
  * members as seen from the receiver, and finds no context function there; a context function type
  * itself, and any form it does not follow, stops it with its reason.
  */
private[cardinality] object ContextFunctions {

  /** One binder a context function puts around a body: its type, the scope to read it in, and its
    * clause in the chain — the binders of one clause are given together, each `?=>` nests the next.
    */
  final case class Binder(tpe: Type, context: ResolutionContext, clause: Int)

  /** The binders a result type puts around a whole body: none, or why the chain is not read. A name
    * the sources do not supply (nor the stdlib model) ends the chain, for a caller whose own
    * reading of that name already fails. `written` gives binders of the context their arguments as
    * written.
    */
  def binders(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver,
      written: List[(String, Type, ResolutionContext)] = Nil
  ): Either[String, List[Binder]] = {
    val arguments = written.map((name, arg, scope) => name -> Argument(arg, Site(scope, Map.empty)))
    new Chain(resolver, strict = false)
      .walk(tpe, Site(context, arguments.toMap), Rest(tpe, Nil))
      .map(_ => Nil)
  }

  /** The binders as `binders` reads them, where nothing else reads the type: a name not supplied
    * stops the chain with its reason.
    */
  def required(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver
  ): Either[String, List[Binder]] =
    new Chain(resolver, strict = true)
      .walk(tpe, Site(context, Map.empty), Rest(tpe, Nil))
      .map(_ => Nil)

  /** The types besides its own that a value of `tpe`, read as `shape`, answers implicit search for:
    * none, as this reading finds no context function chain.
    */
  def results(
      @annotation.unused tpe: Type,
      @annotation.unused shape: Shape,
      @annotation.unused context: ResolutionContext,
      @annotation.unused resolver: Resolver
  ): List[Shape] = Nil

  /** Where a type is written: its scope, and each alias or lambda parameter's argument there, as
    * written and where it is written.
    */
  final private case class Site(context: ResolutionContext, written: Map[String, Argument])

  final private case class Argument(tpe: Type, site: Site)

  private type Read = Either[String, Unit]

  private val Plain: Read = Right(())

  /** What a type read is still to be applied to — its argument lists, innermost first — and the
    * type as the declaration being read spells it, which a reason names.
    */
  final private case class Rest(spelled: Type, lists: List[List[Argument]]) {
    def next: Rest = copy(lists = lists.drop(1))
    def push(args: List[Argument]): Rest = copy(lists = args :: lists)
  }

  final private class Chain(resolver: Resolver, strict: Boolean) {

    def walk(tpe: Type, site: Site, rest: Rest): Read =
      application(tpe, site, rest).getOrElse(leaf(tpe, site, rest))

    // A form that applies a head to arguments, or applies itself to the pending ones.
    private def application(tpe: Type, site: Site, rest: Rest): Option[Read] = tpe match {
      case lambda: Type.Lambda if rest.lists.nonEmpty =>
        Some(reduced(lambda, site, rest.lists.head, rest.next))
      case a: Type.Apply =>
        val (callee, args) = TypeApplications.headAndArguments(a)
        Some(applied(callee, args.map(Argument(_, site)), site, rest))
      case named @ (_: Type.Name | _: Type.Select) =>
        Some(applied(named, rest.lists.headOption.getOrElse(Nil), site, rest.next))
      case _ => None
    }

    // A form applied to nothing more: a type that is never a context function ends the chain.
    private def leaf(tpe: Type, site: Site, rest: Rest): Read = tpe match {
      case _: (Type.Function | Type.PolyFunction | Type.Tuple | Lit | Type.Singleton | Type.ByName |
            Type.Repeated) =>
        Plain
      case refined: Type.Refine if rest.lists.isEmpty =>
        refined.tpe.fold(Plain)(walk(_, site, rest))
      case _ => notRead(rest)
    }

    // A head and its arguments, each argument read where it is written: an alias or lambda
    // parameter is its argument, an application is read first, a binder of the scope is read by
    // what it is bound to, and a name by its declaration.
    private def applied(callee: Type, args: List[Argument], site: Site, rest: Rest): Read =
      callee match {
        case Type.Name(param) if site.written.contains(param) =>
          substituted(site.written(param), args, rest)
        case _: Type.Apply | _: Type.ApplyInfix => walk(callee, site, rest.push(args))
        case n: Type.Name if site.context.variables.contains(n.value) =>
          if (rest.lists.isEmpty) variable(n, args, site.context) else notRead(rest)
        case _: Type.Name | _: Type.Select => named(callee, args, site.context, rest)
        case _                             => notRead(rest)
      }

    // An argument in a head's place, applied to the head's arguments: read in the argument's own
    // scope with the arguments kept in theirs.
    private def substituted(argument: Argument, args: List[Argument], rest: Rest): Read =
      if (args.isEmpty) walk(argument.tpe, argument.site, rest.copy(spelled = argument.tpe))
      else
        TypeApplications.lambda(argument.tpe) match {
          case None    => applied(argument.tpe, args, argument.site, rest)
          case Some(_) => notRead(rest)
        }

    // β-reduction: the body read where the lambda is written, each parameter its argument.
    private def reduced(lambda: Type.Lambda, site: Site, args: List[Argument], rest: Rest): Read = {
      val params = lambda.tparamClause.values
      val scope = site.context.bind(params.zip(args).map((p, a) => p.name.value -> bound(p, a)))
      walk(lambda.tpe, Site(scope, site.written ++ params.map(_.name.value).zip(args)), rest)
    }

    private def bound(parameter: Type.Param, argument: Argument): Resolved =
      resolver.operators.argument(Kinds.of(parameter), argument.tpe, argument.site.context)

    // A binder of the scope, which this reading sees only as what it is bound to: a rigid binder
    // wraps nothing.
    private def variable(name: Type.Name, args: List[Argument], context: ResolutionContext): Read =
      if (args.isEmpty)
        context.variables(name.value) match {
          case Right(_: Shape.Function) => Left("context function behind a substituted type")
          case Right(_)                 => Plain
          case Left(reason)             => unsupplied(reason)
        }
      else
        resolver.operators.applicable(name, context) match {
          case Right(_: Operator.Rigid) => Plain
          case _ => Left("context function behind an applied type parameter not read")
        }

    // A name: a type member as seen from the receiver `this` is read as, a source alias followed
    // with its arguments (an inherited one is read where it is declared, under binders this
    // reading does not substitute); a class or a library type ends the chain.
    private def named(
        callee: Type,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Read = {
      val name = TypeApplications.name(callee)
      val at = context.referencing(callee)
      if (RefinedMembers.throughValue(callee, context.frame))
        Left(s"context function result through a path not read: ${Spelling.of(rest.spelled)}")
      else
        member(callee, args, at, rest).getOrElse(
          resolver.declaration(callee, name, at) match {
            case Found.Entry(entry)
                if visibleAlias(entry, context) && resolver.inheritsName(callee, name, at) =>
              notRead(rest)
            case Found.Entry(entry) => declared(entry, args, context, rest)
            case Found.Absent       => resolver.builtin(name, at).fold(unsupplied, _ => Plain)
            case Found.Foreign(path, _) if StdlibModel.entry(path).nonEmpty => Plain
            case failed: Found.Failed => unsupplied(failed.reason)
          }
        )
    }

    // A type member is what its receiver makes it (members M2).
    private def member(
        callee: Type,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Option[Read] =
      resolver
        .unappliedMember(callee, args.map(_.tpe), context)
        .map {
          case Right(TypeMembers.Meaning.Alias(body, scope)) =>
            walk(body, Site(scope, Map.empty), rest.copy(spelled = callee))
          case Right(_)     => Plain
          case Left(reason) => unsupplied(reason)
        }

    // An alias visible as one here: an opaque one hidden here is no context function at this use.
    private def visibleAlias(entry: TypeEntry, context: ResolutionContext): Boolean =
      entry.tree.is[Defn.Type] && !OpaqueTypes.hidden(entry, context.useSite)

    // A transparent alias visible here is followed with its arguments.
    private def declared(
        entry: TypeEntry,
        args: List[Argument],
        context: ResolutionContext,
        rest: Rest
    ): Read =
      entry.tree match {
        case alias: Defn.Type
            if visibleAlias(entry, context) && alias.tparamClause.values.size == args.size =>
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
              walk(alias.body, Site(scope, params.map(_.name.value).zip(args).toMap), rest)
            )
        case _ => Plain
      }

    private def notRead(rest: Rest): Read =
      Left(s"context function not read: ${Spelling.of(rest.spelled)}")

    private def unsupplied(reason: String): Read = if (strict) Left(reason) else Plain

  }

}
