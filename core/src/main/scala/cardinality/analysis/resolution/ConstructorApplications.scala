package cardinality.analysis.resolution

import scala.meta.{Decl, Defn, Tree, Type, XtensionClassifiable, XtensionSyntax}

import cardinality.analysis.MethodAnalysis.Resolved
import cardinality.analysis.inhabitation.ExistentialInputs
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{Arguments, Operator, ResolutionContext}
import cardinality.analysis.{BuiltinTypes, Resolver}
import cardinality.capacity.Counter
import cardinality.types.{Kind, Kinds, TypeApplications}

/** Constructor applications, read by kind (hkt R1, R3).
  *
  * The callee is resolved first: its declaration says which arguments are constructors. A
  * constructor argument is an operator — a rigid binder passes through, anything else is a closure
  * over the scope it was written in, read only when applied — and a proper one is read as a type
  * where the application is written. A callee whose declaration was not supplied has no kinds to
  * read its arguments by, so only its own reason is reported.
  *
  * Applying a rigid binder gives an uninterpreted application over its arguments' nominal keys
  * (`NominalKeys`); applying a closure β-reduces in the closure's scope, so free names in the
  * constructor resolve where it was written and its parameters shadow any outer binder.
  */
final private[cardinality] class ConstructorApplications(
    resolver: Resolver,
    keys: NominalKeys,
    builtins: ShadowedBuiltins,
    named: (String, Arguments, ResolutionContext) => Resolved,
    sourceNames: Set[String]
) {
  private val existentialCaptures = new ExistentialInputs.Captures
  private val callees = new Callees(resolver, resolver.operators.argument)

  // Infix types are binary constructor applications, not term-method calls: even a colon-ending
  // operator keeps lhs before rhs. Associativity is already encoded in the parser's nested AST.
  // Both spellings share argument resolution, qualified lookup and representation/arity checks.
  def resolve(callee: Type, arguments: List[Type], context: ResolutionContext): Resolved =
    memberApplied(callee, context).map(Left(_)).getOrElse {
      if (projectable(callee, arguments, context))
        MethodProjections.resolve(
          Type.Apply(callee, Type.ArgClause(arguments)),
          context,
          resolver,
          sourceNames
        )
      else applyCallee(callee, context, arguments, context)
    }

  // A first-order member applied to arguments is ill-kinded, and a higher-kinded one is refused
  // where its path is read (members M1): an applied member is never read as a type here.
  private def memberApplied(callee: Type, context: ResolutionContext): Option[String] =
    resolver
      .typeMember(callee, context)
      .map(_.fold(identity, _ => s"type member applied to arguments: ${callee.syntax}"))

  // A projection reads a source alias's applied body; a wildcard argument, a binder or an
  // inherited member has no such body to read.
  private def projectable(callee: Type, arguments: List[Type], context: ResolutionContext) = {
    val name = TypeApplications.name(callee)
    if (arguments.exists(_.is[Type.Wildcard])) false
    else if (context.variables.contains(name)) false
    else if (resolver.inheritsName(callee, name, context)) false
    else MethodProjections.alias(callee, context, resolver).nonEmpty
  }

  // The callee read where it was written, the arguments where the application is.
  private def applyCallee(
      callee: Type,
      calleeContext: ResolutionContext,
      arguments: List[Type],
      context: ResolutionContext
  ): Resolved =
    callees.of(callee, calleeContext).flatMap {
      case Callee.Lambda(lambda, scope) => applyLambda(lambda, scope, arguments, context)
      case Callee.Bound(operator: Operator.Rigid, name) =>
        keys.rigid(operator, name, arguments, context)
      case Callee.Bound(Operator.Closure(tpe, scope), _) =>
        applyCallee(tpe, scope, arguments, context)
      case Callee.Named(named, scope) => applyNamed(named, scope, arguments, context)
    }

  private def applyLambda(
      lambda: Type.Lambda,
      scope: ResolutionContext,
      arguments: List[Type],
      context: ResolutionContext
  ): Resolved = {
    MethodTypeLambdas
      .checked(lambda, arguments.size)
      .flatMap(_ => read(lambda, arguments.zip(lambda.tparamClause.values.map(Kinds.of)), context))
      .flatMap((witnesses, args) =>
        MethodTypeLambdas(lambda, args, scope, resolver).map(packaged(witnesses, _))
      )
  }

  private def applyNamed(
      callee: Type,
      calleeContext: ResolutionContext,
      arguments: List[Type],
      context: ResolutionContext
  ): Resolved = {
    val name = TypeApplications.spelled(callee)
    kinds(callee, name, calleeContext).flatMap { kinds =>
      if (kinds.size != arguments.size) Left(s"type argument arity: $name")
      else
        read(callee, arguments.zip(kinds), context).flatMap((witnesses, args) =>
          // The name is read at its own reference: a closure's callee, written in a parent clause,
          // sees the imports of the file that wrote it.
          named(
            name,
            Arguments(args, arguments.map(_ -> context)),
            calleeContext.referencing(callee)
          )
            .map(packaged(witnesses, _))
        )
    }
  }

  // The kinds a named callee takes: a declaration's own, a builtin's or model entry's proper
  // parameters (the same arity its key reads), and none for a name no supplied declaration answers
  // — the callee's reason is then the only true one.
  private def kinds(
      callee: Type,
      name: String,
      context: ResolutionContext
  ): Either[String, List[Kind]] =
    resolver.declaration(callee, name, context) match {
      case Found.Entry(entry) => Right(ConstructorKinds.parameters(entry.tree).map(Kinds.of))
      case Found.Absent       => builtins.denoted(name, context).flatMap(proper)
      case Found.Foreign(path, binding) =>
        builtins.importedKey(path, binding, context).flatMap(proper)
      case failed: Found.Failed => Left(failed.reason)
    }

  private def proper(canonical: String): Either[String, List[Kind]] =
    BuiltinTypes.arity(canonical).map(List.fill(_)(Kind.Proper))

  private def read(
      callee: Type,
      arguments: List[(Type, Kind)],
      context: ResolutionContext
  ): Either[String, (List[String], List[Shape])] =
    existentialCaptures.arguments(
      callee,
      arguments,
      context.frame,
      (arg, kind) => resolver.operators.argument(kind, arg, context)
    )

  private def packaged(witnesses: List[String], shape: Shape): Shape =
    if (witnesses.isEmpty) shape else Shape.Existential(witnesses, shape)

}

/** The kind of a type written as an argument, where its declaration fixes it (hkt R3): a lambda's
  * from its parameters, a class's or a builtin's from the parameters it takes, a fully applied one
  * proper. A program that passes an argument of another kind is rejected by Scala 3.9.0 ("does not
  * have the same kind as its bound"), whether or not the parameter is ever applied. A name whose
  * declaration is not supplied, an alias that names a constructor without parameters of its own
  * (`type F = Option`) and a placeholder lambda have no kind read here: they are checked where they
  * are applied.
  */
final private[cardinality] class ConstructorKinds(resolver: Resolver) {
  import ConstructorKinds.*

  def of(tpe: Type, context: ResolutionContext): Option[Kind] = tpe match {
    case lambda: Type.Lambda              => Some(arrow(lambda.tparamClause.values.map(Kinds.of)))
    case _: Type.Tuple | _: Type.Function => Some(Kind.Proper)
    case _: Type.Name | _: Type.Select    => named(tpe, context).flatMap(kind)
    case applied: Type.Apply              =>
      named(applied.tpe, context).flatMap(fullyApplied(_, applied.argClause.values.size))
    case _ => None
  }

  private def named(tpe: Type, context: ResolutionContext): Option[Declared] = {
    val name = TypeApplications.spelled(tpe)
    resolver.declaration(tpe, name, context) match {
      case Found.Entry(entry) => Some(Declared.Source(entry.tree))
      case Found.Absent       =>
        BuiltinTypes
          .canonical(name)
          .flatMap(canonical => BuiltinTypes.arity(canonical).toOption.map(Declared.Builtin(_)))
      case _ => None
    }
  }

}

private[cardinality] object ConstructorKinds {

  /** A declaration a constructor name resolves to: supplied source, or a builtin of some arity. */
  enum Declared {
    case Source(tree: Tree)
    case Builtin(arity: Int)
  }

  /** The type parameters a declaration takes; a type-lambda alias without parameters of its own
    * takes its lambda's.
    */
  def parameters(tree: Tree): List[Type.Param] = tree match {
    case alias: Defn.Type if alias.tparamClause.values.isEmpty =>
      lambdaOf(alias).fold(Nil)(_.tparamClause.values)
    case d: Defn      => Counter.parameters(d)
    case d: Decl.Type => d.tparamClause.values
    case _            => Nil
  }

  private[resolution] def kind(declared: Declared): Option[Kind] = declared match {
    case Declared.Builtin(arity) => Some(arrow(List.fill(arity)(Kind.Proper)))
    case Declared.Source(alias: Defn.Type) if alias.tparamClause.values.isEmpty =>
      lambdaOf(alias).map(lambda => arrow(lambda.tparamClause.values.map(Kinds.of)))
    case Declared.Source(alias: Defn.Type) if lambdaOf(alias).nonEmpty => None
    case Declared.Source(
          tree @ (_: Defn.Type | _: Decl.Type | _: Defn.Class | _: Defn.Trait | _: Defn.Enum)
        ) =>
      Some(arrow(parameters(tree).map(Kinds.of)))
    case _ => None
  }

  // An application of every parameter is a proper type; a lambda alias's gives its lambda.
  private[resolution] def fullyApplied(declared: Declared, arguments: Int): Option[Kind] =
    declared match {
      case Declared.Source(alias: Defn.Type) if alias.tparamClause.values.nonEmpty =>
        lambdaOf(alias) match {
          case Some(lambda) if alias.tparamClause.values.size == arguments =>
            Some(arrow(lambda.tparamClause.values.map(Kinds.of)))
          case Some(_) => None
          case None    => Option.when(alias.tparamClause.values.size == arguments)(Kind.Proper)
        }
      case other =>
        kind(other).collect { case kind if kind.arity == arguments && arguments > 0 => Kind.Proper }
    }

  def lambdaOf(alias: Defn.Type): Option[Type.Lambda] =
    TypeApplications.lambda(alias.body).flatMap(_.toOption)

  private[resolution] def arrow(holes: List[Kind]): Kind =
    if (holes.isEmpty) Kind.Proper else Kind.Arrow(holes)

}
