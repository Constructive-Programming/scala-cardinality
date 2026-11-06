package cardinality.analysis.resolution

import scala.meta.{Defn, Type}

import cardinality.analysis.model.{Operator, ResolutionContext, ResolutionOptics, TypeOperators}
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.{Kinds, Spelling, Traversals, TypeApplications}

import MethodAnalysis.{Resolved, TypeEntry}

/** What a constructor application applies, read where the callee is written. Both readings of an
  * application — the structural shape and the nominal key — dispatch on it the same way and differ
  * only in what they do with each case.
  */
private[cardinality] enum Callee {
  case Lambda(lambda: Type.Lambda, scope: ResolutionContext)
  case Bound(operator: Operator, name: Type.Name)
  case Named(callee: Type, scope: ResolutionContext)
}

/** One application being read: the callee, read in `scope` where it is written, and the arguments,
  * read in `context` where the application is.
  */
final private[cardinality] case class Application(
    callee: Type,
    scope: ResolutionContext,
    args: List[Type],
    context: ResolutionContext
) {
  def spelled: String = TypeApplications.spelled(callee)
}

/** The callees of one reading of applications, whose arguments it binds as `bind` reads them. */
final private[cardinality] class Callees(resolver: Resolver, bind: TypeOperators.Binder) {

  def of(callee: Type, context: ResolutionContext): Either[String, Callee] =
    TypeApplications.lambda(callee) match {
      case Some(lambda) => lambda.map(Callee.Lambda(_, context))
      case None         =>
        callee match {
          case n: Type.Name if context.variables.contains(n.value) =>
            resolver.operators.applicable(n, context).map(Callee.Bound(_, n))
          case partial: Type.Apply => opened(partial, context).map(Callee.Lambda(_, _))
          case _                   => Right(Callee.Named(callee, context))
        }
    }

  /** Each parameter bound to its argument as `bind` reads it. */
  def bindAll(
      parameters: List[Type.Param],
      args: List[Type],
      context: ResolutionContext
  ): Either[String, List[(String, Resolved)]] =
    Traversals.sequence(parameters.zip(args).map { (parameter, arg) =>
      bind(Kinds.of(parameter), arg, context).map(parameter.name.value -> Right(_))
    })

  // A partially applied type-lambda alias — `Forget[F]` for `type Forget[F[_]] = [X, A] =>> …` —
  // is its lambda, with the alias's parameters bound to the given arguments in its own scope.
  private def opened(
      applied: Type.Apply,
      context: ResolutionContext
  ): Either[String, (Type.Lambda, ResolutionContext)] = {
    val head = applied.tpe
    val name = TypeApplications.spelled(head)
    val notAlias =
      s"partially applied constructor is not a type-lambda alias: ${Spelling.of(applied)}"
    Found.named(WrittenType(head, context, resolver), name) match {
      // An inherited alias reads under its parent's instantiation, which this scope lacks.
      case Some(Found.Entry(_)) if resolver.inheritsName(head, name, context) =>
        Left(s"partially applied inherited type member not resolved: $name")
      case Some(Found.Entry(entry)) =>
        ResolutionOptics.entryTypeDefinition
          .getOption(entry)
          .flatMap(alias => lambdaOf(alias).map(alias -> _))
          .toRight(notAlias)
          .flatMap((alias, lambda) =>
            opening(entry, alias, Application(head, context, applied.argClause.values, context))
              .map(lambda -> _)
          )
      case Some(failed: Found.Failed) => Left(failed.reason)
      case Some(Found.Absent)         => Left(s"unresolved type: $name")
      case None                       => Left(notAlias)
    }
  }

  private def lambdaOf(alias: Defn.Type): Option[Type.Lambda] =
    ConstructorKinds.lambdaOf(alias).filter(_ => alias.tparamClause.values.nonEmpty)

  /** An alias's own scope, opened for an application: its declaration readable where the callee is
    * written, its parameters bound to the arguments, read where the application is.
    */
  def opening(
      entry: TypeEntry,
      alias: Defn.Type,
      application: Application
  ): Either[String, ResolutionContext] = {
    val parameters = alias.tparamClause.values
    for {
      _ <- OpaqueTypes.readable(application.scope, entry)
      _ <- TypeApplications.checkApplication(
        application.spelled,
        parameters,
        application.args.size
      )
      bound <- bindAll(parameters, application.args, application.context)
    } yield application.scope
      .inScope(entry.owner, resolver.typeParameters(entry.owner) ++ bound)
      .enter(entry.key)
  }

}
