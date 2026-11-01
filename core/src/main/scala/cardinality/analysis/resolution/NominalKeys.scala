package cardinality.analysis.resolution

import scala.meta.{dialects, Decl, Defn, Dialect, Tree, Type, XtensionClassifiable, XtensionSyntax}

import cardinality.analysis.inhabitation.Inhabitation.{Head, Shape}
import cardinality.analysis.model.{Operator, ResolutionContext}
import cardinality.analysis.{BuiltinTypes, MethodAnalysis, Resolver}
import cardinality.capacity.Counter
import cardinality.types.{Kind, Kinds, TypeApplications, TypeSubstitution}

import MethodAnalysis.{sequence, Resolved, TypeEntry}

/** The identity of a type written as an argument of a rigid constructor (hkt R2).
  *
  * A rigid `F` separates types the structural shapes identify on purpose: `Option[A]` and
  * `Either[Unit, A]` are both `1 + A`, yet `F[Option[A]]` and `F[Either[Unit, A]]` are different
  * types with no program between them. So an argument is read as a key — the declarations its names
  * resolve to, its binders' identities — never as a shape. A key respects Scala's type equalities:
  * a transparent alias is expanded where it is declared, an opaque type only where it is visible,
  * tuple and function sugar is the `TupleN` / `FunctionN` it spells, and an operator is its η-long
  * lambda with de Bruijn-like parameters, renumbered in order so α-equivalent lambdas share one
  * key. What needs subtyping or overlap to identify — unions of distinct keys, paths, refinements,
  * abstract members, opaque types outside their scope, names whose declaration is not supplied — is
  * refused with the reason naming it.
  */
final private[cardinality] class NominalKeys(
    resolver: Resolver,
    shadowing: (String, ResolutionContext) => Option[String]
) {
  private given Dialect = dialects.Scala3

  private type Keyed = Either[String, Shape]

  private def written(callee: Type, args: List[Type], context: ResolutionContext) =
    Application(callee, context, args, context)

  // A proper parameter is bound to its argument's key, a higher-kinded one to its operator.
  private val callees = new Callees(resolver, resolver.operators.binder(read))

  // Lambda parameters are numbered below zero while a key is built, and from zero once it is
  // renumbered, so a finished key never collides with one being built.
  private var lambdas = 0

  /** The key of an argument of the given kind: a proper type's key, or an operator's. */
  def argument(tpe: Type, kind: Kind, context: ResolutionContext): Keyed =
    keyOf(tpe, kind, context).map(KeyNumbering.renumbered)

  /** A rigid binder applied to arguments: an uninterpreted application over their keys. Scala 3.9.0
    * rejects an abstract constructor applied to a wildcard (E043, an unreducible application), so
    * such an application is no type and is never counted.
    */
  def rigid(
      operator: Operator.Rigid,
      name: Type.Name,
      args: List[Type],
      context: ResolutionContext
  ): Keyed =
    if (args.exists(_.is[Type.Wildcard]))
      Left(s"unreducible application of a higher-kinded binder to a wildcard: ${name.value}")
    else if (args.size != operator.kind.arity) Left(s"type argument arity: ${name.value}")
    else
      arguments(args, operator.kind.holes, context)
        .map(keys => Shape.rigid(operator.id, keys.map(KeyNumbering.renumbered)))

  private def keyOf(tpe: Type, kind: Kind, context: ResolutionContext): Keyed = kind match {
    case Kind.Proper       => read(tpe, context)
    case arrow: Kind.Arrow => operator(tpe, arrow, context)
  }

  private def arguments(args: List[Type], kinds: List[Kind], context: ResolutionContext) =
    sequence(args.zip(kinds).map(keyOf(_, _, context)))

  private def read(tpe: Type, context: ResolutionContext): Keyed =
    if (context.visiting.size >= resolver.limits.maxTypeDepth)
      Left("type resolution budget exhausted")
    else
      tpe match {
        case n: Type.Name if context.variables.contains(n.value) =>
          variable(n.value, context.variables(n.value))
        case Type.Tuple(args) => builtin(s"Tuple${args.size}", written(tpe, args, context))
        case f: Type.Function =>
          val arity = f.paramClause.values.size
          builtin(s"Function$arity", written(tpe, f.paramClause.values :+ f.res, context))
        case union if TypeSubstitution.unionAlternatives(union).size > 1 =>
          alternatives(union, context)
        case a: Type.Apply => applied(written(a.tpe, a.argClause.values, context))
        case a: Type.ApplyInfix if a.op.value != "&" =>
          applied(written(a.op, List(a.lhs, a.rhs), context))
        case _: Type.Name | _: Type.Select            => applied(written(tpe, Nil, context))
        case _: Type.Lambda | _: Type.AnonymousLambda =>
          Left(s"type constructor used as a proper type: ${tpe.syntax}")
        case other => unestablished(other)
      }

  // A substituted argument keeps the identity it was read with: a binder, a rigid application or a
  // ground scalar. A structural substitution has lost it.
  private def variable(name: String, bound: Resolved): Keyed =
    bound.flatMap {
      case key @ (_: Shape.Atom | _: Shape.Applied) => Right(key)
      case Shape.Ground(ground, _)                  => Right(nominal(s"scala.$ground", Nil))
      case _: Shape.Constructor => Left(s"higher-kinded parameter used as a proper type: $name")
      case _ => Left(s"nominal identity of substituted argument not tracked: $name")
    }

  // `A | A` is `A`; distinct alternatives need an overlap proof.
  private def alternatives(union: Type, context: ResolutionContext): Keyed =
    sequence(TypeSubstitution.unionAlternatives(union).map(read(_, context))).flatMap { keys =>
      keys.distinct match {
        case List(single) => Right(single)
        case _            => unestablished(union)
      }
    }

  private def applied(application: Application): Keyed =
    callees.of(application.callee, application.scope).flatMap {
      case Callee.Lambda(lambda, scope) =>
        keyLambda(lambda, scope, application.args, application.context)
      case Callee.Bound(operator: Operator.Rigid, name) =>
        rigid(operator, name, application.args, application.context)
      case Callee.Bound(Operator.Closure(tpe, scope), _) =>
        applied(application.copy(callee = tpe, scope = scope))
      case Callee.Named(_, _) => named(application)
    }

  private def keyLambda(
      lambda: Type.Lambda,
      scope: ResolutionContext,
      args: List[Type],
      context: ResolutionContext
  ): Keyed =
    for {
      _ <- MethodTypeLambdas.checked(lambda, args.size)
      body <- TypeSubstitution.lambdaBody
        .getOption(lambda)
        .toRight("type lambda has a non-type body")
      bindings <- callees.bindAll(lambda.tparamClause.values, args, context)
      key <- read(body, scope.bind(bindings).enter(s"lambda:${lambda.pos.start}:${lambda.syntax}"))
    } yield key

  private def named(application: Application): Keyed = {
    import application.{callee, scope, spelled}
    callee match {
      case s: Type.Select if RefinedMembers.pathType(s.qual, scope.frame).nonEmpty =>
        Left(s"nominal identity of path type not established: ${s.syntax}")
      case _ =>
        resolver.declaration(callee, spelled, scope) match {
          // An inherited member is read under its parent's instantiation, which a key does not
          // carry: `P[Int]#T` and `P[Long]#T` are different types.
          case Found.Entry(_) if resolver.inheritsName(callee, spelled, scope) =>
            Left(s"nominal identity of inherited type member not established: $spelled")
          case Found.Entry(entry)   => declared(entry, application)
          case Found.Absent         => builtin(spelled, application)
          case failed: Found.Failed => Left(failed.reason)
        }
    }
  }

  // An alias, opaque or not, is read through: a transparent one is the type it names, an opaque
  // one only where it is visible. Outside its scope an opaque type is not a readable key: its API
  // may relate it to other types (`inside: F[W[A]] => F[A]`), which no key records.
  private def declared(entry: TypeEntry, application: Application): Keyed =
    entry.tree match {
      case alias: Defn.Type                         => expand(entry, alias, application)
      case d: (Defn.Class | Defn.Trait | Defn.Enum) =>
        declaration(entry, Counter.parameters(d), application)
      case _: Decl.Type =>
        Left(s"nominal identity of abstract type member not established: ${application.spelled}")
      case other => unestablished(other)
    }

  // A transparent alias is the type it names, read where it is declared; a type-lambda alias
  // without parameters of its own applies its lambda there.
  private def expand(entry: TypeEntry, alias: Defn.Type, application: Application): Keyed = {
    import application.{args, context}
    TypeApplications.lambda(alias.body) match {
      case Some(lambda) if alias.tparamClause.values.isEmpty && args.nonEmpty =>
        for {
          scope <- callees.opening(entry, alias, application.copy(args = Nil))
          body <- lambda
          key <- keyLambda(body, scope, args, context)
        } yield key
      case Some(_) =>
        OpaqueTypes
          .readable(application.scope, entry)
          .flatMap(_ => Left(s"type constructor used as a proper type: ${application.spelled}"))
      case None => callees.opening(entry, alias, application).flatMap(read(alias.body, _))
    }
  }

  // A class, trait or enum is its own identity, applied to its arguments' keys.
  private def declaration(
      entry: TypeEntry,
      parameters: List[Type.Param],
      application: Application
  ): Keyed =
    if (parameters.size != application.args.size) Left(s"type argument arity: ${entry.name}")
    else
      arguments(application.args, parameters.map(Kinds.of), application.context)
        .map(nominal(TypeIdentities.key(entry), _))

  private def builtin(name: String, application: Application): Keyed = {
    val plain = name.stripPrefix("_root_.")
    BuiltinTypes.canonical(name) match {
      case None            => Left(s"unresolved type: $plain")
      case Some(canonical) =>
        val written = application.callee.is[Type.Name] || application.callee.is[Type.Select]
        Option.when(written)(plain).flatMap(shadowing(_, application.scope)) match {
          case Some(reason) => Left(reason)
          case None         =>
            val args = application.args
            if (args.size != BuiltinTypes.nominalArity(canonical))
              Left(s"type argument arity: $plain")
            else
              arguments(args, args.map(_ => Kind.Proper), application.context)
                .map(nominal(s"scala.$canonical", _))
        }
    }
  }

  // An operator's key is its η-long lambda: applied to fresh parameters, abstracted over them.
  // Operators whose holes are themselves higher-kinded are not keyed.
  private def operator(tpe: Type, kind: Kind.Arrow, context: ResolutionContext): Keyed =
    if (kind.holes.exists(_ != Kind.Proper)) unestablished(tpe)
    else {
      lambdas -= 1
      val id = lambdas
      val names = kind.holes.indices.map(index => s"<parameter $id $index>").toList
      val parameters = names.zipWithIndex.map { (name, index) =>
        name -> Right(Shape.Applied(Head.Bound(id, index), Nil))
      }
      applied(Application(tpe, context, names.map(Type.Name(_)), context.bind(parameters)))
        .map(body => Shape.Applied(Head.Lambda(id, kind.arity), List(body)))
    }

  private def nominal(declaration: String, args: List[Shape]): Shape =
    Shape.Applied(Head.Nominal(declaration), args)

  private def unestablished(tree: Tree): Keyed =
    Left(s"nominal identity of argument not established: ${tree.syntax.replaceAll("\\s+", " ")}")

}

/** Lambda parameters renumbered by their lambda's position in the key: α-equivalent keys equal. */
private[cardinality] object KeyNumbering {

  def renumbered(key: Shape): Shape = renumber(key, Map.empty, 0)._1

  private def renumber(shape: Shape, scope: Map[Int, Int], next: Int): (Shape, Int) =
    shape match {
      case Shape.Applied(Head.Lambda(id, arity), body) =>
        val (kids, after) = all(body, scope + (id -> next), next + 1)
        (Shape.Applied(Head.Lambda(next, arity), kids), after)
      case Shape.Applied(Head.Bound(id, index), args) =>
        val (kids, after) = all(args, scope, next)
        (Shape.Applied(Head.Bound(scope.getOrElse(id, id), index), kids), after)
      case other =>
        val (kids, after) = all(Shape.children(other), scope, next)
        (Shape.rebuild(other, kids)(identity), after)
    }

  private def all(shapes: List[Shape], scope: Map[Int, Int], next: Int): (List[Shape], Int) =
    shapes.foldLeft((List.empty[Shape], next)) {
      case ((done, counter), shape) =>
        val (renumbered, after) = renumber(shape, scope, counter)
        (done :+ renumbered, after)
    }

}
