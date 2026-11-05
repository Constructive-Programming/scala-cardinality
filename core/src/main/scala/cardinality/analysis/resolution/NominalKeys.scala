package cardinality.analysis.resolution

import scala.meta.{dialects, Defn, Dialect, Tree, Type}

import cardinality.analysis.inhabitation.Inhabitation.{Head, Shape}
import cardinality.analysis.model.{Operator, ResolutionContext}
import cardinality.analysis.{BuiltinTypes, MethodAnalysis, Resolver}
import cardinality.types.{Kind, Spelling, Traversals, TypeApplications}

import MethodAnalysis.{Resolved, TypeEntry}

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
    builtins: ShadowedBuiltins
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

  /** A rigid binder applied to arguments: an uninterpreted application over their keys. */
  def rigid(operator: Operator.Rigid, args: List[Type], context: ResolutionContext): Keyed =
    arguments(args, operator.kind.holes, context)
      .map(keys => Shape.rigid(operator.id, keys.map(KeyNumbering.renumbered)))

  private def keyOf(tpe: Type, kind: Kind, context: ResolutionContext): Keyed = kind match {
    case Kind.Proper       => read(tpe, context)
    case arrow: Kind.Arrow => operator(tpe, arrow, context)
  }

  private def arguments(args: List[Type], kinds: List[Kind], context: ResolutionContext) =
    Traversals.sequence(args.zip(kinds).map(keyOf(_, _, context)))

  private def read(tpe: Type, context: ResolutionContext): Keyed =
    tpe match {
      case n: Type.Name if context.variables.contains(n.value) =>
        variable(n.value, context.variables(n.value))
      // Tuple and arrow syntax spell `scala.TupleN` / `scala.FunctionN` themselves: no name to
      // shadow.
      case Type.Tuple(args) => builtin(s"scala.Tuple${args.size}", written(tpe, args, context))
      case f: Type.Function =>
        val arity = f.paramClause.values.size
        builtin(s"scala.Function$arity", written(tpe, f.paramClause.values :+ f.res, context))
      case a: Type.Apply                 => applied(written(a.tpe, a.argClause.values, context))
      case _: Type.Name | _: Type.Select => applied(written(tpe, Nil, context))
      case other                         => unestablished(other)
    }

  // A substituted argument keeps the identity it was read with: a binder or a rigid application.
  // A structural substitution has lost it.
  private def variable(name: String, bound: Resolved): Keyed =
    bound.flatMap {
      case key @ (_: Shape.Atom | _: Shape.Applied) => Right(key)
      case _ => Left(s"nominal identity of substituted argument not tracked: $name")
    }

  private def applied(application: Application): Keyed =
    callees.of(application.callee, application.scope).flatMap {
      case Callee.Bound(operator: Operator.Rigid, _) =>
        rigid(operator, application.args, application.context)
      case Callee.Named(_, _) => named(application)
      case _                  => unestablished(application.callee)
    }

  private def named(application: Application): Keyed = {
    import application.{callee, scope, spelled}
    member(application).getOrElse(
      resolver.declaration(callee, spelled, scope) match {
        // An inherited member is read under its parent's instantiation, which a key does not
        // carry: `P[Int]#T` and `P[Long]#T` are different types.
        case Found.Entry(_) if resolver.inheritsName(callee, spelled, scope) =>
          Left(s"nominal identity of inherited type member not established: $spelled")
        case Found.Entry(entry) => declared(entry, application)
        case Found.Absent       => builtins.denoted(spelled, scope).flatMap(builtin(_, application))
        case failed: Found.Failed => Left(failed.reason)
      }
    )
  }

  // A type member is its path's: an alias the key of its body where it is declared, an abstract
  // member its atom (members M1) — the identity a binder has.
  private def member(application: Application): Option[Keyed] =
    resolver
      .unappliedMember(application.callee, application.args, application.scope)
      .map(
        _.flatMap(
          _.fold(
            read,
            atom => Right(Shape.Atom(atom)),
            (_, _) => Left("higher-kinded type member used as a proper type")
          )
        )
      )

  // An alias, opaque or not, is read through: a transparent one is the type it names, an opaque
  // one only where it is visible. Outside its scope an opaque type is not a readable key: its API
  // may relate it to other types (`inside: F[W[A]] => F[A]`), which no key records.
  private def declared(entry: TypeEntry, application: Application): Keyed =
    entry.tree match {
      case alias: Defn.Type => expand(entry, alias, application)
      case other            => unestablished(other)
    }

  // A transparent alias is the type it names, read where it is declared.
  private def expand(entry: TypeEntry, alias: Defn.Type, application: Application): Keyed =
    TypeApplications.lambda(alias.body) match {
      case None    => callees.opening(entry, alias, application).flatMap(read(alias.body, _))
      case Some(_) => Left(s"type constructor used as a proper type: ${application.spelled}")
    }

  // A builtin or model declaration, by its canonical key, applied to its arguments' keys.
  private def builtin(canonical: String, application: Application): Keyed = {
    val args = application.args
    BuiltinTypes
      .arity(canonical)
      .flatMap(_ =>
        arguments(args, args.map(_ => Kind.Proper), application.context).map(nominal(canonical, _))
      )
  }

  // An operator's key is its η-long lambda: applied to fresh parameters, abstracted over them.
  private def operator(tpe: Type, kind: Kind.Arrow, context: ResolutionContext): Keyed = {
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
    Left(s"nominal identity of argument not established: ${Spelling.of(tree)}")

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
