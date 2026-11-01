package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.{dialects, Decl, Defn, Dialect, Tree, Type, XtensionSyntax}

import cardinality.analysis.MethodAnalysis.{sequence, Frame, TypeEntry}
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.inhabitation.{EvidenceAnalysis, ExistentialInputs}
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{BuiltinTypes, Resolver}
import cardinality.capacity.Counter
import cardinality.types.Kinds

/** A subtype bound on a proper binder is an ambient proof (hkt R5): `A <: U` is `A <:< U`, `A >: L`
  * is `L <:< A`, always available and never a value — in the body, `summon[A <:< U]` compiles, and
  * a proof's only behaviour is the upcast (`liftCo` / `substituteCo` exist only for covariant
  * constructors, and an invariant rigid application is never lifted).
  *
  * Phase B1: a bound to a binder or a rigid application is a directed view between nominal
  * endpoints (`EvidenceAnalysis`; a cycle of them is an equality). Phase B2: a bound to a closed
  * structural type — a sealed family, a case or final class, an enum, a builtin — views the
  * binder's values as the bound for elimination only (upper), or builds the binder's goal as the
  * bound (lower). `A <: Any` and `A >: Nothing` say nothing; `AnyRef`, `AnyVal` and `Matchable`
  * above say nothing of the binder's values, but a structural lower bound below them must be a
  * class of that kind (`BoundChecks`). Each is that builtin only where the shape reading would read
  * it so: a channel that may rebind the name (an unsupplied wildcard for a top name) leaves a bound
  * to read, which then refuses with the channel's reason. A bound to an open trait needs that
  * trait's members as capabilities; any other unreadable bound keeps its own reason. A bound is
  * read only for a binder the environment mentions, and for the binders its bounds mention in turn.
  */
final private[cardinality] class BinderBounds(resolver: Resolver, observer: Frame) {
  import BinderBounds.*

  private given Dialect = dialects.Scala3

  /** Why a clause of the observer's scopes is no Scala program, if one is not: the signature is
    * then no program either, whatever its result and whether or not the clause's binders are read.
    */
  lazy val invalid: List[String] = observer.chain.flatMap(frame => Kinds.invalid(frame.typeParams))

  /** The proofs of every bounded binder the shapes mention, and why a bound could not be read. An
    * invalid clause is reported alone.
    */
  def of(shapes: List[Shape]): (List[Shape], List[String]) =
    if (invalid.isEmpty) proofs(shapes) else (Nil, invalid)

  private def proofs(shapes: List[Shape]): (List[Shape], List[String]) = {
    val read = collect(mentioned(shapes), Set.empty, Nil)
    val declared = read.collect { case (id, Right(bounds)) => id -> bounds }
    val refused = BoundChecks.refusals(declared, BoundChecks.available(shapes))
    val proofs =
      declared.flatMap((id, bounds) => if (refused.contains(id)) Nil else bounds.proofs(id))
    val reasons = read.flatMap {
      case (_, Left(reason)) => List(reason)
      case (id, Right(_))    => refused.get(id).toList
    }
    (proofs, reasons)
  }

  @tailrec private def collect(
      pending: List[String],
      seen: Set[String],
      read: List[(String, Either[String, Declared])]
  ): List[(String, Either[String, Declared])] =
    pending match {
      case Nil                    => read
      case id :: rest if seen(id) => collect(rest, seen, read)
      case id :: rest             =>
        resolver.boundedBinders.get(id) match {
          case Some((scope, parameter)) =>
            val bounds = this.read(scope, parameter)
            val next = bounds.toOption.toList.flatMap(declared => mentioned(declared.shapes))
            collect(rest ++ next, seen + id, read :+ (id -> bounds))
          case None => collect(rest, seen + id, read)
        }
    }

  private def mentioned(shapes: List[Shape]): List[String] =
    shapes.flatMap(ExistentialInputs.freeAtoms).distinct.filter(resolver.boundedBinders.contains)

  private def read(scope: Frame, parameter: Type.Param): Either[String, Declared] = {
    val context =
      ResolutionContext(scope, resolver.typeParameters(scope), Set.empty, Some(observer))
    val upper = parameter.bounds.hi.filterNot(TopTypes.isAny(_, context, resolver)).map { bound =>
      classBound(bound, context) match {
        case Some(name) => Right(Bound.Class(name))
        case None       => read(bound, context, parameter, upper = true).map(Bound.Upper(_))
      }
    }
    val lower =
      parameter.bounds.lo.filterNot(TopTypes.isNothing(_, context, resolver)).map { bound =>
        read(bound, context, parameter, upper = false).map(
          Bound.Lower(_, instanceClass(bound, context))
        )
      }
    sequence(upper.toList ++ lower.toList).map(Declared(parameter, _))
  }

  private def read(
      bound: Type,
      context: ResolutionContext,
      parameter: Type.Param,
      upper: Boolean
  ): Either[String, Shape] =
    resolver.resolve(bound, context) match {
      case Right(shape) if EvidenceAnalysis.endpoint(shape) || closed(shape) => Right(shape)
      case Right(_)                                                          =>
        Left(s"bound to a structural type requires coercion analysis: ${parameter.syntax}")
      case Left(_) if upper && membersOnly(bound, context) =>
        Left(s"upper bound requires capability analysis: ${parameter.syntax}")
      case Left(reason) => Left(reason)
    }

  // A bound declared without a sealed structure — an open trait or class, an abstract member — is
  // known by its members alone: its values are read through them, as capabilities.
  private def membersOnly(bound: Type, context: ResolutionContext): Boolean =
    declaration(head(bound), context).exists(entry => open(entry.tree))

  private def open(declaration: Tree): Boolean = declaration match {
    case t: Defn.Trait => !Counter.isSealed(t)
    case c: Defn.Class => !Counter.isSealed(c)
    case _: Decl.Type  => true
    case _             => false
  }

  // The class a structural lower bound's values are instances of, where its declarations fix it:
  // `AnyVal` for a value class or a value builtin; `AnyRef` for any other class, an enum, a trait
  // declared without parents (its superclass is `Object`) or with a parent that is an `AnyRef`, a
  // tuple, a function or a reference builtin. A trait whose parents fix no class may be universal
  // (`extends Any`): not read.
  private def instanceClass(bound: Type, context: ResolutionContext): Option[String] =
    bound match {
      case _: Type.Tuple | _: Type.Function => Some("AnyRef")
      case _                                => named(head(bound), context, Set.empty)
    }

  private def named(tpe: Type, context: ResolutionContext, seen: Set[String]): Option[String] =
    declaration(tpe, context) match {
      case Some(entry) if seen(entry.key) => None
      case Some(entry)                    =>
        entry.tree match {
          case c: Defn.Class =>
            val value =
              c.templ.inits.headOption.exists(_.tpe.syntax.stripPrefix("scala.") == "AnyVal")
            Some(if (value) "AnyVal" else "AnyRef")
          case t: Defn.Trait if t.templ.inits.isEmpty => Some("AnyRef")
          case t: Defn.Trait                          =>
            val scope =
              ResolutionContext(entry.owner, resolver.typeParameters(entry.owner), Set.empty)
            val parents = t.templ.inits.map(init => named(head(init.tpe), scope, seen + entry.key))
            Option.when(parents.contains(Some("AnyRef")))("AnyRef")
          case _: Defn.Enum => Some("AnyRef")
          case _            => None
        }
      // A name no declaration takes is the builtin it spells, if it spells one.
      case None =>
        Option
          .when(Found.named(resolver, tpe, tpe.syntax, context).contains(Found.Absent))(tpe.syntax)
          .flatMap(BuiltinTypes.canonical)
          .flatMap(builtinClass)
    }

  private def declaration(named: Type, context: ResolutionContext): Option[TypeEntry] =
    Found.named(resolver, named, named.syntax, context).collect { case Found.Entry(entry) => entry }

  // `AnyRef` (or `Object`), `AnyVal` or `Matchable` above, when the name is the builtin's where it
  // is read: past every channel that may rebind a top name (`TopTypes.denoted`), as `Any` above
  // and `Nothing` below are.
  private def classBound(bound: Type, context: ResolutionContext): Option[String] =
    TopTypes.denoted(bound, context, resolver).filterNot(_ == TopTypes.Kind.Universal).map(_.label)

}

private[cardinality] object BinderBounds {

  /** One binder's bounds as read, with its declaration for the reasons that name it. */
  final case class Declared(parameter: Type.Param, bounds: List[Bound]) {
    def proofs(id: String): List[Shape] = bounds.flatMap(_.proof(Shape.Atom(id)))
    def shapes: List[Shape] = bounds.flatMap(_.shape)
  }

  enum Bound {
    case Upper(bound: Shape)
    // A lower bound, with the class its values are instances of where the declaration fixes it.
    case Lower(bound: Shape, instanceClass: Option[String])
    // `AnyRef`, `AnyVal` or `Matchable` above: no proof about the binder's values.
    case Class(name: String)

    def proof(atom: Shape.Atom): Option[Shape] = this match {
      case Upper(bound)    => Some(Shape.Evidence(atom, bound, false))
      case Lower(bound, _) => Some(Shape.Evidence(bound, atom, false))
      case Class(_)        => None
    }

    def shape: Option[Shape] = this match {
      case Upper(bound)    => Some(bound)
      case Lower(bound, _) => Some(bound)
      case Class(_)        => None
    }

  }

  // The class a builtin's values are instances of.
  private def builtinClass(canonical: String): Option[String] = canonical match {
    case "Unit" | "Boolean" | "Byte" | "Short" | "Char" | "Int" | "Long" | "Float" | "Double" =>
      Some("AnyVal")
    case "String" | "Option" | "Either" | "AnyRef"                          => Some("AnyRef")
    case other if other.startsWith("Tuple") || other.startsWith("Function") => Some("AnyRef")
    case _                                                                  => None
  }

  // An applied bound's constructor.
  private def head(bound: Type): Type = bound match {
    case applied: Type.Apply => applied.tpe
    case other               => other
  }

  // A structural bound whose values the solver can eliminate and build: no path, proof, package or
  // constructor inside it.
  private def closed(shape: Shape): Boolean =
    !Shape.exists(shape) {
      case _: Shape.Singleton | _: Shape.Evidence | _: Shape.Existential => true
      case _: Shape.Constructor | _: Shape.Repeated                      => true
      case _                                                             => false
    }

}
