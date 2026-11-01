package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.*
import cardinality.types.*

import MethodAnalysis.{Frame, Resolved, TypeEntry}

/** Member equations are resolved in their declaration scope; use-site accessibility is checked by
  * the caller. Bounds are not erased into guessed representations.
  */
private[cardinality] object RefinedMembers {
  private given Dialect = dialects.Scala3

  /** A member of a qualifier type. Read as a type projection `T#N` (`projection`), an unbounded
    * abstract member of `T` denotes no type: each value of `T` has its own (`∃p. p.N`), and no
    * stable path picks one.
    */
  def resolve(
      qualifier: Type,
      member: String,
      context: ResolutionContext,
      resolver: Resolver,
      projection: Boolean = false
  ): Resolved =
    new Reader(resolver, Option.when(projection)(s"${Spelling.of(qualifier)}#$member"))
      .member(qualifier, member, context)

  def pathType(path: Term.Ref, frame: Frame): Option[Either[String, (Type, Frame)]] = path match {
    case name: Term.Name =>
      frame.chain.iterator.map(declaredPath(name.value, _)).find(_.nonEmpty).flatten
    case _ => None
  }

  /** The innermost scope of a frame's chain that binds a term name as a parameter or value. */
  def declaring(name: String, frame: Frame): Option[Frame] =
    frame.chain.find(declaresValue(name, _))

  /** Whether a scope binds a term name as a parameter or value. */
  def declaresValue(name: String, owner: Frame): Boolean = declaredPath(name, owner).nonEmpty

  /** The type a scope declares a term name with, when it binds the name: why it is no stable path
    * otherwise.
    */
  def declaredValue(name: String, owner: Frame): Option[Either[String, Type]] =
    declaredPath(name, owner).map(_.map(_._1))

  private def declaredPath(name: String, owner: Frame): Option[Either[String, (Type, Frame)]] = {
    val parameter = owner.params.find(_.name.value == name).map(parameterType)
    val binding = owner.stats.collectFirst(Function.unlift(bindingType(name, _)))
    parameter.orElse(binding).map {
      case Some(tpe) => Right(tpe -> owner)
      case None      => Left(s"unavailable stable path type: $name")
    }
  }

  // A `var` or by-name parameter is no stable path: each read may denote another value.
  private def parameterType(parameter: Term.Param): Option[Type] =
    if (parameter.mods.exists(_.is[Mod.VarParam]) || parameter.decltpe.exists(_.is[Type.ByName]))
      None
    else parameter.decltpe

  private def bindingType(name: String, stat: Stat): Option[Option[Type]] = stat match {
    case d: Defn.Val if TypeApplications.binds(name, d.pats) => Some(d.decltpe)
    case d: Decl.Val if TypeApplications.binds(name, d.pats) => Some(Some(d.decltpe))
    case d: Defn.Var if TypeApplications.binds(name, d.pats) => Some(None)
    case d: Decl.Var if TypeApplications.binds(name, d.pats) => Some(None)
    case d: Defn.Def if d.name.value == name                 => Some(None)
    case d: Decl.Def if d.name.value == name                 => Some(None)
    case _                                                   => None
  }

  private case class Definition(
      owner: Frame,
      parameters: List[Type.Param],
      body: Option[Type],
      stats: List[Stat],
      template: Boolean = false
  )

  private def definition(entry: TypeEntry): Definition = entry.tree match {
    case alias: Defn.Type if !alias.mods.exists(_.is[Mod.Opaque]) =>
      Definition(entry.owner, alias.tparamClause.values, Some(alias.body), Nil)
    case d: (Defn.Trait | Defn.Class) =>
      Definition(entry.owner, d.tparamClause.values, None, d.templ.body.stats, template = true)
    case _ => Definition(entry.owner, Nil, None, Nil)
  }

  private def declarations(stats: List[Stat]): Map[String, Stat] =
    stats.collect {
      case d: Defn.Type => d.name.value -> d
      case d: Decl.Type => d.name.value -> d
    }.toMap

  private def hidden(declaration: Stat.WithMods): Boolean =
    declaration.mods.exists {
      case _: Mod.Opaque | _: Mod.Private | _: Mod.Protected => true
      case _                                                 => false
    }

  // `projected` spells the projection being read, when the qualifier is a type rather than a path.
  private class Reader(resolver: Resolver, projected: Option[String]) {

    private case class Request(name: String, arguments: List[Type], member: String) {

      /** The type the request names, applied as written. */
      def tpe: Type = {
        val named = dialects.Scala3(name).parse[Type].getOrElse(Type.Name(name))
        if (arguments.isEmpty) named else Type.Apply(named, Type.ArgClause(arguments))
      }

    }

    def member(qualifier: Type, name: String, context: ResolutionContext): Resolved = {
      val key = s"member:${context.frame.id}:${qualifier.syntax}#$name"
      if (context.visiting(key)) Left(s"recursive refined member: ${Spelling.of(qualifier)}#$name")
      else if (context.visiting.size >= resolver.limits.maxTypeDepth)
        Left("type resolution budget exhausted")
      else readQualifier(qualifier, name, context.enter(key))
    }

    private def readQualifier(qualifier: Type, name: String, context: ResolutionContext): Resolved =
      qualifier match {
        case refined: Type.Refine => refinement(refined, name, context)
        case applied: Type.Apply  =>
          source(applied.tpe.syntax.stripPrefix("_root_."), applied.argClause.values, name, context)
        case n: Type.Name          => source(n.value, Nil, name, context)
        case selected: Type.Select =>
          source(selected.syntax.stripPrefix("_root_."), Nil, name, context)
        case _ => Left(s"unavailable refined member: ${Spelling.of(qualifier)}#$name")
      }

    private def refinement(
        refined: Type.Refine,
        name: String,
        context: ResolutionContext
    ): Resolved =
      if (declarations(refined.body.stats).contains(name))
        fromStats(refined.body.stats, name, context, refined.tpe.map(_ -> context))
      else
        refined.tpe.fold[Resolved](Left(s"unavailable refined member: $name"))(
          member(_, name, context)
        )

    private def fromStats(
        stats: List[Stat],
        name: String,
        context: ResolutionContext,
        owner: Option[(Type, ResolutionContext)]
    ): Resolved = {
      val declared = declarations(stats)
      val key = s"refinement:${context.frame.id}:${stats.map(_.syntax).mkString(";")}:$name"
      if (context.visiting(key)) Left(s"recursive refined member: $name")
      else {
        val scoped = context.enter(key)
        memberBody(declared, name, scoped)
          .flatMap { body =>
            val known = (n: String) => declared.contains(n) || scoped.variables.contains(n)
            ownMembers(name, body, owner, known).toLeft(body)
          }
          .flatMap { body =>
            val referenced = TypeSubstitution.typeNames(body).filter(declared.contains).distinct
            val substitutions =
              referenced.map(member => member -> fromStats(stats, member, scoped, owner)).toMap
            scoped.bind(substitutions).read(body, resolver)
          }
      }
    }

    // A member body read in a projection, outside its template: `this` there, and a name its owner
    // declares or inherits, mean the owner's own members, which no stable path reaches.
    private def ownMembers(
        name: String,
        body: Type,
        owner: Option[(Type, ResolutionContext)],
        known: String => Boolean
    ): Option[String] =
      if (body.collect { case _: Term.This => () }.nonEmpty)
        Some(s"refined member $name refers to its own value: ${Spelling.of(body)}")
      else
        owner.flatMap { (tpe, scope) =>
          TypeSubstitution
            .typeNames(body)
            .filterNot(known)
            .find(resolver.declaresMember(tpe, _, scope))
            .map(inherited => s"refined member $name refers to its owner's member $inherited")
        }

    private def memberBody(
        declared: Map[String, Stat],
        name: String,
        context: ResolutionContext
    ): Either[String, Type] = declared.get(name) match {
      case Some(d: Stat.WithMods) if hidden(d) =>
        Left(s"non-public or opaque refined member: $name")
      case Some(d: Defn.Type) if d.tparamClause.values.isEmpty => Right(d.body)
      case Some(d: Decl.Type) if d.tparamClause.values.isEmpty => exactBound(d, declared, context)
      case Some(_) => Left(s"higher-kinded refined member: $name")
      case None    => Left(s"unavailable refined member: $name")
    }

    private def exactBound(
        declaration: Decl.Type,
        declared: Map[String, Stat],
        context: ResolutionContext
    ): Either[String, Type] = (declaration.bounds.lo, declaration.bounds.hi) match {
      case (Some(lo), Some(hi)) if lo.structure == hi.structure   => Right(lo)
      case (None, Some(hi)) if bottomBound(hi, declared, context) => Right(hi)
      case (None, None) if projected.nonEmpty                     =>
        Left(s"type projection of abstract member ${projected.mkString}: no stable path")
      case _ =>
        Left(
          s"abstract refined member has non-exact bounds: ${Spelling.of(declaration)}"
        )
    }

    private def bottomBound(
        bound: Type,
        declared: Map[String, Stat],
        context: ResolutionContext
    ): Boolean =
      if (declared.contains(bound.syntax)) false
      else context.read(bound, resolver) == Right(Inhabitation.Shape.Sum(Nil))

    private def source(
        name: String,
        arguments: List[Type],
        memberName: String,
        context: ResolutionContext
    ): Resolved =
      if (context.variables.contains(name))
        Left(s"unavailable member on type parameter: $name#$memberName")
      else
        resolver.lookup(name, context.frame) match {
          case List(entry) =>
            applyDefinition(definition(entry), Request(name, arguments, memberName), context)
          case Nil => Left(s"unavailable refined member: $name#$memberName")
          case _   => Left(s"ambiguous type: $name")
        }

    private def applyDefinition(
        defined: Definition,
        request: Request,
        context: ResolutionContext
    ): Resolved =
      instantiate(request.name, defined, request.arguments, context).flatMap { scoped =>
        val template = Option.when(defined.template)(request.tpe -> context)
        defined.body match {
          case Some(body) => member(body, request.member, scoped)
          case None       => fromStats(defined.stats, request.member, scoped, template)
        }
      }

    private def instantiate(
        name: String,
        defined: Definition,
        arguments: List[Type],
        context: ResolutionContext
    ): Either[String, ResolutionContext] =
      TypeApplications
        .checkApplication(name, defined.parameters, arguments.size, "refined type constructor")
        .map { _ =>
          val replacements = defined.parameters
            .zip(arguments)
            .map { (parameter, argument) =>
              parameter.name.value -> resolver.operators.argument(
                Kinds.of(parameter),
                argument,
                context
              )
            }
            .toMap
          context.inScope(defined.owner, resolver.typeParameters(defined.owner) ++ replacements)
        }

  }

}
