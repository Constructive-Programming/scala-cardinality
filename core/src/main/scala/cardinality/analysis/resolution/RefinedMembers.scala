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

  def resolve(
      qualifier: Type,
      member: String,
      context: ResolutionContext,
      resolver: Resolver
  ): Resolved = new Reader(resolver).member(qualifier, member, context)

  def pathType(path: Term.Ref, frame: Frame): Option[Either[String, (Type, Frame)]] = path match {
    case name: Term.Name =>
      frame.chain.iterator.map(declaredPath(name.value, _)).find(_.nonEmpty).flatten
    case _ => None
  }

  private def declaredPath(name: String, owner: Frame): Option[Either[String, (Type, Frame)]] = {
    val parameter = owner.params.find(_.name.value == name).map(parameterType)
    val binding = owner.stats.collectFirst(Function.unlift(bindingType(name, _)))
    parameter.orElse(binding).map {
      case Some(tpe) => Right(tpe -> owner)
      case None      => Left(s"unavailable stable path type: $name")
    }
  }

  private def parameterType(parameter: Term.Param): Option[Type] =
    if (parameter.mods.exists(_.is[Mod.VarParam])) None else parameter.decltpe

  private def binds(name: String, patterns: List[Pat]): Boolean =
    patterns.exists {
      case Pat.Var(bound) => bound.value == name
      case _              => false
    }

  private def bindingType(name: String, stat: Stat): Option[Option[Type]] = stat match {
    case d: Defn.Val if binds(name, d.pats)  => Some(d.decltpe)
    case d: Decl.Val if binds(name, d.pats)  => Some(Some(d.decltpe))
    case d: Defn.Var if binds(name, d.pats)  => Some(None)
    case d: Decl.Var if binds(name, d.pats)  => Some(None)
    case d: Defn.Def if d.name.value == name => Some(None)
    case d: Decl.Def if d.name.value == name => Some(None)
    case _                                   => None
  }

  private case class Definition(
      owner: Frame,
      parameters: List[Type.Param],
      body: Option[Type],
      stats: List[Stat]
  )

  private def definition(entry: TypeEntry): Definition = entry.tree match {
    case alias: Defn.Type if !alias.mods.exists(_.is[Mod.Opaque]) =>
      Definition(entry.owner, alias.tparamClause.values, Some(alias.body), Nil)
    case traitDef: Defn.Trait =>
      Definition(entry.owner, traitDef.tparamClause.values, None, traitDef.templ.body.stats)
    case classDef: Defn.Class =>
      Definition(entry.owner, classDef.tparamClause.values, None, classDef.templ.body.stats)
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

  private class Reader(resolver: Resolver) {
    private case class Request(name: String, arguments: List[Type], member: String)

    def member(qualifier: Type, name: String, context: ResolutionContext): Resolved = {
      val key = s"member:${context.frame.id}:${qualifier.syntax}#$name"
      if (context.visiting(key)) Left(s"recursive refined member: ${qualifier.syntax}#$name")
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
        case _ => Left(s"unavailable refined member: ${qualifier.syntax}#$name")
      }

    private def refinement(
        refined: Type.Refine,
        name: String,
        context: ResolutionContext
    ): Resolved =
      if (declarations(refined.body.stats).contains(name))
        fromStats(refined.body.stats, name, context)
      else
        refined.tpe.fold[Resolved](Left(s"unavailable refined member: $name"))(
          member(_, name, context)
        )

    private def fromStats(stats: List[Stat], name: String, context: ResolutionContext): Resolved = {
      val declared = declarations(stats)
      val key = s"refinement:${context.frame.id}:${stats.map(_.syntax).mkString(";")}:$name"
      if (context.visiting(key)) Left(s"recursive refined member: $name")
      else {
        val scoped = context.enter(key)
        memberBody(declared, name, scoped).flatMap { body =>
          val referenced = body.collect {
            case n: Type.Name if declared.contains(n.value) => n.value
          }.distinct
          val substitutions =
            referenced.map(member => member -> fromStats(stats, member, scoped)).toMap
          scoped.copy(variables = scoped.variables ++ substitutions).read(body, resolver)
        }
      }
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
      case _                                                      =>
        Left(
          s"abstract refined member ${declaration.name.value} has non-exact bounds: ${declaration.syntax}"
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
        defined.body match {
          case Some(body) => member(body, request.member, scoped)
          case None       => fromStats(defined.stats, request.member, scoped)
        }
      }

    private def instantiate(
        name: String,
        defined: Definition,
        arguments: List[Type],
        context: ResolutionContext
    ): Either[String, ResolutionContext] =
      if (defined.parameters.exists(TypeApplications.constrained))
        Left(s"constrained refined type constructor: $name")
      else if (defined.parameters.size != arguments.size) Left(s"type argument arity: $name")
      else {
        val replacements = defined.parameters
          .zip(arguments)
          .map { (parameter, argument) =>
            parameter.name.value -> context.read(argument, resolver)
          }
          .toMap
        Right(
          context.inScope(defined.owner, resolver.typeParameters(defined.owner) ++ replacements)
        )
      }

  }

}
