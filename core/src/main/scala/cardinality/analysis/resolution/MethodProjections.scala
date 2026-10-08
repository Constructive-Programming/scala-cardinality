package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.model.*
import cardinality.types.*

import MethodAnalysis.{sequence, Frame, Resolved, TypeEntry}

/** Match types and transparent aliases share syntax-preserving projection normalization.
  * Constructor lambdas belong to application resolution, not this feature family.
  */
private[cardinality] object MethodProjections {
  private given Dialect = dialects.Scala3

  def unapply(tpe: Type): Option[Type] = tpe match {
    case _: Type.Match | _: Type.Name | _: Type.Select => Some(tpe)
    case _                                             => None
  }

  def accepts(tpe: Type, frame: Frame, resolver: Resolver): Boolean = tpe match {
    case _: Type.Match                 => true
    case _: Type.Name | _: Type.Select =>
      alias(TypeApplications.name(tpe), frame, resolver).nonEmpty
    case _ => false
  }

  def alias(name: String, frame: Frame, resolver: Resolver): Option[(TypeEntry, Defn.Type)] =
    resolver.lookup(name, frame) match {
      case List(entry) =>
        entry.tree match {
          case definition: Defn.Type
              if !definition.mods.exists(_.is[Mod.Opaque]) &&
                TypeApplications.lambda(definition.body).isEmpty =>
            Some(entry -> definition)
          case _ => None
        }
      case _ => None
    }

  def resolve(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver,
      sourceNames: Set[String]
  ): Resolved =
    new Normalizer(resolver, sourceNames)
      .syntax(tpe, context)
      .flatMap { (normalized, bindings) =>
        context.copy(variables = bindings).read(normalized, resolver)
      }
      .flatMap(SingletonIntersections.validate(_, context.useSite, resolver))

  private class Normalizer(resolver: Resolver, sourceNames: Set[String]) {
    private type Prepared = Either[String, (Type, Map[String, Resolved])]
    private var tokenNumber = 0

    private def nextToken(): String = {
      tokenNumber += 1
      s"$$cardinalityProjection$tokenNumber"
    }

    private def freshToken(variables: Map[String, Resolved]): String = {
      var token = nextToken()
      while (sourceNames(token) || variables.contains(token)) {
        token = nextToken()
      }
      token
    }

    // Keep tuple shells, but freeze all other leaves to caller-resolved Shapes. A nominal
    // product must never become a tuple merely because its stored representation is a product.
    def syntax(tpe: Type, context: ResolutionContext): Prepared =
      if (context.visiting.size >= resolver.limits.maxTypeDepth)
        Left("match type resolution budget exhausted")
      else
        tpe match {
          case Type.Tuple(parts)                        => prepareTuple(parts, context)
          case Type.Match.After_4_9_9(scrutinee, block) =>
            prepareMatch(tpe, scrutinee, block, context)
          case other => prepareReference(other, context)
        }

    private def prepareTuple(parts: List[Type], context: ResolutionContext): Prepared =
      sequence(parts.map(syntax(_, context))).map { prepared =>
        Type.Tuple(prepared.map(_._1)) -> prepared.flatMap(_._2).toMap
      }

    private def prepareMatch(
        matched: Type,
        scrutinee: Type,
        block: Type.CasesBlock,
        context: ResolutionContext
    ): Prepared =
      syntax(scrutinee, context).flatMap { (known, bindings) =>
        projectionFailure(known, block.cases) match {
          case Some(reason) => Left(reason)
          case None         =>
            reduceMatch(
              Type.Match.After_4_9_9(known, block),
              context.copy(variables = context.variables ++ bindings).enter(matched.structure)
            )
        }
      }

    // Every case must be eligible, including cases that precede the selected projection.
    private def projectionFailure(known: Type, cases: List[TypeCase]): Option[String] =
      if (!known.is[Type.Tuple])
        Some("inert match type: scrutinee is not a proven tuple; concrete substitution required")
      else if (!cases.forall(c => projectionPattern(c.pat)))
        Some("match type requires type-identity/disjointness proof for fixed or nominal patterns")
      else if (!cases.forall(c => determinateProjection(c.pat, known)))
        Some("inert match type: nested scrutinee is not a proven tuple")
      else None

    private def reduceMatch(matched: Type.Match, context: ResolutionContext): Prepared =
      MatchTypes.reduced(matched, Map.empty) match {
        case Some(body) => syntax(body, context)
        case None       =>
          Left("nonmatching tuple match type: no case matches the proven tuple")
      }

    private def prepareReference(tpe: Type, context: ResolutionContext): Prepared = {
      val (name, args) = tpe match {
        case application: Type.Apply =>
          TypeApplications.name(application.tpe) -> application.argClause.values
        case _ => TypeApplications.name(tpe) -> Nil
      }
      val definition =
        if (context.variables.contains(name)) None else alias(name, context.frame, resolver)
      definition match {
        case Some(declaration) => applyAlias(name, args, declaration, context)
        case None              => freeze(tpe, context)
      }
    }

    private def applyAlias(
        name: String,
        args: List[Type],
        declaration: (TypeEntry, Defn.Type),
        context: ResolutionContext
    ): Prepared = {
      val (entry, declared) = declaration
      val key = (entry.owner.path :+ entry.name).mkString(".")
      val parameters = declared.tparamClause.values
      if (context.visiting(key)) Left(s"recursive type requires a structural proof: $key")
      else
        TypeApplications
          .checkApplication(name, parameters, args.size)
          .flatMap { _ =>
            sequence(args.map(syntax(_, context))).flatMap { prepared =>
              prepareAliasBody(entry, declared, prepared, context.enter(key))
            }
          }
    }

    private def prepareAliasBody(
        entry: TypeEntry,
        declared: Defn.Type,
        prepared: List[(Type, Map[String, Resolved])],
        context: ResolutionContext
    ): Prepared = {
      val substitutions =
        declared.tparamClause.values
          .zip(prepared)
          .map((p, a) => TypeName.of(p.name.value) -> a._1)
          .toMap
      val bindings = resolver.typeParameters(entry.owner) ++ prepared.flatMap(_._2).toMap
      // Alias bodies use the declaration scope; only frozen arguments retain caller bindings.
      syntax(
        MatchTypes.replace(declared.body, substitutions),
        context.inScope(entry.owner, bindings)
      )
    }

    private def freeze(tpe: Type, context: ResolutionContext): Prepared = {
      val token = freshToken(context.variables)
      Right(Type.Name(token) -> Map(token -> context.read(tpe, resolver)))
    }

    private def projectionPattern(tpe: Type): Boolean = tpe match {
      case _: Type.Wildcard  => true
      case Type.Name(name)   => name.headOption.exists(_.isLower)
      case Type.Tuple(parts) => parts.forall(projectionPattern)
      case _                 => false
    }

    private def determinateProjection(pattern: Type, known: Type): Boolean = pattern match {
      case Type.Tuple(elements) =>
        known match {
          case Type.Tuple(parts) =>
            elements.size != parts.size ||
            elements.zip(parts).forall((element, part) => determinateProjection(element, part))
          case _ => false
        }
      case _ => true
    }

  }

}
