package cardinality

import scala.meta.*
import MethodAnalysis.{Frame, Resolved, TypeEntry, sequence}

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
      frame: Frame,
      variables: Map[String, Resolved],
      resolver: Resolver,
      visiting: Set[String],
      sourceNames: Set[String]
  ): Resolved =
    new Normalizer(resolver, sourceNames)
      .syntax(tpe, frame, variables, visiting)
      .flatMap { (normalized, bindings) => resolver.resolve(normalized, frame, bindings, visiting) }
      .flatMap(SingletonIntersections.validate(_, frame, resolver))

  private class Normalizer(resolver: Resolver, sourceNames: Set[String]) {
    private type Prepared = Either[String, (Type, Map[String, Resolved])]
    private var tokenNumber = 0

    private def freshToken(variables: Map[String, Resolved]): String = {
      tokenNumber += 1
      var token = s"$$cardinalityProjection$tokenNumber"
      while (sourceNames(token) || variables.contains(token)) {
        tokenNumber += 1
        token = s"$$cardinalityProjection$tokenNumber"
      }
      token
    }

    // Keep tuple shells, but freeze all other leaves to caller-resolved Shapes. A nominal
    // product must never become a tuple merely because its stored representation is a product.
    def syntax(
        tpe: Type,
        frame: Frame,
        variables: Map[String, Resolved],
        visiting: Set[String]
    ): Prepared =
      if (visiting.size >= resolver.limits.maxTypeDepth)
        Left("match type resolution budget exhausted")
      else
        tpe match {
          case Type.Tuple(parts) =>
            sequence(parts.map(syntax(_, frame, variables, visiting))).map { prepared =>
              Type.Tuple(prepared.map(_._1)) -> prepared.flatMap(_._2).toMap
            }
          case Type.Match(scrutinee, cases) =>
            syntax(scrutinee, frame, variables, visiting).flatMap { (known, bindings) =>
              if (!known.is[Type.Tuple])
                Left(
                  "inert match type: scrutinee is not a proven tuple; concrete substitution required"
                )
              else if (!cases.forall(c => projectionPattern(c.pat)))
                Left(
                  "match type requires type-identity/disjointness proof for fixed or nominal patterns"
                )
              else if (!cases.forall(c => determinateProjection(c.pat, known)))
                Left("inert match type: nested scrutinee is not a proven tuple")
              else
                MatchTypes.reduced(Type.Match(known, cases), Map.empty) match {
                  case Some(body) =>
                    syntax(body, frame, variables ++ bindings, visiting + tpe.structure)
                  case None =>
                    Left("nonmatching tuple match type: no case matches the proven tuple")
                }
            }
          case other =>
            val (name, args) = other match {
              case application: Type.Apply =>
                TypeApplications.name(application.tpe) -> application.argClause.values
              case _ => TypeApplications.name(other) -> Nil
            }
            val definition = if (variables.contains(name)) None else alias(name, frame, resolver)
            definition match {
              case Some((entry, declared)) =>
                val key = (entry.owner.path :+ entry.name).mkString(".")
                val parameters = declared.tparamClause.values
                if (visiting(key)) Left(s"recursive type requires a structural proof: $key")
                else if (parameters.exists(TypeApplications.constrained))
                  Left(s"constrained type constructor: $name")
                else if (parameters.size != args.size) Left(s"type argument arity: $name")
                else
                  sequence(args.map(syntax(_, frame, variables, visiting))).flatMap { prepared =>
                    val substitutions =
                      parameters
                        .zip(prepared)
                        .map((p, a) => TypeName.of(p.name.value) -> a._1)
                        .toMap
                    val bindings =
                      resolver.typeParameters(entry.owner) ++ prepared.flatMap(_._2).toMap
                    syntax(
                      MatchTypes.replace(declared.body, substitutions),
                      entry.owner,
                      bindings,
                      visiting + key
                    )
                  }
              case None =>
                val token = freshToken(variables)
                Right(
                  Type.Name(token) -> Map(
                    token -> resolver.resolve(other, frame, variables, visiting)
                  )
                )
            }
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
