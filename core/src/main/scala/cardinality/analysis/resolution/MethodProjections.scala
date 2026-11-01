package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.*
import cardinality.types.*

import MethodAnalysis.{sequence, Resolved, TypeEntry}

/** Match types and transparent aliases share syntax-preserving projection normalization.
  * Constructor lambdas belong to application resolution, not this feature family.
  */
private[cardinality] object MethodProjections {
  private given Dialect = dialects.Scala3

  def unapply(tpe: Type): Option[Type] = tpe match {
    case _: Type.Match | _: Type.Name | _: Type.Select => Some(tpe)
    case _                                             => None
  }

  def accepts(tpe: Type, context: ResolutionContext, resolver: Resolver): Boolean = tpe match {
    case _: Type.Match                 => true
    case _: Type.Name | _: Type.Select => alias(tpe, context, resolver).nonEmpty
    case _                             => false
  }

  /** The transparent alias a constructor names at its own position: imports and the precedence of
    * its scope decide, as for any other reference. An alias with a higher-kinded parameter is read
    * by kind (`ConstructorApplications`), not by this normalizer, which freezes every argument as a
    * type.
    */
  def alias(
      callee: Type,
      context: ResolutionContext,
      resolver: Resolver
  ): Option[(TypeEntry, Defn.Type)] =
    resolver.declaration(callee, TypeApplications.spelled(callee), context) match {
      case Found.Entry(entry) =>
        ResolutionOptics.entryTypeDefinition
          .getOption(entry)
          .filter(definition =>
            !definition.mods.exists(_.is[Mod.Opaque]) &&
              TypeApplications.lambda(definition.body).isEmpty &&
              definition.tparamClause.values.forall(Kinds.of(_) == Kind.Proper)
          )
          .map(entry -> _)
      case _ => None
    }

  def resolve(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver,
      sourceNames: Set[String]
  ): Resolved = {
    val normalizer = new Normalizer(resolver, sourceNames)
    normalizer
      .syntax(tpe, context)
      .flatMap { (normalized, bindings) =>
        ResolutionOptics.variablesLens
          .replace(bindings)(context)
          .copy(arguments = Map.empty)
          .read(normalized, resolver)
      }
      .flatMap(SingletonIntersections.validate(_, context.useSite, resolver))
      .left
      .map(normalizer.spelled)
  }

  private val Placeholder = "$cardinalityProjection"

  // A whole placeholder name: a source name that merely starts or ends with one's text
  // (`` `$cardinalityProjection1a` ``) is not one.
  private val PlaceholderToken = {
    val quoted = java.util.regex.Pattern.quote(Placeholder)
    ("(?<![\\p{L}\\p{N}_$])" + quoted + "\\d+(?![\\p{L}\\p{N}_$])").r
  }

  // A match type as written, and whether its alias bounds it (a bounded one never stands stuck).
  private case class MatchType(
      whole: Type,
      scrutinee: Type,
      block: Type.CasesBlock,
      bounded: Boolean
  )

  private class Normalizer(resolver: Resolver, sourceNames: Set[String]) {
    private type Prepared = Either[String, (Type, Map[String, Resolved])]
    private var tokenNumber = 0
    private var spellings = Map.empty[String, String]

    private def freshToken(variables: Map[String, Resolved]): String = {
      val (token, next) =
        inhabitation.Inhabitation.Shape.freshName(Placeholder, tokenNumber + 1)(t =>
          sourceNames(t) || variables.contains(t)
        )
      tokenNumber = next - 1
      token
    }

    /** A reason in the reader's words: each frozen leaf a reason quotes is a placeholder only this
      * normalizer knows, so it is spelled back as the type the source wrote there. A literal type's
      * text is what the source wrote, whatever it looks like (`"$cardinalityProjection1"`).
      */
    def spelled(reason: String): String =
      Spelling.outsideLiterals(reason, PlaceholderToken)(found => spellings.getOrElse(found, found))

    // Keep tuple shells, but freeze all other leaves to caller-resolved Shapes. A nominal
    // product must never become a tuple merely because its stored representation is a product.
    def syntax(tpe: Type, context: ResolutionContext): Prepared =
      if (context.visiting.size >= resolver.limits.maxTypeDepth)
        Left("match type resolution budget exhausted")
      else
        tpe match {
          case Type.Tuple(parts)                        => prepareTuple(parts, context)
          case Type.Match.After_4_9_9(scrutinee, block) =>
            prepareMatch(MatchType(tpe, scrutinee, block, bounded = false), context)
          case other => prepareReference(other, context)
        }

    private def prepareTuple(parts: List[Type], context: ResolutionContext): Prepared =
      sequence(parts.map(syntax(_, context))).map { prepared =>
        Type.Tuple(prepared.map(_._1)) -> prepared.flatMap(_._2).toMap
      }

    // A first case that matches everything reduces whatever the scrutinee is: no earlier case needs
    // a disjointness proof (members MT2). A single projection case on a rigid scrutinee is stuck,
    // and stands as its projection atom (MT3) unless the match type is bounded.
    private def prepareMatch(matched: MatchType, context: ResolutionContext): Prepared = {
      val entered = context.enter(matched.whole.structure)
      matched.block.cases.headOption.filter(c =>
        c.pat.is[Type.Wildcard] || c.pat.is[Type.PatWildcard]
      ) match {
        case Some(irrefutable) => syntax(irrefutable.body, entered)
        case None              =>
          syntax(matched.scrutinee, context).flatMap { (known, bindings) =>
            reduceOrStick(known, bindings, matched, entered)
          }
      }
    }

    // A scrutinee known to be a tuple reduces; a rigid one under a single projection is stuck.
    private def reduceOrStick(
        known: Type,
        bindings: Map[String, Resolved],
        matched: MatchType,
        context: ResolutionContext
    ): Prepared =
      stuck(known, bindings, matched).getOrElse {
        projectionFailure(known, matched.block.cases)
          .toLeft(())
          .flatMap(_ =>
            reduceMatch(Type.Match.After_4_9_9(known, matched.block), context.bind(bindings))
          )
      }

    private def stuck(
        known: Type,
        bindings: Map[String, Resolved],
        matched: MatchType
    ): Option[Prepared] =
      (known, StuckProjections.projection(matched.block.cases)) match {
        case (Type.Name(token), Some((arity, index))) =>
          bindings.get(token).collect {
            case Right(Shape.Atom(id))
                if StuckProjections.instantiable(id, resolver.boundedBinders.contains) =>
              if (matched.bounded)
                Left(
                  s"inert match type: a bounded match type stays stuck on ${SourceBindings.label(id)}"
                )
              else
                frozen(
                  Right(Shape.Atom(StuckProjections.Projection(arity, index, id))),
                  bindings,
                  Spelling.of(matched.whole)
                )
          }
        case _ => None
      }

    // Every case must be eligible, including cases that precede the selected projection.
    private def projectionFailure(known: Type, cases: List[TypeCase]): Option[String] =
      if (!known.is[Type.Tuple])
        Some("inert match type: scrutinee is not a proven tuple (concrete substitution required)")
      else if (!cases.forall(c => MatchTypes.projectionPattern(c.pat)))
        Some("match type requires type-identity/disjointness proof for fixed or nominal patterns")
      else if (!cases.forall(c => MatchTypes.determinateHylo((c.pat, known))))
        Some("inert match type: nested scrutinee is not a proven tuple")
      else None

    private def reduceMatch(matched: Type.Match, context: ResolutionContext): Prepared =
      MatchTypes.reduced(matched, Map.empty) match {
        case Some(body) => syntax(body, context)
        case None       =>
          Left("nonmatching tuple match type: no case matches the proven tuple")
      }

    private def prepareReference(tpe: Type, context: ResolutionContext): Prepared = {
      val (callee, args) = tpe match {
        case application: Type.Apply => application.tpe -> application.argClause.values
        case _                       => tpe -> Nil
      }
      val name = TypeApplications.name(callee)
      (args, context.arguments.get(name)) match {
        case (Nil, Some(written)) if context.variables.contains(name) => rewritten(name, written)
        case _                                                        =>
          member(callee, args, context).getOrElse {
            // An inherited member is read where it is declared, by the ordinary resolution.
            val definition =
              if (context.variables.contains(name) || resolver.inheritsName(tpe, name, context))
                None
              else alias(callee, context, resolver)
            definition match {
              case Some(declaration) => applyAlias(name, args, declaration, context)
              case None              => freeze(tpe, context)
            }
          }
      }
    }

    // A binder an application bound is read again from the argument as written, in the scope it was
    // written in: a tuple argument stays a tuple a projection can reduce (members MT1').
    private def rewritten(name: String, written: (Type, ResolutionContext)): Prepared = {
      val (argument, scope) = written
      val key = s"argument:$name:${argument.structure}"
      if (scope.visiting(key)) Left(s"recursive type argument: $name")
      else syntax(argument, scope.enter(key))
    }

    // A type member's alias is normalized where it is declared, with `this` its path's; an abstract
    // member is its atom, frozen like any other leaf.
    private def member(callee: Type, args: List[Type], context: ResolutionContext) =
      resolver.unappliedMember(callee, args, context).map {
        case Right(meaning) => meaning.fold(syntax, _ => freeze(callee, context))
        case Left(_)        => freeze(callee, context)
      }

    private def applyAlias(
        name: String,
        args: List[Type],
        declaration: (TypeEntry, Defn.Type),
        context: ResolutionContext
    ): Prepared = {
      val (entry, declared) = declaration
      val key = entry.key
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
      val scope = context.inScope(entry.owner, bindings)
      TypeSubstitution.replace(declared.body, substitutions) match {
        case matched @ Type.Match.After_4_9_9(scrutinee, block) =>
          prepareMatch(MatchType(matched, scrutinee, block, declared.bounds.hi.nonEmpty), scope)
        case body => syntax(body, scope)
      }
    }

    // A leaf an alias body passes on may itself be a placeholder of an outer alias's argument
    // (`PGrate[S, …]` expands to `Aux[$1, …]`): it is recorded already spelled, so one pass
    // restores a reason however deep the aliases nest.
    private def freeze(tpe: Type, context: ResolutionContext): Prepared =
      frozen(context.read(tpe, resolver), context.variables, Spelling.of(tpe))

    private def frozen(
        shape: Resolved,
        variables: Map[String, Resolved],
        written: String
    ): Prepared = {
      val token = freshToken(variables)
      spellings += token -> spelled(written)
      Right(Type.Name(token) -> Map(token -> shape))
    }

  }

}
