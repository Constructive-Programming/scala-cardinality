package cardinality.analysis.resolution

import scala.meta.{Defn, Type, XtensionSyntax}

import cardinality.analysis.*
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.model.*
import cardinality.types.{TypeApplications, TypeSubstitution}

import Inhabitation.Shape
import MethodAnalysis.{sequence, Resolved, TypeEntry}
import RuntimeClasses.{Alternative, Runtime, Top}

/** A Scala union read as the tagged sum its alternatives make, where one exists.
  *
  * Scala unions carry no constructor tag, and equal structural shapes do not prove equal nominal
  * types. Exact type equalities apply first, before any alternative is read: a union alias is its
  * alternatives, `T | Any` is `Any`, one class spelled twice is one alternative, a class absorbs
  * the classes known to derive from it, and `Nothing` / `Null` (null-free model) are empty. What
  * remains is the sum of its alternatives only when each has a runtime class, no class carries two
  * alternatives (type arguments are erased), the classes are pairwise disjoint, and each sealed
  * family's subclasses are all declarations its shape counts: then an exhaustive class-test `match`
  * is the sum's eliminator, total, well defined, and returning each value at the type it was
  * injected from — a restricted test, unlike one on `Any`, which needs a default case.
  */
final private[cardinality] class UnionSums(resolver: Resolver, classes: RuntimeClasses) {

  // One alternative as written, the scope it is read in, and its runtime class.
  final private case class Written(
      tpe: Type,
      context: ResolutionContext,
      found: Either[String, Alternative]
  )

  def resolve(union: Type, context: ResolutionContext): Resolved = {
    // One spelling read twice in one scope is one type; an alias's parameters may differ per use.
    val written = flatten(union, context)
      .distinctBy((tpe, scope) => tpe.syntax -> scope)
      .map((tpe, scope) => Written(tpe, scope, classes.alternative(tpe, scope)))
    if (written.exists(_.found.exists(_.runtime == Top(TopTypes.Kind.Universal))))
      Right(TopTypes.terminal)
    else {
      val kept = absorbed(written)
      sequence(kept.map(w => w.context.read(w.tpe, resolver))).left
        .map(reason => s"union ${union.syntax}: $reason")
        .flatMap { shapes =>
          kept.zip(shapes).filterNot(_._2 == Shape.Sum(Nil)) match {
            case Nil               => Right(Shape.Sum(Nil))
            case List((_, single)) => Right(single)
            // Aliases of the same free binder denote the very same type.
            case (_, atom: Shape.Atom) :: rest if rest.forall(_._2 == atom) => Right(atom)
            case inhabited                                                  =>
              tagged(inhabited.map(_._1)).left
                .map(detail =>
                  "union alternatives require an overlap and discrimination proof: " +
                    s"${union.syntax} — $detail"
                )
                .map(_ => Shape.Sum(inhabited.map(_._2)))
          }
        }
    }
  }

  /** Whether `tpe` names or applies a union alias — one whose body, read through the aliases it
    * names or applies, is a union — so that its alternatives are read as written here.
    */
  def aliased(tpe: Type, context: ResolutionContext): Boolean =
    aliasOf(tpe, context).exists((entry, alias, _) => unionBody(entry, alias, context))

  // The alternatives of a union, through the union aliases it names: an alias's alternatives are
  // read in its own scope, and a parameter standing alone is the argument written here.
  private def flatten(tpe: Type, context: ResolutionContext): List[(Type, ResolutionContext)] =
    TypeSubstitution
      .unionAlternatives(tpe)
      .flatMap(alternative =>
        unionAlias(alternative, context).getOrElse(List(alternative -> context))
      )

  private def unionAlias(
      tpe: Type,
      context: ResolutionContext
  ): Option[List[(Type, ResolutionContext)]] =
    aliasOf(tpe, context)
      .filter((entry, alias, _) => unionBody(entry, alias, context))
      .flatMap { (entry, alias, args) =>
        val byParameter = alias.tparamClause.values.map(_.name.value).zip(args).toMap
        context
          .enterAlias(
            entry,
            resolver.typeParameters(entry.owner),
            AliasReading.Union,
            args.map(context.read(_, resolver))
          )
          .map(scope =>
            flatten(alias.body, scope).flatMap {
              case (Type.Name(param), inner) if (inner eq scope) && byParameter.contains(param) =>
                flatten(byParameter(param), context)
              case other => List(other)
            }
          )
      }

  // The visible, transparent alias `tpe` names or applies at its own arity, with its arguments.
  private def aliasOf(
      tpe: Type,
      context: ResolutionContext
  ): Option[(TypeEntry, Defn.Type, List[Type])] = {
    val (head, args) = TypeApplications.applied
      .getOption(tpe)
      .fold(tpe -> List.empty[Type])(app => app.tpe -> TypeApplications.applyArguments.get(app))
    val name = TypeApplications.name(head)
    Option
      .when(named(head))(name)
      .filterNot(context.variables.contains)
      .map(resolver.declaration(head, _, context))
      .collect {
        case Found.Entry(entry) if !OpaqueTypes.hidden(entry, context.useSite) => entry
      }
      .flatMap(entry =>
        entry.tree match {
          case alias: Defn.Type if alias.tparamClause.values.size == args.size =>
            Some((entry, alias, args))
          case _ => None
        }
      )
  }

  private def named(head: Type): Boolean = head match {
    case _: Type.Name | _: Type.Select => true
    case _                             => false
  }

  // Whether an alias's body is a union, directly or through the aliases it names or applies.
  private def unionBody(entry: TypeEntry, alias: Defn.Type, context: ResolutionContext): Boolean =
    context
      .enterAlias(entry, resolver.typeParameters(entry.owner), AliasReading.UnionBody)
      .exists { scope =>
        alias.body match {
          case TypeSubstitution.Union(_, _) => true
          case body => aliasOf(body, scope).exists((e, a, _) => unionBody(e, a, scope))
        }
      }

  // An unapplied alternative is dropped when an earlier one has its class, or another one's class
  // is known to be its ancestor: both are type equalities, so the dropped one is never read.
  private def absorbed(written: List[Written]): List[Written] = {
    val runtimes = written.map(w => unapplied(w.found))
    written.zip(runtimes).zipWithIndex.collect {
      case ((alternative, own), index) if !own.exists(absorbedBy(runtimes, index)) => alternative
    }
  }

  private def absorbedBy(runtimes: List[Option[Runtime]], index: Int)(own: Runtime): Boolean =
    runtimes.zipWithIndex.exists {
      case (Some(theirs), other) if other != index => absorbs(theirs, earlier = other < index)(own)
      case _                                       => false
    }

  private def absorbs(theirs: Runtime, earlier: Boolean)(own: Runtime): Boolean =
    if (theirs == own) earlier else classes.derives(own, theirs)

  private def unapplied(found: Either[String, Alternative]): Option[Runtime] =
    found.toOption.filterNot(_.applied).map(_.runtime)

  private def tagged(alternatives: List[Written]): Either[String, Unit] =
    sequence(alternatives.map(_.found)).flatMap { found =>
      val runtimes = found.map(_.runtime)
      val pairs = for {
        (left, index) <- runtimes.zipWithIndex
        right <- runtimes.drop(index + 1)
      } yield classes.disjoint(left, right)
      // An overlap is the first thing to name; a family read short matters only past it.
      RuntimeClasses.all(pairs ++ runtimes.map(classes.enumerable))
    }

}
