package cardinality.types

import scala.meta.*

import dev.constructive.eo.data.PSVec
import dev.constructive.eo.optics.{Getter, Lens, Optional, Plated, Prism}
import dev.constructive.eo.schemes.Schemes

// Parameter substitution shared by match-type reduction and its callers.
private[cardinality] object TypeSubstitution {
  private given Dialect = dialects.Scala3

  val typeNamePrism = Prism.optional[Type, String](
    {
      case Type.Name(name) => Some(name)
      case _               => None
    },
    Type.Name(_)
  )

  val patVarName = TypeApplications.patVarName

  val lambdaBody: Optional[Type.Lambda, Type.Lambda, Type, Type] =
    Optional(
      lambda =>
        lambda.body match {
          case body: Type => Right(body)
          case _          => Left(lambda)
        },
      { case (lambda, body) => lambda.copy(tpe = body) }
    )

  val polyBody: Optional[Type.PolyFunction, Type.PolyFunction, Type, Type] =
    Optional(
      poly =>
        poly.body match {
          case body: Type => Right(body)
          case _          => Left(poly)
        },
      { case (poly, body) => poly.copy(tpe = body) }
    )

  private val refineBodyLens =
    Lens[Type.Refine, Stat.Block](
      _.body,
      (refinement, body) => refinement.copy(tpe = refinement.tpe, body = body)
    )

  private val blockStatsLens =
    Lens[Stat.Block, List[Stat]](_.stats, (block, stats) => block.copy(stats = stats))

  private val refineStatsLens = refineBodyLens.andThen(blockStatsLens)

  given platedType: Plated[Type] = Plated.fromChildren[Type](
    children = {
      case applied: Type.Apply                      => applied.tpe :: applied.argClause.values
      case infix: Type.ApplyInfix                   => List(infix.lhs, infix.op, infix.rhs)
      case Type.Tuple(args)                         => args
      case f: Type.Function                         => f.paramClause.values :+ f.res
      case f: Type.ContextFunction                  => f.paramClause.values :+ f.res
      case p: Type.TypedParam                       => List(p.typ)
      case Type.ByName(v)                           => List(v)
      case Type.Repeated(v)                         => List(v)
      case a: Type.Annotate                         => List(a.tpe)
      case e: Type.Existential                      => List(e.tpe)
      case p: Type.Project                          => List(p.qual)
      case Type.With(left, right)                   => List(left, right)
      case a: Type.AnonymousLambda                  => List(a.tpe)
      case w: Type.Wildcard                         => w.bounds.lo.toList ++ w.bounds.hi.toList
      case l: Type.Lambda                           => lambdaBody.getOption(l).toList
      case p: Type.PolyFunction                     => polyBody.getOption(p).toList
      case r: Type.Refine                           => r.tpe.toList
      case Type.Match.After_4_9_9(scrutinee, block) =>
        scrutinee :: block.cases.flatMap(c => List(c.pat, c.body))
      case _ => Nil
    },
    rebuild = (tpe, kids) =>
      tpe match {
        case applied: Type.Apply =>
          TypeApplications.applyArguments.replace(kids.tail)(applied.copy(tpe = kids.head))
        case infix: Type.ApplyInfix =>
          val op = kids(1) match {
            case name: Type.Name => name
            case _               => infix.op
          }
          infix.copy(lhs = kids.head, op = op, rhs = kids(2))
        case _: Type.Tuple    => Type.Tuple(kids)
        case f: Type.Function =>
          f.copy(paramClause = Type.FuncParamClause(kids.init), res = kids.last)
        case f: Type.ContextFunction =>
          f.copy(paramClause = Type.FuncParamClause(kids.init), res = kids.last)
        case p: Type.TypedParam      => p.copy(typ = kids.head)
        case _: Type.ByName          => Type.ByName(kids.head)
        case _: Type.Repeated        => Type.Repeated(kids.head)
        case a: Type.Annotate        => a.copy(tpe = kids.head)
        case e: Type.Existential     => e.copy(tpe = kids.head)
        case p: Type.Project         => p.copy(qual = kids.head)
        case _: Type.With            => Type.With(kids.head, kids(1))
        case a: Type.AnonymousLambda => a.copy(tpe = kids.head)
        case w: Type.Wildcard        =>
          val (lo, rest) =
            w.bounds.lo.fold((Option.empty[Type], kids))(_ => (kids.headOption, kids.drop(1)))
          val hi = w.bounds.hi.flatMap(_ => rest.headOption)
          w.copy(bounds = w.bounds.copy(lo = lo, hi = hi, context = w.bounds.context))
        case l: Type.Lambda =>
          kids.headOption.fold(l)(lambdaBody.replace(_)(l))
        case p: Type.PolyFunction =>
          kids.headOption.fold(p)(polyBody.replace(_)(p))
        case r: Type.Refine =>
          r.copy(tpe = r.tpe.zip(kids.headOption).map(_._2), body = r.body)
        case Type.Match.After_4_9_9(_, block) =>
          val cases = block.cases.zip(kids.tail.grouped(2).toList).map {
            case (c, List(pat, body)) => c.copy(pat = pat, body = body)
            case (c, _)               => c
          }
          Type.Match.After_4_9_9(kids.head, Type.CasesBlock(cases))
        case leaf => leaf
      }
  )

  // Stop before descending into nested lambdas: each lambda owns its own holes.
  val localPlate: Plated[Type] = Plated.fromChildren[Type](
    children = {
      case _: Type.AnonymousLambda | _: Type.Lambda => Nil
      case other                                    => platedType.childrenVec(other).toList
    },
    rebuild = (tpe, kids) =>
      tpe match {
        case _: Type.AnonymousLambda | _: Type.Lambda => tpe
        case other                                    => platedType.rebuild(other, PSVec.from(kids))
      }
  )

  def localCata[A](alg: (Type, List[A], List[Type] => Type) => A): Type => A = {
    val getter = Schemes.cata[Type, A] { (node, kids) =>
      alg(node, kids.toList, rebuilt => localPlate.rebuild(node, PSVec.from(rebuilt)))
    }(using localPlate)
    getter.get
  }

  def hylo[S, A](expand: S => List[S], merge: (S, List[A]) => A): S => A = {
    val getter = Schemes.hylo[S, A](
      seed => PSVec.from(expand(seed)),
      (seed, kids) => merge(seed, kids.toList)
    )
    getter.get
  }

  def universe(tpe: Type): List[Type] = Plated.universe(tpe)

  def localUniverse(tpe: Type): List[Type] = Plated.universe(tpe)(using localPlate)

  def typeNames(tpe: Type): List[String] =
    universe(tpe).flatMap(typeNamePrism.getOption)

  val unionAlternatives: Getter[Type, List[Type]] =
    Schemes.hylo[Type, List[Type]](
      {
        case Type.Or(left, right)                            => PSVec.of(left, right)
        case union: Type.ApplyInfix if union.op.value == "|" => PSVec.of(union.lhs, union.rhs)
        case _                                               => PSVec.empty
      },
      (node, kids) => if (kids.isEmpty) List(node) else kids.iterator.flatten.toList
    )

  val described: Getter[Type, String] =
    Schemes.hylo[Type, String](
      {
        case applied: Type.Apply           => PSVec.Single(applied.tpe)
        case annotate: Type.Annotate       => PSVec.Single(annotate.tpe)
        case existential: Type.Existential => PSVec.Single(existential.tpe)
        case refine: Type.Refine           => PSVec.from(refine.tpe)
        case _                             => PSVec.empty
      },
      (tpe, kids) =>
        tpe match {
          case name: Type.Name        => name.value
          case _: Type.Apply          => kids(0)
          case infix: Type.ApplyInfix => infix.op.value
          case _: Type.Annotate       => kids(0)
          case _: Type.Existential    => kids(0)
          case _: Type.Refine         =>
            if (kids.isEmpty) "a refinement" else s"a refinement of ${kids(0)}"
          case _: Type.Match        => "a match type"
          case _: Type.Lambda       => "a type lambda"
          case _: Type.PolyFunction => "a polymorphic function type"
          case _: Type.Wildcard     => "a wildcard"
          case other                => other.syntax.replaceAll("\\s+", " ")
        }
    )

  private val substituted: Getter[Type, Map[TypeName, Type] => Type] =
    Schemes.cata[Type, Map[TypeName, Type] => Type] { (node, kids) => env =>
      node match {
        case selected: Type.Select => selected
        case lambda: Type.Lambda   =>
          val scoped = env -- boundNames(lambda.tparamClause.values)
          if (kids.isEmpty) lambda else lambdaBody.replace(kids(0)(scoped))(lambda)
        case poly: Type.PolyFunction =>
          val scoped = env -- boundNames(poly.tparamClause.values)
          if (kids.isEmpty) poly else polyBody.replace(kids(0)(scoped))(poly)
        case refinement: Type.Refine =>
          val base = if (kids.isEmpty) None else Some(kids(0)(env))
          val scoped = env -- refineMembers(refinement)
          val withStats = refineStatsLens.modify(_.map(replaceStat(_, scoped)))(refinement)
          withStats.copy(tpe = base, body = withStats.body)
        case Type.Match.After_4_9_9(_, block) =>
          val cases = block.cases.zipWithIndex.map { (c, index) =>
            val patFn = kids(1 + index * 2)
            val bodyFn = kids(2 + index * 2)
            val scoped = env -- caseBoundNames(c.pat)
            TypeCase(patFn(scoped), bodyFn(scoped))
          }
          Type.Match.After_4_9_9(kids(0)(env), Type.CasesBlock(cases))
        case Type.Name(name) if env.contains(TypeName.of(name)) =>
          env(TypeName.of(name))
        case _ =>
          platedType.rebuild(node, PSVec.from(kids.iterator.map(_(env))))
      }
    }

  // A type with the definition's parameters replaced by the argument types, syntax kept as it is.
  def replace(tpe: Type, arguments: Map[TypeName, Type]): Type =
    if (arguments.isEmpty) tpe else substituted.get(tpe)(arguments)

  private def boundNames(params: List[Type.Param]): Set[TypeName] =
    params.map(p => TypeName.of(p.name.value)).toSet

  private def caseBoundNames(pat: Type): Set[TypeName] =
    typeNames(pat).filter(_.headOption.exists(_.isLower)).map(TypeName.of).toSet

  private def refineMembers(refinement: Type.Refine): Set[TypeName] =
    refinement.body.stats.collect {
      case d: Defn.Type => TypeName.of(d.name.value)
      case d: Decl.Type => TypeName.of(d.name.value)
    }.toSet

  private def replaceBounds(bounds: Type.Bounds, env: Map[TypeName, Type]): Type.Bounds =
    bounds.copy(
      lo = bounds.lo.map(replace(_, env)),
      hi = bounds.hi.map(replace(_, env)),
      context = bounds.context.map(replace(_, env))
    )

  private def replaceStat(stat: Stat, env: Map[TypeName, Type]): Stat = stat match {
    case d: Defn.Type =>
      val scoped = env -- boundNames(d.tparamClause.values)
      d.copy(body = replace(d.body, scoped))
    case d: Decl.Type =>
      val scoped = env -- boundNames(d.tparamClause.values)
      d.copy(bounds = replaceBounds(d.bounds, scoped))
    case d: Decl.Val =>
      d.copy(decltpe = replace(d.decltpe, env))
    case d: Defn.Val =>
      d.copy(decltpe = d.decltpe.map(replace(_, env)))
    case other => other
  }

}
