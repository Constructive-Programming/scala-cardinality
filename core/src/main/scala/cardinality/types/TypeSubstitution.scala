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

  private def bodyOptional[S <: Tree](
      body: S => Tree,
      set: (S, Type) => S
  ): Optional[S, S, Type, Type] =
    Optional(
      s =>
        body(s) match {
          case t: Type => Right(t)
          case _       => Left(s)
        },
      { case (s, t) => set(s, t) }
    )

  val lambdaBody: Optional[Type.Lambda, Type.Lambda, Type, Type] =
    bodyOptional(_.body, (l, b) => l.copy(tpe = b))

  val polyBody: Optional[Type.PolyFunction, Type.PolyFunction, Type, Type] =
    bodyOptional(_.body, (p, b) => p.copy(tpe = b))

  private val refineStatsLens =
    Lens[Type.Refine, Stat.Block](_.body, (r, b) => r.copy(tpe = r.tpe, body = b))
      .andThen(Lens[Stat.Block, List[Stat]](_.stats, (b, s) => b.copy(stats = s)))

  def plated[S](parts: S => (List[S], List[S] => S)): Plated[S] =
    Plated.fromChildren(s => parts(s)._1, (s, kids) => parts(s)._2(kids))

  given platedType: Plated[Type] = plated {
    case a: Type.Apply =>
      (a.tpe :: a.argClause.values) ->
        (k => TypeApplications.applyArguments.replace(k.tail)(a.copy(tpe = k.head)))
    case i: Type.ApplyInfix =>
      List(i.lhs, i.op, i.rhs) -> { k =>
        val op = k(1) match {
          case name: Type.Name => name
          case _               => i.op
        }
        i.copy(lhs = k.head, op = op, rhs = k(2))
      }
    case Type.Tuple(args)     => args -> Type.Tuple.apply
    case f: Type.FunctionType =>
      (f.paramClause.values :+ f.res) -> { k =>
        val params = Type.FuncParamClause(k.init)
        f match {
          case c: Type.ContextFunction => c.copy(paramClause = params, res = k.last)
          case p: Type.Function        => p.copy(paramClause = params, res = k.last)
        }
      }
    case p: Type.TypedParam      => List(p.typ) -> (k => p.copy(typ = k.head))
    case Type.ByName(v)          => List(v) -> (k => Type.ByName(k.head))
    case Type.Repeated(v)        => List(v) -> (k => Type.Repeated(k.head))
    case a: Type.Annotate        => List(a.tpe) -> (k => a.copy(tpe = k.head))
    case e: Type.Existential     => List(e.tpe) -> (k => e.copy(tpe = k.head))
    case p: Type.Project         => List(p.qual) -> (k => p.copy(qual = k.head))
    case Type.With(l, r)         => List(l, r) -> (k => Type.With(k.head, k(1)))
    case a: Type.AnonymousLambda => List(a.tpe) -> (k => a.copy(tpe = k.head))
    case w: Type.Wildcard        =>
      (w.bounds.lo.toList ++ w.bounds.hi.toList) -> { k =>
        val (lo, rest) = w.bounds.lo.fold((Option.empty[Type], k))(_ => (k.headOption, k.drop(1)))
        val hi = w.bounds.hi.flatMap(_ => rest.headOption)
        w.copy(bounds = w.bounds.copy(lo = lo, hi = hi, context = w.bounds.context))
      }
    case l: Type.Lambda =>
      lambdaBody.getOption(l).toList -> (k => k.headOption.fold(l)(lambdaBody.replace(_)(l)))
    case p: Type.PolyFunction =>
      polyBody.getOption(p).toList -> (k => k.headOption.fold(p)(polyBody.replace(_)(p)))
    case r: Type.Refine =>
      r.tpe.toList -> (k => r.copy(tpe = r.tpe.zip(k.headOption).map(_._2), body = r.body))
    case Type.Match.After_4_9_9(scrutinee, block) =>
      (scrutinee :: block.cases.flatMap(c => List(c.pat, c.body))) -> { k =>
        val cases = block.cases.zip(k.tail.grouped(2).toList).map {
          case (c, List(pat, body)) => c.copy(pat = pat, body = body)
          case (c, _)               => c
        }
        Type.Match.After_4_9_9(k.head, Type.CasesBlock(cases))
      }
    case leaf => Nil -> (_ => leaf)
  }

  // Stop before descending into nested lambdas: each lambda owns its own holes.
  val localPlate: Plated[Type] = plated {
    case l: (Type.AnonymousLambda | Type.Lambda) => Nil -> (_ => l)
    case other                                   =>
      platedType.childrenVec(other).toList -> (k => platedType.rebuild(other, PSVec.from(k)))
  }

  def localCata[A](alg: (Type, List[A], List[Type] => Type) => A): Type => A =
    Schemes
      .cata[Type, A] { (node, kids) =>
        alg(node, kids.toList, rebuilt => localPlate.rebuild(node, PSVec.from(rebuilt)))
      }(using localPlate)
      .get

  def hylo[S, A](expand: S => List[S], merge: (S, List[A]) => A): S => A =
    Schemes
      .hylo[S, A](
        seed => PSVec.from(expand(seed)),
        (seed, kids) => merge(seed, kids.toList)
      )
      .get

  def typeNames(tpe: Type): List[String] =
    Plated.universe(tpe).flatMap(typeNamePrism.getOption)

  val unionAlternatives: Type => List[Type] =
    hylo[Type, List[Type]](
      {
        case Type.Or(left, right)                            => List(left, right)
        case union: Type.ApplyInfix if union.op.value == "|" => List(union.lhs, union.rhs)
        case _                                               => Nil
      },
      (node, kids) => if (kids.isEmpty) List(node) else kids.flatten
    )

  val described: Type => String =
    hylo[Type, String](
      {
        case a: Type.Apply       => List(a.tpe)
        case a: Type.Annotate    => List(a.tpe)
        case e: Type.Existential => List(e.tpe)
        case r: Type.Refine      => r.tpe.toList
        case _                   => Nil
      },
      (tpe, kids) =>
        tpe match {
          case name: Type.Name                                    => name.value
          case _: (Type.Apply | Type.Annotate | Type.Existential) => kids.head
          case infix: Type.ApplyInfix                             => infix.op.value
          case _: Type.Refine                                     =>
            kids.headOption.fold("a refinement")(base => s"a refinement of $base")
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
          replaceScoped(lambda, lambda.tparamClause.values, lambdaBody, kids, env)
        case poly: Type.PolyFunction =>
          replaceScoped(poly, poly.tparamClause.values, polyBody, kids, env)
        case refinement: Type.Refine =>
          val base = if (kids.isEmpty) None else Some(kids(0)(env))
          val scoped = env -- refineMembers(refinement)
          val withStats = refineStatsLens.modify(_.map(replaceStat(_, scoped)))(refinement)
          withStats.copy(tpe = base, body = withStats.body)
        case Type.Match.After_4_9_9(_, block) =>
          val cases = block.cases.zipWithIndex.map { (c, index) =>
            val scoped = env -- caseBoundNames(c.pat)
            TypeCase(kids(1 + index * 2)(scoped), kids(2 + index * 2)(scoped))
          }
          Type.Match.After_4_9_9(kids(0)(env), Type.CasesBlock(cases))
        case Type.Name(name) if env.contains(TypeName.of(name)) =>
          env(TypeName.of(name))
        case _ =>
          platedType.rebuild(node, PSVec.from(kids.iterator.map(_(env))))
      }
    }

  private def replaceScoped[A <: Type](
      node: A,
      params: List[Type.Param],
      optic: Optional[A, A, Type, Type],
      kids: PSVec[Map[TypeName, Type] => Type],
      env: Map[TypeName, Type]
  ): A =
    if (kids.isEmpty) node else optic.replace(kids(0)(env -- boundNames(params)))(node)

  // A type with the definition's parameters replaced by the argument types, syntax kept as it is.
  def replace(tpe: Type, arguments: Map[TypeName, Type]): Type =
    if (arguments.isEmpty) tpe else substituted.get(tpe)(arguments)

  private def boundNames(params: List[Type.Param]): Set[TypeName] =
    params.map(p => TypeName.of(p.name.value)).toSet

  private def caseBoundNames(pat: Type): Set[TypeName] =
    typeNames(pat).filter(_.headOption.exists(_.isLower)).map(TypeName.of).toSet

  private def refineMembers(refinement: Type.Refine): Set[TypeName] =
    refinement.body.stats.collect {
      case d: (Defn.Type | Decl.Type) => TypeName.of(d.name.value)
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
