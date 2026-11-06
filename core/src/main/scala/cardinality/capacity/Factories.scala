package cardinality.capacity

import scala.meta.*

import cardinality.analysis.model.{FactorySyntax, MemberSyntax}

/** Rule B's factories on the stored-value side (v1.md §2, register D42): a class whose constructor
  * is restricted holds what its public secondaries and factories build, each at most min(its
  * parameters, the fields), the class the largest of these, over the fields a public member or a
  * public field shows (none: one value). "Public" is unrestricted: a row has no measuring site. The
  * estimate reads one source at a time, so the factories it finds are its companion's and its
  * body's. A factory on the class's own receiver is not known; one declaring the class whose body
  * is read as no construction (after two delegating calls) counts 1. A class with type parameters
  * keeps its fields' product.
  */
private[capacity] object Factories {

  /** None where the fields' product is the row; else the space the class's builders build, or why
    * it is not known.
    */
  def bound(
      stats: List[Stat],
      d: Defn.Class,
      size: Term.Param => Size
  ): Option[Either[String, Size]] =
    Option.when(d.ctor.mods.exists(MemberSyntax.restricted) && d.tparamClause.values.isEmpty) {
      val name = d.name.value
      val companions: List[Stat] = stats.collect {
        case o: Defn.Object if o.name.value == name && public(o.mods) => o.templ.body.stats
      }.flatten
      val body: List[Stat] = d.templ.body.stats
      val scopes: List[(List[Stat], Boolean)] = List(companions -> true, body -> false)
      val all: List[Stat] = scopes.flatMap(_._1)
      val builds = (term: Term, _: Unit) => constructs(term, name)
      val callees = (call: Term, _: Unit) =>
        FactorySyntax.callee(call).toList.flatMap { (_, callee) =>
          all
            .filter(MemberSyntax.defined(_).contains(callee))
            .flatMap(FactorySyntax.definition)
            .map(_._1 -> ())
        }
      val secondaries = body.collect {
        case c: Ctor.Secondary if public(c.mods) =>
          Some(product(c.paramClauses.toList.flatMap(_.values), size))
      }
      val factories = scopes.flatMap { (in, singleton) =>
        in.collect(Function.unlift { stat =>
          FactorySyntax.definition(stat).filter(_ => public(modsOf(stat))).flatMap {
            (term, declared) =>
              if (FactorySyntax.constructs(term, (), builds, callees))
                Some(if (singleton) space(stat, size) else None)
              else
                Option.when(declared.exists(names(_, name)))(
                  Option.unless(FactorySyntax.compiled(term))(UnitSize: Size)
                )
          }
        })
      }
      val fields = product(
        FactorySyntax.observed(
          d,
          stat => public(modsOf(stat)),
          p => MemberSyntax.fieldParams(d).exists(_ eq p) && public(p.mods)
        ),
        size
      )
      val built = secondaries ++ factories
      val bounded = built.map(_.map(_.min(fields)))
      if (built.isEmpty) Left(s"receiver has no public constructor: $name")
      else if (bounded.contains(Some(fields))) Right(fields)
      else
        bounded
          .foldLeft(Option(NothingSize: Size))((acc, b) =>
            acc.flatMap(a => b.map(Size.order.max(a, _)))
          )
          .toRight(s"receiver space of its public constructors and factories not compared: $name")
    }

  private def public(mods: List[Mod]): Boolean = !mods.exists(MemberSyntax.restricted)

  private def modsOf(stat: Stat): List[Mod] = stat match {
    case s: Stat.WithMods => s.mods
    case _                => Nil
  }

  // A factory's parameters' space; a parameter over its own type parameters is not known.
  private def space(stat: Stat, size: Term.Param => Size): Option[Size] = stat match {
    case d: Defn.Def =>
      val tparams = d.paramClauseGroups.flatMap(_.tparamClause.values).map(_.name.value).toSet
      val params = d.paramClauseGroups.flatMap(_.paramClauses).flatMap(_.values)
      val generic = params.exists(_.decltpe.exists(_.collect {
        case Type.Name(n) => n
      }.exists(tparams)))
      Option.unless(generic)(product(params, size))
    case _ => Some(UnitSize)
  }

  private def product(params: List[Term.Param], size: Term.Param => Size): Size =
    params.foldLeft(UnitSize: Size)(_ * size(_))

  private def constructs(term: Term, name: String): Boolean = term match {
    case Term.New(init)                    => names(init.tpe, name)
    case Term.Apply.After_4_6_0(callee, _) => creator(callee, name)
    case _                                 => false
  }

  private def creator(callee: Term, name: String): Boolean = callee match {
    case Term.ApplyType.After_4_6_0(inner, _)   => creator(inner, name)
    case Term.Select(inner, Term.Name("apply")) => creator(inner, name)
    case Term.Apply.After_4_6_0(inner, _)       => creator(inner, name)
    case Term.Name(n)                           => n == name
    case Term.Select(_, Term.Name(n))           => n == name
    case _                                      => false
  }

  private def names(tpe: Type, name: String): Boolean = tpe match {
    case Type.Apply.After_4_6_0(inner, _) => names(inner, name)
    case Type.Name(n)                     => n == name
    case Type.Select(_, Type.Name(n))     => n == name
    case _                                => false
  }

}
