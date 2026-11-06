package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.{Card, Inhabitation}
import cardinality.analysis.model.ContextBounds

import MethodAnalysis.{Frame, Resolved}

/** The public factories of a class no constructor callable from the measured declaration's site
  * builds (v1.md §2, rule B): the methods and `val`s callable there that construct it. A definition
  * constructs the class where its result — through blocks, local `val`s, `if` and `match` branches
  * and ascriptions — is a direct construction of it (`new C(…)`, a creator application `C(…)`, a
  * case class's `apply`), whatever type it declares: a family-typed `PSVec.of` constructs the case
  * `Slice` it builds. One declaring the class as its result whose body is read as no construction
  * (it delegates, or returns a value it was given) is a factory whose body is not read.
  *
  * Each factory is one bound: its parameters' space where it is closed (a readable body), unknown
  * where it is not or where the factory runs on a receiver of a class or trait (its receiver is an
  * input too), and 1 for a body not read. The measured declaration is no factory: its body is the
  * choice being counted.
  */
private[cardinality] object ReceiverFactories {

  /** One bound per factory of `owner` callable from `from`: Some(space) where it is known. */
  def of(
      owner: Frame,
      from: Frame,
      resolver: Resolver,
      closed: (Type, Frame, Map[String, Resolved]) => Option[Card]
  ): List[Option[Card]] =
    owner.name.toList.flatMap { name =>
      // The syntactic reading first: accessibility is asked only of a definition that builds or
      // declares the class.
      for {
        scope <- resolver.memberScopes
        stat <- scope.stats
        (body, declared) <- definition(stat).toList
        readable <- factory(body, declared, owner, name, scope, resolver).toList
        if !measured(stat, from) && resolver.accessible(stat, scope, from) &&
          reachable(scope, from, resolver)
      } yield
        if (!readable) Some(Card.one)
        else if (!singleton(scope)) None
        else
          stat match {
            case d: Defn.Def =>
              val group = d.paramClauseGroups
              val tparams = group.flatMap(_.tparamClause.values)
              val params =
                group.flatMap(_.paramClauses).flatMap(_.values) ++ ContextBounds.evidence(tparams)
              val atoms = tparams
                .map(p => p.name.value -> (Right(Inhabitation.Shape.Atom(p.name.value)): Resolved))
              closed(
                SecondaryConstructors.arguments(params),
                scope,
                resolver.typeParameters(scope) ++ atoms
              )
            case _ => Some(Card.one)
          }
    }

  // A method's or a single `val`'s body and declared result.
  private def definition(stat: Stat): Option[(Term, Option[Type])] = stat match {
    case d: Defn.Def                     => Some(d.body -> d.decltpe)
    case v: Defn.Val if v.pats.size == 1 => Some(v.rhs -> v.decltpe)
    case _                               => None
  }

  // Some(readable) where the definition is a factory of the class: Some(true) where its result
  // constructs it, Some(false) where it only declares it.
  private def factory(
      body: Term,
      declared: Option[Type],
      owner: Frame,
      name: String,
      scope: Frame,
      resolver: Resolver
  ): Option[Boolean] =
    if (results(body, Map.empty).exists(constructs(_, owner, name, scope, resolver))) Some(true)
    else Option.when(declared.exists(names(_, owner, name, scope, resolver)))(false)

  // The terms a body may evaluate to: its result through blocks, local vals and branches.
  private def results(term: Term, locals: Map[String, Term]): List[Term] = term match {
    case Term.Block(stats :+ (last: Term)) =>
      val vals = stats.collect { case Defn.Val(_, List(Pat.Var(n)), _, rhs) => n.value -> rhs }
      results(last, locals -- stats.flatMap(model.MemberSyntax.defined) ++ vals)
    case Term.If.After_4_4_0(_, yes, no, _)  => results(yes, locals) ++ results(no, locals)
    case Term.Match.After_4_9_9(_, cases, _) => cases.cases.flatMap(c => results(c.body, locals))
    case Term.Ascribe(inner, _)              => results(inner, locals)
    case Term.Name(n) if locals.contains(n)  => results(locals(n), locals - n)
    case other                               => List(other)
  }

  private def constructs(
      term: Term,
      owner: Frame,
      name: String,
      scope: Frame,
      resolver: Resolver
  ): Boolean = term match {
    case Term.New(init)                    => names(init.tpe, owner, name, scope, resolver)
    case Term.Apply.After_4_6_0(callee, _) => creator(callee, owner, name, scope, resolver)
    case _                                 => false
  }

  // A creator application's callee: the class's name, applied to types or not, or its `apply`.
  private def creator(
      callee: Term,
      owner: Frame,
      name: String,
      scope: Frame,
      resolver: Resolver
  ): Boolean = callee match {
    case Term.ApplyType.After_4_6_0(inner, _)      => creator(inner, owner, name, scope, resolver)
    case Term.Select(inner, Term.Name("apply"))    => creator(inner, owner, name, scope, resolver)
    case Term.Apply.After_4_6_0(inner, _)          => creator(inner, owner, name, scope, resolver)
    case Term.Name(`name`)                         => means(name, owner, scope, resolver)
    case Term.Select(qualifier, Term.Name(`name`)) => qualifies(qualifier.syntax, owner)
    case _                                         => false
  }

  // Whether a type spells the class: its name where the scope resolves it to the class, or a
  // selection of it through its enclosing template's name.
  private def names(
      tpe: Type,
      owner: Frame,
      name: String,
      scope: Frame,
      resolver: Resolver
  ): Boolean = tpe match {
    case Type.Apply.After_4_6_0(inner, _)          => names(inner, owner, name, scope, resolver)
    case Type.Name(`name`)                         => means(name, owner, scope, resolver)
    case Type.Select(qualifier, Type.Name(`name`)) => qualifies(qualifier.syntax, owner)
    case _                                         => false
  }

  private def means(name: String, owner: Frame, scope: Frame, resolver: Resolver): Boolean =
    resolver.lookup(name, scope).exists(_.self.exists(_.id == owner.id))

  private def qualifies(qualifier: String, owner: Frame): Boolean =
    owner.parent.exists(p => qualifier == p.path.mkString(".") || p.name.contains(qualifier))

  // A scope whose definitions the site may reach: every template around it accessible there.
  private def reachable(scope: Frame, from: Frame, resolver: Resolver): Boolean =
    scope.chain.zip(scope.chain.drop(1)).forall { (frame, around) =>
      frame.tree.forall(resolver.accessible(_, around, from))
    }

  // A definition that runs on no varying receiver: one of a package, an object or a given with no
  // parameters.
  private def singleton(scope: Frame): Boolean = scope.tree.forall {
    case _: (Pkg | Pkg.Object | Defn.Object | Source) => true
    case g: Defn.Given                                => g.paramClauseGroups.isEmpty
    case _                                            => false
  }

  private def measured(definition: Tree, from: Frame): Boolean =
    from.chain.exists(_.tree.exists(_ eq definition))

}
