package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.{Card, Inhabitation}
import cardinality.analysis.model.{ContextBounds, FactorySyntax, MemberSyntax}

import MethodAnalysis.{Frame, Resolved}

/** The public factories of a class no constructor callable from the measured declaration's site
  * builds (v1.md §2, rule B, register D42): the methods and `val`s callable there that construct it
  * (`FactorySyntax`). A construction is `new C(…)`, a creator application `C(…)` or a case class's
  * `apply`, whatever type the definition declares: a family-typed `PSVec.of` constructs the case
  * `Slice` it builds. A result that calls another method (callable or not) is read through that
  * method's body, and through one more call; past that, one declaring the class as its result is a
  * factory whose body is not read.
  *
  * Each factory is one bound: its parameters' space where it is closed (a readable body), unknown
  * where it is not, where the factory runs on a receiver of a class or trait (its receiver is an
  * input too) or where its body is a compiled dependency's (not supplied), and 1 for a body read as
  * no construction. The measured declaration is no factory: its body is the choice being counted.
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
      for {
        (stat, scope, readable) <- found(owner, name, resolver)
        if !measured(stat, from) && resolver.accessible(stat, scope, from) &&
          reachable(scope, from, resolver)
      } yield
        if (readable.isEmpty) None
        else if (readable.contains(false)) Some(Card.one)
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

  /** The class's constructor parameters the site observes: a field it may select, or one a member
    * it may call reads.
    */
  def observed(owner: Frame, d: Defn.Class, from: Frame, resolver: Resolver): List[Term.Param] = {
    val fields = MemberSyntax.fieldParams(d)
    FactorySyntax.observed(
      d,
      stat => resolver.accessible(stat, owner, from),
      p => fields.exists(_ eq p) && resolver.reaches(p.mods, owner, from)
    )
  }

  // Every method and single `val` of a member scope with its scope: by the name it defines, and by
  // each name it mentions; and each class's factories once found. Built once per index.
  final private class Index(
      val defined: Map[String, List[(Stat, Frame)]],
      val mentioning: Map[String, List[(Stat, Frame)]],
      val factories: scala.collection.mutable.Map[String, List[(Stat, Frame, Option[Boolean])]]
  )

  private val indexes = new java.util.WeakHashMap[Resolver, Index]()

  private def index(resolver: Resolver): Index =
    indexes.synchronized {
      Option(indexes.get(resolver)).getOrElse {
        val all = resolver.memberScopes.flatMap(scope =>
          scope.stats.collect {
            case stat if FactorySyntax.definition(stat).nonEmpty => stat -> scope
          }
        )
        val built = new Index(
          all
            .flatMap((stat, scope) => MemberSyntax.defined(stat).map(_ -> (stat, scope)))
            .groupMap(_._1)(_._2),
          all
            .flatMap((stat, scope) =>
              stat.collect { case n: Name => n.value }.distinct.map(_ -> (stat, scope))
            )
            .groupMap(_._1)(_._2),
          scala.collection.mutable.Map.empty
        )
        indexes.put(resolver, built)
        built
      }
    }

  // The definitions of the class's factories, whatever the site, each readable or not. Only a
  // definition that mentions the class, or a method one does through at most two calls, can be.
  private def found(
      owner: Frame,
      name: String,
      resolver: Resolver
  ): List[(Stat, Frame, Option[Boolean])] = {
    val idx = index(resolver)
    idx.synchronized(idx.factories.get(owner.id)).getOrElse {
      def named(defs: List[(Stat, Frame)]) = defs.flatMap(d => MemberSyntax.defined(d._1)).toSet
      def mentioning(names: Set[String]) = names.toList.flatMap(idx.mentioning.getOrElse(_, Nil))
      val m0 = idx.mentioning.getOrElse(name, Nil)
      val m1 = mentioning(named(m0))
      val m2 = mentioning(named(m1))
      val reachable = named(m0) ++ named(m1)
      val builds = (term: Term, scope: Frame) => constructs(term, owner, name, scope, resolver)
      val callees = (call: Term, scope: Frame) => calleesOf(call, scope, idx.defined, reachable)
      val result = (m0 ++ m1 ++ m2).distinctBy(_._1).flatMap { (stat, scope) =>
        FactorySyntax.definition(stat).flatMap { (body, declared) =>
          if (FactorySyntax.constructs(body, scope, builds, callees))
            Some((stat, scope, Some(true)))
          else
            Option.when(declared.exists(names(_, owner, name, scope, resolver)))(
              (stat, scope, Option.unless(FactorySyntax.compiled(body))(false))
            )
        }
      }
      idx.synchronized(idx.factories.update(owner.id, result))
      result
    }
  }

  // The bodies a call may run: a definition of its name around the caller for a bare name or one
  // on `this`, or one of a template named as its qualifier's last segment.
  private def calleesOf(
      call: Term,
      scope: Frame,
      index: Map[String, List[(Stat, Frame)]],
      reachable: Set[String]
  ): List[(Term, Frame)] =
    FactorySyntax.callee(call).toList.filter((_, name) => reachable(name)).flatMap {
      (qualifier, name) =>
        val around = scope.chain.map(_.id).toSet
        index
          .getOrElse(name, Nil)
          .collect {
            case (stat, at) if qualifier.fold(around(at.id)) {
                  case _: Term.This => around(at.id)
                  case q            => at.name.contains(q.syntax.split('.').last)
                } =>
              FactorySyntax.definition(stat).map(_._1 -> at)
          }
          .flatten
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
