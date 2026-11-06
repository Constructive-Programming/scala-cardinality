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
      val index = definitions(resolver)
      val builds = (term: Term, scope: Frame) => constructs(term, owner, name, scope, resolver)
      val callees = (call: Term, scope: Frame) => calleesOf(call, scope, index)
      // The syntactic reading first: accessibility is asked only of a definition that builds or
      // declares the class.
      for {
        (stat, scope) <- index.values.flatten.toList
        (body, declared) <- FactorySyntax.definition(stat).toList
        readable <- (
          if (FactorySyntax.constructs(body, scope, builds, callees)) Some(true)
          else Option.when(declared.exists(names(_, owner, name, scope, resolver)))(false)
        ).toList
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

  // Every method and single `val` of a member scope, by name, with its scope; built once per index.
  private val indexes = new java.util.WeakHashMap[Resolver, Map[String, List[(Stat, Frame)]]]()

  private def definitions(resolver: Resolver): Map[String, List[(Stat, Frame)]] =
    indexes.synchronized {
      Option(indexes.get(resolver)).getOrElse {
        val built = resolver.memberScopes
          .flatMap(scope =>
            scope.stats.collect {
              case stat if FactorySyntax.definition(stat).nonEmpty =>
                MemberSyntax.defined(stat).map(_ -> (stat, scope))
            }.flatten
          )
          .groupMap(_._1)(_._2)
        indexes.put(resolver, built)
        built
      }
    }

  // The bodies a call may run: a definition of its name around the caller for a bare name or one
  // on `this`, or one of a template named as its qualifier's last segment.
  private def calleesOf(
      call: Term,
      scope: Frame,
      index: Map[String, List[(Stat, Frame)]]
  ): List[(Term, Frame)] =
    FactorySyntax.callee(call).toList.flatMap { (qualifier, name) =>
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
