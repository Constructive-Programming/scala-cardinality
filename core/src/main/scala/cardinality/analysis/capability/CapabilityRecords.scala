package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{Arguments, MemberPath, MemberSyntax, ResolutionContext}
import cardinality.analysis.resolution.{
  FinalImplementations,
  InheritedScopes,
  OverrideIdentity,
  Representations,
  TemplateClosures,
  TypeMembers
}
import cardinality.analysis.{ConstrainedReceivers, ForeignParents, MethodAnalysis, Resolver}
import cardinality.capacity.Counter
import cardinality.types.TypeApplications

import MethodAnalysis.{sequence, Frame, Resolved, TypeEntry}

/** A value of a source class or trait, read as the record of what observes it (capabilities C0–C4).
  *
  * Under the model a value is observed only through its accessible members — the universal ones
  * (`==`, `hashCode`, `toString`, …) are runtime equality and reflection, excluded — so it is the
  * tuple of their behaviors: Reynolds' and Cook's procedural reading of objects. One shape serves
  * an input and a goal, so a record is read only where the two readings coincide:
  *
  *   - an *open* template (a trait without parameters, or a class whose constructor parameters are
  *     all public fields and whose constructor every subclass reaches): the product of its open
  *     members — abstract or overridable — and `val` fields. An input observes exactly those (a
  *     derived member is concrete code over them); a goal may pick any tuple of them, since an
  *     anonymous subclass overriding each with the chosen term realizes it.
  *   - a *final* class (or value class) whose constructor is public and whose every constructor
  *     parameter is observable — a public field, or exposed by a public member that is its η-alias:
  *     the product of those parameters. An instance is its constructor arguments, every member a
  *     fixed function of them, and the aliases observe each argument injectively.
  *
  * An unbounded abstract type member no level defines opens instead: the record is the package ∃X.
  * members-over-X each value carries (`Shape.Existential`, opened per value by the solver).
  * Anything else — a bounded or higher-kinded abstract member, one the measured signature names
  * through a path, a self type, parents outside the sources, hidden state, a constructor only some
  * scope reaches — keeps a reason.
  */
final private[cardinality] class CapabilityRecords(resolver: Resolver, identity: OverrideIdentity) {
  import CapabilityRecords.*

  /** The record a source class or trait applied to `args` (as `name` spells it) is read as, at
    * `context`'s use site. Its proper parameters remember their arguments as written, as a case
    * class's do, so a match type over one of them reduces on a tuple argument (members MT1').
    */
  def resolve(name: String, entry: TypeEntry, args: Arguments, context: ResolutionContext) = {
    val key = entry.key
    entry.self.toRight(s"abstract type or method-valued representation: $key").flatMap { template =>
      resolver.operators
        .applied(name, template.typeParams)(args.shapes)
        .flatMap { bound =>
          val env = resolver.typeParameters(template) ++ bound
          Reading(
            template,
            env,
            args.proper(template.typeParams).toMap,
            context.enter(key),
            key
          ).record
        }
    }
  }

  /** One record being read: its template, the binders its arguments fix (and those arguments as
    * written), and the use site.
    */
  final private case class Reading(
      template: Frame,
      env: Map[String, Resolved],
      written: Map[String, (Type, ResolutionContext)],
      context: ResolutionContext,
      key: String
  ) {
    private val useSite = context.useSite

    def record: Resolved =
      TypeMembers
        .selfType(template)
        .toLeft(())
        .flatMap(_ => walk)
        .flatMap { steps =>
          val table = new ReceiverMembers(template, steps, identity, resolver)
          // A `final abstract` class is neither instantiated nor extended, and a template whose
          // construction only throws is never built (register D26): neither has a value.
          new Construction(resolver, steps).of(table.levels, useSite.tree).built.flatMap { built =>
            if (!built || template.tree.exists(neverInstantiated)) Right(Shape.Sum(Nil))
            else if (template.tree.exists(fixedClass)) finalRecord
            else openRecord(steps, table)
          }
        }

    private def walk: Either[String, List[InheritedScopes.Step]] =
      sequence(InheritedScopes.walk(List(template -> env), resolver, useSite))

    // A final class: its observable constructor parameters, each an argument a public
    // constructor takes freely.
    private def finalRecord: Resolved =
      ConstrainedReceivers.reason(template).toLeft(()).flatMap { _ =>
        sequence(template.params.map(observable)).map(Shape.Product(_))
      }

    private def fieldType(parameter: Term.Param, scope: Scope = Scope.empty): Resolved =
      parameter.decltpe
        .toRight(s"missing field type: $key")
        .flatMap(read(_, template, scope))

    private def observable(parameter: Term.Param): Resolved =
      fieldType(parameter).flatMap { field =>
        if (publicField(parameter, template)) Right(field)
        else
          Either.cond(
            exposures(parameter.name.value).exists(alias => observed(alias).contains(field)),
            field,
            s"final class field not observable injectively: $key.${parameter.name.value}"
          )
      }

    // What an alias observes: a parameterless one its result, an η-expansion its callable.
    private def observed(alias: Defn.Def): Option[Shape] =
      if (alias.paramClauseGroups.isEmpty)
        alias.decltpe.flatMap(read(_, template, Scope.empty).toOption)
      else observe(alias, template, alias.name.value, Scope.empty).toOption.flatten

    // The public members whose body is the parameter itself or its η-expansion.
    private def exposures(parameter: String): List[Defn.Def] =
      template.stats.collect {
        case d: Defn.Def if visible(d, template) && alias(d, parameter) => d
      }

    // An open template: its own `val` fields and its open members — a parent's overridable fields
    // and the fields of the parameterized levels no class of it passes arguments to among them —
    // each a free choice of its type. Levels are read in linearization order, so the declaration
    // a member's slot keeps is the nearest one, whose type the member has. An unbounded abstract
    // type member no level defines is the witness of the package each value carries: the members
    // read over the witness, and the record is that package ∃X. members-over-X — an input opens it
    // with the value's own witness, a goal of it is a result construction the model does not pick.
    private def openRecord(steps: List[InheritedScopes.Step], table: ReceiverMembers): Resolved = {
      val all = table.levels
      val closures = TemplateClosures.of(template, all)
      val (unread, names) = abstractSplit(all)
      refusal(all, closures, unread).toLeft(()).flatMap { _ =>
        val scope = Scope(steps, witnessEnv(names))
        val open = table.open(None, TargetViews.ViewMode.Refuse).map(_.left.map(_.reason))
        val members = open.collect { case Right(c) => c.tree } ++ all.flatMap(abstracts)
        for {
          _ <- selfView(members, all).toLeft(())
          fields <- sequence(template.params.map(valField(_, scope)))
          concrete <- sequence(
            open.map(_.flatMap(c => observe(c.tree, c.level, c.name, scope)))
          )
          declared <- declarations(all, table, scope)
        } yield {
          val product = Shape.Product(fields ++ (concrete ++ declared).flatten)
          if (names.isEmpty) product else Shape.Existential(names, product)
        }
      }
    }

    // A value of the record may be the measured declaration's own receiver when some instance is of
    // both templates — always for two traits, which a subclass may mix — so a member of its name
    // and arity may be the declaration itself: calling it re-enters the declaration (capabilities
    // C6), which this reading does not split.
    private def selfView(members: List[Tree], all: List[Frame]): Option[String] =
      useSite.tree
        .flatMap(measured)
        .filter(_ => useSite.parent.exists(shareInstance(_, all)))
        .flatMap { (name, arity) =>
          members.collectFirst {
            case m if MemberSyntax.names(m).contains(name) && arityOf(m) == arity =>
              s"capability record member may be the measured declaration on its receiver: $key.$name"
          }
        }

    // Whether one instance may be of the measured declaration's template and of this record's: two
    // unrelated classes cannot (a class has one superclass chain), nor can an owner no subclass
    // extends (an object, a given, a final class) and a template outside its linearization.
    private def shareInstance(owner: Frame, all: List[Frame]): Boolean = {
      val ownerLevels = owner :: sequence(
        InheritedScopes.walk(List(owner -> resolver.typeParameters(owner)), resolver, owner)
      ).toOption.toList.flatten.map(_.declared)
      val related = all.exists(_.id == owner.id) || ownerLevels.exists(_.id == template.id)
      val classLike =
        owner.tree.exists(t => t.is[Defn.Class] || t.is[Defn.Object] || t.is[Defn.Given])
      val apart = resolver.fixedTemplate(owner) || template.tree.exists(_.is[Defn.Class])
      related || !(classLike && apart)
    }

    private def refusal(
        all: List[Frame],
        closures: List[TemplateClosures.Closure],
        unread: List[String]
    ) = {
      import TemplateClosures.Closure
      val restricted = closures.collectFirst {
        case _: Closure.PrivateConstructor | _: Closure.Hidden =>
          s"capability record of a template only its scope instantiates: $key"
      }
      val foreign = ForeignParents.reasons(all, resolver).headOption
      val unreadMember = all.flatMap(_.stats).collectFirst {
        case _: Defn.ExtensionGroup =>
          s"capability record with an extension member not read: $key"
      }
      val types = unread.map(t => s"capability record with abstract type member not read: $key.$t")
      val parameters = template.tree.collect {
        case t: Defn.Trait if t.ctor.paramClauses.exists(_.values.nonEmpty) =>
          s"capability record with trait parameters not read: $key"
      }
      restricted.orElse(foreign).orElse(parameters).orElse(unreadMember).orElse(types.headOption)
    }

    // A type member no level defines, split by what this reading does: bounded or higher-kinded
    // ones keep a reason, and so do any the measured signature names through a path, where the
    // witness and the path atom would have to be one; the rest open as the witnesses of the
    // package each value carries (∃X. members-over-X).
    private def abstractSplit(all: List[Frame]): (List[String], List[String]) = {
      val stats = all.flatMap(_.stats)
      val defined = stats.collect { case d: Defn.Type => d.name.value }.toSet
      val members = stats
        .collect { case d: Decl.Type if !defined(d.name.value) => d }
        .groupBy(_.name.value)
        .toList
        .map { (name, declared) =>
          // The strictest declaration decides: bounds or a binder on any level keep it unread.
          name -> declared.maxBy(d => if (openable(d)) 0 else 1)
        }
      val (unread, open) = members.partition { case (name, d) => !openable(d) || mentions(name) }
      (unread.map(_._1), open.map(_._1))
    }

    private def openable(d: Decl.Type): Boolean =
      d.tparamClause.values.isEmpty && d.bounds.lo.isEmpty && d.bounds.hi.isEmpty

    // The atoms the opened witnesses read as inside the record: one per abstract member, named by
    // it, so the package's members share their witness and the solver opens it per value.
    private def witnessEnv(names: List[String]): Map[String, Resolved] =
      names.map(name => name -> Right(Shape.Atom(name))).toMap

    // Whether the use site's signature names the member through a path (`p.X`, `T#X`): such a
    // spelling must denote the same witness the opened package binds, which this reading cannot
    // yet agree on with the path atom the projection reads as.
    private def mentions(name: String): Boolean =
      signatureTypes.exists(mentions(name, _))

    private def mentions(name: String, tpe: Type): Boolean = tpe match {
      case Type.Select(_, Type.Name(`name`))  => true
      case Type.Project(_, Type.Name(`name`)) => true
      case other                              =>
        other.children.collect { case t: Type => t }.exists(mentions(name, _))
    }

    private def signatureTypes: List[Type] = useSite.tree.toList.flatMap { tree =>
      val parameters =
        MemberSyntax.groups(tree).flatMap(_.paramClauses).flatMap(_.values.flatMap(_.decltpe))
      val result = tree match {
        case d: Defn.Def        => d.decltpe.toList
        case d: Decl.Def        => List(d.decltpe)
        case v: Defn.Val        => v.decltpe.toList
        case v: Decl.Val        => List(v.decltpe)
        case v: Defn.Var        => v.decltpe.toList
        case v: Decl.Var        => List(v.decltpe)
        case g: Defn.GivenAlias => List(g.decltpe)
        case g: Defn.Given      => g.templ.inits.collect { case i: Init => i.tpe }
        case c: Defn.Class      => c.ctor.paramClauses.flatMap(_.values.flatMap(_.decltpe))
        case e: Defn.Enum       => e.ctor.paramClauses.flatMap(_.values.flatMap(_.decltpe))
        case t: Defn.Trait      => t.ctor.paramClauses.flatMap(_.values.flatMap(_.decltpe))
        case _                  => Nil
      }
      parameters ++ result
    }

    // A constructor parameter of an open class: a public field is one any instance may give any
    // value of its type; hidden state is observed by no field, but a goal still needs it built.
    private def valField(parameter: Term.Param, scope: Scope): Resolved =
      if (!publicField(parameter, template))
        Left(s"open class with hidden constructor state: $key.${parameter.name.value}")
      else fieldType(parameter, scope)

    // The abstract declarations no concrete member implements, one per member: a declaration an
    // earlier level (the template first) restates is that member again.
    private def declarations(
        all: List[Frame],
        table: ReceiverMembers,
        scope: Scope
    ): Either[String, List[Option[Shape]]] = {
      val open = all.flatMap(level => abstracts(level).map(level -> _))
      sequence(open.zipWithIndex.map {
        case ((level, stat), index) =>
          for {
            implemented <- table.implemented(stat, level, None)
            again <- restated(level, stat, open.take(index), scope.steps)
            shape <-
              if (implemented || again) Right(None) else declared(level, stat, table, scope)
          } yield shape
      })
    }

    private def restated(
        level: Frame,
        stat: Stat,
        earlier: List[(Frame, Stat)],
        steps: List[InheritedScopes.Step]
    ): Either[String, Boolean] = {
      val names = MemberSyntax.names(stat)
      val answers = earlier.map { (other, prior) =>
        (stat, prior) match {
          case (d: Decl.Def, o: Decl.Def) =>
            identity.sameMember((d, level), (o, other), template, steps)
          // A value and a parameterless method of one name are one member (a `val` implements or
          // restates the `def`); a method with parameters is another.
          case _ =>
            Some(
              MemberSyntax.names(prior).exists(names.contains) &&
                MemberSyntax.groups(stat).isEmpty && MemberSyntax.groups(prior).isEmpty
            )
        }
      }
      ReceiverMembers.settle(answers, names.mkString(", "))
    }

    private def declared(
        level: Frame,
        stat: Stat,
        table: ReceiverMembers,
        scope: Scope
    ): Either[String, Option[Shape]] =
      if (table.universal(stat, level)) Right(None)
      else
        MemberSyntax.names(stat).headOption match {
          case Some(name) if MemberSyntax.deferredInline(stat) =>
            observe(stat, level, name, scope).flatMap(
              _.flatMap(DeferredInline.refusal(_, s"$key.$name", useSite.tree, resolver))
                .toLeft(None)
            )
          case Some(name) => observe(stat, level, name, scope)
          case None       => Right(None)
        }

    // A member's observation where its level declares it: a member that only ever builds Unit is
    // none at all.
    private def observe(
        member: Tree,
        level: Frame,
        name: String,
        scope: Scope
    ): Either[String, Option[Shape]] =
      if (!visible(member, level))
        Left(s"capability record with members hidden at the use site: $key")
      else
        MemberSyntax.observation(member, s"$key.$name", s"$key.$name").flatMap {
          case MemberSyntax.Observation.Value(tpe) =>
            read(tpe, level, scope).map(Some(_))
          case MemberSyntax.Observation.Applied(callable, result) =>
            MemberSyntax.applied(
              callable,
              result,
              read(_, level, scope, callable.binders)
            )
        }

    // A member's type, read where its level declares it with `this` the record's template, over
    // the scope's opened witnesses. The member's own binders bind last, so an argument written
    // for a class parameter they shadow is not reread for them.
    private def read(
        tpe: Type,
        level: Frame,
        scope: Scope,
        binders: Map[String, Resolved] = Map.empty
    ): Resolved = {
      val at = scope.opened ++ scope.steps.find(_.declared.id == level.id).fold(env)(_.env)
      val scoped = context.inScope(level, at)
      resolver.resolve(
        tpe,
        (if (level.id == template.id) scoped.written(written) else scoped)
          .bind(binders)
          .copy(observer = Some(useSite))
          .as(MemberPath.self(template, env))
      )
    }

    // A field is public unless a modifier restricts it.
    private def visible(member: Tree, level: Frame): Boolean = member match {
      case p: Term.Param => !p.mods.exists(MemberSyntax.restricted)
      case other         => resolver.accessible(other, level, useSite)
    }

  }

  /** Where the record's members are read: the levels' walked environments, and the witnesses of the
    * opened abstract members, which every level's members share.
    */
  private case class Scope(steps: List[InheritedScopes.Step], opened: Map[String, Resolved])
  private object Scope { val empty: Scope = Scope(Nil, Map.empty) }

}

private[cardinality] object CapabilityRecords {

  /** A source class or trait the record reading covers: not a sealed family, an enum, a template
    * with only type members or a case class — those have their own readings — except a final case
    * class with more than its product (members, parents, more clauses, a constructor modifier): a
    * final class, whose record its constructor arguments are.
    */
  def applies(tree: Tree): Boolean = tree match {
    case t: Defn.Trait => !Counter.isSealed(t) && !Representations.typesOnly(t)
    case c: Defn.Class if c.mods.exists(_.is[Mod.Case]) =>
      FinalImplementations.isFinal(c) && !Representations.plainCase(c)
    case c: Defn.Class => !Counter.isSealed(c) && !Representations.typesOnly(c)
    case _             => false
  }

  // A final concrete class or a value class: its constructor arguments are its whole state.
  private def fixedClass(tree: Tree): Boolean = tree match {
    case c: Defn.Class => FinalImplementations.isFinal(c) || TypeApplications.valueClass(c)
    case _             => false
  }

  // The measured member's name and parameter count, where the use site is a member.
  private def measured(tree: Tree): Option[(String, Int)] = tree match {
    case d: (Defn.Def | Decl.Def | Defn.Val | Decl.Val) =>
      MemberSyntax.names(d).headOption.map(_ -> arityOf(d))
    case _ => None
  }

  private def arityOf(member: Tree): Int = MemberSyntax.parameters(MemberSyntax.groups(member)).size

  // A level's abstract term members; an abstract `var` is its getter (the setter is an effect).
  private def abstracts(level: Frame): List[Stat] =
    level.stats.collect { case d: (Decl.Def | Decl.Val | Decl.Var | Decl.Given) => d }

  private def neverInstantiated(tree: Tree): Boolean = tree match {
    case c: Defn.Class => Representations.finalAbstract(c)
    case _             => false
  }

  // A `var` parameter is a field holding the argument it was given, as a `val` one is: the model has
  // no mutation. A case class's first clause is fields; a later one is not.
  private def publicField(parameter: Term.Param, template: Frame): Boolean =
    ReceiverMembers.fieldParams(template).exists(_ eq parameter) &&
      !parameter.mods.exists(MemberSyntax.restricted)

  // `def m = p`, or `def m(x1, …, xn) = p(x1, …, xn)` with one clause of its own parameters.
  private def alias(member: Defn.Def, parameter: String): Boolean =
    (member.paramClauseGroups.flatMap(_.paramClauses), member.body) match {
      case (Nil, Term.Name(name))                                        => name == parameter
      case (List(clause), Term.Apply.After_4_6_0(Term.Name(name), args)) =>
        name == parameter && member.paramClauseGroups.forall(_.tparamClause.values.isEmpty) &&
        args.values.map(_.syntax) == clause.values.map(_.name.value)
      case _ => false
    }

}
