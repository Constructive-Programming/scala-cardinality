package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{MemberPath, MemberSyntax, ResolutionContext}
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
  *     all public `val`s and whose constructor every subclass reaches): the product of its open
  *     members — abstract or overridable — and `val` fields. An input observes exactly those (a
  *     derived member is concrete code over them); a goal may pick any tuple of them, since an
  *     anonymous subclass overriding each with the chosen term realizes it.
  *   - a *final* class (or value class) whose constructor is public and whose every constructor
  *     parameter is observable — a public `val`, or exposed by a public member that is its η-alias:
  *     the product of those parameters. An instance is its constructor arguments, every member a
  *     fixed function of them, and the aliases observe each argument injectively.
  *
  * Anything else — abstract type members, a self type, parents outside the sources, hidden state, a
  * constructor only some scope reaches — keeps a reason.
  */
final private[cardinality] class CapabilityRecords(resolver: Resolver, identity: OverrideIdentity) {
  import CapabilityRecords.*

  /** The record a source class or trait applied to `args` (as `name` spells it) is read as, at
    * `context`'s use site.
    */
  def resolve(name: String, entry: TypeEntry, args: List[Shape], context: ResolutionContext) = {
    val key = entry.key
    entry.self.toRight(s"abstract type or method-valued representation: $key").flatMap { template =>
      resolver.operators
        .applied(name, template.typeParams)(args)
        .flatMap { bound =>
          val env = resolver.typeParameters(template) ++ bound
          Reading(template, env, context.enter(key), key).record
        }
    }
  }

  /** One record being read: its template, the binders its arguments fix, and the use site. */
  final private case class Reading(
      template: Frame,
      env: Map[String, Resolved],
      context: ResolutionContext,
      key: String
  ) {
    private val useSite = context.useSite

    def record: Resolved =
      TypeMembers
        .selfType(template)
        .toLeft(())
        .flatMap(_ => walk)
        .flatMap(steps =>
          if (template.tree.exists(fixedClass)) finalRecord
          // A `final abstract` class is neither instantiated nor extended: it has no value.
          else if (template.tree.exists(neverInstantiated)) Right(Shape.Sum(Nil))
          else openRecord(steps)
        )

    private def walk: Either[String, List[InheritedScopes.Step]] =
      sequence(InheritedScopes.walk(List(template -> env), resolver, useSite))

    // A final class: its observable constructor parameters, each an argument a public
    // constructor takes freely.
    private def finalRecord: Resolved =
      ConstrainedReceivers.reason(template).toLeft(()).flatMap { _ =>
        sequence(template.params.map(observable)).map(Shape.Product(_))
      }

    private def fieldType(parameter: Term.Param): Resolved =
      parameter.decltpe.toRight(s"missing field type: $key").flatMap(read(_, template, env))

    private def observable(parameter: Term.Param): Resolved =
      fieldType(parameter).flatMap { field =>
        if (publicField(parameter)) Right(field)
        else
          Either.cond(
            exposures(parameter.name.value).exists(alias => observed(alias).contains(field)),
            field,
            s"final class field not observable injectively: $key.${parameter.name.value}"
          )
      }

    // What an alias observes: a parameterless one its result, an η-expansion its callable.
    private def observed(alias: Defn.Def): Option[Shape] =
      if (alias.paramClauseGroups.isEmpty) alias.decltpe.flatMap(read(_, template, env).toOption)
      else observe(alias, template, alias.name.value, Nil).toOption.flatten

    // The public members whose body is the parameter itself or its η-expansion.
    private def exposures(parameter: String): List[Defn.Def] =
      template.stats.collect {
        case d: Defn.Def if visible(d, template) && alias(d, parameter) => d
      }

    // An open template: its own `val` fields and its open members — a parent's overridable fields
    // and the fields of the parameterized levels no class of it passes arguments to among them —
    // each a free choice of its type. Levels are read in linearization order, so the declaration
    // a member's slot keeps is the nearest one, whose type the member has.
    private def openRecord(steps: List[InheritedScopes.Step]): Resolved = {
      val table = new ReceiverMembers(template, steps, identity, resolver)
      val all = table.levels
      val closures = TemplateClosures.of(template, all)
      refusal(all, closures).toLeft(()).flatMap { _ =>
        val open = table.open(None)
        val members = open.collect { case Right(c) => c.tree } ++
          all.flatMap(_.stats.collect { case d: (Decl.Def | Decl.Val | Decl.Given) => d })
        for {
          _ <- selfView(members, all).toLeft(())
          fields <- sequence(template.params.map(valField))
          concrete <- sequence(open.map(_.flatMap(c => observe(c.tree, c.level, c.name, steps))))
          declared <- declarations(all, table, steps)
        } yield Shape.Product(fields ++ (concrete ++ declared).flatten)
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

    private def refusal(all: List[Frame], closures: List[TemplateClosures.Closure]) = {
      import TemplateClosures.Closure
      val restricted = closures.collectFirst {
        case _: Closure.PrivateConstructor | _: Closure.Hidden =>
          s"capability record of a template only its scope instantiates: $key"
      }
      val foreign = ForeignParents.reasons(all, resolver).headOption
      val unreadMember = all.flatMap(_.stats).collectFirst {
        case _: Decl.Var | _: Defn.Var => s"capability record with a mutable member not read: $key"
        case _: Defn.ExtensionGroup => s"capability record with an extension member not read: $key"
      }
      val types = abstractTypes(all).map(t =>
        s"capability record with abstract type member not read: $key.$t"
      )
      val parameters = template.tree.collect {
        case t: Defn.Trait if t.ctor.paramClauses.exists(_.values.nonEmpty) =>
          s"capability record with trait parameters not read: $key"
      }
      restricted.orElse(foreign).orElse(parameters).orElse(unreadMember).orElse(types.headOption)
    }

    // A type member no level defines: a value's own choice, an existential this reading does not
    // open in a record.
    private def abstractTypes(all: List[Frame]): List[String] = {
      val stats = all.flatMap(_.stats)
      val defined = stats.collect { case d: Defn.Type => d.name.value }.toSet
      stats.collect { case d: Decl.Type if !defined(d.name.value) => d.name.value }.distinct
    }

    // A constructor parameter of an open class: a public `val` is a field any instance may give any
    // value of its type; hidden state is observed by no field, but a goal still needs it built.
    private def valField(parameter: Term.Param): Resolved =
      if (!publicField(parameter))
        Left(s"open class with hidden constructor state: $key.${parameter.name.value}")
      else fieldType(parameter)

    // The abstract declarations no concrete member implements, one per member: a declaration an
    // earlier level (the template first) restates is that member again.
    private def declarations(
        all: List[Frame],
        table: ReceiverMembers,
        steps: List[InheritedScopes.Step]
    ): Either[String, List[Option[Shape]]] = {
      val open = all.flatMap(level =>
        level.stats.collect { case d: (Decl.Def | Decl.Val | Decl.Given) => level -> d }
      )
      sequence(open.zipWithIndex.map {
        case ((level, stat), index) =>
          for {
            implemented <- table.implemented(stat, level, template.tree.getOrElse(stat))
            again <- restated(level, stat, open.take(index), steps)
            shape <-
              if (implemented || again) Right(None) else declared(level, stat, table, steps)
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
        steps: List[InheritedScopes.Step]
    ): Either[String, Option[Shape]] =
      if (table.universal(stat, level)) Right(None)
      else
        MemberSyntax.names(stat).headOption match {
          case Some(name) if MemberSyntax.deferredInline(stat) =>
            observe(stat, level, name, steps).flatMap(
              _.flatMap(DeferredInline.refusal(_, s"$key.$name", useSite.tree, resolver))
                .toLeft(None)
            )
          case Some(name) => observe(stat, level, name, steps)
          case None       => Right(None)
        }

    // A member's observation where its level declares it: a member that only ever builds Unit is
    // none at all.
    private def observe(
        member: Tree,
        level: Frame,
        name: String,
        steps: List[InheritedScopes.Step]
    ): Either[String, Option[Shape]] =
      if (!visible(member, level))
        Left(s"capability record with members hidden at the use site: $key")
      else
        MemberSyntax.observation(member, s"$key.$name", s"$key.$name").flatMap {
          case MemberSyntax.Observation.Value(tpe) =>
            read(tpe, level, envAt(level, steps)).map(Some(_))
          case MemberSyntax.Observation.Applied(callable, result) =>
            val at = envAt(level, steps) ++ callable.binders
            read(callable.tpe, level, at).flatMap(callable.close) match {
              case Left(_) if MemberSyntax.buildsUnit(result, read(_, level, at)) => Right(None)
              case other => other.map(Some(_))
            }
        }

    private def envAt(level: Frame, steps: List[InheritedScopes.Step]): Map[String, Resolved] =
      steps.find(_.declared.id == level.id).fold(env)(_.env)

    // A member's type, read where its level declares it with `this` the record's template.
    private def read(tpe: Type, level: Frame, at: Map[String, Resolved]): Resolved =
      resolver.resolve(
        tpe,
        context
          .inScope(level, at)
          .copy(observer = Some(useSite))
          .as(MemberPath.self(template, env))
      )

    // A field is public unless a modifier restricts it.
    private def visible(member: Tree, level: Frame): Boolean = member match {
      case p: Term.Param => !p.mods.exists(MemberSyntax.restricted)
      case other         => resolver.accessible(other, level, useSite)
    }

  }

}

private[cardinality] object CapabilityRecords {

  /** A source class or trait the record reading covers: not a sealed family, an enum, a case class
    * or a template with only type members — those have their own readings.
    */
  def applies(tree: Tree): Boolean = tree match {
    case t: Defn.Trait => !Counter.isSealed(t) && !Representations.typesOnly(t)
    case c: Defn.Class =>
      !Counter.isSealed(c) && !c.mods.exists(_.is[Mod.Case]) && !Representations.typesOnly(c)
    case _ => false
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

  private def neverInstantiated(tree: Tree): Boolean = tree match {
    case c: Defn.Class => Representations.finalAbstract(c)
    case _             => false
  }

  private def publicField(parameter: Term.Param): Boolean =
    parameter.mods.exists(_.is[Mod.ValParam]) && !parameter.mods.exists(MemberSyntax.restricted)

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
