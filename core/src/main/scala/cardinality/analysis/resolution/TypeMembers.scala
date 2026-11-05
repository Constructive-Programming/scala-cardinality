package cardinality.analysis.resolution

import scala.annotation.unused
import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, Resolved, TypeEntry}
import cardinality.analysis.Resolver
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{MemberPath, ResolutionContext}
import cardinality.types.{Kind, Kinds, Spelling, TypeApplications}

import TemplateClosures.Closure

/** Type members read on stable paths (members M0–M2, M5).
  *
  * A type member is a member of a path: `this` of a template, a parameter, a declared `val`. Along
  * the path's linearization — its refinement, its template, the template's supplied parents — an
  * alias is the member's meaning wherever it is declared (Scala lets an alias be overridden only by
  * an equal one) and is read where it is declared, with `this` still the path. A member every level
  * leaves abstract, unbounded and first order, is a rigid atom keyed by the path and the name: the
  * path is the opening of an existential package (Mitchell–Plotkin; Cardelli–Leroy dot notation),
  * so two paths never share it, and a singleton alias `q: p.type` is the same path. Its count is
  * the count of the same template with the member as a fresh type parameter: a subclass picks the
  * member as a caller picks an argument.
  *
  * That parity needs a free choice, so a member the path's template cannot leave to a subclass
  * written anywhere is refused (`Closure`), as are bounded and higher-kinded members, whose
  * semantics this reading does not carry, and a member some level declares as a class. A private
  * member is not inherited: read where it is declared, it is that template's own, whatever receiver
  * inherits the reading. Inside a refinement `p: T { … }`, `this` and the members the refinement or
  * T declares are p's (Scala types the refinement as a class extending T).
  */
final private[cardinality] class TypeMembers(
    lookup: SourceLookup,
    inherited: InheritedTypeMembers,
    resolver: Resolver
) {
  import TypeMembers.*

  private val linearizations =
    mutable.Map.empty[(String, String, Map[String, Resolved]), Levels]

  /** What a type name or a path selection means when it denotes a type member; None when it denotes
    * none this reading follows (a binder, a class, a package-level alias, an unknown path).
    */
  def of(tpe: Type, context: ResolutionContext): Option[Either[String, Meaning]] = tpe match {
    case n: Type.Name if refinedMember(n.value, context) =>
      context.refinedThis.map(meaning(_, n.value, context))
    case n: Type.Name if !context.variables.contains(n.value) =>
      inherited.locate(n.value, context) match {
        case Some(Right((receiver, (entry, _)))) if memberTree(entry.tree) =>
          Some(on(receiver, n.value, context))
        case Some(_)                                    => None
        case None if !declaredInChain(n.value, context) => None
        case None                                       =>
          resolver.resolveName(n.value, context) match {
            case Found.Entry(entry) => lexical(entry, n.value, context)
            case _                  => None
          }
      }
    case Type.Select(qualifier, name) =>
      path(qualifier, context).map(_.flatMap(meaning(_, name.value, context)))
    case _ => None
  }

  /** A member's meaning as a shape: an alias read where it is declared, or its rigid atom. */
  def denoted(meaning: Meaning, context: ResolutionContext): Resolved =
    meaning
      .fold(
        (body, scope) => scope.read(body, resolver),
        atom => Right(Shape.Atom(atom)),
        (atom, _) => Left(s"higher-kinded type member used as a proper type: $atom")
      )
      .flatMap(SingletonIntersections.validate(_, context.useSite, resolver))

  // Inside a refinement, a name the refined value declares is its member (Scala reports one an
  // enclosing scope of the same file also defines as ambiguous, so a valid program means this).
  private def refinedMember(name: String, context: ResolutionContext): Boolean =
    context.refinedThis.exists(declarationsOf(_, name).nonEmpty)

  // A declaration a lexical lookup found, read as a member of its template's `this` where it is
  // one: an abstract member always, an alias where `this` is another receiver's (members M2).
  private def lexical(
      entry: TypeEntry,
      name: String,
      context: ResolutionContext
  ): Option[Either[String, Meaning]] =
    Option.when(templateMember(entry, context))(on(entry.owner, name, context))

  private def templateMember(entry: TypeEntry, context: ResolutionContext): Boolean =
    isTemplate(entry.owner) &&
      (entry.tree.is[Decl.Type] || (memberTree(entry.tree) && redirected(entry.owner, context)))

  // Whether a template of the reference's chain declares the name as a type member: the only names
  // a lexical lookup can bind to one.
  private def declaredInChain(name: String, context: ResolutionContext): Boolean =
    context.frame.chain.exists(frame =>
      isTemplate(frame) && lookup.declaredIn(frame, name).exists(entry => memberTree(entry.tree))
    )

  private def on(template: Frame, name: String, context: ResolutionContext) =
    meaning(selfPath(template, context), name, context)

  // `this` of a template whose body is read as a member of another receiver is that receiver
  // (members M2) — for the innermost template only: an outer `O.this`, or an outer self alias, is
  // the enclosing instance, whichever receiver inherits the body.
  private def receiverPath(template: Frame, context: ResolutionContext): Option[MemberPath] =
    context.self.filter(path =>
      innermost(context).exists(_.id == template.id) && covers(path, template)
    )

  private def selfPath(template: Frame, context: ResolutionContext): MemberPath =
    receiverPath(template, context).getOrElse(
      MemberPath.self(template, resolver.typeParameters(template))
    )

  // `this` is another receiver, or the template's own at arguments of a capability record: an alias
  // body then reads the record's arguments, never the template's own binders nor the binders of
  // the member being read.
  private def redirected(template: Frame, context: ResolutionContext): Boolean =
    receiverPath(template, context).exists(path =>
      path.key != MemberPath.selfKey(template) ||
        path.template.exists(_._2 != resolver.typeParameters(template))
    )

  private def innermost(context: ResolutionContext): Option[Frame] =
    context.frame.chain.find(isTemplate)

  private def covers(path: MemberPath, template: Frame): Boolean =
    levels(path).exists(_.exists(_._1.id == template.id))

  // The meaning of member `name` on a path: the first alias along its linearization (a private one
  // of the template being read first), or the member atom where every level leaves it abstract and
  // free.
  private def meaning(
      path: MemberPath,
      name: String,
      context: ResolutionContext
  ): Either[String, Meaning] = {
    val key = path.atom(name)
    // Only a cyclic alias, which Scala rejects, reads a member inside its own reading.
    if (context.visiting(key)) Left(s"recursive type member: ${path.spell(name)}")
    else
      candidates(path, name, context).flatMap { found =>
        found
          .collectFirst(Function.unlift(c => definition(c.stat).map(_ -> c.scope)))
          .map((body, scope) => Right(Meaning.Alias(body, scope.enter(key))))
          .getOrElse(
            abstractMeaning(path, name, found.collect { case Candidate(d: Decl.Type, _) => d })
          )
      }
  }

  // The declarations of the member along the path, each with the scope its body is read in: each
  // level of the template's linearization, where `this` is the path. A private one is not inherited.
  private def candidates(
      path: MemberPath,
      name: String,
      context: ResolutionContext
  ): Either[String, List[Candidate]] = {
    path.template.fold(Right(Nil)) { (template, _) =>
      levels(path).map { found =>
        found.flatMap { (frame, env) =>
          lookup.declaredIn(frame, name).flatMap { entry =>
            Option
              .when(frame.id == template.id || publicAt(entry, context.useSite))(entry.tree)
              .collect { case stat: Stat => Candidate(stat, context.inScope(frame, env).as(path)) }
          }
        }
      }
    }
  }

  private def publicAt(entry: TypeEntry, at: Frame): Boolean =
    inherited.visible(entry.tree, entry.owner, at)

  // Bounded, higher-kinded and unfree members are refused.
  private def abstractMeaning(
      path: MemberPath,
      name: String,
      declared: List[Decl.Type]
  ): Either[String, Meaning] = {
    val member = path.spell(name)
    if (declared.isEmpty)
      Left(s"unavailable type member: $member")
    else
      declared.find(!trivial(_)) match {
        case Some(d) => Left(s"bounded abstract type member $member (${Spelling.of(d.bounds)})")
        case None    =>
          unsupplied(path, name)
            .orElse(unfree(path, name))
            .orElse(path.fixed.map(why => s"abstract type member $member is fixed: $why"))
            .toLeft(declared.find(_.tparamClause.values.nonEmpty) match {
              case Some(d) =>
                Meaning.RigidOperator(
                  path.atom(name),
                  Kind.Arrow(d.tparamClause.values.map(Kinds.of))
                )
              case None => Meaning.Rigid(path.atom(name))
            })
      }
  }

  private def unsupplied(path: MemberPath, name: String): Option[String] =
    path.template
      .flatMap((template, _) => inherited.unsuppliedAncestors(template).headOption)
      .map(parent =>
        s"type member $name of ${path.label} may be defined by unsupplied parent $parent"
      )

  // Why no subclass may pick the member (`Closure`). A sealed family's cases fix it only on a path
  // of the family's type, and only where every case is closed: inside the family `this` is its
  // receiver, which the sealed-receiver gate (G2) answers for, and an open case lets any file pick.
  private def unfree(path: MemberPath, name: String): Option[String] =
    path.template.flatMap { (template, _) =>
      val owner = template.path.last
      closures(template).collectFirst {
        case Closure.Sealed if !path.isSelf && !resolver.openFamily(template) =>
          s"abstract type member $name of ${path.label} is fixed by the cases of sealed $owner"
      }
    }

  private def closures(template: Frame): List[Closure] =
    TemplateClosures.of(
      template,
      levels(MemberPath.self(template, resolver.typeParameters(template))).toOption.toList.flatten
        .map(_._1)
    )

  // The template and its supplied parents, each with the arguments the path gives it, memoized per
  // path and arguments: `this` of a capability record is the template's own path at the record's
  // arguments, one key read at many instantiations.
  private def levels(path: MemberPath): Levels =
    path.template.fold[Levels](Right(Nil)) { (own, env) =>
      linearizations.getOrElseUpdate(
        (path.key, own.id, env), {
          val steps = InheritedScopes.walk(List(own -> env), resolver, own)
          steps.collectFirst { case Left(reason) => reason }.toLeft {
            (own -> env) :: steps.collect { case Right(step) => step.declared -> step.env }
          }
        }
      )
    }

  // The stable path a qualifier denotes, when it denotes one this reading follows: `this` (inside a
  // refinement, the refined value), a self alias, or a parameter or declared `val`, through
  // singleton aliases. A name is its innermost binding: a parameter or value of a nearer scope
  // hides an enclosing template's self alias, and a refined value's own member hides both.
  private def path(
      qualifier: Term,
      context: ResolutionContext
  ): Option[Either[String, MemberPath]] =
    qualifier match {
      case Term.Name(name) =>
        val chain = context.frame.chain.map(_.id)
        val bound = RefinedMembers.declaring(name, context.frame).map(f => chain.indexOf(f.id))
        selfAlias(name, context)
          .filterNot(t => bound.exists(_ <= chain.indexOf(t.id)))
          .map(t => selfType(t).toLeft(selfPath(t, context)))
          .orElse(binding(name, context))
      case _ => None
    }

  // `outer =>` names `this` of its template.
  private def selfAlias(name: String, context: ResolutionContext): Option[Frame] =
    context.frame.chain.find(frame =>
      frame.tree
        .flatMap(TypeApplications.template)
        .exists(_.body.selfOpt.exists(_.name.value == name))
    )

  // A parameter or `val` the chain declares; one whose declared type names no template this
  // reading has is left to the refined-member reading, which names what it lacks.
  private def binding(
      name: String,
      context: ResolutionContext
  ): Option[Either[String, MemberPath]] =
    RefinedMembers.pathType(Term.Name(name), context.frame).flatMap {
      case Left(reason)             => Some(Left(reason))
      case Right((declared, owner)) =>
        if (isTemplate(owner)) Some(Left(s"path through the field $name not followed"))
        else valuePath(name, Value(s"${owner.id}:$name", declared, owner), context)
    }

  private def valuePath(
      name: String,
      value: Value,
      context: ResolutionContext
  ): Option[Either[String, MemberPath]] = {
    val scope = context.inScope(value.level, resolver.typeParameters(value.level))
    declaredPath(Named(value.key, name), value.declared, scope)
      .map(_.map(path => path.copy(fixed = path.fixed.orElse(fixedValue(name, value)))))
  }

  // Whoever supplies a path may pick its abstract members only where the path's value is theirs: a
  // method's parameter (the caller passes any subclass, refined). A local value is fixed by its own
  // definition: its members admit no values but its own.
  private def fixedValue(name: String, value: Value): Option[String] =
    Option.when(value.level.params.forall(_.name.value != name))(s"$name is a local value")

  /** The type a stable input is read as. A type-only refinement pins members of the input's path,
    * which a reading of the path follows, and asks nothing else of the value: it is read as its
    * base where some value of the base has the refined members — the base is an open template a
    * subclass may give them, and no refined member is fixed or bounded in a way the base may
    * contradict. Otherwise the refined type may have no value at all, and the input is refused
    * rather than counted as one of its base's.
    */
  def stableInput(tpe: Type, context: ResolutionContext): Either[String, Type] = tpe match {
    case r: Type.Refine if r.body.stats.forall(typeMember) =>
      r.tpe match {
        case None       => Left(notShown(r, "no base"))
        case Some(base) =>
          declaredPath(Named(s"input:${r.syntax}", Spelling.of(r)), r, context)
            .fold(Left(notShown(r, s"${Spelling.of(base)} is not a supplied template")))(
              _.map(_ => base)
            )
      }
    case _ => Right(tpe)
  }

  /** A refinement read as a value type outside a stable input: its constraints are not erased. */
  def refinement(r: Type.Refine, @unused context: ResolutionContext): Resolved =
    Left(s"refined representation unavailable (constraints not erased): ${Spelling.of(r)}")

  /** Whether a type declares or inherits a type member: it may, as far as this reading tells. */
  def declaresMember(owner: Type, name: String, context: ResolutionContext): Boolean = true

  // Why no value of a supplied base may have a refinement's members, if none may: the base cannot
  // be extended freely, or already declares a refined member in a way the refinement may contradict
  // (an alias, even to the same type — telling the two apart is not attempted).
  private def refinable(r: Type.Refine, base: MemberPath): Option[String] = {
    val closed = base.template.flatMap { (template, _) =>
      val owner = template.path.last
      val unsupplied = inherited.unsuppliedAncestors(template).map(Closure.Unsupplied(_))
      (closures(template) ++ unsupplied).headOption.map(_ => s"$owner is not extended freely")
    }
    closed
      .orElse(levels(base).left.toOption)
      .orElse(r.body.stats.flatMap(stat => declaredName(stat).map(stat -> _)).collectFirst {
        case (stat, name) if declarationsOf(base, name).exists(clashes(stat, _)) =>
          s"${base.template.fold(base.label)(_._1.path.last)} already defines $name"
      })
      .map(notShown(r, _))
  }

  // Every declaration of a member along a path: its refinement's, and each level's it inherits.
  private def declarationsOf(path: MemberPath, name: String): List[Stat] =
    path.refinement.map(_.stat).filter(declares(_, name)) ++ path.template.toList.flatMap {
      (template, _) =>
        levels(path).toOption.toList.flatten.flatMap { (frame, _) =>
          lookup
            .declaredIn(frame, name)
            .filter(publicAt(_, template))
            .map(_.tree)
            .collect { case stat: Stat => stat }
        }
    }

  // The path a declared type gives a binding: a singleton alias is the path it names, a refinement
  // adds its members to its base's, and a class or trait (directly or through an alias) is the
  // template whose linearization defines the rest.
  private def declaredPath(
      at: Named,
      declared: Type,
      scope: ResolutionContext
  ): Option[Either[String, MemberPath]] = {
    val step = s"path:${at.key}:${declared.syntax}"
    // Only a cyclic path type, which Scala rejects, declares a path through itself.
    if (scope.visiting(step)) Some(Left(s"recursive path type: ${at.label}"))
    else
      declared match {
        case r: Type.Refine if r.body.stats.forall(typeMember) =>
          val refined = r.body.stats.map(MemberPath.Refined(_, scope.enter(step)))
          (unshown(r.body.stats), r.tpe) match {
            case (None, Some(base)) =>
              declaredPath(at, base, scope.enter(step)).map(_.flatMap { p =>
                refinable(r, p).toLeft(p.copy(refinement = refined ++ p.refinement))
              })
            case _ => Some(Left(notShown(r, "refined members not read")))
          }
        case _ => named(at, declared, scope.enter(step))
      }
  }

  private def named(at: Named, declared: Type, scope: ResolutionContext) = {
    val (callee, args) = declared match {
      case a: Type.Apply => a.tpe -> a.argClause.values
      case other         => other -> Nil
    }
    callee match {
      case _: Type.Name | _: Type.Select =>
        // A declared type the lookup refuses leaves the path refused for that reason: no other
        // reading of the path is more specific.
        resolver.declaration(callee, TypeApplications.spelled(callee), scope) match {
          case Found.Entry(entry) => template(at, entry, args, scope)
          case _                  => None
        }
      case _ => None
    }
  }

  private def template(
      at: Named,
      entry: TypeEntry,
      args: List[Type],
      scope: ResolutionContext
  ): Option[Either[String, MemberPath]] = (entry.tree, entry.self) match {
    case (d: (Defn.Class | Defn.Trait), Some(frame)) =>
      Some(
        bound(entry.name, d.tparamClause.values, args, scope)
          .map(env => at.path(Some(frame -> (resolver.typeParameters(frame) ++ env))))
      )
    case _ => None
  }

  // A declaration's parameters bound to the arguments of its application, each read by its kind.
  private def bound(
      name: String,
      parameters: List[Type.Param],
      args: List[Type],
      scope: ResolutionContext
  ): Either[String, Map[String, Resolved]] =
    TypeApplications
      .checkApplication(name, parameters, args.size)
      .map(_ =>
        parameters
          .zip(args)
          .map((p, a) => p.name.value -> resolver.operators.argument(Kinds.of(p), a, scope))
          .toMap
      )

}

private[cardinality] object TypeMembers {

  /** A type member's meaning: an alias body and the scope it is read in, or a rigid atom. */
  enum Meaning {
    case Alias(body: Type, scope: ResolutionContext)
    case Rigid(atom: String)

    /** A higher-kinded abstract member: the path atom as a rigid constructor, applied at its kind's
      * arity (`Shape.rigid`) — the witness the path's package binds, read the way the method-side
      * reads its own higher-kinded binders.
      */
    case RigidOperator(atom: String, kind: Kind)

    /** The meaning read one of three ways: an alias body in its scope, the atom, or the constructor
      * the path binds.
      */
    def fold[A](
        alias: (Type, ResolutionContext) => A,
        rigid: String => A,
        operator: (String, Kind) => A
    ): A = this match {
      case Alias(body, scope)        => alias(body, scope)
      case Rigid(atom)               => rigid(atom)
      case RigidOperator(atom, kind) => operator(atom, kind)
    }

  }

  private type Levels = Either[String, List[(Frame, Map[String, Resolved])]]

  // A declaration of the member at one level, and the scope its body is read in.
  final private case class Candidate(stat: Stat, scope: ResolutionContext)

  // A binding's path identity and how a reason names it.
  final private case class Named(key: String, label: String) {

    def path(template: Option[(Frame, Map[String, Resolved])]): MemberPath =
      MemberPath(key, label, template)

  }

  // A value a path starts from: its key, and its declared type at the scope that declares it.
  final private case class Value(key: String, declared: Type, level: Frame)

  // A declaration a member reading follows: an abstract type, or a first-order transparent alias.
  private def memberTree(tree: Tree): Boolean = tree match {
    case _: Decl.Type => true
    case d: Defn.Type => alias(d)
    case _            => false
  }

  private def alias(d: Defn.Type): Boolean =
    !d.mods.exists(_.is[Mod.Opaque]) && d.tparamClause.values.isEmpty &&
      TypeApplications.lambda(d.body).isEmpty

  // The type a declaration fixes its member to: an alias's body, or an exact bound pair's.
  private def definition(stat: Stat): Option[Type] = stat match {
    case d: Defn.Type if alias(d) => Some(d.body)
    case d: Decl.Type => d.bounds.lo.filter(lo => d.bounds.hi.exists(_.structure == lo.structure))
    case _            => None
  }

  def typeMember(stat: Stat): Boolean = stat.is[Decl.Type] || stat.is[Defn.Type]

  /** Why a type-only refinement's own members may describe no value, if they may: a bound pair the
    * reading cannot show non-empty (`>: Int <: String` has no instance; one bound alone is met by
    * `Nothing` or `Any`), or a member it does not follow.
    */
  def unshown(stats: List[Stat]): Option[String] = stats.collectFirst {
    case d: Defn.Type if !alias(d) => s"higher-kinded or opaque refined member: ${d.name.value}"
  }

  /** Whether some instance gives every abstract member a type: with one bound or none each, Nothing
    * meets every upper bound and Any every lower one; two bounds may admit nothing.
    */
  def instantiable(stats: List[Stat]): Boolean = true

  // Whether a refined member may contradict a declaration of the base: one that fixes the member
  // meets anything but an unbounded abstract member; a one-sided bound (`<: U`, met by Nothing;
  // `>: L`, met by Any) meets an alias, a class, or a bound on the other side.
  private def clashes(refined: Stat, existing: Stat): Boolean =
    definition(refined).isEmpty || !trivialStat(existing)

  // A one-sided refined bound against a declared bound on the other side.
  private def notShown(r: Type.Refine, why: String): String =
    s"refinement not shown satisfiable: ${Spelling.of(r)} ($why)"

  private def declares(stat: Stat, name: String): Boolean = declaredName(stat).contains(name)

  private def declaredName(stat: Stat): Option[String] = stat match {
    case d: Defn.Type => Some(d.name.value)
    case _            => None
  }

  /** A self type adds members `this` does not inherit, which this reading does not enumerate. */
  def selfType(template: Frame): Option[String] =
    template.tree
      .flatMap(TypeApplications.template)
      .flatMap(_.body.selfOpt.flatMap(_.decltpe))
      .map(tpe => s"self type not resolved: ${Spelling.of(tpe)}")

  private def isTemplate(frame: Frame): Boolean = frame.isTemplate

  private def trivial(d: Decl.Type): Boolean = d.bounds.lo.isEmpty && d.bounds.hi.isEmpty

  private def trivialStat(stat: Stat): Boolean = stat match {
    case d: Decl.Type => trivial(d) && d.tparamClause.values.isEmpty
    case _            => false
  }

}
