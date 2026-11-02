package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.model.{MemberSyntax, ResolutionContext}
import cardinality.analysis.resolution.*
import cardinality.types.TypeApplications

import MethodAnalysis.Frame
import TargetViews.{Applied, Resolution, View}

/** The concrete members of one receiver `this` — a template owner of the measured declaration —
  * over its supplied linearization, classified (capabilities C1, C2, C5).
  *
  * A concrete member is *effective* when no level nearer the receiver redefines it, and a
  * declaration a concrete member implements is that member. A parent's constructor `val` is a
  * concrete member too (a subclass may override it); the receiver's own are its constructor's
  * parameters. An effective member no instance of the receiver can replace (a fixed receiver, a
  * `final`, `private` or `inline` member, a given or a member object) is *derived*: concrete code
  * computing from the environment it shares, which adds no choice. One an instance may override is
  * *open*: across admissible receivers it is every behavior its type allows — a subclass whose
  * override returns a stored function realizes any tuple of member behaviors — so it is a free
  * capability like an abstract one, its default body only one of those behaviors. An open member
  * whose default re-enters the measured declaration is a view (`TargetViews`): excluded when
  * definite, a reason when possible. A default that only throws is no behavior at all (register
  * D26): the member reads exactly as its abstract twin. A parameterized parent no class of the
  * linearization passes arguments to leaves them to the class that finally extends the receiver:
  * its fields are free state, and state no public field observes is a reason.
  */
final private[cardinality] class ReceiverMembers(
    root: Frame,
    steps: List[InheritedScopes.Step],
    identity: OverrideIdentity,
    resolver: Resolver
) {
  import ReceiverMembers.*

  private val linearization = new Linearization(root :: steps.map(_.declared), resolver)

  /** The receiver's supplied levels in linearization order, the receiver first. */
  val levels: List[Frame] = linearization.order

  private val concretes: List[Concrete] = levels.flatMap { level =>
    val fields = if (level.id == root.id) Nil else fieldParams(level)
    level.stats.flatMap(stat => Concrete.of(level, stat)) ++
      fields.map(p => Concrete(level, p, p.name.value))
  }

  /** Whether an inherited declaration of `level` is implemented by a concrete member or a
    * constructor `val` of another level: then that member, not the declaration, is the receiver's.
    * Whether the measured declaration implements it is the target's own identity question
    * (`OverrideIdentity.matches`).
    */
  def implemented(declaration: Stat, level: Frame, target: Tree): Either[String, Boolean] = {
    val names = MemberSyntax.names(declaration)
    val field = MemberSyntax.groups(declaration).isEmpty &&
      levels.exists(l => l.id != level.id && names.exists(fields(l)))
    val candidates =
      concretes.filter(c => c.level.id != level.id && names.contains(c.name) && !(c.tree eq target))
    if (field) Right(true)
    else
      declaration match {
        case d: Decl.Def =>
          settle(candidates.map(c => sameSlot((d, level), (c.tree, c.level))), d.name.value)
        case _: (Decl.Val | Decl.Var | Decl.Given) => Right(candidates.exists(_.parameterless))
        case _                                     => Right(false)
      }
  }

  /** The effective open members of the receiver, each with its level, and why any other member or
    * any free state leaves the environment unread. `measured` is the declaration whose views are
    * judged: this receiver's own member, or one of a template it encloses.
    */
  def open(measured: Option[Measured]): List[Either[String, Concrete]] = {
    val closures = TemplateClosures.of(root, levels)
    val views = measured.fold(Map.empty[Concrete, View])(viewsOf)
    destructured(closures) ++ hiddenParameters ++ concretes.flatMap { c =>
      overridden(c) match {
        case Left(reason)                => List(Left(reason))
        case Right(true) if freeState(c) => List(Left(hiddenState(c.level, c.name)))
        case Right(true)                 => Nil
        case Right(false)                => verdict(c, closures, views, measured).toList
      }
    }
  }

  // An effective member: none for the declaration itself, a universal member, or a derived one;
  // a reason for a refusal or a possible view; the member when it is open and no definite view.
  private def verdict(
      c: Concrete,
      closures: List[TemplateClosures.Closure],
      views: Map[Concrete, View],
      measured: Option[Measured]
  ): Option[Either[String, Concrete]] =
    if (measured.exists(_.tree eq c.tree) || universal(c.tree, c.level)) None
    else
      classification(c, closures) match {
        case Some(refused) => refused.left.toOption.map(Left(_))
        case None          =>
          views.getOrElse(c, View.Free) match {
            case View.Definite => None
            case View.Possible =>
              Some(Left(s"overridable member may re-enter the measured declaration: ${label(c)}"))
            case View.Free => Some(Right(c))
          }
      }

  // A field of a level no class of the linearization passes arguments to: every value of its type,
  // whichever class finally extends the receiver passes it.
  private def freeState(c: Concrete): Boolean = c.field && !passed(c.level)

  // The constructor parameters of unpassed levels that no field observes: free state hidden from
  // every member but the level's own code.
  private def hiddenParameters: List[Either[String, Concrete]] =
    levels.drop(1).filterNot(passed).flatMap { level =>
      val fields = fieldParams(level)
      level.params
        .filterNot(p => fields.exists(_ eq p))
        .map(p => Left(hiddenState(level, p.name.value)))
    }

  private def hiddenState(level: Frame, name: String): String =
    s"open class with hidden constructor state: ${level.path.mkString(".")}.$name"

  // Some class of the linearization names the level among its parents: a class passes every
  // parameterized parent its arguments, explicitly or by default.
  private def passed(level: Frame): Boolean =
    levels.exists(other =>
      other.tree.exists(_.is[Defn.Class]) && linearization.parents(other.id).contains(level.id)
    )

  /** Whether a member of a level restates a universal member of `Any` (C0: outside the model). */
  def universal(member: Tree, level: Frame): Boolean =
    MemberSyntax.universal(member)(tpe =>
      TopTypes.isAny(
        tpe,
        ResolutionContext(level, resolver.typeParameters(level), Set.empty),
        resolver
      )
    )

  // A pattern definition defines members this reading does not name one by one; on a receiver an
  // instance may override them, they are unread.
  private def destructured(
      closures: List[TemplateClosures.Closure]
  ): List[Either[String, Concrete]] =
    if (resolver.fixedTemplate(root) || closures.contains(TemplateClosures.Closure.Final)) Nil
    else
      levels.flatMap(_.stats).collect {
        case v: Defn.Val if !v.pats.forall(_.is[Pat.Var]) && !Concrete.unoverridable(v) =>
          Left(s"destructured member of an open receiver not read: ${root.path.mkString(".")}")
      }

  // Whether a nearer level redefines the member: a level before the member's own in the receiver's
  // linearization (the receiver included) defining the same slot, or the receiver's own
  // constructor `val`.
  private def overridden(c: Concrete): Either[String, Boolean] =
    if (c.level.id != root.id && c.parameterless && fields(root)(c.name)) Right(true)
    else {
      val nearer = linearization.preceding(c.level.id)
      val candidates = concretes.filter(o => nearer(o.level.id) && o.name == c.name)
      settle(candidates.map(o => sameSlot((c.tree, c.level), (o.tree, o.level))), c.name)
    }

  // Two members of one name are one slot: methods by override identity; a value, a field, an
  // object, a given or a parameterless method and another such are one member; a value and a
  // method with parameters are two.
  private def sameSlot(left: (Tree, Frame), right: (Tree, Frame)): Option[Boolean] =
    (left._1, right._1) match {
      case (l: (Decl.Def | Defn.Def), r: (Decl.Def | Defn.Def)) =>
        identity.sameMember((l, left._2), (r, right._2), root, steps)
      case (l, r) => Some(Signature.of(l).clauses == 0 && Signature.of(r).clauses == 0)
    }

  // Derived (`Some(Right)`), refused (`Some(Left)`), or open (`None`). Free state is open unless no
  // public field observes it.
  private def classification(
      c: Concrete,
      closures: List[TemplateClosures.Closure]
  ): Option[Either[String, Unit]] =
    if (freeState(c)) Option.when(c.hidden)(Left(hiddenState(c.level, c.name)))
    else if (
      resolver.fixedTemplate(root) || closures.contains(TemplateClosures.Closure.Final) || c.fixed
    )
      Some(Right(()))
    else if (TemplateClosures.fileScoped(root))
      Some(
        Either.cond(
          !fileOverrides.overridden(c.name),
          (),
          s"member of a template only its file extends may be overridden there: ${label(c)}"
        )
      )
    else None

  private lazy val fileOverrides = new FileOverrides(root, levels, resolver)

  private lazy val calls = new ReceiverCalls(levels, resolver)

  private def label(c: Concrete): String = s"${root.path.mkString(".")}.${c.name}"

  // Views of the measured declaration among every concrete member's default and every nested
  // template's body (derived members carry a view to the open members calling them).
  //
  // Implicit search is not syntax: `summon[T]`, a `using` clause filled implicitly or a conversion
  // may evaluate any given or implicit member of the receiver. So every body that may search — any
  // but a literal; a stored field reads none — mentions those members, and one of them that may
  // re-enter the declaration makes the body a possible view.
  private def viewsOf(measured: Measured): Map[Concrete, View] = {
    val slots = new Slots(measured)
    val trees = concretes.map(c => c.level -> c.tree) ++ slots.templates ++ slots.anonymous
    val nodes = trees.map((level, tree) => (slots.of(level, tree), tree, slots.reads(level, tree)))
    val implicits =
      nodes.collect { case (slot, tree, _) if MemberSyntax.implicitMember(tree) => slot }.toSet
    val searched = nodes.map { (slot, _, reads) =>
      slot -> (if (reads.searches) reads.copy(possible = reads.possible ++ implicits) else reads)
    }
    concretes.zip(TargetViews.classify(slots.target, searched)).toMap
  }

  // A receiver's term members and nested templates, grouped into slots: one slot per member
  // (override identity across levels), each nested class, trait or object its own.
  final private class Slots(measured: Measured) {
    private val ownTemplate = measured.template.id == root.id

    private val entries: List[(Frame, Tree, String)] = levels.flatMap { level =>
      level.stats.flatMap(stat => MemberSyntax.names(stat).map((level, stat, _))) ++
        fieldParams(level).map(p => (level, p, p.name.value))
    }

    val templates: List[(Frame, Tree)] = levels.flatMap { level =>
      level.stats.collect { case t @ (_: Defn.Class | _: Defn.Trait | _: Defn.Enum) => level -> t }
    }

    /** The anonymous givens: no name reaches them, implicit search does. */
    val anonymous: List[(Frame, Tree)] = levels.flatMap { level =>
      level.stats.collect {
        case g: (Defn.GivenAlias | Defn.Given) if g.name.value.isEmpty => level -> g
      }
    }

    // Each name's slots, and whether some identity among its members is unknown.
    private val (grouped, unsettled) = {
      val byName = entries.groupBy(_._3).toList.map { (name, found) =>
        val (groups, unknown) = found.foldLeft((List.empty[List[(Tree, Frame)]], false)) {
          case ((groups, unknown), (level, tree, _)) =>
            val answers = groups.map(g => sameSlot((tree, level), g.head))
            answers.indexOf(Some(true)) match {
              case -1 => (groups :+ List(tree -> level), unknown || answers.contains(None))
              case at => (groups.updated(at, groups(at) :+ (tree -> level)), unknown)
            }
        }
        (name, groups, unknown)
      }
      (
        byName.flatMap((name, groups, _) => groups.map(name -> _)),
        byName.collect { case (name, _, true) => name }.toSet
      )
    }

    private def typeId(index: Int): Int = grouped.size + index

    /** The measured declaration's slot: its own on this receiver, a slot of its own otherwise. */
    val target: Int =
      if (ownTemplate) grouped.indexWhere(_._2.exists(_._1 eq measured.tree)) else -1

    def of(level: Frame, tree: Tree): Int =
      templates.indexWhere(_._2 eq tree) match {
        case -1    => grouped.indexWhere(_._2.exists((t, l) => (t eq tree) && l.id == level.id))
        case index => typeId(index)
      }

    // A body that may search implicits may also select an enclosing template's given or implicit
    // member, which reaches the declaration as `Outer.this.g` would.
    def reads(level: Frame, tree: Tree): TargetViews.Reads = {
      val self = TargetViews.Self(selves(level), level.path.lastOption.toSet)
      val names = scope(level)
      val read = TargetViews.reads(bodyOf(tree), self, names)
      read.copy(outer = read.outer || (read.searches && outerImplicits(level, names)))
    }

    private def outerImplicits(level: Frame, names: TargetViews.Scope): Boolean =
      level.chain.tail.filter(_.isTemplate).exists { frame =>
        frame.stats.filter(MemberSyntax.implicitMember).exists { stat =>
          frame.path.lastOption.exists(names.outer(_, MemberSyntax.names(stat).mkString))
        }
      }

    // A field is a stored value: reading it runs no code.
    private def bodyOf(tree: Tree): TargetViews.Body = tree match {
      case d: Defn.Def =>
        TargetViews.Body(d.body, MemberSyntax.parameterNames(d.paramClauseGroups), d.decltpe)
      case g: Defn.GivenAlias =>
        TargetViews.Body(g.body, MemberSyntax.parameterNames(g.paramClauseGroups), Some(g.decltpe))
      case v: Defn.Val   => TargetViews.Body(v.rhs, Set.empty, v.decltpe)
      case v: Defn.Var   => TargetViews.Body(v.body, Set.empty, v.decltpe)
      case _: Term.Param => TargetViews.Body(Lit.Unit(), Set.empty, None)
      case other         => TargetViews.Body(other, Set.empty, None)
    }

    private def scope(level: Frame): TargetViews.Scope = new TargetViews.Scope {
      private val visible = linearization.bases(level.id)

      def own(name: String, applied: Applied): Option[Resolution] = {
        val candidates = grouped.zipWithIndex.collect {
          case ((`name`, members), id) if members.exists((_, l) => visible(l.id)) =>
            id -> Signature.of(members.head._1)
        }
        Option.when(candidates.nonEmpty) {
          candidates.filter(_._2.accepts(applied)) match {
            case List((id, signature)) if !unsettled(name) =>
              Resolution(Set(id), signature.evaluates(applied))
            case _ => Resolution(candidates.map(_._1).toSet, evaluated = false)
          }
        }
      }

      def selected(path: List[String], name: String, applied: Applied): Option[Resolution] =
        Option.when(!ownTemplate && measured.path.contains(path) && name == measured.name)(
          Resolution(Set(-1), measured.evaluates(applied))
        )

      def named(name: String): Set[Int] =
        grouped.zipWithIndex.collect { case ((`name`, _), id) => id }.toSet ++
          templates.indices.filter(i => templateName(templates(i)._2) == name).map(typeId) ++
          Option.when(!ownTemplate && name == measured.name)(-1)

      def enclosing(name: String): Option[String] =
        level.chain.tail
          .find(f => f.isTemplate && definesOrTakes(f, name))
          .flatMap(_.path.lastOption)

      def outer(template: String, name: String): Boolean =
        level.chain.tail.find(f => f.isTemplate && f.path.lastOption.contains(template)) match {
          case None        => true
          case Some(frame) => reaches(frame, name)
        }

      def byValue(head: Term, contextual: List[Boolean], typed: Boolean): List[Boolean] =
        calls
          .byValue(head, ReceiverCalls.Call(contextual, typed), level, visible)
          .getOrElse(List.fill(contextual.size)(false))

      // A declaration outside the sources may reach the body: an unsupplied wildcard import,
      // parent or self type around it.
      private lazy val unread = resolver
        .unreadChannel(ResolutionContext(level, resolver.typeParameters(level), Set.empty))
        .nonEmpty

      def converts: Boolean = unread || resolver.insertions.conversion

      def evaluatesQualifier(
          member: Option[String],
          name: String,
          applied: Applied,
          at: Int
      ): Boolean =
        TargetViews.anyMembers(name) || member.exists(calls.declares(_, name, applied, visible)) ||
          !(converts || resolver.insertions.extensions(name) || imported(name, at))

      def deferredOperand(op: String): Boolean =
        unread || resolver.insertions.byName(op) || resolver.insertions.extensions(op)

      // A name an import or export binds outside the sources where the body reads it (an import
      // binds only after itself), or one the lookup cannot follow: it may be an extension.
      private def imported(name: String, at: Int): Boolean =
        resolver.resolveName(
          name,
          ResolutionContext(
            level,
            resolver.typeParameters(level),
            Set.empty,
            at = Some(level.input -> at)
          )
        ) match {
          case _: Found.Failed => true
          case _               => false
        }

      // Whether an enclosing template's member may evaluate the declaration: unknown unless every
      // definition of it there is fixed code whose body names no member, no `this` and not the
      // declaration (a constructor parameter is a value, no code).
      private def reaches(frame: Frame, name: String): Boolean = {
        val definitions = frame.stats.filter(MemberSyntax.names(_).contains(name))
        val fixed = resolver.fixedTemplate(frame)
        if (frame.params.exists(_.name.value == name) && definitions.isEmpty) false
        else
          name.isEmpty || definitions.isEmpty || definitions.exists { d =>
            (!fixed && !Concrete.unoverridable(d)) || !d.is[Defn] || bodyOf(d)._1.collect {
              case Term.Name(n) if n == measured.name || definesOrTakes(frame, n) => ()
              case _: Term.This                                                   => ()
            }.nonEmpty
          }
      }
    }

    private def templateName(tree: Tree): String = MemberSyntax.templateName(tree).getOrElse("")

    private def definesOrTakes(frame: Frame, name: String): Boolean =
      frame.params.exists(_.name.value == name) ||
        frame.stats.exists(MemberSyntax.names(_).contains(name))

  }

  private def selves(level: Frame): Set[String] =
    level.tree
      .flatMap(TypeApplications.template)
      .flatMap(_.body.selfOpt)
      .map(_.name.value)
      .filter(_.nonEmpty)
      .toSet

}

private[cardinality] object ReceiverMembers {

  /** The measured declaration as a receiver sees it: its own member, or a member of a template it
    * encloses, reached through `path` when every template between is an object.
    */
  final case class Measured(tree: Tree, name: String, template: Frame, path: Option[List[String]]) {

    /** Whether a call through the path certainly runs the declaration: the arity selects it among
      * the members its template defines, and the template inherits nothing that could compete.
      */
    def evaluates(applied: Applied): Boolean = {
      val rivals = template.stats.filter(MemberSyntax.names(_).contains(name))
      template.parents.isEmpty &&
      rivals.filter(Signature.of(_).accepts(applied)).forall(_ eq tree) &&
      Signature.of(tree).evaluates(applied)
    }

  }

  /** One slot or none, from identity answers against each candidate: an unknown answer with no
    * known match leaves the identity unresolved.
    */
  def settle(answers: List[Option[Boolean]], name: String): Either[String, Boolean] =
    if (answers.contains(Some(true))) Right(true)
    else if (answers.forall(_.contains(false))) Right(false)
    else Left(s"inherited declaration identity not resolved: $name")

  /** The constructor parameters that are members: `val`/`var` ones, and a case class's first
    * clause.
    */
  def fieldParams(frame: Frame): List[Term.Param] =
    frame.tree match {
      case Some(c: Defn.Class) =>
        val clauses = c.ctor.paramClauses.toList
        val caseFields =
          if (c.mods.exists(_.is[Mod.Case])) clauses.take(1).flatMap(_.values) else Nil
        clauses
          .flatMap(_.values)
          .filter(p => MemberSyntax.keptParameter(p) || caseFields.exists(_ eq p))
      case Some(t: Defn.Trait) =>
        t.ctor.paramClauses.toList.flatMap(_.values).filter(MemberSyntax.keptParameter)
      case _ => Nil
    }

  def fields(frame: Frame): Set[String] = fieldParams(frame).map(_.name.value).toSet

  /** How a member is called: its value clauses (`using` ones are filled implicitly) and the
    * argument counts the first accepts (defaults and repeated parameters widen it).
    */
  final case class Signature(clauses: Int, first: Range) {

    def accepts(applied: Applied): Boolean =
      clauses == 0 || applied.clauses == 0 || first.contains(applied.first)

    /** A value or a parameterless member runs when named; a method once every clause is applied (a
      * bare or partial one is η-expanded).
      */
    def evaluates(applied: Applied): Boolean = clauses == 0 || applied.clauses >= clauses

  }

  object Signature {

    def of(tree: Tree): Signature = {
      val clauses =
        MemberSyntax.groups(tree).flatMap(_.paramClauses).filterNot(MemberSyntax.contextual)
      Signature(
        clauses.size,
        clauses.headOption.fold(0 to 0) { clause =>
          val required = clause.values.count(p => p.default.isEmpty && !MemberSyntax.repeated(p))
          if (clause.values.exists(MemberSyntax.repeated)) required to Int.MaxValue
          else required to clause.values.size
        }
      )
    }

  }

  /** A concrete member of a level: its name and its tree (a definition, or a parent's field). */
  final case class Concrete(level: Frame, tree: Stat | Term.Param, name: String) {

    def parameterless: Boolean = Signature.of(tree).clauses == 0

    def fixed: Boolean = Concrete.unoverridable(tree)

    def field: Boolean = tree.is[Term.Param]

    /** A field no code outside its level reads. */
    def hidden: Boolean = tree match {
      case p: Term.Param => p.mods.exists(_.is[Mod.Private])
      case _             => false
    }

  }

  object Concrete {

    /** No instance replaces a final, private or inline member, a given, a member object or a `var`
      * (Scala 3 makes givens and objects final, and lets nothing override a `var`: E164).
      */
    def unoverridable(member: Tree): Boolean = member match {
      case _: (Defn.GivenAlias | Defn.Given | Defn.Object | Defn.Var) => true
      case p: Term.Param    => p.mods.exists(m => m.is[Mod.VarParam] || fixing(m))
      case d: Stat.WithMods => d.mods.exists(fixing)
      case _                => false
    }

    private def fixing(mod: Mod): Boolean =
      mod.is[Mod.Final] || mod.is[Mod.Inline] || (mod match {
        case p: Mod.Private => p.within.is[Name.Anonymous] || p.within.is[Term.This]
        case _              => false
      })

    def of(level: Frame, stat: Stat): List[Concrete] = stat match {
      case _: (Defn.Def | Defn.GivenAlias | Defn.Given | Defn.Object | Defn.Val | Defn.Var) =>
        MemberSyntax.names(stat).filter(_.nonEmpty).map(Concrete(level, stat, _))
      case _ => Nil
    }

  }

}
