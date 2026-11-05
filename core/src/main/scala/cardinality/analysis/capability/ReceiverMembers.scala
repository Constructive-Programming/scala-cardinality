package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.model.{DeclarationIdentity, MemberSyntax, ResolutionContext}
import cardinality.analysis.resolution.*
import cardinality.types.{Overrides, TypeApplications}

import MethodAnalysis.Frame
import TargetViews.{Applied, Resolution, View, ViewMode}

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
  def implemented(
      declaration: Stat,
      level: Frame,
      target: Option[Own]
  ): Either[String, Boolean] = {
    val names = MemberSyntax.names(declaration)
    val field = MemberSyntax.groups(declaration).isEmpty &&
      levels.exists(l => l.id != level.id && names.exists(fields(l)))
    val candidates =
      concretes.filter(c =>
        c.level.id != level.id && names.contains(c.name) && !target.exists(_.is(c))
      )
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
  def open(measured: Option[Measured], mode: ViewMode): List[Either[Refusal, Concrete]] = {
    val closures = TemplateClosures.of(root, levels)
    val views = measured.fold(Map.empty[Concrete, View])(viewsOf)
    val refused = destructured(closures).map(reason => Left(Refusal(reason)))
    refused ++ concretes.flatMap { c =>
      overridden(c) match {
        case Left(reason) => List(Left(Refusal(reason)))
        case Right(true)  => Nil
        case Right(false) =>
          if (measured.exists(_.is(c))) Nil
          else verdict(c, closures, mode.effective(views.getOrElse(c, View.Free)))
      }
    }
  }

  // An effective member other than the declaration itself: none for a universal member, a derived
  // one or a definite view; a reason for a refusal or a possible view; the member when it is open.
  private def verdict(
      c: Concrete,
      closures: List[TemplateClosures.Closure],
      view: View
  ): Option[Either[Refusal, Concrete]] =
    if (universal(c.tree, c.level)) None
    else
      classification(c, closures) match {
        case Some(refused) => refused.left.toOption.map(reason => Left(Refusal(reason)))
        case None          =>
          view match {
            case View.Definite => None
            case View.Possible => Some(Left(Refusal(s"${TargetViews.Reentry}: ${label(c)}", true)))
            case View.Free     => Some(Right(c))
          }
      }

  // A field of a level no class of the linearization passes arguments to: every value of its type,
  // whichever class finally extends the receiver passes it.
  private def freeState(c: Concrete): Boolean = c.field && !passed(c.level)

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
  private def destructured(closures: List[TemplateClosures.Closure]): List[String] =
    if (resolver.fixedTemplate(root) || closures.contains(TemplateClosures.Closure.Final)) Nil
    else
      levels.flatMap(_.stats).collect {
        case v: Defn.Val if !v.pats.forall(_.is[Pat.Var]) && !Concrete.unoverridable(v) =>
          s"destructured member of an open receiver not read: ${root.path.mkString(".")}"
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
    if (freeState(c)) None
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

  private lazy val fileOverrides = new FileOverrides(root, levels)

  private lazy val calls = new ReceiverCalls(levels, resolver)

  private def label(c: Concrete): String = s"${root.path.mkString(".")}.${c.name}"

  // Views of the measured declaration among every concrete member's default, every nested
  // template's body and what constructing a level runs (derived members carry a view to the open
  // members calling them).
  //
  // Implicit search is not syntax: `summon[T]`, a `using` clause filled implicitly or a conversion
  // may evaluate any given or implicit member of the receiver — the measured declaration itself
  // when it is one, abstract or not. So every body that may search — any but a literal; a stored
  // field reads none — mentions those members, and one of them that may re-enter the declaration
  // makes the body a possible view.
  private def viewsOf(measured: Measured): Map[Concrete, View] = {
    val slots = new Slots(measured)
    val trees = concretes.map(c => c.level -> c.tree) ++ slots.templates ++ slots.constructions
    val nodes = trees.map((level, tree) => (slots.of(level, tree), tree, slots.reads(level, tree)))
    val implicits =
      nodes.collect { case (slot, tree, _) if MemberSyntax.implicitMember(tree) => slot }.toSet ++
        Option.when(MemberSyntax.implicitMember(measured.tree))(slots.target)
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

    // What constructing a level runs besides its fields' initializers (their own slots): each
    // secondary constructor's body, and the statements of its template, which the primary
    // constructor runs — every constructor, a subclass's too, runs it.
    val constructions: List[(Frame, Tree)] = levels.flatMap { level =>
      level.tree.flatMap(TypeApplications.template).toList.map(level -> _) ++
        level.stats.collect { case c: Ctor.Secondary => level -> c }
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

    private def constructionId(index: Int): Int = grouped.size + templates.size + index

    /** The measured declaration's slot: its own on this receiver, a slot of its own otherwise. */
    val target: Int =
      if (ownTemplate) grouped.indexWhere(_._2.exists(_._1 eq measured.tree)) else -1

    def of(level: Frame, tree: Tree): Int =
      (templates.indexWhere(_._2 eq tree), constructions.indexWhere(_._2 eq tree)) match {
        case (-1, -1) => grouped.indexWhere(_._2.exists((t, l) => (t eq tree) && l.id == level.id))
        case (-1, index) => constructionId(index)
        case (index, _)  => typeId(index)
      }

    // Constructing the levels `built` (by id): their constructors and template statements run, and
    // each strict field initializer.
    def constructed(built: Set[String]): Set[Int] = {
      val runs =
        constructions.indices.filter(i => built(constructions(i)._1.id)).map(constructionId)
      val initialized = grouped.zipWithIndex.collect {
        case ((_, members), id) if members.exists((t, l) => built(l.id) && strict(t)) => id
      }
      (runs ++ initialized).toSet
    }

    // A construction this reading cannot follow: every level's constructors and initializers may
    // run, and the class's own code any member, the measured one too.
    def unknown: Set[Int] = constructed(levels.map(_.id).toSet) + target

    // Building a class whose supplied base classes are `bases`: the receiver levels among them run
    // their constructors and initializers, and a level of the class outside the receiver's that
    // runs code of its own may call any member it inherits, the measured one too.
    def building(entry: MethodAnalysis.TypeEntry, bases: Set[String]): Option[Set[Int]] = {
      val own = levels.map(_.id).toSet
      val shared = bases.intersect(own)
      if (shared.isEmpty || bases.subsetOf(own)) Some(constructed(shared))
      else
        TypeIdentities.classFrame(entry).flatMap(below(_, own)).map { runs =>
          constructed(shared) ++ Option.when(runs)(target)
        }
    }

    // Whether a level of a class outside the receiver's runs code when the class is built.
    private def below(frame: Frame, own: Set[String]): Option[Boolean] =
      InheritedScopes
        .ancestors(frame, resolver)
        .map(ancestors => (frame :: ancestors).filterNot(l => own(l.id)).exists(runsCode))

    // Building a level runs its template's statements and strict initializers, its arguments to
    // its parents and possibly a secondary constructor.
    private def runsCode(level: Frame): Boolean =
      level.tree.flatMap(TypeApplications.template).exists { template =>
        template.inits.exists(_.argClauses.exists(_.values.nonEmpty)) ||
        template.body.stats.exists(stat => stat.is[Term] || stat.is[Ctor.Secondary] || strict(stat))
      }

    private def strict(member: Tree): Boolean = member match {
      case v: Defn.Val => !v.mods.exists(_.is[Mod.Lazy])
      case _: Defn.Var => true
      case _           => false
    }

    // A body that may search implicits may also select an enclosing template's given or implicit
    // member, which reaches the declaration as `Outer.this.g` would.
    def reads(level: Frame, tree: Tree): TargetViews.Reads = {
      val self = TargetViews.Self(selves(level), level.path.lastOption.toSet)
      val names = scope(level)
      val read = TargetViews.reads(bodyOf(tree), self, names)
      // A secondary constructor first calls another constructor of its class.
      val chained =
        if (tree.is[Ctor.Secondary]) constructed(linearization.bases(level.id)) else Set.empty
      read.copy(
        possible = read.possible ++ chained,
        outer = read.outer || (read.searches && outerImplicits(level, names))
      )
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
        TargetViews.Body(
          d.body,
          MemberSyntax.parameterNames(d.paramClauseGroups),
          d.decltpe,
          TargetViews.returnsParameter(d)
        )
      case g: Defn.GivenAlias =>
        TargetViews.Body(g.body, MemberSyntax.parameterNames(g.paramClauseGroups), Some(g.decltpe))
      case v: Defn.Val       => TargetViews.Body(v.rhs, Set.empty, v.decltpe)
      case v: Defn.Var       => TargetViews.Body(v.body, Set.empty, v.decltpe)
      case c: Ctor.Secondary =>
        TargetViews.Body(c.body, c.paramClauses.flatMap(_.values).map(_.name.value).toSet, None)
      case _: Term.Param => TargetViews.Body(Lit.Unit(), Set.empty, None)
      case other         => TargetViews.Body(other, Set.empty, None)
    }

    private def scope(level: Frame): TargetViews.Scope = new LevelScope(level)

    // What the names a body written at `level` spells mean on this receiver.
    final private class LevelScope(level: Frame) extends TargetViews.Scope {
      private val visible = linearization.bases(level.id)

      // The member slots of `name` a body written at `level` may select: only the levels its
      // scope sees (`visible`, the level and the levels it extends).
      private def visibleSlots(name: String): List[(Int, List[(Tree, Frame)])] =
        grouped.zipWithIndex.collect {
          case ((`name`, members), id) if members.exists((_, l) => visible(l.id)) => id -> members
        }

      def own(name: String, applied: Applied): Option[Resolution] = {
        val candidates =
          visibleSlots(name).map((id, members) => id -> Signature.of(members.head._1))
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

      // The slots a body written at `level` may denote: the members and templates its own scope
      // sees (`visible`, the level and the levels it extends), not the receiver's own additions -
      // a parent's body resolves its names where it is written, not on the subclass that
      // inherits it (register C6; `Construction` reads each level in its own scope too).
      def named(name: String): Set[Int] =
        visibleSlots(name).map(_._1).toSet ++
          templates.indices
            .filter(i => templateName(templates(i)._2) == name && visible(templates(i)._1.id))
            .map(typeId) ++
          Option.when(!ownTemplate && name == measured.name)(-1)

      // A local definition is the measured one only by identity: a name a body binds again is
      // another definition.
      def local(definition: Tree, applied: Applied): Option[Resolution] =
        Option.when(!ownTemplate && DeclarationIdentity.same(definition, measured.tree))(
          Resolution(Set(target), Signature.of(definition).evaluates(applied))
        )

      // A secondary constructor's `this(…)` builds its own class; any other construction what
      // building the class it names runs (`building`). A class outside the sources extends none of
      // the levels; one this reading cannot follow — its name, a parent of it, a block import that
      // may rebind it — may be any. A creator application builds a class only where its name is a
      // supplied class's.
      def constructed(constructs: TargetViews.Constructs, at: Int): Set[Int] =
        constructs match {
          case TargetViews.Constructs.Unfollowed                          => unknown
          case TargetViews.Constructs.Named(tpe, name, imported, creator) =>
            val context = contextAt(level, at)
            val head = TypeApplications.headAndArguments(tpe)._1
            resolver.declaration(head, InheritedScopes.typeName(tpe), context) match {
              case Found.Entry(entry) if creator && !entry.tree.is[Defn.Class] => Set.empty
              case _ if imported                                               => unknown
              case Found.Entry(entry)                                          =>
                TypeIdentities
                  .lineage(tpe, context, resolver)
                  .filter(_.complete)
                  .flatMap(lineage => building(entry, lineage.bases))
                  .getOrElse(unknown)
              case _ if creator => Set.empty
              case Found.Absent =>
                if (resolver.builtin(name, context).isRight) Set.empty else unknown
              case _ => unknown
            }
        }

      def enclosing(name: String): Option[String] =
        level.chain.tail
          .find(f => f.isTemplate && definesOrTakes(f, name))
          .flatMap(_.path.lastOption)

      def outer(template: String, name: String): Boolean =
        level.chain.tail
          .find(f => f.isTemplate && f.path.lastOption.contains(template))
          .forall(reaches(_, name))

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
          !(converts || resolver.insertions.extensions(name))

      def deferredOperand(op: String): Boolean =
        unread || resolver.insertions.byName(op) || resolver.insertions.extensions(op)

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
            }.nonEmpty
          }
      }

    }

    private def templateName(tree: Tree): String = MemberSyntax.templateName(tree).getOrElse("")

  }

  // The context a name written at `offset` in `level` is read in.
  private def contextAt(level: Frame, offset: Int): ResolutionContext =
    ResolutionContext(
      level,
      resolver.typeParameters(level),
      Set.empty,
      at = Some(level.input -> offset)
    )

  private def definesOrTakes(frame: Frame, name: String): Boolean =
    frame.params.exists(_.name.value == name) ||
      frame.stats.exists(MemberSyntax.names(_).contains(name))

  private def selves(level: Frame): Set[String] =
    level.tree
      .flatMap(TypeApplications.template)
      .flatMap(_.body.selfOpt)
      .map(_.name.value)
      .filter(_.nonEmpty)
      .toSet

}

private[cardinality] object ReceiverMembers {

  /** Why a member or free state leaves the environment unread; `view` when it is a possible view of
    * the measured declaration (capabilities C6), which `ViewSandwich` may read between bounds.
    */
  final case class Refusal(reason: String, view: Boolean = false)

  /** The measured definition among a level's members: its tree and its name, since a definition of
    * several names (`val a, b = …`) is one tree whose other names are members beside it.
    */
  final case class Own(tree: Tree, name: String) {

    def is(member: Concrete): Boolean = (member.tree eq tree) && member.name == name

  }

  /** The measured declaration as a receiver sees it: its own member, or a member of a template it
    * encloses, reached through `path` when every template between is an object.
    */
  final case class Measured(tree: Tree, name: String, template: Frame, path: Option[List[String]]) {

    def is(member: Concrete): Boolean = Own(tree, name).is(member)

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

  /** The constructor parameters that are members (`MemberSyntax.fieldParams`). */
  def fieldParams(frame: Frame): List[Term.Param] =
    frame.tree.toList.flatMap(MemberSyntax.fieldParams)

  /** A class's or trait's constructor parameter clauses; none for any other template. */
  def constructorClauses(frame: Frame): List[List[Term.Param]] =
    frame.tree.toList.flatMap {
      case c: Defn.Class => c.ctor.paramClauses.toList.map(_.values)
      case t: Defn.Trait => t.ctor.paramClauses.toList.map(_.values)
      case _             => Nil
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

  }

  object Concrete {

    /** No instance replaces a final, private or inline member, a given, a member object or a `var`
      * (`Overrides.overridable`).
      */
    def unoverridable(member: Tree): Boolean = !Overrides.overridable(member)

    def of(level: Frame, stat: Stat): List[Concrete] = stat match {
      case _: (Defn.Def | Defn.GivenAlias | Defn.Given | Defn.Object | Defn.Val | Defn.Var) =>
        MemberSyntax.names(stat).map(Concrete(level, stat, _))
      case _ => Nil
    }

  }

}
