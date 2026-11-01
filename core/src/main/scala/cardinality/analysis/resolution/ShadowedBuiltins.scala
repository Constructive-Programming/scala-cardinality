package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{BuiltinTypes, Resolver, SourceIndex}
import cardinality.types.TypeSubstitution

/** A builtin name (`Int`, `Boolean`, `Option`) a reference may bind through a channel simple-name
  * lookup does not walk.
  *
  * The builtins come from the root import `scala.*`, the weakest binding Scala has, so any
  * declaration a reference reaches by its simple name shadows them (or is an ambiguity Scala
  * rejects). Lookup reads lexical declarations, package members and import clauses. It does not
  * read what an enclosing template inherits from its parents or its self type, an enum's cases
  * inside its body, what a wildcard-imported object inherits or exports, a package object's
  * inherited members, or export clauses. With `trait Base { type Int = Boolean }`, `Int` is
  * `Boolean` in `trait Sub { self: Base => … }`, after `object O extends Base; import O.*`, in the
  * package of `package object p extends Base` and through `export O.*`: Scala 3.9.0 rejects `val w:
  * Int = 3` in each.
  *
  * This reading walks those channels from every scope around the reference and refuses the builtin
  * when one reaches a supplied declaration of the name or an export that binds it. A step it cannot
  * follow is refused too when it may lead into the supplied sources: a parent, self type or alias
  * whose name resolves to nothing it reads, or a wildcard prefix headed by a supplied term. The
  * walk runs for a name some supplied type, enum case or export spells; resolving the reference
  * through the member it reaches is the environment lane's.
  *
  * A channel may also reach declarations the sources do not supply — a parent or self type outside
  * them, a wildcard namespace no supplied declaration provides — which may bind any name at all.
  * The top names are refused there whether or not the sources spell them (with `object Defs extends
  * ext.X`, `Any` after `import Defs.*` is `Boolean` when `X` declares `type Any = Boolean`), and so
  * is the one-value shortcut on a declared result. Every other builtin name waits for the stdlib
  * and foreign-names cluster, which knows the namespaces whose members it can list.
  */
final private[cardinality] class ShadowedBuiltins(
    resolver: Resolver,
    types: List[TypeEntry],
    sources: List[Source],
    wildcards: (Frame, Option[(String, Int)]) => List[List[String]]
) {
  import ShadowedBuiltins.*

  private val cases: Set[(String, String)] = types.flatMap { entry =>
    (entry.tree, entry.self) match {
      case (d: Defn.Enum, Some(frame)) => caseNames(d).map(frame.id -> _)
      case _                           => Nil
    }
  }.toSet

  private val declarations = types.map(entry => entry.owner.id -> entry.name).toSet ++ cases

  private val typeNames = types.map(_.name).toSet

  private val spelled = typeNames ++ cases.map(_._2) ++ sources.flatMap(exportedNames)

  private val terms = sources.flatMap(declaredTerms).toSet

  private val enums = cases.map(_._1)

  private lazy val modules = resolver.modules.groupBy(_.path)

  private lazy val classes =
    types.filter(_.self.nonEmpty).groupBy(entry => entry.owner.path :+ entry.name)

  private val channels =
    scala.collection.mutable.Map.empty[(Frame, Option[(String, Int)]), Option[Stop]]

  /** The builtin a simple name lookup did not find denotes, unless a channel lookup does not walk
    * may bind it.
    */
  def resolve(name: String, args: List[Shape], context: ResolutionContext): Either[String, Shape] =
    BuiltinTypes.resolve(name, args).flatMap { builtin =>
      val walked = Option.when(spelled(name))(new Walk(Some(name)).from(context)).flatten
      val unread = TopTypes.named(name).flatMap(_ => channel(context))
      walked.orElse(unread).map(refusal(s"builtin name $name", _)).toLeft(builtin)
    }

  /** Whether a supplied declaration, enum case or export selector spells this simple name: one no
    * channel can reach is not a declaration of the sources.
    */
  def mayBeSupplied(name: String): Boolean = spelled(name)

  /** A channel around the reference through which some declaration the sources do not supply, or
    * one this reading cannot follow, may bind a name: what is not read, and through what.
    */
  def unread(context: ResolutionContext): Option[String] =
    channel(context).map(refusal("builtin names", _))

  // The same for every name no supplied declaration spells, so it is read once per reference — and
  // per frame, not per frame id: a constructor's frame shares its class's id but not its members
  // (Scala 3.9.0 reads `class C(a: Any) extends X` with `scala.Any` beside `X`'s `type Any`).
  private def channel(context: ResolutionContext): Option[Stop] =
    channels.getOrElseUpdate(context.frame -> context.at, new Walk(None).from(context))

  // One name's search — or, with no name, the search for a step that leaves the sources: each frame
  // whose members it may reach, once, with the step from the reference that first reached it.
  final private class Walk(name: Option[String]) {

    def from(context: ResolutionContext): Option[Stop] =
      search(context.frame.chain.flatMap(entrances(_, context.at)), Set.empty, None)

    // A member found wins over a step that could not be followed; that one refuses only when the
    // walk finds nothing else. A step outside the sources binds no name the sources spell, so only
    // the nameless search stops at it.
    @tailrec private def search(
        pending: List[Step],
        seen: Set[String],
        doubt: Option[Stop]
    ): Option[Stop] =
      pending match {
        case Nil                         => doubt
        case (found: Step.Found) :: _    => Some(found)
        case (step: Step.Doubt) :: rest  => search(rest, seen, doubt.orElse(Some(step)))
        case (step: Step.Unread) :: rest =>
          search(rest, seen, doubt.orElse(Option.when(name.isEmpty)(step)))
        case Step.Reach(frame, via) :: rest =>
          if (seen(frame.id)) search(rest, seen, doubt)
          else
            declared(frame) match {
              case Some(member) => Some(Step.Found(s"${qualified(frame)}.$member", via))
              case None         => search(rest ++ members(frame, Some(via)), seen + frame.id, doubt)
            }
      }

    private def declared(frame: Frame): Option[String] =
      name.filter(n => declarations(frame.id -> n))

    private def isSearched(n: String): Boolean = name.contains(n)

    // What one scope around the reference opens: an enum's cases and an enclosing template's
    // members beyond its own declarations, a package's objects and exports, and wildcard imports.
    private def entrances(scope: Frame, at: Option[(String, Int)]): List[Step] = {
      val own =
        if (!template(scope)) Nil
        else
          name.filter(n => cases(scope.id -> n)) match {
            case Some(n) => List(Step.Found(s"${qualified(scope)}.$n", s"case of ${label(scope)}"))
            case None    => members(scope, None)
          }
      val packaged =
        if (!resolver.isPackage(scope)) Nil
        else if (unsupplied(scope))
          List(Step.Unread(qualified(scope), s"package clause ${qualified(scope)}"))
        else resolver.peers(scope.path).flatMap(members(_, None))
      val imported = wildcards(scope, at).flatMap { prefix =>
        targets(prefix, scope, s"wildcard import of ${prefix.mkString(".")}")
      }
      own ++ packaged ++ imported ++ unbound(scope)
    }

    // A package clause opens every member of its package, and one no supplied unit declares a member
    // of — the outer clause of `package a; package b`, with `a` written elsewhere — opens
    // declarations the sources do not hold (Scala 3.9.0 reads `Any` there as `a`'s `type Any`).
    private def unsupplied(scope: Frame): Boolean =
      scope.path.nonEmpty && !resolver.peers(scope.path).exists(declaresMembers)

    // A wildcard clause whose prefix lookup does not bind (`import Outer.this.o.*`) opens a
    // supplied value's members; a root-anchored one, a package's.
    private def unbound(scope: Frame): List[Step] =
      scope.stats
        .collect { case clause: Import => clause.importers }
        .flatten
        .filter(importer =>
          SourceIndex.importPath(importer.ref).isEmpty &&
            importer.importees.exists(_.is[Importee.Wildcard])
        )
        .flatMap { importer =>
          val ref = spell(importer.ref)
          val via = s"wildcard import of $ref"
          // A root-anchored prefix is a package path read from the root alone.
          if (ref.startsWith("_root_."))
            targets(ref.stripPrefix("_root_.").split('.').toList, scope.chain.last, via)
          else List(Step.Doubt(ref, via))
        }

    // The frames one frame's members come from besides its own declarations: its parents, its self
    // type (read only from inside its body: the constructor's own frame holds no statements), and
    // what its export clauses forward. With no step yet, the frame's own channel names the step.
    private def members(frame: Frame, via: Option[String]): List[Step] = {
      def step(channel: String) = via.getOrElse(s"$channel of ${label(frame)}")
      val parents = parentClauses(frame).flatMap(init =>
        named(init.tpe, frame.parent.getOrElse(frame), step("parents"), 0)
      )
      val selves =
        if (frame.stats.isEmpty) Nil
        else selfTypes(frame).flatMap(named(_, frame, step("self type"), 0))
      parents ++ selves ++ exports(frame, step("export"))
    }

    private def exports(frame: Frame, via: String): List[Step] =
      frame.stats
        .collect { case clause: Export => clause.importers }
        .flatten
        .flatMap { importer =>
          val prefix = SourceIndex.importPath(importer.ref)
          importer.importees.flatMap {
            case _: Importee.Wildcard =>
              prefix
                .fold(List(Step.Doubt(s"export ${spell(importer)}", via)))(targets(_, frame, via))
            case selector if bound(selector).exists(isSearched) =>
              List(Step.Found(s"export ${spell(importer)}", via))
            case _ => Nil
          }
        }

    // The frames a dotted prefix may denote, anchored at every package around `from` (a `_root_`
    // prefix at the root alone): objects, packages, and the companions of enums (their cases) and
    // classes (nothing this reads).
    private def targets(prefix: List[String], from: Frame, via: String): List[Step] = {
      val (relative, paths) = prefix match {
        case SourceIndex.Root :: path => (path, List(path))
        case _ => (prefix, from.chain.flatMap(_.path.inits).distinct.map(_ ++ prefix))
      }
      val frames = paths.flatMap(path => modules.getOrElse(path, Nil) ++ resolver.peers(path))
      val companions = paths.flatMap(classes.getOrElse(_, Nil)).flatMap(_.self)
      if (frames.nonEmpty || companions.nonEmpty)
        (frames ++ companions.filter(frame => enums(frame.id))).map(Step.Reach(_, via))
      else if (relative.headOption.exists(terms)) List(Step.Doubt(relative.mkString("."), via))
      else List(Step.Unread(prefix.mkString("."), via))
    }

    // A parent, self type or alias body naming a supplied template reaches it; one that resolves to
    // an alias is followed; a name lookup refuses, or one a supplied type spells but lookup does not
    // find, may be that type.
    private def named(tpe: Type, from: Frame, via: String, depth: Int): List[Step] =
      parentName(tpe) match {
        case None                       => List(Step.Doubt(spell(tpe), via))
        case Some(_) if depth > MaxHops => List(Step.Doubt(spell(tpe), via))
        case Some(parent)               =>
          val at = Option.when(tpe.pos.start >= 0)(from.input -> tpe.pos.start)
          resolver.resolveName(
            parent,
            ResolutionContext(from, Map.empty, Set.empty, at = at)
          ) match {
            case Found.Entry(entry) =>
              (entry.self, entry.tree) match {
                case (Some(frame), _)         => List(Step.Reach(frame, via))
                case (None, alias: Defn.Type) => named(alias.body, entry.owner, via, depth + 1)
                case _                        => List(Step.Doubt(parent, via))
              }
            case Found.Blocked(_) | Found.Foreign(_)               => List(Step.Doubt(parent, via))
            case Found.Absent if typeNames(parent.split('.').last) => List(Step.Doubt(parent, via))
            // A name found nowhere is outside the sources, unless it is a root or marker type.
            case Found.Absent if InheritedTypeMembers.markerName(parent) => Nil
            case Found.Absent => List(Step.Unread(parent, via))
          }
      }

  }

}

private[cardinality] object ShadowedBuiltins {

  // Alias hops a parent name is followed through before the walk gives up on it.
  private val MaxHops = 16

  private enum Step {
    case Reach(frame: Frame, via: String)
    case Found(declaration: String, via: String)
    // A step this reading cannot follow, which may lead into the supplied sources.
    case Doubt(unread: String, via: String)
    // A step that leads outside the supplied sources.
    case Unread(unread: String, via: String)
  }

  // Where a search stops: a declaration of the name, or a step it cannot see past.
  private type Stop = Step.Found | Step.Doubt | Step.Unread

  private def refusal(subject: String, stop: Stop): String = stop match {
    case Step.Found(declaration, via) => s"$subject may be shadowed by $declaration ($via)"
    case Step.Doubt(what, via)        =>
      s"$subject may be shadowed through $what, which is not resolved ($via)"
    case Step.Unread(what, via) =>
      s"$subject may be shadowed through $what, which is not supplied ($via)"
  }

  /** Whether a resolution's reason is this refusal: a builtin reading that may stand for a
    * different declaration, so no other reading of the same name may be trusted in its place.
    */
  def shadows(reason: String): Boolean =
    reason.startsWith("builtin name ") && reason.contains(" may be shadowed ")

  // A package object, or a clause with a statement other than a nested clause or an import.
  private def declaresMembers(frame: Frame): Boolean =
    frame.tree.exists(_.is[Pkg.Object]) || frame.stats.exists(stat =>
      !stat.is[Pkg] && !stat.is[Import]
    )

  private def template(frame: Frame): Boolean = frame.tree.exists {
    case _: Defn.Class | _: Defn.Trait | _: Defn.Object | _: Defn.Enum | _: Defn.Given |
        _: Pkg.Object =>
      true
    case _ => false
  }

  private def templateOf(frame: Frame): Option[Template] = frame.tree.collect {
    case d: Defn.Class  => d.templ
    case d: Defn.Trait  => d.templ
    case d: Defn.Object => d.templ
    case d: Defn.Enum   => d.templ
    case d: Defn.Given  => d.templ
    case d: Pkg.Object  => d.templ
  }

  // The constructor's own frame drops its parents: its parameters are read outside the template's
  // members (Scala 3.9.0 types `class Sub(x: Int) extends Base` with `scala.Int`).
  private def parentClauses(frame: Frame): List[Init] = frame.tree match {
    case Some(d: Pkg.Object) => d.templ.inits
    case _                   => frame.parents
  }

  private def selfTypes(frame: Frame): List[Type] =
    templateOf(frame).flatMap(_.body.selfOpt).flatMap(_.decltpe).toList.flatMap(components)

  private def components(tpe: Type): List[Type] = tpe match {
    case TypeSubstitution.Meet(left, right) => components(left) ++ components(right)
    case other                              => List(other)
  }

  private def parentName(tpe: Type): Option[String] = tpe match {
    case Type.Name(value)      => Some(value)
    case selected: Type.Select => Some(selected.syntax)
    case applied: Type.Apply   => parentName(applied.tpe)
    case _                     => None
  }

  private def caseNames(d: Defn.Enum): List[String] = d.templ.body.stats.flatMap {
    case c: Defn.EnumCase         => List(c.name.value)
    case r: Defn.RepeatedEnumCase => r.cases.map(_.value)
    case _                        => Nil
  }

  private def qualified(frame: Frame): String = frame.path.mkString(".")

  // A tree as one line of a reason.
  private def spell(tree: Tree): String = tree.syntax.replaceAll("\\s+", " ")

  private def label(frame: Frame): String = frame.tree match {
    case Some(_: Pkg.Object)        => s"package object ${qualified(frame)}"
    case None if frame.path.isEmpty => "the empty package"
    case None                       => s"package ${qualified(frame)}"
    case _                          => qualified(frame)
  }

  // The name an explicit selector binds.
  private def bound(selector: Importee): Option[String] = selector match {
    case Importee.Name(name)      => Some(name.value)
    case Importee.Rename(_, name) => Some(name.value)
    case _                        => None
  }

  // The names export selectors bind: what a forwarder may put behind a builtin's spelling.
  private def exportedNames(source: Source): List[String] =
    source
      .collect { case clause: Export => clause.importers.flatMap(_.importees) }
      .flatten
      .flatMap(bound)

  // The term names the sources declare (pattern variables of any kind included): a wildcard prefix
  // headed by one of them may open supplied members; any other head names a foreign namespace.
  private def declaredTerms(source: Source): List[String] = source.collect {
    case d: Defn.Object => d.name.value
    case d: Pkg.Object  => d.name.value
    case d: Defn.Def    => d.name.value
    case d: Decl.Def    => d.name.value
    case d: Defn.Given  => d.name.value
    case p: Term.Param  => p.name.value
    case Pat.Var(name)  => name.value
  }

}
