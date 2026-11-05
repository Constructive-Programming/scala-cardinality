package cardinality.analysis.resolution

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.StdlibModel.Spelling
import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.analysis.model.{ResolutionContext, ScopeTerms}
import cardinality.analysis.{BuiltinTypes, ImportBinding, Resolver, SourceIndex, StdlibModel}
import cardinality.types.{
  ListedNamespaces,
  Spelling as TreeSpelling,
  TypeApplications,
  TypeSubstitution
}

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
  * them, a wildcard namespace no supplied declaration provides or that is open to dependencies (a
  * package the sources spell only as a prefix), a wildcard whose prefix's first name such a
  * wildcard may bind (`doubted`: `compose.*` after `import zzz.*` may be `zzz.compose.*`) — which
  * may bind any name at all. Every builtin and stdlib-model name is refused there whether or not
  * the sources spell it — except through a `scala.Dynamic` parent (the structural-member marker: it
  * declares no member a count reads, and the structural calls its subclasses enable are outside the
  * model, which the run logs a notice for) — the sources spell it (with
  * `object Defs extends ext.X`, `Any` after `import Defs.*` is `Boolean` when `X` declares
  * `type Any = Boolean`; `import lib.*` may bring a `lib.Option`), and so is the one-value shortcut
  * on a declared result — except through the namespaces `ListedNamespaces` lists, which bind only
  * the members they declare: the stdlib's (`quotes.reflect`'s `Singleton`) and the dependencies'
  * (cats-core 2.13.0's syntax and instances objects, kyo 1.0.0-RC7's package), so the root
  * spellings survive their wildcards.
  */
final private[cardinality] class ShadowedBuiltins(
    resolver: Resolver,
    types: List[TypeEntry],
    sources: List[Source],
    clauses: (Frame, Option[(String, Int)]) => List[ImportBinding],
    doubted: (List[String], Frame, Option[(String, Int)]) => Option[String]
) {
  import ShadowedBuiltins.*

  private val cases: Set[(String, String)] = types.flatMap { entry =>
    (entry.tree, entry.self) match {
      case (d: Defn.Enum, Some(frame)) => ScopeTerms.enumCases(d).map(frame.id -> _)
      case _                           => Nil
    }
  }.toSet

  private val declarations = types.map(entry => entry.owner.id -> entry.name).toSet ++ cases

  private val typeNames = types.map(_.name).toSet

  // The type names a bare simple name could denote: the types a package owns. A type nested in an
  // object or class (`kyo.Tag.Dynamic`) is reachable only through that container's name.
  private val packageTypeNames =
    types.filter(entry => resolver.isPackage(entry.owner)).map(_.name).toSet

  private def spellable(name: String): Boolean =
    if (name.contains('.')) typeNames(name.split('.').last) else packageTypeNames(name)

  private val spelled = typeNames ++ cases.map(_._2) ++ sources.flatMap(exportedNames)

  private val terms = sources.flatMap(declaredTerms).toSet

  private val enums = cases.map(_._1)

  private lazy val modules = resolver.modules.groupBy(_.path)

  private lazy val classes =
    types.filter(_.self.nonEmpty).groupBy(entry => entry.owner.path :+ entry.name)

  private val channels =
    scala.collection.mutable.Map.empty[(Frame, Option[(String, Int)], Option[String]), Option[Stop]]

  // The searches under way, and how many met themselves again: a parent whose standing is asked
  // while its own scope's channel is being searched (`import O.*` beside `trait T extends
  // Serializable`, `object O extends T`) is doubted, and no search that met one is remembered.
  private val searching = scala.collection.mutable.Set.empty[Search]
  private var cycles = 0

  /** The builtin or stdlib-model entry a name lookup did not find denotes, unless a channel lookup
    * does not walk may bind it; for a name neither holds, the declaration a stdlib namespace opened
    * by a wildcard import declares, or why the information is absent.
    *
    * A root import is the weakest binding Scala has, so any channel to declarations the sources do
    * not supply may rebind a builtin spelling — a wildcard import of an unsupplied namespace
    * included, unless the model lists that namespace and it spells no builtin. A `_root_`-anchored
    * path is read from the root alone, so it names a builtin only through `scala` or `java`. A
    * dotted path relative to a root import (`Predef.String`, `util.Either`) is no builtin either: a
    * top-level package of its first name outside the sources outranks the root import (Scala 3.9.0
    * fixtures).
    */
  def resolve(
      written: String,
      args: List[Shape],
      context: ResolutionContext
  ): Either[String, Shape] =
    reading(BuiltinTypes.known(_, args))(written, context)

  /** What a named import of a path outside the sources denotes: the builtin or stdlib-model entry
    * at that path, read as a root-anchored spelling of it would be — unless a channel may bind the
    * path's head `scala` or `java` to another declaration — or the absence of its declaration.
    */
  def imported(
      path: String,
      binding: String,
      args: List[Shape],
      context: ResolutionContext
  ): Either[String, Shape] =
    importedReading(BuiltinTypes.known(_, args))(path, binding, context)

  /** The declaration `resolve` would read a spelling as, by its canonical key — what a nominal
    * identity names — under the same refusals.
    */
  def denoted(written: String, context: ResolutionContext): Either[String, String] =
    reading(keyed)(written, context)

  /** The declaration `imported` would read a named import's path as, by its canonical key. */
  def importedKey(
      path: String,
      binding: String,
      context: ResolutionContext
  ): Either[String, String] =
    importedReading(keyed)(path, binding, context)

  // How a root spelling the model holds is read; None for one it does not hold.
  private type Known[A] = String => Option[Either[String, A]]

  // A builtin or model entry is its own canonical key, whatever the model makes of its values: an
  // identity, not a reading.
  private val keyed: Known[String] = BuiltinTypes.canonical(_).map(Right(_))

  // One reading of a spelling no source declaration answers: `known` reads a root spelling it
  // holds, a listed namespace's wildcard reads a simple name as a named import of its member would.
  private def reading[A](known: Known[A])(
      written: String,
      context: ResolutionContext
  ): Either[String, A] =
    Spelling.of(written) match {
      case Spelling.TopLevel(_) =>
        Left(absent(written, context))
      // No top-level package outside the sources is named like a root import's member (the
      // never-shadowed assumption); one the sources declare, or a source term, is read as written.
      case Spelling.Relative(name, head)
          if !terms(head) && !resolver.namespace(List(head)) && known(s"scala.$name").nonEmpty =>
        standingReading(name, anchored = false, known(s"scala.$name").get, context)
      case Spelling.Relative(name, _)      => Left(absent(name, context))
      case Spelling.Rooted(name, anchored) =>
        known(name) match {
          case Some(reading) => standingReading(name, anchored, reading, context)
          case None          =>
            opened(name, context)
              .map(importedReading(known)(_, Wildcard, context))
              .getOrElse(Left(absent(name, context)))
        }
    }

  private def importedReading[A](known: Known[A])(
      path: String,
      binding: String,
      context: ResolutionContext
  ): Either[String, A] =
    (Spelling.imported(path) match {
      case Spelling.Rooted(name, _) =>
        known(name).map(standingReading(name, anchored = false, _, context))
      case _ => None
    }).getOrElse(Left(Found.Foreign(path, binding).reason))

  /** The channel through which a declaration other than the root imports' may bind a name the
    * stdlib model reads (a parent clause's `Serializable`, `java.io.Serializable`), if one is open
    * where the name is read. Only a spelling through the root imports names such a declaration.
    */
  def rootShadow(name: String, context: ResolutionContext): Option[String] =
    Spelling.of(name) match {
      case Spelling.Rooted(path, anchored) => declared(path, anchored, context).map(route)
      case _                               => None
    }

  private def standingReading[A](
      name: String,
      anchored: Boolean,
      reading: Either[String, A],
      context: ResolutionContext
  ): Either[String, A] =
    declared(name, anchored, context).fold(reading)(stop =>
      Left(refusal(s"builtin name $name", stop))
    )

  // What rebinds a root spelling under the never-shadowed assumption: a declaration a channel
  // reaches (or a package `scala` / `java` of the sources), or a channel that leaves the sources
  // into the standard library or the JDK, whose classes named like builtins the model does not
  // list. A step this reading cannot follow, or one that leaves the sources anywhere else — an
  // unsupplied wildcard, parent or self type — binds no builtin name.
  private def declared(name: String, anchored: Boolean, context: ResolutionContext): Option[Stop] =
    Option
      .unless(anchored)(
        (walked(name, context) ++ channel(context, Some(name.takeWhile(_ != '.'))) ++
          subpackage(name, context)).find {
          case _: Step.Found                 => true
          case _: Step.Doubt                 => false
          case Step.Unread(what, _, doubted) =>
            !doubted && ListedNamespaces.platform(what.split('.').toList)
        }
      )
      .flatten

  // Why a root spelling may stand for another declaration where it is read: a supplied declaration
  // of the name lookup does not reach, a channel to declarations the sources do not supply, or a
  // package `scala` / `java` of the sources inside an enclosing package. Model reading rests on the
  // complement: no package outside the sources is named `scala` or `java` inside a source package.
  private def standing(name: String, anchored: Boolean, context: ResolutionContext): Option[Stop] =
    Option
      .unless(anchored)(
        walked(name, context)
          .orElse(channel(context, Some(name.takeWhile(_ != '.'))))
          .orElse(subpackage(name, context))
      )
      .flatten

  private def walked(name: String, context: ResolutionContext): Option[Stop] =
    Option.when(spelled(name))(guarded(Search(Some(name), context.frame, context.at))).flatten

  private def subpackage(name: String, context: ResolutionContext): Option[Stop] = {
    val head = name.takeWhile(_ != '.')
    Option
      .when(name.contains('.') && StdlibModel.roots(head))(
        context.frame.chain
          .filter(scope => resolver.isPackage(scope) && scope.path.nonEmpty)
          .map(_.path :+ head)
          .find(path => resolver.peers(path).nonEmpty)
      )
      .flatten
      .map(path => Step.Found(s"package ${path.mkString(".")}", "a package of the sources"))
  }

  // The declaration a wildcard import of a listed stdlib namespace binds a simple name to around
  // the reference, nearest scope first.
  private def opened(name: String, context: ResolutionContext): Option[String] =
    context.frame.chain.iterator.flatMap(listedMember(name, _, context.at)).nextOption()

  // The declaration a wildcard import of a stdlib namespace the model lists binds a simple name to
  // in one scope, before a position: the namespace's member, when the model lists it there.
  private def listedMember(name: String, scope: Frame, at: Option[(String, Int)]): Option[String] =
    Option
      .unless(name.contains('.'))(
        listedIn(scope, at).iterator
          .flatMap(_.typeBinding(name, root = true))
          .collectFirst { case ListedNamespaces.Binding.Member(fqn) => fqn }
      )
      .flatten

  // The stdlib namespaces wildcard imports open around the reference, nearest first.
  private def namespaces(context: ResolutionContext): List[ListedNamespaces.Namespace] =
    context.frame.chain.flatMap(listedIn(_, context.at))

  // The stdlib namespaces one scope's wildcard imports open before `at`.
  private def listedIn(scope: Frame, at: Option[(String, Int)]): List[ListedNamespaces.Namespace] =
    clauses(scope, at).flatMap(clause =>
      clause.wildcards.flatMap(listed(_, scope, ShadowedBuiltins.at(scope, clause)))
    )

  /** The listed stdlib namespace a wildcard prefix written in a scope opens before a position, when
    * its first name denotes the library's.
    */
  def listed(
      prefix: List[String],
      scope: Frame,
      at: Option[(String, Int)]
  ): Option[ListedNamespaces.Namespace] =
    ListedNamespaces.of(prefix).filter(library(_, prefix, scope, at))

  // Whether a prefix spelling a listed namespace is the library's: anchored at the root, or headed
  // by the root package it names, which no named import or source term rebinds — or, for a
  // namespace reached through a term (`quotes.reflect`), by that term: imported by name from its
  // own path, or opened by a wildcard of the listed namespace declaring it, before the clause.
  private def library(
      namespace: ListedNamespaces.Namespace,
      prefix: List[String],
      scope: Frame,
      at: Option[(String, Int)]
  ): Boolean =
    (namespace.anchor, prefix) match {
      case (None, _) => rootHead(prefix, scope, at)
      case (Some(anchor), head :: _) if head != SourceIndex.Root && !terms(head) =>
        namedImport(head, scope, at).fold(opens(anchor, scope, at))(path =>
          path.filterNot(_ == SourceIndex.Root) == anchor && rootHead(path, scope, at)
        )
      case _ => false
    }

  // Whether a path's first name is the root package it spells: anchored, or rebound by no named
  // import or source term.
  private def rootHead(path: List[String], scope: Frame, at: Option[(String, Int)]): Boolean =
    path match {
      case SourceIndex.Root :: _ => true
      case head :: _             => !terms(head) && namedImport(head, scope, at).isEmpty
      case Nil                   => false
    }

  // Whether a wildcard of the listed namespace declaring a term is open before `at`.
  private def opens(term: List[String], scope: Frame, at: Option[(String, Int)]): Boolean =
    scope.chain.exists(outer =>
      clauses(outer, at).exists(clause =>
        clause.wildcards.exists(prefix =>
          listed(prefix, outer, ShadowedBuiltins.at(outer, clause))
            .exists(_.termBinding(term.last).exists {
              case ListedNamespaces.Binding.Member(fqn) => fqn == term.mkString(".")
              case _                                    => false
            })
        )
      )
    )

  // The path the nearest named import of a simple name imports, if one binds it before `at`.
  private def namedImport(
      name: String,
      scope: Frame,
      at: Option[(String, Int)]
  ): Option[List[String]] =
    scope.chain.iterator
      .flatMap(outer =>
        clauses(outer, at).reverse.flatMap(_.named).collectFirst { case (`name`, path) => path }
      )
      .nextOption()

  // Why a name no supplied declaration and no model entry answers is not read. A name some supplied
  // declaration spells or a path through a supplied term is a gap in reading the sources; any other
  // is a declaration the sources do not hold, reached perhaps through a channel that leaves them.
  private def absent(name: String, context: ResolutionContext): String = {
    val head = name.takeWhile(_ != '.')
    if (typeNames(name.split('.').last) || (name.contains('.') && terms(head)))
      s"unresolved type: $name"
    else {
      val unlisted = namespaces(context).find(_.types.isEmpty).map { namespace =>
        s", may be a member of ${namespace.path} (${namespace.what}), whose members the model " +
          "does not list"
      }
      val through = channel(context, Some(head)).map(stop => s", may be bound ${route(stop)}")
      s"declaration not supplied: $name (${StdlibModel.undeclared}" +
        s"${unlisted.orElse(through).getOrElse("")})"
    }
  }

  /** Whether a supplied declaration, enum case or export selector spells this simple name: one no
    * channel can reach is not a declaration of the sources.
    */
  def mayBeSupplied(name: String): Boolean = spelled(name)

  /** A channel around the reference through which some declaration the sources do not supply, or
    * one this reading cannot follow, may bind a name: what is not read, and through what.
    */
  def unread(context: ResolutionContext): Option[String] =
    channel(context, None).map(refusal("builtin names", _))

  // The same for every name no supplied declaration spells, so it is read once per reference — and
  // per frame, not per frame id: a constructor's frame shares its class's id but not its members
  // (Scala 3.9.0 reads `class C(a: Any) extends X` with `scala.Any` beside `X`'s `type Any`).
  // A builtin's own search skips the clauses that leave its name out (`import p.{Boolean as _, *}`).
  private def channel(context: ResolutionContext, subject: Option[String]): Option[Stop] = {
    val key = (context.frame, context.at, subject)
    channels.getOrElse(
      key, {
        val before = cycles
        val found = guarded(Search(None, context.frame, context.at, subject))
        if (cycles == before) channels.update(key, found)
        found
      }
    )
  }

  private def guarded(search: Search): Option[Stop] =
    if (searching(search)) {
      cycles += 1
      Some(Step.Doubt(search.name.getOrElse("a channel"), SelfReached, selfReached = true))
    } else {
      searching += search
      try new Walk(search.name, search.name.orElse(search.subject)).from(search.frame, search.at)
      finally searching -= search
    }

  // One name's search — or, with no name, the search for a step that leaves the sources: each frame
  // whose members it may reach, once, with the step from the reference that first reached it. The
  // subject is the name whose binding is at stake: a wildcard that leaves it out is no channel.
  final private class Walk(name: Option[String], subject: Option[String]) {

    def from(frame: Frame, at: Option[(String, Int)]): Option[Stop] =
      search(frame.chain.flatMap(entrances(_, at)), Set.empty, None)

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
      val imported = clauses(scope, at).flatMap(clause =>
        subject.fold(clause.wildcards)(clause.opening).flatMap { prefix =>
          val via = s"wildcard import of ${prefix.mkString(".")}"
          targets(prefix, scope, via, ShadowedBuiltins.at(scope, clause))
        }
      )
      own ++ packaged ++ imported ++ unbound(scope)
    }

    // A package clause opens every member of its package, and one no supplied unit declares a member
    // of — the outer clause of `package a; package b`, with `a` written elsewhere — opens
    // declarations the sources do not hold (Scala 3.9.0 reads `Any` there as `a`'s `type Any`).
    private def unsupplied(scope: Frame): Boolean =
      scope.path.nonEmpty && !resolver.peers(scope.path).exists(SuppliedNamespaces.declaresMembers)

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
          val ref = TreeSpelling.of(importer.ref)
          val via = s"wildcard import of $ref"
          // A root-anchored prefix is a package path read from the root alone.
          if (ref.startsWith("_root_."))
            targets(ref.stripPrefix("_root_.").split('.').toList, scope.chain.last, via, None)
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
      frame.exported
        .flatMap { importer =>
          val prefix = SourceIndex.importPath(importer.ref)
          importer.importees.flatMap {
            case _: Importee.Wildcard if subject.exists(SourceIndex.hiddenNames(importer)) => Nil
            case _: Importee.Wildcard                                                      =>
              prefix.fold(List(Step.Doubt(s"export ${TreeSpelling.of(importer)}", via)))(
                targets(_, frame, via, None)
              )
            case selector if SourceIndex.boundName(selector).exists(isSearched) =>
              List(Step.Found(s"export ${TreeSpelling.of(importer)}", via))
            case _ => Nil
          }
        }

    // The frames a dotted prefix may denote, anchored at every package around `from` (a `_root_`
    // prefix at the root alone): objects, packages, and the companions of enums (their cases) and
    // classes (nothing this reads). A prefix whose first name a wildcard over a namespace no source
    // supplies may bind may lead outside the sources too, whatever it denotes there.
    private def targets(
        prefix: List[String],
        from: Frame,
        via: String,
        at: Option[(String, Int)]
    ): List[Step] = {
      val (relative, paths) = prefix match {
        case SourceIndex.Root :: path => (path, List(path))
        case _ => (prefix, from.chain.flatMap(_.path.inits).distinct.map(_ ++ prefix))
      }
      val frames = paths.flatMap(path => modules.getOrElse(path, Nil) ++ resolver.peers(path))
      val companions = paths.flatMap(classes.getOrElse(_, Nil)).flatMap(_.self)
      val outside = doubted(prefix, from, at).map(why =>
        Step.Unread(relative.mkString("."), s"$via: $why", doubted = true)
      )
      if (frames.nonEmpty || companions.nonEmpty)
        (frames ++ companions.filter(frame => enums(frame.id))).map(Step.Reach(_, via)) ++
          outside.orElse(open(paths, frames, relative, via))
      else if (relative.headOption.exists(terms)) List(Step.Doubt(relative.mkString("."), via))
      else
        listed(prefix, from, at).fold(List(Step.Unread(prefix.mkString("."), via)))(namespace =>
          outside.toList ++ declares(namespace, via)
        )
    }

    // A package the sources spell only as a prefix holds whatever a dependency declares there.
    private def open(
        paths: List[List[String]],
        frames: List[Frame],
        relative: List[String],
        via: String
    ): Option[Step] =
      Option.when(
        frames.forall(resolver.isPackage) &&
          !paths.flatMap(resolver.peers).exists(SuppliedNamespaces.declaresMembers)
      )(Step.Unread(relative.mkString("."), via))

    // A stdlib namespace the model lists binds the root spelling a search is about only where the
    // table says it declares it (`quotes.reflect.Singleton`).
    private def declares(namespace: ListedNamespaces.Namespace, via: String): Option[Step] =
      subject
        .flatMap(namespace.typeBinding(_, root = true))
        .collect { case ListedNamespaces.Binding.Member(fqn) => Step.Found(fqn, via) }

    // A parent, self type or alias body naming a supplied template reaches it; one that resolves to
    // an alias is followed; a name lookup refuses, or one a supplied type spells but lookup does not
    // find, may be that type.
    private def named(tpe: Type, from: Frame, via: String, depth: Int): List[Step] =
      parentName(tpe) match {
        case None                       => List(Step.Doubt(TreeSpelling.of(tpe), via))
        case Some(_) if depth > MaxHops => List(Step.Doubt(TreeSpelling.of(tpe), via))
        case Some(parent)               =>
          val at = Option.when(tpe.pos.start >= 0)(from.input -> tpe.pos.start)
          val context = ResolutionContext(from, Map.empty, Set.empty, at = at)
          // A `scala.Dynamic` parent is the structural-member marker (StdlibModel's parents
          // table holds it memberless): it declares no member a count reads, and the structural
          // calls its subclasses enable (`selectDynamic`, …) are outside the counting model —
          // the run logs the notice. The step supplies nothing and is followed no further,
          // spelled through the root package and rebound by no source declaration or import.
          if (
            dynamicMarker(parent) && !terms("Dynamic") && !packageTypeNames("Dynamic") &&
            namedImport("Dynamic", from, at).isEmpty
          ) Nil
          else
            resolver.resolveName(parent, context) match {
              case Found.Entry(entry) =>
                (entry.self, entry.tree) match {
                  case (Some(frame), _)         => List(Step.Reach(frame, via))
                  case (None, alias: Defn.Type) => named(alias.body, entry.owner, via, depth + 1)
                  case _                        => List(Step.Doubt(parent, via))
                }
              case _: Found.Blocked => List(Step.Doubt(parent, via))
              // An import binds it outside the sources, where only the stdlib model's summaries are
              // known to declare no type — and only where they are the root imports' declarations.
              case Found.Foreign(path, _) if InheritedTypeMembers.markerName(path) =>
                summarized(path, via, context)
              case Found.Foreign(path, _) => List(Step.Unread(path, via))
              // A bare name found nowhere may still be a package's type this reading missed; a type
              // nested in an object or class is reachable only through its container's name.
              case Found.Absent if spellable(parent) => List(Step.Doubt(parent, via))
              // A name found nowhere is outside the sources, unless it is a root or marker type.
              case Found.Absent if InheritedTypeMembers.markerName(parent) =>
                summarized(parent, via, context)
              case Found.Absent => List(Step.Unread(parent, via))
            }
      }

    private def summarized(name: String, via: String, context: ResolutionContext): List[Step] =
      standing(name, anchored = false, context).toList.map {
        case _: Step.Unread    => Step.Unread(name, via)
        case doubt: Step.Doubt => Step.Doubt(name, via, doubt.selfReached)
        case _: Step.Found     => Step.Doubt(name, via)
      }

  }

}

private[cardinality] object ShadowedBuiltins {

  // Alias hops a parent name is followed through before the walk gives up on it.
  private val MaxHops = 16

  // How a name a listed namespace's wildcard binds is reported when the model does not read it.
  private val Wildcard = "wildcard import"

  // Why a search doubts a parent whose standing it is itself establishing.
  private val SelfReached = "a search that reaches itself"

  // A clause's own position, where the names in its prefix are read — with the reference's or
  // without one: a clause never reads itself or a later clause (with every clause of the scope in
  // view, `import quotes.reflect.*` would ask after its own prefix forever).
  private def at(scope: Frame, clause: ImportBinding): Option[(String, Int)] =
    Some(scope.input -> (clause.start - 1))

  // One search: a name's, or the nameless search for a channel, from a frame at a position.
  private case class Search(
      name: Option[String],
      frame: Frame,
      at: Option[(String, Int)],
      subject: Option[String] = None
  )

  private enum Step {
    case Reach(frame: Frame, via: String)
    case Found(declaration: String, via: String)
    // A step this reading cannot follow, which may lead into the supplied sources — or one whose
    // standing rests on a search under way.
    case Doubt(unread: String, via: String, selfReached: Boolean = false)
    // A step that leads outside the supplied sources; `doubted` when only an unsupplied wildcard
    // that may rebind the prefix's first name leads it there.
    case Unread(unread: String, via: String, doubted: Boolean = false)
  }

  // Where a search stops: a declaration of the name, or a step it cannot see past.
  private type Stop = Step.Found | Step.Doubt | Step.Unread

  // What may shadow the name, and through which clause, follow the separator: a namespace such as
  // `cats` reads like prose, and a report tallies one category per kind of stop, not per namespace.
  private def refusal(subject: String, stop: Stop): String = stop match {
    case Step.Found(declaration, via) =>
      s"$subject may be shadowed by a declaration: $declaration ($via)"
    case doubt: Step.Doubt =>
      s"$subject may be shadowed through a reference not resolved: ${doubt.unread} (${why(doubt)})"
    case Step.Unread(what, via, _) =>
      s"$subject may be shadowed through a reference not supplied: $what ($via)"
  }

  // A step a search could not see past, as the way a binding may come through it.
  private def route(stop: Stop): String = stop match {
    case Step.Found(declaration, via) => s"through $declaration ($via)"
    case doubt: Step.Doubt => s"through ${doubt.unread}, which is not resolved (${why(doubt)})"
    case Step.Unread(what, via, _) => s"through $what, which is not supplied ($via)"
  }

  // The clause a doubt came through, and the search under way its standing rests on.
  private def why(doubt: Step.Doubt): String =
    if (doubt.selfReached && doubt.via != SelfReached) s"${doubt.via}, in $SelfReached"
    else doubt.via

  /** Whether a resolution's reason is this refusal: a builtin reading that may stand for a
    * different declaration, so no other reading of the same name may be trusted in its place.
    */
  def shadows(reason: String): Boolean =
    reason.startsWith("builtin name ") && reason.contains(" may be shadowed ")

  private def template(frame: Frame): Boolean = templateOf(frame).nonEmpty

  private def templateOf(frame: Frame): Option[Template] = frame.tree.flatMap {
    case d: Pkg.Object => Some(d.templ)
    case tree          => TypeApplications.template(tree)
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

  // The structural-member marker's spellings: `scala.Dynamic` itself, or the bare name the
  // caller checks against the source's own declarations and imports.
  private def dynamicMarker(parent: String): Boolean =
    parent == "scala.Dynamic" || parent == "Dynamic"

  private def qualified(frame: Frame): String = frame.path.mkString(".")

  // A tree as one line of a reason.

  private def label(frame: Frame): String = frame.tree match {
    case Some(_: Pkg.Object)        => s"package object ${qualified(frame)}"
    case None if frame.path.isEmpty => "the empty package"
    case None                       => s"package ${qualified(frame)}"
    case _                          => qualified(frame)
  }

  // The names export selectors bind: what a forwarder may put behind a builtin's spelling.
  private def exportedNames(source: Source): List[String] =
    source
      .collect { case clause: Export => clause.importers.flatMap(_.importees) }
      .flatten
      .flatMap(SourceIndex.boundName)

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
