package cardinality.analysis.expansion

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.{MemberSyntax, ResolutionContext, ScopeTerms}
import cardinality.analysis.resolution.*
import cardinality.analysis.{MethodAnalysis, Resolver, SourceIndex}
import cardinality.types.{Overrides, Spelling, TypeApplications}

import MethodAnalysis.Frame

/** A supplied definition a body may reach: the tree, and the frame of its own parameters, whose
  * parent is the scope that declares it.
  */
final private[expansion] case class Definition(tree: Tree, frame: Frame) {
  def name: String = frame.path.lastOption.getOrElse("")
}

/** The instances a value may be, as far as its members go. */
private[expansion] enum Receiver {
  case Template(frame: Frame)
  case Library(key: String)
  case Variable
  case Unread(why: String)

  /** A value an expansion types as its argument's own class, a subclass of `receiver`'s: a
    * parameter of the expanded method (spelled `via`), or `this` of a template a class may extend.
    */
  case Open(receiver: Receiver, via: String)
}

/** What a term name or path denotes where an inline body reads it. */
private[expansion] enum Meaning {

  /** A value with its declared type (read in `scope`) or none, or one whose receiver is known;
    * `via` spells the path when an expansion may type it more narrowly (`Receiver.Open`).
    */
  case Value(tpe: Option[Type], scope: Frame, via: Option[String] = None)
  case Instance(receiver: Receiver)

  /** Supplied methods a call may select: every overload of the name in its scope. */
  case Methods(definitions: List[Definition])

  /** A supplied object. */
  case Module(frame: Frame)
  case Package(path: List[String], supplied: Boolean)

  /** A library object of the table (`Some`, `NonFatal`, …). */
  case Library(name: String)

  /** A member the reading lists without a definition to read — a library member or a universal one
    * — that takes at most `arity` argument clauses and returns `result`.
    */
  case Call(name: String, result: Receiver, arity: Int)

  /** A member of `scala.compiletime`, by its full name. */
  case CompileTime(fqn: String)
  case Refused(what: String)

  /** The refusal a meaning reads as, when it is one. */
  def refusal: Option[String] = this match {
    case CompileTime(fqn) => Some(s"uses $fqn")
    case Refused(what)    => Some(what)
    case _                => None
  }

}

/** The meaning of the names an inline body spells, read as Scala binds them (register D11, the
  * order `resolution.TermBindings` encodes for the method side): a scope's parameters, its own and
  * inherited members, its export and import clauses (an explicit selector before a wildcard), the
  * package's other files, then the root imports. A binding this reading cannot follow — an export,
  * an import or a parent outside the sources, a self type — is a refusal, never a guess.
  */
final private[expansion] class BodyTerms(resolver: Resolver) {

  private val ancestors = mutable.Map.empty[String, Either[String, List[Frame]]]

  /** The supplied ancestors of a template, nearest first, or why they are not read. */
  def levels(template: Frame): Either[String, List[Frame]] =
    ancestors.getOrElseUpdate(
      template.id, {
        val walked =
          InheritedScopes
            .walk(List(template -> resolver.typeParameters(template)), resolver, template)
        walked
          .collectFirst { case Left(reason) => reason }
          .toLeft(walked.flatMap(_.toOption).map(_.declared))
      }
    )

  /** What a simple name means read at offset `at` inside `frame`. The parameters of an inline
    * method (and of its extension group) are its expansion's arguments, which may be of a subclass.
    */
  def named(name: String, frame: Frame, at: Int): Meaning = {
    val expanded = expandedFrames(frame).map(_.id).toSet
    frame.chain.iterator
      .flatMap(scope => bound(name, scope, at, expanded(scope.id)))
      .nextOption()
      .getOrElse(rooted(name, frame, at))
  }

  // An inline method around the reading, and its extension group: a definition nested in the body
  // is copied into each expansion with the method's arguments in place of its parameters.
  private def expandedFrames(frame: Frame): List[Frame] =
    frame.chain.filter(_.tree.exists(MemberSyntax.inline)).flatMap { method =>
      method :: method.parent.filter(_.tree.exists(_.is[Defn.ExtensionGroup])).toList
    }

  private def bound(name: String, scope: Frame, at: Int, expanded: Boolean): Option[Meaning] =
    scope.params
      .find(_.name.value == name)
      .map(parameter(_, scope, expanded))
      .orElse(selfAlias(name, scope))
      .orElse(memberOf(scope, name))
      .orElse(own(scope.stats, name, scope))
      .orElse(imported(name, scope, at))
      .orElse(packaged(name, scope))

  // A template's members, its inherited ones included, and those of the synthetic object that holds
  // a package's top-level definitions (`Any`'s), as `TermBindings.scope` reads them.
  private def memberOf(scope: Frame, name: String): Option[Meaning] =
    Option
      .when(scope.isTemplate || resolver.isPackage(scope))(
        member(scope, name).map(throughThis(scope, name, _))
      )
      .flatten

  // A parameter of the expanded method is its argument; a class parameter kept as a field is a
  // member of `this`.
  private def parameter(param: Term.Param, scope: Frame, expanded: Boolean): Meaning = {
    val value: Meaning.Value = Meaning.Value(param.decltpe, scope)
    val name = param.name.value
    if (expanded) value.copy(via = Some(name))
    else if (scope.isTemplate && FinalImplementations.field(param, name, scope))
      throughThis(scope, name, value)
    else value
  }

  // A template's self alias is the template itself.
  private def selfAlias(name: String, scope: Frame): Option[Meaning] =
    self(scope).collect {
      case Self(Term.Name(alias), _) if alias == name => Meaning.Instance(thisOf(scope))
    }

  private def self(scope: Frame): Option[Self] =
    scope.tree.flatMap(TypeApplications.template).flatMap(_.body.selfOpt)

  /** `this` of a template, open: a class may extend it. */
  private def thisOf(template: Frame): Receiver =
    Receiver.Open(Receiver.Template(template), "this")

  // Expansions select a member on the receiver's class at the call site (fixtures review-A6 h3,
  // h7): an inline override there is expanded, so a member a subclass may override is refused on
  // `this`, and a field is a value of a type the subclass may narrow.
  private def throughThis(scope: Frame, name: String, found: Meaning): Meaning =
    narrowedMember(found, overrider(Receiver.Template(scope), name), name, "this")

  private def narrowedMember(
      found: Meaning,
      overrider: Option[String],
      name: String,
      via: String
  ): Meaning = (overrider, found) match {
    case (None, _) | (_, _: Meaning.Refused) => found
    case (_, value: Meaning.Value)           => value.copy(via = Some(s"$via.$name"))
    case (Some(label), _)                    =>
      Meaning.Refused(
        s"selects $name on $via, which a call site may type as a subclass of $label " +
          s"that overrides $name inline"
      )
  }

  // The class an open receiver's member may be overridden below, if a subclass may override it.
  private def overrider(receiver: Receiver, name: String): Option[String] = receiver match {
    case Receiver.Template(frame) =>
      Option.when(overridable(frame, name))(frame.path.mkString("."))
    case Receiver.Library(key) => Option.when(LibraryInlines.extensible(key))(key)
    case _                     => None
  }

  // A template no class extends: a final class, an object, a given, an enum (its cases have no
  // body to override with), an anonymous class, a package's synthetic object.
  private def closed(template: Frame): Boolean =
    !template.isTemplate || template.tree.exists(t =>
      FinalImplementations.isFinal(t) || t.is[Defn.Enum] || t.is[Term.NewAnonymous]
    )

  /** Whether a subclass of an open template may override its member `name`: some definition of the
    * name on its supplied ancestry is neither final, private, inline nor an object, or none is
    * supplied and the name is no universal member (which the model excludes) — a parent outside the
    * sources is refused where the member is read.
    */
  private def overridable(template: Frame, name: String): Boolean =
    !closed(template) && levels(template).forall { above =>
      val definitions = (template :: above).flatMap(definitionsOf(_, name))
      if (definitions.isEmpty)
        !(LibraryInlines.universal(name) || LibraryInlines.references(name))
      else definitions.exists(Overrides.overridable)
    }

  private def definitionsOf(level: Frame, name: String): List[Tree] =
    level.stats.filter(MemberSyntax.defined(_).contains(name)) ++ level.params.filter(p =>
      p.name.value == name && FinalImplementations.field(p, name, level)
    )

  /** The members of a template named `name`: its own (its parameters included), and what its
    * supplied ancestors pass on — their fields and their definitions but private ones. A parent
    * outside the sources may declare the name as well, which refuses, unless the library table
    * reads it (`Any`, `Product`, a function type, `Conversion`) without the name.
    */
  private def member(template: Frame, name: String): Option[Meaning] =
    levels(template) match {
      case Left(reason) =>
        Some(Meaning.Refused(s"reads $name, whose template is not read: $reason"))
      case Right(above) =>
        val found = declared(template, name) ++ above.flatMap(inherited(_, name))
        val frames = (template :: above).map(withoutFunctionParents(_, name))
        InheritedTerms.foreign(frames, name, resolver) match {
          case Some(parent) =>
            Some(Meaning.Refused(s"reads $name, which ${parent.parent} may declare (not supplied)"))
          case None => Option.when(found.nonEmpty)(combine(found))
        }
    }

  // A function type or `scala.Conversion` among a template's parents declares only the library
  // table's function members (`apply`, `andThen`, …): it binds no other name.
  private def withoutFunctionParents(frame: Frame, name: String): Frame =
    if (LibraryInlines.functionMember(name)) frame
    else frame.copy(parents = frame.parents.filterNot(functionParent(_, frame)))

  // A parent clause is read in the scope around the template, which its own parents do not reach.
  private def functionParent(init: Init, from: Frame): Boolean =
    resolver.outsideParent(init, from).exists(outside => LibraryInlines.functionClass(outside.path))

  private def inherited(level: Frame, name: String): List[Meaning] =
    declared(level.copy(stats = level.stats.filterNot(ScopeTerms.privateMember)), name)

  private def own(stats: List[Stat], name: String, scope: Frame): Option[Meaning] =
    Option.when(ScopeTerms.defined(stats)(name))(combine(declared(scope, name)))

  /** What a scope's own statements define under a name, each as a meaning. */
  private def declared(scope: Frame, name: String): List[Meaning] =
    scope.stats.flatMap {
      case d @ (_: Defn.Def | _: Decl.Def | _: Defn.GivenAlias)
          if MemberSyntax.names(d).contains(name) =>
        List(method(d, MemberSyntax.groups(d), scope))
      case v @ Valued(_, declared, _) if MemberSyntax.defined(v).contains(name) =>
        List(Meaning.Value(declared, scope))
      case o: Defn.Object if o.name.value == name => List(module(scope, name))
      case _                                      => Nil
    }

  private def combine(found: List[Meaning]): Meaning = {
    val methods = found.collect { case Meaning.Methods(ds) => ds }.flatten
    if (methods.nonEmpty) Meaning.Methods(methods)
    else
      found
        .collectFirst { case m: Meaning.Module => m }
        .orElse(found.headOption)
        .getOrElse(Meaning.Refused("reads a name no definition binds"))
  }

  private def method(tree: Tree, groups: List[Member.ParamClauseGroup], owner: Frame): Meaning =
    Meaning.Methods(
      List(
        Definition(
          tree,
          SourceIndex.methodFrame(
            tree,
            MemberSyntax.names(tree).headOption.getOrElse(""),
            groups,
            owner
          )
        )
      )
    )

  private def module(scope: Frame, name: String): Meaning =
    resolver.modules
      .find(m => m.parent.exists(_.id == scope.id) && m.path.lastOption.contains(name))
      .map(Meaning.Module(_))
      .getOrElse(Meaning.Refused(s"reads object $name, which is not indexed"))

  // Import clauses of one scope written before the reference: an explicit selector of the name
  // binds it; otherwise each wildcard is asked, and one this reading cannot list refuses.
  private def imported(name: String, scope: Frame, at: Int): Option[Meaning] = {
    val importers = scope.stats.collect {
      case i: Import if i.pos.start < at => i.importers.map(_ -> i.pos.start)
    }.flatten
    importers.iterator
      .flatMap((importer, start) => selected(importer, name).map(absolute(_, scope, start)))
      .nextOption()
      .orElse(
        importers.iterator
          .flatMap((importer, start) => opened(importer, name, scope, start))
          .nextOption()
      )
  }

  // The path an explicit selector binds the name to.
  private def selected(importer: Importer, name: String): Option[List[String]] =
    importer.importees
      .collectFirst {
        case Importee.Name(n) if n.value == name => n.value
      }
      .flatMap(member => SourceIndex.importPath(importer.ref).map(_ :+ member))

  // The prefix is read before the clause, so no clause binds its own first segment.
  private def opened(
      importer: Importer,
      name: String,
      scope: Frame,
      start: Int
  ): Option[Meaning] = {
    if (!ScopeTerms.opensAll(importer)) None
    else
      SourceIndex.importPath(importer.ref) match {
        case None =>
          Some(Meaning.Refused(s"reads $name, which ${Spelling.of(importer.ref)}.* may bind"))
        case Some(path) => openedBy(path, name, scope, start)
      }
  }

  private def openedBy(path: List[String], name: String, scope: Frame, at: Int): Option[Meaning] =
    absolute(path, scope, at) match {
      case Meaning.Package(full, true) if !LibraryInlines.libraryPackage(full) =>
        packageMember(full, name, None)
      case Meaning.Module(frame) => member(frame, name)
      case _                     =>
        Some(
          Meaning.Refused(
            s"reads $name, which the wildcard import ${path.mkString(".")}.* may bind"
          )
        )
    }

  // A path written in an import or a selection, its first segment read where the clause is.
  private def absolute(path: List[String], scope: Frame, at: Int): Meaning = path match {
    case head :: rest => from(named(head, scope, at), rest)
    case Nil          => Meaning.Refused("reads an empty path")
  }

  private def from(start: Meaning, rest: List[String]): Meaning =
    rest.foldLeft(start)((meaning, segment) => select(meaning, segment))

  // Another file of the package defines the name, or the package holds a subpackage of it.
  private def packaged(name: String, scope: Frame): Option[Meaning] =
    Option.when(resolver.isPackage(scope))(packageMember(scope.path, name, Some(scope.id))).flatten

  private def packageMember(
      path: List[String],
      name: String,
      except: Option[String]
  ): Option[Meaning] = {
    val peers = resolver.peers(path).filterNot(peer => except.contains(peer.id))
    val found =
      peers.filter(peer => ScopeTerms.defined(peer.stats)(name)).flatMap(declared(_, name))
    if (found.nonEmpty) Some(combine(found))
    else
      Option.when(resolver.namespace(path :+ name))(Meaning.Package(path :+ name, supplied = true))
  }

  // A name no scope binds: the root imports, or a top-level package of the sources.
  private def rooted(name: String, frame: Frame, offset: Int): Meaning = {
    val context = at(frame, offset)
    resolver.unreadChannel(context).orElse(resolver.rootShadow(name, context)) match {
      case Some(reason) =>
        Meaning.Refused(s"reads $name, which may be bound outside the sources: $reason")
      case None =>
        if (resolver.namespace(List(name))) Meaning.Package(List(name), supplied = true)
        else
          LibraryInlines.root(name) match {
            case Some(LibraryInlines.Root.Module(module)) => Meaning.Library(module)
            case Some(LibraryInlines.Root.Package(path))  => Meaning.Package(path, supplied = false)
            case None                                     =>
              Meaning.Refused(
                s"reads $name, which is declared neither in the sources nor in the library table"
              )
          }
    }
  }

  /** A context reading names at `offset` of `frame`'s source. */
  def at(frame: Frame, offset: Int): ResolutionContext =
    ResolutionContext(frame, Map.empty, Set.empty, at = Some(frame.input -> offset))

  /** A member of what `meaning` denotes, selected by a path or an expression. */
  def select(meaning: Meaning, name: String): Meaning = meaning match {
    case Meaning.Package(path, supplied) => inPackage(path, name, supplied)
    case Meaning.Module(frame)           =>
      member(frame, name).getOrElse(
        Meaning.Refused(s"selects $name of object ${frame.path.mkString(".")}, which declares none")
      )
    case value: Meaning.Value       => selectOn(receiverOf(value), name)
    case Meaning.Instance(receiver) => selectOn(receiver, name)
    case refused: Meaning.Refused   => refused
    case _                          => Meaning.Refused(s"selects $name, not read")
  }

  // A call this reading lists, whose result it does not read.
  private def call(name: String, arity: Int): Meaning.Call =
    Meaning.Call(name, Receiver.Unread(s"the result of $name"), arity)

  /** The receiver a value denotes: its declared type's, open when an expansion may narrow it. */
  def receiverOf(value: Meaning.Value): Receiver = {
    val declared = receiver(value.tpe, value.scope)
    value.via.fold(declared)(Receiver.Open(declared, _))
  }

  private def inPackage(path: List[String], name: String, supplied: Boolean): Meaning = {
    val full = path :+ name
    if (LibraryInlines.libraryPackage(full))
      Meaning.Package(full, supplied = resolver.namespace(full))
    else
      LibraryInlines.compileTime(full) match {
        case Some(fqn) => Meaning.CompileTime(fqn)
        case None      =>
          Option
            .when(supplied)(packageMember(path, name, None))
            .flatten
            .getOrElse(
              Meaning.Refused(
                s"reads ${full.mkString(".")}, which the sources and the library table do not declare"
              )
            )
      }
  }

  /** A member of an instance. */
  def selectOn(receiver: Receiver, name: String): Meaning = receiver match {
    case _ if LibraryInlines.universal(name) => call(name, 0)
    case Receiver.Open(inner, via)           =>
      narrowedMember(selectOn(inner, name), overrider(inner, name), name, via)
    case Receiver.Template(frame) =>
      if (LibraryInlines.references(name)) call(name, 1)
      else
        member(frame, name).getOrElse(
          Meaning.Refused(
            s"selects $name, no member of ${frame.path.mkString(".")} (an extension or conversion is not resolved)"
          )
        )
    case Receiver.Library(key) =>
      if (LibraryInlines.members(key).exists(_(name)))
        Meaning.Call(
          name,
          Receiver.Unread(s"the result of $name on $key, which depends on its arguments"),
          LibraryInlines.clauses(name)
        )
      else Meaning.Refused(s"selects $name on $key, which the library table does not list")
    case _ => Meaning.Refused(s"selects $name on a value whose type is not read")
  }

  /** The receiver a declared type denotes, read in `scope`; `variables` are type parameters bound
    * around it besides the scope's own.
    */
  def receiver(tpe: Option[Type], scope: Frame, variables: Set[String] = Set.empty): Receiver =
    tpe.fold(Receiver.Unread("a type the body infers"))(classOf(_, scope, variables, Set.empty))

  private def classOf(
      tpe: Type,
      scope: Frame,
      variables: Set[String],
      seen: Set[String]
  ): Receiver =
    tpe match {
      case f: Type.Function => Receiver.Library(s"Function${f.paramClause.values.size}")
      case Type.Tuple(args) if args.forall(!_.is[Type.TypedParam]) =>
        Receiver.Library(s"Tuple${args.size}")
      case a: Type.Apply => classOf(a.tpe, scope, variables, seen)
      case Type.Name(name) if variables(name) || variable(name, scope) => Receiver.Variable
      case named @ (_: Type.Name | _: Type.Select) => declaredClass(named, scope, variables, seen)
      case other                                   => Receiver.Unread(Spelling.of(other))
    }

  // Whether a type name is a type parameter of a scope around `scope`.
  private def variable(name: String, scope: Frame): Boolean =
    scope.chain.exists(_.typeParams.exists(_.name.value == name))

  /** What a type name written in `scope` denotes there, and the context it is read in. */
  def typeDeclaration(named: Type, scope: Frame): (Found, ResolutionContext) = {
    val context = at(scope, named.pos.start)
    (resolver.declaration(named, TypeApplications.name(named), context), context)
  }

  private def declaredClass(
      named: Type,
      scope: Frame,
      variables: Set[String],
      seen: Set[String]
  ): Receiver = {
    val (found, context) = typeDeclaration(named, scope)
    found match {
      case Found.Entry(entry) =>
        entry.tree match {
          case _: (Defn.Class | Defn.Trait | Defn.Enum) =>
            entry.self.map(Receiver.Template(_)).getOrElse(Receiver.Unread(entry.key))
          case alias: Defn.Type if !seen(entry.key) =>
            classOf(
              alias.body,
              entry.owner,
              variables ++ alias.tparamClause.values.map(_.name.value),
              seen + entry.key
            )
          case _ => Receiver.Unread(entry.key)
        }
      case Found.Absent =>
        resolver.builtin(named.syntax, context).fold(Receiver.Unread(_), Receiver.Library(_))
      case failed: Found.Failed => Receiver.Unread(failed.reason)
    }
  }

}
