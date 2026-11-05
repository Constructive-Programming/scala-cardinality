package cardinality.capacity

import scala.meta.*

import cardinality.types.*
import dev.constructive.eo.optics.Plated

// Objects and packages are namespaces (docs/research/stored-value-names.md). A type reference
// `O.T` names the member `T` that the body of the object or package `O` defines, and nothing else:
// no same-named type elsewhere, no last segment read on its own. Scala keeps types and terms
// apart, so an object is never a type — it is the namespace its members live in.
//
// An object's members are solved with the body that defines the object: every member, at any
// depth of objects, joins that body's system under its qualified key (`O.T`, `O.P.T`) and reads
// in the object's lexical view — its own names first, then the enclosing body's. A recursion that
// crosses an object boundary is then one system (μ, widening and the lazy ν completion alike), and
// a reference from outside reads the very value the object's own rows report.
private[cardinality] object Namespaces {

  // Trees are parsed and reprinted under Scala 3 dialect rules.
  private given scala3: Dialect = dialects.Scala3

  /** An object a body defines: its name, the key prefix its members are solved under, and the
    * definition.
    */
  final case class Module(name: TypeName, prefix: TypeName, defn: Defn.Object) {

    def stats: List[Stat] = defn.templ.body.stats

    def key(member: TypeName): TypeName = Namespaces.key(prefix, member)

    /** The objects the body defines, under keys inside this one's. */
    lazy val nested: List[Module] = modules(stats, Some(prefix))

    /** The cases of the enum the object accompanies, if it does: members of the object. */
    private lazy val cases: EnumCompanions = EnumCompanions.of(defn)

    // The term names the object binds: its body's, and its enum's cases (values not read).
    private lazy val terms: Map[TypeName, Bound] =
      cases.terms.map(_ -> Bound.Value(None)).toMap ++ Walk.termsOf(stats, key)

    /** What the body adds to the scope around it: its own types (shadowing the enclosing body's,
      * solved under this object's keys) and its term names — its enum's cases among both.
      */
    lazy val layer: Layer =
      Layer(types = Walk.typeNames(stats) ++ cases.types, terms = terms, rekey = Some(key))

    /** The term names the body binds, under their keys: how a path through the object (`O.I.T`, or
      * `I.T` under `import O.*`) finds its nested objects and values from outside.
      */
    def keyedTerms: Map[TypeName, Bound] = terms.map((n, b) => key(n) -> b)

    /** The class cases the object holds, under their keys, each with why it is not read. */
    def refusals: Map[TypeName, Definition.Unbound] =
      cases.types.map(n => key(n) -> cases.refusal(n)).toMap

    /** The clauses each statement of the body reads with. */
    lazy val clauses: Stat => Imports = Imports.before(stats)

    // Each type the body defines, by the statement that defines it.
    private lazy val statements: Map[TypeName, Stat] = Walk.statements(stats)

    /** The scope the body reads in, from a scope that holds its members under their keys: with what
      * its parents pass on behind its own names (`Inheritance`).
      */
    def view(scope: Scope): Scope = Inheritance.enter(scope, defn).within(layer)

    /** A scope at one of the body's statements: with the clauses written before it. */
    def at(st: Stat)(scope: Scope): Scope = scope.within(Layer(imports = clauses(st)))

    /** The same, at the statement that defines a type of the body. */
    def atType(name: TypeName)(scope: Scope): Scope = statements.get(name).fold(scope)(at(_)(scope))

  }

  def key(prefix: TypeName, member: TypeName): TypeName =
    TypeName.of(s"${prefix.value}.${member.value}")

  // The key infix of a name a clause binds to a namespace no body solves.
  private val imported = ".import "

  /** A key as its reference was written: a clause-bound name's own name, any other key itself. */
  def written(key: String): String = key.split(imported).last

  /** The objects a body defines, each keyed inside `outer` when it is nested. */
  def modules(stats: List[Stat], outer: Option[TypeName] = None): List[Module] =
    stats.collect { case d: Defn.Object => module(d, outer) }

  /** One object, keyed inside `outer` when it is nested. */
  def module(d: Defn.Object, outer: Option[TypeName]): Module = {
    val name = Walk.named(d)
    Module(name, outer.fold(name)(key(_, name)), d)
  }

  /** Every object, at any depth, with the view its body reads in: its own over the view its
    * defining statement reads in (`outer`).
    */
  def views(
      modules: List[Module],
      outer: Stat => Scope => Scope
  ): List[(Module, Scope => Scope)] =
    modules.flatMap { m =>
      val view: Scope => Scope = sc => m.view(outer(m.defn)(sc))
      (m, view) :: views(m.nested, st => sc => m.at(st)(view(sc)))
    }

  /** Every member equation of the objects, under its key and read in its object's view at its
    * defining statement, with that view: what the solver reads a member's own syntax in.
    */
  def lifted(
      modules: List[Module],
      outer: Stat => Scope => Scope
  ): List[((TypeName, Named), Scope => Scope)] =
    views(modules, outer).flatMap { (m, view) =>
      Counter.equations(m.stats).map {
        case (name, named) =>
          val at: Scope => Scope = sc => m.atType(name)(view(sc))
          (m.key(name) -> named.viewed(at), at)
      }
    }

  /** What the solver reads off a body with its objects: the body's own shapes with references to
    * its objects' members keyed, and each object's shapes under its keys. A name a member reads is
    * its object's own member first, then an enclosing object's, then the body's; a name a clause
    * written before it binds is the clause's — the key of the object member it names when the
    * object is solved here, else a key no body solves — and a definition's own parameters stay
    * themselves.
    */
  def shapes(stats: List[Stat], modules: List[Module], clauses: Stat => Imports): Shapes = {
    val top = modules.map(m => m.name -> m.prefix).toMap
    val solved = Clauses(top, everyModule(modules).map(m => m.prefix -> m.layer.types).toMap)
    val own = Walk.typeNames(stats)
    def names(st: Stat) = solved.keys(TypeName.of(""), clauses(st)) -- own
    modules.foldLeft(
      Shapes.of(stats, (d, tpe) => keyed(tpe, names(d) -- Counter.parameterNames(d), top))
    )((acc, m) => acc ++ moduleShapes(m, names(m.defn), solved))
  }

  /** Every object, at any depth. */
  private[capacity] def everyModule(modules: List[Module]): List[Module] =
    modules.flatMap(m => m :: everyModule(m.nested))

  private def moduleShapes(m: Module, outer: Map[TypeName, Type], around: Clauses): Shapes = {
    val solved = around.copy(modules = around.modules ++ m.nested.map(n => n.name -> n.prefix))
    val inScope = solved.modules
    def names(st: Stat) =
      outer ++ solved.keys(m.prefix, m.clauses(st)) ++
        m.layer.types.map(n => n -> Type.Name(m.key(n).value))
    val own = Shapes.of(
      m.stats,
      (d, tpe) => keyed(tpe, names(d) -- Counter.parameterNames(d), inScope)
    )
    val rekeyed = Shapes(
      own.aliases.map((n, body) => m.key(n) -> body),
      own.arms.map((n, arms) => m.key(n) -> arms),
      own.parents.map((n, children) => m.key(n) -> children.map(m.key)),
    )
    m.nested.foldLeft(rekeyed)((acc, n) => acc ++ moduleShapes(n, names(n.defn), solved))
  }

  /** The objects in scope by name, with their key prefixes, and the types each object (at any
    * depth, by prefix) defines: what a clause over an object solved in this system binds.
    */
  final private case class Clauses(
      modules: Map[TypeName, TypeName],
      types: Map[TypeName, Set[TypeName]]
  ) {

    // The names a statement's clauses bind, each as the key it is solved under, so the solver's
    // dependency graph links a recursion through a clause: a wildcard's members over an object
    // solved here (a later wildcard over an earlier one), and an explicit selector's member — or,
    // for one whose object is not solved here, a key no body solves, so the graph never links it to
    // a same-named definition the clause shadows.
    def keys(owner: TypeName, imports: Imports): Map[TypeName, Type] = {
      val wildcarded = imports.wildcards.reverse.flatMap { w =>
        prefix(w.path).toList.flatMap { p =>
          types.getOrElse(p, Set.empty).filterNot(w.excluded).map(n => n -> keyName(key(p, n)))
        }
      }
      val selected = imports.types.map { (n, selector) =>
        n -> prefix(selector.path).fold(keyName(TypeName.of(s"${owner.value}$imported${n.value}")))(
          p => keyName(key(p, selector.member))
        )
      }
      wildcarded.toMap ++ selected
    }

    private def prefix(path: List[TypeName]): Option[TypeName] = path match {
      case root :: rest if modules.contains(root) => Some(keyOf(modules(root), rest))
      case _                                      => None
    }

    private def keyName(name: TypeName): Type = Type.Name(name.value)
  }

  // A type with references to objects' members replaced by their keys (`O.T` → the name `O.T`),
  // then plain names by what `names` maps them to.
  private def keyed(
      tpe: Type,
      names: Map[TypeName, Type],
      modules: Map[TypeName, TypeName]
  ): Type = {
    val selects = Plated.transform[Type] {
      case select @ Type.Select(qualifier, Type.Name(member)) =>
        path(qualifier) match {
          case Some(root :: rest) if modules.contains(root) =>
            Type.Name(keyOf(modules(root), rest :+ TypeName.of(member)).value)
          case _ => select
        }
      case other => other
    }(tpe)(using TypeSubstitution.platedType)
    TypeSubstitution.replace(selects, names)
  }

  private def keyOf(prefix: TypeName, rest: List[TypeName]): TypeName = rest.foldLeft(prefix)(key)

  /** A stable path as the names it is made of: `a.b.c` as `a`, `b`, `c`, and `_root_` as the name
    * `_root_`. Anything else — `this`, `super`, an application — is not a path a namespace can be
    * read by.
    */
  def path(term: Term): Option[List[TypeName]] = term match {
    case Term.Name(name)                  => Some(List(TypeName.of(name)))
    case Term.Select(inner, Term.Name(n)) => path(inner).map(_ :+ TypeName.of(n))
    case _                                => None
  }

  private val root: TypeName = TypeName.of("_root_")

  /** What a stable path names: a supplied package, an object (the key prefix its members are solved
    * under, in the scope that solved them), a value with the type it is known to have, a name a
    * clause binds that the estimate does not follow (`provenance` says which), a path of the
    * standard library that `ListedNamespaces` may list (`Stdlib`), or nothing the sources hold.
    */
  enum Target {
    case Package(path: List[TypeName], library: Library)
    case Obj(prefix: TypeName, in: Scope, exposure: Exposure = Exposure.none)
    case Value(declared: Option[Type], in: Scope)
    case Unknown(provenance: Option[String])
    case Stdlib(path: List[String])
    case Absent

    /** The namespace one more segment of a path names. */
    def select(name: TypeName): Target = this match {
      case _: (Package | Obj)  => step(name).getOrElse(Absent)
      case Unknown(provenance) => Unknown(provenance)
      case _: Value            => Unknown(None)
      case Stdlib(path)        => Stdlib(path :+ name.value)
      case Absent              => Absent
    }

    // A package's object, value or subpackage of that name; an object's nested object or value.
    private[Namespaces] def step(name: TypeName): Option[Target] = this match {
      case Package(path, library) =>
        library
          .pkg(path)
          .flatMap(s => s.term(name).map(target(_, s)))
          .orElse(Option.when(library.isPackage(path :+ name))(Package(path :+ name, library)))
      case Obj(prefix, in, _) => in.term(key(prefix, name)).map(target(_, in))
      case _                  => None
    }

    /** How a wildcard over this namespace binds a type name: the namespace's own member, a reason —
      * a namespace the library has not solved yet, or one no source supplies or that is open to
      * dependencies (a package the sources spell only as a prefix), which may bind anything — or
      * nothing.
      */
    def member(name: TypeName, wildcard: Wildcard): Option[Found] = this match {
      case _ if supplied =>
        val exposure = this.exposure
        exposure
          .withheld(name, wildcard)
          .orElse(Option.when(declared(name).isEmpty)(exposure.unread(wildcard)).flatten)
          .map(why => Found.Reason(Definition.Unbound.Imported(name.value, why)))
          .orElse(declared(name))
      case Unknown(Some(why)) =>
        val first = wildcard.path.headOption.fold("")(_.value)
        Some(
          Found.Reason(
            Definition.Unbound.Imported(
              name.value,
              ListedNamespaces.mayBindThrough(wildcard.written, first, why)
            )
          )
        )
      case _: Stdlib => None
      case _ => Some(Found.Reason(Definition.Unbound.Imported(name.value, mayBind(wildcard))))
    }

    /** How a wildcard over this namespace binds a path's first name: the namespace it names, a
      * reason, or none when a listed library namespace declares no term of that name.
      */
    def head(name: TypeName, wildcard: Wildcard): Option[Target] = this match {
      case namespace @ (_: Package | _: Obj) if supplied =>
        val exposure = this.exposure
        exposure.withheld(name, wildcard) match {
          case Some(why) => Some(Unknown(Some(why)))
          case None      =>
            namespace.step(name).orElse(exposure.unread(wildcard).map(why => Unknown(Some(why))))
        }
      // A wildcard over a namespace that is itself in doubt passes on the clause it rests on.
      case Unknown(Some(why)) => Some(Unknown(Some(why)))
      case _: Stdlib          => None
      case _                  => Some(Unknown(Some(mayBind(wildcard))))
    }

    /** How an explicit clause over this namespace binds the type it names: the member a supplied
      * package or object defines; a member of a package of a cycle still being solved; none when
      * the namespace has only a term of that name (the clause imports no type); otherwise a reason
      * (`unfollowed`).
      */
    def selected(selector: Selector): Option[Found] =
      declared(selector.member).orElse {
        Option.unless(supplied && step(selector.member).isDefined)(
          Found.Reason(
            Definition.Unbound.Imported(selector.member.value, unfollowed(selector, this))
          )
        )
      }

    // What a clause over a supplied object or package imports of what its body defines.
    private def exposure: Exposure = this match {
      case Package(path, library) => library.exposure(path)
      case Obj(_, _, exposure)    => exposure
      case _                      => Exposure.none
    }

    // Whether this is a namespace a source supplies whole: an object, or a package a supplied file
    // declares a member in.
    private def supplied: Boolean = this match {
      case Package(path, library) => library.complete(path)
      case _: Obj                 => true
      case _                      => false
    }

    // The member a supplied package or object defines, or the reason a package of a cycle still
    // being solved gives; nothing for anything else.
    private def declared(name: TypeName): Option[Found] = this match {
      case Package(path, library) =>
        library.pkg(path) match {
          case Some(s) => Option.when(s.defines(name))(Found.Defined(s, name))
          case None    => library.pendingMember(path, name).map(Found.Reason(_))
        }
      case Obj(prefix, in, _) =>
        Option.when(in.defines(key(prefix, name)))(Found.Defined(in, key(prefix, name)))
      case _ => None
    }

    /** Whether a wildcard (or `given` selector) over this namespace may bind a term name. */
    def bindsTerm(name: TypeName, wildcard: Wildcard): Boolean =
      !supplied || head(name, wildcard).nonEmpty

    /** What `member` read through this namespace is. */
    def located(member: TypeName): Option[Located] = this match {
      case Package(path, library) =>
        library
          .pkg(path)
          .map(Located.Member(member, _))
          .orElse(library.pendingMember(path, member).map(Located.Pending(_)))
      case Obj(prefix, in, _)  => Some(Located.Member(key(prefix, member), in))
      case _: Value            => Some(Located.Unread(None))
      case Unknown(provenance) => Some(Located.Unread(provenance))
      case _: Stdlib | Absent  => None
    }

  }

  // Why an explicit clause's path names nothing the estimate reads: a value's member, or one an
  // object inherits or exports, is not followed; a path whose first name is in doubt (a wildcard
  // over an unsupplied namespace may bind it) passes the doubt on — the clause may well name a
  // supplied declaration; any other path names no supplied source.
  private def unfollowed(selector: Selector, path: Target): String = path match {
    case Target.Unknown(Some(why)) =>
      selector.because(s"${selector.path.headOption.fold("")(_.value)} $why")
    case _ => selector.because("not supplied")
  }

  /** The provenance of a name a wildcard over an unknown namespace may bind. */
  def mayBind(wildcard: Wildcard): String = ListedNamespaces.mayBind(wildcard.written)

  /** How a wildcard binds a name: a namespace's member under its key, or a reason. */
  enum Found {
    case Defined(in: Scope, key: TypeName)
    case Reason(reason: Definition.Unbound)
  }

  private[capacity] def target(bound: Bound, in: Scope): Target = bound match {
    case Bound.Module(prefix, exposure) => Target.Obj(prefix, in, exposure)
    case Bound.Value(declared)          => Target.Value(declared, in)
  }

  /** What a stable path names where it is read. Its first name resolves as Scala resolves an
    * identifier: the innermost term binding around the read — an object, a value or a parameter of
    * an enclosing body — then an explicit clause (the namespace it names), then a member of a
    * namespace a wildcard names (latest first; one no source supplies may bind it), then an object,
    * value or subpackage of the packages the read's clauses open, innermost first, then a root
    * package. `_root_.` starts from the root packages.
    */
  def target(scope: Scope, names: List[TypeName]): Target = stdlib(
    names,
    names match {
      case first :: rest if first != root => rest.foldLeft(head(scope, first))(_.select(_))
      case _                              => Target.Absent
    }
  )

  // A path the sources hold nothing at that spells a namespace `ListedNamespaces` lists from a root
  // package is the library's: its first name binds nothing of the sources.
  private def stdlib(names: List[TypeName], found: Target): Target = {
    val path = names.map(_.value)
    found match {
      case Target.Absent if ListedNamespaces.of(path).exists(_.anchor.isEmpty) =>
        Target.Stdlib(path.dropWhile(_ == root.value))
      case other => other
    }
  }

  private def head(scope: Scope, first: TypeName): Target =
    Inheritance.term(first, scope).getOrElse(unbound(scope, first))

  // A first name no template around the read inherits: its innermost term binding, else what the
  // clauses, the opened packages and the root packages bind.
  private def unbound(scope: Scope, first: TypeName): Target =
    scope.term(first) match {
      case Some(bound) => target(bound, scope)
      case None        =>
        scope.clause(first) match {
          case Some(selector) => selected(scope, selector)
          case None           =>
            val library = scope.library
            wildcardHead(scope, first)
              .orElse(
                scope.openedPackages.iterator
                  .map(packageHead(scope, _, first))
                  .find(_ != Target.Absent)
              )
              .orElse(
                Option.when(library.isPackage(List(first)))(Target.Package(List(first), library))
              )
              .getOrElse(Target.Absent)
        }
    }

  // A first name a package the read's clauses open holds: an object, value or subpackage of its
  // files, else what its export clauses and package object's parents pass on.
  private def packageHead(scope: Scope, path: List[TypeName], first: TypeName): Target =
    Target.Package(path, scope.library).select(first) match {
      case Target.Absent => scope.library.head(path).getOrElse(Target.Absent)
      case found         => found
    }

  // A first name an explicit clause binds: the namespace its member is, its path read with the
  // clauses written before it, or the clause as a reason when no supplied source declares it.
  private def selected(scope: Scope, selector: Selector): Target =
    target(scope.withClauses(selector.prior), selector.path) match {
      case path @ (_: Target.Package | _: Target.Obj) =>
        path.select(selector.member) match {
          case Target.Absent => Target.Unknown(Some(unfollowed(selector, path)))
          case found         => found
        }
      case other => Target.Unknown(Some(unfollowed(selector, other)))
    }

  // The first wildcard, latest first, whose namespace has a term of that name — or that names a
  // namespace no source supplies, which may.
  private def wildcardHead(scope: Scope, first: TypeName): Option[Target] =
    scope.wildcards.indices.iterator
      .flatMap { i =>
        val w = scope.wildcards(i)
        if (w.excluded(first)) None else target(scope.earlier(i), w.path).head(first, w)
      }
      .nextOption()

  /** What a qualifier denotes for the member it selects. */
  enum Located {

    /** An object or a package: the member's key and the scope that solved the namespace, which may
      * still not define the key — the qualifier resolved, the member did not.
      */
    case Member(key: TypeName, in: Scope)

    /** A value whose type is not known, a path through a value, or a name a clause binds: not
      * followed, so the reference is its own reason (with the clause, if one binds it).
      */
    case Unread(provenance: Option[String])

    /** A member of a package the library solves after this one, on a cycle of packages. */
    case Pending(reason: Definition.Unbound)
  }

  /** What a type reference names: the definition a bare name binds, or the one a qualified name
    * resolves to — with the scope that solved it, its key there, and whether it was qualified.
    * Anything else, a type parameter among it, names nothing.
    */
  def lookup[A](scope: Scope, tpe: Type)(
      found: (Scope, TypeName, Boolean) => Option[A]
  ): Option[A] =
    tpe match {
      case Type.Name(name) if !scope.isBinder(TypeName.of(name)) =>
        scope.binding(TypeName.of(name)) match {
          case Binding.Here               => found(scope, TypeName.of(name), false)
          case Binding.Elsewhere(in, key) => found(in, key, false)
          case _                          => None
        }
      case Type.Select(q, Type.Name(n)) =>
        locate(scope, q, TypeName.of(n)).flatMap {
          case Located.Member(key, in) => found(in, key, true)
          case _                       => None
        }
      case _ => None
    }

  /** What `q.member` reads through, when `q` is a stable path (see `target`). */
  def locate(scope: Scope, qualifier: Term, member: TypeName): Option[Located] =
    path(qualifier).flatMap(target(scope, _).located(member))

  // The qualifiers under which the standard library's base types and constructors are written in
  // full (`scala.Int`, `java.lang.String`, `scala.collection.immutable.List`): read as the base
  // types they name when no source package or object of that path exists — the one exception to
  // reading a qualified name through its qualifier.
  private val standard: Set[String] = Set(
    "scala",
    "_root_.scala",
    "scala.Predef",
    "Predef",
    "scala.collection.immutable",
    "_root_.scala.collection.immutable",
    "scala.util",
    "_root_.scala.util",
  )

  def standardQualifier(qualifier: Term): Boolean = standard(qualifier.syntax)

  /** A root-import throwable spelled through its own package or the `scala` alias of it. */
  def standardThrowable(select: Type.Select): Boolean = {
    val name = select.name.value
    BaseTypes.throwables.get(name).exists { throwable =>
      select.syntax.stripPrefix("_root_.") == throwable.fqn ||
      (throwable.scalaAlias && select.qual.syntax.stripPrefix("_root_.") == "scala")
    }
  }

}
