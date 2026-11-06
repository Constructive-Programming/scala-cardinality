package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.StdlibModel
import cardinality.analysis.resolution.InheritedTypeMembers
import cardinality.analysis.resolution.InheritedTypeMembers.MemberKind
import cardinality.types.Trees.ClassDefn
import cardinality.types.{Overrides, Trees, TypeName, TypeSubstitution}

/** A template whose body a read is inside, when it has parents or a self type: the types they name,
  * the scope those are read in (`around`, outside the body), and the type and term names declared
  * at or inside the body around the read, which keep their meaning over what it inherits.
  */
final private[cardinality] case class Inheriting(
    parents: List[Type],
    around: Scope,
    types: Set[TypeName],
    terms: Set[TypeName]
) {

  /** The same template, read inside a body that declares `types` and `terms`. */
  def defining(more: Set[TypeName], moreTerms: Set[TypeName]): Inheriting =
    if (more.isEmpty && moreTerms.isEmpty) this
    else copy(types = types ++ more, terms = terms ++ moreTerms)

}

// Scala binds a simple name inside a template body to the template's members — inherited ones and
// those of its self type included — before any enclosing declaration of a package, an import or
// the root imports (Scala 3.9.0 reads `Boolean` in `object O extends Base { case class U(b:
// Boolean) }` as `Base`'s `type Boolean = Int` over a package's own `Boolean`, and a path's first
// name `p` as an inherited `val p: H.type` — a body member or a `val` parameter — over a package's
// `object p`: `StoredScopesSpec`'s `inheritedAliases`, `inheritedScopes`, `inheritedTerms`). A
// declaration at or inside the body keeps its meaning, and an enclosing class, object, method or
// block that declares the name too makes the reference ambiguous (E049), so a valid program never
// asks this reading to choose there. A parent outside the sources may declare any name — unless the
// stdlib model knows it declares none — and a parent's export clauses define aliases it passes on
// like declarations (Scala 3 reference, export clauses), which are not followed: where the meaning
// would otherwise come from a package, an import or the root imports, the read is refused with that
// parent named, as the method side's `InheritedTypeMembers` refuses it. A member is read in the body
// of the parent that declares it; one whose parent takes type arguments is refused.
private[capacity] object Inheritance {

  /** What a template passes on: a member (`Bound`), a reason (`Unread`), or nothing (`Absent`). */
  enum Found[+A] {
    case Bound[+A](value: A) extends Found[A]
    case Unread(reason: Definition.Unbound) extends Found[Nothing]
    case Absent extends Found[Nothing]
  }

  /** The scope `d`'s body reads in, from the scope around it, with `d` pushed when it has parents
    * or a self type; `terms` are the term names `d` binds besides its body's (its parameters).
    */
  def enter(scope: Scope, d: Defn, terms: Set[TypeName] = Set.empty): Scope =
    Walk.templateOf(d).map(parents).filter(_.nonEmpty).fold(scope) { named =>
      Scope.templatesLens.modify(Inheriting(named, scope, Set.empty, terms) :: _)(scope)
    }

  /** Whether an object of a body, at any depth, extends a class or trait of that body: a system
    * reads what it inherits only once it holds its classes' members (`Walk.system`).
    */
  def fromBody(stats: List[Stat]): Boolean = {
    def objects(in: List[Stat]): List[Defn.Object] =
      in.collect { case o: Defn.Object => o }.flatMap(o => o :: objects(o.templ.body.stats))
    def types(in: List[Stat]): List[String] = in.flatMap {
      case d: ClassDefn   => List(d.name.value)
      case o: Defn.Object => types(o.templ.body.stats)
      case _              => Nil
    }
    val defined = types(stats).toSet
    objects(stats).exists(o => parents(o.templ).flatMap(head).exists(defined))
  }

  // The last name a parent type is spelled with: `Base` of `p.Base[A]`.
  private def head(tpe: Type): Option[String] = Counter.nameOf(Trees.callee(tpe)).map(_.value)

  /** How a bare type name binds through the templates around the read, if it does there. */
  def bind(name: TypeName, templates: List[Inheriting]): Option[Binding] =
    walk(templates)(
      _.types(name),
      t => member(t.parents, Lookup(name, MemberKind.Type, t.around, Nil))(declaredType)
    ).map(_.fold(Binding.Clause(_), identity))

  /** What a path's first name names through the templates around the read, if it does there. */
  def term(first: TypeName, scope: Scope): Option[Namespaces.Target] =
    walk(Scope.templatesLens.get(scope))(
      _.terms(first),
      t => member(t.parents, Lookup(first, MemberKind.Term, t.around, Nil))(declaredTerm)
    ).map(_.fold(reason => Namespaces.Target.Unknown(Some(reason.render)), identity))

  // The templates innermost first: a name declared at or inside one keeps its meaning; else the
  // first that passes it on binds it, unless a template inside it has a parent that may (a reason);
  // past every template, such a parent still is one.
  private def walk[A](templates: List[Inheriting])(
      declares: Inheriting => Boolean,
      passes: Inheriting => Found[A]
  ): Option[Either[Definition.Unbound, A]] = {
    @tailrec
    def loop(
        rest: List[Inheriting],
        unread: Option[Definition.Unbound]
    ): Option[Either[Definition.Unbound, A]] =
      rest match {
        case Nil                   => unread.map(Left(_))
        case t :: _ if declares(t) => None
        case t :: outer            =>
          passes(t) match {
            case Found.Bound(value)   => Some(unread.toLeft(value))
            case Found.Unread(reason) => loop(outer, unread.orElse(Some(reason)))
            case Found.Absent         => loop(outer, unread)
          }
      }
    loop(templates, None)
  }

  /** One member looked up through parents: its name, whether it is a type or a term, the scope the
    * parent clauses are read in, and the parents on the way — a guard against a cycle of them.
    */
  final private case class Lookup(
      name: TypeName,
      kind: MemberKind,
      around: Scope,
      seen: List[Defn]
  ) {

    def through(members: Members): Lookup = copy(seen = members.defn :: seen)

    def from(members: Members): Lookup = copy(around = members.outside)

    /** Why the member is not read: a parent, through `channel`, may declare it. */
    def unread(channel: String): Found[Nothing] = Found.Unread(refusal(channel))

    def refusal(channel: String): Definition.Unbound =
      Definition.Unbound.Refusal(InheritedTypeMembers.unresolved(kind, name.value, channel))

  }

  private val unresolved = "whose declaration is not resolved"

  // What a member's declaration in a parent's body is, given what is read.
  private type Declared[A] = (Members, Lookup) => Found[A]

  // A member some parent passes on: a declaration of a parent's body, else its own parents'.
  private def member[A](parents: List[Type], lookup: Lookup)(declared: Declared[A]): Found[A] =
    first(parents.map(parent(_, lookup)(declared)))

  // A member any parent declares wins: Scala rejects two parents declaring it unequally, and an
  // abstract declaration is implemented by the concrete one. Only then may an unread parent.
  private def first[A](all: List[Found[A]]): Found[A] =
    all
      .collectFirst { case bound: Found.Bound[A] => bound }
      .orElse(all.collectFirst { case unread: Found.Unread => unread })
      .getOrElse(Found.Absent)

  // What one parent passes on: a source class or trait through its members (`declared`), nothing
  // for a stdlib parent the model knows declares no type member, and a reason for anything else.
  private def parent[A](tpe: Type, lookup: Lookup)(declared: Declared[A]): Found[A] =
    resolve(lookup.around, tpe) match {
      case Right(Some(members)) if lookup.seen.exists(_ eq members.defn) => Found.Absent
      case Right(Some(members)) => declared(members, lookup.through(members))
      case Right(None)          => Found.Absent
      case Left(channel)        => lookup.unread(channel)
    }

  /** What a parent type names, read where its clause is written: a source class or trait (its
    * members), a stdlib parent the model knows to declare no type member (None), or the channel an
    * unread one is named through — `unsupplied parent zz.Base`, `parent BA, whose declaration is
    * not read`, or the clause that binds its name.
    */
  def resolve(around: Scope, tpe: Type): Either[String, Option[Members]] = {
    val callee = Trees.callee(tpe)
    Namespaces.lookup(around, callee)((in, key, _) => Some(in.membersAt(key))) match {
      case Some(Some(members)) => Right(Some(members))
      case Some(None)          => Left(s"parent ${tpe.syntax}, whose declaration is not read")
      case None                =>
        bound(around, callee) match {
          case None if StdlibModel.parent(callee.syntax).nonEmpty => Right(None)
          case None      => Left(s"unsupplied parent ${tpe.syntax}")
          case Some(why) => Left(s"parent ${tpe.syntax}, $why")
        }
    }
  }

  // How the sources may bind a parent they do not resolve: a clause or a doubted path's prefix, by
  // what it says; None where they hold nothing of that spelling — a root or marker type then, or a
  // stdlib parent the model summarizes (`InheritedTypeMembers.markerName`), declares no type member.
  private def bound(around: Scope, callee: Type): Option[String] = callee match {
    case Type.Name(n) =>
      around.binding(TypeName.of(n)) match {
        case Binding.Free                                        => None
        case Binding.Clause(Definition.Unbound.Imported(_, why)) => Some(why)
        case _                                                   => Some(unresolved)
      }
    case Type.Select(q, _) =>
      Namespaces.path(q).map(Namespaces.target(around, _)).collect {
        case Namespaces.Target.Unknown(why)                         => why.getOrElse(unresolved)
        case _: Namespaces.Target.Value                             => "whose prefix is a value"
        case _: (Namespaces.Target.Package | Namespaces.Target.Obj) => unresolved
      }
    case _ => Some(unresolved)
  }

  // A type the parent's body declares under the name, read there: an abstract one is the
  // template's own to choose; an opaque one is hidden outside its body, where a reference reads
  // it as one value (`Counter`), under type arguments too; any other is not read under them. Else
  // an export of the body may define it, or its own parents pass it on.
  private def declaredType(members: Members, lookup: Lookup): Found[Binding] = {
    val name = lookup.name.value
    inherited(members).collectFirst(
      Trees.typeNamed(name).orElse { case d: Decl.Type if d.name.value == name => d }
    ) match {
      case Some(_: Decl.Type) => Found.Bound(Binding.Clause(Definition.Unbound.Member(name)))
      case Some(alias: Defn.Type) if Counter.isOpaqueAlias(alias) =>
        Found.Bound(Binding.Elsewhere(members.inside, lookup.name))
      case Some(_) if members.params.nonEmpty =>
        Found.Bound(Binding.Clause(underArguments(lookup, members)))
      case Some(_) => Found.Bound(Binding.Elsewhere(members.inside, lookup.name))
      case None    => unlessExported(declaredType)(members, lookup)
    }
  }

  // A term the parent declares under the name: an object or a value of its body, or a `val`
  // parameter — one of a singleton type is the path that type names, read where the value is
  // declared — else an export of the body may define it, or its own parents pass it on.
  private def declaredTerm(members: Members, lookup: Lookup): Found[Namespaces.Target] =
    Walk
      .termsOf(inherited(members))
      .get(lookup.name)
      .map(_ -> members.inside)
      .orElse(fields(members.defn).get(lookup.name).map(_ -> parameters(members))) match {
      case Some(_) if members.params.nonEmpty =>
        Found.Bound(Namespaces.Target.Unknown(Some(underArguments(lookup, members).render)))
      case Some((Bound.Value(Some(Type.Singleton(ref))), in)) =>
        Found.Bound(
          Namespaces.path(ref).fold(Namespaces.Target.Unknown(None))(Namespaces.target(in, _))
        )
      case Some((bound, in)) => Found.Bound(Namespaces.target(bound, in))
      case None              => unlessExported(declaredTerm)(members, lookup)
    }

  // The constructor parameters a subclass inherits as members: `val` and `var` ones, and a case
  // class's first clause — not a plain one, which is no member, nor a private one.
  private def fields(d: Defn): Map[TypeName, Bound] = {
    val kept = Overrides.inheritedFields(d).map(_.name.value)
    Walk.parameters(d).filter((name, _) => kept.contains(name.value)).toMap
  }

  // Where a constructor parameter's type is read: outside the body, with the class's binders and
  // its earlier parameters (`class C(val a: A, val b: a.T)`).
  private def parameters(members: Members): Scope = Counter.signature(members.defn)(members.outside)

  // A name the parent's export clauses may define is an alias member that is not followed;
  // otherwise its own parents may pass the name on.
  private def unlessExported[A](declared: Declared[A]): Declared[A] = (members, lookup) =>
    if (ParentExports.define(members, lookup.name, lookup.kind))
      lookup.unread(InheritedTypeMembers.exportsOf(Walk.named(members.defn).value))
    else ancestral(declared)(members, lookup)

  private def underArguments(lookup: Lookup, members: Members): Definition.Unbound =
    lookup.refusal(s"type arguments of ${members.owner.value}")

  // The statements of a parent's body a subclass inherits. A `private` (or `private[this]`) one is
  // not; a qualified-private one (`private[q]`) is, and Scala binds the name to it from anywhere —
  // a reference from outside `q` is an access error (Scala 3.9.0, E173), so in a valid program the
  // binding stands (`StoredScopesSpec`'s `qualifiedPrivate`).
  private def inherited(members: Members): List[Stat] =
    Walk.template(members.defn).filterNot {
      case member: Stat.WithMods => Overrides.unqualifiedPrivate(member.mods)
      case _                     => false
    }

  // What a parent's own parents pass on, read where its `extends` clause is written.
  private def ancestral[A](declared: Declared[A]): Declared[A] = (members, lookup) =>
    Walk
      .templateOf(members.defn)
      .fold(Found.Absent)(t => member(parents(t), lookup.from(members))(declared))

  // The types a template's extends clause and self type name: an intersection's parts each.
  private def parents(t: Template): List[Type] =
    t.inits.map(_.tpe) ++ t.body.selfOpt.flatMap(_.decltpe).toList.flatMap(conjuncts)

  private def conjuncts(tpe: Type): List[Type] = tpe match {
    case TypeSubstitution.Meet(l, r) => conjuncts(l) ++ conjuncts(r)
    case other                       => List(other)
  }

}
