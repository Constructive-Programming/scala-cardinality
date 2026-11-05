package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.StdlibModel
import cardinality.analysis.resolution.InheritedTypeMembers
import cardinality.analysis.resolution.InheritedTypeMembers.MemberKind
import cardinality.types.{Overrides, Trees, TypeName}

/** A template whose body a read is inside, when it has parents: the types they name, the scope
  * those are read in (`around`, outside the body), and the type and term names declared at or
  * inside the body around the read, which keep their meaning over what it inherits.
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

// Scala binds a simple name inside a template body to the template's members, inherited ones
// included, before any enclosing declaration of a package, an import or the root imports (Scala
// 3.9.0 reads `Boolean` in `object O extends Base { case class U(b: Boolean) }` as `Base`'s
// `type Boolean = Int` over a package's own `Boolean`). A
// declaration at or inside the body keeps its meaning, and an enclosing class, object, method or
// block that declares the name too makes the reference ambiguous (E049), so a valid program never
// asks this reading to choose there. A parent outside the sources may declare any name — unless the
// stdlib model knows it declares none — and a parent's export clauses define aliases it passes on
// like declarations (Scala 3 reference, export clauses), which are not followed: where the meaning
// would otherwise come from a package, an import or the root imports, the read is refused with that
// parent named, as the method side's `InheritedTypeMembers` refuses it. A member a parent's body
// declares is not read: the read is refused with that parent named too.
private[capacity] object Inheritance {

  /** The scope `d`'s body reads in, from the scope around it, with `d` pushed when it has parents.
    * `terms` are the term names `d` binds besides its body's (its parameters).
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
      case d: (Defn.Class | Defn.Trait | Defn.Enum) => List(d.name.value)
      case o: Defn.Object                           => types(o.templ.body.stats)
      case _                                        => Nil
    }
    val defined = types(stats).toSet
    objects(stats).exists(o => parents(o.templ).flatMap(head).exists(defined))
  }

  // The last name a parent type is spelled with: `Base` of `p.Base[A]`.
  private def head(tpe: Type): Option[String] = Counter.nameOf(Trees.callee(tpe)).map(_.value)

  /** How a bare type name binds through the templates around the read, if it does there: the reason
    * a parent may declare it.
    */
  def bind(name: TypeName, templates: List[Inheriting]): Option[Binding] =
    walk(templates)(
      _.types(name),
      t => member(t.parents, Lookup(name, MemberKind.Type, t.around, Nil))(declaresType)
    ).map(Binding.Clause(_))

  /** What a path's first name names through the templates around the read, if it does there. */
  def term(first: TypeName, scope: Scope): Option[Namespaces.Target] =
    walk(Scope.templatesLens.get(scope))(
      _.terms(first),
      t => member(t.parents, Lookup(first, MemberKind.Term, t.around, Nil))(declaresTerm)
    ).map(reason => Namespaces.Target.Unknown(Some(reason.render)))

  // The templates innermost first: a name declared at or inside one keeps its meaning; else the
  // first parent that may declare it is the reason.
  private def walk(templates: List[Inheriting])(
      declares: Inheriting => Boolean,
      passes: Inheriting => Option[Definition.Unbound]
  ): Option[Definition.Unbound] = {
    @tailrec
    def loop(
        rest: List[Inheriting],
        unread: Option[Definition.Unbound]
    ): Option[Definition.Unbound] =
      rest match {
        case Nil                   => unread
        case t :: _ if declares(t) => None
        case t :: outer            => loop(outer, unread.orElse(passes(t)))
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
    def refusal(channel: String): Definition.Unbound =
      Definition.Unbound.Refusal(InheritedTypeMembers.unresolved(kind, name.value, channel))

  }

  private val unresolved = "whose declaration is not resolved"

  // Whether a parent's body declares the member, given what is read.
  private type Declares = (Members, Lookup) => Option[Definition.Unbound]

  // The first parent that may declare the member, through its own parents if need be.
  private def member(parents: List[Type], lookup: Lookup)(
      declares: Declares
  ): Option[Definition.Unbound] =
    parents.iterator.flatMap(parent(_, lookup)(declares)).nextOption()

  // What one parent passes on: a source class or trait through its members (`declares`), nothing
  // for a stdlib parent the model knows declares no type member, and a reason for anything else.
  private def parent(tpe: Type, lookup: Lookup)(declares: Declares): Option[Definition.Unbound] =
    resolve(lookup.around, tpe) match {
      case Right(Some(members)) if lookup.seen.exists(_ eq members.defn) => None
      case Right(Some(members)) => declares(members, lookup.through(members))
      case Right(None)          => None
      case Left(channel)        => Some(lookup.refusal(channel))
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
        case _: (Namespaces.Target.Package | Namespaces.Target.Obj) => unresolved
      }
    case _ => Some(unresolved)
  }

  // A member the parent's body declares is not read: the parent is the reason. Else an export of
  // the body may define it, or its own parents pass it on.
  private def declaresType(members: Members, lookup: Lookup): Option[Definition.Unbound] = {
    val name = lookup.name.value
    val declares = Trees.typeNamed(name).orElse { case d: Decl.Type if d.name.value == name => d }
    if (inherited(members).exists(declares.isDefinedAt)) Some(declaredBy(members, lookup))
    else unlessExported(declaresType)(members, lookup)
  }

  private def declaresTerm(members: Members, lookup: Lookup): Option[Definition.Unbound] =
    if (
      Walk.termsOf(inherited(members)).contains(lookup.name) ||
      Overrides.inheritedFields(members.defn).exists(_.name.value == lookup.name.value)
    ) Some(declaredBy(members, lookup))
    else unlessExported(declaresTerm)(members, lookup)

  private def declaredBy(members: Members, lookup: Lookup): Definition.Unbound =
    lookup.refusal(s"a member of ${members.owner.value}, not read")

  // A parent with export clauses may define any name as an alias member, which is not followed;
  // otherwise its own parents may pass the name on.
  private def unlessExported(declares: Declares): Declares = (members, lookup) =>
    if (Walk.template(members.defn).exists(_.is[Export]))
      Some(lookup.refusal(InheritedTypeMembers.exportsOf(Walk.named(members.defn).value)))
    else ancestral(declares)(members, lookup)

  // The statements of a parent's body a subclass inherits. A `private` (or `private[this]`) one is
  // not; a qualified-private one (`private[q]`) is, and Scala binds the name to it from anywhere —
  // a reference from outside `q` is an access error (Scala 3.9.0, E173), so in a valid program the
  // binding stands.
  private def inherited(members: Members): List[Stat] =
    Walk.template(members.defn).filterNot {
      case member: Stat.WithMods => Overrides.unqualifiedPrivate(member.mods)
      case _                     => false
    }

  // What a parent's own parents pass on, read where its `extends` clause is written.
  private def ancestral(declares: Declares): Declares = (members, lookup) =>
    Walk.templateOf(members.defn).flatMap(t => member(parents(t), lookup.from(members))(declares))

  // The types a template's extends clause names.
  private def parents(t: Template): List[Type] = t.inits.map(_.tpe)

}
