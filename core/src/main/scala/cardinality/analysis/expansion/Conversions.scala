package cardinality.analysis.expansion

import scala.meta.*

import cardinality.analysis.MethodAnalysis.{Frame, TypeEntry}
import cardinality.analysis.model.{MemberSyntax, ScopeTerms}
import cardinality.analysis.resolution.Found
import cardinality.analysis.{Resolver, SourceIndex}
import cardinality.types.{Spelling, TypeApplications}

/** The implicit conversions and extensions of the sources an inline body may meet (register D27).
  *
  * The compiler inserts a conversion around an expression whose type does not conform, and tries a
  * conversion or an extension when no member of a selected name accepts the arguments (reference,
  * *Implicit Conversions*, *Extension Methods*; fixture a6 fb). A conversion is a given or an
  * implicit value whose type is `scala.Conversion` or a supplied class extending it — however the
  * type is spelled: through an alias, a renaming import — an implicit object extending it, an
  * implicit method of one value clause, or an implicit class. It is inline when it is an inline
  * given or method, or when the `apply` it runs is. Scala looks for them where the body is written
  * and in the implicit scope of the types involved; this reading does not type the arguments, so it
  * keeps every declaration that may be found from the body: one the body or a method around it
  * declares, one of an enclosing scope (a package the body is in included) or of a template the
  * body's templates inherit, one an import of the body's scopes names, every member of an object
  * that holds a type or is a companion (some type's implicit scope), of a class or trait (an object
  * may inherit it), of a given instance, and of a package that declares an opaque type (its
  * top-level wrapper is the opaque type's implicit scope), and what an export clause defines again
  * in a scope found so (an export defines aliases, register D11). A definition local to another
  * method is visible nowhere else. A class outside the sources is not read as a conversion (its
  * parents are not supplied; the view rule's assumption, register D20).
  */
final private[expansion] class Conversions(resolver: Resolver, terms: BodyTerms) {

  /** A conversion a reason names, whether it is expanded where it is inserted, its scope, and the
    * inline method or given alias it expands, where it is one.
    */
  final case class Candidate(
      label: String,
      inline: Boolean,
      scope: Frame,
      expands: Option[Definition] = None,
      declared: Option[Stat] = None
  )

  private lazy val conversions: List[Candidate] =
    resolver.memberScopes.flatMap(scope =>
      scope.stats.flatMap(stat => conversion(stat, scope).map(_.copy(declared = Some(stat))))
    )

  private lazy val extensions: List[Extension] =
    resolver.memberScopes.flatMap(scope => extensionsIn(scope.stats, scope))

  // Export clauses, with the scope each defines its aliases in.
  private lazy val exports: List[(Frame, List[Importer])] =
    resolver.memberScopes.flatMap(scope =>
      scope.stats.collect { case e: Export => scope -> e.importers }
    )

  // The extensions an expansion would not run as one function of its values were they selected:
  // inline ones, and those that take a contextual clause.
  private def extensionsIn(stats: List[Stat], scope: Frame): List[Extension] =
    stats
      .collect { case e: Defn.ExtensionGroup => e }
      .flatMap(group =>
        ScopeTerms
          .extensionMethods(group)
          .filter(contextualExtension(group, _))
          .map(Extension(group, _, scope))
      )

  private def contextualExtension(
      group: Defn.ExtensionGroup,
      method: Defn.Def | Decl.Def
  ): Boolean =
    MemberSyntax.inline(method) ||
      (group.paramClauseGroup.toList.flatMap(_.paramClauses) ++
        MemberSyntax.groups(method).flatMap(_.paramClauses)).exists(MemberSyntax.contextual)

  /** The inline conversions a body read in `frame` may meet. */
  def inlineConversions(frame: Frame): Iterator[Candidate] = candidates(frame).filter(_.inline)

  /** The supplied inline methods of a name, each with its frame: what a conversion's result may run
    * when a selection of the name is applied through it.
    */
  def inlineMethods(name: String): List[Definition] = inlines.getOrElse(name, Nil)

  private lazy val inlines: Map[String, List[Definition]] =
    resolver.memberScopes
      .flatMap(scope =>
        scope.stats.flatMap {
          case d: Defn.Def if MemberSyntax.inline(d) => List(expansion(d, scope))
          case e: Defn.ExtensionGroup                =>
            val frame = SourceIndex.extensionFrame(e, scope)
            ScopeTerms.extensionMethods(e).collect {
              case d: Defn.Def if MemberSyntax.inline(d) => expansion(d, frame)
            }
          case _ => Nil
        }
      )
      .groupBy(_.tree match {
        case d: Defn.Def => d.name.value
        case _           => ""
      })

  private def expansion(d: Defn.Def | Defn.GivenAlias, scope: Frame): Definition =
    Definition(
      d,
      SourceIndex.methodFrame(
        d,
        MemberSyntax.names(d).headOption.getOrElse(""),
        MemberSyntax.groups(d),
        scope
      )
    )

  /** The first conversion that may answer an applied selection read in `frame`. */
  def conversion(frame: Frame): Option[Candidate] = candidates(frame).nextOption()

  /** The extensions of the name, inline or contextual, a selection in `frame` may meet, each with
    * the scope declaring it; unknown when one is local to a method around the body (no scope this
    * reading resolves it in).
    */
  def extensionsOf(name: String, frame: Frame): Extensions =
    if (extensionsIn(local(frame), frame).exists(_.name == name)) Extensions.Unknown
    else Extensions.Known(extensions.filter(e => e.name == name && visible(e.scope, frame)))

  // The body's own conversions and those of the methods around it, then the visible ones.
  private def candidates(frame: Frame): Iterator[Candidate] =
    local(frame).iterator.flatMap(conversion(_, frame)) ++
      conversions.iterator.filter(c => visible(c.scope, frame) && granted(c, frame))

  // A conversion `private[p]` to a package `p` is inserted only inside `p`: Scala never selects an
  // inaccessible implicit. Another restriction is kept (its grant is not read here).
  private def granted(c: Candidate, frame: Frame): Boolean =
    c.declared.forall {
      case stat: Stat.WithMods if packagePrivate(stat.mods, c.scope) =>
        resolver.accessible(stat, c.scope, frame)
      case _ => true
    }

  private def packagePrivate(mods: List[Mod], scope: Frame): Boolean =
    mods.exists {
      case Mod.Private(within: Name) if !within.is[Name.Anonymous] =>
        scope.chain.exists(f => resolver.isPackage(f) && f.path.contains(within.value))
      case _ => false
    }

  // The statements of the methods around a body, its own method included: what they define is
  // visible inside them alone.
  private def local(frame: Frame): List[Stat] =
    frame.chain
      .flatMap(_.tree)
      .collect { case method: (Defn.Def | Defn.GivenAlias) => method }
      .flatMap(_.collect { case stat: Stat => stat }.drop(1))

  private def conversion(stat: Stat, scope: Frame): Option[Candidate] = stat match {
    case g: Defn.GivenAlias if converts(g.decltpe, scope) =>
      Some(
        candidate(g.name, g.decltpe, MemberSyntax.inline(g), scope)
          .copy(expands = Option.when(MemberSyntax.inline(g))(expansion(g, scope)))
      )
    case g: Decl.Given if converts(g.decltpe, scope) =>
      Some(candidate(g.name, g.decltpe, expanded = false, scope))
    case g: Defn.Given =>
      g.templ.inits
        .find(init => converts(init.tpe, scope))
        .map(init => candidate(g.name, init.tpe, instanceApply(g.templ, scope), scope))
    case o: Defn.Object if implicitConversionObject(o, scope) =>
      Some(Candidate(o.name.value, instanceApply(o.templ, scope), scope))
    case Implicit(pats, tpe) if converts(tpe, scope) =>
      Some(
        Candidate(TypeApplications.boundNames(pats).mkString(", "), inlineApply(tpe, scope), scope)
      )
    case d: Defn.Def
        if MemberSyntax.implicitModifier(d.mods) && oneValueClause(d.paramClauseGroups) =>
      Some(
        Candidate(
          d.name.value,
          MemberSyntax.inline(d),
          scope,
          Option.when(MemberSyntax.inline(d))(expansion(d, scope))
        )
      )
    case c: Defn.Class if MemberSyntax.implicitModifier(c.mods) =>
      Some(Candidate(c.name.value, inline = false, scope))
    case _ => None
  }

  // A conversion given whose own type is no class: named, or by its type when anonymous.
  private def candidate(name: Name, tpe: Type, expanded: Boolean, scope: Frame): Candidate =
    Candidate(
      if (name.value.isEmpty) s"given ${Spelling.of(tpe)}" else name.value,
      expanded || inlineApply(tpe, scope),
      scope
    )

  /** An `implicit` value or variable with a declared type: its patterns and that type. */
  private object Implicit {

    def unapply(stat: Stat): Option[(List[Pat], Type)] = stat match {
      case member: Stat.WithMods if MemberSyntax.implicitModifier(member.mods) =>
        stat match {
          case Valued(pats, Some(tpe), _) => Some(pats -> tpe)
          case _                          => None
        }
      case _ => None
    }

  }

  private def implicitConversionObject(o: Defn.Object, scope: Frame): Boolean =
    MemberSyntax.implicitModifier(o.mods) && o.templ.inits.exists(i => converts(i.tpe, scope))

  private def oneValueClause(groups: List[Member.ParamClauseGroup]): Boolean =
    groups.flatMap(_.paramClauses).filterNot(MemberSyntax.contextual).map(_.values.size) == List(1)

  // An instance's `apply`: its own, or one its supplied parents declare.
  private def instanceApply(templ: Template, scope: Frame): Boolean =
    appliesInline(templ.body.stats) || templ.inits.exists(init => inlineApply(init.tpe, scope))

  private def appliesInline(stats: List[Stat]): Boolean =
    stats.exists(stat => MemberSyntax.inline(stat) && MemberSyntax.names(stat).contains("apply"))

  /** Whether a type is `scala.Conversion`, an alias of it or a supplied class extending it; a name
    * this reading cannot resolve may be any of these. Each alias and class is followed once on the
    * way (`seen`): a declaration met again closes a cycle valid Scala cannot write, and answers
    * that it may convert.
    */
  private def converts(tpe: Type, scope: Frame): Boolean = conversionType(tpe, scope, Set.empty)

  private def conversionType(tpe: Type, scope: Frame, seen: Set[String]): Boolean =
    named(tpe).exists { name =>
      terms.typeDeclaration(name, scope) match {
        case (Found.Entry(entry), _) => declaresConversion(entry, seen)
        // A wildcard over a namespace no source supplies may bind the name: the supplied
        // declaration it hides is read; the alternative, a declaration outside the sources, is not
        // read as a conversion, as no class outside the sources is (above).
        case (Found.Blocked(_, Some(entry)), _) => declaresConversion(entry, seen)
        case (Found.Absent, context)            =>
          LibraryInlines.conversionClass(name.syntax) &&
          resolver.rootShadow(name.syntax, context).isEmpty
        case (Found.Foreign(path, _), _) => LibraryInlines.conversionClass(path)
        case (_: Found.Blocked, _)       => true
      }
    }

  private def declaresConversion(entry: TypeEntry, seen: Set[String]): Boolean =
    seen(entry.key) || (entry.tree match {
      case alias: Defn.Type => conversionType(alias.body, entry.owner, seen + entry.key)
      case _                => entry.self.exists(extendsConversion(_, seen + entry.key))
    })

  // The head a type applies, when it is a name or a path.
  private def named(tpe: Type): Option[Type] =
    TypeApplications.headAndArguments(tpe)._1 match {
      case name: (Type.Name | Type.Select) => Some(name)
      case _                               => None
    }

  private def extendsConversion(template: Frame, seen: Set[String]): Boolean =
    lineage(template).exists(level =>
      level.parents.exists(init => conversionType(init.tpe, level.parent.getOrElse(level), seen))
    )

  private def lineage(template: Frame): List[Frame] =
    template :: terms.levels(template).getOrElse(Nil)

  // A conversion's type whose `apply` is inline: a supplied class declaring one, itself or
  // through a supplied ancestor.
  private def inlineApply(tpe: Type, scope: Frame): Boolean =
    named(tpe).exists(name =>
      terms.typeDeclaration(name, scope)._1 match {
        case Found.Entry(entry) =>
          entry.self.exists(lineage(_).exists(level => appliesInline(level.stats)))
        case _ => false
      }
    )

  private def visible(scope: Frame, frame: Frame): Boolean = reached(scope, frame, Set.empty)

  /** Whether a member of `scope` may be found from a body read in `frame`: the scope encloses it,
    * an import around it names the scope, the scope is some type's implicit scope, or an export
    * clause defines the member again in a scope found so. Reachability is the least answer, so a
    * scope already asked on the way (`asked`, an export cycle) adds nothing.
    */
  private def reached(scope: Frame, frame: Frame, asked: Set[String]): Boolean =
    enclosing(scope, frame) || imported(scope, frame) || implicitScope(scope) ||
      exports.exists((owner, importers) =>
        !asked(owner.id) && names(importers, scope) && reached(owner, frame, asked + scope.id)
      )

  private def enclosing(scope: Frame, frame: Frame): Boolean =
    frame.chain.exists(around =>
      around.id == scope.id ||
        (resolver.isPackage(around) && resolver.isPackage(scope) &&
          around.path.startsWith(scope.path)) ||
        (around.isTemplate && terms.levels(around).exists(_.exists(_.id == scope.id)))
    )

  private def imported(scope: Frame, frame: Frame): Boolean =
    (frame.chain.flatMap(_.stats) ++ local(frame)).exists {
      case i: Import => names(i.importers, scope)
      case _         => false
    }

  // Whether clauses name the scope by its path's last segment (a relative path included); a path
  // this reading cannot spell may name any.
  private def names(importers: List[Importer], scope: Frame): Boolean =
    importers.exists(importer =>
      SourceIndex.importPath(importer.ref).forall(_.lastOption == scope.path.lastOption)
    )

  private def implicitScope(scope: Frame): Boolean = scope.tree match {
    case Some(_: Defn.Object) => holdsType(scope) || companion(scope)
    case Some(_: (Defn.Class | Defn.Trait | Defn.Enum | Defn.Given)) => true
    case _ => resolver.isPackage(scope) && scope.stats.exists(opaque)
  }

  private def holdsType(scope: Frame): Boolean =
    scope.stats.exists(stat =>
      stat.is[Defn.Type] || stat.is[Decl.Type] || MemberSyntax.templateName(stat).nonEmpty
    )

  private def companion(scope: Frame): Boolean =
    scope.parent.exists(owner =>
      owner.stats.exists {
        case d: Defn.Type => scope.path.lastOption.contains(d.name.value)
        case other        =>
          !other.is[Defn.Object] &&
          MemberSyntax.templateName(other).exists(name => scope.path.lastOption.contains(name))
      }
    )

  private def opaque(stat: Stat): Boolean = stat match {
    case d: Defn.Type => d.mods.exists(_.is[Mod.Opaque])
    case _            => false
  }

}

/** The extensions of a name a selection may meet: those known, or unknown ones. */
private[expansion] enum Extensions {
  case Unknown
  case Known(extensions: List[Extension])

  /** Whether some extension may answer. */
  def any: Boolean = this match {
    case Unknown           => true
    case Known(extensions) => extensions.nonEmpty
  }

}

/** An inline or contextual extension method, its group and the scope declaring the group. */
final private[expansion] case class Extension(
    group: Defn.ExtensionGroup,
    method: Defn.Def | Decl.Def,
    scope: Frame
) {
  def name: String = method.name.value
}
