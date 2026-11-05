package cardinality.types

import scala.annotation.tailrec
import scala.meta.*

import cardinality.analysis.model.MemberSyntax

/** Which members a subclass may override, read from syntax alone (scalac E164). */
private[cardinality] object Overrides {

  /** Whether a subclass may override a member. An object, class, trait or enum is no member a
    * subclass replaces, and Scala 3 lets nothing override a given or a `var`; a final, inline or
    * unqualified-private member is fixed as well. A declaration is overridable: it is implemented —
    * a deferred given (`given g: T = deferred`) too, which each class extending its trait supplies.
    */
  def overridable(member: Tree): Boolean = member match {
    case _: (Defn.Object | Defn.Class | Defn.Trait | Defn.Enum) => false
    case g: Defn.GivenAlias                                     => deferred(g)
    case _: (Defn.Given | Defn.Var)                             => false
    case p: Term.Param    => !p.mods.exists(m => m.is[Mod.VarParam] || fixing(m))
    case d: Stat.WithMods => !d.mods.exists(fixing)
    case _                => true
  }

  /** A given alias whose body is spelled as `scala.compiletime.deferred` (bare, or selected through
    * `compiletime` or `scala.compiletime`): an abstract given, unless the name means another value
    * where it is read — a reading that binds the member resolves it (`ReceiverMembers`).
    */
  def deferred(g: Defn.GivenAlias): Boolean = g.body match {
    case Term.Name("deferred")                    => true
    case Term.Select(path, Term.Name("deferred")) => compiletime(path.syntax)
    case _                                        => false
  }

  private val compiletime = Set("compiletime", "scala.compiletime", "_root_.scala.compiletime")

  /** Whether modifiers make a member `private` or `private[this]`. */
  def unqualifiedPrivate(mods: List[Mod]): Boolean = mods.exists {
    case Mod.Private(within) => within.is[Name.Anonymous] || within.is[Term.This]
    case _                   => false
  }

  private def fixing(mod: Mod): Boolean =
    mod.is[Mod.Final] || mod.is[Mod.Inline] || unqualifiedPrivate(List(mod))

  /** The constructor parameters of a class or trait a subclass inherits as members: its fields
    * (`MemberSyntax.fieldParams`) but a `private` one.
    */
  def inheritedFields(level: Tree): List[Term.Param] =
    MemberSyntax.fieldParams(level).filterNot(p => unqualifiedPrivate(p.mods))

  /** What a parent clause denotes, read where it is written: a supplied class or trait (its level,
    * whose body and clause are read in turn), a parent outside the sources with the members the
    * model lists a subclass may override (none for a marker), or one this reading does not read.
    */
  enum Parent[+L] {
    case Source(level: L)
    case Library(overridable: Set[String])
    case Unread
  }

  /** A parent clause of a leaf's ancestry: the level writing it, its spelling, what it denotes. */
  final case class Clause[L](owner: L, name: String, parent: Parent[L])

  /** How a side reads a leaf's ancestry: a level's body and fields, the parents its clause names
    * (with their spellings), and an identity that stops a level read twice.
    */
  trait Parents[L] {
    def body(level: L): List[Stat]
    def fields(level: L): List[Term.Param]
    def parents(level: L): List[(String, Parent[L])]
    def key(level: L): String
  }

  /** The parent clauses of a level and of each source parent it reaches, breadth first. */
  def ancestry[L](level: L, parents: Parents[L]): List[Clause[L]] = {
    def clauses(of: L) = parents.parents(of).map((name, parent) => Clause(of, name, parent))
    @tailrec
    def walk(pending: List[Clause[L]], seen: Set[String], read: List[Clause[L]]): List[Clause[L]] =
      pending match {
        case Nil                                                => read.reverse
        case (clause @ Clause(_, _, Parent.Source(at))) :: rest =>
          val more = if (seen(parents.key(at))) Nil else clauses(at)
          walk(rest ++ more, seen + parents.key(at), clause :: read)
        case clause :: rest => walk(rest, seen, clause :: read)
      }
    walk(clauses(level), Set(parents.key(level)), Nil)
  }

  /** Why a case class leaf of a sealed family (at `level`, with its `ancestry`) is not closed, if
    * it is not. A case class no `final` closes may be extended in any file, and a subclass may
    * override a member it has — its own, a source parent's (a body member, an extension, a `val`
    * parameter), or one the model lists of a library parent: its values then differ beyond its
    * fields. A parameterless member fixed anywhere in the ancestry, or a field of the leaf, fixes
    * its name for every subclass; a member with parameters fixes nothing (what it overrides depends
    * on its parameter types). So every parent is read first: one not read is the reason. Members
    * restating `Any`'s, the ones the compiler synthesizes for a case class and a field's `val` (an
    * override is another value of its type) are outside the model.
    */
  def openLeaf[L](
      leaf: Defn.Class,
      level: L,
      ancestry: List[Clause[L]],
      parents: Parents[L]
  ): Option[String] =
    if (leaf.mods.exists(_.is[Mod.Final])) None
    else
      ancestry
        .collectFirst { case Clause(_, name, Parent.Unread) => s"parent not read: $name" }
        .orElse {
          val sources = ancestry.collect { case Clause(_, _, Parent.Source(at)) => at }
          val fixed = inheritedFields(leaf).map(_.name.value).toSet ++
            (level :: sources).flatMap(members(_, parents)).filter(fixes).flatMap(paramless)
          observed(leaf.templ.body.stats, fixed)
            .map(m => s"overridable member $m")
            .orElse(inherited(ancestry, fixed, parents))
        }

  // The first overridable member of a source parent, breadth first. A library parent's listed
  // members are not read.
  private def inherited[L](
      ancestry: List[Clause[L]],
      fixed: Set[String],
      parents: Parents[L]
  ): Option[String] =
    ancestry.iterator
      .map {
        case Clause(_, name, Parent.Source(at)) =>
          observed(members(at, parents), fixed).map(m => s"$name.$m")
        case _ => None
      }
      .collectFirst { case Some(member) => s"overridable member $member" }

  private def members[L](level: L, parents: Parents[L]): List[Tree] =
    parents.body(level) ++ parents.fields(level)

  // A member no subclass may override fixes its name, unless private: that one is not inherited.
  private def fixes(member: Tree): Boolean =
    !overridable(member) && (member match {
      case m: Stat.WithMods => !unqualifiedPrivate(m.mods)
      case _                => true
    })

  // The first concrete member a subclass may override, but a parameterless fixed one.
  private def observed(members: List[Tree], fixed: Set[String]): Option[String] =
    members.filter(overridable).flatMap(concrete).collectFirst {
      case (name, parameterless) if !(parameterless && fixed(name)) => name
    }

  // A concrete member's names, each with whether it is parameterless; none restating `Any`'s.
  private def concrete(member: Tree): List[(String, Boolean)] = member match {
    case d: Defn.Def if !MemberSyntax.universal(d)(_ => false) =>
      List(d.name.value -> d.paramClauseGroups.isEmpty)
    case v: Defn.Val => TypeApplications.boundNames(v.pats).map(_ -> true)
    case _           => Nil
  }

  // A parameterless member's names: such members of one name in a linearization are one member.
  private def paramless(member: Tree): List[String] = member match {
    case d: Defn.Def if d.paramClauseGroups.isEmpty => List(d.name.value)
    case _                                          => Nil
  }

}
