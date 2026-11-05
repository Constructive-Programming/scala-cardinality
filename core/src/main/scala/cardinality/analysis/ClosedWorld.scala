package cardinality.analysis

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.MemberSyntax
import cardinality.types.{Overrides, TypeApplications, TypeName}

/** The closed world (docs/plans/v1.md §2): the supplied sources and dependency declarations are the
  * whole program, so no subclass, override or implementation exists outside them. A concrete class
  * no supplied template extends is final, and so is a concrete member of a class or trait no
  * supplied template overrides. `finals` reads the inputs with those `final` modifiers written in,
  * and every reading of a final class or member applies them from there.
  *
  * Templates are matched by the simple name their declaration or parent clause spells, through
  * aliases (`type SA = S`), so two same-named templates share their subclasses and a parent this
  * reading cannot name may define anything: either only leaves a class or member open (a member
  * open only through such a parent is refused where a receiver reads it). A constructor parameter
  * is left as written (its modifiers are the signature's spelling), and so is a `val` of a literal
  * with no type, which `final` would type as the literal: a receiver's members ask
  * `Resolver.overriding` instead.
  */
final private[cardinality] class ClosedWorld(sources: List[Source]) {
  import ClosedWorld.*

  // A template: its name (none for an anonymous one), the parents it names, what it defines.
  final private class Node(
      val name: Option[String],
      val parents: List[String],
      val defines: Set[String]
  )

  private val nodes: List[Node] = sources.flatMap(_.collect {
    case c: Defn.EnumCase => node(Some(c.name.value), c.inits, Nil, Nil)
    case tree if TypeApplications.template(tree).nonEmpty =>
      val template = TypeApplications.template(tree).get
      node(
        MemberSyntax.templateName(tree),
        template.inits,
        template.body.stats,
        MemberSyntax.fieldParams(tree)
      )
  })

  private def node(
      name: Option[String],
      inits: List[Init],
      stats: List[Stat],
      params: List[Term.Param]
  ) =
    Node(
      name,
      inits.map(TypeApplications.parentName),
      (stats.flatMap(stat => MemberSyntax.defined(stat) ++ typeName(stat)) ++ params.map(
        _.name.value
      )).toSet
    )

  private def typeName(stat: Stat): Option[String] = stat match {
    case d: Defn.Type => Some(d.name.value)
    case d: Decl.Type => Some(d.name.value)
    case _            => None
  }

  private val named: Map[String, List[Node]] =
    nodes.flatMap(n => n.name.map(_ -> n)).groupMap(_._1)(_._2)

  // The templates naming each parent, and the aliases whose body mentions each word.
  private val children: Map[String, List[Node]] =
    nodes.flatMap(n => n.parents.distinct.map(_ -> n)).groupMap(_._1)(_._2)

  private val aliases: Map[String, List[String]] = sources
    .flatMap(_.collect {
      case d: Defn.Type => TypeName.words(d.body).distinct.map(_ -> d.name.value)
    })
    .flatten
    .groupMap(_._1)(_._2)

  private val extending = mutable.Map.empty[String, (Set[String], List[Node])]

  /** The names standing for `name` or a template extending it (aliases included), and the templates
    * extending it at any depth.
    */
  private def below(name: String): (Set[String], List[Node]) =
    extending.getOrElseUpdate(
      name, {
        val found = mutable.LinkedHashSet(name)
        val below = mutable.LinkedHashSet.empty[Node]
        val queue = mutable.Queue(name)
        while (queue.nonEmpty) {
          val next = queue.dequeue()
          val reached = aliases.getOrElse(next, Nil) ++ children.getOrElse(next, Nil).flatMap { n =>
            below += n
            n.name
          }
          reached.filter(found.add).foreach(queue.enqueue)
        }
        found.toSet -> below.toList
      }
    )

  /** Whether some supplied template extends a template of this name. */
  def extended(name: String): Boolean = below(name)._2.nonEmpty

  /** Whether some supplied template extends both named templates: an instance may be of both. */
  def mixes(left: String, right: String): Boolean = {
    val others = below(right)._2
    below(left)._2.exists(n => others.exists(_ eq n))
  }

  // The names a template of this name inherits from, at any depth.
  private def ancestors(name: String): Set[String] =
    inherited.getOrElseUpdate(
      name, {
        val found = mutable.LinkedHashSet.empty[String]
        val queue = mutable.Queue(name)
        while (queue.nonEmpty)
          named
            .getOrElse(queue.dequeue(), Nil)
            .flatMap(_.parents)
            .filter(found.add)
            .foreach(queue.enqueue)
        found.toSet
      }
    )

  private val inherited = mutable.Map.empty[String, Set[String]]

  /** How a template extending one of this name may override its member `member`: it defines the
    * name, or mixes in, from beyond the owner's ancestry, a parent that does — or one outside the
    * sources whose members are not known, which may.
    */
  def overriding(owner: String, member: String): Overriding =
    overrides.getOrElseUpdate(
      (owner, member), {
        val (found, extenders) = below(owner)
        reached(extenders, found ++ ancestors(owner), member)
      }
    )

  private val overrides = mutable.Map.empty[(String, String), Overriding]

  /** Whether some template extending one of this name may override its member `member`. */
  def overridden(owner: String, member: String): Boolean =
    overriding(owner, member) != Overriding.No

  // How the templates `from`, or the ancestry they name short of `stop`, define the member: one of
  // them does, or a parent no supplied template has may, unless the library lists its members.
  private def reached(from: List[Node], stop: Set[String], member: String): Overriding = {
    val seen = mutable.HashSet.empty[String] ++= stop
    val queue = mutable.Queue.empty[String]
    def visit(nodes: List[Node]): Overriding =
      if (nodes.exists(_.defines(member))) Overriding.Supplied
      else {
        nodes.foreach(_.parents.filter(seen.add).foreach(queue.enqueue))
        Overriding.No
      }
    var found = visit(from)
    while (queue.nonEmpty && found != Overriding.Supplied) {
      val parent = queue.dequeue()
      val here = named.get(parent) match {
        case Some(nodes) => visit(nodes)
        case None        =>
          library
            .get(parent)
            .fold(Overriding.Unknown)(members =>
              if (members(member)) Overriding.Supplied else Overriding.No
            )
      }
      found = strongest(List(found, here))
    }
    found
  }

  /** The `final` modifiers the closed world writes into a source, as offsets to insert them at. */
  def insertions(source: Source): List[(Int, String)] =
    source.collect {
      case c: Defn.Class
          if open(c.mods) && c.ctor.mods.isEmpty && !abstractMembers(c.templ) &&
            !extended(c.name.value) =>
        at(c, c.mods).toList
      case c: Defn.Class if !c.mods.exists(_.is[Mod.Final]) => members(c.name.value, c.templ)
      case t: Defn.Trait                                    => members(t.name.value, t.templ)
    }.flatten

  private def members(owner: String, template: Template): List[(Int, String)] =
    template.body.stats.flatMap {
      case d: Defn.Def if openMember(d.mods) && !overridden(owner, d.name.value) =>
        at(d, d.mods).toList
      case v: Defn.Val
          if openMember(v.mods) && !constant(v) &&
            MemberSyntax.defined(v).forall(!overridden(owner, _)) =>
        at(v, v.mods).toList
      case t: Defn.Type
          if openMember(t.mods) && !t.mods.exists(_.is[Mod.Opaque]) &&
            !overridden(owner, t.name.value) =>
        at(t, t.mods).toList
      case _ => Nil
    }

  // A literal `val` with no type: `final` would make its type the literal's.
  private def constant(v: Defn.Val): Boolean = v.decltpe.isEmpty && v.rhs.is[Lit]

  // Where `final` goes: after the annotations, before every other modifier and the keyword, so the
  // indentation a braceless body reads is kept.
  private def at(tree: Tree, mods: List[Mod]): Option[(Int, String)] =
    Some(
      mods
        .collect { case a: Mod.Annot => a.pos.end }
        .maxOption
        .fold(tree.pos.start -> "final ")(_ -> " final")
    )

  // A class with a non-public constructor is built only where its scope says, which no reading
  // enumerates, so it stays as written. A class declaring an abstract member is abstract in all but
  // name: whoever builds one implements it, as an abstract class's subclass does.
  private def abstractMembers(template: Template): Boolean =
    template.body.stats.exists(_.is[Decl])

}

private[cardinality] object ClosedWorld {

  /** How a member may be overridden: not at all, by a supplied definition, or only through a parent
    * outside the sources whose members are not known.
    */
  enum Overriding {
    case No, Unknown, Supplied
  }

  private def strongest(found: List[Overriding]): Overriding =
    found.maxByOption(_.ordinal).getOrElse(Overriding.No)

  /** The inputs as the closed world reads them: every class and member it closes written `final`.
    */
  def finals(inputs: List[MethodAnalysis.Input]): List[MethodAnalysis.Input] = {
    val world = new ClosedWorld(inputs.map(_.tree))
    inputs.map { input =>
      world.insertions(input.tree).distinct.sorted.reverse match {
        case Nil     => input
        case offsets =>
          val text = offsets.foldLeft(input.tree.pos.input.text) {
            case (text, (at, word)) =>
              text.substring(0, at) + word + text.substring(at)
          }
          dialects
            .Scala3(text)
            .parse[Source]
            .toOption
            .fold(input)(MethodAnalysis.Input(input.path, _))
      }
    }
  }

  // A modifier list a closing `final` may join: none already fixes, seals or abstracts it.
  private def open(mods: List[Mod]): Boolean =
    !mods.exists(m =>
      m.is[Mod.Final] || m.is[Mod.Sealed] || m.is[Mod.Open] || m.is[Mod.Abstract] ||
        m.is[Mod.Inline]
    )

  // A member's: a private one is not inherited, so nothing overrides it either.
  private def openMember(mods: List[Mod]): Boolean =
    open(mods) && !Overrides.unqualifiedPrivate(mods)

  // Parents outside the sources whose members the library fixes: markers with none, and the
  // members `Product` and `Equals` declare.
  private val library: Map[String, Set[String]] = {
    val equals = Set("canEqual", "equals")
    Map(
      "Any" -> Set.empty[String],
      "AnyRef" -> Set.empty[String],
      "Object" -> Set.empty[String],
      "Matchable" -> Set.empty[String],
      "Serializable" -> Set.empty[String],
      "AnyVal" -> Set.empty[String],
      "Equals" -> equals,
      "Product" -> (equals ++ Set(
        "productArity",
        "productElement",
        "productPrefix",
        "productIterator",
        "productElementName",
        "productElementNames"
      ))
    )
  }

}
