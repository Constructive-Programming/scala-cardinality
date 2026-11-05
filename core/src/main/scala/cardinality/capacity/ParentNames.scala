package cardinality.capacity

import scala.meta.*

import cardinality.types.Trees

// What the plain name in a subtype's `extends` clause denotes, read in the family's own file as
// Scala reads it: the innermost scope around the clause that declares, inherits or exports a type
// of that name binds it. Scala 3.9.0 rejects an import that would rebind a name another definition
// of the same file binds (an ambiguity), but an inherited member or an export rebinds it silently:
// `object O extends Base { case object Z extends P }` extends `Base`'s `P` when `Base` declares
// one. Whatever the file does not settle — a parent it does not declare, an export over a path it
// cannot follow, a scope no declaration of the name encloses — is unknown.
private[capacity] object ParentNames {

  enum Denotes {
    case Parent, Other, Unknown
  }

  /** What `name` in the clause of `subtype` (a template's owner, an enum case) denotes: `parent`,
    * another type, or nothing this file settles.
    */
  def denotes(subtype: Tree, parent: Tree, name: String): Denotes =
    Trees.innermost(subtype)(Reading(parent, name).lexical).getOrElse(Denotes.Unknown)

  private def stats(scope: Tree): Option[List[Stat]] = scope match {
    case p: Pkg    => Some(p.body.stats)
    case s: Source => Some(s.stats)
    case _         => None
  }

  // Several answers at one level: a parent's own `P` is refused (one per instance), a certain other
  // type is never this parent, and only then may an unknown member rebind it.
  private def strongest(found: List[Option[Denotes]]): Option[Denotes] =
    List(Denotes.Parent, Denotes.Other, Denotes.Unknown).find(found.flatten.contains)

  // One name read for one parent.
  final private case class Reading(parent: Tree, name: String) {

    // What one scope around the clause binds the name to: a template's members; a package's or a
    // file's declarations.
    def lexical(scope: Tree): Option[Denotes] = scope match {
      case t: Template => members(t, Set.empty)
      case other       => stats(other).flatMap(declared)
    }

    // The type members a template has under the name: declared in its body, exported by it, or
    // inherited from a parent — `seen` guards a cycle of parents.
    def members(t: Template, seen: Set[Tree]): Option[Denotes] =
      declared(t.body.stats).orElse(exported(t)).orElse(inherited(t, seen))

    private def declared(stats: List[Stat]): Option[Denotes] =
      stats
        .collectFirst(Trees.typeNamed(name))
        .map(d => if (d eq parent) Denotes.Parent else Denotes.Other)
        .orElse(stats.collectFirst { case d: Decl.Type if d.name.value == name => Denotes.Other })

      // A template's export clauses may bind the name to a member of a path, which this file does not

    // settle.
    private def exported(t: Template): Option[Denotes] =
      Option.when(t.body.stats.exists(_.is[Export]))(Denotes.Unknown)

    // What the template's parents declare under the name: a parent this file declares says through
    // its own members; any other parent's members are unknown.
    private def inherited(t: Template, seen: Set[Tree]): Option[Denotes] =
      strongest(t.inits.map { init =>
        classNamed(t.parent.getOrElse(t), init.tpe).filterNot(seen) match {
          case Some(c) => members(c.templ, seen + c)
          case None    => Some(Denotes.Unknown)
        }
      })

  }

  // The class or trait a parent type names, read from the scope around its owner: the innermost
  // declaration of the file with that name, when it is one.
  private def classNamed(owner: Tree, tpe: Type): Option[Defn.Class | Defn.Trait] = {
    val callee = tpe match {
      case Type.Name(n) => Some(n)
      case _            => None
    }
    callee
      .flatMap(n => Trees.innermost(owner)(stats(_).flatMap(_.collectFirst(Trees.typeNamed(n)))))
      .collect {
        case c: Defn.Class => c
        case t: Defn.Trait => t
      }
  }

}
