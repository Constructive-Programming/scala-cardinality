package cardinality.capacity

import scala.meta.*

import cardinality.types.Trees.ClassDefn
import cardinality.types.{Trees, TypeName}

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
    case t: Template   => Some(t.body.stats)
    case p: Pkg        => Some(p.body.stats)
    case s: Source     => Some(s.stats)
    case b: Term.Block => Some(b.stats)
    case _             => None
  }

  // Several answers at one level: a parent's own `P` is refused (one per instance), a certain other
  // type is never this parent, and only then may an unknown member rebind it.
  private def strongest(found: List[Option[Denotes]]): Option[Denotes] =
    List(Denotes.Parent, Denotes.Other, Denotes.Unknown).find(found.flatten.contains)

  // One name read for one parent.
  final private case class Reading(parent: Tree, name: String) {

    // What one scope around the clause binds the name to: a template's owner's type parameters,
    // its members; a package's, a file's or a block's declarations; a method's type parameters.
    def lexical(scope: Tree): Option[Denotes] = scope match {
      case t: Template => typeParameter(t.parent).orElse(members(t, Set.empty))
      case d: Defn.Def => typeParameter(Some(d))
      case other       => stats(other).flatMap(declared)
    }

    private def typeParameter(owner: Option[Tree]): Option[Denotes] =
      owner
        .collect {
          case d: ClassDefn => d.tparamClause.values
          case d: Defn.Def  => d.paramClauseGroups.flatMap(_.tparamClause.values)
        }
        .filter(_.exists(_.name.value == name))
        .map(_ => Denotes.Other)

    // The type members a template has under the name: declared in its body, exported by it, or
    // inherited from a parent — `seen` guards a cycle of parents.
    def members(t: Template, seen: Set[Tree]): Option[Denotes] =
      declared(t.body.stats).orElse(exported(t, seen)).orElse(inherited(t, seen))

    private def declared(stats: List[Stat]): Option[Denotes] =
      stats
        .collectFirst(Trees.typeNamed(name))
        .map(d => if (d eq parent) Denotes.Parent else Denotes.Other)
        .orElse(stats.collectFirst { case d: Decl.Type if d.name.value == name => Denotes.Other })

    // An export of the name, or a wildcard export, binds it to the member of the path it reads: an
    // object of this file whose members say, or nothing the file settles.
    private def exported(t: Template, seen: Set[Tree]): Option[Denotes] =
      strongest(
        t.body.stats
          .collect { case e: Export => e.importers.map(e -> _) }
          .flatten
          .flatMap { (e, importer) =>
            importer.importees
              .collect {
                case Importee.Name(n) if n.value == name                       => Some(name)
                case Importee.Rename(from, to) if to.value == name             => Some(from.value)
                case _: Importee.Wildcard if !importer.importees.exists(hides) => None
              }
              .map(member => through(importer.ref, e, member, seen))
          }
      )

    private def hides(importee: Importee): Boolean = importee match {
      case Importee.Unimport(n)     => n.value == name
      case Importee.Rename(from, _) => from.value == name
      case _                        => false
    }

    // The member a path's object has under `member` (the name itself, for a wildcard): what the
    // object's members say. A wildcard over an object with no such member binds nothing; any other
    // path is unknown.
    private def through(
        path: Term.Ref,
        from: Tree,
        member: Option[String],
        seen: Set[Tree]
    ): Option[Denotes] =
      path match {
        case Term.Name(obj) =>
          objectNamed(from, obj).filterNot(seen) match {
            case Some(o) =>
              val found = Reading(parent, member.getOrElse(name)).members(o.templ, seen + o)
              if (member.isEmpty) found else found.orElse(Some(Denotes.Unknown))
            case None => Some(Denotes.Unknown)
          }
        case _ => Some(Denotes.Unknown)
      }

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

  // The object a plain term name means where it is read: the innermost scope of the file binding a
  // term of that name — a member, a value, a method, a given or a parameter — when that is an
  // object.
  private def objectNamed(from: Tree, name: String): Option[Defn.Object] =
    Trees
      .innermost(from)(scope =>
        terms(scope).get(TypeName.of(name)).map {
          case _: Bound.Module =>
            stats(scope).flatMap(_.collectFirst {
              case o: Defn.Object if o.name.value == name => o
            })
          case _ => None
        }
      )
      .flatten

  // The term names a scope binds: its statements', and a class's or method's parameters.
  private def terms(scope: Tree): Map[TypeName, Bound] = {
    val parameters = scope match {
      case t: Template => t.parent.collect { case d: Defn => Walk.parameters(d) }.getOrElse(Nil)
      case d: Defn.Def => Walk.parameters(d)
      case _           => Nil
    }
    parameters.toMap ++ stats(scope).fold(Map.empty[TypeName, Bound])(Walk.termsOf(_))
  }

  // The class or trait a parent type names, read from the scope around its owner: the innermost
  // declaration of the file with that name, when it is one.
  private def classNamed(owner: Tree, tpe: Type): Option[Defn.Class | Defn.Trait] = {
    val callee = tpe match {
      case Type.Apply.After_4_6_0(Type.Name(n), _) => Some(n)
      case Type.Name(n)                            => Some(n)
      case _                                       => None
    }
    callee
      .flatMap(n => Trees.innermost(owner)(stats(_).flatMap(_.collectFirst(Trees.typeNamed(n)))))
      .collect {
        case c: Defn.Class => c
        case t: Defn.Trait => t
      }
  }

}
