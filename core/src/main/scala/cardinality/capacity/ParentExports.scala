package cardinality.capacity

import scala.meta.*

import cardinality.analysis.resolution.InheritedTypeMembers.MemberKind
import cardinality.types.TypeName

// A parent's export clauses define alias members it passes on like its declarations (Scala 3
// reference, export clauses: "type members are aliased by type definitions, and term members are
// aliased by method definitions"). `Inheritance` does not follow them: a name one may define is
// refused, as the method side's `InheritedTypeMembers` refuses it (`exports of P`). Whether one may
// is read in the parent's body, as the body's own reads are: a selector that names a member of that
// kind in its namespace, or a namespace that may hold one; a wildcard whose namespace binds the name
// or may (`StoredScopesSpec`'s `inheritedExports`).
private[capacity] object ParentExports {

  /** Whether the export clauses of `members`' body may define `name` in the namespace `kind` reads.
    */
  def define(members: Members, name: TypeName, kind: MemberKind): Boolean = {
    val stats = Walk.template(members.defn)
    stats.exists(_.is[Export]) && {
      val exports = Imports.exports(stats)
      val body = members.inside.within(Layer(imports = exports))
      selected(body, exports, name, kind) || wildcard(body, exports, name, kind)
    }
  }

  // A selector of the name: a type one defines a type unless its namespace has only a term of that
  // name; a term one, unless its supplied object has no such term.
  private def selected(body: Scope, exports: Imports, name: TypeName, kind: MemberKind): Boolean =
    exports.named.contains(name) && (kind match {
      case MemberKind.Type =>
        body.typeClause(name).exists(s => namespace(body, s).selected(s).nonEmpty)
      case MemberKind.Term =>
        body.clause(name).exists { s =>
          namespace(body, s) match {
            case obj: Namespaces.Target.Obj => obj.select(s.member) != Namespaces.Target.Absent
            case _                          => true
          }
        }
    })

  private def namespace(body: Scope, selector: Selector): Namespaces.Target =
    Namespaces.target(body.withClauses(selector.prior), selector.path)

  private def wildcard(body: Scope, exports: Imports, name: TypeName, kind: MemberKind): Boolean =
    body.wildcards.indices.exists { i =>
      val w = body.wildcards(i)
      exports.wildcards.exists(_.same(w)) && !w.excluded(name) && {
        val namespace = Namespaces.target(body.earlier(i), w.path)
        kind match {
          case MemberKind.Type => !w.givens && namespace.member(name, w).nonEmpty
          case MemberKind.Term => namespace.bindsTerm(name, w)
        }
      }
    }

}
