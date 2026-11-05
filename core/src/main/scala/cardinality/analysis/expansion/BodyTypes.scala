package cardinality.analysis.expansion

import scala.meta.*

/** The types an inline body writes, read for what an expansion decides at its call site (register
  * D27): `scala.compiletime`'s operations and quoted types. `paths` reads a type's qualifier as the
  * walk reads a term path.
  */
final private[expansion] class BodyTypes(
    terms: BodyTerms,
    paths: (Term, Scope) => Option[Meaning]
) {

  /** The first reason a type the body writes is read at the call site, if any. */
  def types(tpe: Type, scope: Scope): Option[String] = tpe match {
    case Type.Name(name) if !scope.variables(name) =>
      compileTime(terms.named(name, scope.frame, tpe.pos.start))
    case s: Type.Select => paths(s.qual, scope).flatMap(compileTime)
    case other          =>
      other.children.iterator
        .flatMap {
          case t: Type => types(t, scope)
          case _       => None
        }
        .nextOption()
  }

  private def compileTime(meaning: Meaning): Option[String] = meaning match {
    case found: Meaning.CompileTime => found.refusal
    case _                          => None
  }

}
