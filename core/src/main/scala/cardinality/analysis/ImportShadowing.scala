package cardinality.analysis

import scala.meta.*

import cardinality.types.{Spelling, TypeName}

/** An import can put a *different* type behind a name the resolution reads as a builtin (`Option`,
  * `Boolean`). Lookup binds what a clause with a pure path brings, and export clauses the same way;
  * what it cannot bind is a clause whose prefix is no path (`import this.x.N`): every name its
  * selectors and prefix spell.
  */
private[cardinality] object ImportShadowing {

  /** Why a clause may rebind a builtin name, if it may. */
  def reason(clause: Import): Option[String] = {
    val unbound = clause.importers
      .filter(importer => SourceIndex.importPath(importer.ref).isEmpty)
      .flatMap(importer =>
        TypeName.words(importer.ref) ++ importer.importees.flatMap(SourceIndex.boundName)
      )
      .toSet
    // The root imports' names: the shape vocabulary, rigid constructors' keys, the top types.
    Option.when(BuiltinTypes.rootNames.exists(unbound.contains))(
      s"imported environment not resolved: ${Spelling.of(clause)}"
    )
  }

}
