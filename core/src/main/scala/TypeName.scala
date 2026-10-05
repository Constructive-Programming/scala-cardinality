package cardinality

/** The simple name a reference uses for a type: the key a scope resolves, the name a definition is
  * known by, and what a type parameter is called.
  *
  * Opaque, the way this codebase treats a value it does not want confused with its own text: a name
  * *is* its string at runtime, and the compiler still keeps a name apart from a package path, a
  * printed signature, or a report's prose.
  */
opaque type TypeName = String

object TypeName {

  /** A printed name (`Type.Name`, a definition's identifier) as the key it stands for. */
  def of(value: String): TypeName = value

  /** A dotted path (`dev.constructive.eo.data`) as the names it is made of. */
  def path(value: String): List[TypeName] = value.split('.').toList

  extension (name: TypeName) {

    /** The name as the text it is: a scope's key, a definition's label, a report's row. */
    def value: String = name
  }

}
