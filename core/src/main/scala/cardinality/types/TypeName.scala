package cardinality.types

/** The simple name a reference uses for a type: the key a scope resolves, opaque so a name is never
  * confused with a path, signature, or prose; at runtime a name *is* its string.
  */
opaque type TypeName = String

object TypeName {

  /** A printed name as the key it stands for. */
  def of(value: String): TypeName = value

  /** A dotted path (`dev.constructive.eo.data`) as the names it is made of. */
  def path(value: String): List[TypeName] = value.split('.').toList

  extension (name: TypeName) {

    def value: String = name
  }

}
