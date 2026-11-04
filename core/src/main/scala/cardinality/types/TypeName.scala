package cardinality.types

import scala.meta.*

/** The simple name a reference uses for a type: the key a scope resolves, opaque so a name is never
  * confused with a path, signature, or prose; at runtime a name *is* its string.
  */
opaque type TypeName = String

object TypeName {
  private given Dialect = dialects.Scala3

  /** A printed name as the key it stands for. */
  def of(value: String): TypeName = value

  /** A dotted path (`dev.constructive.eo.data`) as the names it is made of. */
  def path(value: String): List[TypeName] = value.split('.').toList

  /** Every identifier a tree spells: the names a mention test or an import can mean. */
  def words(tree: Tree): Array[String] = tree.syntax.split("[^\\p{L}\\p{N}_$]+")

  extension (name: TypeName) {

    def value: String = name
  }

}
