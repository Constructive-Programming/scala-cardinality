package cardinality.analysis.model

/** The term members every value has, which the model does not count as observations (register D12):
  * readers that skip, inherit or exclude them share this one list.
  */
private[cardinality] object UniversalMembers {

  /** `Any`'s members. */
  val any: Set[String] =
    Set(
      "==",
      "!=",
      "equals",
      "hashCode",
      "toString",
      "##",
      "getClass",
      "isInstanceOf",
      "asInstanceOf"
    )

  /** `AnyRef`'s final members, which only a class instance has. */
  val anyRef: Set[String] = Set("eq", "ne", "synchronized")

  /** `java.lang.Object`'s other methods, which a class inherits (and may override, `clone`). */
  val objectMethods: Set[String] = Set("clone", "finalize", "notify", "notifyAll", "wait")

  /** `scala.Product`'s parameterless members: reflection over a value's fields (v1.md §2). */
  val productValues: Set[String] =
    Set("productArity", "productElementNames", "productIterator", "productPrefix")

  /** `scala.Product`'s members of one `Int` parameter, which read a field by its position. */
  val productReaders: Set[String] = Set("productElement", "productElementName")

}
