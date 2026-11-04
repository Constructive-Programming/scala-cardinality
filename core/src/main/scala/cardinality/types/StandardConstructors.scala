package cardinality.types

/** The standard library's type constructors the estimate models, by bare name: the standard
  * library's only where no source binds the name.
  */
private[cardinality] object StandardConstructors {

  // Data constructors: over `u`-values, a value over any supertype's values too (`Set[u]`).
  private val data: Set[String] = Set(
    "Option",
    "Either",
    "List",
    "Vector",
    "Seq",
    "IndexedSeq",
    "LazyList",
    "Stream",
    "Array",
    "Set",
    "Map",
  )

  /** Every constructor the algebra models: the data constructors and `PartialFunction`. */
  val modelled: Set[String] = data + "PartialFunction"

  /** The slots holding arguments as data: a data constructor's all, a partial function's codomain.
    */
  def slots(callee: String, arity: Int): Option[List[Boolean]] =
    if (data(callee)) Some(List.fill(arity)(true))
    else Option.when(callee == "PartialFunction")(List(false, true))

}
