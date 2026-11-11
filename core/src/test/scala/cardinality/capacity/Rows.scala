package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count

/** How the name-resolution specs read a source set: each stored-value row as a reader sees it, and
  * one constructor's method-side count.
  */
private[capacity] object Rows {

  /** A row's number, `—` for an abstraction, or the class of its `?` with the reasons it names. */
  def verdict(d: Definition): String =
    if (d.unbound.isEmpty) d.size.fold("—")(_.render)
    else {
      val kind = Definition.gap(d) match {
        case Some(Definition.Gap.Unresolved) => "unresolved"
        case Some(Definition.Gap.Open)       => "open"
        case _                               => "instantiation"
      }
      d.unbound.map(_.render).mkString(s"$kind[", ", ", "]")
    }

  private def table(definitions: List[Definition]): Map[(String, Definition.Kind), String] =
    definitions.map(d => (d.name, d.kind) -> verdict(d)).toMap

  /** One source's rows, read alone: compiled first, as every fixture is (`Compiled`). */
  def rows(code: String): Map[(String, Definition.Kind), String] =
    table(Counter.definitions(cardinality.Compiled.source(code)))

  /** The rows of a source set laid out as files, read as the report reads a compiled module: the
    * files on disk and their classes. Support the set withholds (`Compiled.withheld`,
    * `Compiled.withheldFile`) is compiled against and not written, so this reading is not supplied
    * it either.
    */
  def rowsOf(files: (String, String)*): Map[(String, Definition.Kind), String] =
    table(cardinality.Compiled.report(files*).definitions)

  /** The method side's count for the signature `name` of a source set. */
  def constructor(name: String, files: (String, String)*): Count =
    cardinality.Compiled
      .analyze(cardinality.Compiled.inputs(files*))
      .find(_.name == name)
      .map(_.count)
      .getOrElse(throw new AssertionError(s"no signature $name"))

}
