package cardinality

/** One named definition a source introduces, as the report sees it.
  *
  * `name` is qualified by the package and the enclosing definitions (`data.Affine.Miss`), and
  * `size` is the cardinality of the type itself: how many values a reference to it can hold. An
  * abstract class or trait has no cardinality of its own — the inhabitants belong to the concrete
  * cases that extend it — so its size is `None`.
  */
final case class Definition(
    name: String,
    kind: Definition.Kind,
    params: List[String],
    size: Option[Size],
    unresolved: List[String],
    line: Int,
)

object Definition {

  /** What kind of definition this is; the kind decides how its size reads. */
  enum Kind {

    /** A concrete class: its constructor parameters multiply into its size. */
    case Class

    /** An abstract class or trait: no inhabitants of its own. */
    case Abstract

    /** An enum: the sum over its cases. */
    case Enum

    /** A module (`object`, `case object`): a single instance. */
    case Object

    /** A value at the top level of a source: a single value. Its declared type is not measured —
      * the value itself is one inhabitant.
      */
    case Value

    /** A type alias: names another type, adds no inhabitants of its own. */
    case Alias

    /** An opaque type: one value outside the scope that defines it. */
    case Opaque
  }

  /** How a definition reads in a report: `Getter[S, A]` for a class, `data.Affine` for the
    * companion of a trait, `Flag` for an alias.
    */
  def signature(definition: Definition): String =
    definition.name + (definition.params match {
      case Nil  => ""
      case list => list.mkString("[", ", ", "]")
    })

}
