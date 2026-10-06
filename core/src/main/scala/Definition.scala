package cardinality

/** One named definition a source introduces, as the report sees it: a name qualified by package and
  * enclosing definitions (`data.Affine.Miss`), the cardinality a reference to it holds (no size of
  * its own for abstractions), and the reasons the calculator could not bound it.
  */
final case class Definition(
    name: String,
    kind: Definition.Kind,
    params: List[String],
    size: Option[Size],
    unbound: List[Definition.Unbound],
    line: Int,
)

object Definition {

  /** Why a row has no number: a name the sources do not define, the definition's own type
    * parameters (`Parameter`, or `HigherKinded` for `F[_]`), an open abstraction any subtype could
    * extend, or syntax the calculator does not model yet.
    */
  enum Unbound {

    /** The size is a function of the instantiation; no single number exists. */
    case Parameter(name: String)

    /** As above, for a parameter that takes parameters of its own (`F[_]`). */
    case HigherKinded(name: String, arity: Int)

    /** Any subtype anywhere may add values, so the reference has no bound at all. */
    case Open(name: String)

    case Unknown(name: String)

    /** Syntax the calculator does not model yet: a match type, a type lambda, a refinement. */
    case Syntax(what: String)

    /** How the kind reads in a row: a name, or what kind of syntax it is. */
    def render: String = this match {
      case Parameter(name)           => name
      case HigherKinded(name, arity) => s"$name[${List.fill(arity)("_").mkString(", ")}]"
      case Open(name)                => name
      case Unknown(name)             => name
      case Syntax(what)              => what
    }

  }

  /** A row whose only reasons are its own type parameters: every instantiation has its own. */
  def instantiationDependent(definition: Definition): Boolean =
    definition.unbound.nonEmpty && definition.unbound.forall {
      case Unbound.Parameter(_) | Unbound.HigherKinded(_, _) => true
      case _                                                 => false
    }

  /** A row an open abstraction unbounds — bounding it would be a claim about unseen code. */
  def open(definition: Definition): Boolean =
    definition.unbound.exists(_.isInstanceOf[Unbound.Open])

  /** What kind of definition this is; the kind decides how its size reads. */
  enum Kind {

    case Class

    /** An abstract class or trait: no inhabitants of its own. */
    case Abstract

    case Enum

    case Object

    /** A top-level value: one inhabitant; its declared type is not measured. */
    case Value

    case Alias

    /** An alias whose body is a type lambda: applying it is where a size would come from, so a row
      * of this kind has no size to report — a shape, not a gap.
      */
    case Constructor

    /** An opaque type: one value outside the scope that defines it. */
    case Opaque
  }

  /** How a definition reads in a report: `Getter[S, A]`, or `data.Affine` for a companion. */
  def signature(definition: Definition): String =
    definition.name + (definition.params match {
      case Nil  => ""
      case list => list.mkString("[", ", ", "]")
    })

}
