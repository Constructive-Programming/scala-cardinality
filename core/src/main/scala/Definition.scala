package cardinality

/** One named definition a source introduces, as the report sees it.
  *
  * `name` is qualified by the package and the enclosing definitions (`data.Affine.Miss`), and
  * `size` is the cardinality of the type itself: how many values a reference to it can hold. An
  * abstract class or trait has no cardinality of its own — the inhabitants belong to the concrete
  * cases that extend it — so its size is `None`.
  *
  * `unbound` is what stopped the calculator from bounding the row, by kind: a name the sources do
  * not define, the definition's own type parameters, an open abstraction, or syntax the calculator
  * does not model. The kind is what lets a report tell a row that *has* no number — a template,
  * whose size is a function of its instantiation — from one the calculator cannot read yet.
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

  /** Why a row has no number: the kinds of thing that stopped the count at that definition. */
  enum Unbound {

    /** A type parameter the definition declares, or an enclosing definition does: the size is a
      * function of the instantiation, and no single number exists for the template.
      */
    case Parameter(name: String)

    /** The same for a parameter that takes parameters of its own (`F[_]`, `F[_, _]`): an applied
      * `F[A]` is a value space only the instantiation decides.
      */
    case HigherKinded(name: String, arity: Int)

    /** An unsealed abstraction the sources define: any subtype anywhere may add values, so the
      * reference has no bound at all.
      */
    case Open(name: String)

    /** A name the supplied sources do not define. */
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

  /** A row whose only reasons are its own type parameters has no number to show: every
    * instantiation has its own, and a caller supplies it.
    */
  def instantiationDependent(definition: Definition): Boolean =
    definition.unbound.nonEmpty && definition.unbound.forall {
      case Unbound.Parameter(_) | Unbound.HigherKinded(_, _) => true
      case _                                                 => false
    }

  /** A row an open abstraction unbounds: the number would be a claim about code the sources do not
    * contain.
    */
  def open(definition: Definition): Boolean =
    definition.unbound.exists(_.isInstanceOf[Unbound.Open])

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
