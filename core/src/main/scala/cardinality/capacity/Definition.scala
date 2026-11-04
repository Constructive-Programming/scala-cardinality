package cardinality.capacity

import cardinality.types.TypeName

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

  /** Why a row has no number. */
  enum Unbound {

    /** The size is a function of the instantiation; no single number exists. */
    case Parameter(name: String)

    /** As above, for a parameter that takes parameters of its own (`F[_]`). */
    case HigherKinded(name: String, arity: Int)

    /** An abstract type member of an enclosing class or trait: inside the template it is `this.M`,
      * which each instance chooses, so the size is a function of that choice as of a parameter.
      */
    case Member(name: String)

    /** Any subtype anywhere may add values, so the reference has no bound at all. */
    case Open(name: String)

    case Unknown(name: String)

    /** A name a clause binds that the estimate does not follow, and which clause (`provenance`). */
    case Imported(name: String, provenance: String)

    /** Syntax the calculator does not model yet: a match type, a type lambda, a refinement. */
    case Syntax(what: String)

    /** A declaration of the sources the estimate read but refuses to count whole (a family it does
      * not sum, a recursion it does not solve): the method side refuses the positions it reads.
      */
    case Refusal(what: String)

    /** How the kind reads in a row: a name, or what kind of syntax it is. */
    def render: String = this match {
      case Parameter(name)            => name
      case HigherKinded(name, arity)  => s"$name[${List.fill(arity)("_").mkString(", ")}]"
      case Member(name)               => s"member $name"
      case Open(name)                 => name
      case Unknown(name)              => name
      case Imported(name, provenance) => s"$name ($provenance)"
      case Syntax(what)               => what
      case Refusal(what)              => what
    }

    /** What the reason is about, without the word `render` gives its kind: the name or syntax an
      * artifact puts after the reason's `tag` (`member:M`, not `member:member M`).
      */
    def subject: String = this match {
      case Member(name) => name
      case other        => other.render
    }

    /** The kind as a machine-readable artifact tags a reason (`unknown:Schema`, `param:F[_]`). */
    def tag: String = this match {
      case _: (Parameter | HigherKinded) => "param"
      case _: Member                     => "member"
      case _: Open                       => "open"
      case _: Unknown                    => "unknown"
      case _: Imported                   => "imported"
      case _: Syntax                     => "syntax"
      case _: Refusal                    => "refusal"
    }

    /** The same reason for a name a clause renamed (`import a.{N as M}` binds `M`). */
    def renamed(name: TypeName): Unbound = this match {
      case Imported(_, provenance) => Imported(name.value, provenance)
      case other                   => other
    }

    /** Which class of gap the reason is. */
    def group: Gap = this match {
      case _: (Parameter | HigherKinded | Member)     => Gap.Instantiation
      case _: Open                                    => Gap.Open
      case _: (Unknown | Imported | Syntax | Refusal) => Gap.Unresolved
    }

    /** A reason whose answer depends on the instantiation: no single number exists, whatever the
      * caller supplies.
      */
    def instantiationDependent: Boolean = this match {
      case Parameter(_) | HigherKinded(_, _) => true
      case _                                 => false
    }

    /** An unsealed abstraction any subtype anywhere may extend: the reference has no bound. */
    def opensAbstraction: Boolean = this match {
      case Open(_) => true
      case _       => false
    }

  }

  /** The classes of reasons, in the order they decide a row: unresolved hides nothing behind an
    * open abstraction, which in turn leaves no number for an instantiation to give.
    */
  enum Gap(val heading: String => String) {
    case Unresolved extends Gap(names => s"unresolved: $names")
    case Open extends Gap(names => s"open to implementations ($names)")
    case Instantiation extends Gap(names => s"depends on its instantiation ($names)")
  }

  /** The class that decides a row with reasons: the first `Gap` any of them belongs to. */
  def gap(definition: Definition): Option[Gap] =
    definition.unbound.map(_.group).minByOption(_.ordinal)

  /** Every class of the row's reasons, in `Gap` order, each heading the reasons of its class. */
  def reasons(definition: Definition): String =
    Gap.values.toList
      .flatMap { gap =>
        val names = definition.unbound.filter(_.group == gap).map(_.render)
        Option.when(names.nonEmpty)(gap.heading(names.mkString(", ")))
      }
      .mkString("; ")

  /** What kind of definition this is; the kind decides how its size reads. */
  enum Kind {

    case Class

    /** An abstract class or trait: no inhabitants of its own. */
    case Abstract

    case Enum

    case Object

    /** A top-level value: `min(1, |T|)` of its declared type, 1 with none; never its initializer.
      */
    case Value

    case Alias

    /** An alias whose body is a type lambda: applying it is where a size would come from, so a row
      * of this kind has no size to report — a shape, not a gap.
      */
    case Constructor

    /** An opaque type: its row reads the representation, which only its own scope sees. */
    case Opaque
  }

  /** How a definition reads in a report: `Getter[S, A]`, or `data.Affine` for a companion. */
  def signature(definition: Definition): String =
    definition.name + (definition.params match {
      case Nil  => ""
      case list => list.mkString("[", ", ", "]")
    })

}
