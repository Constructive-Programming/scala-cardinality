package cardinality.analysis.model

import scala.meta.Stat

import cardinality.analysis.MethodAnalysis.{Frame, Resolved}

/** A stable path whose type members a reading follows (members M1): `this` (`isSelf`), a parameter,
  * a declared `val`, or a singleton alias of one. `key` is its identity: two paths are two packages
  * of one existential, so their abstract members never meet. `template` is the class its type
  * names, as the path instantiates it; `refinement` the members a refinement pins, each with the
  * scope it was written in; `fixed` why no supplier picks its members, when its definition does.
  */
final private[cardinality] case class MemberPath(
    key: String,
    label: String,
    template: Option[(Frame, Map[String, Resolved])],
    refinement: List[MemberPath.Refined] = Nil,
    fixed: Option[String] = None,
    isSelf: Boolean = false
) {

  /** The atom of an abstract member every level of the path leaves open. */
  def atom(name: String): String = MemberPath.Atom(key, spell(name))

  def spell(name: String): String = s"$label#$name"

}

private[cardinality] object MemberPath {

  /** A refined member and the scope its refinement was written in. */
  final case class Refined(stat: Stat, scope: ResolutionContext)

  /** `this` of a template, read with the template's own binders. */
  def self(template: Frame, binders: Map[String, Resolved]): MemberPath =
    MemberPath(
      selfKey(template),
      s"${template.path.lastOption.getOrElse("")}.this",
      Some(template -> binders),
      isSelf = true
    )

  /** The identity of `this` of a template. */
  def selfKey(template: Frame): String = s"this@${template.id}"

  /** A member atom carries how a reason spells it ahead of its identity, so a reason can name it.
    */
  object Atom {

    private val Spelled = """(?s)member:(\d+):(.*)""".r

    def apply(key: String, spelled: String): String = s"member:${spelled.length}:$spelled$key"

    /** How a reason spells a member atom. */
    def unapply(atom: String): Option[String] = atom match {
      case Spelled(length, rest) if rest.length >= length.toInt => Some(rest.take(length.toInt))
      case _                                                    => None
    }

  }

}
