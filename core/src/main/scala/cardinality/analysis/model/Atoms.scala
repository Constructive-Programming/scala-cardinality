package cardinality.analysis.model

/** How a reason names an atom. A binder's atom is `<scope>:<name>`; a member's is
  * `member:<length>:<spelling><key>`, its spelling ahead of its identity.
  */
private[cardinality] object Atoms {

  /** How a reason names a binder's atom: the binder's own name. */
  private def name(atom: String): String = atom.substring(atom.lastIndexOf(':') + 1)

  /** How a reason names an atom: a member by its path, a binder by its name. */
  def label(atom: String): String = Member.unapply(atom).getOrElse(name(atom))

  /** A member atom carries how a reason spells it ahead of its identity, so a reason can name it.
    */
  object Member {

    private val Spelled = """(?s)member:(\d+):(.*)""".r

    def apply(key: String, spelled: String): String = s"member:${spelled.length}:$spelled$key"

    /** How a reason spells a member atom. */
    def unapply(atom: String): Option[String] = atom match {
      case Spelled(length, rest) if rest.length >= length.toInt => Some(rest.take(length.toInt))
      case _                                                    => None
    }

  }

}
