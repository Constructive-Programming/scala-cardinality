package cardinality.types

import scala.meta.*
import scala.meta.tokenizers.Tokenized
import scala.util.matching.Regex

/** Source text on one line, the way a report row, an artifact field and a reason quote it. A row
  * must stay one line and one declaration read one way however it was laid out, so only layout
  * changes (`TokenLayout`). Text the tokenizer cannot read back exactly — prose, a character Scala
  * does not have — keeps its characters, its whitespace collapsed outside literals (`line`).
  */
private[cardinality] object Spelling {
  private given Dialect = dialects.Scala3

  def of(tree: Tree): String = flat(tree.syntax)

  def flat(text: String): String = text.tokenize match {
    case Tokenized.Success(tokens) if tokens.map(_.text).mkString == text =>
      TokenLayout.joined(tokens)
    case _ => line(text)
  }

  /** Text on one line: every run of whitespace is one space, except inside a string or character
    * literal or a backquoted name (a literal type with two spaces is not the one with one).
    */
  def line(text: String): String =
    Layout
      .replaceAllIn(
        text,
        found => if (found.matched.head.isWhitespace) " " else Regex.quoteReplacement(found.matched)
      )
      .trim

  // A one-line literal or backquoted name, kept whole. A raw string that spans lines cannot stay
  // on one line as written, so its line breaks collapse with the rest of the layout.
  private val Literal = """"(?:[^"\\\n]|\\.)*"|`[^`\n]+`|'(?:[^'\\\n]|\\.)'"""

  private val Layout = s"$Literal|\\s+".r

  private val Literals = Literal.r

  /** `text` with the inside of every literal and backquoted name blanked out, character for
    * character: a scan of the result cuts the original at the same index, never inside a literal.
    */
  def masked(text: String): String =
    Literals.replaceAllIn(
      text,
      found => {
        val quoted = found.matched
        val inside = Blank.toString * (quoted.length - 2)
        Regex.quoteReplacement(s"${quoted.head}$inside${quoted.last}")
      }
    )

  /** What `masked` writes inside a literal: no bracket, separator, quote or whitespace. */
  val Blank: Char = '\u0001'

  /** `text` with `replace` applied to every match of `pattern` outside a literal (`masked`). */
  def outsideLiterals(text: String, pattern: Regex)(replace: String => String): String = {
    val (out, rest) = pattern.findAllMatchIn(masked(text)).foldLeft((new StringBuilder, 0)) {
      case ((out, from), found) =>
        val matched = text.substring(found.start, found.end)
        (out ++= text.substring(from, found.start) ++= replace(matched), found.end)
    }
    out.append(text.substring(rest)).toString
  }

  /** A declared name as a signature starts with it: backquoted where the source must quote it, and
    * set apart from a result's colon when it ends in an operator character or `_`, which would
    * otherwise take the colon into the name (`+: : Boolean`, not `+:: Boolean`).
    */
  def declared(name: Name, beforeColon: Boolean): String = {
    val spelled = name.syntax
    if (beforeColon && spelled.lastOption.exists(c => c == '_' || operatorCharacter(c)))
      s"$spelled "
    else spelled
  }

  /** A character an operator name is made of (Scala's `opchar`, Unicode math and other symbols). */
  def operatorCharacter(c: Char): Boolean =
    OperatorCharacters.contains(c) || Character.getType(c) == Character.MATH_SYMBOL ||
      Character.getType(c) == Character.OTHER_SYMBOL

  private val OperatorCharacters = "!#%&*+-/:<=>?@\\^|~"

}
