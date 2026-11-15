package cardinality.reporting

import scala.util.matching.Regex

/** The category a reason counts under when a report tallies what blocked its rows.
  *
  * A reason reads `<category>: <detail>`. Its detail may hold colons of its own (`A <: S`, `A:
  * Type`, a nested reason), so the category is the text before the first `: ` outside brackets and
  * literals whose colon ends no operator name (`<:`, `::`, `+:`), each run of code words in it read
  * as one `_`: every subtype, parameter or name of one kind tallies once. A union reason names its
  * union in parentheses, `union (A | B): <reason>`, and counts under the reason its alternative
  * stopped on (`union _: unresolved type`), whatever the alternatives are.
  *
  * A capital, a digit, an operator character, a literal or a bracket around it marks a word as
  * code, so a producer puts a name — which may be lower-case — after the separator, never before.
  */
object Blockers {

  def category(reason: String): String = {
    val (head, detail) = split(Scanned(reason))
    if (head.startsWith(UnionHead) && head.endsWith(")") && detail.nonEmpty)
      s"union _: ${category(detail)}"
    else abstracted(Scanned(head))
  }

  private val UnionHead = "union ("

  /** A text and its copy with the inside of every literal and backquoted name blanked out,
    * character for character: a scan of the copy cuts the text at the same index, never inside a
    * literal.
    */
  final private case class Scanned(text: String) {

    val masked: String = Literals.replaceAllIn(
      text,
      found => {
        val quoted = found.matched
        val inside = Blank.toString * (quoted.length - 2)
        Regex.quoteReplacement(s"${quoted.head}$inside${quoted.last}")
      }
    )

    // A colon right after an operator character (Scala's `opchar`, Unicode math and other
    // symbols) is part of an operator name or a bound (`+:`, `<:`). After a letter, a digit, `_`, a
    // bracket or a quote it is the separator: `Missing_: …` is the name `Missing_` and its detail,
    // as a producer that writes a name before the separator means.
    def operator(colon: Int): Boolean = colon > 0 && {
      val c = masked(colon - 1)
      OperatorCharacters.contains(c) || Character.getType(c) == Character.MATH_SYMBOL ||
      Character.getType(c) == Character.OTHER_SYMBOL
    }

  }

  /** A word of a category as written and as scanned, the bracket depth before it, and whether it
    * starts the reason.
    */
  final private case class Word(text: String, masked: String, depth: Int, first: Boolean)

  // The category and its detail, cut at the first separator: a `: ` outside brackets and literals
  // whose colon ends no operator name.
  private def split(reason: Scanned): (String, String) = {
    val scanned = reason.masked
    val separator = scanned.indices.foldLeft((0, Option.empty[Int])) {
      case (found @ (_, Some(_)), _) => found
      case ((depth, None), i)        =>
        scanned(i) match {
          case '(' | '[' | '{' => (depth + 1, None)
          case ')' | ']' | '}' => (depth - 1, None)
          case ':' if depth == 0 && scanned.startsWith(": ", i) && !reason.operator(i) =>
            (depth, Some(i))
          case _ => (depth, None)
        }
    }
    val text = reason.text
    separator._2.fold((text, ""))(i => (text.take(i), text.drop(i + 2)))
  }

  // The category's prose, each maximal run of code words in it replaced by `_`. Words are read off
  // the masked text, so a literal with spaces is one word, and a word inside brackets or braces —
  // a keyword of a refinement, a lower-case type argument — is code.
  private def abstracted(head: Scanned): String = {
    val found = Words.findAllMatchIn(head.masked).toList
    val masks = found.map(m => head.masked.substring(m.start, m.end))
    val depths = masks.scanLeft(0) { (depth, masked) =>
      depth + masked.count(Opening.contains(_)) - masked.count(Closing.contains(_))
    }
    val words = found.lazyZip(masks).lazyZip(depths).toList.zipWithIndex.map {
      case ((m, masked, depth), index) =>
        Word(head.text.substring(m.start, m.end), masked, depth, first = index == 0)
    }
    val runs = words.foldLeft(List.empty[(Boolean, List[Word])]) { (acc, word) =>
      val isCode = word.depth > 0 || literal(word) || code(word)
      acc match {
        case (true, run) :: rest if isCode => (true, word :: run) :: rest
        case _                             => (isCode, List(word)) :: acc
      }
    }
    runs.reverse
      .map {
        case (true, run) => placeholder(run.reverse)
        case (_, run)    => run.map(_.text).mkString(" ")
      }
      .mkString(" ")
  }

  private val Words = "\\S+".r

  private val Opening = "[{"

  private val Closing = "]}"

  private def literal(word: Word): Boolean =
    word.masked.contains('"') || word.masked.contains(Blank)

  // A run of code keeps the punctuation of the prose around it: a parenthesis it closes or opens
  // without its partner, and a comma after it.
  private def placeholder(run: List[Word]): String = {
    val written = run.map(_.text).mkString(" ")
    val comma = if (written.endsWith(",")) "," else ""
    val body = written.stripSuffix(",")
    val balance = body.count(_ == '(') - body.count(_ == ')')
    val open = if (balance > 0 && body.startsWith("(" * balance)) "(" * balance else ""
    val close = if (balance < 0 && body.endsWith(")" * -balance)) ")" * -balance else ""
    s"${open}_$close$comma"
  }

  // A word of code: a name, path, type or operator. Inside a category, an upper-case letter marks
  // a name except at the start of the reason, where it begins the sentence.
  private def code(word: Word): Boolean = {
    val written = core(word)
    written.nonEmpty && (written.exists(c => c.isDigit || CodeMarks.contains(c)) || named(word))
  }

  // Whether a word with a core is capitalized as a name.
  private def named(word: Word): Boolean = {
    val written = core(word)
    written.drop(1).exists(_.isUpper) || (!word.first && written.head.isUpper)
  }

  // A word without the parentheses around it and a comma after it.
  private def core(word: Word): String =
    word.text.dropWhile(_ == '(').reverse.dropWhile(c => c == ')' || c == ',').reverse

  private val CodeMarks = "[]{}()|&#.=<>*$_`^?:"

  // Scanning a reason without cutting inside a literal or a backquoted name.

  // A one-line literal or backquoted name, kept whole. A raw string that spans lines cannot stay
  // on one line as written, so its line breaks collapse with the rest of the layout.
  private val Literal = """"(?:[^"\\\n]|\\.)*"|`[^`\n]+`|'(?:[^'\\\n]|\\.)'"""

  private val Literals = Literal.r

  /** What `Scanned` writes inside a literal: no bracket, separator, quote or whitespace. */
  private val Blank: Char = '\u0001'

  private val OperatorCharacters = "!#%&*+-/:<=>?@\\^|~"

}
