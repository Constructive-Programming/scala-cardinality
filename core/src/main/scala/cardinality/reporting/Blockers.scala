package cardinality.reporting

import cardinality.types.Spelling

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
    val (head, detail) = split(reason)
    if (head.startsWith(UnionHead) && head.endsWith(")") && detail.nonEmpty)
      s"union _: ${category(detail)}"
    else abstracted(head)
  }

  private val UnionHead = "union ("

  // The category and its detail, cut at the first separator: a `: ` outside brackets and literals
  // whose colon ends no operator name.
  private def split(reason: String): (String, String) = {
    val scanned = Spelling.masked(reason)
    val separator = scanned.indices.foldLeft((0, Option.empty[Int])) {
      case (found @ (_, Some(_)), _) => found
      case ((depth, None), i)        =>
        scanned(i) match {
          case '(' | '[' | '{' => (depth + 1, None)
          case ')' | ']' | '}' => (depth - 1, None)
          case ':' if depth == 0 && scanned.startsWith(": ", i) && !operator(scanned, i) =>
            (depth, Some(i))
          case _ => (depth, None)
        }
    }
    separator._2.fold((reason, ""))(i => (reason.take(i), reason.drop(i + 2)))
  }

  // A colon right after an operator character is part of an operator name or a bound (`+:`, `<:`).
  // After a letter, a digit, `_`, a bracket or a quote it is the separator: `Missing_: …` is the
  // name `Missing_` and its detail, as a producer that writes a name before the separator means.
  private def operator(scanned: String, colon: Int): Boolean =
    colon > 0 && Spelling.operatorCharacter(scanned(colon - 1))

  // The category's prose, each maximal run of code words in it replaced by `_`. Words are read off
  // the masked text, so a literal with spaces is one word, and a word inside brackets or braces —
  // a keyword of a refinement, a lower-case type argument — is code.
  private def abstracted(head: String): String = {
    val scanned = Spelling.masked(head)
    val words = Word
      .findAllMatchIn(scanned)
      .toList
      .map(found =>
        (head.substring(found.start, found.end), scanned.substring(found.start, found.end))
      )
    val depths = words.scanLeft(0) {
      case (depth, (_, masked)) =>
        depth + masked.count(Opening.contains(_)) - masked.count(Closing.contains(_))
    }
    val runs = words.zip(depths).zipWithIndex.foldLeft(List.empty[(Boolean, List[String])]) {
      case (acc, (((word, masked), depth), index)) =>
        val isCode = depth > 0 || literal(masked) || code(word, first = index == 0)
        acc match {
          case (true, run) :: rest if isCode => (true, word :: run) :: rest
          case _                             => (isCode, List(word)) :: acc
        }
    }
    runs.reverse
      .map {
        case (true, run) => placeholder(run.reverse.mkString(" "))
        case (_, run)    => run.mkString(" ")
      }
      .mkString(" ")
  }

  private val Word = "\\S+".r

  private val Opening = "[{"

  private val Closing = "]}"

  private def literal(masked: String): Boolean =
    masked.contains('"') || masked.contains(Spelling.Blank)

  // A run of code keeps the punctuation of the prose around it: a parenthesis it closes or opens
  // without its partner, and a comma after it.
  private def placeholder(run: String): String = {
    val comma = if (run.endsWith(",")) "," else ""
    val body = run.stripSuffix(",")
    val balance = body.count(_ == '(') - body.count(_ == ')')
    val open = if (balance > 0 && body.startsWith("(" * balance)) "(" * balance else ""
    val close = if (balance < 0 && body.endsWith(")" * -balance)) ")" * -balance else ""
    s"${open}_$close$comma"
  }

  // A word of code: a name, path, type or operator. Inside a category, an upper-case letter marks
  // a name except at the start of the reason, where it begins the sentence.
  private def code(word: String, first: Boolean): Boolean = {
    val core = word.dropWhile(_ == '(').reverse.dropWhile(c => c == ')' || c == ',').reverse
    core.nonEmpty && (
      core.exists(c => c.isDigit || CodeMarks.contains(c)) ||
        core.drop(1).exists(_.isUpper) ||
        (!first && core.head.isUpper)
    )
  }

  private val CodeMarks = "[]{}()|&#.=<>*$_`^?:"

}
