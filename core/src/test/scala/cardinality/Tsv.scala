package cardinality

import cardinality.types.Spelling

/** The tab-separated artifacts the opt-in runners write and read back: every field one line with no
  * tab, so a row splits back into its fields; a list joins its items with `; `; columns are read by
  * name, so a reader survives an added column.
  *
  * Inside braces `; ` is Scala's own statement separator, which `Spelling` writes between a
  * refinement's members (`Two { type X = Int; type Y = Boolean }`), so it stays as written and is
  * no list separator; an item's `; ` anywhere else is written `, `.
  */
private[cardinality] object Tsv {

  val Separator = "; "

  // A run of whitespace holding a tab or a line break is one space; spaces alone are what a row
  // printed, a literal type's included, and stay.
  def field(text: String): String = text.replaceAll("\\s*[\\t\\r\\n]\\s*", " ").trim

  def list(items: List[String]): String =
    items
      .map { item =>
        val text = field(item)
        listed(text).foldLeft(text)((written, at) => written.patch(at, ", ", Separator.length))
      }
      .mkString(Separator)

  def items(field: String): List[String] =
    if (field.isEmpty) Nil
    else {
      val cuts = listed(field)
      (0 :: cuts.map(_ + Separator.length))
        .zip(cuts :+ field.length)
        .map((from, to) => field.substring(from, to))
    }

  // Where `; ` stands outside every literal and every pair of braces. Pairs match innermost first,
  // so a brace a cut-short item leaves open does not hold the separators after it.
  private def listed(text: String): List[Int] = {
    val scanned = Spelling.masked(text)
    val braces = scanned.indices
      .foldLeft((List.empty[Int], List.empty[(Int, Int)])) {
        case ((open, pairs), i) if scanned(i) == '{'       => (i :: open, pairs)
        case ((at :: open, pairs), i) if scanned(i) == '}' => (open, (at, i) :: pairs)
        case (state, _)                                    => state
      }
      ._2
    Separator.r
      .findAllMatchIn(scanned)
      .map(_.start)
      .filterNot(at => braces.exists((open, close) => open < at && at < close))
      .toList
  }

  /** A header and its rows, every field sanitized, ending in a newline. */
  def table(header: List[String], rows: List[List[String]]): String =
    (header :: rows).map(_.map(field).mkString("\t")).mkString("", "\n", "\n")

  /** The rows of a table, each as its fields by column name. */
  def rows(text: String): List[Map[String, String]] =
    text.linesIterator.toList match {
      case Nil            => Nil
      case header :: rest =>
        val names = header.split('\t').toList
        rest.filter(_.nonEmpty).map(line => names.zip(line.split("\t", -1)).toMap)
    }

}
