package cardinality.compiled

import tastyquery.SourcePosition
import tastyquery.Trees.*

/** One source as a report shows it: its text gives a row its signature and names what the compiler
  * left nameless, and where the classes place a definition is checked against it.
  */
final private[compiled] class Source(val path: String, text: String) {

  // The offset each line starts at.
  private val lines: Array[Int] =
    (0 +: text.indices.filter(text.charAt(_) == '\n').map(_ + 1)).toArray

  /** Whether `name` is written at `offset`, in backticks or not. */
  def spells(name: String, offset: Int): Boolean =
    text.startsWith(name, offset) || text.startsWith(s"`$name`", offset)

  /** Whether the text puts a position's point on the line and column the classes recorded: an edit
    * that changes the length of a line before it moves one of them.
    */
  def placed(pos: SourcePosition): Boolean = {
    val line = lineOf(pos.pointOffset)
    !pos.hasLineColumnInformation ||
    (line == pos.pointLine + 1 && pos.pointOffset - lines(line - 1) == pos.pointColumn)
  }

  /** The line, counted from one, of the `extension` an extension method is written under: the last
    * one before its first parameter.
    */
  def extensionLine(method: DefDef): Int = {
    val first = method.paramLists.flatMap(_.merge).map(_.pos.startOffset).minOption
    val keyword = first.fold(-1)(text.lastIndexOf("extension", _))
    lineOf(if (keyword < 0) method.pos.startOffset else keyword)
  }

  private def lineOf(offset: Int): Int = {
    val found = java.util.Arrays.binarySearch(lines, offset)
    if (found >= 0) found + 1 else -found - 1
  }

  /** Whether the definition named at `from` is written with a parameter clause in parentheses. */
  def parenthesized(from: Int, name: String): Boolean =
    text.substring(from, clauses(from, name)).contains('(')

  /** A definition's signature as written, on one line: its name, its clauses and its declared
    * result, without modifiers or body. An inferred result is not spelled.
    */
  def signature(tree: ValOrDefDef | ClassDef): String = {
    val from = tree.pos.pointOffset
    val (head, name, declared) = tree match {
      case v: ValDef if v.symbol.isGivenOrUsing => ("given ", v.name.toString, Some(v.tpt))
      case d: DefDef if d.symbol.isGivenOrUsing => ("given ", d.name.toString, Some(d.resultTpt))
      case v: ValDef => (if (v.symbol.kind == Var) "var " else "val ", v.name.toString, Some(v.tpt))
      case d: DefDef => ("", d.name.toString, Some(d.resultTpt))
      case c: ClassDef => ("", c.name.toString, None)
    }
    val result = declared.map(_.pos).filter(p => !p.isUnknown && !p.isZeroExtent)
    val until = result.filter(_.startOffset >= from).fold(clauses(from, name))(_.endOffset)
    val written = Source.line(text.substring(from, until.min(text.length)))
    // An anonymous given is written as its type alone.
    if (head == "given " && result.exists(_.startOffset == from)) s"given: $written"
    else head + written
  }

  private val Var = tastyquery.Modifiers.TermSymbolKind.Var

  // Where the clauses after `name`, written at `from`, end: each bracket group that follows it.
  private def clauses(from: Int, name: String): Int = {
    @scala.annotation.tailrec
    def after(at: Int): Int = {
      val next = text.indexWhere(c => c != ' ' && c != '\t', at)
      if (next >= 0 && "[(".contains(text.charAt(next))) after(closing(next) + 1) else at
    }
    // A name in backticks is placed at the tick or after it.
    val ticks =
      if (text.startsWith("`", from)) 2 else if (text.startsWith("`", from + name.length)) 1 else 0
    after((from + name.length + ticks).min(text.length))
  }

  // The offset of the bracket closing the one at `open` (the text's end when none does).
  private def closing(open: Int): Int = {
    val depths = text.indices.iterator.drop(open).scanLeft(0) { (depth, at) =>
      val c = text.charAt(at)
      if ("[(".contains(c)) depth + 1 else if ("])".contains(c)) depth - 1 else depth
    }
    val closed = depths.indexOf(0, 1)
    if (closed < 0) text.length - 1 else open + closed - 1
  }

}

private[compiled] object Source {

  // One line, spaced as a signature is printed.
  private def line(written: String): String =
    written.replaceAll("\\s+", " ").replace("( ", "(").replace(" )", ")").replace(",)", ")").trim

}
