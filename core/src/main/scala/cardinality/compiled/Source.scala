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
  def parenthesized(from: Int, name: String): Boolean = clauses(from, name)._2

  /** A definition's signature as written, on one line: its name, its clauses and its declared
    * result, without modifiers or body. An inferred result is not spelled.
    */
  def signature(tree: ValOrDefDef | ClassDef): String = {
    val point = tree.pos.pointOffset
    val (head, name, declared) = tree match {
      case v: ValDef if v.symbol.isGivenOrUsing => ("given ", v.name.toString, Some(v.tpt))
      case d: DefDef if d.symbol.isGivenOrUsing => ("given ", d.name.toString, Some(d.resultTpt))
      case v: ValDef => (if (v.symbol.kind == Var) "var " else "val ", v.name.toString, Some(v.tpt))
      // A secondary constructor is written `this`, with no result.
      case d: DefDef if d.name.toString == "<init>" => ("", "this", None)
      case d: DefDef                                => ("", d.name.toString, Some(d.resultTpt))
      case c: ClassDef                              => ("", c.name.toString, None)
    }
    // A name in backticks placed after its opening tick starts at the tick.
    val from = if (point > 0 && text.charAt(point - 1) == '`') point - 1 else point
    val result = declared.map(_.pos).filter(p => !p.isUnknown && !p.isZeroExtent)
    val end = result.filter(_.startOffset >= from).fold(clauses(point, name)._1)(_.endOffset)
    val until = balanced(from.min(text.length), end.min(text.length))
    val written = Source.line(text.substring(from.min(text.length), until))
    // An anonymous given is written as its type alone.
    if (head == "given " && result.exists(_.startOffset == from)) s"given: $written"
    else head + written
  }

  private val Var = tastyquery.Modifiers.TermSymbolKind.Var

  // Where the clauses after `name`, written at `from`, end (each bracket group that follows it),
  // and whether one of them is in parentheses.
  private def clauses(from: Int, name: String): (Int, Boolean) = {
    @scala.annotation.tailrec
    def after(at: Int, round: Boolean): (Int, Boolean) = {
      val next = text.indexWhere(c => c != ' ' && c != '\t', at)
      if (next >= 0 && "[(".contains(text.charAt(next)))
        after(closing(next) + 1, round || text.charAt(next) == '(')
      else (at, round)
    }
    // A name in backticks is placed at the tick or after it.
    val ticks =
      if (text.startsWith("`", from)) 2 else if (text.startsWith("`", from + name.length)) 1 else 0
    after((from + name.length + ticks).min(text.length), round = false)
  }

  // `until`, past the closing parentheses the text from `from` leaves open: a result written in
  // parentheses (`given g: (A ?=> B)`) is placed inside them.
  private def balanced(from: Int, until: Int): Int = {
    val open =
      text.substring(from, until).count(_ == '(') - text.substring(from, until).count(_ == ')')
    (1 to open).foldLeft(until) { (at, _) =>
      val next = text.indexWhere(!_.isWhitespace, at)
      if (next >= 0 && text.charAt(next) == ')') next + 1 else at
    }
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

  /** A reason as the source would write the names it quotes: no `$package` object, no `$` of a
    * module class, no `C.this.` prefix, a root-imported name bare (`Int`, `String`, `Option`) and
    * `&` and `|` infix (`Int & String`, not `scala.&[scala.Int, scala.Predef.String]`).
    */
  def spelled(reason: String): String = {
    val plain = Plain.foldLeft(reason)((text, rule) => rule._1.replaceAllIn(text, rule._2))
    infix(plain)
  }

  private val Plain = List(
    """(?<![^\s(\[,.])[^\s.\[\](),]+\$package\$?\.""".r -> "",
    """\b\w+\$?\.this\.""".r -> "",
    """(\w)\$(?=[.\[\],)\s]|$)""".r -> "$1",
    """(^|[\s(\[,])\.(?=[A-Za-z_])""".r -> "$1",
    """(?<![\w.$])(?:scala\.Predef|scala|java\.lang)\.([A-Z]\w*)\b(?![.\w])""".r -> "$1"
  )

  // `scala.&[A, B]` and `scala.|[A, B]` as `A & B` and `A | B`, each side spelled first.
  private def infix(text: String): String = {
    val at = List("scala.&[", "scala.|[").map(text.indexOf).filter(_ >= 0).minOption
    at match {
      case None        => text
      case Some(start) =>
        val open = start + "scala.&".length
        val parts = arguments(text, open)
        parts match {
          case Some((List(left, right), close)) =>
            val op = text.charAt(start + "scala.".length)
            def side(part: String) = {
              val spelled = infix(part.trim)
              if (op == '&' && spelled.contains(" | ")) s"($spelled)" else spelled
            }
            infix(text.take(start) + side(left) + s" $op " + side(right) + text.drop(close + 1))
          case _ => text
        }
    }
  }

  // The top-level arguments of the bracket at `open`, and where it closes.
  private def arguments(text: String, open: Int): Option[(List[String], Int)] = {
    @scala.annotation.tailrec
    def go(at: Int, depth: Int, start: Int, found: List[String]): Option[(List[String], Int)] =
      if (at >= text.length) None
      else
        text.charAt(at) match {
          case '[' | '(' | '{'               => go(at + 1, depth + 1, start, found)
          case ']' | ')' | '}' if depth == 1 =>
            Some(((text.substring(start, at) :: found).reverse, at))
          case ']' | ')' | '}'   => go(at + 1, depth - 1, start, found)
          case ',' if depth == 1 => go(at + 1, depth, at + 1, text.substring(start, at) :: found)
          case _                 => go(at + 1, depth, start, found)
        }
    go(open + 1, 1, open + 1, Nil)
  }

  /** One line, spaced as a signature is printed: a literal or a name in backticks as written; a run
    * of whitespace or comments one space, none inside brackets or between clauses (`f(x: A)(y:
    * A)`); a line break before a member of a refinement or a case `; `; a match type's braceless
    * cases in braces; a trailing comma dropped.
    */
  private[compiled] def line(written: String): String = {
    val out = new StringBuilder
    // The braces open at each point, `true` for the ones a braceless match type implies.
    var braces = List.empty[Boolean]
    var at = 0
    def copy(until: Int) = { out ++= written.substring(at, until.min(written.length)); at = until }
    while (at < written.length) written.charAt(at) match {
      case '"' | '\'' | '`' => copy(literal(written, at))
      case c if c.isWhitespace || written.startsWith("//", at) || written.startsWith("/*", at) =>
        val next = blank(written, at)
        val broken = written.substring(at, next).contains('\n')
        val before = out.lastOption.getOrElse('(')
        val after = if (next == written.length) ')' else written.charAt(next)
        if (broken && member(written, next, "case") && out.toString.endsWith("match")) {
          out ++= " { "
          braces = true :: braces
        } else if (
          broken && braces.nonEmpty && Members.exists(member(written, next, _)) &&
          !"{;".contains(before)
        )
          out ++= "; "
        else if (
          !"([".contains(before) && !")],".contains(after) &&
          !(")]".contains(before) && "([".contains(after))
        ) out += ' '
        at = next
      case ',' if ")]".exists(written.lift(blank(written, at + 1)).contains) => at += 1
      case '{'                                                               =>
        braces = false :: braces
        copy(at + 1)
      case '}' =>
        val (implied, rest) = braces.span(identity)
        implied.foreach(_ => out ++= " }")
        braces = rest.drop(1)
        copy(at + 1)
      case _ => copy(at + 1)
    }
    braces.filter(identity).foreach(_ => out ++= " }")
    out.toString
  }

  // The keywords that start a refinement's member or a match type's case.
  private val Members = List("def", "val", "var", "type", "given", "case")

  private def member(written: String, at: Int, keyword: String): Boolean =
    written.startsWith(keyword, at) &&
      !written.lift(at + keyword.length).exists(c => c.isLetterOrDigit || c == '_')

  // Where the whitespace and comments from `at` end.
  @scala.annotation.tailrec
  private def blank(written: String, at: Int): Int =
    if (at >= written.length) written.length
    else if (written.charAt(at).isWhitespace) blank(written, at + 1)
    else if (written.startsWith("//", at))
      blank(written, Some(written.indexOf('\n', at)).filter(_ >= 0).getOrElse(written.length))
    else if (written.startsWith("/*", at))
      blank(written, Some(written.indexOf("*/", at + 2)).filter(_ >= 0).fold(written.length)(_ + 2))
    else at

  // Where the literal or backticked name at `at` ends: a string (an escape, and in an interpolated
  // one `$"`, kept inside it), a character, or one quote mark of a symbol or a quote (`'{`).
  private def literal(written: String, at: Int): Int = {
    def from(i: Int) = i.min(written.length)
    written.charAt(at) match {
      case '`' =>
        from(Some(written.indexOf('`', at + 1)).filter(_ >= 0).fold(written.length)(_ + 1))
      case '"' if written.startsWith("\"\"\"", at) =>
        from(Some(written.indexOf("\"\"\"", at + 3)).filter(_ >= 0).fold(written.length)(_ + 3))
      case '"' =>
        val interpolated = at > 0 && written.charAt(at - 1).isLetter
        @scala.annotation.tailrec
        def close(i: Int): Int =
          if (i >= written.length) written.length
          else if (written.charAt(i) == '\\') close(i + 2)
          else if (
            interpolated && written
              .charAt(i) == '$' && written.lift(i + 1).exists("\"$".contains(_))
          )
            close(i + 2)
          else if (written.charAt(i) == '"') i + 1
          else close(i + 1)
        close(at + 1)
      case _ if written.lift(at + 1).contains('\\') =>
        from(Some(written.indexOf('\'', at + 2)).filter(_ >= 0).fold(written.length)(_ + 1))
      case _ if written.lift(at + 2).contains('\'') => from(at + 3)
      case _                                        => at + 1
    }
  }

}
