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
    val (head, name, declared) = parts(tree)
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

  // The keyword a definition is written after, its name and its declared result.
  private def parts(tree: ValOrDefDef | ClassDef): (String, String, Option[TypeTree]) =
    tree match {
      case v: ValDef => (keyword(v), v.name.toString, Some(v.tpt))
      // A secondary constructor is written `this`, with no result.
      case d: DefDef if d.name.toString == "<init>" && !d.symbol.isGivenOrUsing =>
        ("", "this", None)
      case d: DefDef   => (keyword(d), d.name.toString, Some(d.resultTpt))
      case c: ClassDef => ("", c.name.toString, None)
    }

  private def keyword(definition: ValOrDefDef): String = definition match {
    case _ if definition.symbol.isGivenOrUsing => "given "
    case v: ValDef                             => if (v.symbol.kind == Var) "var " else "val "
    case _                                     => ""
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
  private[compiled] def line(written: String): String = new Spacing(written).line

  // The keywords that start a refinement's member or a match type's case.
  private val Members = List("def", "val", "var", "type", "given", "case")

  // One pass of `line` over `written`, copied to `out` up to `at`.
  final private class Spacing(written: String) {
    private val out = new StringBuilder
    // The braces open at each point, `true` for the ones a braceless match type implies.
    private var braces = List.empty[Boolean]
    private var at = 0

    def line: String = {
      while (at < written.length) step()
      braces.filter(identity).foreach(_ => out ++= " }")
      out.toString
    }

    private def step(): Unit = {
      val next = blank(at)
      if (next > at) spacing(next)
      else
        written.charAt(at) match {
          case '"' | '\'' | '`'                                         => copy(literal(at))
          case ',' if ")]".exists(written.lift(blank(at + 1)).contains) => at += 1
          case '{'                                                      =>
            braces = false :: braces
            copy(at + 1)
          case '}' => close()
          case _   => copy(at + 1)
        }
    }

    private def copy(until: Int): Unit = {
      out ++= written.substring(at, until.min(written.length))
      at = until
    }

    // A run of whitespace or comments up to `next`.
    private def spacing(next: Int): Unit = {
      val broken = written.substring(at, next).contains('\n')
      if (broken && opensCases(next)) {
        out ++= " { "
        braces = true :: braces
      } else if (broken && separates(next)) out ++= "; "
      else if (spaced(next)) out += ' '
      at = next
    }

    // A match type's braceless cases start at `next`.
    private def opensCases(next: Int): Boolean =
      member(next, "case") && out.toString.endsWith("match")

    // A member of a refinement or a case starts at `next`, after another.
    private def separates(next: Int): Boolean =
      braces.nonEmpty && Members.exists(member(next, _)) && !"{;".contains(before)

    // A space, but none after an opening bracket, before a closing one or a comma, or between
    // clauses.
    private def spaced(next: Int): Boolean = {
      val after = if (next == written.length) ')' else written.charAt(next)
      !"([".contains(before) && !")],".contains(after) &&
      !(")]".contains(before) && "([".contains(after))
    }

    private def before: Char = out.lastOption.getOrElse('(')

    private def close(): Unit = {
      val (implied, rest) = braces.span(identity)
      implied.foreach(_ => out ++= " }")
      braces = rest.drop(1)
      copy(at + 1)
    }

    private def member(at: Int, keyword: String): Boolean =
      written.startsWith(keyword, at) &&
        !written.lift(at + keyword.length).exists(c => c.isLetterOrDigit || c == '_')

    // Where the whitespace and comments from `at` end.
    @scala.annotation.tailrec
    private def blank(at: Int): Int =
      if (at >= written.length) written.length
      else if (written.charAt(at).isWhitespace) blank(at + 1)
      else if (written.startsWith("//", at)) blank(past("\n", at))
      else if (written.startsWith("/*", at)) blank(past("*/", at + 2))
      else at

    // Where the first `token` from `from` ends (the text's end when none does).
    private def past(token: String, from: Int): Int = {
      val found = written.indexOf(token, from)
      if (found < 0) written.length else found + token.length
    }

    // Where the literal or backticked name at `at` ends: a string, a character, or one quote mark
    // of a symbol or a quote (`'{`).
    private def literal(at: Int): Int = written.charAt(at) match {
      case '`'                                     => past("`", at + 1)
      case '"' if written.startsWith("\"\"\"", at) => past("\"\"\"", at + 3)
      case '"'                                     => string(at)
      case _                                       => quote(at)
    }

    // Where the string at `at` ends: an escape, and in an interpolated one `$"`, kept inside it.
    private def string(at: Int): Int = {
      val interpolated = at > 0 && written.charAt(at - 1).isLetter
      @scala.annotation.tailrec
      def end(i: Int): Int =
        if (i >= written.length) written.length
        else if (written.charAt(i) == '\\') end(i + 2)
        else if (interpolated && dollar(i)) end(i + 2)
        else if (written.charAt(i) == '"') i + 1
        else end(i + 1)
      end(at + 1)
    }

    // An interpolated string's `$"` or `$$`.
    private def dollar(i: Int): Boolean =
      written.charAt(i) == '$' && written.lift(i + 1).exists("\"$".contains(_))

    // Where a character literal at `at` ends, or one quote mark of a symbol or a quote.
    private def quote(at: Int): Int =
      if (written.lift(at + 1).contains('\\')) past("'", at + 2)
      else if (written.lift(at + 2).contains('\'')) (at + 3).min(written.length)
      else at + 1

  }

}
