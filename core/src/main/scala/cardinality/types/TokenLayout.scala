package cardinality.types

import scala.meta.XtensionClassifiable
import scala.meta.tokens.Token

/** A token sequence written on one line the way Scala reads it (`Spelling.flat`).
  *
  * Layout between two tokens is one space, none just inside a parenthesis or bracket, and a
  * trailing comma goes. A line break Scala reads as a statement separator is written `; `: inside
  * braces when the token before can end a statement and the token after can begin one (the
  * classical newline rule). An operator name continues its expression at a line end unless a
  * definition follows (`type F = ~>` then `type G = Int`), and at a line start only as Scala 3's
  * leading infix operator: after no blank line, with whitespace and an operand after it, so `-bar`
  * begins a statement. A braceless `match` opens an indentation region at its cases' column; one
  * line cannot hold that region, so it is written in the braces it stands for, its cases separated
  * as statements: `T match\n  case A => B\n  case _ => C` reads
  * `T match { case A => B; case _ => C }`.
  */
private[types] object TokenLayout {

  def joined(tokens: Iterable[Token]): String = {
    val written = tokens
      .foldLeft((List.empty[Written], "")) {
        case ((acc, layout), trivia: Token.Trivia)        => (acc, layout + trivia.text)
        case ((acc, layout), token) if token.text.isEmpty => (acc, layout)
        case ((acc, layout), token)                       => ((token, Gap.of(layout)) :: acc, "")
      }
      ._1
      .reverse
    val kept = written.zip(written.drop(1).map(Some(_)) :+ None).collect {
      case (token, next) if !trailingComma(token._1, next.map(_._1)) => token
    }
    val start = Line(Vector.empty, None, Nil, kept.headOption.fold(0)(w => column(w._1)))
    kept
      .zip(kept.drop(1).map(Some(_)) :+ None)
      .foldLeft(start)((line, step) => line.add(step._1, step._2))
      .finished
  }

  // The layout that stood before a written token: none, spaces only, a line break, a blank line.
  private enum Gap {
    case None, Space, Break, Blank

    def broken: Boolean = this == Break || this == Blank
  }

  private object Gap {
    private val BlankLine = "\n[ \t\r]*\n".r

    def of(layout: String): Gap =
      if (layout.isEmpty) None
      else if (!layout.contains('\n')) Space
      else if (BlankLine.findFirstIn(layout).nonEmpty) Blank
      else Break

  }

  private type Written = (Token, Gap)

  // A bracket open around a token, or the indentation region a braceless `match` opened, at the
  // column of its cases.
  private enum Region {
    case Bracket(token: Token)
    case Indented(column: Int)
  }

  // The spelling so far, the last token written, the regions open around the next token, and the
  // indentation of the line being read.
  final private case class Line(
      out: Vector[String],
      previous: Option[Token],
      regions: List[Region],
      indent: Int
  ) {

    def add(written: Written, after: Option[Written]): Line = {
      val (token, gap) = written
      val at = column(token)
      if (
        gap.broken && previous.exists(_.is[Token.KwMatch]) && token.is[Token.KwCase] && at > indent
      )
        Line(out :+ " { " :+ token.text, Some(token), Region.Indented(at) :: regions, at)
      else {
        val left = regions.takeWhile(leaves(token, gap, at))
        val rest = regions.drop(left.size)
        val ended = left.nonEmpty || previous.exists(ends(_, token))
        val between =
          if (gap.broken && separates(rest, at) && ended && begins(token, gap, after)) "; "
          else if (gap == Gap.None || closing(token) || (left.isEmpty && previous.exists(opening)))
            ""
          else " "
        Line(
          out ++ Vector.fill(left.size)(" }") :+ between :+ token.text,
          Some(token),
          nested(rest, token),
          if (gap.broken) at else indent
        )
      }
    }

    def finished: String = (out ++ regions.collect { case Region.Indented(_) => " }" }).mkString

  }

  private def column(token: Token): Int = token.pos.startColumn

  // An indentation region ends at a line indented less than its cases, or at a bracket closing
  // around it.
  private def leaves(token: Token, gap: Gap, at: Int)(region: Region): Boolean = region match {
    case Region.Indented(cases) =>
      (gap.broken && at < cases) || closing(token) || token.is[Token.RightBrace]
    case Region.Bracket(_) => false
  }

  // A line break separates statements inside braces, and in an indentation region at its column.
  private def separates(regions: List[Region], at: Int): Boolean = regions.headOption.exists {
    case Region.Bracket(open)   => open.is[Token.LeftBrace]
    case Region.Indented(cases) => cases == at
  }

  private def nested(regions: List[Region], token: Token): List[Region] =
    if (opening(token) || token.is[Token.LeftBrace]) Region.Bracket(token) :: regions
    else if (closing(token) || token.is[Token.RightBrace]) regions.drop(1)
    else regions

  private def ends(token: Token, next: Token): Boolean = token match {
    case ident: Token.Ident => !symbolic(ident) || Definitions.exists(_(next))
    case _                  => Ending.exists(_(token))
  }

  private def begins(token: Token, gap: Gap, after: Option[Written]): Boolean = token match {
    case ident: Token.Ident if symbolic(ident) || ident.text.startsWith("`") =>
      gap == Gap.Blank || !after.exists((next, space) => space != Gap.None && operand(next))
    case _: Token.Ident => true
    case _              => !NotBeginning.exists(_(token))
  }

  private def operand(token: Token): Boolean =
    token.is[Token.Ident] || !(NotBeginning ++ Definitions).exists(_(token))

  private val Ending: List[Token => Boolean] = List(
    _.is[Token.Constant[?]],
    _.is[Token.KwThis],
    _.is[Token.KwNull],
    _.is[Token.KwTrue],
    _.is[Token.KwFalse],
    _.is[Token.KwReturn],
    _.is[Token.KwType],
    _.is[Token.Underscore],
    closing,
    _.is[Token.RightBrace]
  )

  private val NotBeginning: List[Token => Boolean] = List(
    _.is[Token.KwCatch],
    _.is[Token.KwElse],
    _.is[Token.KwExtends],
    _.is[Token.KwFinally],
    _.is[Token.KwForsome],
    _.is[Token.KwMatch],
    _.is[Token.KwWith],
    _.is[Token.KwYield],
    _.is[Token.KwThen],
    _.is[Token.KwDo],
    _.is[Token.Comma],
    _.is[Token.Dot],
    _.is[Token.Semicolon],
    _.is[Token.Colon],
    _.is[Token.Equals],
    _.is[Token.RightArrow],
    _.is[Token.LeftArrow],
    _.is[Token.Subtype],
    _.is[Token.Viewbound],
    _.is[Token.Supertype],
    _.is[Token.Hash],
    _.is[Token.LeftBracket],
    _.is[Token.RightParen],
    _.is[Token.RightBracket],
    _.is[Token.RightBrace]
  )

  // Tokens that begin a definition or a case, never an operand.
  private val Definitions: List[Token => Boolean] = List(
    _.is[Token.KwType],
    _.is[Token.KwDef],
    _.is[Token.KwVal],
    _.is[Token.KwVar],
    _.is[Token.KwCase],
    _.is[Token.KwClass],
    _.is[Token.KwTrait],
    _.is[Token.KwObject],
    _.is[Token.KwGiven],
    _.is[Token.KwImport],
    _.is[Token.KwExport],
    _.is[Token.KwEnum],
    _.is[Token.KwPrivate],
    _.is[Token.KwProtected],
    _.is[Token.KwOverride],
    _.is[Token.KwFinal],
    _.is[Token.KwSealed],
    _.is[Token.KwAbstract],
    _.is[Token.KwImplicit],
    _.is[Token.KwLazy],
    _.is[Token.At]
  )

  private def symbolic(name: Token.Ident): Boolean =
    !name.text.startsWith("`") && name.text.lastOption.exists(Spelling.operatorCharacter)

  private def trailingComma(token: Token, next: Option[Token]): Boolean =
    token.is[Token.Comma] && next.exists(closing)

  private def opening(token: Token): Boolean =
    token.is[Token.LeftParen] || token.is[Token.LeftBracket]

  private def closing(token: Token): Boolean =
    token.is[Token.RightParen] || token.is[Token.RightBracket]

}
