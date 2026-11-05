package cardinality.types

import scala.meta.XtensionClassifiable
import scala.meta.tokens.Token

/** A token sequence written on one line the way Scala reads it (`Spelling.flat`).
  *
  * Layout between two tokens is one space, none just inside a parenthesis or bracket, and a
  * trailing comma goes. A line break Scala reads as a statement separator is written `; `: inside
  * braces when the token before can end a statement and the token after can begin one (the
  * classical newline rule). An operator name continues its expression at a line end unless a
  * definition follows (`type F = ~>` then `type G = Int`).
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
    kept.foldLeft(Line(Vector.empty, None, Nil))(_.add(_)).out.mkString
  }

  // The layout that stood before a written token: none, spaces only, a line break.
  private enum Gap {
    case None, Space, Break
  }

  private object Gap {

    def of(layout: String): Gap =
      if (layout.isEmpty) None
      else if (!layout.contains('\n')) Space
      else Break

  }

  private type Written = (Token, Gap)

  // The spelling so far, the last token written, and the brackets open around the next token.
  final private case class Line(out: Vector[String], previous: Option[Token], open: List[Token]) {

    def add(written: Written): Line = {
      val (token, gap) = written
      val between =
        if (
          gap == Gap.Break && open.headOption.exists(_.is[Token.LeftBrace]) &&
          previous.exists(ends(_, token)) && !NotBeginning.exists(_(token))
        ) "; "
        else if (gap == Gap.None || closing(token) || previous.exists(opening)) ""
        else " "
      Line(out :+ between :+ token.text, Some(token), nested(open, token))
    }

  }

  private def nested(open: List[Token], token: Token): List[Token] =
    if (opening(token) || token.is[Token.LeftBrace]) token :: open
    else if (closing(token) || token.is[Token.RightBrace]) open.drop(1)
    else open

  private def ends(token: Token, next: Token): Boolean = token match {
    case ident: Token.Ident => !symbolic(ident) || Definitions.exists(_(next))
    case _                  => Ending.exists(_(token))
  }

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
