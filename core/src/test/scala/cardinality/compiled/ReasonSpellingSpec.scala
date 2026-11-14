package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

/** A reason names what it quotes as the source would (`Source.spelled`, and the reasons that name a
  * path or a declaration).
  */
class ReasonSpellingSpec extends Specification {

  def is = s2"""
    No `$$package` object, module `$$`, `C.this.` or root prefix; `&` and `|` infix $plain
    An inline override reached by a selection says so                            $selection
    one a called inline method selects is reached by that call                   $deeper
    An anonymous given is named by its line, as its row is                       $anonymousGiven
    A local subclass of a sealed family is named as one                          $localChild
  """

  private def reasons(code: String): Map[String, List[String]] =
    cardinality.Compiled
      .analyze(cardinality.Compiled.inputs("Example.scala" -> code))
      .map(entry =>
        entry.name -> (entry.count match {
          case Count.Unresolved(found) => found
          case _                       => Nil
        })
      )
      .toMap

  def plain =
    List(
      "Scopes$package$.ev[T]" -> "ev[T]",
      "vr.a$package.I" -> "vr.I",
      "eb.E$.Int" -> "eb.E.Int",
      "R$.fbound[A]" -> "R.fbound[A]",
      "FT2.this.C[G]" -> "C[G]",
      "a type: .Default[A]" -> "a type: Default[A]",
      "scala.&[scala.Int, scala.Predef.String]" -> "Int & String",
      "scala.&[scala.|[A, B], java.lang.Object]" -> "(A | B) & Object",
      "scala.collection.immutable.List[scala.Option[A]]" -> "scala.collection.immutable.List[Option[A]]",
      "copy$default$1" -> "copy$default$1",
      "d.01_intro$package$.f" -> "d.f",
      "my-file$package$.x" -> "x",
      "O$.this.T" -> "T",
      "p.scala.Int" -> "p.scala.Int"
    ).map((compiled, written) => Source.spelled(compiled) === written).reduce(_.and(_))

  def selection =
    reasons(
      """|trait V { def g: Boolean = true; inline def f: Boolean = g }
         |trait VO extends V { override inline def g: Boolean = scala.compiletime.codeOf(1) == "1" }
         |""".stripMargin
    )("V.f") === List(
      "inline method: selects g on this, which a call site may type as a subclass of V that " +
        "overrides g inline, and inline method VO.g uses scala.compiletime.codeOf"
    )

  def anonymousGiven =
    reasons(
      "package d\nobject O {\n" +
        "  inline given Conversion[Int, String] = (i: Int) => scala.compiletime.codeOf(i)\n" +
        "  inline def g(i: Int): String = summon[Conversion[Int, String]](i)\n}\n"
    )("d.O.g") === List(
      "inline method: calls inline method d.O.<given@3>, which uses scala.compiletime.codeOf"
    )

  def localChild =
    reasons(
      "package s\nsealed trait F\nobject M { def make(b: Boolean): F = { class L extends F; new L } }\n"
    )("s.M.make") === List("sealed family has local or anonymous subclasses in its file: F")

  def deeper =
    reasons(
      """|trait V { def g: Boolean = true; inline def h: Boolean = g; inline def f: Boolean = h }
         |trait VO extends V { override inline def g: Boolean = scala.compiletime.codeOf(1) == "1" }
         |""".stripMargin
    )("V.f") === List(
      "inline method: calls inline method VO.g, which uses scala.compiletime.codeOf"
    )

}
