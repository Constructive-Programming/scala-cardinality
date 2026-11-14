package cardinality.compiled

import org.specs2.Specification

/** A row's signature as the source writes it, on one line (`Source.signature`). */
class SignatureSpec extends Specification {

  def is = s2"""
    A secondary constructor is `this` and its clauses               $secondary
    A given whose type is in parentheses keeps the closing one      $parenthesized
    A literal type keeps its whitespace                             $literal
    A refinement over several lines separates its members           $refinement
    A braceless match type result reads as valid Scala              $braceless
    Curried clauses over several lines read as on one               $curried
    A name in backticks keeps both ticks                            $backticks
    Layout and a trailing comma are dropped                         $layout
    Literals, comments and line breaks inside a block stay one line $scanned
  """

  private def all(code: String): List[String] =
    cardinality.Compiled
      .analyze(cardinality.Compiled.inputs("Example.scala" -> code))
      .map(_.signature)

  private def signatures(code: String): Map[String, String] =
    cardinality.Compiled
      .analyze(cardinality.Compiled.inputs("Example.scala" -> code))
      .map(entry => entry.name -> entry.signature)
      .toMap

  def secondary =
    all(
      "package s\nfinal class P(val n: Boolean) { def this(u: Unit) = this(true) }\n"
    ).toSet === Set("P(val n: Boolean)", "this(u: Unit)")

  def parenthesized =
    signatures(
      "package g\ntrait Getter[S, A]\nobject G {\n" +
        "  given lifted: (Boolean ?=> Getter[Unit, Boolean]) = new Getter[Unit, Boolean] {}\n}\n"
    )("g.G.lifted") === "given lifted: (Boolean ?=> Getter[Unit, Boolean])"

  def literal = {
    val found = signatures(
      "package l\nimport scala.annotation.targetName\nobject O {\n" +
        "  def f(x: \"a  b\" & Int): Int = 1\n" +
        "  @targetName(\"f2\") def g(x: \"a b\" & Int): Int = 2\n}\n"
    )
    (found("l.O.f") === "f(x: \"a  b\" & Int): Int").and(
      found("l.O.g") === "g(x: \"a b\" & Int): Int"
    )
  }

  def refinement =
    signatures(
      "package r\ntrait Two { type X; type Y }\nobject R {\n" +
        "  def h(b: Boolean): Two {\n    type X = Int\n    type Y = Boolean\n  } = ???\n}\n"
    )("r.R.h") === "h(b: Boolean): Two { type X = Int; type Y = Boolean }"

  def braceless =
    signatures(
      "package m\nobject M:\n  def f[T](t: T): T match\n    case Int => String\n" +
        "    case _ => Boolean\n  = ???\n"
    )("m.M.f") === "f[T](t: T): T match { case Int => String; case _ => Boolean }"

  def curried =
    signatures("package c\nobject O {\n  def f[A](x: A)\n      (y: A): A = x\n}\n")(
      "c.O.f"
    ) === "f[A](x: A)(y: A): A"

  def backticks =
    signatures(
      "package b\nabstract class P {\n  def `type`: Boolean\n  def `a b`(on: Boolean): Boolean\n}\n"
    ).values.toSet === Set("P", "`type`: Boolean", "`a b`(on: Boolean): Boolean")

  def layout =
    signatures("package s\nobject O {\n  def pick[A](\n      x: A,\n      y: A,\n  ): A = x\n}\n")(
      "s.O.pick"
    ) === "pick[A](x: A, y: A): A"

  def scanned = {
    def line(written: String) = Source.line(written)
    (line("f(q: Char = '\"',\n  s: String = \",\")") === "f(q: Char = '\"', s: String = \",\")")
      .and(line("f(s: String = \"\\\\\",\n  t: Int)") === "f(s: String = \"\\\\\", t: Int)")
      .and(line("f(g: Int => Int = { x =>\n  x + 1 })") === "f(g: Int => Int = { x => x + 1 })")
      .and(line("f(x: Int, // the \"x\n  y: Int)") === "f(x: Int, y: Int)")
      .and(line("T {\n  type X =\n    Int\n  def y: Int\n}") === "T { type X = Int; def y: Int }")
  }

}
