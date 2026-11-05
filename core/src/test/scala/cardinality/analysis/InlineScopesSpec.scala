package cardinality.analysis

import org.specs2.Specification

/** What an inline body reaches besides its own calls: the definitions written inside it, the
  * templates `this` names there, the clauses an application applies to a callee's result, the
  * classes an expansion reifies, and conversions behind aliases (register D27). Every fixture is
  * valid Scala 3.9.0 (probes/a6-fix2, run: the printed values are the ones the comments quote).
  */
class InlineScopesSpec extends Specification with InventoryFixtures {

  def is = s2"""
    Runtime classes, alias chains and verdicts
      a conversion behind any number of aliases is found                $aliasChains
  """

  // Fixture aliases: f(1) prints "none", then "abcd" under a call-site given String — nine aliases
  // deep (review-A6 q depth8: eight were past the old cut-off and counted).
  def aliasChains = {
    val aliases = (2 to 9).map(n => s"type A$n[X, Y] = A${n - 1}[X, Y]").mkString("\n")
    val found = rows(s"""import scala.compiletime.summonFrom
      |type A1[X, Y] = Conversion[X, Y]
      |$aliases
      |inline given A9[Int, String] = (x: Int) => summonFrom { case s: String => s; case _ => "none" }
      |object U { inline def f(x: Int): String = x }""".stripMargin)
    refused(found("U.f"), "may expand the inline conversion given A9[Int, String]")
  }

}
